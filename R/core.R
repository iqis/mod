#' Make a Module
#'
#' Institute a module object inline or from a file.
#' mod::ule() is a useful shorthand for module() when this package is not attached.
#'
#' @param ... module expression
#' @param module module object, or path to a module file
#' @param parent the enclosing environment
#' @param lock lock the environment; logical
#' @param expose_private expose the private environment as `..private..`; logical
#'
#' @return an \code{environment} of class \code{module} containing defined objects
#'
#' @examples
#'
#' # from file
#' module_path <- system.file("misc", "example_module.R", package = "mod")
#' example_module <- source_module(module_path)
#'
#' example_module$e(123)
#'
#' # inline
#' my_module <- mod::ule({
#'     a <- 1
#'     .a <- 2
#'     f <- function(){.a}
#' })
#'
#' my_module$a
#' my_module$f
#'
#' @export
#'
module <- function(..., parent = parent.frame(), lock = TRUE, expose_private = FALSE){
        code <- deparse(substitute(...))
        temp_file <- tempfile("inline_module")
        write(code, temp_file)

        source_module(temp_file, parent = parent, lock = lock, expose_private = expose_private)
}

#' @rdname module
#' @export
#'
ule <- module


#' @rdname module
#' @export
#'
source_module <- function(module, parent = baseenv(), lock = TRUE, expose_private = FALSE) {

        # if neither from module(), nor already has .R ext, auto suffix with .R
        # small .r is forbidden
        if (grepl("inline_module", module) | grepl("\\.R$", module)) {} else {
                module <- paste0(module, ".R")
        }
        # make a new environment, private. This envir has everything inside the module
        private <- new.env(parent = parent)

        # initialize context signatures
        assign("..module..", NULL, envir = private) # an empty signature, for future use
        assign("..name..", "", envir = private) # name of module
        assign("..path..", module, envir = private) # file path of module
        assign("..parent..", parent, envir = private) # specified parent env
        assign("..search..", function() search_path_envirs(parent.env(private)), envir = private) # private's search path
        assign("..require..", c(), envir = private) # names of packages used in the module
        assign("..link..", new.env(parent = parent), envir = private) # an environment that has objects from used packages
        assign("..shim..", new.env(parent = private$..link..), envir = private) # bindings from this `mod` package
        assign("..mask..", new.env(parent = private$..shim..), envir = private) # bindings that masks functions forbidden in module
        parent.env(private) <- private$..mask..
        assign("..provide..", c(), envir = private) # names of provided objects
        assign("..refer..", list(), envir = private) # names of referred modules
        assign("..public..", new.env(parent = private), envir = private) # public env


        # inject mask bindings to ..mask..
        mapply(assign,
               x = ls(masks),
               value = mget(ls(masks), envir = masks),
               envir = list(private$..mask..))
        parent.env(private) <- private$..mask..

        # inject mod package bindings to ..shim..
        mod_ns <- asNamespace("mod")
        mapply(assign,
               x = ls(mod_ns),
               value = mget(ls(mod_ns), envir = mod_ns),
               envir = list(private$..shim..))

        # ..public.. => ..private.. => ..mask.. => ..shim.. => ..link.. => ..parent..

        # source everything from file to private
        sys.source(file = module, envir = private)

        # Do Provide
        # provide the variables specified in ..provide..
        # if ..provide.. is empty, provide everything except for `..` prefixed private objs

        # list of objects to be placed in public, from ..provide..;
        obj_name_list <- if (length(private$..provide..) != 0) {
                private$..provide..
        } else {
                ls(private, all.names = TRUE) #This includes hidden objs with name starting w. "."
        }

        # Remove "private" objects with name starting w. ".." from list
        obj_name_list <- obj_name_list[!grepl("^\\.\\.", obj_name_list)]

        # Assign stuff from obj_list to ..public..
        if (length(obj_name_list) > 0){
                mapply(assign,
                       x = obj_name_list,
                       value = mget(obj_name_list, private),
                       envir = list(private$..public..),
                       SIMPLIFY = FALSE)
        }
        # Assign back obj_name_list to ..provide..
        private$..provide.. <- obj_name_list



        if (expose_private) assign("..private..", private, envir = private$..public..)

        if (lock) lockEnvironment(private$..public.., bindings = TRUE)

        attr(private$..public.., "name") <- private$..name..
        attr(private$..public.., "path") <- private$..path..

        class(private$..public..) <- c("module", class(private$..public..))

        return(private$..public..)
}


#' Attach a Module to the Search Path
#'
#' @inheritParams module
#' @param as name when attached to search; character
#' @return \code{TRUE} if successful; invisible
#'
#' @examples
#'
#' module_path <- system.file("misc", "example_module.R", package = "mod")
#' example_module <- source_module(module_path)
#'
#' # Attach module object to search path
#' use(example_module)
#' # or directly from file
#' use(module_path, "example_module")
#'
#' @export
#'
use <- function(module, as, parent = baseenv(), lock = TRUE, expose_private = FALSE){
        if (is_module(module)) {
                env <- module
                if (missing(as)) as <- deparse(substitute(module))
        } else if (is.character(module) || file.exists(module)) {
                env <- source_module(module = module, parent = parent, lock = lock, expose_private = expose_private)
                bare_name <- function(path){
                        gsub("(\\.+)(?!.*\\1).*$", "", basename(path), perl = TRUE)
                }
                if (missing(as)) as <- bare_name(module)
        } else {
                stop("requires module object or path to R file")
        }

        name <- paste0("module:",as)
        if (name %in% search()) drop(as)
        get("attach", envir = .BaseNamespaceEnv, mode = "function")(
                what = env, name = name
        )
        invisible(TRUE)
}


#' Test if an Object is a Module
#'
#' @param x An object
#' @return \code{TRUE} if the object is a \code{module}, \code{FALSE} otherwise
#' @export
is_module <- function(x) {
        inherits(x, "module")
}


#' Print a Module
#'
#' @param x an object
#' @param ... dot-dot-dot, ignored
#'
#' @return the object itself; invisible
#' @export
#'
print.module <- function(x, ...){
        module_name <- attr(x, "name")
        cat(paste0("<module",
                   `if`(nchar(module_name) > 0, paste0(":", module_name)),
                   ">"),
            "\n")

        obj_name_list <- ls(x, all.names = TRUE)
        obj_class_list <- lapply(obj_name_list, function(y) class(get(y, envir = x)))
        print_line <- function(name, class){
                cat(paste0("- ", name, ": <", paste(class, collapse = ", "), ">\n"))
        }
        mapply(print_line, name = obj_name_list, class = obj_class_list)
        invisible(x)
}

#' Detach a Module from the Search Path
#'
#' If no argument is supplied, detach the most recently attached module.
#'
#' @param  name name of the module to exit from; character
#' @return \code{TRUE} if successful; invisible
#'
#' @examples
#'
#' use(mod::ule({
#'    a <- 1
#' }), as = "my_module")
#'
#' use(mod::ule({
#'    b <- 2
#' }), as = "my_other_module")
#'
#' search()
#'
#' # by name
#' drop("my_module")
#'
#' # and at the head position
#' drop()
#'
#' search()
#'
#' @export
#'
drop <- function(name) {
        if (missing(name)) {
                search_path <- search()
                name <- search_path[grepl("module:", search_path)][1]
        } else {
                name <- paste0("module:", name)
        }

        if (is.na(name)) stop("no module attached in search path")

        detach(name = name, character.only = TRUE)
        invisible(TRUE)
}

