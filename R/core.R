#' Make a Module
#'
#' Institute a module object inline or from a file.
#' mod::ule() is a useful shorthand for module() when this package is not attached.
#'
#' @param ... module expression
#' @param path path to a module file
#' @param parent the enclosing environment
#' @param lock lock the environment; logical
#'
#' @return an \code{environment} of class \code{module} containing defined objects
#'
#' @examples
#'
#' # from file
#' module_path <- system.file("misc", "example_module.R", package = "mod")
#' example_module <- acquire(module_path)
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
#' @seealso \code{\link{use}}, \code{\link{drop}}
#'
#' @export
#'
module <- function(..., parent = parent.frame(), lock = TRUE){
        code <- deparse(substitute(...))
        temp_file <- tempfile("inline_module")
        write(code, temp_file)

        acquire(temp_file, parent = parent, lock = lock)
}

#' @rdname module
#' @export
#'
ule <- module

#' @rdname module
#' @export
#'
acquire <- function(path, parent = baseenv(), lock = TRUE) {

        # if neither from module(), nor already has .R ext, auto suffix with .R
        # small .r is forbidden
        if (grepl("inline_module", path) | grepl("\\.R$", path)) {} else {
                path <- paste0(path, ".R")
        }
        # make a new environment, private. This envir has everything inside the module
        private <- new.env(parent = parent)

        # initialize context & signatures
        assign("..module..", NULL, envir = private) # an empty signature, for future use
        assign("..name..", "", envir = private) # name of module
        assign("..path..", path, envir = private) # file path of module
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
        sys.source(file = path, envir = private)

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

        res <- private$..public..

        if (lock) lockEnvironment(res, bindings = TRUE)

        structure(res,
                  class = "module",
                  name = private$..name..,
                  path = private$..path..,
                  private = private
        )
}


#' Extract the Private Environment of a Module
#'
#' @param module a module
#'
#' @return environment
#' @export
#'
#' @examples
#'
#' m <- mod::ule({a <- 1})
#' pvt <- private(m)
#'
#' ls(pvt, all.names = TRUE)
private <- function(module){
        `if`(!is_module(module), stop("Not a module"))
        attr(module, "private")
}


#' Attach a Module to the Search Path
#'
#' If the module as a name, defined by name(), it will always be used for the search path.
#'
#' @inheritParams module
#' @param module a module object, or path to a module file
#' @param as name when attached to search; character
#' @return \code{TRUE} if successful; invisible
#'
#' @examples
#'
#' module_path <- system.file("misc", "example_module.R", package = "mod")
#' example_module <- acquire(module_path)
#'
#' # Attach module object to search path
#' use(example_module)
#' # or directly from file
#' use(module_path, "example_module")
#'
#' @seealso \code{\link{drop}}
#'
#' @export
#'
use <- function(module, as, parent = baseenv(), lock = TRUE){
        if (is_module(module)) {
                env <- module
                if (missing(as)) as <- deparse(substitute(module))
        } else if (is.character(module) || file.exists(module)) {
                env <- acquire(path = module, parent = parent, lock = lock)
                bare_name <- function(path){
                        gsub("(\\.+)(?!.*\\1).*$", "", basename(path), perl = TRUE)
                }
                if (missing(as)) as <- bare_name(module)
        } else {
                stop("requires module object or path to R file")
        }

        # if package has name(), always use name
        module_name <- attr(env, "name")
        as <- `if`(nchar(module_name) > 0, module_name, as)

        name <- paste0("module:",as)
        if (name %in% search()) drop(as)
        get("attach", envir = .BaseNamespaceEnv, mode = "function")(
                what = env, name = name
        )
        invisible(TRUE)
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
#' @seealso \code{\link{use}}
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
