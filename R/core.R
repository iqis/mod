#' Make a module
#'
#'
#' @examples
#' module_path <- system.file("misc", "example_module.R", package = "modular")
#' example_module <- acquire(module_path)
#'
#' ls(example_module)
#'
#' example_module$a
#' example_module$e(123)
#'
#' @param ... module expression
#' @param module module object, or path to a module file
#' @param parent the enclosing environment
#' @param lock lock the environment
#' @param expose_private expose the private environment as `..private..`
#' @param dot function expression used for active binding to `.`
#' @param as name when attached to search path with `use()`
#'
#' @return an environment containing objects from the module
#' @export
#'
module <- function(..., parent = parent.frame(), lock = TRUE, expose_private = FALSE){
        code <- deparse(substitute(...))
        temp_file <- tempfile("inline_module")
        write(code, temp_file)

        acquire(temp_file, parent = parent, lock = lock, expose_private = expose_private)
}

#' @rdname module
#' @export
bare_module <- function(..., parent = baseenv(), lock = TRUE, expose_private = FALSE){
        res <- module(..., parent = parent, lock = lock, expose_private = expose_private)
        class(res) <- c("bare_module", class(res))
        res
}



#' @rdname module
#' @export
#'
acquire <- function(module, parent = baseenv(), lock = TRUE, expose_private = FALSE) {

        # if neither from module(), nor already has .R ext, auto suffix with .R
        # small .r is forbidden
        if (grepl("inline_module", module) | grepl("\\.r$|\\.R$", module)) {} else {
                module <- paste0(module, ".R")
        }
        # make a new environment, private. This envir has everything inside the module
        private <- new.env(parent = parent)

        # initialize context signatures
        assign("..module..", NULL, envir = private) # an empty signature, for future use
        assign("..path..", module, envir = private) # file path of module
        assign("..parent..", parent, envir = private) # specified parent env
        assign("..search..", function() search_path_envirs(parent.env(private)), envir = private) # private's search path
        assign("..use..", c(), envir = private) # names of used packages
        assign("..link..", new.env(parent = parent), envir = private) # an environment that has objects from used packages
        assign("..shim..", new.env(parent = private$..link..), envir = private)
        parent.env(private) <- private$..shim..
        assign("..provide..", c(), envir = private) # names of provided objects
        assign("..refer..", list(), envir = private) # names of referred modules
        assign("..public..", new.env(parent = private), envir = private) # public env

        # inject modular bindings to private
        modular_ns <- asNamespace("modular")
        mapply(assign,
               x = ls(modular_ns),
               value = mget(ls(modular_ns), envir = asNamespace("modular")),
               envir = list(private$..shim..))
        parent.env(private) <- private$..shim.. # attach private to ..shim..

        # ..public.. => ..private.. => ..shim.. => ..link.. => ..parent..

        # source everything from file to private
        sys.source(file = module, envir = private)

        # ====== Provide ======
        # provide the variables specified in ..provide..
        # if ..provide is empty, provide everything except for `..` prefixed private objs

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

        class(private$..public..) <- c("module", class(private$..public..))

        return(private$..public..)
}



#' @rdname module
#' @export
#'
expose <- function(module, as, parent = baseenv(), lock = TRUE, expose_private = FALSE){
        if (is_module(module)) {
                env <- module
                if (missing(as)) as <- deparse(substitute(module))
        } else if (is.character(module) || file.exists(module)) {
                env <- acquire(module = module, parent = parent, lock = lock, expose_private = expose_private)
                if (missing(as)) as <- bare_name(module)
        } else {
                stop("requires module object or path to R file")
        }

        name <- paste0("module:",as)
        if (name %in% search()) drop(as)
        get("attach", envir = .BaseNamespaceEnv, mode = "function")(
                what = env, name = name
        )
}




