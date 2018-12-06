#' Provide objects from a module
#'
#' @examples
#'
#' provide(a)
#'
#' a <- 1
#' b <- 2
#' c <- 3
#' @param ... dot-dot-dot: name of any object to be accessible by user
#' @export
provide <- function(...) {
        dots <- as.character(match.call(expand.dots = FALSE)$...)
        assign(x = ".provide", value = dots, envir = parent.frame())
}


#' Acquire a module from a file
#'
#' any hidden object (which name/symbol preceded by `.`) are only accessible through
#' function defined in the module.
#'
#' @examples
#' my_module_path <- system.file("misc", "my_module.R", package = "ntr")
#'
#' my_module <- acquire(my_module_path)
#'
#' ls(my_module)
#'
#' my_module$a
#' my_module$e(123)
#'
#' @param file path to an R file
#' @param all_objects Boolean; whether to include all objects, disregarding `provide()` declarations
#' @return an environment containing objects from the module
#' @export
acquire <- function(file, all_objects = FALSE) {
        private <- new.env() # private environment
        sys.source(file = file, envir = private) # source everything from file to private
        assign(".public", new.env(), envir = private) # public environment inside private

        # list of objects to be placed in public, from .provide;
        # if undefined, everything other than hidden objs
        obj_list <- if (all_objects || !exists(x = ".provide", envir = private)) {
                ls(private)
        } else {
                private$.provide
        }

        # assign objects from private to public
        for (obj in obj_list) {
                assign(x = obj, value = get(obj, envir = private), envir = private$.public)
        }

        lockEnvironment(private$.public, bindings = TRUE)

        return(private$.public)
}



## use()? procure() grab() bring()

#' Exposes the objects in one environment to another
#'
#'@param source the providing environment
#'@param target the receiving environment
#'@export
refer <- function(source, target = parent.frame()){
        ## add arguments: only, exclude, rename(that takes a list), prefix
        for (obj_name in ls(source, all.names = TRUE)) {
                assign(x = obj_name, value = get(obj_name, envir = as.environment(source)), envir = target)
        }
}
