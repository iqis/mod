#' Provide objects from a module
#'
#' @examples
#'
#' \dontrun{
#' provide(a, c)
#'
#' a <- 1
#' b <- 2
#' c <- 3
#' d <- 4
#' }
#' @param ... dot-dot-dot: name of any object to be accessible by user
#' @export
provide <- function(...) {
        `if`(identical(globalenv(), parent.frame()), stop("Only use in a module, as to use interactively is not meaningful"))
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

        class(private$.public) <- c("module", class(private$.public))

        return(private$.public)
}

#'@export
is_module <- function(x) {
        inherits(x, "module")
}

#' Exposes the objects from one environment to another
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

#' Enter a module
#'
#' Load and attach a module to the search path. This method disregards `provide()` statement.
#'
#' @example
#' enter("~/R/my_module.R")
#'
#' @param module path to an R file or a symbol for a module object
#' @param name name of the module to be used in search path
#' @param all_objects Boolean; whether to include all objects, disregarding `provide()` declarations
#' @param ... dot-dot-dot, any additional arguments for 'attach' function
#' @export
enter <- function(module, as = paste0(basename(file)), all_objects = FALSE, ...){
        if (file.exists(module)) {
                env <- acquire(file = file, all_objects = all_objects)
        } else {
                env <- module
        }
        attach(what = env, name = paste0("module:",as), ...)

}


#' Exit from a module
#'
#' Detach a module from the search path.
#'
#' @param  name name of the module to exit from
#' @export
exit <- function(name) {
        if (missing(name)) {
                search_path <- search()
                name <- search_path[grepl("module:", search_path)][1]
        } else {
                name <- paste0("module:", name)
        }

        if (is.na(name)) stop("no module attached in search path")

        detach(name = name, character.only = TRUE)
}

