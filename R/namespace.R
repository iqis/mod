#' Enter a module
#'
#' Load and attach a module to the search path. This method disregards `provide()` statement.
#'
#' @example
#' enter("C:/Users/siqi/OneDrive/R-Play/lispr/R/my_module.R")
#'
#' @param file path to an R file
#' @param name name of the module to be used in search path
#' @param ... dot-dot-dot, any additional arguments for 'attach' function
#' @export
enter <- function(file, as = paste0(basename(file)), ...){
        env <- new.env()
        sys.source(file = file, envir = env)

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



#' Provide objects from a module
#'
#' @param ... dot-dot-dot
#' @return NULL
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
#'
#'
#' @param file path to an R file
#' @param all_objects Boolean; whether to include all objects, disregarding `provide()` declarations
#' @return an environment containing objects from the module
#' @export
acquire <- function(file, all_objects = FALSE) {
        private <- new.env()
        sys.source(file = file, envir = private)
        assign(".public", new.env(), envir = private)

        obj_list <- if (all_objects || !exists(x = ".provide", envir = private)) {
                ls(private)
        } else {
                private$.provide
        }

        for (obj in obj_list) {
                assign(x = obj, value = get(obj, envir = private), envir = private$.public)
        }

        lockEnvironment(private$.public, bindings = TRUE)

        return(private$.public)
}



## use()? procure() grab() bring()

#' Exposes the objects in one environment to another
#'
#'@param source an environment that provides the objects
#'@param target an environment that receives the objects
#'@export
refer <- function(source, target = parent.frame()){
        ## add arguments: only, exclude, rename(that takes a list), prefix
        for (obj_name in ls(source, all.names = TRUE)) {
                assign(x = obj_name, value = get(obj_name, envir = as.environment(source)), envir = target)
        }
}
