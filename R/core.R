#' Declare a module in-line
#'
#' @param ... expression
#' @param all_objects Boolean; whether to include all objects, disregarding `provide()` declarations
#'
#' @return an environment containing objects from the module
#' @export
#'
#' @examples
module <- function(..., all_objects = FALSE){
        code <- deparse(substitute(...))
        temp_file <- tempfile("modular_tmp")
        write(code, temp_file)
        acquire(temp_file, all_objects = all_objects)
}



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
#' any hidden object (which name/symbol preceded by `.`) is only accessible through
#' functions defined in the module.
#'
#' @examples
#' my_module_path <- system.file("misc", "my_module.R", package = "modular")
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
        if (grepl("modular_tmp", file) | grepl("\\.r$|\\.R$", file)) {} else {
                file <- paste0(file, ".R")
                } # if neither tempfile from module(), nor has .R ext names, auto suffix with .R
        sys.source(file = file, envir = private) # source everything from file to private
        assign("..public", new.env(), envir = private) # public environment inside private

        # list of objects to be placed in public, from .provide;
        # if undefined, everything other than hidden objs
        obj_list <- if (all_objects || !exists(x = ".provide", envir = private)) {
                ls(private, all.names = TRUE) #This includes hidden objs with name starting w. "."
        } else {
                private$.provide
        }

        # Remove "private" objects with name starting w. ".." from list
        obj_list <- obj_list[!grepl("^\\.\\.", obj_list)]

        # assign objects from private to public
        for (obj in obj_list) {
                assign(x = obj, value = get(obj, envir = private), envir = private$..public)
        }

        lockEnvironment(private$..public, bindings = TRUE)

        class(private$..public) <- c("module", class(private$..public))

        return(private$..public)
}

#' Test if the object is a module
#'
#' @param x An object
#' @export
is_module <- function(x) {
        inherits(x, "module")
}

print.module <- function(x, ...){
        cat("<module>", "\n")



}

#' Exposes the objects from one environment to another
#'
#'@param source the providing environment
#'@param target the receiving environment
#'@export
refer <- function(source, target = parent.frame()){
        ## add arguments: only, exclude, rename(that takes a list), prefix
        assertthat::assert_that(is.environment(source), is.environment(target))

        for (obj_name in ls(source, all.names = TRUE)) {
                assign(x = obj_name, value = get(obj_name, envir = as.environment(source)), envir = target)
        }
}

#' Use a module
#'
#' Load and attach a module to the search path
#'
#' @examples
#' \dontrun{
#' use("~/R/my_module.R")
#' }
#' @param module path to an R file or a symbol for a module object
#' @param all_objects Boolean; whether to include all objects, disregarding `provide()` declarations
#' @param as name to be used in the search path
#' @param ... dot-dot-dot, any additional arguments for 'attach' function
#'
#' @export
use <- function(module, as, all_objects = FALSE, ...){
        if (is_module(module)) {
                env <- module
                if (missing(as)) as <- deparse(substitute(module))
        } else if (is.character(module) || file.exists(module)) {
                env <- acquire(file = module, all_objects = all_objects)
                if (missing(as)) as <- bare_name(module)
        } else {
                stop("requires module object or path to R file")
        }

        name <- paste0("module:",as)
        if (name %in% search()) drop(as)
        attach(what = env, name = name, ...)
}


#' Drop a module
#'
#' Detach a module from the search path.
#'
#' @param  name name of the module to exit from
#' @export
drop <- function(name) {
        if (missing(name)) {
                search_path <- search()
                name <- search_path[grepl("module:", search_path)][1]
        } else {
                name <- paste0("module:", name)
        }

        if (is.na(name)) stop("no module attached in search path")

        detach(name = name, character.only = TRUE)
}


bare_name <- function(path){
        gsub("(\\.+)(?!.*\\1).*$", "", basename(path), perl = TRUE)
}
