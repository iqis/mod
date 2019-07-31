#' Load a module from a file
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
#' @param file path to an R file
#' @param all_objects Boolean; whether to include all objects, disregarding `provide()` declarations
#' @return an environment containing objects from the module
#' @export
acquire <- function(file, all_objects = FALSE) {
        private <- new.env(parent = .GlobalEnv) # private environment inside globalenv
        if (grepl("modular_tmp", file) | grepl("\\.r$|\\.R$", file)) {} else {
                file <- paste0(file, ".R")
        } # if neither tempfile from module(), nor already has .R ext, auto suffix with .R
        sys.source(file = file, envir = private) # source everything from file to private

        # list of objects to be placed in public, from .provide;
        obj_name_list <- if (all_objects || !exists(x = "..provide..", envir = private)) {
                ls(private, all.names = TRUE) #This includes hidden objs with name starting w. "."
        } else {
                private$..provide..
        }

        # Remove "private" objects with name starting w. ".." from list
        obj_name_list <- obj_name_list[!grepl("^\\.\\.", obj_name_list)]

        # Assign stuff from obj_list to ..public
        private$..public.. <- as.environment(mget(obj_name_list, private))

        lockEnvironment(private$..public.., bindings = TRUE)

        class(private$..public..) <- c("module", class(private$..public..))

        return(private$..public..)
}


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
        `if`(identical(globalenv(), parent.frame()), stop("Only use provide() in a module, as to use interactively is not meaningful"))
        dots <- as.character(match.call(expand.dots = FALSE)$...)
        assign(x = "..provide..", value = dots, envir = parent.frame())
}


#' Refer bindings from a module to another
#'
#'@param source the providing module
#'@export
refer <- function(source){
        ## add arguments: only, exclude, rename(that takes a list), prefix

        obj_name_list = ls(source, all.names = TRUE)

        mapply(assign,
               x = obj_name_list,
               value = mget(obj_name_list, source),
               envir = list(parent.frame())
               )
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


