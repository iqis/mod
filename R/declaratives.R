#' Provide objects from a module
#'
#' Only legal inside a module context
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
        `if`(!exists("..module..", parent.frame(), inherits = FALSE),
             stop("Only use provide() in a module."))

        obj_names <- as.character(match.call(expand.dots = FALSE)$...)
        existing_obj_names <- get("..provide..", envir = parent.frame())
        obj_names <- unique(c(existing_obj_names, obj_names))
        assign("..provide..", unique(obj_names, existing_obj_names), envir = parent.frame())
}


#' Refer bindings from a module to another
#'
#' Only legal inside a module context
#'
#' @param ... names of modules; dot-dot-dot
#' @param include names to include; character vector
#' @param exclude names to excludde; character vector
#' @param prefix prefix to names; character
#' @param sep separator between prefix and names; character
#'
#' @export
refer <- function(..., include = c(), exclude = c(), prefix = "", sep = "."){
        `if`(!exists("..module..", parent.frame(), inherits = FALSE),
             stop("Only use provide() in a module."))

        dots <- as.character(match.call(expand.dots = FALSE)$...)
        sources <- lapply(dots, get, envir = parent.frame())
        names(sources) <- dots


        lapply(sources, `attr<-`, "refer_include", include)
        lapply(sources, `attr<-`, "refer_exclude", exclude)
        `if`(deparse(substitute(prefix)) == ".",
             mapply(`attr<-`, x = sources, which = list("refer_prefix"), value = dots),
             lapply(sources, `attr<-`, "refer_prefix", prefix))
        lapply(sources, `attr<-`, "refer_sep", sep)

        assign("..refer..",
               c(get("..refer..", envir = parent.frame()), sources),
               parent.frame())
}


#' @export
use <- function(package){
        `if`(!exists("..module..", parent.frame(), inherits = FALSE),
             stop("Only use use() in a module."))

        `if`(!package %in% installed.packages(),
             stop(paste(package, "is not an installed package")))

        parent <- parent.frame()

        existing_pkg_names <- get("..use..", envir = parent.frame())
        pkg_names <- c(existing_pkg_names, package)

        assign("..use..", pkg_names, parent)
        browser()
        ##
}

#' @export
depend <- function(packages = list()){
        `if`(!exists("..module..", parent.frame(), inherits = FALSE),
             stop("Only use depend() in a module."))
        # only check dependencies.
        #

}
