#' Make a Thing
#'
#' A "thing" is a special object made based on a module.
#' Contains an active binding, defined with the `dot` argument.
#'
#' @inheritParams module
#' @param dot function expression used for active binding to `.`
#' @examples
#'
#' my_thing <- mod::thing({
#'     a <- 1
#' }, dot = function() a)
#'
#' my_thing$.
#'
#' my_thing[]
#'
#' @export
#'
thing <- function(..., dot, parent = parent.frame(), lock = TRUE, expose_private = FALSE){
        res <- module(..., parent = parent, lock = FALSE, expose_private = TRUE)
        if (!missing(dot)) {
                dot <- substitute(dot)
                makeActiveBinding(".", eval(dot, envir = res$..private..), env = res)
        }

        if (!expose_private) rm("..private..", envir = res)
        if (lock) lockEnvironment(res, bindings = TRUE)

        class(res) <- c("thing", class(res))
        res
}

#' Test if the Object is a Thing
#'
#' @param x an object
#' @export
is_thing <- function(x) {
        inherits(x, "thing")
}

#' Invoke the Active Binding in a Thing
#'
#' @param x a thing
#' @param ... dot-dot-dot, ignored
#' @export
`[.thing` <- function(x, ...){
        x$.
}
