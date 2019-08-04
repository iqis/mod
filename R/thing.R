#' Make a thing
#'
#'
#' @param ...
#'
#' @param dot
#' @param parent
#' @param lock
#' @param expose_private
#'
#' @rdname module
#' @export
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

#' Test if the object is a module
#'
#' @param x An object
#' @export
is_thing <- function(x) {
        inherits(x, "thing")
}

#' @rdname thing
#' @export
`[.thing` <- function(x, ...){
        x$.
}
