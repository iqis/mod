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

#' @export
`[.thing` <- function(x, ...){
        x$.
}
