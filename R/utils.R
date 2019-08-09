#' Use a Package as if a Module
#'
#' @param package name of a package; character
#'
#' @return a module that contains a package's exported objects
#' @export
#'
#' @examples
#'
#' covr <- as_module("covr")
#' ls(covr)
#' \dontrun{
#' covr$codecov()
#' }
#'
as_module <- function(package){
        pkg_ns <- asNamespace(package, base.OK = FALSE)
        res <- list2env(mget(x = ls(pkg_ns$.__NAMESPACE__.$exports),
                             envir = pkg_ns,
                             inherits = FALSE),
                        parent = baseenv())
        lockEnvironment(res, bindings = TRUE)
        structure(res, class = "module")
}

# Masks

masks <- new.env(parent = emptyenv())

masks$library <- function(){
        stop("Only use library() outside of a module;
             To load and attach a package locally, use require().")
}

masks$attach <- function(){
        stop("Only use attach() outside of a module;
             To copy objects from another module, use refer();
             To load and attach a package locally, use require().")
}

masks$detach <- function(){
        stop("Only use detach() outside of a module.")
}

masks$install.packages <- function(){
        stop("Only use install.packages() outside of a module.")
}

masks$update.packages <- function(){
        stop("Only use update.packages() outside of a module.")
}

masks$use <- function(){
        stop("Only use use() outside of a module;
             To copy objects from another module, use refer().")
}

masks$drop <- function(){
        stop("Only use drop() outside of a module;")
}

masks$source <- function(){
        stop("Only use source() outside of a module;
             To make a nested module, use module()/acquire().")
}


# Helpers
search_path_envirs <- function(where = parent.frame()){
        where <- `if`(is.function(where), environment(where), where)
        `if`(!is.environment(where), stop("Not an environment"))
        `if`(identical(where, emptyenv()),
             list(where),
             c(where, search_path_envirs(parent.env(where))))
}

search_path <- function(where = parent.frame()){
        sapply(search_path_envirs(where), environmentName)
}
