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
