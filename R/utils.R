#' Test if the object is a module
#'
#' @param x An object
#' @export
is_module <- function(x) {
        inherits(x, "module")
}

#' Print module
#'
#' @param x object
#' @param ... dot-dot-dot
#'
#' @return NULL
#' @export
#'
print.module <- function(x, ...){
        cat("<module>", "\n")

        # The following doesn't work when built, why?

        # class(x) <- class(x)[2:length(x)]
        # print(x)

        obj_name_list <- ls(x, all.names = TRUE)
        obj_class_list <- lapply(obj_name_list, function(y) class(get(y, envir = x)))
        print_line <- function(name, class){
                cat(paste0("- ", name, ": <", paste(class, collapse = ", "), ">\n"))
        }
        mapply(print_line, name = obj_name_list, class = obj_class_list)
        invisible(x)
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


search_path_envirs <- function(where = parent.frame()){
        where <- `if`(is.function(where), environment(where), where)
        `if`(!is.environment(where), stop("Not an environment"))
        `if`(identical(where, emptyenv()),
             list(where),
             c(where, search_path_envirs(parent.env(where))))
}

search_path_envir_names <- function(where = parent.frame()){
        sapply(search_path_envirs(where), environmentName)
}
