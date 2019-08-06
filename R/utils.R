# Masks

library <- function(){
        stop("Only use library() outside of a module")
}

attach <- function(){
        stop("Only use attach() outside of a module")
}

install.packages <- function(){
        stop("Only use install.packages() outside of a module")
}

update.packages <- function(){
        stop("Only use update.packages() outside of a module")
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
