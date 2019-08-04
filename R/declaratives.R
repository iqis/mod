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
#'
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
refer <- function(..., include = c(), exclude = c(), prefix = "", sep = "."){
        `if`(!exists("..module..", parent.frame(), inherits = FALSE),
             stop("Only use provide() in a module."))

        private <- parent.frame()


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

        # = Do Refer =

        if (length(sources) != 0){

                `if`(length(unique(private$..refer..)) < length(private$..refer..),
                     stop("refer() a module at most once."))

                source_obj_name_list <- lapply(sources, ls, all.names = TRUE)
                source_include_list <- lapply(sources, attr, which = "refer_include")
                source_exclude_list <- lapply(sources, attr, which = "refer_exclude")
                source_prefix_list <- lapply(sources, attr, which = "refer_prefix")
                source_sep_list <- lapply(sources, attr, which = "refer_sep")

                source_obj_name_list <-
                        mapply(function(src_obj, src_incl, src_excl){
                                res <- src_obj
                                res <- `if`(is.null(src_incl), res, intersect(res, src_incl))
                                res <- `if`(is.null(src_excl), res, setdiff(res, src_excl))
                                res
                        },
                        src_obj = source_obj_name_list,
                        src_incl = source_include_list,
                        src_excl = source_exclude_list,
                        SIMPLIFY = FALSE)

                # prefix obj names, if specified
                source_obj_name_list2 <- mapply(
                        function(prefix, obj_name, sep){
                                `if`(nchar(prefix) >= 1,
                                     paste(prefix, obj_name, sep = sep),
                                     obj_name)
                        },
                        prefix = source_prefix_list,
                        obj_name = source_obj_name_list,
                        sep = source_sep_list,
                        SIMPLIFY = FALSE)

                # Intra-source name conflict

                intersect_w_others <-  function(x, i){
                        mapply(intersect, x[-i], x[i], SIMPLIFY = FALSE)
                }

                source_conflict_name_list <-
                        if (length(source_obj_name_list2) > 1){
                                res <- lapply(seq_along(source_obj_name_list2),
                                              function(i) {
                                                      intersect_w_others(x = source_obj_name_list2,
                                                                         i = i)})
                                names(res) <- names(source_obj_name_list2)
                                res
                        } else {
                                list()
                        }


                `if`(length(unique(unlist(source_conflict_name_list))) > 0,
                     stop(paste0("name conflict among sources: ",
                                 paste(unique(unlist(source_conflict_name_list)),
                                       collapse = ", "))))

                # Source-target name conflict
                target_obj_name_list <- ls(private, all.names = TRUE)

                conflict_name_list <- lapply(source_obj_name_list2,
                                             intersect,
                                             y = target_obj_name_list)

                `if`(length(unlist(conflict_name_list)) > 0,
                     stop(paste0("name conflict: ",
                                 paste(c(conflict_name_list),
                                       collapse = ", "))))

                # refer objs from source to target, with renaming
                for (i in 1:length(sources)) {
                        mapply(assign,
                               x = source_obj_name_list2[[i]],
                               value = mget(source_obj_name_list[[i]], sources[[i]]),
                               envir = list(private),
                               SIMPLIFY = FALSE
                        )
                }
        }
}


#' Attach package to local search path
#'
#' @param package
#'
require <- function(package){
        `if`(!exists("..module..", parent.frame(), inherits = FALSE),
             stop("Only use mod::require() in a module."))

        package <- substitute(package)
        package <- `if`(is.character(package),package,deparse(package))

        `if`(!exists("..module..", parent.frame(), inherits = FALSE),
             stop("Only use use() in a module."))

        `if`(!package %in% installed.packages(),
             stop(paste(package, "is not an installed package")))

        private <- parent.frame()

        existing_pkg_names <- get("..use..", envir = private)
        pkg_names <- c(existing_pkg_names, package)

        assign("..use..", pkg_names, private)

        pkg_ns <- asNamespace(package)
        import_list <- unique(names(pkg_ns$.__NAMESPACE__.$imports))[-1] # get rid of 'base"

        temp_envir <- new.env()

        # inject imports
        if (length(import_list) > 0) {
                for (i in 2:length(import_list)) {
                        mapply(assign,
                               x = ls(asNamespace(import_list[i])),
                               value = mget(x = ls(asNamespace(import_list[i])),
                                            envir = asNamespace(import_list[i])),
                               envir = list(temp_envir))
                }
        } else {


        }

        # inject package: everything, not just exports
        mapply(assign,
               x = ls(pkg_ns),
               value = mget(x = ls(pkg_ns), envir = pkg_ns),
               envir = list(temp_envir))

        # transfer objs
        mapply(assign,
               x = ls(temp_envir),
               value = mget(x = ls(temp_envir), envir = temp_envir),
               envir = list(private$..link..))
}

#' Declare dependencies
#'
#' @param packages
#'
#' @return
#' @export
#'
#' @examples
depend <- function(packages = list()){
        `if`(!exists("..module..", parent.frame(), inherits = FALSE),
             stop("Only use depend() in a module."))
        # only check dependencies.
        #

}
