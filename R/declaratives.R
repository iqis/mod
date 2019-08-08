#' Provide Objects from a Module
#'
#' Can only be used inside a module expression.
#' If this function is used, only the names included as argument are public.
#' If not used, every name in the module will be public.
#'
#' @param ... name of any object to be accessible by user; name or character
#' @examples
#'
#' mod_a <- mod::ule({
#'     # names included in provide() are public, however...
#'     provide(var,.var, ..var)
#'     var <- 1
#'     .var <- 2
#'     ..var <- 3 # objects denoted by .. prefix are always private.
#'     another_var <- 4 # objects not included in provide() are also private.
#' })
#'
#' mod_b <- mod::ule({
#'     # if no call to provide(), all objects are public, except...
#'     var <- 1
#'     .var <- 2
#'     ..var <- 3 # objects denoted by .. prefix are always private.
#' })
#'
#' ls(mod_a)
#' ls(mod_b)
#'
provide <- function(...) {
        `if`(!exists("..module..", parent.frame(), inherits = FALSE),
             stop("Only use provide() in a module."))

        obj_names <- as.character(match.call(expand.dots = FALSE)$...)
        existing_obj_names <- get("..provide..", envir = parent.frame())
        obj_names <- unique(c(existing_obj_names, obj_names))
        assign("..provide..", unique(obj_names, existing_obj_names), envir = parent.frame())
}


#' Copy Bindings from a Module to Another
#'
#' Can only be used inside a module expression.
#' Makes reference to objects from one module, with specified filters.
#'
#' @param ... names of modules; dot-dot-dot
#' @param include names to include; character
#' @param exclude names to excludde; character
#' @param prefix prefix to names; character
#' @param sep separator between prefix and names; character
#' @examples
#'
#' mod_a <- mod::ule(number <- 1)
#' mod_b <- mod::ule(number <- 2)
#'
#' mod_c <- mod::ule({
#'     refer(mod_a, mod_b, prefix = .)
#'     number <- mod_a.number + mod_b.number
#' })
#'
#' mod_c$number
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


#' Load/Attach Package to Local Search Path
#'
#' Can only be used in a module expression.
#' Emulates the effect of base::require() in its containing module, making functions and their chain of environment availab.e
#'  Masks base::require() inside a module context.
#'
#' @param package name of the package; name or character
#' @examples
#'
#' mod_tcl <- mod::ule({
#'     require(tcltk)
#'     f <- tcl
#' })
#'
#' identical(mod_tcl$f, tcltk::tcl)
#'
require <- function(package){
        `if`(!exists("..module..", parent.frame(), inherits = FALSE),
             stop("Only use mod::require() in a module."))

        package <- substitute(package)
        package <- `if`(is.character(package),package,deparse(package))

        `if`(system.file("", package = package) == "",
             stop(paste(package, "is not an installed package")))

        private <- parent.frame()

        existing_pkg_names <- get("..require..", envir = private)
        pkg_names <- c(existing_pkg_names, package)

        assign("..require..", pkg_names, private)

        # mod::require() dumps all bindings from every dependency into ..link..,
        # is this good enough? or it's necessary to implement a real local search path?
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
