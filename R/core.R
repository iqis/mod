#' Make a module
#'
#'
#' @examples
#' module_path <- system.file("misc", "example_module.R", package = "modular")
#' example_module <- acquire(module_path)
#'
#' ls(example_module)
#'
#' example_module$a
#' example_module$e(123)
#'
#' @param ... module expression
#' @param module module object, or path to a module file
#' @param parent the enclosing environment
#' @param lock lock the environment
#' @param expose_private expose the private environment as `..private..`
#' @param dot function expression used for active binding to `.`
#' @param as name when attached to search path with `use()`
#'
#' @return an environment containing objects from the module
#' @export
#'
module <- function(..., parent = parent.frame(), lock = TRUE, expose_private = FALSE){
        code <- deparse(substitute(...))
        temp_file <- tempfile("inline_module")
        write(code, temp_file)

        acquire(temp_file, parent = parent, lock = lock, expose_private = expose_private)
}

#' @rdname module
#' @export
bare_module <- function(..., parent = baseenv(), lock = TRUE, expose_private = FALSE){
        res <- module(..., parent = parent, lock = lock, expose_private = expose_private)
        class(res) <- c("bare_module", class(res))
        res
}



#' @rdname module
#' @export
#'
acquire <- function(module, parent = baseenv(), lock = TRUE, expose_private = FALSE) {

        # if neither from module(), nor already has .R ext, auto suffix with .R
        # small .r is forbidden
        if (grepl("inline_module", module) | grepl("\\.r$|\\.R$", module)) {} else {
                module <- paste0(module, ".R")
        }
        # make a new environment, private. This envir has everything inside the module
        private <- new.env(parent = parent)

        # initialize context signatures
        assign("..module..", NULL, envir = private)
        assign("..parent..", parent, envir = private)
        assign("..search..", function() search_path_envirs(parent.env(private)), envir = private)
        assign("..file..", module, envir = private)
        assign("..use..", c(), envir = private)
        assign("..provide..", c(), envir = private)
        assign("..refer..", list(), envir = private)


        # source everything from file to private
        sys.source(file = module, envir = private)

        # = Provide =
        # provide the variables specified in ..provide..
        # if ..provide is empty, provide everything except for `..` prefixed private objs

        # list of objects to be placed in public, from ..provide..;
        obj_name_list <- if (length(private$..provide..) != 0) {
                private$..provide..
        } else {
                ls(private, all.names = TRUE) #This includes hidden objs with name starting w. "."
        }

        # Remove "private" objects with name starting w. ".." from list
        obj_name_list <- obj_name_list[!grepl("^\\.\\.", obj_name_list)]

        # Assign stuff from obj_list to ..public..
        private$..public.. <- as.environment(mget(obj_name_list, private))

        # Assign back obj_name_list to ..provide..
        private$..provide.. <- obj_name_list

        # = Refer =

        if (length(private$..refer..) != 0){

                `if`(length(unique(private$..refer..)) < length(private$..refer..),
                     stop("refer() a module at most once."))

                source_obj_name_list <- lapply(private$..refer.., ls, all.names = TRUE)
                source_include_list <- lapply(private$..refer.., attr, which = "refer_include")
                source_exclude_list <- lapply(private$..refer.., attr, which = "refer_exclude")
                source_prefix_list <- lapply(private$..refer.., attr, which = "refer_prefix")
                source_sep_list <- lapply(private$..refer.., attr, which = "refer_sep")

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
                target_obj_name_list <- ls(private$..public.., all.names = TRUE)

                conflict_name_list <- lapply(source_obj_name_list2,
                                             intersect,
                                             y = target_obj_name_list)

                `if`(length(unlist(conflict_name_list)) > 0,
                        stop(paste0("name conflict: ",
                                    paste(c(conflict_name_list),
                                          collapse = ", "))))

                # refer objs from source to target, with renaming
                for (i in 1:length(private$..refer..)) {
                        mapply(assign,
                               x = source_obj_name_list2[[i]],
                               value = mget(source_obj_name_list[[i]], private$..refer..[[i]]),
                               envir = list(private$..public..)
                        )
                }
        }


        if (expose_private) assign("..private..", private, envir = private$..public..)

        if (lock) lockEnvironment(private$..public.., bindings = TRUE)

        class(private$..public..) <- c("module", class(private$..public..))

        return(private$..public..)
}


#' @rdname module
#' @export
#'
expose <- function(module, as, parent = baseenv(), lock = TRUE, expose_private = FALSE){
        if (is_module(module)) {
                env <- module
                if (missing(as)) as <- deparse(substitute(module))
        } else if (is.character(module) || file.exists(module)) {
                env <- acquire(module = module, parent = parent, lock = lock, expose_private = expose_private)
                if (missing(as)) as <- bare_name(module)
        } else {
                stop("requires module object or path to R file")
        }

        name <- paste0("module:",as)
        if (name %in% search()) drop(as)
        get("attach", envir = .BaseNamespaceEnv, mode = "function")(
                what = env, name = name
        )
}




