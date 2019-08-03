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
        temp_file <- tempfile("modular_tmp")
        write(code, temp_file)
        acquire(temp_file, parent = parent, lock = lock, expose_private = expose_private)
}


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

#' @rdname module
#' @export
#'
acquire <- function(module, parent = .GlobalEnv, lock = TRUE, expose_private = FALSE) {

        private <- new.env(parent = parent)
        assign("..refer..", list(), envir = private)
        assign("..provide..", list(), envir = private)
        assign("..module..", NULL, envir = private)

        if (grepl("modular_tmp", module) | grepl("\\.r$|\\.R$", module)) {} else {
                module <- paste0(module, ".R")
        } # if neither tempfile from module(), nor already has .R ext, auto suffix with .R
        sys.source(file = module, envir = private) # source everything from file to private

        # = Provide =

        # list of objects to be placed in public, from ..provide..;
        obj_name_list <- if (length(private$..provide..) != 0) {
                private$..provide..
        } else {
                ls(private, all.names = TRUE) #This includes hidden objs with name starting w. "."
        }

        # Remove "private" objects with name starting w. ".." from list
        obj_name_list <- obj_name_list[!grepl("^\\.\\.", obj_name_list)]
        # Assign stuff from obj_list to ..public
        private$..public.. <- as.environment(mget(obj_name_list, private))

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
use <- function(module, as, parent = .GlobalEnv, lock = TRUE, expose_private = FALSE){
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
        attach(what = env, name = name)
}




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
#' @export
provide <- function(...) {
        `if`(exists("..module..", parent.frame()),NULL,
             stop("Only use provide() in a module, as to use interactively is not meaningful"))

        dots <- as.character(match.call(expand.dots = FALSE)$...)
        assign("..provide..", dots, envir = parent.frame())
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
#' @export
refer <- function(..., include = c(), exclude = c(), prefix = "", sep = "."){
        `if`(exists("..module..", parent.frame()),NULL,
             stop("Only use provide() in a module, as to use interactively is not meaningful"))

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

