test_that("as_module() works", {
        expect_s3_class(mod::as_module("covr"), "module")
        expect_true(is.environment(mod::as_module("covr")))
})


test_that("masks work inside module", {
        expect_error(mod::ule(library()))
        expect_error(mod::ule(attach()))
        expect_error(mod::ule(detach()))
        expect_error(mod::ule(install.packages()))
        expect_error(mod::ule(update.packages()))
        expect_error(mod::ule(use()))
        expect_error(mod::ule(drop()))
        expect_error(mod::ule(source()))
})


test_that("helpers work", {
        test_mod <- mod::ule({},
                             parent = baseenv(),
                             expose_private = TRUE)
        expect_length(test_mod$..private..$..search..(), 5)
        expect_length(mod:::search_path(test_mod$..private..), 6)
})
