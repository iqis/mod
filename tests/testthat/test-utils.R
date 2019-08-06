test_that("masks work inside module", {
        expect_error(mod::ule(library()))
        expect_error(mod::ule(attach()))
        expect_error(mod::ule(install.packages()))
        expect_error(mod::ule(update.packages()))
})


