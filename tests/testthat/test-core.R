mod_test <- mod::ule({
        a <- 1
        b <- 2
        f <- function() a + b
})

test_that("module has class", {
        expect_s3_class(mod_test, "module")
})

test_that("is_module() works", {
        expect_true(mod::is_module(mod_test))
})


test_that("print.module() actually prints stuff ", {suppressMessages({
        expect_output(mod:::print.module(mod_test))
})})


test_that("use() can attach a module to the search path", {
        mod::use(mod_test)
        expect_true("module:mod_test" %in% search())
})

test_that("use() detaches the previously attached package first", {
        mod::use(mod_test)
        mod::use(mod_test)
        expect_equal(unique(search()), search())
})

test_that("use() returns error when no file found", {
        expect_error(mod::use("a_fake_path"))
})

test_that("drop() can drop a module from the search path", {
        mod::use(mod_test)
        mod::drop("mod_test")
        mod::use(mod_test)
        mod::drop() # no arg also works
        expect_false("module:mod_test" %in% search())
})

test_that("drop() returns error when no module in search path", {
        expect_error(drop())
})


