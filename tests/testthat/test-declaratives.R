mod_pvd <- mod::ule({
        provide(a, .a, ..b)
        provide(a, a, a)
        a <- 1
        .a <- 1
        ..b <- 2
})

test_that("provide() provides both normal and hidden objs, but not private objs;
          can handle duplicate names", {
        expect_equal(sort(ls(mod_pvd, all.names = TRUE)), sort(c("a", ".a")))
})



mod_req <- mod::ule({
        require(purrr)
        map <- map
})

test_that("require() do not leak side effect to global environment", {
        expect_true(!"package:purrr" %in% search())
})

test_that("require() gets actual, original, identical objects", {
        expect_identical(mod_req$map, purrr::map)
})
