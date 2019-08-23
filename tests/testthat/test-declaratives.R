test_that("declaratives refuse to work outside a module", {
        expect_error(mod:::require(covr))
        expect_error(mod:::provide(var1, var2))
        expect_error(mod:::refer(my_mod))
})


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
        require(covr)
        codecov <- codecov
})

test_that("require() do not leak side effect to global environment", {
        expect_true(!"package:purrr" %in% search())
})

test_that("require() gets actual, original, identical objects", {
        expect_identical(mod_req$codecov, covr::codecov)
})


one <- mod::ule({
        number <- 1
        pinyin <- "yi1"
})

two <- mod::ule({
        number <- 2
        pinyin <- "er4"
})

three <- mod::ule({
        number <- 3
        pinyin <- "san1"
})

four <- mod::ule({
        number <- 4
        pinyin <- "si4"
})

test_that("refer() actually refers", {
        target <- mod::ule({
                refer(one)
        })
        expect_identical(sort(ls(target)), sort(c("number", "pinyin")))
})

test_that("refer() stops name conflict", {
        expect_error(mod::ule(refer(one, two)))
})

test_that("refer() prefixes right with `.`", {
        target <- mod::ule({
                refer(one, two, prefix = ., sep = ".")
        })

        expect_identical(sort(ls(target)),
                         sort(c("one.number", "one.pinyin", "two.number", "two.pinyin")))
})

named_mod_1 <- mod::ule({
        name("wow")
})

named_mod_2 <- mod::ule({
        name(wow)
})

test_that("name() works", {
        expect_identical(attr(named_mod_1, "name"), attr(named_mod_2, "name"), "wow")

})
