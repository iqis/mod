thing_test <- mod::thing({
        a <- 1
}, dot = function()a)

test_that("thing() and its active binding works", {
        expect_true(mod::is_thing(thing_test))
        expect_equal(thing_test$., thing_test$a, thing_test[])
})
