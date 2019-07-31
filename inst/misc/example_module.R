provide(a,d,e)

a <- 1
.b <- 2
..c <- 3

d <- function(){a + .b + ..c}

e <- function(some_number) {d() + some_number}

