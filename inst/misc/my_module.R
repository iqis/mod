provide(a,e)


a <- 1
b <- 2
c <- 3

d <- function(){a + b + c}

.hidden_number = 1000


e <- function(some_number) {d() + .hidden_number + some_number}

