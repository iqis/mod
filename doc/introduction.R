## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
require(mod)

## ------------------------------------------------------------------------
my <- module({
        a <- 1
        b <- 2
        f <- function(x, y) x + y
})

## ------------------------------------------------------------------------
ls(my)

## ------------------------------------------------------------------------
my$a
my$b
my$f(my$a, my$b)

## ------------------------------------------------------------------------
with(my, 
     f(a,b))

## ------------------------------------------------------------------------
use(my)

## ------------------------------------------------------------------------
search()

## ------------------------------------------------------------------------
f(a,b)

## ------------------------------------------------------------------------
drop("my")

## ------------------------------------------------------------------------
ls(my)

my_other<- module({
        refer(my)
        c <- 4
        d <- 5
        f <- function() print("foo") 
})

ls(my_other)
my_other$f()

## ------------------------------------------------------------------------
my_other <- module({
        refer(my, prefix = .)
        c <- 4
        d <- 5
        f <- function() print("foo") 
})

ls(my_other)

my_other$my.f(1, 2)
my_other$f()

## ------------------------------------------------------------------------
room_101 <- module({
        ..diary <- "Dear Diary: I used SPSS today..."
        get_diary <- function(){
                ..diary
        }
})

## ------------------------------------------------------------------------
ls(room_101)
room_101$..diary
room_101$get_diary()

## ------------------------------------------------------------------------
room_102 <- module({
        provide(open_info, get_classified)
        
        open_info <- "I am a data scientist."
        classified_info <- "I can't get the database driver to work."
        get_classified <- function(){
                classified_info
        }
})

ls(room_102)
room_102$open_info
room_102$classified_info
room_102$get_classified()

## ------------------------------------------------------------------------
mpg_analysis <- module({
    require(ggplot2)
    plot <- qplot(mtcars$mpg)    
})
mpg_analysis$plot

## ------------------------------------------------------------------------
"package:ggplot2" %in% search()

## ------------------------------------------------------------------------
counter <- module({
        ..count <- 0
        add_one <- function(){
                #Its necessary to use `<<-` operator.
                ..count <<- ..count + 1 
        }
        reset <- function(){
                ..count <<- 0
        }
        get_count <- function(){
                ..count
        }
})

## ------------------------------------------------------------------------
# initial count: 0
counter$get_count() 

# add_one() twice
counter$add_one() 
counter$add_one() 

# ..count is now 2
counter$get_count() 

# reset ..count to 0
counter$reset()

# ..count is back to 0 again
counter$get_count()

## ------------------------------------------------------------------------
mode(my)
is.environment(my)

## ----error = TRUE--------------------------------------------------------
my$a <- 1
my$new_var <- 1

## ------------------------------------------------------------------------
my_yet_another <- module({
        .var <- "I'm hidden!"
})

## ------------------------------------------------------------------------
ls(my_yet_another)
ls(my_yet_another, all.names = TRUE)

## ------------------------------------------------------------------------
my_yet_another$.var

## ------------------------------------------------------------------------
module_path <- system.file("misc/example_module.R", package = "mod")

## ------------------------------------------------------------------------
example_module <- acquire(module_path)
ls(example_module)
example_module$a
example_module$d()
example_module$e(100)

## ------------------------------------------------------------------------
use(module_path)
ls("module:example_module")
a
d()
e(100)

## ------------------------------------------------------------------------
detach("package:mod")

my_mind <- mod::ule({
  provide(good_thought)
  good_thought <- "I love working on this package!"
  bad_thought <- "I worry that no one will appreciate it."
})

mod::use(my_mind)
good_thought

