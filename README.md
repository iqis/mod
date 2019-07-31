
<!-- README.md is generated from README.Rmd. Please edit that file -->

# modular

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/iqis/modular?branch=master&svg=true)](https://ci.appveyor.com/project/iqis/modular)
[![Travis build
status](https://travis-ci.org/iqis/modular.svg?branch=master)](https://travis-ci.org/iqis/modular)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The `modular` package is lightweight module system; It provides a simple
way to structure program and data into modules for programming and
interactive use, without the formalities of R packages.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("iqis/modular")
```

## Programming Interface

  - Make a module:`module()`, `acquire()`
  - Declare public variables within a module: `provide()`
  - Attach a module to the search path: `use()`
  - Import bindings from one module to another: `refer()`

## Examples

Define an inline module:

``` r
my <- module({
        a <- 1
        b <- 2
        add_nums <- function(x, y) x + y
})
```

The resulting module contains the variables defined within.

``` r
ls(my)
#> [1] "a"        "add_nums" "b"
```

Subset the module.

``` r
my$a
#> [1] 1
my$b
#> [1] 2
my$add_nums(my$a, my$b)
#> [1] 3
```

Use the `with()` to spare qualification.

``` r
with(my, 
     add_nums(a,b))
#> [1] 3
```

### Attach to the Search Path

Just like a package, a module can be attached to the search path.

``` r
use(my)
```

The `my` module is attached to the search path as “module:my”.

``` r
search()
#>  [1] ".GlobalEnv"        "module:my"         "package:modular"  
#>  [4] "package:stats"     "package:graphics"  "package:grDevices"
#>  [7] "package:utils"     "package:datasets"  "package:methods"  
#> [10] "Autoloads"         "package:base"
```

This is the preferred way to spare qualification.

``` r
add_nums(a,b)
#> [1] 3
```

### Refer Bindings

Use `refer()` to “import” variables from another module.

``` r
ls(my)
#> [1] "a"        "add_nums" "b"

my_other<- module({
        refer(my)
        
        c <- 4
        d <- 5
})

ls(my_other)
#> [1] "a"        "add_nums" "b"        "c"        "d"
```

### Private Variables

A variable is *private* if its name starts with `..`.

``` r
room_101 <- module({
        ..diary <- "Dear Diary: I used SPSS today..."
        get_diary <- function(){
                ..diary
        }
})
```

A private variable cannot be seen or touched. There is no way to access
the `..diary` from the outside, except by a function defined within the
module. This can be useful if you want to shield some information from
the user or other programs.

``` r
ls(room_101)
#> [1] "get_diary"
room_101$..diary
#> NULL
room_101$get_diary()
#> [1] "Dear Diary: I used SPSS today..."
```

Another way is using `provide()` function to declair public variables,
while all others become private.

``` r
room_102 <- module({
        provide(open_info, get_classified)
        
        open_info <- "I am a data scientist."
        classified_info <- "I can't get the database driver to work."
        get_classified <- function(){
                classified_info
        }
})

ls(room_102)
#> [1] "get_classified" "open_info"
room_102$open_info
#> [1] "I am a data scientist."
room_102$classified_info
#> NULL
room_102$get_classified()
#> [1] "I can't get the database driver to work."
```

### Simlate OOP

The below example simulates the essential behavior of an object in
Object-oriented Programming by manipulating the state of `..count`.

``` r
counter <- module({
        ..count <- 0
        add_one <- function(){
                #Its necessary to use `<<-` operator, as ..count lives in the parent frame.
                ..count <<- ..count + 1 
        }
        reset <- function(){
                ..count <<- 0
        }
        get_count <- function(){
                ..count
        }
})
```

The following demonstration should be self-explanatory:

``` r
counter$get_count() 
#> [1] 0

counter$add_one() 
counter$add_one() 

counter$get_count() 
#> [1] 2

counter$reset()

counter$get_count()
#> [1] 0
```

It is imperative that `modular` be only adopted in the simplest cases,
as it is not made for OOP, and do not support most typical OOP features,
such as deep copy, inheritance, or else. If full-featured OOP is
desired, use
[`R6`](https://cran.r-project.org/web/packages/R6/index.html).

## Note

#### Environment

A module *is* an environment. This means that every rule that applies to
environments, such as copy-by-reference, applies to modules as well.

``` r
mode(my)
#> [1] "environment"
is.environment(my)
#> [1] TRUE
```

#### Locked

It is impossible to either change the value of a variable or add a new
variable to a module. A private variable’s value can only be changed by
a function defined within the module, as shown previously.

``` r
my$a <- 888
#> Error in my$a <- 888: cannot change value of locked binding for 'a'
my$c <- 666
#> Error in my$c <- 666: cannot add bindings to a locked environment
```

#### Hidden Variables

As a general R rule, names that start with `.` define hidden variables.

``` r
my_yet_another <- module({
        .var <- "I'm hidden!"
})
```

Hidden variables are not returned by `ls()`, unless specified.

``` r
ls(my_yet_another)
#> character(0)
ls(my_yet_another, all.names = TRUE)
#> [1] ".var"
```

Nonetheless, in `modular`, they are treated the same as public
variables.

``` r
my_yet_another$.var
#> [1] "I'm hidden!"
```

#### Load/Attach from File

``` r
module_path <- system.file("misc/example_module.R", package = "modular")
```

To load and assign to variable:

``` r
example_module <- acquire(module_path)
ls(example_module)
#> [1] "a" "d" "e"
example_module$a
#> [1] 1
example_module$d()
#> [1] 6
example_module$e(100)
#> [1] 106
```

To load and attach to search path:

``` r
use(module_path)
#> The following object is masked from module:my:
#> 
#>     a
ls("module:example_module")
#> [1] "a" "d" "e"
a
#> [1] 1
d()
#> [1] 6
e(100)
#> [1] 106
```
