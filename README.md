
<!-- README.md is generated from README.Rmd. Please edit that file -->

# modular

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/iqis/modular?branch=master&svg=true)](https://ci.appveyor.com/project/iqis/modular)
[![Travis build
status](https://travis-ci.org/iqis/modular.svg?branch=master)](https://travis-ci.org/iqis/modular)
<!-- badges: end -->

The `modular` package provide a simple way to structure program and data
into modules, without the formalities of R packages.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("iqis/modular")
```

``` r
require(modular)
#> Loading required package: modular
#> 
#> Attaching package: 'modular'
#> The following object is masked from 'package:base':
#> 
#>     drop
```

## Example

Define an inline module with `module()`:

``` r
my_module <- module({
        a <- 1
        b <- 2
        add_nums <- function(x, y) x + y
})
```

The resulting module is an environment that contains defined variables.

``` r
str(my_module)
#> Classes 'module', 'environment' <environment: 0x0000000013520508>
ls(my_module)
#> [1] "a"        "add_nums" "b"
```

Subset the module with `$` operator to access the variables.

``` r
my_module$a
#> [1] 1
my_module$b
#> [1] 2
my_module$add_nums(my_module$a, my_module$b)
#> [1] 3
```

Using the `with()` function, the user can access everything in the
module without qualifying with the module’s name.

``` r
with(my_module, 
     add_nums(a,b))
#> [1] 3
```

### Hidden & Private Variables

If a name of a variable is prepended with `.` or `..`, they are *hidden*
or *private*, respectively.

``` r
my_module2 <- module({
        .hidden_var <- 2
        ..private_var <- 3
        get_private_var <- function(){
                ..private_var
        }
})
```

As an R rule, a hidden variable cannot be seen, unless one specifies the
`all.names` argument to be `TRUE` in `ls()`.

``` r
ls(my_module2)
#> [1] "get_private_var"
ls(my_module2, all.names = TRUE)
#> [1] "get_private_var"
```

However, a `.hidden_var` is still directly accessible.

``` r
my_module2$.hidden_var
#> NULL
```

A private variable, in contrast, cannot be seen or touched. There is no
way to access the `..private_var` from the outside, except by a function
defined within the module. This can be useful if you want to shield some
information from the user or other programs.

``` r
my_module2$..private_var
#> NULL
my_module2$get_private_var()
#> [1] 3
```

It is possible to use a `provide()` function to explicitly declair
public variables; In the mean time, all other objects excluded from
`provide()` will become private variables.

``` r
my_module3 <- module({
        provide(a, get_b)
        a <- 1
        b <- 2
        get_b <- function(){
                b
        }
})
```

``` r
my_module3$a
#> [1] 1
my_module3$b
#> NULL
my_module3$get_b()
#> [1] 2
```

### Simlate OOP

The below example, in effect simulates the behavior of an object
Object-oriented Programming by manipulating the state of `..count`.

``` r
my_counter <- module({
        ..count <- 0
        add_one <- function(){
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

``` r
my_counter$get_count()
#> [1] 0
my_counter$add_one()
my_counter$add_one()
my_counter$get_count()
#> [1] 2
my_counter$reset()
my_counter$get_count()
#> [1] 0
```
