
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mod::ule

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/iqis/mod?branch=master&svg=true)](https://ci.appveyor.com/project/iqis/mod)
[![Travis build
status](https://travis-ci.org/iqis/mod.svg?branch=master)](https://travis-ci.org/iqis/mod)
[![codecov](https://codecov.io/gh/iqis/mod/branch/master/graph/badge.svg)](https://codecov.io/gh/iqis/mod)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/mod)](https://cran.r-project.org/package=mod)
![GitHub License](https://img.shields.io/github/license/iqis/mod)

<!-- badges: end -->

The `mod` package provides a simple way to structure program and data
into flexible and independent units for programming and interactive use.
Modules indtroduced by the `mod` package offer the benefit of modules
from other languages, while keeping an authentic R flavor.

## Why?

Programs need to be oraganized into units.

A package is a very robust solution, but is formal and compliated;
packages require additional knowledge to R and must be installed in the
local library by `install.packages()`. On the other hand, simple R
scripts, often executed by `source()`, are brittle and may create
namespace conflict; They are unsuitable for building tools.

Modules fills the hollow middleground by allowing programs to live
separately and work together at the same time. Each module has its own
scope, host is own objects, an can attach different packages without
interefering with each other or the global search path. They can be
created either inline with other programs or from a standalone file, and
can be used in the user’s working environment, as a part of a package,
or in other modules.

## Installation

Install the published version from
[CRAN](https://CRAN.R-project.org/package=mod) with:

``` r
install.packages("mod")
```

or,

Install the development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("iqis/mod")
```

## Use

The `mod` package is designed to be used either attached or unattached
to your search path.

The following demonstrations show the package attached:

``` r
require(mod)
#> Loading required package: mod
#> 
#> Attaching package: 'mod'
#> The following object is masked from 'package:base':
#> 
#>     drop
```

## Vocabulary

The `mod` package has a simple UI:

  - Make a module:
      - Inline:`module()`/`mod::ule()`
      - From a file: `acquire()`
  - The search path:
      - Attach a module: `use()`
      - Detach a module: `drop()`
  - Inside a module:
      - Declare public objects: `provide()`
      - Attach a package locally: `require()`
      - Copy objects from another module: `refer()`
      - Name the module: `name()`

## Examples

Define an inline module:

``` r
my <- module({
        a <- 1
        b <- 2
        f <- function(x, y) x + y
})
```

The resulting module contains the objects defined within.

``` r
ls(my)
#> [1] "a" "b" "f"
```

Subset the module to access its objects.

``` r
my$a
#> [1] 1
my$b
#> [1] 2
my$f(my$a, my$b)
#> [1] 3
```

Use `with()` to aceess the objects with their bare names.

``` r
with(my, 
     f(a,b))
#> [1] 3
```

### Attach a Module to the Search Path

Just like a package, a module can be attached to the search path.

``` r
use(my)
```

The `my` module is attached to the search path as “module:my”.

``` r
search()
#>  [1] ".GlobalEnv"        "module:my"         "package:mod"      
#>  [4] "package:stats"     "package:graphics"  "package:grDevices"
#>  [7] "package:utils"     "package:datasets"  "package:methods"  
#> [10] "Autoloads"         "package:base"
```

And you can use the objects inside directly, just like those from a
package.

``` r
f(a,b)
#> [1] 3
```

Detach the module from the search path when done, if desired.

``` r
drop("my")
```

### Make objects Available to another Module

Use `refer()` to “copy” objects from another module. In the following
example, we create a new module `my_other` that uses the objects from
`my`, which is previsouly defined.

``` r
ls(my)
#> [1] "a" "b" "f"

my_other<- module({
        refer(my)
        c <- 4
        d <- 5
        f <- function() print("foo") 
})

ls(my_other)
#> [1] "a" "b" "c" "d" "f"
my_other$f()
#> [1] "foo"
```

In addition to its own objects, `my_other` module has all objects from
`my`, except `f`: because `my_other` module also has a `f` object, and
replaces the `f` from `my` module.

We can re-define `my_other` and prepend objects from `my` with *my*.
This way, both `f`s are available.

``` r
my_other <- module({
        refer(my, prefix = .)
        c <- 4
        d <- 5
        f <- function() print("foo") 
})

ls(my_other)
#> [1] "c"    "d"    "f"    "my.a" "my.b" "my.f"

my_other$my.f(1, 2)
#> [1] 3
my_other$f()
#> [1] "foo"
```

### Use a package

The `mod` package provides a `require()` function. `mod:::require()`
works in the same manner as do `base::require()`, but makes a packages
available for use in its containing module only.

``` r
mpg_analysis <- module({
    require(ggplot2)
    plot <- qplot(mtcars$mpg)    
})
#> Registered S3 methods overwritten by 'ggplot2':
#>   method         from 
#>   [.quosures     rlang
#>   c.quosures     rlang
#>   print.quosures rlang
mpg_analysis$plot
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="100%" />

Meanwhile, the global search path remain unaffected, not containing the
`ggplot2` package:

``` r
"package:ggplot2" %in% search()
#> [1] FALSE
```

## Notes

#### Environment

A module *is* an environment. This means that every rule that applies to
environments, such as copy-by-reference, applies to modules as well.

``` r
mode(my)
#> [1] "environment"
is.environment(my)
#> [1] TRUE
```

#### Terms

Some may wonder the choice of terms. Why `refer()` and `provide()`?
Further, why not `import()` and `export()`? This is because we feel
*import* and *export* are used too commonly, in both R, and other
popular languages with varying meanings. The popular
[`reticulate`](https://CRAN.R-project.org/package=reticulate) package
also uses `import()`. To avoid confusion, we decided to introduce some
synonyms. With analogous semantics,
[`refer()`](https://clojuredocs.org/clojure.core/refer) is borrowed from
Clojure, while
[`provide()`](https://docs.racket-lang.org/reference/require.html?q=provide#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._provide%29%29)
from Racket; Both languages are R’s close relatives.

#### Locked

A module is locked by default. It is impossible to either change the
value of a object or add a new object to a module.

``` r
my$a <- 1
#> Error in my$a <- 1: cannot change value of locked binding for 'a'
my$new_var <- 1
#> Error in my$new_var <- 1: cannot add bindings to a locked environment
```

#### Hidden Objects

As a general R rule, names that start with `.` define hidden objects.

``` r
my_yet_another <- module({
        .var <- "I'm hidden!"
})
```

Hidden objects are not returned by `ls()`, unless specified.

``` r
ls(my_yet_another)
#> character(0)
ls(my_yet_another, all.names = TRUE)
#> [1] ".var"
```

Nonetheless, in a module, they are treated the same as public objects.

``` r
my_yet_another$.var
#> [1] "I'm hidden!"
```

#### Load/Attach from File

``` r
module_path <- system.file("misc/example_module.R", package = "mod")
```

To load and assign to object:

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
ls("module:example_module")
#> [1] "a" "d" "e"
a
#> [1] 1
d()
#> [1] 6
e(100)
#> [1] 106
```

#### Modules and Packages

As it could be confusing how to deal with modules and packages, the
following clarification is made:

  - Attach a Package
      - To the local “search path”, at module context: `require()`
      - To the global search path, at global environment: `require()`
  - Attach a Module
      - To another module’s local “search path”: not available\*
      - To the global search path: `use()`
  - Copy Objects from a Module
      - To another module: `refer()`
      - To the global environment: not available\*\*
  - Use Modules inside a Package?
      - Yes, the package must `Depends` or `Imports` the `mod` package

\*: Modules cannot be attached to another module’s “seach path”. Use
`refer()` instead to make clearly visible objects in the module context.
\*\*: Objects cannot be batch-copied from a module to the global
environment, use `use()` to attach the module to the global search path
in order to use them directly.

These two features seek to avoid one very important problem `mod`
packages intends to solve: conflicting names.

#### Unattached

As aforementioned, the package is designed to be usable both attached
and unattached.

If you use the package unattached, you must always qualify the object
name with `::`, such as `mod::ule()`, a shorthand for `mod::module()`.
However, while inside a module, the `mod` package is always available,
so you do not need to use `::`. Note that in the following example,
`provide()` inside the module expression is unqualified.

See:

``` r
detach("package:mod")

my_mind <- mod::ule({
  provide(good_thought)
  good_thought <- "I love working on this package!"
  bad_thought <- "I worry that no one will appreciate it."
})

mod::use(my_mind)
good_thought
#> [1] "I love working on this package!"
```

##
