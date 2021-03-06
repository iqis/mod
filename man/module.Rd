% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{module}
\alias{module}
\alias{ule}
\alias{acquire}
\title{Make a Module}
\usage{
module(expr, parent = parent.frame(), lock = TRUE)

ule(expr, parent = parent.frame(), lock = TRUE)

acquire(path, parent = baseenv(), lock = TRUE)
}
\arguments{
\item{expr}{module expression}

\item{parent}{the enclosing environment}

\item{lock}{lock the environment; logical}

\item{path}{path to a module file}
}
\value{
an \code{environment} of class \code{module} containing defined objects
}
\description{
Institute a module object inline or from a file.
mod::ule() is a useful shorthand for module() when this package is not attached.
}
\details{
Only use \code{lock = FALSE} for runtime debugging. It is otherwise necessary to keep the module locked.
}
\examples{

# from file
module_path <- system.file("misc", "example_module.R", package = "mod")
example_module <- acquire(module_path)

example_module$e(123)

# inline
my_module <- mod::ule({
    a <- 1
    .a <- 2
    f <- function(){.a}
})

my_module$a
my_module$f

}
\seealso{
\code{\link{use}}, \code{\link{drop}}
}
