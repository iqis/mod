# modular
  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/iqis/modular?branch=master&svg=true)](https://ci.appveyor.com/project/iqis/modular)
  [![Travis build status](https://travis-ci.org/iqis/modular.svg?branch=master)](https://travis-ci.org/iqis/modular)


R is a language ..

A module is simply an R file
One R file can only host one module

A module can be read into the session as a `module` object via `acquire()`
A module, whether an R file, or an object, can be attached to the search path via `use()`

Detach a module with `drop()`, which masks `base::drop()`
