## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


- __Examples for Unexported Functions__
  - All unexported functions are referred in the examples with `mod:::` qualification.
  - `require()`, `provide()` and `refer()` are not exported, buts still work without `mod:::`. This can only happen inside a module context defined by `module()`, because they are included in one of a module's enclosing environments. The examples included run with no error, are true to the user experience. 
  - Therefore, is there a way to get around the warnings? 
  
- __`\value` in .Rd files__
  - All functions with return value have `\value` in their respective .Rd files
  
- __Appropriate Use of `\dontrun{}`__
  - All examples do not contain `\dontrun{}`.
  
- __`installed.packages()`__
  - All calls to `installed.packages()` removed 
