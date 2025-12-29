# build.scm

No dependency build system script for C.

Currently work in progress.

## Use

Copy `buildlib.scm` into root folder. 
Then create a `build.scm` file with the following base:

``` scheme
(add-to-load-path ".")
(use-modules (buildlib))

;; (disable-default-failure) if you want to handle errors yourself

(configure)

(compile-c)

;; (install) not yet supported
```

Example options for `configure` are: `(configure #:c-compiler "cc" #:source-dir "src" #:build-dir "build" #:optimization "-O0")`

More can be seen in the `buildlib.scm` file.

After that, if you have Guile Scheme installed, you can simply execute it with `guile build.scm`.

Compatibility with other Schemes is dubious.

For Guile, you can also copy the script or compile it to your site folder 
(see `(display %load-path)` for script and `(display %load-compiled-path)` for compiled `.go` files). 
This change would allow you to get rid of `(add-to-load-path ".")`.
However, note that copying the script directly makes it easier to modify and distribute.
