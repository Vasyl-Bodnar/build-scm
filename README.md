# build.scm
Build system library and script for C with no dependencies other than Guile and a few system commands.

Capable of caching and has plenty of options to tweak, while being kept simple to use.

Currently work in progress. 
It fulfills my usecases, 
but I make no promises for stability of the codebase ,
nor that it will work well for your usecase.

## Use
You can use this template on github, or clone the repo and reinit it to use it.

Otherwise for raw Quickstart:

Copy `buildlib.scm` into root folder. 
Then create a `build.scm` file with the following base:

``` scheme
#! /usr/local/bin/guile -s
!#

;; You can also use absolute path to root here, and in configure
(add-to-load-path ".")
(use-modules (buildlib))

(let ((config (configure #:exe-name "my-app"))) ;; and/or #:lib-name "my-lib", lots of other settings

  (compile-c config)

  (install config #f) ;; Second argument is an optional condition for all commands

  (clean config #f)
```

You can also find a slightly more involved example in the `build.scm`.
Otherwise you have the full power of Scheme to make it work for your usecase.

Note that `configure` has defaults across many parts, e.g. `#:c-compiler "cc" #:source-dir "src" #:build-dir "build" #:optimization "-O0"` and more.
If you do not find these appealing, do change them.

You can also check what config outputs with `(print-config config)`. Or just for errors with `(display (config-error config))`.
The config structure does not mutate for any of the fields, except for errors.

More information can be seen in the `buildlib.scm` file.

After that, if you have Guile Scheme installed, and the `build.scm` is executable, you can simply run it, `build.scm` or `./build.scm`.
Or, of course, `guile build.scm` in which case you don't need to make it executable nor add the shebang.

You can also copy the script or compile it to your site folder 
(see `(display %load-path)` for script and `(display %load-compiled-path)` for compiled files). 
This change would allow you to get rid of `(add-to-load-path)`.
However, note that copying the script directly makes it easier to modify for personal usage and distribute.

## Compatibility and Licensing
Compatibility with other Schemes is dubious and untested.

This is intended to work on *nix systems, others may or may not work.

Note that `buildlib.scm` is licensed under MPL version 2.0.

`build.scm` and code examples in `README` and `src` are licensed under BSD-3
