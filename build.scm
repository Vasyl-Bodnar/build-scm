#! /usr/local/bin/guile -s
!#
;;; SPDX-License-Identifier: BSD-3-Clause

;; Use current folder, can also use enviroment variables here for absolute
;; You can also provide root path to (configure), but "." is default anyway
(add-to-load-path ".")
(use-modules (buildlib))

;; Can also enable/disable cache, or increase number of configs to keep
;; Note that each config keeps all its objects, so too many, unless necessary, might take up too much space
;;(cache #:enable #t #:keep-configs 3)

;; Can do multiple configurations and compilations, use different names of course
(configure #:exe-name "zzz-example"
           #:lib-source-dir "src/lib" #:lib-name "libzzz-example" #:lib-type 'both
           #:derive '(XXX))

(compile-c)

;; Simple conditionals to check if one of the arguments was install or clean
(install (memq #t (map (lambda (x) (equal? "install" x))
                       (command-line))))

(clean (memq #t (map (lambda (x) (equal? "clean" x))
                     (command-line))))
