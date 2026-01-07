#! /usr/local/bin/guile -s
!#
;;; SPDX-License-Identifier: BSD-3-Clause

;; Use current folder, can also use enviroment variables here for absolute
;; You can also provide root path to (configure), but "." is default anyway
(add-to-load-path ".")
(use-modules (buildlib))

;; Simple conditionals to check if one of the arguments was install or clean
;; You can do more complicated checks or argument parsing
(define install? (memq #t (map (lambda (x) (equal? "install" x)) (command-line))))
(define clean? (memq #t (map (lambda (x) (equal? "clean" x)) (command-line))))
(define compile? (not clean?))

;; Can also enable/disable cache, or increase number of compiler configs to keep
;; Note that each config keeps all of its objects, so too many might take up too much space
;;(cache #:enable #t #:keep-configs 3)

;; Can do multiple configurations and compilations
(configure #:exe-name "zzz-example"
           #:lib-name "libzzz-example" #:lib-source-dir "src/lib" #:lib-type 'both
           #:link '("m") ;; or if you want to specify '((static "m"))
           ;; Other linking options are #:link-path '(pathes-to-libs) #:include '(pathes-to-includes)
           #:derive '(XXX))

(compile-c compile?)

(install install?)

(clean clean?)
