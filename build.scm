#! /usr/local/bin/guile -s
!#
;;; SPDX-License-Identifier: BSD-3-Clause

;; Use current folder, can also use enviroment variables here for absolute
;; You can also provide root path to configure, but "." is default anyway
(add-to-load-path ".")
(use-modules (buildlib))

(disable-default-failure)

(configure #:exe-name "zzz-example"
           #:lib-source-dir "src/lib" #:lib-name "libzzz-example" #:lib-type 'both
           #:derive '(XXX))

(compile-c)

(install (memq #t (map (lambda (x) (equal? "install" x))
                       (command-line))))

(clean (memq #t (map (lambda (x) (equal? "clean" x))
                     (command-line))))
