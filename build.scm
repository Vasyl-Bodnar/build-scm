#! /usr/local/bin/guile -s
!#

(define root (dirname (canonicalize-path (current-filename))))

(add-to-load-path root)
(use-modules (buildlib))

(disable-default-failure)

(configure #:root root #:exe-name "zzz-example")

(compile-c)

;;(install) We do not want to install an example

(clean (memq #t (map (lambda (x) (equal? "clean" x))
                     (command-line))))
