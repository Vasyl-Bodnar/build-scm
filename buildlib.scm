;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
(define-module (buildlib)
  #:autoload (ice-9 optargs) (define*)
  #:autoload (ice-9 ftw) (nftw scandir)
  #:export (configure compile-c install disable-default-failure))

(define *c-compiler* #f)
(define *source-directory* #f)
(define *build-directory* #f)
(define *obj-build-directory* #f)
(define *executable* #f)
(define *extra-args* #f)

(define *fatal-fail* #t)

(define (info . strs)
  (display (string-append (apply string-append strs) "\n"))
  #t)

(define (warn . strs)
  (display (string-append (apply string-append (cons "WARNING: " strs)) "\n"))
  #t)

(define (fail . strs)
  (display (string-append (apply string-append (cons "FAILURE: " strs)) "\n"))
  (if (eq? *fatal-fail* 'ok)
      (set! *fatal-fail* 'fail)
      (if *fatal-fail* (set! *fatal-fail* #f)))
  #f)

(define (check-fail)
  (if *fatal-fail*
      *fatal-fail*
      (exit #f)))

(define (disable-default-failure)
  (set! *fatal-fail* 'ok))

(define* (configure #:key (c-compiler "cc") (exe-name "a") (source-dir "src") (build-dir "build") (obj-dir "obj") (optimization "-O0") (debug "") (wall ""))
  (set! *c-compiler* c-compiler)
  (set! *source-directory* source-dir)
  (set! *build-directory* build-dir)
  (set! *obj-build-directory* (string-append build-dir file-name-separator-string obj-dir))
  (set! *executable* (string-append build-dir file-name-separator-string exe-name))
  (set! *extra-args* (string-append optimization
                                    (if (equal? debug "") debug (string-append " " debug))
                                    (if (equal? wall "") wall (string-append " " wall))))
  (unless (system* *c-compiler* "--version")
    (fail "Could not find a c compiler: " *c-compiler*))
  (unless (let ((st (stat *source-directory* #f)))
            (and st (eq? (stat:type st) 'directory)))
    (fail "Could not find a source directory named " *source-directory*))
  (unless (let ((st (stat *build-directory* #f)))
            (and st (eq? (stat:type st) 'directory)))
    (begin (warn "Could not find a build directory named " *build-directory*)
           (info "Creating the build directory")
           (mkdir *build-directory*)))
  (unless (let ((st (stat *obj-build-directory* #f)))
            (and st (eq? (stat:type st) 'directory)))
    (begin (warn "Could not find an object directory named " *obj-build-directory*)
           (info "Creating the obj directory")
           (mkdir *obj-build-directory*)))
  (check-fail))

(define* (compile-c #:key (num-threads #f))
  (unless (and *c-compiler* *source-directory* *build-directory* *extra-args*)
    (fail "Must run (configure) first"))
  (let ((objs '()))
    (nftw
     *source-directory*
     (lambda (filename statinfo flag base level)
       (case flag
         ((regular)
          (unless (equal? (basename filename) (basename filename ".c"))
            (let ((obj-filename (string-append *obj-build-directory* file-name-separator-string (basename filename ".c") ".o")))
              (set! objs (cons obj-filename objs))
              (if (file-exists? obj-filename) ;; TODO: This should probably be based on hash
                  (info "Already compiled, skipping " filename)
                  (begin
                    (info "Compiling " filename)
                    (let ((result (status:exit-val (system* *c-compiler* *extra-args* "-c" filename "-o" obj-filename))))
                      (if (= result 0) #t
                          (fail "Could not compile a file: " filename "\n" "Returned " (number->string result)))))))))
         ((directory) (info "Entering a directory: " filename))
         ((invalid-stat) (fail "Could not stat a file: " filename))
         ((directory-not-readable) (fail "Directory is not readable: " filename))
         ((stale-symlink) (fail "Could not follow a symlink: " filename)))))
    (if (check-fail)
        (begin
          (info "Linking an executable")
          (let ((result (status:exit-val (apply system* (cons *c-compiler* (append objs (list "-o" *executable*)))))))
            (if (= result 0) #t (fail "Could not link an executable\nReturned " (number->string result)))))))
  (check-fail))

;; Currently unimplemented
;;(define (install))
