;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
(define-module (buildlib)
  #:autoload (ice-9 optargs) (define*)
  #:autoload (ice-9 ftw) (nftw)
  #:export (configure compile-c install clean disable-default-failure))

(define *c-compiler* #f)
(define *source-directory* #f)
(define *build-directory* #f)
(define *obj-build-directory* #f)
(define *executable* #f)
;; Libraries are WIP
(define *library* #f)
(define *library-type* #f)
(define *extra-args* #f)

(define *root* #f)
(define *metadata* #f)

(define fss file-name-separator-string)

(define *fatal-fail* #t)

(define (info . strs)
  (display (apply string-append strs))
  (newline)
  #t)

(define (warn . strs)
  (display (apply string-append (cons "WARNING: " strs)))
  (newline)
  #t)

(define (fail . strs)
  (display (apply string-append (cons "FAILURE: " strs)))
  (newline)
  (if (eq? *fatal-fail* 'ok)
      (set! *fatal-fail* 'fail)
      (if *fatal-fail* (set! *fatal-fail* #f)))
  #f)

(define (return-fail)
  (if *fatal-fail*
      *fatal-fail*
      (exit #f)))

(define (check-fail)
  (and (return-fail) (not (equal? return-fail 'fail))))

(define (disable-default-failure)
  (set! *fatal-fail* 'ok))

;; lib-type is 'none 'static or 'dynamic
(define* (configure #:key (c-compiler "cc") (root (current-filename))
                    (exe-name "a") (lib-name "a") (lib-type 'none)
                    (source-dir "src") (build-dir "build") (obj-dir "obj")
                    (optimization "-O0") (debug "") (wall ""))
  (set! *root* (canonicalize-path root))
  (set! *c-compiler* c-compiler)
  (set! *source-directory* (string-append *root* fss source-dir))
  (set! *build-directory* (string-append *root* fss build-dir))
  (set! *obj-build-directory* (string-append *build-directory* fss obj-dir))
  (set! *executable* (string-append *build-directory* fss exe-name))
  (set! *extra-args*
        (string-append optimization
                       (if (equal? debug "") debug (string-append " " debug))
                       (if (equal? wall "") wall (string-append " " wall))))
  (set! *metadata* (string-append *build-directory* fss ".metadata"))
  (unless (equal? lib-type 'none)
    (begin
      (set! *library* (string-append *build-directory* fss lib-name))
      (set! *library-type* lib-type)))
  (unless (system* *c-compiler* "--version")
    (fail "Could not find a c compiler: " *c-compiler*))
  (unless (let ((st (stat *source-directory* #f)))
            (and st (eq? (stat:type st) 'directory)))
    (fail "Could not find a source directory named " *source-directory*))
  (unless (let ((st (stat *build-directory* #f)))
            (and st (eq? (stat:type st) 'directory)))
    (begin
      (warn "Could not find a build directory named " *build-directory*)
      (info "Creating the build directory")
      (mkdir *build-directory*)))
  (unless (let ((st (stat *obj-build-directory* #f)))
            (and st (eq? (stat:type st) 'directory)))
    (begin
      (warn "Could not find an object directory named " *obj-build-directory*)
      (info "Creating the obj directory")
      (mkdir *obj-build-directory*)))
  (unless (file-exists? *metadata*)
    (begin
      (warn "Could not find metadata")
      (info "Creating new metadata")
      (let ((file (open *metadata* O_CREAT)))
        (close file))))
  (return-fail))

;; TODO: HASH
(define (check-cache src obj)
  (let ((file-hash ""))
    (and (file-exists? obj)
         (call-with-input-file *metadata*
           (lambda (port)
             #f)))))

(define (compile-c-object-file src obj)
  (info "Compiling " (basename src))
  (let ((result (status:exit-val (system* *c-compiler* *extra-args* "-c" src "-o" obj))))
    (if (= result 0) #t
        (fail "Could not compile a file: " (basename src) "\nReturned " (number->string result)))))

(define (link-exe objs)
  (info "Linking an executable")
  (let ((result (status:exit-val (apply system* (cons *c-compiler* (append objs (list "-o" *executable*)))))))
    (if (= result 0) #t (fail "Could not link an executable\nReturned " (number->string result)))))

;; TODO: HASH
(define (hash-inputs objs)
  (info "Hashing sources"))

;; TODO: Currently no threading, futures should make it easy enough
(define* (compile-c #:key (num-threads #f))
  (unless (and *c-compiler*
               *source-directory*
               *build-directory*
               *obj-build-directory*
               *executable*
               *extra-args*)
    (fail "Must run (configure) first"))
  (let ((objs '()))
    (nftw
     *source-directory*
     (lambda (filename statinfo flag base level)
       (case flag
         ((regular)
          (unless (equal? (basename filename) (basename filename ".c"))
            (let ((obj-filename (string-append *obj-build-directory* fss (basename filename ".c") ".o")))
              (set! objs (cons obj-filename objs))
              (if (check-cache filename obj-filename)
                  (info "Already compiled, skipping " (basename filename))
                  (compile-c-object-file filename obj-filename)))))
         ((directory) (info "Entering a directory: " (basename filename)))
         ((invalid-stat) (fail "Could not stat a file: " (basename filename)))
         ((directory-not-readable) (fail "Directory is not readable: " (basename filename)))
         ((stale-symlink) (fail "Could not follow a symlink: " (basename filename))))))
    (if (check-fail) (link-exe objs))
    (if (check-fail) (hash-inputs objs)))
  (return-fail))

(define* (install #:key (prefix "/usr/local"))
  (info "Copying to local bin")
  (let ((result (status:exit-val (system* "sudo" "cp" *executable* (string-append prefix fss "bin" fss (basename *executable*))))))
    (unless (= result 0) (fail "Could not copy an executable\nReturned " (number->string result))))
  (return-fail))

;; This is a careful clean that only targets what it itself may have produced
;; If you truly need a nuclear option, it shall be provided
(define* (clean  #:optional (conditional #t) #:key (nuclear #f))
  (if conditional
      (begin
        (info "Cleaning builds")
        (if nuclear
            (let ((result (status:exit-val (system* "rm" "-r" *build-directory*))))
              (unless (= result 0) (fail "Could not delete the build directory\nReturned " (number->string result))))
            (begin
              (delete-file *executable*)
              (delete-file *metadata*)
              (nftw *obj-build-directory*
                    (lambda (filename statinfo flag base level)
                      (if (and (eq? flag 'regular) (not (equal? (basename filename) (basename filename ".o"))))
                          (begin (delete-file filename) #t)
                          #t)))
              (let ((result (status:exit-val (system* "rmdir" *obj-build-directory*))))
                (unless (= result 0) (fail "Could not delete the obj directory\nReturned " (number->string result))))
              (let ((result (status:exit-val (system* "rmdir" *build-directory*))))
                (unless (= result 0) (fail "Could not delete the build directory\nReturned " (number->string result))))))))
  (return-fail))
