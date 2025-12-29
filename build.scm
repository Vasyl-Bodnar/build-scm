(add-to-load-path ".")
(use-modules (buildlib))

(disable-default-failure)

(configure #:exe-name "zaxia-text")

(compile-c)

;;(install)
