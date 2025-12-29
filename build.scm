(add-to-load-path ".")
(use-modules (buildlib))

(disable-default-failure)

(configure)

(compile-c)
