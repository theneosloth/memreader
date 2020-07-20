;;;; memreader.asd

(asdf:defsystem #:memreader
    :description "Describe memreader here"
    :author "Your Name <your.name@example.com>"
    :license  "Specify license here"
    :version "0.0.1"
    :depends-on (#:cffi)
    :serial t
    :components ((:file "package")
                 (:file "memreader")))
