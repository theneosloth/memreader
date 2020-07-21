;;;; memreader.asd

(asdf:defsystem #:memreader
  :description "Describe memreader here"
  :author "Stefan Kuznetsov"
  :version "0.0.1"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "memreader")))
