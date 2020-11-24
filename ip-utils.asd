;;;; ip-utils.asd

(asdf:defsystem #:ip-utils
  :description "Describe ip-utils here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-ppcre
               :split-sequence
               :trivia)
  :components ((:file "package")
               (:file "ip-utils")))
