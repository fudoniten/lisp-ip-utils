;;;; package.lisp

(defpackage #:ip-utils
  (:use #:cl)

  (:import-from #:cl-ppcre
                #:regex-replace
                #:split)

  (:import-from #:trivia
                #:match))
