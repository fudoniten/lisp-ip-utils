;;;; package.lisp

(defpackage #:ip-utils
  (:use #:cl)

  (:import-from #:cl-ppcre
                #:regex-replace
                #:split)

  (:import-from #:split-sequence
                #:split-sequence)

  (:import-from #:trivia
                #:match)

  (:export      #:ipv4
                #:ipv6
                #:ipv4-p
                #:ipv6-p
                #:v4-network
                #:v6-network
                #:network-min-ip
                #:network-max-up
                #:ip-on-network-p))
