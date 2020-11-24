;;;; ip-utils.lisp

(in-package #:ip-utils)

(defun split-string (string &optional (char #\Space))
  (split-sequence:split-sequence char string))

(defclass ip ()
  ((ip-number :initarg :ip-number)))

(defclass ipv4 (ip)
  ())

(defclass ipv6 (ip)
  ())

(defclass v4-network (ip)
  ((significant-bits :initarg :significant-bits)))

(defclass v6-network (ip)
  ((significant-bits :initarg :significant-bits)))

(define-condition invalid-ip (error)
  ((ip  :initarg :ip)
   (msg :initarg :msg)))

(define-condition invalid-ipv4 (invalid-ip) ())

(define-condition invalid-ipv6 (invalid-ip) ())

(define-condition invalid-network (error)
  ((network :initarg :network)
   (msg     :initarg :msg)))

(define-condition invalid-v4-network (invalid-network) ())

(define-condition invalid-v6-network (invalid-network) ())

(defun ipv4-elements (str)
  (handler-case
      (mapcar #'parse-integer (split-string str #\.))
    (sb-int:simple-parse-error (_)
      (declare (ignorable _))
      (error 'invalid-ipv4 :ip str))))

(defun ipv4-p (str)
  (let ((iplst (ipv4-elements str)))
    (and (= (length iplst) 4)
         (every (lambda (n) (<= 0 n 255))
                iplst))))

(defun ipv4-number (str)
  (handler-case
      (let ((els (ipv4-elements str)))
        (loop for el in (reverse els)
              for i from 0 to (length els)
              sum (ash el (* i 8)) into total
              finally (return total)))
    (error (_)
      (declare (ignorable _))
      (error 'invalid-ipv4 :ip str))))

(defun ipv4 (str)
  (make-instance 'ipv4 :ip-number (ipv4-number str)))

(defun v4-network (str)
  (handler-case
      (let ((els (split "/" str)))
        (make-instance 'v4-network
                       :ip-number (ipv4-number (car els))
                       :significant-bits (parse-integer (cadr els))))
    (error (_)
      (declare (ignorable _))
      (error 'invalid-v4-network :network str))))

(defun ipv6-elements (str)
  (flet ((split-bytes (str) (split-string str #\:))
         (parse-hex (str) (if (equal str "") 0 (parse-integer str :radix 16))))
    (let ((parts (mapcar #'split-bytes (split "::"
                                              (regex-replace "::$"
                                                             str
                                                             "::0")))))
      (match parts
        ((list only) (mapcar #'parse-hex only))
        ((list first last) (let ((middle (make-list
                                          (- 8 (length first)
                                             (length last))
                                          :initial-element "0")))
                             (mapcar #'parse-hex (append first middle last))))
        (_ nil)))))

(defun ipv6-number (str)
  (handler-case
      (let ((els (ipv6-elements str)))
        (if (/= 8 (length els))
            nil
            (loop for el in (reverse els)
                  for i from 0 to (length els)
                  sum (ash el (* i 16)) into total
                  finally (return total))))
    (sb-int:simple-parse-error (_)
      (declare (ignorable _))
      (error 'invalid-ipv6 :ip str))))

(defun ipv6 (str)
  (make-instance 'ipv6 :ip-number (ipv6-number str)))

(defun v6-network (str)
  (handler-case
      (let ((els (split "/" str)))
        (make-instance 'v6-network
                       :ip-number (ipv6-number (car els))
                       :significant-bits (parse-integer (cadr els))))
    (error (_)
      (declare (ignorable _))
      (error 'invalid-v6-network :network str))))

(defun ipv6-p (str)
  (handler-case
      (ipv6 str)
    (invalid-ipv6 (e)
      (declare (ignorable e))
      nil)))

(defgeneric network-min-ip (network)
  (:documentation "Return the first ip in the given network range."))

(defgeneric network-max-ip (network)
  (:documentation "Return the last ip in the given network range."))

(defmethod network-min-ip ((network v4-network))
  (with-slots (ip-number significant-bits) network
    (make-instance 'ipv4
                   :ip-number (ash (ash ip-number (- significant-bits 32))
                                   (- 32 significant-bits)))))

(defmethod network-max-ip ((network v4-network))
  (with-slots (ip-number significant-bits) network
    (with-slots ((min-ip-number ip-number)) (network-min-ip network)
      (make-instance 'ipv4
                     :ip-number (+ min-ip-number
                                   (- (expt 2 (- 32 significant-bits)) 1))))))

(defmethod network-min-ip ((network v6-network))
  (with-slots (ip-number significant-bits) network
    (make-instance 'ipv4
                   :ip-number (ash (ash ip-number (- significant-bits 128))
                                   (- 128 significant-bits)))))

(defmethod network-max-ip ((network v6-network))
  (with-slots (ip-number significant-bits) network
    (with-slots ((min-ip-number ip-number)) (network-min-ip network)
      (make-instance 'ipv6
                     :ip-number (+ min-ip-number
                                   (- (expt 2 (- 64 significant-bits)) 1))))))

(defgeneric ip-on-network-p (ip network)
  (:documentation "Check whether an IP is within a given network range."))

(defun ip-on-network-helper (ip network)
  (with-slots ((min-ip-number ip-number)) (network-min-ip network)
    (with-slots ((max-ip-number ip-number)) (network-max-ip network)
        (with-slots (ip-number) ip
          (< min-ip-number ip-number max-ip-number)))))

(defmethod ip-on-network-p ((ip ipv4) (network v4-network))
  (ip-on-network-helper ip network))

(defmethod ip-on-network-p ((ip ipv6) (network v6-network))
  (ip-on-network-helper ip network))

(defmethod ip-on-network-p (ip network) nil)
