(cl:in-package :cl-user)

(cl:defpackage #:clicl-feebs
  (:nicknames #:cleebs)
  (:use #:cl #:alexandria #:clicl #:feebs)
  (:export #:publish-feeb-brain
           #:feeb-repl
           )) 
  
