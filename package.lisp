(defpackage #:cl-stringr
  (:use #:cl)
  (:export
   ;; Basics
   #:str-length
   #:str-sub
   #:str-c
   #:str-trim
   
   ;; Pattern Matching
   #:str-detect
   #:str-subset
   #:str-count
   #:str-extract
   #:str-extract-all
   
   ;; Mutation
   #:str-replace
   #:str-replace-all
   
   ;; Structure
   #:str-split
   #:str-join
   
   ;; Templates
   #:str-glue
   
   ;; DSL
   #:with-str-context
   #:glue
   #:map-str
   #:filter-str))

(defpackage #:cl-stringr-user
  (:use #:cl #:cl-stringr))
