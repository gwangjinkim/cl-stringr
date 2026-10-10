(defpackage #:cl-stringr
  (:use #:cl)
  (:nicknames #:stringr)
  (:export
   ;; Basics
   #:str-length
   #:str-sub
   #:str-c
   #:str-trim
   #:str-to-upper
   #:str-to-lower
   #:str-to-title
   #:str-to-sentence
   
   ;; Pattern modifiers and integration protocol
   #:regex #:fixed #:coll #:boundary #:str-escape #:str-equal
   #:pattern-object #:pattern-object-p #:pattern-kind #:pattern-values
   #:pattern-options #:pattern-names #:pattern-named-values
   #:pattern-scanner #:pattern-matches-p #:pattern-error #:pattern-warning

   ;; Pattern Matching
   #:str-starts #:str-ends #:str-which #:str-like #:str-ilike
   #:str-locate #:str-locate-all #:str-match #:str-match-all #:invert-match
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
