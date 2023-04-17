(module sn-utils racket

  (provide sn-dict-ks-vs
           sn-line->entry
           sn-list->dict
           )

 
  ;; utils
  ;; [k] [v] -> [(k,v)]
  ;; Easy
  ;; Converts list of keys and values to a dictionary
  (define (sn-dict-ks-vs ks vs)  
    (cond
      [(and (empty? ks) (empty? vs))  ;; if both key and value is empty, return empty list
       '()]
      [else
       (cons
        (cons (car ks) (car vs))  ;; cons the cons of the first of keys and first of values ...
        (sn-dict-ks-vs (cdr ks) (cdr vs)))])) ;; ... and recursively cons the rest of the keys and values
  

  
  ;; Medium
  ;; str -> (a,[a])
  ;; split a line into an entry by converting each character to a symbol
  (define (sn-line->entry ln)
    (map (lambda (char)  ;; apply to split line
           (string->symbol char))  ;; convert char to symbol
         (string-split ln)))  ;; split line into characters

  
  ;; [(a,b)] -> [(a,b)] 
  ;; Easy
  ;; Converts a key value pair to a list
  (define (sn-list->dict es)
    es)  ;; return es as already a list
  )