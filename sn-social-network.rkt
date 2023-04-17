(module sn-social-network racket

  (provide 
   sn-ff-for
   sn-cmn-frnds-btwn
   sn-cmn-frnds
   sn-frnd-cnt
   sn-frndlst-user
   sn-unfrndlst-user  )

  (require "sn-graph.rkt")
  (require "sn-utils.rkt")
  
  ;; social-network.
  ;; Easy
  ;; Returns the values associated with a key.
  ;; [(k,v)]| (u,vu) -> vu
  (define (sn-ff-for graph u1)
    (if (equal? (car (car graph)) u1)  ;; if the key of the first element is equal to the key
        (cdr (car graph))  ;; return the corresponding values for that key
        (sn-ff-for (cdr graph) u1)))  ;; recursively call the method with the rest of the graph


  ;; Medium
  ;; [(k,v)]|(u1,f1)|(u2,f2) ->
  ;; f2 & f3
  ;; Return the common friends of two users.
  ;; (filter friends present in both users)
  (define (sn-cmn-frnds-btwn graph u1 u2)
    (if (empty? (sn-ff-for graph u1))  ;; if user has no friends
        '()  ;; return empty list
        (filter
         (lambda (frnd) (member frnd (sn-ff-for graph u2)))  ;; filter out the members of u1 that are in u2
         (sn-ff-for graph u1))))  ;; filter friends of u1

  ;; Hard
  ;; Return a list of pairs of users and their friend count.
  ;; (cons key and length of values recursively)
  (define (sn-frnd-cnt graph)   
    (if (empty? graph)  ;; if graph is empty
        '()  ;; return an empty list
        (cons (cons (car (car graph)) (length (cdr (car graph))))  ;; cons the cons of key of the first element in graph and the number of values...
              (sn-frnd-cnt (cdr graph)))))  ;; ... with the recursively called rest of the graph

  ;; pre: length > 0
  ;; Find User with the greatest amount of friends in the dictionary, assuming the dictionary has at least one value.
  ;; (Apply > from left to right, finding greatest friend count in graph)
  (define (sn-frndlst-user op graph)
    (foldl (lambda (left-pair right-pair)  ;; apply fold left to right using lambda function
             (if (op (cdr left-pair) (cdr right-pair))  ;; if the value in left pair is greater than the value in the right pair
                 left-pair  ;; return left pair
                 right-pair))  ;; return right pair
           (car (sn-frnd-cnt graph))  ;; fold using car of friend count list
           (cdr (sn-frnd-cnt graph))))  ;; apply function to cdr of friend count list
              
          
  ;; pre: length > 0
  ;; Find User with the least amount of friends in the dictionary, assuming the dictionary has at least one value.
  ;; Call frndlst-user function because implementation is the same
  (define (sn-unfrndlst-user graph)
    (sn-frndlst-user < graph))


  ;; this is for free. Do not mdify (ROM)
  (define (sn-cmn-frnds-ff graph u)
    (let*
        ([keys (sn-users graph)]
         [vals (map
                (lambda (key)
                  (sn-cmn-frnds-btwn graph u key))
                keys)]
       
         )
      (sn-dict-ks-vs keys vals)))


  ;; this is for free. Do not mdify (ROM)
  (define (sn-cmn-frnds graph )
    (let*
        ([keys (sn-users graph)]
         [vals (map
                (lambda (key)
                  (sn-cmn-frnds-ff graph key))
                keys)]
         )
      (sn-dict-ks-vs keys vals)))

  )

