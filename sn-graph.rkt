(module sn-graph racket
  (provide sn-consistent
           sn-empty
           sn-add-user
           sn-users
           sn-add-frndshp
           )

  ;; required libraries. (imported above)
  ;;(require racket/dict)
  ;;(require racket/set)

  
  ; Hard
  (define (sn-consistent p) #t)

  
  ;; graph
  ;; -> [a]
  ;; Easy (+0.5)
  ;; Defines an empty list for the social network
  (define sn-empty (list))

  
  ;; Easy
  ;; [(k,v)] -> [u]
  ;; Returns a list of all the users in the social network dictionary
  (define (sn-users graph)
    (if (empty? graph)
        '()
        (cons (car (car graph)) (sn-users (cdr graph)))))
  
  
  ;; Hard
  ;; [(k,v)] u -> [(k,v)] | (u,{})
  ;; Checks if a user is in a social network, if they're not, it adds them to the network.
  (define (sn-add-user graph user)
    (if (andmap  ;; andmap every for every user in social network, if user exists, then return true, else false
         (lambda (user)
           (equal? user (car (car graph)))) ;; check if user equal to the firts element of a graph
         graph)
        graph ;; if true, return sn
        (cons (list user) graph))) ;; else append user to sn


  ;; Hard
  ;; [(k,v)]|(u1,f1)|(u2,f2) ->
  ;;  [(k,v)] | (u1,f1+{f2}) | (u2,f2+{f1})
  ;; If (car (car graph)) equal to user1 -> (cons user2 (cdr (car graph)))
  ;; Else (cons (car graph) (sn-add-frndshp (cdr graph) user1 user2))
  (define (sn-add-frndshp graph u1 u2)
    (cond
      [(empty? graph)
       '()]
      [(equal? (car (car graph)) u1)
       ;; if key is equal to user 1
       ;; Add user 2 to values
       (cons (append (car graph) (list u2)) (sn-add-frndshp (cdr graph) u1 u2))]
      
      [(equal? (car (car graph)) u2)
       (cons (append (car graph) (list u1)) (sn-add-frndshp (cdr graph) u1 u2))]
      ;; If key is equal to user 2
      ;; Add user 1 to values
      [else
       ;; Else add previous parts of graph
       (cons (car graph) (sn-add-frndshp (cdr graph) u1 u2))]
      ))


  )

