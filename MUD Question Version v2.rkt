#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

;------------------------------------------------------------------------------------------

 ; MUD objects ;
(define objects '((1 "master sword")
                  (5 "fire key")
                  (7 "water stone")
                  (8 "heart container")))

 ; MUD locations ;
(define descriptions '((1 "You are in the lobby, an old man stares at you.")
                       (2 "You are now outside, the smell of fresh air is so refreshing!")
                       (4 "You've stubbled into a forest, it looks easy to get lost.")
                       (9 "Rats! a dead end. It's best to go back to way you came in.")
                       (10 "Rats! a dead end. It's best to go back to way you came in.")
                       (5 "You've located the grass temple and find yourself inside it.")
                       (6 "You find a mountainside, the gates ahead seem to require a key.")
                       (7 "You enter the fire temple. Tt's very hot here, better not stay long.")
                       (3 "You've struck a riverside. There is an odd keystone, something must go here.")
                       (8 "You beat the final temple, here is a piece of heart as your reward!")))

 ; MUD user options ;
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick) ((take) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop) ((use) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))

(define help '(((help) help)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))

(define actions `(,@look ,@pick ,@put ,@inventory ,@help ,@quit))

 ; MUD user decessions ;
(define decisiontable `((1 ((south) 2) ,@actions)
                        (2 ((north) 1) ((east) 3) ((south) 6) ((west) 4) ,@actions)
                        (4 ((north) 9) ((east) 2) ((south) 5) ((west) 10) ,@actions)
                        (9 ((south) 4) ,@actions)
                        (10 ((east) 4) ,@actions)
                        (5 ((north) 4) ((a teleporter) 1) ,@actions)
                        (6 ((north) 2) ((south) 6) ,@actions)
                        (7 ((north) 6) ((a teleporter) 1) ,@actions)
                        (3 ((south) 3) ((west) 2) ,@actions)
                        (8 ((north) 3) ((a teleporter) 1) ,@actions)))

;------------------------------------------------------------------------------------------

 ; Creates object database
(define objectdb (make-hash))

 ; Creates inventory database
(define inventorydb (make-hash))

 ; Available directions ;
(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "This room doesn't seem to have any available exits.\n"))
            ((= 1 n)
             (printf "There is only one exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "There are a multiple exits detected, they are to the ~a.\n" (string-join lostr " and "))))))))

;------------------------------------------------------------------------------------------

 ; Adding object 
(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

(define (add-objects db)
  (for-each
   (lambda (r) 
     (add-object db (first r) (second r))) objects))
 
(add-objects objectdb) ;adds object to database

 ; Displays object 
(define (display-objects db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'bag)
            (printf "You are holding a ~a.\n" output)
            (printf "There is a ~a infront of you.\n" output))))))

 ; Removes object (from room)
(define (remove-object-from-room db id str)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item) 
             (printf "That item is not in this room.\n"))
            (else
             (printf "You've picked up the a ~a.\n" (first item))            
             (add-object inventorydb 'bag (first item))      
             ;item: heart container event
             (if (eq? (first item) "heart container")
                (begin
                  (printf "Congratulations! you've uncovered the temples secrects!\n")
                  (exit))
             (hash-set! db id result)))))))

 ; Removes object (from inventory)
(define (remove-object-from-inventory db id str)
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "You are not carrying that item!\n"))
            (else
             (printf "You used the ~a from your bag.\n" (first item))
             (add-object objectdb id (first item))             
             ;item: fire key event
             (if (eq? (first item) "fire key")
                (begin
                  (printf "You've unlocked the fire temple doors!\n")
                  (startgame 7))
             ;item: water stone event
             (if (eq? (first item) "water stone")
                (begin
                  (printf "You've unlocked the water temple doors!\n")
                  (startgame 8))
             (hash-set! db 'bag result))))))))

 ; Pick-up user item
(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))

 ; Put-down user item
(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))

 ; Displays user help message 
(define (display-help)
  (printf "     TIP: Hey listen! Find all temples and uncover their secrects! (Grass> Fire> Water)
COMMANDS: - input 'look' : will list all available directions from your position.
          - input 'pick' : allows you to obtain an item and stores it in your inventory. 
          - input 'put'  : allows you to put items down or into another object.
          - input 'bag'  : will display all the items you currently have stashed. 
          - input 'exit' : quits the game at anytime. (WARNING: all progess will be lost)\n"))

 ; Displays user inventory
(define (display-inventory)
  (display-objects inventorydb 'bag))

 ; Joins parameter to list of atoms
(define (slist->string l)
  (string-join (map symbol->string l)))

;------------------------------------------------------------------------------------------

 ; Refactored this code:
(define (ass-ref assqlist id function)
  (cdr (function id assqlist)))

#| (define (assv-ref assqlist id) |#
#|  (cdr (assv id assqlist)))     |#

(define (get-description id)
  (car (ass-ref descriptions id assq))) ;changes made to ass-ref

(define (get-keywords id)
  (let ((keys (ass-ref decisiontable id assq))) ;changes made to ass-ref
    (map (lambda (key) (car key)) keys)))

;------------------------------------------------------------------------------------------

 ; Outputs a list
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))

(define (lookup id tokens)
  (let* ((record (ass-ref decisiontable id assv)) ;changed to ass-ref 
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))

(define (display-description id)
  (printf "~a\n" (get-description id)))

;------------------------------------------------------------------------------------------

 ; Start Game ;
(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (when description
      (display-description id)
      (display-objects objectdb id))
    (printf "> ")
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (printf "Invalid input\n")
               (loop id #f))
              ((eq? response 'look)
               (get-directions id)
               (loop id #t))
              ((eq? response 'pick)
               (pick-item id input)
               (loop id #f))
              ((eq? response 'drop)
               (put-item id input)
               (loop id #f))
              ((eq? response 'inventory)
               (display-inventory)
               (loop id #f))
              ((eq? response 'help) ;extra function added
               (display-help)
               (loop id #f)) 
              ((eq? response 'quit)
               (printf "Thanks for playing!\n")
               (exit)))))))

;------------------------------------------------------------------------------------------

 ; Output
(startgame 1)