#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

;------------------------------------------------------------------------------------------

;; The maze structure will be based on N by M
(struct maze (N M tbl))

;; Manages cell properties
(define (connections tbl c) (dict-ref tbl c '()))

;; Overwriting any existing mapping for key
(define (connect! tbl c n) 
  (dict-set! tbl c (cons n (connections tbl c)))
  (dict-set! tbl n (cons c (connections tbl n))))
(define (connected? tbl a b) (member a (connections tbl b)))

;------------------------------------------------------------------------------------------

;; Returns a maze of a given size
(define (build-maze N M)
  (define tbl (make-hash))
  (define (visited? tbl c) (dict-has-key? tbl c))
  (define (neigbours c)
    (filter 
     (match-lambda [(list i j) (and (<= 0 i (- N 1)) (<= 0 j (- M 1)))])
     (for/list ([d '((0 1) (0 -1) (-1 0) (1 0))]) (map + c d))))
  ; generates the maze
  (let move-to-cell ([c (list (random N) (random M))])
    (for ([n (shuffle (neigbours c))] #:unless (visited? tbl n))
      (connect! tbl c n)
      (move-to-cell n)))
  ; return the result
  (maze N M tbl))

;------------------------------------------------------------------------------------------

;; Configuration
(define X 5)
(define Y 5)
(define  start  '(0 0))

;; Maze algorithm with X and Y = M and N
(define m (build-maze X Y))

;; Available directions
(define (paths start)
  (match-define (maze N M tbl) m)
  (map (lambda (x)
         (let ((first (map = start x))
               (second (map < start x)))
           (cond [(car first)
                  (if (cadr second) 'south 'north )]
                 [else
                  (if (car second) 'east 'west )])))
       (connections tbl start)))

;------------------------------------------------------------------------------------------

;; Directions 
(define (move-x room fun)
  (cons (car room) (map (lambda(x) (fun x 1)) (cdr room))))

(define (move-y room fun)
  (cons (fun (car room) 1) (cdr room)))

(define (lookup room direction)
  (cond [(eq? direction 'south)
         (move-x room +)]
        [(eq? direction 'north)
         (move-x room -)]
        [(eq? direction 'west)
         (move-y room -)]
        [(eq? direction 'east)
         (move-y room +)]))

;------------------------------------------------------------------------------------------

;; Start Game
(define (startgame room-id)
  (let loop ((rid room-id))
    (show-maze m rid)
    (printf "You have entered the ~a\n>" (hash-ref rooms rid))
    (let ((input (read)))
      (cond [(eq? input 'quit) (exit)])
      (if (member input (paths  rid))
          (let ((direction (lookup rid input)))
            (cond ((equal? rid direction) (loop rid))
                   ((equal? direction (list (- X 1)(- Y 1)))
                    (show-maze m direction)
                    (displayln "You have reached the end, Congratz!")
                    (exit))
                   (else
                    (loop direction))))
          (begin
            (printf "errm... what do you mean by ~a\n" input)
            (loop rid))))))

;------------------------------------------------------------------------------------------

;; Rooms 
(define room-type '((0 "Entrance")
                    (1 "hall")
                    (2 "hallway")
                    (3 "corridor")
                    (4 "lobby")
                    (5 "hallway")
                    (6 "court")
                    (7 "pass")))

(define (assq-ref assqlist id)
  (cadr (assq id assqlist)))

(define rooms (make-hash))

(define (room-allocator db types)
  (for ((j X))
    (for ((i Y))
      (hash-set! db (list j i) (assq-ref types (random (- (length types) 1)))))))

(room-allocator rooms room-type)

;------------------------------------------------------------------------------------------

;; Shows  maze  with  position
(define (show-maze m pos)
  (match-define (maze X Y tbl) m)
  (for ([i X]) (display "+---"))
  (displayln "+")
  (for ([j Y])
    (display "|")
    (for ([i (- X 0)])
      (if (equal? (list i j) pos)
          (display " *")
          (display "  "))
      (if (connected? tbl (list i j) (list (+ 1 i) j))
          (display "  ")
          (display " |")))
    (newline)
    (for ([i X])
      (if (connected? tbl (list i j) (list i (+ j 1)))
          (display "+   ")
          (display "+---")))
    (displayln "+")))

;------------------------------------------------------------------------------------------

;; Output
(startgame start)
