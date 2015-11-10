
;;;
;;;cs230.ps3.scm
;;;
;;; For this problem set, when defining functions do not use types in 
;;; the lambda expressions. Instead, you should add a comment as to what 
;;; the type should be.

;;; Do not use (require racket/base) for this problem set.
;; ----- Useful functions -----

;; Define a predicate member? that returns #t if obj is a member of
;; lst and #f otherwise.

;; Contrast with the builtin member function, which returns the
;; sublist of lst starting with obj if obj is in the list.
(require racket/class)

(define member?
  (lambda (obj lst)
    (not (not (member obj lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;foldr and foldl are provided in scheme                                                                                                                                           
;;                                                                                                                                                                                 
;;(define accumulate                                                                                                                                                               
;; (lambda (initial op l)                                                                                                                                                          
;;    (cond ((null? l) initial)                                                                                                                                                    
;;      (else                                                                                                                                                                      
;;        (op (car l) (accumulate initial op (cdr l)))))))                                                                                                                         
;;                                                                                                                                                                                 
;;Note: (accumulate  '() cons '(1 2 3 4)) => '(1 2 3 4)
;;(define foldr (lambda (op init lst) (accumulate init op lst)))                                         
;;Note: (foldr cons '() '(1 2 3 4)) => '(1 2 3 4)                                                                              
;;Whereas: (foldl cons '() '(1 2 3 4) => '(4 3 2 1)  
;; ------ Data type definitions -----

;; Directed graph class definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(vertices <graph>) => list of vertices
;;(edges <graph>) => list of edges 
;;
(defclass <graph> ()
  (vertices :initarg :vertices :accessor vertices) 
  (edges :initarg :edges :accessor edges))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(name <vertex>) => the name of the vertex
;;ex: (name (make-vertex 'a)) => a
;;
(defclass <vertex> ()
  (name :initarg :name :accessor name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(start <directed-edge>) => first vertex of directed-edge
;;ex: (start (v1 v2)) => v1
;;(finish <directed-edge>) => second vertex of directed-edge
;;ex: (finish (v1 v2)) => v2
;;
(defclass <directed-edge> ()
  (start :initarg :start :accessor start)
  (finish :initarg :finish :accessor finish))

(define make-vertex
  (lambda (name)
    (make <vertex> :name name)))

(define make-edge
  (lambda (a b) ;a <vertex> b <vertex>
    (make <directed-edge> :start a :finish b)))

;; Two vertices are considered equal if their names are equal

(define equal-vertex?
  (lambda (v1 v2)
    (eq? (name v1) (name v2))))

;; lookup-vertex takes a name and a list of vertices, and finds a vertex
;; with that name.  Useful when you have the name of a vertex and need
;; the vertex itself.

(define lookup-vertex
  (lambda (vname vlist)
    (cond ((null? vlist) #f)
          ((equal? vname (name (first vlist))) (first vlist)) ;replaced car with first
          (else (lookup-vertex vname (rest vlist)))))) ;replaced cdr with rest

;; make-graph takes two lists whose atoms are symbols, one of the form
;;   (v1 v2 v3 ...) 
;; which becomes the list of vertices and the other of the form
;;   ((u1 u2) (u3 u4) ...) 
;; which becomes the list of edges.

(define make-graph
  (lambda (v-names e-list)
    (let* ((v (map make-vertex v-names))
           (create-edge 
              (lambda (name1 name2)
                (make-edge (lookup-vertex name1 v)
                           (lookup-vertex name2 v)))))
        (make <graph>
              :vertices v        
              :edges (map create-edge
                          (map first e-list)
                          (map second e-list))))))

;; Convert a list of vertices to a list of names of vertices

(define name-vertices
  (lambda (vlist)
    (map name vlist)))

;;;Same as standard member function but works with vertices
(define member-vertices 
  (lambda (a lat) 
    (cond ((null? lat) #f) 
          ((equal-vertex? a (car lat)) lat) 
          (else (member-vertices a (cdr lat))))))

;; Find the set difference of two sets represented as lists.  That is,
;; return a list consisting of everything in list1 that is not in
;; list2

(define set-diff-vertices
  (lambda (list1 list2)
    (cond ((null? list1) '()) 
          ((member-vertices (car list1) list2) (set-diff-vertices (cdr list1) list2))
          (#t (cons (car list1) (set-diff-vertices (cdr list1) list2))))))

;; Take the union of two sets represented as lists -- no duplicates

(define union
  (lambda (list1 list2)
    (cond ((null? list1) list2) 
          ((member (car list1) list2) (union (cdr list1) list2))
          (else (cons (car list1) (union (cdr list1) list2))))))
          

;; Take the intersection of two sets represented as lists 

(define intersection
  (lambda (list1 list2)
    (cond ((null? list1) '()) 
          ((member (car list1) list2) 
             (cons (car list1) (intersection (cdr list1) list2)))
          (else (intersection (cdr list1) list2)))))

;;; ----- TESTING EXAMPLES -----

;; ----- Problem 1 -----


;;ALEXA QUESTIONS
;;How do I access just the list of vertices?
;;How do I stop when I find the thing? 
;;Are there sets in racket?
;;Should finding the thing and 
;;Pseudocode
;;Iterate through the list of vertices until you find the right vertex
;;then go through the pairs and add the vertices to a list.
;;return the llist

;;car glist
;;


;;This returns the vertices that can be reached from a single list

(define g1 (make-graph '(a b c d e) 
		       '((a b) (a c) (b c) (b e) (c d) (d b))))

;;takes edges and the list that it will be appended to
;;if vert is the first thing, add to list and then move forward
;;else append without addding to list
(define exits
  (lambda (vert g)
    (letrec ((loop
              (lambda (lst1 edgez)
                (cond ((null? edgez) lst1)
                      ((equal? vert (start (car edgez)))
                       (loop (append lst1 (list (finish (car edgez)))) (cdr edgez)))
                      (else (loop lst1 (cdr edgez)))))))
      (loop '() (edges g) ))))


;(name-vertices (exits (lookup-vertex 'b (vertices g1)) g1))
;(name-vertices (exits (lookup-vertex 'e (vertices g1)) g1))



(define verify-path
  (lambda (g verts)
    (letrec ((loop
              (lambda (verts2 edgezz) ;verts2
                (cond ((null? edgezz) #t) ;;if at end, then yes, this will obvi reach end before verts2
                      ((empty? (filter
                                (lambda (v)
                                  (equal? v (car edgezz)))
                                (exits (car verts2) g))) #f)
                                ;(exits (lookup-vertex (car verts2) (vertices g)) g))) #f)
                      (else (loop (cdr verts2) (cdr edgezz)))))))
      (loop verts (cdr verts))))) 

;(verify-path g1 
;    (map (lambda (x) (lookup-vertex x (vertices g1))) '(a b c d b e)))

;(verify-path g1 
;    (map (lambda (x) (lookup-vertex x (vertices g1))) '(a b c d e)))


;(name-vertices (exits (lookup-vertex 'b (vertices g1)) g1))

;(verify-path g1 
   ;(map (lambda (x) (lookup-vertex x (vertices g1))) '(a b c d b e)))
; ==> #t
  
;(verify-path g1 
;   (map (lambda (x) (lookup-vertex x (vertices g1))) '(a b c d e)))
; ==> #f
  
;; ----- Problem 2 -----
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(label <labeled-edge>) => label of labeled-edge
;;ex: (label (make-labeled-edge a b l)) => l
;;
(defclass <labeled-edge> (<directed-edge>)
  (label :initarg :label :accessor label))

(define make-labeled-edge
  (lambda (a b l) ;a <vertex> b <vertex> l <obj>
    (make <labeled-edge> :start a :finish b :label l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(start-state <automaton>) => start-state of automaton
;;(final-states <automaton>) => list of final-states of automaton
;;    
(defclass <automaton> (<graph>)
  (start-state :initarg :start-state :accessor start-state)
  (final-states :initarg :final-states :accessor final-states)) ;start-state <symbol> final-states <list>
;;note: all accessors that apply to graph apply to automaton
  
;; make-automaton takes four parameters.  
;; The first is a list of symbols of the form (v1 v2 v3 ...) which 
;;   becomes the list of vertices.
;; The second is a list of triples of the form 
;;   ((u1 u2 l1) (u3 u4 l2) ...) which becomes the list of labeled
;;   edges (with the u's symbols which represent vertices and the l's 
;;   objects which become the labels).
;; The third is a single symbol for the start state.
;; The fourth is a list of symbols that represent final states.

(define make-automaton
  (lambda (v-names e-list s-state f-states) ;v-names <list>, e-list <list>, s-state <symbol>, f-states <list>
    (let* ((v (map make-vertex v-names))
           (create-labeled-edge 
              (lambda (name1 name2 label)
                (make-labeled-edge (lookup-vertex name1 v) 
                                   (lookup-vertex name2 v) 
                                   label))))
      (make <automaton>
            :vertices v
            :edges (map create-labeled-edge
                        (map first e-list)
                        (map second e-list)
                        (map third e-list))
            :start-state s-state
            :final-states f-states))))

;(define dfa1
;  (make-automaton '(a b c) 
;	    '((a a 0) (a b 1) (b a 1) (b c 0) (c b 0) (c c 1))
;            'a '(a)))

;;; ----- Problem 3 -----


;;DOUBLE FILTEEERRRRRRRRRRRRRR
(define find-sym
  (lambda (state nfa sym)
    (filter
     (lambda (edge)
       (equal? sym (label edge)))
     (filter (lambda (edge)
              (equal? state (name (start edge))))
            (edges nfa)))))

(define step-dfa
  (lambda (dfa state sym)
    (if (member? state (name-vertices (vertices dfa))) ;;if the state is in the thing to begin with
        (if (< 1 (length (find-sym state dfa sym)))
            #f
            (name (finish (car (find-sym state dfa sym)))))
        #f)))
             
(define dfa1
  (make-automaton '(a b c) 
                  '((a a 0) (a b 1) (b a 1) (b c 0) (c b 0) (c c 1))
                  'a 
                  '(a)))
         
;(step-dfa dfa1 'c 1) ;;==> c
;(step-dfa dfa1 'd 0); ==> #f
;(step-dfa dfa1 'a 0) ;==> a
; (step-dfa dfa1 'a 1); ==> b
; (step-dfa dfa1 'a 2) ==> #f

;(define bad-dfa
;  (make-automaton '(a b c) 
; 	    '((a a 0) (a b 0) (b a 1) (b c 0) (c b 0) (c c 1))
;            'a '(a)))

; (step-dfa bad-dfa 'a 0) ;==> #f

;; ----- Problem 4 -----

(define integer->binary
  (lambda (n)
    (cond ((eq? n 0) '())
	  (else (append (integer->binary (quotient n 2)) 
                        (list (if (even? n) 0 1)))))))

;;How do i get the lists of the states? without a loop?
;(define simulate-dfa
;  (lambda (auto input)
;    (cond ((null? input) (member? (step-dfa auto (start-state auto) (car go))))
;          ((equal? #f (step-dfa auto (start-state auto) (car go))) #f)
;          (else (simulate-dfa auto

;
;((empty? (filter
;                                (lambda (v)
;                                  (equal? v (car edgezz)))
;                                (exits (car verts2) g))) #f) 


(define simulate-dfa
 (lambda (auto input)
   (letrec ((loop (lambda (state go) ;;stop at end of go, which is input, input is symbols
                    (cond ((null? go) (member? state (final-states auto))) ;;if the ultimate state is in final states
                          ((equal? #f (step-dfa auto state (car go))) #f) ;;if step-dfa returns false, it ends
                          (else (loop (step-dfa auto state (car go)) (cdr go)))))));;if it isnt false, then it loops
     (loop (start-state auto) input))))

;(simulate-dfa dfa1 '(1 0 0 1)) ;;==> #t
;(simulate-dfa dfa1 '(1 0 1 1)); ==> #f


          
;(simulate-dfa dfa1 (integer->binary 12))
; (simulate-dfa dfa1 (integer->binary 10))

;; ----- Problem 5 -----

(define nfa1
  (make-automaton '(a b c d e)
	    '((a a 0) (a a 1) (a b 1) (a c 0) (b d 1) (c e 0)
	      (d d 0) (d d 1) (e e 0) (e e 1))
	    'a
	    '(d e)))

;;I now work
(define step-nfa
  (lambda (nfa states sym)
    (letrec ((loop
              (lambda (lst states2)
                (cond ((null? states2) lst)
                      ((if (equal? #f (stepper nfa (car states2) sym))
                           (loop lst (cdr states2))
                           (loop (append lst (stepper nfa (car states2) sym)) (cdr states2))))
                      (else (loop lst (cdr states2)))))))  
      (loop '() states))))
            
(define stepper
  (lambda (nfa state sym)
    (if (member? state (name-vertices (vertices nfa))) ;;if the state is in the thing to begin with
        (if (zero? (length (find-sym state nfa sym)))
            #f
            (map (lambda (x)
                   (name (finish x)))
                 (find-sym state nfa sym)))
            ;;(name (finish (car (find-sym state nfa sym))))
        #f)))

;(step-nfa nfa1 '(a c) 0) 
;;==> (a c e)
;(step-nfa nfa1 '(c e) 0)
;;==> (e e)
;(step-nfa nfa1 '(a) 0)
;;==> (a c)
;(step-nfa nfa1 '(a) 2)
;;==>f


(define in-fin
  (lambda (lst1 lst2)
    (cond ((null? lst1) #f)
          ((member? (car lst1) lst2) #t)
          (else (in-fin (cdr lst1) lst2)))))
           
;IIIIII WOOOOOOOOOOOOOORKKKKKKKKKKKKKKKKKK 
(define simulate-nfa
 (lambda (nfa input)
   (letrec ((loop (lambda (states go)
                     (cond ((null? go) (in-fin states (final-states nfa)))
                           ((equal? #f (step-nfa nfa states (car go))) #f)
                           (else (loop (step-nfa nfa states (car go))
                                 (cdr go)))))))
     (loop (step-nfa nfa (list(start-state nfa)) (car input)) (cdr input)))))
;
(define nfa1
  (make-automaton '(a b c d e)
	    '((a a 0) (a a 1) (a b 1) (a c 0) (b d 1) (c e 0)
	      (d d 0) (d d 1) (e e 0) (e e 1))
	    'a
	    '(d e)))
;
;
;(simulate-nfa nfa1 '(0 0 0))
;(simulate-nfa dfa1 '(1 0 0 1))
;(simulate-nfa dfa1 '(1 0 1 1))
;(simulate-nfa nfa1 '(1 1))
;; ----- Problem 6 -----


(define next-verts
   (lambda (lst graph end)
     (cond ((null? lst) '())
           ((member? end (name-vertices(exits (lookup-vertex (last-v lst) (vertices graph)) graph))) (append lst (list end)))
           (else (append lst (name-vertices(exits (lookup-vertex (last-v lst) (vertices graph)) graph)))))))

;;this works 
(define last-v
  (lambda (lst)
    (cond ((null? (cdr lst)) (car lst))
          (else (last-v (cdr lst))))))

;;Problem: I don't work if the thing is false, which is no bueno
(define path?
  (lambda (v1 v2 graph)
    (letrec ((loop
              (lambda (nextVs counter) ;;where we been and how many we been to
                (cond ((null? (vertices graph)) #f)
                      ((member? v2 nextVs) #t)
                      ((null? nextVs) #f)
                      ((equal? v1 v2) #t)
                      ((zero? counter)
                       (if (member? v2 nextVs)
                           #t
                           #f))
                      (else (loop (next-verts nextVs graph v2) (- counter 1)))))))
      (loop (list v1) (length (vertices graph))))))



(define g2 (make-graph '(a b c) '((a b) (b a) (a c) (c a) (b c))))
(define g3 (make-graph '(a b c d) '((a b) (b c) (a c) (c b) (d b))))
(define g4 (make-graph '(a b c d) '((a b) (a c) (b a) (c a) (a d) (b c) (c b))))


;(path? 'a 'e g1); ==> #t
;(path? 'd 'a g1);; ==> #f
;(path? 'a 'c g2);; ==> #t
;; (path? 'c 'b g2) ==> #t
;; (path? 'd 'd g3) ==> #t
; (path? 'a 'd g3) ;;==> #f
;; (path? 'b 'd g4) ==> #t

;; ----- Problem 7 -----
(defclass <vertex+parent> (<vertex>)
  (parent :initarg :parent :accessor parent))

(define make-vertex+parent
  (lambda (v p) ;v <vertex>, p <obj>                                                                                                                                               
    (make <vertex+parent> :name (name v) :parent p)))




;(name-vertices (exits (lookup-vertex 'b (vertices g1)) g1))


;(name-vertices(exits (lookup-vertex (last-v '(a)) (vertices g1)) g1))
;;for obvious reasons, this works if there is no path
;;Runs forever and crashes if there is a path





;;I WORK
  (define find-path
  (lambda (v1 v2 graph)
    (if (equal? #f (path? v1 v2 graph))
        #f
        (letrec ((loop
              (lambda (nextVs counter)
                (cond
                  ((member? v2 nextVs) nextVs)
                  ((zero? counter) nextVs)
                  ((equal? v1 v2) (append nextVs (list start))) ;;cons start onto nextVs
                  (else (loop (next-verts nextVs graph v2) (- counter 1)))))))
          (loop (list v1) (length (vertices graph)))))))
        
    
;(next-verts (list 'a) g1)
;;Works for false, doesnt work for literally anything else 
(find-path 'a 'e g1) ;;==> (a b e)
(find-path 'd 'a  g1)                   ;;==> #f
(find-path 'a 'c g2) ; ==> (a c)
(find-path 'c 'b g2) ; ==> (c a b)
(find-path 'd 'd g3); ==> (d)
(find-path 'a 'd g3)   ;                 ==> #f
; (name-vertices (find-path 'b 'd g4)) ; ==> (b a d)