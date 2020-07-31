;; This is the code for -- Stable Marriage
(define (match-make proposers proposees)
  (send proposers 'reset)
  (send proposees 'reset)
  (courtship proposers proposers proposees)
  (zip-together (send proposers 'name)
                (send (send proposers 'intended) 'name)))

;Q1,
;an implementation of the Stable Marriage algo- rithm described above, where a subset of the proposers
(define (courtship unengaged-proposers proposers proposees)  
   (if  (not (null? unengaged-proposers))
        (begin
           (send unengaged-proposers 'propose)
           (courtship (currently-unengaged proposers) proposers proposees))
        'done-proposing))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))


; a procedure returning a list of the people appearing as elements of ⟨list⟩ who are not engaged.
(define (currently-unengaged list-of-people)
  (if (null? list-of-people)
      '()
      (if (null? ((car list-of-people) 'intended))
          (cons (car list-of-people)
                (currently-unengaged (cdr list-of-people)))
          (currently-unengaged (cdr list-of-people)))))

;sendsagivenmessagetoeachpersoninthelist
(define (send list-of-people message)
  (map (lambda (person) (person message)) list-of-people))

;if the two people are on each other's intended list they couple
(define (couple? person1 person2)
  (cond ((not (eq? (person1 'intended) person2)) #f)
        ((not (eq? (person2 'intended) person1)) #f)
        (else #t)
     )
  )

;helper method to get rank of p2 in list1
(define n 1)
(define (getRank list1 p2)
    (cond ((null? list1) (write 'oops))
          (else
           (cond ((eq? (car list1) p2) n)
                 (else
                  (set! n (+ 1 n))
                  (getRank (cdr list1) p2))))))

;a predicate telling whether the first argument is liked more than the second argument by a particular person/procedure.  
(define (i-like-more? person1 person2)
  (lambda (person)
    (cond ((< (getRank (person 'loves) person1) (getRank (person 'loves) person2)) #t)
          (else #f)           
          ))
  )


(define (zip-together list1 list2)
  (if (null? list1)
      '()
      (cons (list (car list1) (car list2))
            (zip-together (cdr list1) (cdr list2)))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

;helper method to print things out in lines for problem3
(define (write-line x) (begin (write x) (newline)))

;Q2,Q3
(define (make-person my-name)
  (let ((preference-list '())
        (possible-mates '())
        (current-intended '()))
    (define (me message)
      (cond ((eq? message 'name) my-name)
            ((eq? message 'intended) current-intended)
            ((eq? message 'loves) preference-list)
            ((eq? message 'possible) possible-mates)
            ((eq? message 'reset)
               (set! current-intended '())
               (set! possible-mates preference-list)
               'reset-done)
            ((eq? message 'load-preferences)
               (lambda (plist)
                  (set! preference-list plist)
                  (set! possible-mates plist)
                  (set! current-intended '())
                  'preferences-loaded))
            ((eq? message 'propose)
               (let ((beloved (car possible-mates)))
                 (write-line (list (me 'name) 'is 'proposing 'to (beloved 'name)))
                 (set! possible-mates (cdr possible-mates))                
                 (if (eq? ((beloved 'i-love-you) me)
                          'i-love-you-too)
                     (begin (set! current-intended beloved)
                            (write-line (list ((me 'intended) 'name) 'accepted (me 'name) ''s 'proposal))
                            (write-line (list (me 'name) 'is 'engaged 'to ((me 'intended) 'name)))
                            'we-are-engaged                         
                            )
                     (begin
                       (write-line (list (beloved 'name) 'did 'not 'accept (me 'name) ''s 'proposal))
                     'no-one-loves-me
                     ))))

            ;;;;;;;;;Q2,
            ((eq? message 'i-love-you) 
                (lambda (person)
                  (if (eq? current-intended '()) ;there's no current intended person
                      (begin 
                          (set! current-intended person) ;accept the proposal 
                         'i-love-you-too)                       
                      (if (i-like-more? person current-intended) ;if the proposer is liked more than current intended 
                          (begin ((current-intended 'i-changed-my-mind) me) ;dump current intended 
                                 (set! current-intended person) ;accept new proposer's proposal 
                                 
                                 'i-love-you-too                                 
                                 )
                          'buzz-off-creep)))) ;if not, reject 
                                           
            ((eq? message 'i-changed-my-mind)
               (lambda (lost-love)
                  (cond ((eq? current-intended lost-love)
                         (begin
                            (write-line (list (me 'name) 'is 'dumpped 'by (lost-love 'name)))
                            (set! current-intended '())                            
                            'dumped!))
                        (else                       
                            'there-must-be-some-misunderstanding))))           
            (else 
              (error "Bad message to a person " (list me my-name message)))))
      me))

;; This is a test file for -- Stable Marriage

(define alan (make-person 'Alan))
(define bob (make-person 'Bob))
(define charles (make-person 'Chuck))
(define david (make-person 'Dave))
(define ernest (make-person 'Ernie))
(define franklin (make-person 'Frank))
(define agnes (make-person 'Agnes))
(define bertha (make-person 'Bertha))
(define carol (make-person 'Carol))
(define deborah (make-person 'Debbie))
(define ellen (make-person 'Ellen))
(define francine (make-person 'Fran))

((alan 'load-preferences) 
   (list agnes carol francine bertha deborah ellen))
((bob 'load-preferences) 
   (list carol francine bertha deborah agnes ellen))
((charles 'load-preferences) 
   (list agnes francine carol deborah bertha ellen))
((david 'load-preferences) 
   (list francine ellen deborah agnes carol bertha))
((ernest 'load-preferences) 
   (list ellen carol francine agnes deborah bertha))
((franklin 'load-preferences) 
   (list ellen carol francine bertha agnes deborah))
((agnes 'load-preferences) 
   (list charles alan bob david ernest franklin))
((bertha 'load-preferences) 
   (list charles alan bob david ernest franklin))
((carol 'load-preferences) 
   (list franklin charles bob alan ernest david))
((deborah 'load-preferences) 
   (list bob alan charles franklin david ernest))
((ellen 'load-preferences) 
   (list franklin charles bob alan ernest david))
((francine 'load-preferences) 
   (list alan bob charles david franklin ernest))

(define men (list alan bob charles david ernest franklin))
(define women (list agnes bertha carol deborah ellen francine))
