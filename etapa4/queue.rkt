#lang racket
(require racket/match)

(provide empty-queue)
(provide make-queue)
(provide queue-empty?)
(provide rotate)             ; pentru testare
(provide enqueue)
(provide dequeue)
(provide top)

(provide (struct-out queue)) ; pentru testare

;; În etapa 3 am implementat TDA-ul queue astfel încât să avem cost amortizat O(1)
;; atât pentru enqueue cât și pentru dequeue.
;; Metoda: am reprezentat coada ca pe o colecție de 2 stive:
;; - stiva left: pentru scoaterea de elemente la dequeue 
;; - stiva right: pentru adăugarea de elemente la enqueue 
;;
;; Singurul caz în care o operație nu era O(1) era dequeue atunci când left era goală.
;; Orice asemenea dequeue era O(n), din cauza mutării tuturor elementelor din right în left.
;; În această etapă, ne dorim să îmbunătățim costul operației dequeue pe cazul cel mai
;; defavorabil (cost care în etapa 3 era O(n), din cauza situației de mai sus).
;;
;; Soluția: păstrăm reprezentarea cu 2 stive, dar ne vom asigura că atunci când se face 
;; dequeue stiva left nu este niciodată goală, menținând invariantul:
;;        |left| ≥ |right|      (prin |S| înțelegem dimensiunea stivei S)
;; De fiecare dată când un enqueue sau un dequeue duce la violarea invariantului,
;; efectuăm o rotație:
;;        <left, right>   devine   <left ++ (reverse right), []>
;; Cât timp reprezentâm stivele ca liste Racket, o rotație va avea complexitate O(n),
;; cauzată de append și de reverse. Avem la dispoziție o reprezentare mai bună?
;;
;; Da! Vom reprezenta stiva left ca pe un flux. Spre deosebire de append (notat aici ++)
;; pe liste (care are complexitate O(n)), append pe fluxuri este o operație incrementală:
;; - elementele din rezultat sunt furnizate unul câte unul, atunci când este nevoie
;; - ex: A = fluxul [1,2,3,4,5], reprezentat ca (stream-cons 1 <calcul-întârziat-rest>)
;;       B = un flux oarecare
;;       A ++ B va fi (stream-cons 1 <calcul-întârziat-append-între-restA-și-B>)
;;   (acest rezultat se obține în timp O(1))
;; Astfel rezolvăm complexitatea operației append din expresia "left ++ (reverse right)"
;;
;; Cum rezolvăm complexitatea operației reverse din aceeași expresie?
;; Cum append este deja o operație incrementală, ideea este să efectuăm câte un pas de
;; reverse de fiecare dată când efectuăm un pas de append.
;; Acest truc termină ambele operații cam în același timp, întrucât facem rotații doar 
;; când right devine mai lungă decât left, adică |right| = |left| + 1).
;; Amintiți-vă codul pentru append și pentru reverse cu recursivitate pe coadă:
;; (define (append A B)                     (define (reverse L Acc)
;;   (if (null? A)                            (if (null? L)
;;       B                                        Acc
;;       (cons (car A) (append (cdr A) B))))      (reverse (cdr L) (cons (car L) Acc))))
;;
;; Implementăm o rotație conform axiomelor următoare (observați fuziunea de append și reverse):
;; rotate([], [y], Acc)        = y : Acc                    
;; rotate((x:xs), (y:ys), Acc) = x : rotate(xs, ys, y : Acc)
;; Obs: 
;; - x : rotate(...) reprezintă un pas de append ( : înseamnă cons), ca în codul de mai sus
;; - y : Acc         reprezintă un pas de reverse, ca în codul de mai sus


; Structura queue nu se modifică.
; Ceea ce se modifică este implementarea câmpului left
; - în loc de listă, acesta va fi un flux
; - acest lucru nu este vizibil în definiția structurii queue,
;   ci în implementarea operațiilor acestui tip de date 
(define-struct queue (left right size-l size-r) #:transparent) 


; TODO
; Definiți valoarea care reprezintă o structură queue goală.
(define empty-queue
  (make-queue empty-stream '() 0 0)
)


; TODO
; Implementați o funcție care verifică dacă o coadă este goală.
(define (queue-empty? q)
  (if (and (= (queue-size-l q) 0) (= (queue-size-r q) 0)) #t #f)
)


; TODO
; Implementați funcția rotate, conform axiomelor de mai sus.
; Atenție: ce tip trebuie să aibă Acc?
(define (rotate left right Acc)
  (cond
    [(null? right) (stream-append left Acc)]
    [else (rotate left (cdr right) (stream-append (stream (car right)) Acc))]
  )
)

; TODO
; Implementați o funcție care adaugă un element la sfârșitul unei cozi.
; Veți întoarce coada actualizată.
; Atenție: în urma adăugării unui element, poate fi necesară o rotație!
(define (enqueue x q)
  (cond
    [(< (queue-size-l q) (add1 (queue-size-r q))) (make-queue (rotate (queue-left q) (append (list x) (queue-right q)) empty-stream)
                                                       '()
                                                       (add1 (+ (queue-size-l q) (queue-size-r q)))
                                                       0)]
    [else (make-queue (queue-left q)
                      (append (list x) (queue-right q))
                      (queue-size-l q)
                      (add1 (queue-size-r q)))]
  )
)


; TODO
; Implementați o funcție care scoate primul element dintr-o coadă nevidă
; (nu verificați că e nevidă, pe coada vidă este firesc să dea eroare).
; Veți întoarce coada actualizată.
; Atenție: în urma înlăturării unui element, poate fi necesară o rotație!
(define (dequeue q)
  (cond
    [(<= (queue-size-l q) (queue-size-r q)) (cond
                                              [(not (null? (queue-left q))) (make-queue (rotate (stream-tail (queue-left q) 1) (queue-right q) empty-stream)
                                                                                        '()
                                                                                        (sub1 (+ (queue-size-l q) (queue-size-r q)))
                                                                                        0)]
                                              [else (make-queue (cdr (append (queue-left q) (reverse (queue-right q))))
                                                                '()
                                                                (sub1 (queue-size-r q))
                                                                0)]
                                             
  )]
    [else (cond
            [(not (null? (queue-left q))) (make-queue (stream-tail (queue-left q) 1)
                                                      (queue-right q)
                                                      (sub1 (queue-size-l q))
                                                      (queue-size-r q))]
            [else (make-queue (cdr (append (queue-left q) (reverse (queue-right q))))
                              '()
                              (sub1 (queue-size-r q))
                              0)]
  )]
  )
)


; TODO
; Implementați o funcție care obține primul element dintr-o coadă nevidă
; (nu verificați că e nevidă, pe coada vidă este firesc să dea eroare).
; Veți întoarce elementul aflat la începutul cozii.
(define (top q)
  (cond
    [(stream-empty? (queue-left q)) (car (reverse (queue-right q)))]
    [else (stream-ref (queue-left q) 0)]
  )
)
