#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue)
)

(define (update f counters index)
  (map (λ (x) (if (= (counter-index x) index) (f x) x)) counters)
)

(define tt+
  (λ (x)
    (λ (y) (define new-counter (struct-copy counter x [tt (+ (counter-tt x) y)]))
            (match x
              [(counter index tt et queue)
               new-counter])
    )
  )
)

(define et+
  (λ (x)
    (λ (y) (define new-counter (struct-copy counter x [et (+ (counter-et x) y)]))
            (match x
              [(counter index tt et queue)
               new-counter])
    )
  )
)

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (define new-customer (cons name items))
    (define new-queue (enqueue new-customer (counter-queue C)))
    (define new-customer-time (+ (counter-tt C) items))
    (define new-et (+ (counter-et C) items))
    (cond
      [(queue-empty? (counter-queue C)) (define new-counter (struct-copy counter C [tt new-customer-time]
                                                                                          [et new-et]
                                                                                          [queue new-queue]))
                                 (match C
                                   [(counter index tt et queue)
                                    new-counter])]
      [else (define new-counter (struct-copy counter C [tt new-customer-time] [queue new-queue]))
            (match C
              [(counter index tt et queue)
               new-counter])]
      )))

(define min-field
  (λ (x)
    (λ (y)
      (if (equal? x (quote tt))
         (cond
     [(null? (cdr y)) (cons (counter-index (car y)) (counter-tt (car y)))]
     [(<= (counter-tt (car y)) (cdr ((min-field (quote tt)) (cdr y))))
      (cons (counter-index (car y)) (counter-tt (car y)))]
     [(and (= (counter-tt (car y)) (cdr ((min-field (quote tt)) (cdr y))))
           (> (counter-index (car y)) (cdr ((min-field (quote tt)) (cdr y)))))
      (cons (counter-index (car y)) (counter-tt (car y)))
     ]
     [else ((min-field (quote tt)) (cdr y))]
   )
         (cond
     [(null? (cdr y)) (cons (counter-index (car y)) (counter-et (car y)))]
     [(<= (counter-et (car y)) (cdr ((min-field (quote et)) (cdr y))))
      (cons (counter-index (car y)) (counter-et (car y)))]
     [(and (= (counter-et (car y)) (cdr ((min-field (quote et)) (cdr y))))
           (> (counter-index (car y)) (cdr ((min-field (quote et)) (cdr y)))))
      (cons (counter-index (car y)) (counter-et (car y)))
     ]
     [else ((min-field (quote et)) (cdr y))]
   ))))
)
(define min-tt
  (λ (x) ((min-field 'tt) x))
)
(define min-et
  (λ (x) ((min-field 'et) x))
)

(define (remove-first-from-counter C)   ; testată de checker
  (cond
    [(queue-empty? (counter-queue C)) (make-counter (counter-index C) 0 0 empty-queue)]
    [(queue-empty? (dequeue (counter-queue C))) (make-counter (counter-index C) 0 0 empty-queue)]
    [else (make-counter (counter-index C) (- (counter-tt C) (counter-et C)) (cdr (top (dequeue (counter-queue C)))) (dequeue (counter-queue C)))]
  )
)


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (cond
     [(queue-empty? (counter-queue C)) (make-counter (counter-index C) 0 0 empty-queue)]
     [(> minutes (counter-tt C)) (make-counter (counter-index C) 0 0 empty-queue)]
     [else (make-counter (counter-index C) (- (counter-tt C) minutes) (- (counter-et C) minutes) (counter-queue C))]
    )
  ))
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (serve requests fast-counters slow-counters)
  (define (total-time counters)
           (cond
             [(null? counters) 0]
             [else (+ (counter-tt (car counters)) (total-time (cdr counters)))]))
  
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)

        [(list 'ensure average)
         (cond
           [(>= (quotient (+ (total-time fast-counters) (total-time slow-counters)) (+ (length fast-counters) (length slow-counters))) average)
                                         (define new-counter (make-counter (+ 1 (counter-index (car (reverse slow-counters)))) 0 0 empty-queue))
                                         (append slow-counters (list new-counter))
                                         (serve requests fast-counters (append slow-counters (list new-counter)))]
           [else (serve (cdr requests) fast-counters slow-counters)]
           )
        ]
        
        [(list 'delay index minutes)
         (serve (cdr requests)
                (update (λ (x) (struct-copy counter x [tt (counter-tt ((tt+ x) minutes))] [et (counter-et ((et+ x) minutes))])) fast-counters index)
                (update (λ (x) (struct-copy counter x [tt (counter-tt ((tt+ x) minutes))] [et (counter-et ((et+ x) minutes))])) slow-counters index))
        ]

        [(list name n-items)
         (define min-tt-fast (min-tt fast-counters))
         (define min-tt-slow (min-tt slow-counters))
         (cond
           [(and (<= n-items ITEMS) (<= (cdr min-tt-fast) (cdr min-tt-slow)))
            (serve (cdr requests)
                   (update (λ (x) ((add-to-counter name n-items) x)) fast-counters (car (min-tt fast-counters)))
                   slow-counters)]
           [(and (> n-items ITEMS) (<= (cdr min-tt-fast) (cdr min-tt-slow)))
            (serve (cdr requests)
                   fast-counters
                   (update (λ (x) ((add-to-counter name n-items) x)) slow-counters (car (min-tt slow-counters))))]
           [(and (<= n-items ITEMS) (> (cdr min-tt-fast) (cdr min-tt-slow)))
            (serve (cdr requests)
                   fast-counters
                   (update (λ (x) ((add-to-counter name n-items) x)) slow-counters (car (min-tt slow-counters))))]
           [(and (> n-items ITEMS) (> (cdr min-tt-fast) (cdr min-tt-slow)))
            (serve (cdr requests)
                   fast-counters
                   (update (λ (x) ((add-to-counter name n-items) x)) slow-counters (car (min-tt slow-counters))))]
         )
        ]

        [x
         (serve (cdr requests)
                (map (λ (C) ((pass-time-through-counter x) C)) fast-counters)
                (map (λ (C) ((pass-time-through-counter x) C)) slow-counters))
        ]
      )
  )
)
        
