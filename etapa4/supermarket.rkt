#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue
(define-struct counter (index tt et queue) #:transparent)

(define (empty-counter index)
  (make-counter index 0 0 empty-queue)
)
  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

(define (pass-time-through-counter minutes)
  (λ (C)
    (cond
     [(queue-empty? (counter-queue C)) (make-counter (counter-index C) 0 0 empty-queue)]
     [(> minutes (counter-tt C)) (make-counter (counter-index C) 0 0 empty-queue)]
     [else (make-counter (counter-index C) (- (counter-tt C) minutes) (- (counter-et C) minutes) (counter-queue C))]
    )
  ))

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

(define (update f counters index)
  (map (λ (x) (if (= (counter-index x) index) (f x) x)) counters)
)

(define (total-time counters)
           (cond
             [(null? counters) 0]
             [else (+ (counter-tt (car counters)) (total-time (cdr counters)))]))

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


(define (add-to-counter name items)    
  (λ (C)                               
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

(define (serve requests fast-counters slow-counters)
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)

        [(list 'ensure average)
         'yourcodehere
        ]
        
        [(list 'delay index minutes)
         (serve (cdr requests)
                (update (λ (x) (struct-copy counter x [tt (counter-tt ((tt+ x) minutes))] [et (counter-et ((et+ x) minutes))])) fast-counters index)
                (update (λ (x) (struct-copy counter x [tt (counter-tt ((tt+ x) minutes))] [et (counter-et ((et+ x) minutes))])) slow-counters index))
        ]

        [(list name n-items)
         'yourcodehere
        ]

        [x
         'yourcodehere
        ]

        [(list 'close index) 'yourcodehere]
      )
  )
)