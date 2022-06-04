#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 null))


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.
(define (update f counters index)
  (map (λ (x) (if (= (counter-index x) index) (f x) x)) counters)
)


; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.
(define tt+
  (λ (x)
    (λ (y) (define new-counter (struct-copy counter x [tt (+ (counter-tt x) y)]))
            (match x
              [(counter index tt et queue)
               new-counter])
    )
  )
)



; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define et+
   (λ (x)
    (λ (y) (define new-counter (struct-copy counter x [et (+ (counter-et x) y)]))
            (match x
              [(counter index tt et queue)
               new-counter])
    )
  )
)


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.
(define add-to-counter
  (λ (x)
    (λ (y)
      (λ (z)
        (define new-customer (cons y z))
        (define new-queue (append (counter-queue x) (list new-customer)))
        (define new-customer-time (+ (counter-tt x) z))
        (define new-et (+ (counter-et x) z))
        (cond
          [(null? (counter-queue x)) (define new-counter (struct-copy counter x [tt new-customer-time] [et new-et] [queue new-queue]))
                                     (match x
                                       [(counter index tt et queue)
                                        new-counter])]
          [else (define new-counter (struct-copy counter x [tt new-customer-time] [queue new-queue]))
                (match x
          [(counter index tt et queue)
           new-counter])]
          ))))
)


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel: 
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)
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
  (λ (x) ((min-field 'tt) x))) ; folosind funcția de mai sus
(define min-et
  (λ (x) ((min-field 'et) x))) ; folosind funcția de mai sus


; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.
(define (remove-first-from-counter C)
  (define q (counter-queue C))

  ; Functie care calculeaza timpul total al cumparatorilor de la casa
  (define (total-time q)
    (cond
      [(null? q) 0]
      [(null? (cdr q)) (cdr (car q))]
      [else (+ (cdr (car q)) (total-time (cdr q)))]
    )
  )
  
  (cond
    [(null? (counter-queue C)) C]
    [(null? (cdr (counter-queue C))) (make-counter (counter-index C) 0 0 '())]
    [else (define new-time (- (counter-tt C) (+ (cdr (car (counter-queue C))) (- (counter-tt C) (total-time q)))))
          (make-counter (counter-index C) new-time (cdr (car (cdr (counter-queue C)))) (cdr (counter-queue C)))]
  )
)
    

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)
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
                                         (define new-counter (make-counter (+ 1 (counter-index (car (reverse slow-counters)))) 0 0 '()))
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

        [(list 'remove-first)       
         (serve (cdr requests)
                (update (λ (x) (remove-first-from-counter x)) fast-counters (car (min-et fast-counters)))
                (update (λ (x) (remove-first-from-counter x)) slow-counters (car (min-et slow-counters))))
        ]

        [(list name n-items)
         (define min-tt-fast (min-tt fast-counters))
         (define min-tt-slow (min-tt slow-counters))
         (cond
           [(and (<= n-items ITEMS) (<= (cdr min-tt-fast) (cdr min-tt-slow)))
            (serve (cdr requests)
                   (update (λ (x) (((add-to-counter x) name) n-items)) fast-counters (car (min-tt fast-counters)))
                   slow-counters)]
           [(and (> n-items ITEMS) (<= (cdr min-tt-fast) (cdr min-tt-slow)))
            (serve (cdr requests)
                   fast-counters
                   (update (λ (x) (((add-to-counter x) name) n-items)) slow-counters (car (min-tt slow-counters))))]
           [(and (<= n-items ITEMS) (> (cdr min-tt-fast) (cdr min-tt-slow)))
            (serve (cdr requests)
                   fast-counters
                   (update (λ (x) (((add-to-counter x) name) n-items)) slow-counters (car (min-tt slow-counters))))]
           [(and (> n-items ITEMS) (> (cdr min-tt-fast) (cdr min-tt-slow)))
            (serve (cdr requests)
                   fast-counters
                   (update (λ (x) (((add-to-counter x) name) n-items)) slow-counters (car (min-tt slow-counters))))]
         )
        ]
      )
  )
)

