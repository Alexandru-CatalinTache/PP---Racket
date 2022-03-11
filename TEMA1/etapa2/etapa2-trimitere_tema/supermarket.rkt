#lang racket                                            ;ETAPA 2
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
  empty-counter (make-counter index 0 0 '()))


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.
(define (update f counters index)
  (if (null? counters) '()
  (update_helper f counters index '())))

(define (update_helper f counters index acc)
  (cond((null? counters) acc)
     ((equal? (counter-index (car counters)) index) (update_helper f (cdr counters) index (append acc (list (aplicare_f f counters index))) ))
     (else
     (update_helper f (cdr counters) index (append acc  (list (car counters)))))))

(define (aplicare_f f counters index)
   (f (car (filter (lambda (x) (equal? (counter-index x) index)) counters))))



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
(define (tt+ min)     ;forma ((tt+ min) C)
  (lambda (x)               
      (match x
    [(counter index tt et queue)
     x (struct-copy counter x [tt (+ (counter-tt x) min)])])))


;((tt+ 200) (counter 1 1 1 '()))


; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define (et+ min)
 (lambda (x)               
      (match x
    [(counter index tt et queue)
     x (struct-copy counter x [et (+ (counter-et x) min)])])))


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.

(define (add-to-counter C name n-items)
  (if(null? (counter-queue C)) (update_var_1 C name n-items)
     (update_var_2 C name n-items)))
     
(define (update_var_1 C name n-items)
  C (struct-copy counter C [tt (+ (counter-tt C) n-items)] [et (+ (counter-et C) n-items)] [queue (append (counter-queue C) (list(cons name n-items)))] ))

  (define (update_var_2 C name n-items)
          C (struct-copy counter C [tt (+ (counter-tt C) n-items)] [queue (append (counter-queue C) (list(cons name n-items)))] ))   

;verificare
;(define C1 (empty-counter 1))
;(define C2 (empty-counter 2))
;(define C3 (empty-counter 3))
;(define C4 (empty-counter 4))
;(add-to-counter C3 'ana 11)

; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)
(define (gen-min L elem-min f)
 (cond ((null? L) elem-min)
        ((< (f (car L)) elem-min) (gen-min (cdr L) (f (car L)) f))
        (else (gen-min (cdr L) elem-min f))))

(define (newmin-gen x counters f)     ; returnare pereche (index tt/et) -> x=tt/et min
  (if(equal? (f (car counters)) x)  (cons (counter-index (car counters)) x)
     (newmin-gen x (cdr counters) f)))

; folosind funcția de mai sus
(define (min-tt counters)
  (newmin-gen (gen-min (cdr counters) (counter-tt (car counters )) counter-tt) counters counter-tt))

; folosind funcția de mai sus  
(define (min-et  counters)
  (newmin-gen (gen-min (cdr counters) (counter-et (car counters )) counter-et) counters counter-et))

; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.


(define (remove-first-from-counter C)
 (counter (counter-index C) (update_tt 0 (cdr (counter-queue C))) (update_et (cdr (counter-queue C))) (cdr (counter-queue C))))

(define (update_tt tt queue)
  (if(null? queue) tt
     (update_tt (+ tt (cdr (car queue))) (cdr queue))))

(define (update_et  queue)
  (if(null? queue) 0
      (cdr (car queue))))


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
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'ensure average) (my_ensure average (append fast-counters slow-counters) fast-counters slow-counters requests)]
        [(list name n) (adaugare_client (car requests) requests fast-counters slow-counters (find_counter_type (car requests) n slow-counters fast-counters) name n)]
        [(list 'remove-first) 0]   ;(my_remove requests fast-counters slow-counters (remove_type (select_L (append fast-counters slow-counters) '()) fast-counters slow-counters))]
        [(list 'delay index minutes)   (my_delay requests index minutes (ftype index slow-counters fast-counters) (append fast-counters slow-counters) fast-counters slow-counters )])))


;remove-first:
(define (my_remove requests fast-counters slow-counters type)
  (if(equal? type 'slow) (serve (cdr requests) fast-counters (remove_modify slow-counters))
     (serve (cdr requests) (remove_modify fast-counters) slow-counters)))

(define (remove_type L fastc slow-counters)
  (if(null?  (filter (lambda (x) (equal? (counter-index x) (car(min-et L))) ) slow-counters)) 'fast
     'slow))

(define (remove_modify L)
  (help_remove_modify L '() 0))

(define (help_remove_modify L acc found)
  (cond((null? L) acc)
     ((and (= found 0) (equal? (counter-index (car L)) (car (min-et L)))) (help_remove_modify (cdr L)  (append acc (list (remove-first-from-counter (car L)))) 1))
     (else (help_remove_modify (cdr L) (append acc (list (car L))) found))))

(define (select_L L acc)
  (cond((null? L) acc)
       ((null? (counter-queue (car L))) (select_L (cdr L) acc)) 
       (else (select_L  (cdr L) (append acc (list (car L)))))))
     
  

;add:
(define (adaugare_client pereche requests fast-counters slow-counters counter_type name n-items)
  (if(equal? counter_type 'slow) (serve (cdr requests) fast-counters (modify_general pereche slow-counters name n-items))
     (serve (cdr requests) (modify_general pereche fast-counters name n-items) slow-counters)))

(define (find_counter_type pereche n-items slow-counters fast-counters)
  (if(> n-items 5) 'slow
     (find_counter_to_use pereche (append fast-counters slow-counters) slow-counters fast-counters)))

(define (find_counter_to_use pereche L slow-counters fast-counters)
  (if(null?  (filter (lambda (x) (equal? (counter-index x) (car(min-tt L))) ) slow-counters)) 'fast
     'slow))

(define (modify_general pereche L name n-items)
  (help_modify L name n-items '() 0))

(define (help_modify L name n-items acc found)
  (cond((null? L) acc)
     ((and (= found 0) (equal? (counter-index (car L)) (car (min-tt L)))) (help_modify (cdr L) name n-items (append acc (list (add-to-counter (car L) name n-items))) 1))
     (else (help_modify (cdr L) name n-items (append acc (list (car L))) found))))


;ensure:
(define (my_ensure average L fastc slowc requests)
  (if(> average (calculate_average_tt (sum_tt L 0) (length L)) ) (serve (cdr requests) fastc slowc)
     (serve (cdr requests) fastc (modify_slowc average slowc (length L) (sum_tt L 0) 0))))

(define (sum_tt L sum)
  (if(null? L) sum
     (sum_tt (cdr L) (+ sum (counter-tt (car L))))))

(define (calculate_average_tt suma lung)
  (/ suma lung))

(define (modify_slowc average slowc lung suma acc)
  (if(>= average (/ suma lung)) (adaugare_acc_counters slowc acc (find_last_index slowc))
     (modify_slowc average slowc (+ lung 1) suma (+ acc 1))))

(define (adaugare_acc_counters slowc acc index)
  (if(equal? acc 0) slowc
     (adaugare_acc_counters (append slowc (list (counter (+ index 1) 0 0 '()))) (- acc 1) (+ index 1))))

(define (find_last_index slowc)
  (if (null? slowc) 0
       (counter-index (last slowc))))

;verificare
;(sum_tt (list (counter 1 1 1 '()) (counter 2 100 1 '()) (counter 4 1000 1 '())) 0)
;(find_last_index (list (counter 1 1 1 '()) (counter 2 100 1 '()) (counter 24 1000 1 '())))
;(length (list (counter 1 1 1 '()) (counter 2 100 1 '()) (counter 24 1000 1 '())))
;(calculate_average_tt 100 11)
;(adaugare_acc_counters (list (counter 1 1 1 '()) (counter 2 100 1 '()) (counter 111 1000 1 '())) 4 111)


;delay
(define (my_delay requests index minutes type L fast-counters slow-counters)
  (if(equal? type 'slow) (serve (cdr requests) fast-counters (update (et_tt+ minutes) slow-counters index))
       (serve (cdr requests) (update (et_tt+ minutes)  fast-counters index) slow-counters)))

 (define (et_tt+ min)
 (lambda (x)               
      (match x
    [(counter index tt et queue)
     x (struct-copy counter x [et (+ (counter-et x) min)] [tt (+ (counter-tt x) min)])])))

(define (ftype index slowc fastc)
  (cond((null? slowc) 'fast)
       ((null?  (filter (lambda (x) (equal? (counter-index  x) index)  ) slowc)) 'fast)
       ((null? fastc) 'slow)
       ((equal? 1 1) 'slow)))



                                                   

(define (find_counter_by_index index L)    ; returneaza casa ce are indexul respectiv
  (if(equal? index (counter-index (car L))) (car L)
     (find_counter_by_index index (cdr L))))


         

;verificare
;(ftype 3 (list (counter 1 0 0 '()) (counter 11 0 0 '()) (counter 15 0 0 '())) (list (counter 3 0 0 '()) (counter 7 0 0 '()) (counter 10 0 0 '())))
;(define C1 (empty-counter 1))
;(define C2 (empty-counter 2))
;(define C3 (empty-counter 3))
;(define C4 (empty-counter 4))
;(define C5 (make-counter 5 12 8 '((remus . 6) (vivi . 4))))

;(serve '((delay 2 3) (ana 2) (bogdan 3) (ensure 2))
          ; (list C1)
           ;(list C2 C3))
