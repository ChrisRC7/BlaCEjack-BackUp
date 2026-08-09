#lang racket
(require racket/tcp)
(provide iniciar
         bCEj)
(define (carta-aleatoria cartas)
  (list-ref cartas (random (length cartas))))

(define (eliminar-carta-aleatoria carta cartas)
  (cond
    ((null? cartas) '())  ; Si la lista está vacía, retorna una lista vacía.
    ((equal? carta (car cartas)) (cdr cartas))  ; Si la carta es la primera de la lista, la omitimos.
    (else (cons (car cartas) (eliminar-carta-aleatoria carta (cdr cartas))))))  ; Sino, seguimos recursivamente.




(define (crear-baraja lista-cartas baraja)
  (cond
    ((null? lista-cartas) baraja)
    (else (aux-baraja lista-cartas baraja (carta-aleatoria lista-cartas)))))

(define (aux-baraja lista-cartas baraja carta-aleat)
  (cond
    ((null? lista-cartas)' baraja)
    (else (crear-baraja (eliminar-carta-aleatoria carta-aleat lista-cartas) (cons carta-aleat baraja)))))
  


  
(define (valor-carta carta)
  (cond
    ((member carta '(Jh Jd Jc Js Qh Qd Qc Qs Kh Kd Kc Ks))
     10)
    ((member carta '(Ah Ad Ac As))
     'A)
    (else (string->number (substring (symbol->string carta) 0 (- (string-length (symbol->string carta)) 1))))
    )
  )

(define (sumar-cartas lista-cartas)
  (define (sumar-cartas-aux lista acumulador)
    (if (null? lista)
        acumulador
        (if (equal? (valor-carta (car lista)) 'A)
            (if (> (+ acumulador 11) 21)
                (sumar-cartas-aux (cdr lista) (+ acumulador 1))
                (sumar-cartas-aux (cdr lista) (+ acumulador 11)))
            (sumar-cartas-aux (cdr lista) (+ acumulador (valor-carta (car lista)))))))
  (sumar-cartas-aux lista-cartas 0)
  )
(define (mover-as-al-final cartas)
  (define (aux cartas sin-a con-a)
    (cond
      [(null? cartas) (append sin-a con-a)]
      [(or (equal? (car cartas) 'A) (equal? (car cartas) 'Ah) (equal? (car cartas) 'As)) 
       (aux (cdr cartas) sin-a (cons (car cartas) con-a))]
      [else (aux (cdr cartas) (cons (car cartas) sin-a) con-a)]))
  (aux cartas '() '()))



(define (pedir-carta baraja tipo)
  (cond
    ((null? baraja) '())
    ((= tipo 1)
     (car baraja))
    (else
     (cdr baraja))))
(define (send msg in out)  
  (log msg)
  (write-string (string-append msg "\r\n") out)
  (flush-output out))
(define (send-wait in out msg tipo)
  ;; Envía el mensaje al cliente
  (write-string (string-append msg "\r\n") out)
  (flush-output out)
  ;; Espera y lee la respuesta del cliente
  (if (= tipo 1)
      (read-line in 'return-linefeed)
      (displayln ""))
  
  )
(define (puntaje mano)
  (foldl + 0 mano))


(define (calcular-ganador jugador crupier)
  (define puntaje-jugador (sumar-cartas jugador))
  (define puntaje-crupier (sumar-cartas crupier))
  (cond
    [(> puntaje-jugador 21) -1]
    [(> puntaje-crupier 21) 1]
    [(= puntaje-jugador 21) 1]
    [(= puntaje-crupier 21) -1]
    [(> puntaje-jugador puntaje-crupier) 1]
    [(< puntaje-jugador puntaje-crupier) -1]
    [else 0]))


;The code provided is a Scheme function named `bCEj` that seems to be implementing a card game logic, possibly a simplified version of a blackjack game. 
;The function takes multiple parameters including player hands, dealer hand, deck of cards, turn information, and input/output channels.
(define (bCEj X crupier jugador1 jugador2 jugador3 baraja turno in out plantarsec plantarsej plantarsej2 plantarsej3)
 
  (cond
    ((< (length baraja) 4)
     
     (send-wait in out "seacabo" 0))
    ((= X 1)
     (cond
       ((< (length jugador1) 2)
       
        (send-wait in out (symbol->string (pedir-carta baraja 1)) 0)
        (bCEj X crupier (cons (pedir-carta baraja 1) jugador1) jugador2 jugador3 (pedir-carta baraja 2) 2  in out plantarsec plantarsej  plantarsej2 plantarsej3))
       ((< (length crupier) 2)
       
        (send-wait in out (string-append "crup" (symbol->string (pedir-carta baraja 1))) 0)

        (bCEj X (cons (pedir-carta baraja 1) crupier) jugador1 jugador2 jugador3 (pedir-carta baraja 2) 2 in out plantarsec plantarsej  plantarsej2 plantarsej3))

       


       
       
       
       ((and (> (length jugador1) 1) (= turno 2) (= plantarsej 0))
  ; Verificar si la longitud de la mano del jugador es mayor que 1, es el turno del jugador y no se ha plantado
  (displayln "Turno del jugador") ; Mostrar mensaje indicando el turno del jugador
  (displayln jugador1) ; Mostrar la mano del jugador por consola
  (let ((respuesta (send-wait in out "¿Desea pedir carta? (s/n): " 1))) ; Esperar la respuesta del cliente
    (if (string=? respuesta "s") ; Si la respuesta es "s"
        (begin
          ;; Envía la carta pedida al cliente
          (send-wait in out (symbol->string (pedir-carta baraja 1)) 0)
          (begin
            (if (= plantarsec 1) ; Si el crupier se ha plantado
                (begin
                  (bCEj X crupier (cons (pedir-carta baraja 1) jugador1) jugador2 jugador3 (pedir-carta baraja 2) 2 in out plantarsec plantarsej plantarsej2 plantarsej3))
                (begin
                  (bCEj X crupier (cons (pedir-carta baraja 1) jugador1) jugador2 jugador3 (pedir-carta baraja 2) 1 in out plantarsec plantarsej plantarsej2 plantarsej3)))))
      ;; Si la respuesta no es "s", continúa con la lógica del juego
      (bCEj X crupier jugador1 jugador2 jugador3 baraja 1 in out plantarsec 1 0 0))))

       ((and (> (length crupier) 1) (= turno 1) (= plantarsec 0))
  ; Verificar si la longitud de la mano del crupier es mayor que 1, es el turno del crupier y no se ha plantado
  (displayln "Turno del crupier") ; Mostrar mensaje indicando el turno del crupier
  (displayln crupier) ; Mostrar la mano del crupier por consola
  (if (< (sumar-cartas (mover-as-al-final crupier)) 17) ; Si la suma de las cartas del crupier es menor que 17
      (begin
        (send-wait in out (string-append "crup" (symbol->string (pedir-carta baraja 1))) 0) ; Envía la carta pedida al cliente
        (begin
          (if (= plantarsej 1) ; Si el jugador se ha plantado
              (begin
                (bCEj X (cons (pedir-carta baraja 1) crupier) jugador1 jugador2 jugador3 (pedir-carta baraja 2) 1 in out plantarsec plantarsej plantarsej2 plantarsej3))
              (begin
                (bCEj X (cons (pedir-carta baraja 1) crupier) jugador1 jugador2 jugador3 (pedir-carta baraja 2) 2 in out plantarsec plantarsej plantarsej2 plantarsej3)))))
    (bCEj X crupier jugador1 jugador2 jugador3 baraja 2 in out 1 plantarsej plantarsej2 plantarsej3))) ; Si no, continúa con la lógica del juego



       ((= plantarsej plantarsec 1) ; Si tanto el jugador como el crupier se han plantado
        (send-wait in out (string-append "ptj1" (number->string (calcular-ganador (mover-as-al-final jugador1) (mover-as-al-final crupier)))) 0) ; Envía el resultado de la partida al cliente
        (cond
          [(and (=(sumar-cartas (mover-as-al-final crupier))21) (= (length crupier)2)) ; Si la suma de las cartas del crupier es 21 y tiene 2 cartas
           (send-wait in out "puntcbj" 0) ; Envía un mensaje al cliente indicando que el crupier obtuvo blackjack
           
           


           ]
     



)
          
          (send-wait in out "seacabo" 0)
        


          )

       ))




    ;; Para 2 jugadores
    ((= X 2)
    
     (cond


       ((< (length jugador1) 2)
       
        (send-wait in out (symbol->string (pedir-carta baraja 1)) 0)
        (bCEj X crupier (cons (pedir-carta baraja 1) jugador1) jugador2 jugador3 (pedir-carta baraja 2) 2  in out plantarsec plantarsej  plantarsej2 plantarsej3))

       
       ((< (length jugador2) 2)
       
        (send-wait in out (string-append "jg2" (symbol->string (pedir-carta baraja 1))) 0)
        (bCEj X crupier jugador1 (cons (pedir-carta baraja 1) jugador2) jugador3 (pedir-carta baraja 2) 2  in out plantarsec plantarsej  plantarsej2 plantarsej3))
       

       ((< (length crupier) 2)
       
        (send-wait in out (string-append "crup" (symbol->string (pedir-carta baraja 1))) 0)

        (bCEj X (cons (pedir-carta baraja 1) crupier) jugador1 jugador2 jugador3 (pedir-carta baraja 2) 2 in out plantarsec plantarsej  plantarsej2 plantarsej3))

       


       
       
       
       ((and (> (length jugador1) 1) (= turno 2) (= plantarsej 0))
  (send-wait in out (string-append "turno" (number->string turno)) 0)
        (displayln "Turno del jugador")
        (displayln jugador1)
        (let ((respuesta (send-wait in out "¿Desea pedir carta? (s/n): " 1))) ; Espera la respuesta del cliente
          (if (string=? respuesta "s")
              
              (begin
            
                ;; Envía la carta pedida al cliente
                (send-wait in out  (symbol->string (pedir-carta baraja 1)) 0)


                
                (begin
                  (if(= plantarsec plantarsej2 1)
               
                       
                     (bCEj X crupier (cons (pedir-carta baraja 1) jugador1) jugador2 jugador3 (pedir-carta baraja 2) 2  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                      
                     (begin
                       (if(= plantarsej2 1)
                          (bCEj X crupier (cons (pedir-carta baraja 1) jugador1) jugador2 jugador3 (pedir-carta baraja 2) 1  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                          (bCEj X crupier (cons (pedir-carta baraja 1) jugador1) jugador2 jugador3 (pedir-carta baraja 2) 3  in out plantarsec plantarsej  plantarsej2 plantarsej3))))))
                      
                 
                
              ;; Si la respuesta no es "s", continúa con la lógica del juego
              (begin
                (if(= plantarsec plantarsej2 1)
                   (bCEj X crupier jugador1 jugador2 jugador3 baraja 2 in out plantarsec 1 plantarsej2 plantarsej3)
                   (begin
                     (if(= plantarsej2 1)
                        (bCEj X crupier jugador1 jugador2 jugador3 baraja 1 in out plantarsec 1 plantarsej2 plantarsej3)
                        (bCEj X crupier jugador1 jugador2 jugador3 baraja 3 in out plantarsec 1 plantarsej2 plantarsej3))))))))



       ((and (> (length jugador2) 1) (= turno 3) (= plantarsej2 0))
  (send-wait in out (string-append "turno" (number->string turno)) 0)
        (displayln "Turno del jugador 2")
        (displayln jugador2)
        (let ((respuesta (send-wait in out "¿Desea pedir carta? (s/n): " 1))) ; Espera la respuesta del cliente
          (if (string=? respuesta "s")
              
              (begin
            
                ;; Envía la carta pedida al cliente
                (send-wait in out (string-append "jg2" (symbol->string (pedir-carta baraja 1))) 0)


                (begin
                  (if(= plantarsec plantarsej 1)
                     (bCEj X crupier jugador1 (cons (pedir-carta baraja 1) jugador2) jugador3 (pedir-carta baraja 2) 3  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                     (begin
                       (if(= plantarsec 1)
                          (bCEj X crupier jugador1 (cons (pedir-carta baraja 1) jugador2) jugador3 (pedir-carta baraja 2) 2  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                          (bCEj X crupier jugador1 (cons (pedir-carta baraja 1) jugador2) jugador3 (pedir-carta baraja 2) 1  in out plantarsec plantarsej  plantarsej2 plantarsej3)))))
                  
                )
                        
                          
              ;; Si la respuesta no es "s", continúa con la lógica del juego
              (begin
                (if(= plantarsec plantarsej 1)
                   (bCEj X crupier jugador1 jugador2 jugador3 baraja 3 in out plantarsec plantarsej 1 plantarsej3)
                   (begin
                     (if(= plantarsec 1)
                        (bCEj X crupier jugador1 jugador2 jugador3 baraja 2 in out plantarsec plantarsej 1 plantarsej3)
                        (bCEj X crupier jugador1 jugador2 jugador3 baraja 1 in out plantarsec plantarsej 1 plantarsej3)
                        )))



                ))))


       
       ((and (> (length crupier) 1) (= turno 1) (= plantarsec 0))
        (send-wait in out (string-append "turno" (number->string turno)) 0)
     
        (displayln "Turno del crupier")
        (displayln crupier)
        
        (if (< (sumar-cartas (mover-as-al-final crupier)) 17)
            (begin
              
              (send-wait in out (string-append "crup" (symbol->string (pedir-carta baraja 1))) 0)
              (if(= plantarsej plantarsej2 1)
                 (bCEj X (cons (pedir-carta baraja 1) crupier) jugador1 jugador2 jugador3 (pedir-carta baraja 2) 1  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                 (begin
                   (if(= plantarsej 1)
                      (bCEj X (cons (pedir-carta baraja 1) crupier) jugador1 jugador2 jugador3 (pedir-carta baraja 2) 3  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                      (bCEj X (cons (pedir-carta baraja 1) crupier) jugador1 jugador2 jugador3 (pedir-carta baraja 2) 2  in out plantarsec plantarsej  plantarsej2 plantarsej3))))

              )

            (begin         
              (if(= plantarsej plantarsej2 1)
                 (bCEj X crupier jugador1 jugador2 jugador3 baraja 1 in out 1 plantarsej plantarsej2 plantarsej3)
                 (begin
                   (if(= plantarsej 1)
                      (bCEj X crupier jugador1 jugador2 jugador3 baraja 3 in out 1 plantarsej plantarsej2 plantarsej3)
                      (bCEj X crupier jugador1 jugador2 jugador3 baraja 2 in out 1 plantarsej plantarsej2 plantarsej3)))))))


       ((= plantarsej plantarsej2 plantarsec 1)
        (send-wait in out (string-append "ptj1" (number->string (calcular-ganador (mover-as-al-final jugador1) (mover-as-al-final crupier)))) 0)
        (send-wait in out (string-append "ptj2" (number->string (calcular-ganador (mover-as-al-final jugador2) (mover-as-al-final crupier)))) 0)
        
        (cond
          [(and (=(sumar-cartas (mover-as-al-final crupier))21) (= (length crupier)2))
           (send-wait in out "puntcbj" 0)
           
           


           ]
          
          


          
         
           



          )
        (send-wait in out "seacabo" 0)
        


        )

       ))














    ;; TRES JUGADORESSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS

    ((= X 3)
     
     
     (cond


       ((< (length jugador1) 2)
       
        (send-wait in out (symbol->string (pedir-carta baraja 1)) 0)
        (bCEj X crupier (cons (pedir-carta baraja 1) jugador1) jugador2 jugador3 (pedir-carta baraja 2) 4  in out plantarsec plantarsej  plantarsej2 plantarsej3))

       
       ((< (length jugador2) 2)
       
        (send-wait in out (string-append "jg2" (symbol->string (pedir-carta baraja 1))) 0)
        (bCEj X crupier jugador1 (cons (pedir-carta baraja 1) jugador2) jugador3 (pedir-carta baraja 2) 4  in out plantarsec plantarsej  plantarsej2 plantarsej3))

       ((< (length jugador3) 2)
       
        (send-wait in out (string-append "jg3" (symbol->string (pedir-carta baraja 1))) 0)
        (bCEj X crupier jugador1 jugador2 (cons (pedir-carta baraja 1) jugador3) (pedir-carta baraja 2) 4  in out plantarsec plantarsej  plantarsej2 plantarsej3))
       

       ((< (length crupier) 2)
       
        (send-wait in out (string-append "crup" (symbol->string (pedir-carta baraja 1))) 0)

        (bCEj X (cons (pedir-carta baraja 1) crupier) jugador1 jugador2 jugador3 (pedir-carta baraja 2) 4 in out plantarsec plantarsej  plantarsej2 plantarsej3))

       


       
       
       
       ((and (> (length jugador1) 1) (= turno 2) (= plantarsej 0))
        
        (send-wait in out (string-append "turno" (number->string turno)) 0)
        (displayln "Turno del jugador")
        (displayln jugador1)
        (let ((respuesta (send-wait in out "¿Desea pedir carta? (s/n): " 1))) ; Espera la respuesta del cliente
          (if (string=? respuesta "s")
              
              (begin
            
                ;; Envía la carta pedida al cliente
                (send-wait in out  (symbol->string (pedir-carta baraja 1)) 0)


                
                (begin
                  (if(= plantarsec plantarsej2 plantarsej3 1)
               
                       
                     (bCEj X crupier (cons (pedir-carta baraja 1) jugador1) jugador2 jugador3 (pedir-carta baraja 2) 2  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                      
                     (begin
                       (if(= plantarsej2 plantarsej3 1)
                          (bCEj X crupier (cons (pedir-carta baraja 1) jugador1) jugador2 jugador3 (pedir-carta baraja 2) 1  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                          (begin(
                                 if(= plantarsej2 1)
                                   (bCEj X crupier (cons (pedir-carta baraja 1) jugador1) jugador2 jugador3 (pedir-carta baraja 2) 4  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                                   (bCEj X crupier (cons (pedir-carta baraja 1) jugador1) jugador2 jugador3 (pedir-carta baraja 2) 3  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                                   )))))))
                      
                 
                
              ;; Si la respuesta no es "s", continúa con la lógica del juego
              (begin
                (if(= plantarsec plantarsej2 plantarsej3 1)
               
                   (bCEj X crupier jugador1 jugador2 jugador3 baraja 2 in out plantarsec 1 plantarsej2 plantarsej3)
                   (begin
                     (if(= plantarsej2 plantarsej3 1)
                        (bCEj X crupier jugador1 jugador2 jugador3 baraja 1 in out plantarsec 1 plantarsej2 plantarsej3)
                        (begin(
                               if(= plantarsej2 1)
                                 (bCEj X crupier jugador1 jugador2 jugador3 baraja 4 in out plantarsec 1 plantarsej2 plantarsej3)
                                 (bCEj X crupier jugador1 jugador2 jugador3 baraja 3 in out plantarsec 1 plantarsej2 plantarsej3)
                                   


                                 )))))))))



       ((and (> (length jugador2) 1) (= turno 3) (= plantarsej2 0))
        
        (send-wait in out (string-append "turno" (number->string turno)) 0)
        (displayln "Turno del jugador 2")
        (displayln jugador2)
        (let ((respuesta (send-wait in out "¿Desea pedir carta? (s/n): " 1))) ; Espera la respuesta del cliente
          (if (string=? respuesta "s")
              
              (begin
            
                ;; Envía la carta pedida al cliente
                (send-wait in out (string-append "jg2" (symbol->string (pedir-carta baraja 1))) 0)


                (begin
                  (if(= plantarsec plantarsej plantarsej3 1)
                     (bCEj X crupier jugador1 (cons (pedir-carta baraja 1) jugador2) jugador3 (pedir-carta baraja 2) 3  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                     (begin
                       (if(= plantarsec plantarsej3 1)
                          (bCEj X crupier jugador1 (cons (pedir-carta baraja 1) jugador2) jugador3 (pedir-carta baraja 2) 2  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                          (begin
                            (if (= plantarsej3 1)
                                (bCEj X crupier jugador1 (cons (pedir-carta baraja 1) jugador2) jugador3 (pedir-carta baraja 2) 1  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                                (bCEj X crupier jugador1 (cons (pedir-carta baraja 1) jugador2) jugador3 (pedir-carta baraja 2) 4  in out plantarsec plantarsej  plantarsej2 plantarsej3)))))))
                  
                )
                        
                          
              ;; Si la respuesta no es "s", continúa con la lógica del juego
              (begin
                (if(= plantarsec plantarsej plantarsej3 1)
                   (bCEj X crupier jugador1 jugador2 jugador3 baraja 3 in out plantarsec plantarsej 1 plantarsej3)
                   (begin
                     (if(= plantarsec plantarsej3 1)
                        (bCEj X crupier jugador1 jugador2 jugador3 baraja 2 in out plantarsec plantarsej 1 plantarsej3)
                        (begin
                          (if (= plantarsej3 1)
                              (bCEj X crupier jugador1 jugador2 jugador3 baraja 1 in out plantarsec plantarsej 1 plantarsej3)
                              (bCEj X crupier jugador1 jugador2 jugador3 baraja 4 in out plantarsec plantarsej 1 plantarsej3)
                              )))))



                ))))




       
       ((and (> (length jugador3) 1) (= turno 4) (= plantarsej3 0))
        (send-wait in out (string-append "turno" (number->string turno)) 0)
        (displayln "Turno del jugador 3")
        (displayln jugador2)
        (let ((respuesta (send-wait in out "¿Desea pedir carta? (s/n): " 1))) ; Espera la respuesta del cliente
          (if (string=? respuesta "s")
              
              (begin
            
                ;; Envía la carta pedida al cliente
                (send-wait in out (string-append "jg3" (symbol->string (pedir-carta baraja 1))) 0)


                (begin
                  (if(= plantarsec plantarsej plantarsej2 1)
                     (bCEj X crupier jugador1 jugador2 (cons (pedir-carta baraja 1) jugador3) (pedir-carta baraja 2) 4  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                     (begin
                       (if(= plantarsec plantarsej 1)
                          (bCEj X crupier jugador1 jugador2 (cons (pedir-carta baraja 1) jugador3) (pedir-carta baraja 2) 3  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                          (begin
                            (if (= plantarsec 1)
                                (bCEj X crupier jugador1 jugador2 (cons (pedir-carta baraja 1) jugador3) (pedir-carta baraja 2) 2  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                                (bCEj X crupier jugador1 jugador2 (cons (pedir-carta baraja 1) jugador3) (pedir-carta baraja 2) 1  in out plantarsec plantarsej  plantarsej2 plantarsej3)))))))
                  
                )
                        
                          
              ;; Si la respuesta no es "s", continúa con la lógica del juego
              (begin
                (if(= plantarsec plantarsej plantarsej2 1)
                   (bCEj X crupier jugador1 jugador2 jugador3 baraja 4 in out plantarsec plantarsej plantarsej2 1)
                   (begin
                     (if(= plantarsec plantarsej 1)
                        (bCEj X crupier jugador1 jugador2 jugador3 baraja 3 in out plantarsec plantarsej plantarsej2 1)
                        (begin
                          (if (= plantarsec 1)
                              (bCEj X crupier jugador1 jugador2 jugador3 baraja 2 in out plantarsec plantarsej plantarsej2 1)
                              (bCEj X crupier jugador1 jugador2 jugador3 baraja 1 in out plantarsec plantarsej plantarsej2 1)
                              )))))



                ))))



       
       ((and (> (length crupier) 1) (= turno 1) (= plantarsec 0))
        (send-wait in out (string-append "turno" (number->string turno)) 0)
        
        (displayln "Turno del crupier")
        (displayln crupier)
        
        (if (< (sumar-cartas (mover-as-al-final crupier)) 17)
            (begin
              
              (send-wait in out (string-append "crup" (symbol->string (pedir-carta baraja 1))) 0)
              (if(= plantarsej plantarsej2 plantarsej3 1)
                 (bCEj X (cons (pedir-carta baraja 1) crupier) jugador1 jugador2 jugador3 (pedir-carta baraja 2) 1  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                 (begin
                   (if(= plantarsej plantarsej2 1)
                      (bCEj X (cons (pedir-carta baraja 1) crupier) jugador1 jugador2 jugador3 (pedir-carta baraja 2) 4  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                      (begin
                        (if (= plantarsej 1)
                            (bCEj X (cons (pedir-carta baraja 1) crupier) jugador1 jugador2 jugador3 (pedir-carta baraja 2) 3  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                            (bCEj X (cons (pedir-carta baraja 1) crupier) jugador1 jugador2 jugador3 (pedir-carta baraja 2) 2  in out plantarsec plantarsej  plantarsej2 plantarsej3)
                            )))))

              )

            (begin
              (if(= plantarsej plantarsej2 plantarsej3 1)
                 (bCEj X crupier jugador1 jugador2 jugador3 baraja 1 in out 1 plantarsej plantarsej2 plantarsej3)
                 (begin
                   (if(= plantarsej plantarsej2 1)
                      (bCEj X crupier jugador1 jugador2 jugador3 baraja 4 in out 1 plantarsej plantarsej2 plantarsej3)
                      (begin
                        (if (= plantarsej 1)
                            (bCEj X crupier jugador1 jugador2 jugador3 baraja 3 in out 1 plantarsej plantarsej2 plantarsej3)
                            (bCEj X crupier jugador1 jugador2 jugador3 baraja 2 in out 1 plantarsej plantarsej2 plantarsej3)
                            )))))






              
              )))


       ((= plantarsej plantarsej2 plantarsej3 plantarsec 1)
        (send-wait in out (string-append "ptj1" (number->string (calcular-ganador (mover-as-al-final jugador1) (mover-as-al-final crupier)))) 0)
        (send-wait in out (string-append "ptj2" (number->string (calcular-ganador (mover-as-al-final jugador2) (mover-as-al-final crupier)))) 0)
        (send-wait in out (string-append "ptj3" (number->string (calcular-ganador (mover-as-al-final jugador3) (mover-as-al-final crupier)))) 0)
        (cond
          [(and (=(sumar-cartas (mover-as-al-final crupier))21) (= (length crupier)2))
           (send-wait in out "puntcbj" 0)
           
           


           ]
          
          


          
         
           



          )
        (send-wait in out "seacabo" 0)
        


        )

       ))
    
    
    (else (displayln "Fin del juego"))))




;; Define el servidor
(define (iniciar)
  (define listener (tcp-listen 9050 4 #t))

  (let loop ()
    (define-values (in out) (tcp-accept listener))
    (thread 
     (lambda ()
       (define (read)
         (read-line in 'return-linefeed))

       (define (send msg)  
         (log msg)
         (write-string (string-append msg "\r\n") out)
         (flush-output out))

       (define (log msg)
         (printf "\nS: ~a" msg))

       (define name "unknown")
       (define buffer empty)

       (define (list-to-string list del)
         (cond
           [(empty? list) ""]
           [else (string-append (format "~a~a" (first list) del) (list-to-string (rest list) del))]))

       (define (get-range list s e i str)
         (cond
           [(empty? list) str]
           [(if (and (<= s i) (>= e i))
                (get-range (rest list) s e (+ i 1) (string-append str (first list)))             
                (get-range (rest list) s e (+ i 1) str))]))

       (display "\nNew Client connected")     
   

       (let/ec done 
         (let loop ([buf buffer][n name])
           (define line (read))
           (printf "\nR: ~a" line)
           (when (eof-object? line)
             (set! line ""))                     

           ;; Esperar activamente el mensaje "start-game" antes de iniciar el juego
           (when (string=? "start-game" line)
             ;; Iniciar el juego cuando se recibe el mensaje "start-game"
             (define cartas '(Ah Ad Ac As 2h 2d 2c 2s 3h 3d 3c 3s 4h 4d 4c 4s 5h 5d 5c 5s 6h 6d 6c 6s 7h 7d 7c 7s 8h 8d 8c 8s 9h 9d 9c 9s 10h 10d 10c 10s Jh Jd Jc Js Qh Qd Qc Qs Kh Kd Kc Ks))
             (define baraja '())
             (define baraja-desordenada (crear-baraja cartas baraja))
             (bCEj 1 '() '() '() '() baraja-desordenada 1 in out 0 0 0 0)
             )
           (when (string=? "start-game2" line)
             
             ;; Iniciar el juego cuando se recibe el mensaje "start-game"
             (define cartas '(Ah Ad Ac As 2h 2d 2c 2s 3h 3d 3c 3s 4h 4d 4c 4s 5h 5d 5c 5s 6h 6d 6c 6s 7h 7d 7c 7s 8h 8d 8c 8s 9h 9d 9c 9s 10h 10d 10c 10s Jh Jd Jc Js Qh Qd Qc Qs Kh Kd Kc Ks))
             (define baraja '())
             (define baraja-desordenada (crear-baraja cartas baraja))
             (bCEj 2 '() '() '() '() baraja-desordenada 1 in out 0 0 0 0)
             )
           (when (string=? "start-game3" line)
             ;; Iniciar el juego cuando se recibe el mensaje "start-game"
             (define cartas '(Ah Ad Ac As 2h 2d 2c 2s 3h 3d 3c 3s 4h 4d 4c 4s 5h 5d 5c 5s 6h 6d 6c 6s 7h 7d 7c 7s 8h 8d 8c 8s 9h 9d 9c 9s 10h 10d 10c 10s Jh Jd Jc Js Qh Qd Qc Qs Kh Kd Kc Ks))
             (define baraja '())
             (define baraja-desordenada (crear-baraja cartas baraja))
             (bCEj 3 '() '() '() '() baraja-desordenada 1 in out 0 0 0 0)
             )

           (loop buf n)))

       (display "\nClient disconnected")
       (close-input-port in)
       (close-output-port out)))(loop)))

(iniciar)