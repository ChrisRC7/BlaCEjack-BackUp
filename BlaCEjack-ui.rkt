#lang racket/gui

(require racket/tcp)

(define frame (new frame% [label "Ventana con Canvas"]
                          [width 815]
                          [height 620]))



(define my-canvas%
  (class canvas% ; La clase base es canvas%
    ; Define un método de anulación para manejar eventos de ratón
    (define/override (on-event event)
      (let ([x (send event get-x)]
            [y (send event get-y)])
        (when (send event button-down? 'left)
         
          ; Verifica si las coordenadas están dentro del rectángulo especificado
          (cond
            [(and (>= x 275) (<= x 525) (>= y 153) (<= y 208))
             (bCEj 1)]
            [(and (>= x 272) (<= x 524) (>= y 265) (<= y 322))
             (bCEj 2)]
            [(and (>= x 272) (<= x 524) (>= y 373) (<= y 430))
             (bCEj 3)]
            [else
             (displayln "inicio")]))))
    ; Define un método de anulación para manejar eventos de teclado
    
    ; Llama al init de la superclase, pasando todos los argumentos de init
    (super-new)))

(new my-canvas% [parent frame]
                [paint-callback
                 (lambda (canvas dc)
                   (send dc draw-bitmap (read-bitmap "bg-menu.png") 0 0))])

(send frame show #t)





(define (bCEj X)
  
(define (agregar-canvas path panel)
  (new canvas%
       [parent panel]
       [paint-callback
        (lambda (canvas dc)
          (send dc draw-bitmap (read-bitmap path) 0 0))]))

(define (client host port)
  ; Connect to server
  (define-values (in out) (tcp-connect host port))

  ; Function to send a message to the server
  (define (send-msg msg)
    (write-string (string-append (string-trim msg) "\r\n") out)
    (flush-output out))
  
 
  ; Define the GUI components
  (define (agregar-canvas path panel)
    (new canvas%
         [parent panel]
         [paint-callback
          (lambda (canvas dc)
            (send dc draw-bitmap (read-bitmap path) 0 0))]))



  (define frame (new frame%
                     [label "BlaCEjack"]
                     [width 800]
                     [height 600]
                     [style (list 'no-resize-border)]
                     ))
  (agregar-canvas "bg2.png" frame)
 
  (define panel-vertical (new vertical-panel% [parent frame]))
  (define panel-derecho (new vertical-panel% [parent panel-vertical]))

  ; Make a static text message in the frame
  (define msg (new message% [parent frame]
                   [label "No events so far..."]))
  ; Panel izquierdo con dos paneles horizontales
  (define panel-jugadores (new horizontal-panel% [parent panel-vertical]
                               [horiz-margin 20]))
  
    
    (define panel-jugador-2 (new vertical-panel% [parent panel-jugadores][horiz-margin 10]))
  (define panel-jugador (new vertical-panel% [parent panel-jugadores][horiz-margin 10]
                             ))
  
  (define panel-jugador-1 (new vertical-panel% [parent panel-jugadores][horiz-margin 10]))
  
  (define panel-jugador-1-text (new horizontal-panel% [parent panel-jugador-1]))

  (define panel-jugador-1-cards (new horizontal-panel% [parent panel-jugador-1]
                                     [min-height 130]))
(new canvas% [parent panel-jugador-1-text]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-scale 1 1)
        ; Crear la fuente decorativa para el juego de mesa
        (define the-font (make-font #:size 13 #:family 'modern #:weight 'bold)) ; Definir la fuente
        (define font-color (make-object color% 0 100 0)) ; Verde oscuro
        (send dc set-font the-font) ; Establecer la fuente
        (send dc set-text-foreground font-color)
        (send dc draw-text "Jugador 2" 0 0))])


  
  
  (define panel-textJugador (new horizontal-panel% [parent panel-jugador]
                                 ))

  (define panel-cardsJugador (new horizontal-panel% [parent panel-jugador]
                                  [min-height 110] ))

  (new canvas% [parent panel-textJugador]
       [paint-callback
        (lambda (canvas dc)
          (send dc set-scale 1 1)
                  ; Crear la fuente decorativa para el juego de mesa
        (define the-font (make-font #:size 13 #:family 'modern #:weight 'bold)) ; Definir la fuente
          (define font-color (make-object color% 0 100 0)) ; Verde oscuro
        (send dc set-font the-font) ; Establecer la fuente
          (send dc set-text-foreground font-color)
          (send dc draw-text "Jugador 1" 0 0))])
(define ptdj1 0)
(define ptdj2 0)
(define ptdj3 0)

  
(define texto-jugador-1 (string-append "Puntos: " (number->string ptdj1)))
(define texto-jugador-2 (string-append "Puntos: " (number->string ptdj1)))
  (define texto-jugador-3 (string-append "Puntos: " (number->string ptdj1)))




  
  (define panel-jugador-2-text (new horizontal-panel% [parent panel-jugador-2]))
  (define panel-jugador-2-cards (new horizontal-panel% [parent panel-jugador-2]
                                     
                                     [min-height 110]))
  (new canvas% [parent panel-jugador-2-text]
       [paint-callback
        (lambda (canvas dc)
          (send dc set-scale 1 1)
                  ; Crear la fuente decorativa para el juego de mesa
        (define the-font (make-font #:size 13 #:family 'modern #:weight 'bold)) ; Definir la fuente
          (define font-color (make-object color% 0 100 0)) ; Verde oscuro
        (send dc set-font the-font) ; Establecer la fuente
          (send dc set-text-foreground font-color)
          (send dc draw-text "Jugador 3" 0 0))])
(define canvas
  (new canvas%
       [parent panel-textJugador]
       [paint-callback
        (lambda (canvas dc)
          (send dc set-scale 1 1)
                  ; Crear la fuente decorativa para el juego de mesa
        (define the-font (make-font #:size 12 #:family 'modern #:weight 'bold)) ; Definir la fuente
        (send dc set-font the-font) ; Establecer la fuente
          (send dc set-text-foreground "blue")
          (send dc draw-text texto-jugador-1 0 0))]))

  (define canvas2
  (new canvas%
       [parent panel-jugador-1-text]
       [paint-callback
        (lambda (canvas dc)
          (send dc set-scale 1 1)
                  ; Crear la fuente decorativa para el juego de mesa
        (define the-font (make-font #:size 12 #:family 'modern #:weight 'bold)) ; Definir la fuente
        (send dc set-font the-font) ; Establecer la fuente
          (send dc set-text-foreground "blue")
          (send dc draw-text texto-jugador-2 0 0))]))

  (define canvas3
  (new canvas%
       [parent panel-jugador-2-text]
       [paint-callback
        (lambda (canvas dc)
          (send dc set-scale 1 1)
                  ; Crear la fuente decorativa para el juego de mesa
        (define the-font (make-font #:size 12 #:family 'modern #:weight 'bold)) ; Definir la fuente
        (send dc set-font the-font) ; Establecer la fuente
          (send dc set-text-foreground "blue")
          (send dc draw-text texto-jugador-3 0 0))]))

  ; Panel derecho
  (define panel-crupier (new horizontal-panel% [parent panel-derecho]))
  (define panel-izqc (new horizontal-panel% [parent panel-crupier]))
  (agregar-canvas "bg3.png" panel-izqc)
  
  (define panel-dealer (new vertical-panel% [parent panel-crupier]
                            [vert-margin 10]
                            [horiz-margin 10]
                            [min-width 190]
                            [min-height 140]
                            ))


  
  (define panel-derc (new horizontal-panel% [parent panel-crupier]))
  (agregar-canvas "bg3.png" panel-derc)
  (define panel-textCrupier (new horizontal-panel% [parent panel-dealer]
                                 [min-height 0]))
  (define panel-cardsCrupier (new horizontal-panel% [parent panel-dealer]
                                  [enabled 0]
                                  [min-height 90]
                                
                                  ))
  
  (new canvas% [parent panel-textCrupier]
       [paint-callback
        (lambda (canvas dc)
          
                  ; Crear la fuente decorativa para el juego de mesa
        (define the-font (make-font #:size 15 #:family 'modern #:weight 'bold)) ; Definir la fuente
          (define font-color (make-object color% 0 100 0)) ; Verde oscuro
        (send dc set-font the-font) ; Establecer la fuente
          (send dc set-text-foreground font-color)
          (send dc draw-text "Crupier" 0 0))])








  (define panel-botones (new horizontal-panel% [parent panel-derecho]
                           
                             ))
  
  (define panel-izqb (new horizontal-panel% [parent panel-botones]
                          ))
  (agregar-canvas "bg3.png" panel-izqb)
  ; Create buttons for Pedir and Plantarse
  (define btn-pedir (new button% [parent panel-botones]
                         
                         [label "Pedir"]
                         [callback (lambda (button event)
                                     (send-msg "s"))]))

  (define btn-plantarse (new button% [parent panel-botones]
                             [label "Plantarse"]
                             [callback (lambda (button event)
                                         (send-msg "n"))]))


  (define siguiente 1)
  (define tag 0)
  (define siguiente-partida (new button% [parent panel-botones]
                          [label "Jugar de nuevo"]
                          [callback (lambda (button event)
                                      (set! siguiente (cond [ (and (= siguiente 1) (= tag 1)) 0]
                                                            [1]))
                                      (message-box "Jugar de nuevo" "Si ya terminaste tu partida actual al presionar el boton OK se iniciara una nueva, de lo contrario primero debes terminar la partida actual"))]))
  (define tabla-posiciones (new button% [parent panel-botones]
                          [label "Podio"]
                          [callback (lambda (button event)
                                      ;; Crear el podio
                                      
                                      (define podio (crear-podium 'jugador1 'jugador2 'jugador3 ptdj1 ptdj2 ptdj3))

                                      ;; Ordenar el podio usando Quicksort según los puntos
                                      (define podio-ordenado (revertir (quicksort podio)))

                                      ;; Convertir el podio ordenado a texto
                                      (define podio-texto (podio-a-texto podio-ordenado))
                                      (message-box "Tabla Pocisiones" podio-texto))]))

  (define panel-derb (new horizontal-panel% [parent panel-botones]
                          ))

  




(define panel-jugador-turno (new horizontal-panel% [parent panel-jugador]))
  (agregar-canvas "flag.png" panel-jugador-turno)

  (define panel-jugador-1-turno (new horizontal-panel% [parent panel-jugador-1]))
  (agregar-canvas "flag.png" panel-jugador-1-turno)

  (define panel-jugador-2-turno (new horizontal-panel% [parent panel-jugador-2]))
  (agregar-canvas "flag.png" panel-jugador-2-turno)



  
  
(send panel-jugador delete-child panel-jugador-turno)



  

  (cond
    [(= X 3)
     
     (send-msg "start-game3")
     ]
    [(= X 2)
     (send panel-jugadores delete-child panel-jugador-2)
     (send-msg "start-game2")
     ]
    [(= X 1)
     (send-msg "start-game")
     (send panel-jugadores delete-child panel-jugador-2)
     (send panel-jugadores delete-child panel-jugador-1)
     ]

    )

(define (crear-podium jugador1 jugador2 jugador3 pt1 pt2 pt3)
  (list (list jugador1 pt1)
        (list jugador2 pt2)
        (list jugador3 pt3)))

(define (quicksort lst)
  (cond
    [(empty? lst) empty]
    [else
     (let* ([pivot (car lst)]
            [less (filter (lambda (x) (< (cadr x) (cadr pivot))) (cdr lst))]
            [greater (filter (lambda (x) (>= (cadr x) (cadr pivot))) (cdr lst))])
       (append (quicksort less) (list pivot) (quicksort greater)))]))

(define (podio-a-texto podio)
  (define (formato-sublista sublista)
    (string-append (symbol->string (car sublista)) " " (number->string (cadr sublista)) "\n"))
  (string-join (map formato-sublista podio) ""))

  
  (define (revertir lista)
    (define (revertir-aux lista acumulador)
      (if (null? lista)
          acumulador
          (revertir-aux (cdr lista) (cons (car lista) acumulador))))
    (revertir-aux lista '()))
  (define (recorrer-lista lista)
    (cond
      [(empty? lista) '()]
      [else
       (begin
         (displayln (string-append "primero " (car lista)))
         (agregar-canvas (string-append "cards/" (car lista) ".png") panel-cardsCrupier ))
       (recorrer-lista (cdr lista))]))
  (agregar-canvas "bg3.png" panel-derb)
  (define crupier-substrings '())

  ; Start the read loop
  (define (read-loop)
    (define childrenj2 (send panel-jugador-1-cards get-children))
    (define childrenj3 (send panel-jugador-2-cards get-children))

    (define childrenjt1 (send panel-jugador get-children))
    (define childrenjt2 (send panel-jugador-1 get-children))
    (define childrenjt3 (send panel-jugador-2 get-children))
    
    (define children (send panel-cardsJugador get-children))
    (define childrencrup (send panel-cardsCrupier get-children))
    (define (read)
      (read-line in 'return-linefeed))
    (define line (read))
    
    (unless (eof-object? line)
      (display (format "~a\n" line))
      ; Verificar si el mensaje tiene solo dos caracteres
      (cond
        
        [(and (string=? line "ptj11") (<= (string-length line) 7))
         (begin
           
           (begin
             (message-box "Resultado" "Jugador 1 le gano al crupier +1 punto")
             (set! ptdj1 (+ ptdj1 1)) ;; Decrementa ptdj1 en 1
             (set! texto-jugador-1 (string-append "Puntos: " (number->string ptdj1)))

             (send canvas refresh))
           (displayln "")
           )
         ]
        [(and (string=? line "ptj1-1") (<= (string-length line) 7))
         (begin
           
           (begin
             (message-box "Resultado" "Jugador 1 perdio contra el crupier -1 punto")
             (set! ptdj1 (- ptdj1 1)) ;; Decrementa ptdj1 en 1
             (set! texto-jugador-1 (string-append "Puntos: " (number->string ptdj1)))

             (send canvas refresh))
           (displayln "")
           )]



        [(and (string=? line "ptj21") (<= (string-length line) 7))
         (begin
           
           (begin
             (message-box "Resultado" "Jugador 2 le gano al crupier +1 punto")
             (set! ptdj2 (+ ptdj2 1)) ;; Decrementa ptdj1 en 1
             (set! texto-jugador-2 (string-append "Puntos: " (number->string ptdj2)))

             (send canvas2 refresh))
           (displayln "")
           )
         ]
        [(and (string=? line "ptj2-1") (<= (string-length line) 7))
         (begin
           
           (begin
             (message-box "Resultado" "Jugador 2 perdio contra el crupier -1 punto")
             (set! ptdj2 (- ptdj2 1)) ;; Decrementa ptdj1 en 1
             (set! texto-jugador-2 (string-append "Puntos: " (number->string ptdj2)))

             (send canvas2 refresh))
           (displayln "")
           )]




        [(and (string=? line "ptj31") (<= (string-length line) 7))
         (begin
           
           (begin
             
             (message-box "Resultado" "Jugador 3 le gano al crupier +1 punto")
             (set! ptdj3 (+ ptdj3 1)) ;; Decrementa ptdj1 en 1
             (set! texto-jugador-3 (string-append "Puntos: " (number->string ptdj3)))

             (send canvas3 refresh))
           (displayln "")
           )
         ]
        [(and (string=? line "ptj3-1") (<= (string-length line) 7))
         (begin
           
           (begin
               
             (message-box "Resultado" "Jugador 3 perdio contra el crupier -1 punto")

             (set! ptdj3 (- ptdj3 1)) ;; Decrementa ptdj1 en 1
             (set! texto-jugador-3 (string-append "Puntos: " (number->string ptdj3)))

             (send canvas3 refresh))
           (displayln "")
           )]
        
        [(and (string=? line "seacabo") (<= (string-length line) 7))
         (begin
           (set! line "")
           (for ([child childrencrup])
             ;; Eliminar el label del panel
             (send panel-cardsCrupier delete-child child))
           (recorrer-lista (revertir crupier-substrings))
           ;; Agregar un tiempo de espera para que se presione un botón
           
           (set! tag 1)
           (let loop ()
             (when (= siguiente 1)
               
               (sleep 1) ; tiempo de espera en segundos
               (loop))) ; llamada recursiva a loop hasta que la condición se cumpla
           (set! siguiente 1)
           (set! tag 0)
           ;; Iterar sobre los hijos para encontrar el label
           (for ([child children])
             ;; Eliminar el label del panel
             (send panel-cardsJugador delete-child child))
           (define childrencrup2 (send panel-cardsCrupier get-children))
           (for ([child childrencrup2])
             ;; Eliminar el label del panel
             (send panel-cardsCrupier delete-child child))

           (set! crupier-substrings '())

           (cond
             [(= X 2)
              (for ([child childrenj2])
                ;; Eliminar el label del panel
                (send panel-jugador-1-cards delete-child child))
              (send-msg "start-game2")]
  
             [(= X 3)
              (for ([child childrenj2])
                ;; Eliminar el label del panel
                (send panel-jugador-1-cards delete-child child))
              (for ([child childrenj3])
                ;; Eliminar el label del panel
                (send panel-jugador-2-cards delete-child child))
              (send-msg "start-game3")]
  
             [else
              (send-msg "start-game")])

           
         
           
           )]
        [(> 4 (string-length (string-trim line)))
         (begin
           
           (agregar-canvas (string-append "cards/" line ".png") panel-cardsJugador)

           (display line)
           (newline))]
        [(and (string=? (substring line 0 3) "jg2") (<= (string-length line) 7))
         (begin
           
           
           (agregar-canvas (string-append "cards/" (substring line 3) ".png") panel-jugador-1-cards)
               
               
           
           )]
        [(and (string=? line "turno1") (<= (string-length line) 7))
         (begin
           

           (if (>(length childrenjt1) 2)
           (send panel-jugador delete-child panel-jugador-turno)
           (displayln ""))


           
           (if (>(length childrenjt2) 2)
           (send panel-jugador-1 delete-child panel-jugador-1-turno)
           (displayln ""))
           (if (>(length childrenjt3) 2)
           (send panel-jugador-2 delete-child panel-jugador-2-turno)
           (displayln ""))
               
           
           )]
        [(and (string=? line "turno2") (<= (string-length line) 7))
         (begin
           

           (if (<(length childrenjt1) 3)
           (send panel-jugador add-child panel-jugador-turno)
           (displayln ""))


           
           (if (>(length childrenjt2) 2)
           (send panel-jugador-1 delete-child panel-jugador-1-turno)
           (displayln ""))
           (if (>(length childrenjt3) 2)
           (send panel-jugador-2 delete-child panel-jugador-2-turno)
           (displayln ""))
               
           
           )]
        [(and (string=? line "turno3") (<= (string-length line) 7))
         (begin
           
           (displayln (length childrenjt2))
           (if (>(length childrenjt1) 2)
           (send panel-jugador delete-child panel-jugador-turno)
           (displayln ""))

           (if (< (length childrenjt2) 3)
               (send panel-jugador-1 add-child panel-jugador-1-turno)
               (displayln ""))

           
           (if (>(length childrenjt2) 2)
               (send panel-jugador-2 delete-child panel-jugador-2-turno)
               (displayln "")
               )
               
           
           )]
        [(and (string=? line "turno4") (<= (string-length line) 7))
         (begin
           
           (if (> (length childrenjt1) 2)
           (send panel-jugador delete-child panel-jugador-turno)
           (displayln ""))
           (if (>(length childrenjt2) 2)
           (send panel-jugador-1 delete-child panel-jugador-1-turno)
           (displayln ""))

           (if (< (length childrenjt3) 3)
           (send panel-jugador-2 add-child panel-jugador-2-turno)
           (displayln "")
           )
               
           
           )]
        [(and (string=? (substring line 0 3) "jg3") (<= (string-length line) 7))
         (begin
           
           
           (agregar-canvas (string-append "cards/" (substring line 3) ".png") panel-jugador-2-cards)
               
               
           
           )]
        
        [(and (string=? (substring line 0 4) "crup") (<= (string-length line) 7))
         (begin
           (set! crupier-substrings (cons (substring line 4) crupier-substrings))
           (displayln crupier-substrings)
           (if (> (length childrencrup) 0)
           (agregar-canvas (string-append "cards/" (substring line 4) ".png") panel-cardsCrupier)
           (agregar-canvas (string-append "cards/card-back.png") panel-cardsCrupier)
           )
           
           )]
     

      
      [else
       'ignore])
    (read-loop)))


  
  (define rt (thread read-loop)) 
  
  ; Close client when the window is closed
  (define (on-close)
    (kill-thread rt)
    (close-input-port in)
    (close-output-port out))
  
  (send frame show #t)
)

(client "localhost" 9050)
)

