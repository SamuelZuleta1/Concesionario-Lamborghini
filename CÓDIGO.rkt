

#lang racket/gui
(require (lib "graphics.ss" "graphics"))
(open-graphics)

(define chao #f) ; Definimos chao como una variable global para poder acceder a ella desde diferentes funciones
(define ventana #f)

;Poner la imagen de interfaz y rectangulos

(define (inicio)
  (set! ventana (open-viewport "Concesionario Lamborghini" 700 700))
  ((draw-pixmap ventana ) "--------------CAMBIAR-----------------" (make-posn 0 0 ))
  (define p1 (make-posn 200 40))
  (define p2 (make-posn 377 40))
  (define p3 (make-posn 540 40))
  (define ancho 90)
  (define alto 40)
  (define color (make-rgb 1.0 1.0 1.0))
  
  ;verifica a que boton se da click
  
  (define (click-rectangulo p1 p2 p3 ancho alto color descriptor)
    (if  (= 1 (verificar p1 ancho alto (mouse-click-posn descriptor)))
         (begin
           (close-viewport ventana)
          (boton1 #t))
         (if (= 2 (verificar2 p2 ancho alto (mouse-click-posn descriptor)))
             (begin
                (close-viewport ventana)
                (boton2 #t))
             (if  (= 3 (verificar3 p3 ancho alto (mouse-click-posn descriptor)))
                  (begin
                    (close-viewport ventana)
                    (boton3 #t))
                   (dentro-rectangulo p1 p2 p3 ancho alto color)
                   )
             )
         )
    )
                  
                   
  
  (define (verificar p1 ancho alto punto)
    (if (and (>= (posn-x punto) (posn-x p1))
             (<= (posn-x punto) (+ ancho (posn-x p1)))
             (>= (posn-y punto) (posn-y p1))
             (<= (posn-y punto) (+ alto (posn-y p1))))
        1
        0))
  
  (define (verificar2 p2 ancho alto punto2)
    (if (and (>= (posn-x punto2) (posn-x p2))
             (<= (posn-x punto2) (+ ancho (posn-x p2)))
             (>= (posn-y punto2) (posn-y p2))
             (<= (posn-y punto2) (+ alto (posn-y p2))))
        2
        0))
  
  (define (verificar3 p3 ancho alto punto3)
    (if (and (>= (posn-x punto3) (posn-x p3))
             (<= (posn-x punto3) (+ ancho (posn-x p3)))
             (>= (posn-y punto3) (posn-y p3))
             (<= (posn-y punto3) (+ alto (posn-y p3))))
          3
          0))
  
  (define (dentro-rectangulo p1 p2 p3 ancho alto color)
    (begin
      ((draw-rectangle ventana) p1 ancho alto color)
      ((draw-rectangle ventana) p2 ancho alto color)
      ((draw-rectangle ventana) p3 ancho alto color)
      (click-rectangulo p1 p2 p3 ancho alto color (get-mouse-click ventana))))
  
  (dentro-rectangulo p1 p2 p3 ancho alto color)
)

;Mostrar dos fotos por modelo (Funciona)

(define (boton1 mostrar)
  (if mostrar
      (begin
        (set! chao (new frame% [label "Modelos"]
                               [width 400]
                               [height 100]))
        
        (new message% [label "Seleccione el modelo de su prefencia"]
                      [parent chao])
        
        (new button% [label "HURACAN EVO"]
                     [parent chao]
                     [callback (lambda (button event)
                                 
                                (define foto1 (new frame%
[label "Previsualizacion Huracán EVO"]
[width 400]
[height 500]))

(define img1 (make-object bitmap% "--------------CAMBIAR-----------------"))
(define img2 (make-object bitmap% "--------------CAMBIAR-----------------"))
(define current-image img1)
(define canvas (new canvas%
[parent foto1]
[paint-callback
(lambda (canvas dc)
(send dc draw-bitmap current-image 0 0))]))
(define button (new button%
[parent foto1]
[label "Otra vista"]
[callback
(lambda (button event)
(if (equal? current-image img1)
(set! current-image img2)
(set! current-image img1))
(send canvas refresh))]))
(send foto1 show #t))])
       
         (new button% [label "URUS S"]
                     [parent chao]
                     [callback (lambda (button event)
  (define foto2 (new frame%
[label "Previsualizacion Urus S"]
[width 400]
[height 500]))

(define img1 (make-object bitmap% "--------------CAMBIAR-----------------"))
(define img2 (make-object bitmap% "--------------CAMBIAR-----------------"))
(define current-image img1)
(define canvas (new canvas%
[parent foto2]
[paint-callback
(lambda (canvas dc)
(send dc draw-bitmap current-image 0 0))]))

(define button (new button%
[parent foto2]
[label "Otra vista"]
[callback
(lambda (button event)
(if (equal? current-image img1)
(set! current-image img2)
(set! current-image img1))
(send canvas refresh))]))

(send foto2 show #t))])
         
          (new button% [label "VENENO"]
                     [parent chao]
                     [callback (lambda (button event)
                                   (define foto3 (new frame%
[label "Previsualizacion Veneno"]
[width 400]
[height 500]))

(define img1 (make-object bitmap% "--------------CAMBIAR-----------------"))
(define img2 (make-object bitmap% "--------------CAMBIAR-----------------"))
(define current-image img1)
(define canvas (new canvas%
[parent foto3]
[paint-callback
(lambda (canvas dc)
(send dc draw-bitmap current-image 0 0))]))

(define button (new button%
[parent foto3]
[label "Otra vista"]
[callback
(lambda (button event)
(if (equal? current-image img1)
(set! current-image img2)
(set! current-image img1))
(send canvas refresh))]))

(send foto3 show #t))])
                   
          (new button% [label "Menu Principal "]
                     [parent chao]
                     [callback (lambda (button event)
                                 (send chao show #f)
                                 (inicio))])
          
        
        (send chao show #t))
      (send chao show #f)))


; Accion boton 2 (mostrar precios)

( define ( boton2 mostrar )
     (if mostrar
      (begin
        (set! chao (new frame% [label "Precios"]
                               [width 400]
                               [height 100]))
        
        (new message% [label "Consulte el precio del carro deseado "]
                      [parent chao])

        (new button% [label "HURACAN EVO"]
                     [parent chao]
                     [callback (lambda (button event)
                                 (define nueva-ventana (new frame% [label "Precio del HURACAN EVO"]
                                                                [width 400]
                                                                [height 100]))
                                 (new message% [label "El precio de nuestro modelo Huracan EVO es de $250.000 dólares o de 1.000.000.000 COP"]
                                               [parent nueva-ventana])
                                
                              
                                 (send nueva-ventana show #t))])
        
        
        
         (new button% [label "URUS S"]
                     [parent chao]
                     [callback (lambda (button event)
                                 (define nueva-ventana (new frame% [label "Precio de camioneta URUS S"]
                                                                [width 400]
                                                                [height 100]))
                                 (new message% [label "El precio de nuestro modelo URUS S es de $270.000 dólares o de 1.080.000.000 COP"]
                                               [parent nueva-ventana])
                              
                                 (send nueva-ventana show #t))])
                                 
                               
         
          (new button% [label "VENENO"]
                     [parent chao]
                     [callback (lambda (button event)
                                 (define nueva-ventana (new frame% [label "Precio del Lamborghini Veneno"]
                                                                [width 400]
                                                                [height 100]))
                                 (new message% [label "El precio de nuestro modelo Veneno es de $6.5 millones de dólares o de 26.000.000.000 COP"]
                                               [parent nueva-ventana])
                           
                               
                                 (send nueva-ventana show #t))])

        
        (new button% [label "Menu Principal "]
                     [parent chao]
                     [callback (lambda (button event)
                                 (send chao show #f)
                                 (inicio))])
        (send chao show #t))
      (send chao show #f)))


; FACTURACION ELECTRONICA

(define (boton3 mostrar)
  (define nombre 0)
  (define documento 0)
  (define celular 0)
  (define direccion 0)
  (define modelo 0)
  (define pago 0)

  (if mostrar
      (begin
        (set! chao (new frame% [label "Compra tu vehiculo "]
                               [width 400]
                               [height 300]))

        (new text-field% [label " Nombre "]
                         [parent chao]
                         [callback (lambda (tf event)
                                     (set! nombre (send tf get-value)))])

        (new text-field% [label " Documento "]
                         [parent chao]
                         [callback (lambda (tf event)
                                     (set! documento (send tf get-value)))])

        (new text-field% [label " Celular "]
                         [parent chao]
                         [callback (lambda (tf event)
                                     (set! celular (send tf get-value)))])

        (new text-field% [label " Direccion "]
                         [parent chao]
                         [callback (lambda (tf event)
                                     (set! direccion (send tf get-value)))])

        (new text-field% [label " Modelo "]
                         [parent chao]
                         [callback (lambda (tf event)
                                     (set! modelo (send tf get-value)))])

        (new text-field% [label " Forma de pago "]
                         [parent chao]
                         [callback (lambda (tf event)
                                     (set! pago (send tf get-value)))])

        (new button% [label "Finalizar registro"]
                     [parent chao]
                     [callback (lambda (button event)
                                 (define factura (new frame% [label "Recibo"]
                                                            [width 400]
                                                            [height 150]))
                                 
                                 (define resultado-label (new message%
                                                           [parent factura]
                                                           [label (format "Nombre: ~a\nDocumento: ~a\nCelular: ~a\nDireccion: ~a\nModelo: ~a\nPago: ~a" nombre documento celular
                                                                          direccion modelo pago)]))

                                 (new button% [label "Finalizar compra"]
                                              [parent factura]
                                              [callback (lambda (button event)
                                                          (send factura show #f)
                                                          (send chao show #f)
                                                          (mostrar-mensaje-agradecimiento))])

                                 (send chao show #f)
                                 (send factura show #t))])

        (new button% [label "Menu Principal "]
                     [parent chao]
                     [callback (lambda (button event)
                                 (send chao show #f)
                                 (inicio))])

        (send chao show #t))
      (send chao show #f)))


(define (mostrar-mensaje-agradecimiento)
  (define mensaje (new frame% [label "¡Gracias por tu compra!"]
                                   [width 300]
                                   [height 100]))
  
  (define mensaje-texto (new message% [parent mensaje]
                                      [label "Tu compra ha sido procesada con éxito, el vehículo será entregado en la dirección ingresada en un plazo de 2 diás hábiles"]))
   (new button% [label "Menu Principal "]
                     [parent mensaje]
                     [callback (lambda (button event)
                                 (send chao show #f)
                                 (send mensaje show #f)
                                 (inicio))])
 
  (send mensaje show #t))
(inicio)
;
