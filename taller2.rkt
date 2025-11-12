#lang racket

;; ================================================================
;;   PROGRAMACIÓN DECLARATIVA - TALLER 2
;;   Autor: Ernesto Briceño Magaña - 00025620
;; ================================================================


;; ================================================================
;; EJERCICIO 1 - Contar elementos positivos
;; ================================================================
(define (contar-positivos lista)
  (length (filter (lambda (x) (> x 0)) lista)))

(displayln "Ejercicio 1 – Elementos positivos:")
(displayln (string-append "Resultado: "
                          (number->string (contar-positivos '(3 -2 7 0 -5 9)))))
(displayln "------------------------------------------------------------")


;; ================================================================
;; EJERCICIO 2 - Generar lista de cuadrados pares
;; ================================================================
(define (cuadrados-pares lista)
  (map (lambda (x) (* x x))
       (filter even? lista)))

(displayln "Ejercicio 2 – Cuadrados pares:")
(displayln (cuadrados-pares '(1 2 3 4 5 6 7 8)))
(displayln "------------------------------------------------------------")


;; ================================================================
;; EJERCICIO 3 - Factorial recursivo
;; ================================================================
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(displayln "Ejercicio 3 – Factorial de 5:")
(displayln (factorial 5))
(displayln "------------------------------------------------------------")


;; ================================================================
;; EJERCICIO 4 - Elevar al cubo cada número
;; ================================================================
(define (cubos lista)
  (map (lambda (x) (* x x x)) lista))

(displayln "Ejercicio 4 – Cubos:")
(displayln (cubos '(2 3 4)))
(displayln "------------------------------------------------------------")


;; ================================================================
;; EJERCICIO 5 - Sumar elementos impares
;; ================================================================
(define (sumar-impares lista)
  (foldl + 0 (filter odd? lista)))

(displayln "Ejercicio 5 – Suma de impares:")
(displayln (sumar-impares '(1 2 3 4 5 6 7)))
(displayln "------------------------------------------------------------")


;; ================================================================
;; EJERCICIO 6 - Determinar si hay negativos
;; ================================================================
(define (contiene-negativo? lista)
  (ormap (lambda (x) (< x 0)) lista))

(displayln "Ejercicio 6 – Contiene negativos:")
(displayln (contiene-negativo? '(5 9 -3 2)))
(displayln "------------------------------------------------------------")


;; ================================================================
;; EJERCICIO 7 - Suma acumulada
;; ================================================================
(define (suma-acumulada lista)
  (reverse
   (second
    (foldl (lambda (x acc)
             (let* ([suma-previa (first acc)]
                    [lista-previa (second acc)]
                    [nueva-suma (+ suma-previa x)])
               (list nueva-suma (cons nueva-suma lista-previa))))
           (list 0 '())
           lista))))

(displayln "Ejercicio 7 – Suma acumulada:")
(displayln (suma-acumulada '(1 2 3 4)))
(displayln "------------------------------------------------------------")


;; ================================================================
;; EJERCICIO 8 - Concatenar cadenas
;; ================================================================
(define (concatenar-cadenas lista)
  (foldl string-append "" lista))

(displayln "Ejercicio 8 – Concatenar cadenas:")
(displayln (concatenar-cadenas '("Hola" " " "Mundo")))
(displayln "------------------------------------------------------------")


;; ================================================================
;; EJERCICIO 9 - Doble de números mayores que 5
;; ================================================================
(define (dobles-mayores-5 lista)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lista)))

(displayln "Ejercicio 9 – Doble de mayores a 5:")
(displayln (dobles-mayores-5 '(3 6 8 2 10)))
(displayln "------------------------------------------------------------")


;; ================================================================
;; EJERCICIO 10 - Invertir lista
;; ================================================================
(define (invertir lista)
  (foldl (lambda (x acc) (cons x acc)) '() lista))

(displayln "Ejercicio 10 – Lista invertida:")
(displayln (invertir '(1 2 3 4)))
(displayln "------------------------------------------------------------")


;; ================================================================
;; EJERCICIO 11 - Función que recibe función
;; ================================================================
(define (aplicar-funcion f lista)
  (map f lista))

(define (cuadrado x) (* x x))

(displayln "Ejercicio 11 – Aplicar función cuadrado:")
(displayln (aplicar-funcion cuadrado '(1 2 3 4)))
(displayln "------------------------------------------------------------")


;; ================================================================
;; EJERCICIO 12 - Promedio de mayores a 5 (Reto integrador)
;; ================================================================
(define (promedio-mayores-5 lista)
  (let* ([mayores (filter (lambda (x) (> x 5)) lista)]
         [suma (foldl + 0 mayores)]
         [cantidad (length mayores)])
    (if (= cantidad 0) 0 (/ suma cantidad))))

(displayln "Ejercicio 12 – Promedio de números mayores a 5:")
(displayln (promedio-mayores-5 '(3 8 10 4 9 2 7)))
(displayln "------------------------------------------------------------")
