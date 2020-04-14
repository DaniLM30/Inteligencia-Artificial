#lang racket
;Ejemplo de como quedara el grafo despues de reducir el formato de entrada
#|(define *GRAFO*(list 
 (list "Cadiz"  (list "Sevilla" 125))
 (list "Sevilla"  (list "Cadiz" 125)(list "Jaen" 242) (list "Granada" 256))
 (list "Jaen" (list "Sevilla" 242)(list "Granada" 99)(list "Madrid" 335))
 (list "Granada" (list "Sevilla" 256)(list "Jaen" 99)(list "Murcia" 278))
 (list "Murcia" (list "Granada" 278)(list "Albacete" 150)(list "Valencia" 241))
 (list "Albacete" (list "Murcia" 150)(list "Valencia" 191)(list "Madrid" 251))
 (list "Valencia" (list "Murcia" 241)(list "Albacete" 191)(list "Barcelona" 349))
 (list "Madrid" (list "Jaen" 335)(list "Badajoz" 403)(list "Valladolid" 193)(list "Bilbao" 395)(list "Zaragoza" 325)(list"Albacete" 251))
 (list "Badajoz" (list "Madrid" 403))
 (list "Valladolid" (list "Madrid" 193)(list "Vigo" 356)(list "Coruña" 455)(list "Bilbao" 280))
 (list "Zaragoza" (list "Madrid" 325)(list "Bilbao" 324)(list "Barcelona" 296))
 (list "Barcelona" (list "Valencia" 349)(list "Zaragoza" 296)(list "Gerona" 100))
 (list "Gerona" (list "Barcelona" 100))
 (list "Vigo" (list "Coruña" 171)(list "Valladolid" 356))
 (list "Coruña" (list "Vigo" 171)(list "Valladolid" 455))
 (list "Bilbao" (list "Oviedo" 304)(list "Valladolid" 280)(list "Madrid" 395)(list "Zaragoza" 324))
 (list "Oviedo" (list "Bilbao" 304))
 )
)|#

;Crea cada linea del grafo
(define (crea-grafo line)
  (let ([lista (string-split line)])
    (cond
      [(empty? lista)'()]
      [else (list (car lista) ( list (cadr lista) (string->number (caddr lista))))])
    )
)

;Lee el grafo linea a linea
(define (read-graph file)
  (let ([line (read-line file 'any)])
    (if (eof-object? line)
        empty
        (cons (crea-grafo line) (read-graph file))
    )
  )
)

;Convierte el grafo generado de entrada al formato de listas de listas
(define (reduce-grafo grafo)
   (cond
    [(empty? grafo) grafo]
    ;Si existe algun duplicado se unen para que los destinos tengan un origen en comun
    [(not (empty?(comprueba-dobles (caar grafo)(cdr grafo)))) (cons (cons (caar grafo) (comprueba-dobles (caar grafo) grafo)) (reduce-grafo (elimina-dobles (caar grafo) grafo)))]
    ;Si no existen duplicados sobre el primer valor, se reduce el resto del grafo
    [else (cons (car grafo) (reduce-grafo (cdr grafo)))]
       )
  )

;Comprueba si distintos valores de la lista comparten ciudad origen y los une
(define (comprueba-dobles nodo grafo)
  (cond
    [(empty? grafo) empty]
    ;Si el coincide unimos con la busqueda en el resto del grafo
    [(equal? nodo (caar grafo)) (append (cdar grafo) (comprueba-dobles nodo (cdr grafo)))]
    ;Si no coincide, busca en el resto del grafo
    [else (comprueba-dobles nodo (cdr grafo))]
    ))

;Eliminamos los nodos que hayamos tenido que unir al reducir formato
(define (elimina-dobles nodo grafo)
  (cond
    [(empty? grafo) (list)]
    ;Si hay que eliminarlo lo ignoramos
    [(equal? nodo (caar grafo)) (elimina-dobles nodo (cdr grafo))]
    ;Si no hay que eliminarlo, creamos una lista con el nodo y eliminar del resto
    [else (cons (car grafo) (elimina-dobles nodo (cdr grafo)))]
    ))


;Estructura
(define-struct nodo (camino kilometros))


;Insertar ordenado un nodo en lista
(define (inserta-ordenado nodo lista)
  (cond
    [(empty? lista) (list nodo)]
    ;Si los kilometros son menores lo insertamos al principio de la lista
    [(< (nodo-kilometros nodo) (nodo-kilometros (car lista))) (cons nodo lista)]
    ;Si no, construimos una lista insertando ordenado el nodo en el resto de la lista
    [else (cons (car lista) (inserta-ordenado nodo (cdr lista)))]
  )
)

;Insertar los elementos de una lista ordenados en otra
(define (append-ordenado lista1 lista2)
  (cond
    [(not (empty? lista1)) (append-ordenado (cdr lista1)(inserta-ordenado (car lista1) lista2))]
    [else lista2]
   )
)

;Reverse lista
(define (reverse lista)
  (cond
    [(empty? (cdr lista)) lista]
    [else (append (reverse (cdr lista)) (list "-->") (list (car lista)))]
  )
)

; Sucesores y expandir 
(define (sucesores lista nodo)
  (cond
    [(empty? lista) empty]
    ;Generamos un nuevo nodo abierto y lo unimos con el resto de sucesores
    [(cons (make-nodo (cons (caar lista) (nodo-camino nodo)) (+ (cadar lista)(nodo-kilometros nodo))) (sucesores (cdr lista) nodo) )]
   )
)

(define (expandir grafo nodo)
  (cond
    [(empty? grafo) empty]
    ;Si estamos en la zona del grafo con origen en el nodo actual generamos sus sucesores
    [(equal? (caar grafo) (car (nodo-camino nodo))) (sucesores (cdar grafo) nodo)]
    ;Si no, seguimos mirando en el grafo 
    [else  (expandir (cdr grafo) nodo)]
  )
)

; Imprimir nodo y lista de nodos
(define (imprime-nodo nodo)
  (display (nodo-camino nodo))
  (display (nodo-kilometros nodo))
)

(define (imprime-final nodo)
  (newline)
  (display "\tCamino: ")
  (display (reverse (nodo-camino nodo)))
  (newline)
  (display "\tKilometros: ")
  (display (nodo-kilometros nodo))
)

(define (imprime-lista lista)
  (if (not (empty? lista))
      (begin
        (display (nodo-camino (car lista)))
        (display (nodo-kilometros (car lista)))
        (newline)
        (imprime-lista (cdr lista)))
      (newline)
  )
)

; Comprobar Final
(define (es-estado-final nodo final)
  (cond
    [(equal? final (car (nodo-camino nodo))) #t]
    [else #f]   
   )
)

;Busqueda de coste uniforme 
(define (busqueda-uniforme abiertos final grafo)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        ;Si actual es el estado final, lo mostramos
        [(es-estado-final actual final) (newline)(display "Final: ")(imprime-final actual)]
        ;Si no, buscamos anadiendo los sucesores a la lista de abiertos ordenados
        [else (busqueda-uniforme
                   (append-ordenado (expandir grafo actual) (cdr abiertos)) final grafo)
         ]
      )
    )
   )
 )

;Busqueda en anchura
(define (busqueda-anchura abiertos final grafo)
  (when (not (empty? abiertos))
    (let ([actual (car abiertos)])
      (cond
        ;Si actual es el estado final, lo mostramos
        [(es-estado-final actual final) (newline)(display "Final: ")(imprime-final actual)]
        ;Si no, buscamos anadiendo los sucesores a la lista de abiertos al final
        [else (busqueda-anchura
                   (append (cdr abiertos) (expandir grafo actual)) final grafo)
         ]
      )
    )
   )
 )


;Main
(define (main)
  ;Pedimos la ciudad origen y destino
  (display "Cual será la ciudad origen: ")
  (define nodo-inicial (make-nodo (list (read-line)) 0))
  (display "Cual será la ciudad destino: ")
  (define ciudad-final (read-line))

  ;Generamos el grafo a traves de fichero
  (define grafo (reduce-grafo (call-with-input-file "entrada.txt" read-graph)))

  (display "Elige metodo de busqueda: (uniforme, anchura)")
  (define busqueda (read-line))
  
  (cond
    ;Realizamos la busqueda en anchura
    [(equal? "anchura" busqueda) (busqueda-anchura (list nodo-inicial) ciudad-final grafo)]
    ;Realizamos la busqueda uniforme
    [(equal? "uniforme" busqueda) (busqueda-uniforme (list nodo-inicial) ciudad-final grafo)]
    [else "No coincide con ningun metodo disponible"]
   )
  
)

(main)

  



