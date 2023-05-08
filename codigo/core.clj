(ns test2.core
  (:gen-class)
  (:require [clojure.java.io :as reader]))

; Actualizar archivo de inventarios (PRODUCTOS Y MONEDAS)
(defn update-arch [prod-nuevo mon-nuevo arch]
  (spit (clojure.java.io/resource arch) prod-nuevo) (spit (clojure.java.io/resource arch) "\n" :append true) (spit (clojure.java.io/resource arch) mon-nuevo :append true))

; ----- Leer archivo Principal -----
;; (def archivo "main.txt")

(defn read-arch [arch]
  (read-string (slurp (clojure.java.io/resource arch))))

;; (def main (read-arch "main.txt"))

; ----- Read archivo de transacciones -----
(defn read-tran [arch]
  (read-string (slurp (clojure.java.io/resource arch))))

; ----- Read archivo de inventarios -----
(defn read-maquina [arch]
  (with-open [r (clojure.java.io/reader (clojure.java.io/resource arch))]
    (reduce conj [] (line-seq r))))

(defn get-maquina [maq]
  (map read-string maq))

; ----- Graba la info de los archivos de la maquina en especifica -----
(defn define [prod mon tran]
  (def productos prod)
  (def monedas mon)
  (def transacciones tran))

; ----- Funcion para sacar la actualizacion de inventarios despues de todas las transacciones -----
(defn ultimo [lista]
  (if (empty? (next lista))
    (first lista)
    (ultimo (next lista))))

;; ----- Interfaz de Usuario -----
; Ayuda a verificar si las monedas insertadas son validas
(defn checa-monedaI [monedas moneda]
  (cond
    (empty? monedas) false
    (= moneda (first (first monedas))) true
    :else (checa-monedaI (next monedas) moneda)))

(defn transicionI [estadoO moneda]
  (if (checa-monedaI monedas moneda)
    (+ estadoO moneda)
    (concat (list (quote "Moneda de") moneda (quote "no se acepta")))))

(defn verificarI [secuencia estado precio]
  (if (empty? secuencia)
    (cond
      (= estado precio) (quote "Pago exacto") ; Mensaje de pago exacto
      (> estado precio) (concat (list (quote "Cambio de: ") (- estado precio) (quote "pesos"))) ; Calcula y despliega el cambio
      :else (concat (list (quote "Te faltaron: ") (- precio estado) (quote "pesos")))) ; Calcula y despliega el dinero que faltaria para completar transaccion
    (if (list? (transicionI estado (first secuencia))) ; checa si la moneda es valida
      (transicionI estado (first secuencia)) ; Si no es valida manda el mensaje de error
      (verificarI (next secuencia) (transicionI estado (first secuencia)) precio)))) ; Si si es valida entonces pasa a la siguiente moneda

; 3. Si si existe el producto y tiene inventario, checa si el pago es suficiente y calcula el cambio al igual que las monedas sobrantes en caso de que las haya
(defn aceptaI? [registro-producto secuencia]
  (verificarI secuencia 0 (first (next registro-producto))))

; 2. Si efectivamente existe una transaccion, verifica si el producto existe y tiene inventario disponible
;; (defn productoI? [productos producto pago]
;;   (cond 
;;     (empty? productos) (quote "Producto no existe") ; Despliega mensaje de que producto no existe
;;     ((and (= (first (first productos)) producto) (> (first (next (next (first productos)))) 1)) (aceptaI? (first productos) pago)) ; Pasa a verificar el pago
;;     ((and (= (first (first productos)) producto) (<= (first (next (next (first productos)))) 1)) (quote "No hay producto suficiente")) ; Despliega mensaje de que el producto no tiene suficiente inventario
;;     :else (productoI? (next productos) producto pago)))

(defn productoI? [productos producto pago]
  (if (empty? productos)
    (quote "Producto no existe") ; Despliega mensaje de que producto no existe
    (if (and (= (first (first productos)) producto) (> (first (next (next (first productos)))) 1))
      (aceptaI? (first productos) pago)
      (if (and (= (first (first productos)) producto) (<= (first (next (next (first productos)))) 1))
        (quote "No hay producto suficiente")
        (productoI? (next productos) producto pago))))) ; Pasa a verificar el pago

(defn maquina [transacciones]
  (if (empty? transacciones)
    '()
    (cons (productoI? productos (first (first transacciones)) (first (next (first transacciones)))) (maquina (next transacciones)))))

; ----------ACTUALIZACION DE PRODUCTOS----------
(defn encuentra [producto-lista prod]
  (if (= producto-lista prod)
    1
    0)) ; checa si a ese producto en especifico es al que se le resta el inventario o no

(defn actualiza-productos [reg-producto prod]
  (cons (first reg-producto) (list (first (next reg-producto)) (- (first (next (next reg-producto))) (encuentra (first reg-producto) prod))))) ; Actualiza el producto especifico de la transaccion reduciendole 1 a su inventario

; 4. Actualizar inventario de monedas
(defn recorre-productos [lista-productos producto]
  (if (empty? lista-productos)
    '()
    (cons (actualiza-productos (first lista-productos) producto) (recorre-productos (next lista-productos) producto)))) ; Crea el inventario de productos actualizado

; Ayuda a verificar si las monedas insertadas son validas
(defn checa-monedaP [monedas moneda]
  (cond
    (empty? monedas) false
    (= moneda (first (first monedas))) true
    :else (checa-monedaP (next monedas) moneda)))

(defn transicionP [estadoO moneda]
  (if (checa-monedaP monedas moneda)
    (+ estadoO moneda)
    (concat (list (quote "Moneda de") moneda (quote "no se acepta")))))

(defn verificarP [prod lista-productos secuencia estado precio]
  (if (empty? secuencia)
    (cond
      (= estado precio) (recorre-productos lista-productos prod) ; Como si se completa el pago, va a actualiza el inventario de productos
      (> estado precio) (recorre-productos lista-productos prod) ; Como tambien se completa el pago, va a actualiza el inventario de productos
      :else lista-productos) ; Si no se completa el pago entonces no actualiza inventario de productos porque no se compro ni uno
    (if (list? (transicionP estado (first secuencia))) ; checa si la moneda es valida
      lista-productos ; Si no es valida no actualiza el inventario de productos
      (verificarP prod lista-productos (next secuencia) (transicionP estado (first secuencia)) precio)))) ; Si si es valida entonces pasa a la siguiente moneda

; 3. Si si existe el producto y tiene inventario, checa si el pago es suficiente y calcula el cambio al igual que las monedas sobrantes en caso de que las haya
(defn aceptaP? [lista-productos registro-producto secuencia]
  (verificarP (first registro-producto) lista-productos secuencia 0 (first (next registro-producto))))

; 2. Si efectivamente existe una transaccion, verifica si el producto existe y tiene inventario disponible
(defn productoP? [lista-productos productos producto pago]
  (if (empty? productos)
    lista-productos ; Si no existe el producto no actualiza inventario de productos proque no hubo transaccion
    (if (and (= (first (first productos)) producto) (> (first (next (next (first productos)))) 1))
      (aceptaP? lista-productos (first productos) pago)
      (if (and (= (first (first productos)) producto) (<= (first (next (next (first productos)))) 1))
        lista-productos
        (productoP? lista-productos (next productos) producto pago))))) ; Pasa a verificar el pago

; 1. Validar que exista una transaccion y si ya no hay transacciones entonces se acaba
(defn recorre-pagosP [lista-productos lista-pagos]
  (if (empty? lista-pagos)
    nil
    (cons (productoP? lista-productos productos (first (first lista-pagos)) (first (next (first lista-pagos)))) (recorre-pagosP (productoP? lista-productos productos (first (first lista-pagos)) (first (next (first lista-pagos)))) (next lista-pagos)))))
; Crea la lista de todas las iteraciones de actualizacion del inventario de productos

; ----------ACTUALIZACION DE MONEDAS----------
; Saca la lista de las monedas que forman parte del cambio con su cantidad para cada una
(defn lista-cambio [cantidad lista-monedas]
  (cond
    (empty? lista-monedas) '()
    (= (quot cantidad (first (first lista-monedas))) 0) (lista-cambio cantidad (next lista-monedas))
    (and (>= (quot cantidad (first (first lista-monedas))) 1) (>= (first (next (first lista-monedas))) (quot cantidad (first (first lista-monedas))))) (cons (list (first (first lista-monedas)) (quot cantidad (first (first lista-monedas)))) (lista-cambio (rem cantidad (first (first lista-monedas))) (next lista-monedas)))
    (and (= (quot cantidad (first (first lista-monedas))) cantidad) (>= (first (next (first lista-monedas))) (quot cantidad (first (first lista-monedas))))) (cons (list (first (first lista-monedas)) (quot cantidad (first (first lista-monedas)))) (lista-cambio cantidad (next lista-monedas)))
    :else (lista-cambio cantidad (next lista-monedas))))

; Sacar la cantidad de la moneda que sea parte del cambio
(defn cant-cambio-moneda [lista moneda]
  (cond
    (empty? lista) 0
    (= moneda (first (first lista))) (first (next (first lista)))
    :else (cant-cambio-moneda (next lista) moneda)))

(defn cuenta [moneda pago]
  (cond
    (empty? pago) 0
    (= moneda (first pago)) (+ 1 (cuenta moneda (next pago)))
    :else (cuenta moneda (next pago))))

(defn actualiza-monedas [moneda pago cant-cambio]
  (cons (first moneda) (list (- (+ (cuenta (first moneda) pago) (first (next moneda))) (cant-cambio-moneda (lista-cambio cant-cambio (reverse monedas)) (first moneda))) (first (next (next moneda))))))
; Actualiza la moneda especifica de la transaccion haciendo la suma de la cantidad de veces que se encuentra en la transaccion y reduciendole la cantidad que forma parte del cambio

; 4. Actualizar inventario de monedas
(defn recorre-monedas [lista-monedas pago cant-cambio]
  (if (empty? lista-monedas)
    nil
    (cons (actualiza-monedas (first lista-monedas) pago cant-cambio) (recorre-monedas (next lista-monedas) pago cant-cambio)))) ; Crea el inventario de monedas actualizada

; Ayuda a verificar si las monedas insertadas son validas
(defn checa-moneda [monedas moneda]
  (cond
    (empty? monedas) false
    (= moneda (first (first monedas))) true
    :else (checa-moneda (next monedas) moneda)))

(defn transicion [estadoO moneda]
  (if (checa-moneda monedas moneda)
    (+ estadoO moneda)
    (list (quote "Moneda de") moneda (quote "no se acepta"))))

(defn verificar [lista-monedas toda-secuencia secuencia estado precio]
  (if (empty? secuencia)
    (cond
      (= estado precio) (recorre-monedas lista-monedas toda-secuencia 0) ; Como si se completa el pago, va a actualiza el inventario de monedas
      (> estado precio) (recorre-monedas lista-monedas toda-secuencia (- estado precio)) ; Como tambien se completa el pago, va a actualiza el inventario de monedas
      :else lista-monedas) ; Si no se completa el pago entonces no actualiza inventario de momedas porque no se compro ni uno
    (if (list? (transicion estado (first secuencia))) ; checa si la moneda es valida
      lista-monedas ; Si no es valida manda el mensaje de error
      (verificar lista-monedas toda-secuencia (next secuencia) (transicion estado (first secuencia)) precio)))) ; Si si es valida entonces pasa a la siguiente moneda

; 3. Si si existe el producto y tiene inventario, checa si el pago es suficiente y calcula el cambio al igual que las monedas sobrantes en caso de que las haya
(defn acepta? [lista-monedas registro-producto secuencia]
  (verificar lista-monedas secuencia secuencia 0 (first (next registro-producto))))

; 2. Si efectivamente existe una transaccion, verifica si el producto existe y tiene inventario disponible
(defn producto? [lista-monedas productos producto pago]
  (if (empty? productos)
    lista-monedas ; Si el producto no existe, no actualiza el invenrtario de monedas porque no hubo transaccion
    (if (and (= (first (first productos)) producto) (> (first (next (next (first productos)))) 1))
      (acepta? lista-monedas (first productos) pago)
      (if (and (= (first (first productos)) producto) (<= (first (next (next (first productos)))) 1))
        lista-monedas
        (producto? lista-monedas (next productos) producto pago))))) ; Pasa a verifiar el pago

; 1. Validar que exista una transaccion y si ya no hay transacciones entonces se acaba
(defn recorre-pagos [lista-monedas lista-pagos]
  (if (empty? lista-pagos)
    nil
    (cons (producto? lista-monedas productos (first (first lista-pagos)) (first (next (first lista-pagos)))) (recorre-pagos (producto? lista-monedas productos (first (first lista-pagos)) (first (next (first lista-pagos)))) (next lista-pagos)))))
; Crea la lista de todas las iteraciones de actualizacion del inventario de monedas


; -------------------------------------------------------------------------------------
; Lista de productos actualizadas
(defn prod-ids [updated-p]
  (conj updated-p))

; Lista de monedas actualizadas
(defn mon-ids [updated-m]
  (conj updated-m))

; Lista de ganancias de cada maquina
(defn ganancia [updated-mon mon]
  (conj (- (apply + (map (fn [x] (* (first x) (second x))) updated-mon)) (apply + (map (fn [x] (* (first x) (second x))) mon)))))


; ----- definir listas -----
(defn read-main [arch]
  (let [transacciones (read-tran (str (first arch))) productos (first (get-maquina (read-maquina (str (second arch))))) monedas (second (get-maquina (read-maquina (str (second arch))))) maq (str (second arch))]
    (define productos monedas transacciones)
    (let [updated-prod (ultimo (recorre-pagosP productos transacciones)) updated-monedas (ultimo (recorre-pagos monedas transacciones)) lista-tran (maquina transacciones)]
      (update-arch updated-prod updated-monedas maq) ; Actualiza el archivo de inventario
      (list (ganancia updated-monedas monedas) (prod-ids updated-prod) (mon-ids updated-monedas)) ; Genera una lista con todo lo que necesito para mis 5 Resultados
      ;lista-tran ; Activar para hacer pruebas
      )))

; ----- EJECUTAR PARALELIZACION -----
(def ejecutar (time (doall (pmap read-main (read-arch "main.txt")))))

;; ; ========= RESULTADOS ==========
(def lista-prod-final (map second ejecutar))
(def lista-prod-ids (map list (iterate inc 1) lista-prod-final))

(def lista-mon-final (map (fn [x] (second (next x))) ejecutar))
(def lista-mon-ids (map list (iterate inc 1) lista-mon-final))

;; ; Ganancia total del negocio obtenida después de todas las transacciones de venta procesadas.
(println (apply + (map first ejecutar)))

;; ; Lista del top 10% de máquinas con más ganancia mostrando el identificador de máquina y la ganancia correspondiente.



;; ; Lista de identificadores de máquinas que necesitan resurtido de algún producto por estar en el punto de reorden de su inventario. (<= 2)
(def restock '())

(defn no-prod [prod]
  (if (<= (second (next prod)) 2)
    1
    0))

(defn maq-restock? [lista]
  (cond
    (empty? lista) 0
    (= 1 (no-prod (first lista))) 1
    :else (maq-restock? (next lista))))

(defn id-maq-restock [lista res]
  (cond
    (empty? lista) res
    (= (maq-restock? (second (first lista))) 1) (concat res (cons (first (first lista)) (id-maq-restock (next lista) res)))
    :else (id-maq-restock (next lista) res)))


(println (id-maq-restock lista-prod-ids restock))

;; ; Lista de identificadores de máquinas que necesitan resurtido de alguna denominación de moneda por tener menos del límite permitido.
(def restock-m '())

(defn no-mon [mon]
  (if (<= (second mon) 2)
    1
    0))

(defn maq-restock-m? [lista]
  (cond
    (empty? lista) 0
    (= 1 (no-mon (first lista))) 1
    :else (maq-restock-m? (next lista))))

(defn id-maq-restock-m [lista res]
  (cond
    (empty? lista) res
    (= (maq-restock-m? (second (first lista))) 1) (concat res (cons (first (first lista)) (id-maq-restock-m (next lista) res)))
    :else (id-maq-restock-m (next lista) res)))


(println (id-maq-restock-m lista-mon-ids restock-m))

; Lista de identificadores de máquinas que necesitan retiro de monedas de alguna denominación por estar llenas o en el límite para el retiro.
(def retirar-m '())

(defn full-mon [mon]
  (if (> (second mon) (- (second (next mon)) 2))
    1
    0))

(defn maq-retirar-m? [lista]
  (cond
    (empty? lista) 0
    (= 1 (full-mon (first lista))) 1
    :else (maq-retirar-m? (next lista))))

(defn id-maq-retirar-m [lista res]
  (cond
    (empty? lista) res
    (= (maq-retirar-m? (second (first lista))) 1) (concat res (cons (first (first lista)) (id-maq-retirar-m (next lista) res)))
    :else (id-maq-retirar-m (next lista) res)))


(println (id-maq-retirar-m lista-mon-ids retirar-m))