(ns busquedas-no-informadas.core
  (:require [clojure.core.matrix :as matrix]
            [busquedas-no-informadas.arbol :as arbol]
            [busquedas-no-informadas.utilidades :as u])
  (:gen-class))

(def j-1 "B")
(def j-2 "N")

(def posicion-inicial [[j-1 u/n u/n u/n j-2]
                       [j-1 u/n u/n u/n j-2]
                       [j-1 u/n u/n u/n j-2]
                       [j-1 u/n u/n u/n j-2]])

(def posicion-final [[j-2 u/n u/n u/n j-1]
                     [j-2 u/n u/n u/n j-1]
                     [j-2 u/n u/n u/n j-1]
                     [j-2 u/n u/n u/n j-1]])

(def filas (matrix/dimension-count posicion-inicial 0))
(def columnas (matrix/dimension-count posicion-inicial 1))

(defn obtener-posiciones-jugador [jugador matriz]
  "Permite obtener las posiciones en las que se encuentran los alfiles de un jugador"
  (for [[x row] (map-indexed vector matriz) 
        [y val] (map-indexed vector row) 
        :when (= jugador val)]
    [x y]))

(defn dentro-rango
  "Valida si el elemento se encuentra dentro del rango minimo-maximo (minimo <= elemento < maximo)"
  [elemento min max]
  (and (>= elemento min) (< elemento max)))

(defn obtener-movimientos [[x y] otras-fichas f-x f-y]
  "Permite obtener los movimientos que se pueden realizar dada la posicion"
  (loop [i (f-x x)
         j (f-y y)
         mov []]
    (if (and (dentro-rango i 0 filas) (dentro-rango j 0 columnas))
      (recur (f-x i) (f-y j) (if (some #{[i j]} otras-fichas)
                               mov
                               (conj mov [i j])))
      mov)))

(defn anexar-movimientos [mov-add mov]
  (if-not (empty? mov-add)
    (into mov mov-add)
    mov))

(defn obtener-posibles-mov-jugador [jugador matriz]
  "Permite obtener los posibles movimientos que un jugador puede realizar en la matriz"
  (let [posicion-fichas-jugador (obtener-posiciones-jugador jugador matriz)]
    (map (fn [coord]
           (let [otras-coord (remove #{coord} posicion-fichas-jugador)
                 mov-decdec (obtener-movimientos coord otras-coord dec dec)
                 mov-decinc (obtener-movimientos coord otras-coord dec inc)
                 mov-incdec (obtener-movimientos coord otras-coord inc dec)
                 mov-incinc (obtener-movimientos coord otras-coord inc inc)]
             [coord  (-> (anexar-movimientos [] mov-decdec)                  
                         (anexar-movimientos mov-decinc)
                         (anexar-movimientos mov-incdec)
                         (anexar-movimientos mov-incinc))]))
         posicion-fichas-jugador)))

(defn obtener-movimiento-permitidos [movimientos-jugador-turno movimientos-jugador-espera]
  (filter (fn [coord] (not (some #{coord} movimientos-jugador-espera))) movimientos-jugador-turno))

(defn calcular-movimientos-permitidos [mov-jt mov-je]
  (map (fn [[coord movs]]
         [coord (obtener-movimiento-permitidos movs mov-je)]) mov-jt))

(defn buscar-solucion [jt je ramas nivel]
  (loop [[nodo-1 & resto-nodos] ramas
         ultimo-movimiento {jt (:nueva-posicion nodo-1)}
         arbol []]
    (let [matriz (:matriz nodo-1)
          pm-jt (obtener-posibles-mov-jugador jt matriz)
          pm-je (reduce into (map second (obtener-posibles-mov-jugador je posicion-inicial)))
          mov-per-b (calcular-movimientos-permitidos pm-jt pm-je)
          nodos (arbol/crear-ramas posicion-inicial mov-per-b jt)]
      (cond
        (> nivel 2) (cons nodo-1 arbol)
        (nil? nodo-1) arbol
        :else
        (recur resto-nodos
               (assoc ultimo-movimiento jt (:nueva-posicion nodo-1))
               (cons arbol (buscar-solucion je jt nodos (inc nivel)))))))) 

(let [pm-b (obtener-posibles-mov-jugador j-1 posicion-inicial)
      pm-n (reduce into (map second (obtener-posibles-mov-jugador j-2 posicion-inicial)))
      mov-per-b (calcular-movimientos-permitidos pm-b pm-n)
      ramas (arbol/crear-ramas posicion-inicial mov-per-b j-1)]
  (println (buscar-solucion j-2 j-1 ramas 0)))

(u/imprimir-matriz posicion-inicial)

(defn busqueda-no-informadas
  [x]
  (println "Hola " x))

(defn -main []
  (busqueda-no-informadas "Busqueda No Informadas"))
