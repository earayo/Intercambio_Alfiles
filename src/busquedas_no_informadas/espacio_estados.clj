(ns busquedas-no-informadas.espacio-estados
  (:require [clojure.string :as str]
            [clojure.core.matrix :as matrix]
            [busquedas-no-informadas.arbol :as arbol]
            [busquedas-no-informadas.utilidades :as u]
            [cheshire.core :refer :all]))

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
      (if (some #{[i j]} otras-fichas)
        mov
        (recur (f-x i) (f-y j) (conj mov [i j])))
      mov)))

(defn anexar-movimientos [mov-add mov]
  "Permite el anexo de movimientos que pueden ser realizados por un alfil"
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

(defn obtener-movimiento-permitidos
  "Calcual y obtiene los movimientos permitidos para un jugador"
  [movimientos-jugador-turno movimientos-jugador-espera]
  (filter (fn [coord] (not (some #{coord} movimientos-jugador-espera))) movimientos-jugador-turno))

(defn calcular-movimientos-permitidos
  "Calcula los movimientos permitidos para el jugador en turno."
  [mov-jt mov-je]
  (map (fn [[coord movs]]
         [coord (obtener-movimiento-permitidos movs mov-je)]) mov-jt))
