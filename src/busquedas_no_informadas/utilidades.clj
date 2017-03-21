(ns busquedas-no-informadas.utilidades
  (:require [clojure.core.matrix :as matrix]
            [clojure.string :as str]))

(def n "0")

(defn convertir-a-cadena
  "Convierte la matriz a cadena."
  [matriz]
  (let [fn-string (fn [fila]
                    (reduce str fila))]
    (str/join "\n" (map fn-string matriz))))

(defn convertir-a-matriz
  "Convierte la cadena a una matriz."
  [cadena]
  (map (fn [fila] (str/split fila #"")) (str/split cadena #"\n")))

(defn generar-movimiento
  "Permite generar una matriz de acuerdo al movimento de eliminar - crear para un jugador"
  [matriz [x-borrar y-borrar] [x-asig y-asig] jugador]
  (-> (matrix/mset matriz x-borrar y-borrar n)
      (matrix/mset x-asig y-asig jugador)))

(defn imprimir-matriz
  "Permite la impresion de la matriz"
  [matriz cantidad-espacios]
  (doseq [linea matriz]
    (prn (str/join (repeat cantidad-espacios " ")) linea)))

