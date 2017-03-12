(ns busquedas-no-informadas.utilidades
  (:require [clojure.core.matrix :as matrix]))

(def n "0")

(defn generar-movimiento
  "Permite generar una matriz de acuerdo al movimento de eliminar - crear para un jugador"
  [matriz [x-borrar y-borrar] [x-asig y-asig] jugador]
  (-> (matrix/mset matriz x-borrar y-borrar n)
      (matrix/mset x-asig y-asig jugador)))

(defn imprimir-matriz
  "Permite la impresion de la matriz"
  [matriz]
  (doseq [linea matriz]
    (prn linea)))
