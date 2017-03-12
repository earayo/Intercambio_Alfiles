(ns busquedas-no-informadas.arbol
  (:require [clojure.core.matrix :as matrix]
            [busquedas-no-informadas.utilidades :as utilidades]))

(defn crear-ramas
  "Crea las ramas que puede tener un nodo del arbol segun los movimientos permitidos para un jugador"
  [matriz mov-permitidos jugador]
  (reduce (fn [acc [coord-borrar movimientos]]
            (into acc (map (fn [coord]
                             (assoc {:posicion-actual coord-borrar
                                     :nueva-posicion coord
                                     :jugador jugador}
                                    :matriz (utilidades/generar-movimiento matriz coord-borrar coord jugador)))
                           movimientos))) [] mov-permitidos))
