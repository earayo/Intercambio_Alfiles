(ns busquedas-no-informadas.core
  (:require [clojure.core.matrix :as matrix]
            [busquedas-no-informadas.grafo :as grafo]
            [busquedas-no-informadas.utilidades :as u]
            [busquedas-no-informadas.espacio-estados :as ee]
            [cheshire.core :refer :all])
  (:gen-class))

(def grafo (atom {})) ;; Variable que contendrá el grafo de movimientos.

(defn generar-grafo-jugadas [matriz nivel]
  "Crea el grafo de jugadas con los movimientos permitidos de acuerdo al espacio de estados"
  (if (or (= ee/posicion-final matriz) (= nivel 20)) ;; Limitamos la busqueda a 20 niveles debido a problemas de llenado de pila.
    (u/convertir-a-cadena matriz)
    (let [[jt je] (if (= 0 (mod nivel 2)) [ee/j-1 ee/j-2] [ee/j-2 ee/j-1])
          pm-jt (ee/obtener-posibles-mov-jugador jt matriz)
          pm-je (reduce into (map second (ee/obtener-posibles-mov-jugador je matriz)))
          mov-per-jt (grafo/crear-ramas matriz (ee/calcular-movimientos-permitidos pm-jt pm-je) jt)
          siguiente (inc nivel)
          matriz-str (u/convertir-a-cadena matriz)]
      (doseq [nodo mov-per-jt]
        (if-not (grafo/contiene-grafo @grafo nodo)
          (swap! grafo grafo/adicionar-enlace matriz-str (u/convertir-a-cadena nodo))
          
          (generar-grafo-jugadas nodo siguiente))))))

(defn busqueda-por-profundidad 
  "Realiza la busqueda de la solución por el metodo de profundidad"
  []
  (println "Busqueda por Profundidad ")
  (reset! grafo {})
  (generar-grafo-jugadas ee/posicion-inicial 0)
  (prn (grafo/seq-graph-dfs @grafo (u/convertir-a-cadena ee/posicion-inicial))))

(defn busqueda-por-amplitud
  "Realiza la busquda de la solucion por el metodo de Amplitud"
  []
  (println "Busqueda por Amplitud ")
  (reset! grafo {})
  (generar-grafo-jugadas ee/posicion-inicial 0)
  (prn (grafo/seq-graph-bfs @grafo (u/convertir-a-cadena ee/posicion-inicial))))

(defn -main []
  (println "Seleccione el tipo de busqueda: \n 1. Profundidad\n 2. Amplitud")
  (let [op (read-line)]
    (cond
      (= op "1")  (busqueda-por-profundidad)
      (= op "2")  (busqueda-por-amplitud)
      :else (println "Lo sentimos, opcion incorrecta."))))

(-main)
