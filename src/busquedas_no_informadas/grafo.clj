(ns busquedas-no-informadas.grafo
  (:require [clojure.core.matrix :as matrix]
            [busquedas-no-informadas.utilidades :as utilidades]))

(defn crear-ramas
  "Crea las ramas que puede tener un nodo del arbol segun los movimientos permitidos para un jugador"
  [matriz mov-permitidos jugador]
  (reduce (fn [acc [coord-borrar movimientos]]
            (into acc (map (fn [coord]
                             (utilidades/generar-movimiento matriz coord-borrar coord jugador))
                           movimientos))) [] mov-permitidos))

(defn adicionar-nodo
  "Permite la adicion de nodos al grafo"
  [g n] 
  (if (g n) 
    g 
    (assoc g n #{}))) 

(defn adicionar-enlace
  "Crea el enlace entre dos nodos en el grafo"
  [g n1 n2]
  (-> g 
      (adicionar-nodo n1) 
      (adicionar-nodo n2) 
      (update n1 conj n2)))

(defn contiene-grafo
  "Valida si una matriz se encudentra contenida en el grafo"
  [g n]
  (nil? (g n)))

(defn grafo-sequencial
  "Crea los recorridos del grafo"
  [d g s]
  (letfn [(rec-seq [explored frontier]
             (lazy-seq
              (if (empty? frontier)
                nil
                (let [v (peek frontier)
                      neighbors (g v)]
                  (cons v (rec-seq
                           (into explored neighbors)
                           (into (pop frontier) (remove explored neighbors))))))))]
    (rec-seq #{s} (conj d s))))

(def seq-graph-dfs (partial grafo-sequencial [])) ;; Crea la secuencia acorde a profundidad
(def seq-graph-bfs (partial grafo-sequencial (clojure.lang.PersistentQueue/EMPTY))) ;; Crea la secuencia acorde a la amplitud

;; Ejemplo de funcionamiento.
(def G {"B000N\nB000N\nB000N\nB000N" #{"B000N\n0000N\nBB00N\nB000N" "B000N\nBB00N\n0000N\nB000N" "0000N\nBB00N\nB000N\nB000N" "B000N\nB000N\nBB00N\n0000N"}
        "B000N\n0000N\nBB00N\nB000N" #{}
        "B000N\nBB00N\n0000N\nB000N" #{}
        "0000N\nBB00N\nB000N\nB000N" #{}
        "B000N\nB000N\nBB00N\n0000N" #{}})
(seq-graph-dfs G "B000N\nB000N\nB000N\nB000N")
(seq-graph-bfs G "B000N\nB000N\nB000N\nB000N")
(contiene-grafo G "B000N\nB000N\nB000N\nB000N")
