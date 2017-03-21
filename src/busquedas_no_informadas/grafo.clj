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

(defn adicionar-nodo [g n] 
  (if (g n) 
    g 
    (assoc g n {:siguiente #{} :anterior #{}}))) 

(defn adicionar-enlace [g n1 n2]
  (-> g 
      (adicionar-nodo n1) 
      (adicionar-nodo n2) 
      (update-in [n1 :siguiente] conj n2) 
      (update-in [n2 :anterior] conj n1))) 

(defn remover-enlace [g n1 n2] 
  (-> g 
      (adicionar-nodo n1) 
      (adicionar-nodo n2) 
      (update-in [n1 :siguiente] disj n2) 
      (update-in [n2 :anterior] disj n1))) 

(defn remover-node [g n] 
  (if-let [{:keys [next prev]} (g n)] 
    ((comp 
      #(dissoc % n) 
      #(reduce (fn [g* n*] (remover-enlace g* n* n)) % prev) 
      #(reduce (fn [g* n*] (remover-enlace g* n n*)) % next)) 
     g) 
    g)) 

(defn contiene-nodo? [g n] 
  (g n)) 

(defn continene-enlace? [g n1 n2] 
  (get-in g [n1 :siguiente n2])) 

(defn siguientes-nodo [g n] 
  (get-in g [n :siguiente])) 

(defn depth-first-search [g nodo-raiz objetivo?] 
  (loop [lista-abierta (list nodo-raiz)] 
    (when-first [n lista-abierta] 
      (if (objetivo? n) 
        n 
        (recur (concat (siguientes-nodo g n) (rest lista-abierta))))))) 
