(ns directed.travelling-salesman.core
  (:require [clojure.set :as set]
            [directed.travelling-salesman.node :as n]
            [directed.travelling-salesman.route :refer [hamiltonian-cycle?]]
            [directed.weighted-graph :as wg]
            [utils :refer [???]]))

(defn shortest-hamiltonian-cycle-little
  "Находит в данном графе гамильтонов цикл наименьшей стоимости с помощью алгоритма Литтла."
  [graph]
  {:post [(hamiltonian-cycle? graph %)]}
  (loop [best-length ##Inf
         best-cycle nil
         nodes #{(???)}]
    (let [nodes-to-consider (set/select #(< (n/lower-bound %) best-length) nodes)]
      (if (???)
        best-cycle
        (let [node (???)
              remaining-nodes (???)]
          (if (n/leaf? node)
            (let [new-cycle (???)
                  new-length (???)]
              (assert (= new-length (n/lower-bound node)))
              (if (< new-length best-length)
                (recur (???)
                       (???)
                       remaining-nodes)
                (recur (???)
                       (???)
                       remaining-nodes)))
            (recur best-length
                   best-cycle
                   (???))))))))
