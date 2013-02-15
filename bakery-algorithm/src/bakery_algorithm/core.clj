(ns bakery-algorithm.core)

(def ticket-machine (atom -1))

(defn next-ticket []
  (swap! ticket-machine inc))

