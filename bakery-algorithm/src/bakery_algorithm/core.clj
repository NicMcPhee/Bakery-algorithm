(ns bakery-algorithm.core)

(def ticket-machine (atom -1))

(defn next-ticket []
  (swap! ticket-machine inc))

(def free-servers clojure.lang.PersistentQueue/EMPTY)

(defn reset-free-servers! []
  (reset! free-servers clojure.lang.PersistentQueue/EMPTY))

(defn num-free-servers []
  (count @free-servers))

(defn now-free [server]
  (swap! free-servers conj server))

(defn next-server []
  (dosync
    (let [ns (peek @free-servers)]
      (swap! free-servers pop)
      ns)))