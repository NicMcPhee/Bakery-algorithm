(ns bakery-algorithm.core
  (:use bakery-algorithm.logging))

;; The ticket machine

(def ticket-machine (atom -1))

(defn next-ticket []
  (swap! ticket-machine inc))

;; The free servers queue

(def free-servers (ref clojure.lang.PersistentQueue/EMPTY))

(defn reset-free-servers! []
  (dosync
    (ref-set free-servers clojure.lang.PersistentQueue/EMPTY)))

(defn num-free-servers []
  (count @free-servers))

(defn now-free [server]
  (dosync
    (alter free-servers conj server)))

(defn next-server []
  (dosync
    (let [ns (peek @free-servers)]
      (alter free-servers pop)
      ns)))

;; Make our customers and serves

(defn make-server [id]
  (let [server {:id id}]
    ; Every new server is free to help a customer
    (now-free server)
    server))

(defn make-customer {})

(defn make-people [numCustomers numServers]
  (map make-server (range numServers))
  (map make-customer (range numServers (+ numCustomers numServers))))