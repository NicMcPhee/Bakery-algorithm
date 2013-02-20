(ns bakery-algorithm.core
  (:use bakery-algorithm.logging))

;; The ticket machine

(def ticket-machine (atom -1))

(log-reference ticket-machine console)

(defn next-ticket []
  (swap! ticket-machine inc))

;; The free servers queue

(def free-servers (ref clojure.lang.PersistentQueue/EMPTY))

(log-reference free-servers console)

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

;; Make the now-serving sign

(def now-serving (atom 0))

(log-reference now-serving console)

;; Make our customers and serves

(defn make-server [id]
  (let [server {:id id}]
    ; Every new server is free to help a customer
    (now-free server)
    server))

(defn now-serving-updated [customer now-serving-number]
  (when (= (:id @customer) now-serving-number)
    (println "Customer " (:id @customer) " was just informed that we're now serving " now-serving-number)
    (when-let [server (next-server)]
      (println "Customer " (:id @customer) " and server " (:id server) " are now working together.")
      (remove-watch now-serving customer)
      )
    ))

(defn make-customer 
  ([id] (make-customer id 0))
  ([id delay-before-entering]
    (let [c (atom {:id id})]
      (log-reference c console)
      (add-watch now-serving 
                 c 
                 (fn [customer ns old-value new-value] 
                   (now-serving-updated customer new-value)))
      (future
        (Thread/sleep delay-before-entering)
        (swap! c assoc :ticket-number (next-ticket))
        )
      c
      )))

(defn make-people [numCustomers numServers]
  ; I found it easier to have the IDs for servers be negative and the
  ; IDs of customers be non-negative (0 or greater), so I map inc and
  ; then negation across the range for the server IDs.
  (concat (map (comp make-server - inc) (range numServers))
          (map make-customer (range numCustomers))))