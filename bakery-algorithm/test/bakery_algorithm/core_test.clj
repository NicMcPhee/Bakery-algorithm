(ns bakery-algorithm.core-test
  (:use clojure.test
        bakery-algorithm.core))

;;;;;;;;;;;;;

;; Based on an example from Chap 4 of Programming Clojure
;; by Emerick, et al
(defmacro futures
  [n & exprs]
  (vec (for [_ (range n)
             expr exprs]
         `(future ~expr))))
 
(defmacro wait-futures
  [& args]
  `(doseq [f# (futures ~@args)]
           @f#))

;;;;;;;;;;

(deftest ticket-machine-test
  (testing "Ticket increments on each call"
    (reset! ticket-machine -1)
    (is (= 0 (next-ticket)))
    (is (= 1 (next-ticket)))
    (is (= 2 (next-ticket))))
  (testing "Tickets are unique with threads"
    (reset! ticket-machine -1)
    (is (= (range 20)
           (sort (pmap (fn [_] (next-ticket)) (range 20)))))
    ))

(deftest free-server-test
  (testing 
    "Adding and removing in parallel doesn't collide"
    (let [numServers 100]
      (reset-free-servers!)
      (doall (pmap (fn [n] (now-free n)) (range numServers)))
      (is (= numServers (num-free-servers)))
      (is (= (range numServers) 
             (sort (pmap (fn [_] (next-server)) (range numServers)))))
      (is (= 0 (num-free-servers))))))

(deftest make-server-test
  (testing
    "Creating a server causes it to be added to the free server list"
    (reset-free-servers!)
    (is (= 0 (count @free-servers)))
    (make-server 5)
    (is (= 1 (count @free-servers)))
    (is (= 5 (:id (peek @free-servers))))
    (make-server 7)
    (is (= 2 (count @free-servers)))
    (is (= 7 (:id (nth @free-servers 1))))))

(ticket-machine-test)
(free-server-test)
(make-server-test)
