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
  (testing "Adding and removing in parallel doesn't collide"
           (reset-free-servers!)
           (pmap (fn [n] (now-free n)) (range 20))
           (is (= 20 (num-free-servers)))
           (is (= (range 20) 
                  (sort (pmap (fn [_] (next-server))))))
           (is (= 0 (num-free-servers)))))

(ticket-machine-test)