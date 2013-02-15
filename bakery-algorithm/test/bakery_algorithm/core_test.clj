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
    (reset! ticket-machine 0)
    (is (= 0 (next-ticket)))
    (is (= 1 (next-ticket)))
    (is (= 2 (next-ticket))))
  (testing "Tickets are unique and in order with threads"
    (reset! ticket-machine 0)
    (wait-futures 10 (next-ticket))
    ))

(ticket-machine-test)