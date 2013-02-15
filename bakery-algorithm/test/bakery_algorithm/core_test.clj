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

(ticket-machine-test)