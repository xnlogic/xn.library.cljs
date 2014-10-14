(ns xn.library-test
  (:require [cemerick.cljs.test :as test]
            [xn.library :as lib])
  (:require-macros [cemerick.cljs.test :refer (is deftest)]))

(deftest test-dissoc-in
  (is (= nil (lib/dissoc-in nil nil)))
  (is (= nil (lib/dissoc-in nil [])))
  (is (= nil (lib/dissoc-in nil [:x])))
  (is (= nil (lib/dissoc-in {} nil)))
  (is (= nil (lib/dissoc-in {} [])))
  (is (= nil (lib/dissoc-in {} [:x])))
  (is (= {:x 0 :y 1} (lib/dissoc-in {:x 0 :y 1} nil)))
  (is (= {:x 0 :y 1} (lib/dissoc-in {:x 0 :y 1} [])))
  (is (= {:y 1} (lib/dissoc-in {:x 0 :y 1} [:x])))
  (is (= {:y 1} (lib/dissoc-in {:x {:y :z} :y 1} [:x :y])))
  (is (= {:x {:a 1} :y 1} (lib/dissoc-in {:x {:a 1 :b 2} :y 1} [:x :b])))
  (is (= {:x [{:a 1}] :y 1} (lib/dissoc-in {:x [{:a 1 :b 2}] :y 1} [:x 0 :b])))
  (is (= {:x [{:a 1} nil] :y 1} (lib/dissoc-in {:x [{:a 1} {:b 2}] :y 1} [:x 1]))))


(deftest try-not-to-blow-up
  (is (= :data @(lib/volatile :data)))
  (let [x (lib/volatile 1)]
    (swap! x inc)
    (is (= 2 @x))
    (swap! x + 1)
    (is (= 3 @x))
    (swap! x + 1 2)
    (is (= 6 @x))
    (swap! x + 1 2 3 4 5)
    (is (= 21 @x))
    (reset! x 9)
    (is (= 9 @x))))


(deftest try-to-transduce
  (is (= [[:a [[:a 1] [:a 2]]]
          [:b [[:b 3] [:b 4]]]]
         (into [] (lib/grouped-by first)
               [[:a 1] [:b 3] [:a 2] [:b 4]])))

  (is (= [[:a 2] [:b 4]]
         (into [] (lib/lasts-by first)
               [[:a 1] [:b 3] [:a 2] [:b 4]]))))
