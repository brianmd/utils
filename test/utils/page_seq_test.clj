(ns page-seq-test
  (:require [clojure.test :refer :all]
            [utils.page-seq :refer :all]))

(def pages
  (into [] (partition-all 20 (range 56))))

(defn get-page
  [page]
  (println "Loading page" page)
  (nth pages page ()))

(deftest test-paging
  (is (= (page-seq get-page 0) (range 56))))
