(ns utils.page-seq)

;; https://ofnir.net/posts/turning-paging-into-a-lazy-sequence-with-clojure-tranducers/

(defn page-seq
  "Flat sequence of items loaded via the given page loading fn.

get-page-fn should create a seq of items on each page.

Note: page-seq does do chunking, so may read more pages than requested"
  ([get-page-fn] (page-seq get-page-fn 1))
  ([get-page-fn start-page]
    (eduction
      (map get-page-fn)
      (take-while seq)
      cat
      (range start-page 999999))))
