(ns murphydye.utils.step
  (:require
   ;; [murphydye.utils.step :refer [env]]
   ))

(def step-input-path (-> env :paths :local :step-input-path))
(def step-output-path (-> env :paths :local :step-output-path))

(defn stepxml-top-tag []
  [:STEP-ProductInformation
   {:ExportTime (timenow)
                                        ;:ExportContext "Context1"
    :ContextID "Context1"
    :WorkspaceID "Main"
    :UseContextLocale "false"
    }
   [:Products]])
(defn upc-check-digit [string]
  (let [zero (int \0)]
    (loop [accum 0
           s (seq string)
           even-digit? false]
      (if (empty? s)
        (mod (- 10 (mod accum 10)) 10)
        (let [x (- (int (first s)) zero)]
          (println even-digit? x)
          (recur (+ accum (if even-digit? x (* 3 x))) (rest s) (not even-digit?)))))))

(defn add-checksum [string]
  (let [s (zero-pad 11 string)]
    (str s (upc-check-digit s))))
;; (upc-check-digit "87663000027")
;; (upc-check-digit "80432546052")
;; (assert (= "804325460521" (add-checksum "80432546052")))


