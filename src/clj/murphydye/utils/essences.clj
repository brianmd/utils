(ns murphydye.utils.essences)

(defn col-names [definition-vector]
  (->> definition-vector (partition 4) (map first)))

(defmacro make-record [name cols-names]
  `(apply (macro->fn defrecord) ['~name (->> ~cols-names (map name) (map symbol) vec)]))
;; (defmacro make-record [name definition-vector]
;;   `(apply (macro->fn defrecord) ['~name (->> ~(col-names definition-vector) (map name) (map symbol) vec)]))

(defprotocol Validator
  "validate "
  (field-validations [rec] "returns map: {field-name [predicate (fn [name val rec] msg) ...] ...}")
  (errors [rec] "returns map: {field-name [msg ...] ...")
  (valid? [rec] "returns true if errors is empty map"))

(defn not-re-matches [regex string]
  (not (re-matches regex string)))
(def test-failure #(re-matches #"au9234721324189712345qiouqwre" %))
(def required #(and (not (nil? %)) (not= "" %)))
(def digits #(re-matches #"\d*" %))
(def string-float #(re-matches #"[\d.]*" %))
(def alphanumeric? #(re-matches #"[a-zA-Z0-9]*" %))

(def validators
  {:test-failure [test-failure #(str "The deck was stacked against bro: " %)]
   :digits       [digits #(str "Must be digits only: " %)]
   :required     [required (fn [_] "This field is required.")]
   })

