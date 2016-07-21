(println "\n\n-----------------")
(println "loading library com.murphydye.utils.core")

(ns murphydye.utils.core
  (:require
    ;; [clj-http.client :as client]
    ;; [config.core :refer [env]]
   ;; [treadstone.config :refer [env]]
    [clj-http.client :as client]
    [clojure.walk :refer :all]
    [clojure.pprint]
    [clojure.string :as str]
    [clojure.java.io :as io]

    [cheshire.core :as ches]
    [cheshire.generate :refer [add-encoder encode-str remove-encoder]]
    [taoensso.carmine :as car :refer (wcar)]
    ;[cats.core :as m]
    ;[cats.builtin]
    ;[cats.monad.maybe :as maybe]

    [clj-time.core]
    [clj-time.format]

    [clojure.data.codec.base64 :as b64]

    ;; [me.raynes.conch :refer [programs with-programs let-programs] :as sh]
    [com.rpl.specter :as s :include-macros true]
    [net.cgrand.enlive-html :as html]

    [potemkin :as pot]
    [clojure.test :as test]
    ))

(pot/import-vars
 [clojure.test
  is are]
 )

;; (def is test/is)
;; (def are test/are)

;; (with-programs [ls] (ls {:seq true}))
;; (with-programs [ssh] (ssh "neo" "ls -l" {:seq true}))

;; (programs ssh)

;; for debugging examples
;; (defmacro examples [& forms]
;;   `(do ~@forms))
(defmacro examples [& forms]
  )

(defmacro assert= [& args]
  `(assert (= ~@args)))
(defmacro assert-= [& args]
  `(assert (= ~@args)))
(defmacro assert-false
  ([x] `(assert (clojure.core/not ~x)))
  ([x message] `(assert (clojure.core/not ~x) message)))

(examples
 (assert= nil (->int nil) (->int "    "))
 (assert= 34 (->int "   34") (->int "   000000034")))

(defmacro defn-memo [name & body]
  `(def ~name (memoize (fn ~body))))

(defn atom? [x]
  (instance? clojure.lang.Atom x))
  ;; (= clojure.lang.Atom (type x)))
(examples
  (assert (= false (atom? 'x)))
  (assert (= true (atom? (atom 'x))))
  )
(defn atomize [x]
  (if (atom? x) x (atom x)))

(defn ->str [a-name]
  (if (string? a-name)
    a-name
    (if (number? a-name)
      a-name
      (str/replace
       (str/upper-case
        (if (keyword? a-name)
          (name a-name)
          (str a-name)))
       "-" "_"))))

(defn ->keyword [a-string]
  (if (keyword? a-string)
    a-string
    (keyword
     (str/lower-case (str/replace (str a-string) "_" "-")))))

(defn ->int [v]
  (if (nil? v)
    nil
    (if (string? v)
      (let [v (str/trim v)]
        (if (empty? v)
          nil
          (-> v Double/parseDouble int)))
      (int v))))

(defn ->float [v]
  (if (nil? v)
    nil
    (if (string? v)
      (let [v (str/trim v)]
        (if (empty? v)
          nil
          (Double/parseDouble v)))
      (double v))))

(defn as-integer [string]
  (->int string))
;; (if (= (type string) String)
;;   (read-string (as-short-document-num string))
;;   string))


(def map! (comp doall map))
(def maprun (comp dorun map))

(defn third [s]
  (first (next (next s))))

(defn any [s]
  (nth s (rand-int (count s))))

(defn detect
  ([predicate] #(detect predicate %))
  ([predicate coll]
   (some #(if (predicate %) %) coll)))
(examples
 (assert= 9 (detect #(> % 5) [2 9 4 7]) ((detect #(> % 5)) [2 9 4 7]))
 (assert= 6 (detect #(> % 5) (range)) ((detect #(> % 5)) (range))))

(defn ppn
  "pprint each arg, return nil"
  [& args]
  (binding [clojure.pprint/*print-miser-width* 120
            clojure.pprint/*print-right-margin* 120]
    (doseq [arg args] (clojure.pprint/pprint arg))))

(defn ppa
  "pprint each arg, return last arg"
  [& args]
  (apply ppn args)
  (last args))
;; (ppn 3 {:a 3 :q "rew"})

(def rppout *out*)
(defn reset-rpp []
  (def rppout *out*))
;; (reset-rpp)

(defn rppn
  "repl pprint, returning nil"
  [& args]
  (binding [*out* rppout]
    (println "\n-------------------")
    (apply ppn args)))
    ;; (doseq [arg args] (pprint arg))))

(defn rpp
  "repl pprint, returning last arg"
  [& args]
  (apply rppn args)
  (last args))

(defn pp->str [obj]
  (with-out-str (clojure.pprint/pprint obj)))

(defn spit-fn
  "spits each element of seq with ele-fn"
  [ele-fn filename coll & opts]
  (with-open [out (apply clojure.java.io/writer filename :encoding "UTF-8" opts)]
    (binding [*out* out]
      (maprun ele-fn coll))))
(defn spitln
  "spits seq with linefeeds between elements"
  [filename coll & opts]
  (apply spit-fn ppa filename coll opts)
  )
;; (spitln "junky" [1 3 5 7 11])
;; (spitln "junky" {:a [1 2 3 4] :b "hey"})

(defn sans-accumulator
  "reducer helper for functions that don't care about the accumulator"
  ([f] (fn reducefn
         ([])   ;; called at start when no initial value provided
         ([_])  ;; called when finished with collection
         ([_ val] (f val))))
  ([f finish-fn] (fn reducefn
                   ([])
                   ([_] (finish-fn))
                   ([_ val] (f val))))
  ([f finish-fn start-fn] (fn reducefn
                            ([] (start-fn))
                            ([_] (finish-fn))
                            ([_ val] (f val))))
  )

(defn uuid [] (java.util.UUID/randomUUID))
;; (uuid)



(println "(an error here indicates you need a profile.clj file with redis settings!)")



;; (add-encoder clojure.lang.Delay
;;              (fn [c jsonGenerator]
;;                (.writeString jsonGenerator (str c))))

;(map
;  #(add-encoder %
;                (fn [c jsonGenerator]
;                  (.writeString jsonGenerator (str c))))
;  [clojure.lang.Delay org.httpkit.server.AsyncChannel java.lang.Class java.lang.Long])
  ;[clojure.lang.Delay org.httpkit.server.AsyncChannel])

(defn clean-all [x]
  (ches/parse-string (ches/generate-string x)))

(defn stringify-all [x]
  (postwalk str x))
  ;(postwalk #(if(keyword? %)(name %) %) x))

(defonce clojurized-keywords (atom {}))

(defn set-clojurized-keyword [from-key to-key]
  (swap! clojurized-keywords assoc from-key to-key))

(defn clear-clojurized-keywords []
  (reset! clojurized-keywords {}))
;; (clear-clojurized-keywords)

(defn clojurize-keyword!
  "convert mixed case to lower case with hyphens. Considers 'ID' as a token."
  [key]
  (let [skey (if (keyword? key) (name key) (str key))
        s (str/replace skey #"[_\s-()]+" "-")
        s (str/replace s #"[^A-Z-][A-Z]" #(str (first %) "-" (second %)))
        s (str/replace s #"^-+" "")
        s (str/replace s #"-+$" "")
        s (str/lower-case s)
        ;; s (str/join (map #(if (and (<= 65 (int %)) (<= (int %) 90)) (str \- (str/lower-case %)) %) s))
        k (keyword (if (= \- (first s)) (subs s 1) s))]
      k))
;; (clojurize-keyword! "ParentID of middle-child2 (API)")
;; (clojurize-keyword! "ParentID of middle-child2")
;; (clojurize-keyword! "ParentID of middle _--_ _   child2")
;; (clojurize-keyword! "Arbor Size - Fractional")
;; (clojurize-keyword "Arbor Size - Fractional")
;; (humanize! "Arbor Size - Fractional")
;; (humanize "Arbor Size - Fractional")

(defn clojurize-keyword
  "memoized. can override this function's output with your own via set-clojurized-keyword."
  [key]
  (let [skey (if (keyword? key) (name key) (str key))]
    (if-let [k (@clojurized-keywords skey)]
      k
      (set-clojurized-keyword key (clojurize-keyword! skey)))))

(examples
 (clear-clojurized-keywords)
 (clojurize-keyword! "ParentIDW")
 (clojurize-keyword! "PREParentIDW") ; probably want :pre-parent-idw.
 (clojurize-keyword "ParentIDW")
 (str/split "ParentIDW" #"[^A-Z][A-Z]")
 (str/replace "ParentIDW" #"[^A-Z-_][A-Z]" #(str (first %) "-" (second %)))
 (str/replace "Parent-IDW" #"[^A-Z-_][A-Z]" #(str (first %) "-" (second %))))

(defn clojurize-map [m]
  (into {}
        (map! (fn [[k v]] [(clojurize-keyword k) v]) m)))
;; (clojurize-map {"ab_cd" 4 "AbcDef" 9})

;; (defn clojurize-map-keywords [m]
;;   (s/transform (s/walker keyword?)
;;                clojurize-keyword
;;                m))
(examples
 (clojurize-keyword :ParentID)
 @clojurized-keywords
 (clojurize-map-keywords {:ParentID "394" :SubMap {:TestID ["a" 3 :ParentTrap]}})
 )

(defn humanize! [s]
  (str/join " "
            (map str/capitalize
                 (-> s clojurize-keyword name (str/split #"-") ))))
;; (assert= "Parent Id" (humanize! "ParentID"))
;; note: we would prefer "Parent ID"

(defonce humanized-words (atom {}))

(defn set-humanized [from-word to-word]
  (swap! humanized-words assoc from-word to-word)
  to-word)

(defn clear-humanized-words []
  (reset! humanized-words {}))
;; (clear-humanized-words)

(defn humanize
  "memoized. can override this function's output with your own via set-humanized."
  [word]
  (let [word (str/trim (->str word))
        low-word (str/lower-case word)]
    (if-let [w (@humanized-words low-word)]
      w
      (set-humanized low-word (humanize! word)))))
;; (clojurize-keyword! "ParentID of middle-child2")
;; (humanize! "ParentID of middle-child3")
;; (humanize "ParentID of middle-child3")
;; (humanize! "Arbor Size - Fractional")
;; (clear-humanized-words)
;; (humanize "Arbor Size - Fractional")

(defn humanized->id [humanized-string]
  (str/join "_" (str/split humanized-string #"\s")))
;; (assert= "Parent_Id_Of_Middle_Child3" (humanized->id (humanize "ParentID of middle-child3")))



(defn logit [& args]
  (binding [*out* *err*]
    (map clojure.pprint/pprint args))
  (last args))

(defn logit-plain [& args]
  (apply println args)
  (last args))
(defn log-now [obj]
  "stores request in its own file as edn"
  (let [filename (uuid)]
    (spit (str "log/separate/" filename)
          (pr-str obj)))
  obj)

(defn do-log-request
  ([req] (do-log-request req "requests"))
  ([req filename]
   (log-now req)   ;; always save separately
   (spit (str "log/" filename ".log")
         (with-out-str
           (ppa
            [(localtime)
             (if (map? req) (req->printable req) req)
             ]))
         :append true)
   req))
;; (do-log-request 3 "requests")



(defn first-element-equals? [key coll]
  (and (sequential? coll) (= key (first coll))))



(defn floored [x]
  (java.lang.Math/floor (double x)))



(defn select-ranges [rows & ranges]
  (let [r (vec rows)]
    (mapcat #(subvec r (first %) (second %)) ranges)))
(examples
 (assert-=
  (0 1 4)
  (select-ranges [0 1 2 3 4 5 6 7 8 9 10] [0 2] [4 5])
  ))

(defn convert-row-num [row-num num-rows]
  (floored (* num-rows row-num (double 0.01))))

(defn convert-range [a-range num-rows]
  [(convert-row-num (first a-range) num-rows) (convert-row-num (second a-range) num-rows)])

(defn select-percentage-ranges [num-rows rows & ranges]
  (mapcat #(apply subvec (vec rows) (convert-range % num-rows)) ranges))
;; (select-ranges 5 prods [0 20] [30 44])

(defn select-keyword [nested-arr keyword]
  (let [i (atom [])]
    (prewalk #(if (first-element-equals? keyword %) (do (swap! i conj %) %) %) nested-arr)
    @i))
;; (select-keyword [:a [:b 3 4] [:c]] :b)



(defn zero-pad [width string]
  (if string
    (let [s (str (apply str (repeat width "0")) string)]
      (subs s (- (count s) width)))))



(def ^:dynamic level-function-names (list))
(def ^:dynamic levels-to-save 1)
(def ^:dynamic levels-to-print 0)
(def level-results (atom {}))

(defn deflevel-result-handler [result]
  (when (<= (count level-function-names) levels-to-save)
    (swap! level-results assoc level-function-names result))
  (when (<= (count level-function-names) levels-to-print)
    (println "\n------------------------------")
    (println (str "call stack: " level-function-names))
    (clojure.pprint/pprint result)
    (println "....\n")
    )
  result)

(defmacro defun [name args & body]
  (if (or (> levels-to-save 0) (> levels-to-print))
    (println "adding debug logging to" name)
    (println "using plain defn for" name "(not adding debug logging)")
    )
  (if (or (> levels-to-save 0) (> levels-to-print))
    `(defn ~name ~args
       ;(if (and (> (levels-to-print) 0) (= (count level-function-names) 1))
       ;  (println "\n------------------------------"))
       ;(println "level-function-names:" level-function-names (count level-function-names))
       (if (= (count level-function-names) 0)
         (reset! level-results {}))
       (binding [level-function-names (conj level-function-names '~name)]
         (let [result# (do ~@body)]
           (deflevel-result-handler result#)
           result#))
       )
    `(defn ~name ~args ~@body)
    ))



(defmacro macro->fn [m]
  `(fn [& args#]
     (eval
       (cons '~m args#))))


(defn now [] (java.util.Date.))

(defn short-timenow []
  (.format (java.text.SimpleDateFormat. "yyyyMMddHHmmssZ") (now))
  )

(def db-time-format (clj-time.format/formatter "yyyy-MM-dd HH:mm:ss"))
(defn db-timenow []
  (clj-time.format/unparse db-time-format (clj-time.core/now))
  ;; (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss Z") (now))
  )

(defn localtime
  ([] (localtime (now)))
  ([d]
   (let [formatter (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss Z")]
     (.setTimeZone formatter (java.util.TimeZone/getTimeZone "US/Mountain"))
     (.format formatter d))))
;; (localtime)

(defn timenow
  ([] (timenow (now)))
  ([d] (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss Z") d))
  )
;; (timenow)

(defn plain-timenow []
  (clean-all (now))
  )


;(map * [1 2 3] [4 5 6])
;(for [i [1 2 3] j [4 5 5]] (* i j))
;(mapcat (fn[j] (map #(* % j) [4 5 6])) [1 2 3])
;(mapcat (fn[j] (map (fn[i] (* i j)) [4 5 6])) [1 2 3])
;
;(defn zip
;  [& colls]
;  (apply map vector colls))
;
;(for [[i j] (zip [1 2 3] [4 5 6])] (* i j))
;
;(zip [1 2 3] [4 5 6] [7 8 9 9])
;(interleave [1 2 3] [4 5 6])
;(time (dorun (for [x (range 1000) y (range 10000) :while (> x y)] [x y])))
;(time (doall (for [x (range 10) y (range 10) :while (> x y)] [x y])))
;(time (doall (for [x (range 10) y (range 10) :when (> x y)] [x y])))
;(time (dorun (for [x (range 1000) y (range 10000) :when (> x y)] [x y])))
;(for [[x y] '([:a 0] [:b 2] [:c 0]) :when (= y 0)] x)



(defn escape-html
  "Change special characters into HTML character entities."
  [text]
  (.. #^String (str text)
      (replace "&" "&amp;")
      (replace "<" "&lt;")
      (replace ">" "&gt;")
      (replace "\"" "&quot;")))

(defn unescape-html
  "Change special characters into HTML character entities."
  [text]
  (.. #^String (str text)
      (replace "&amp;" "&")
      (replace "&lt;" "<")
      (replace "&gt;" ">")
      (replace "&quot;" "\"")
      (replace "=>" ":")
      ))
#_(defn unescape-fat-arrow-html
  "Change special characters into HTML character entities."
  [text]
  (.. #^String (str text)
      (replace "&amp;" "&")
      (replace "&lt;" "<")
      (replace "&gt;" ">")
      (replace "&quot;" "\"")
      (replace "=>" ":")
      ))


(defn req-sans-unprintable [req]
  #_["compojure.api.middleware/options",
     "cookies",
     "remote-addr",
     "ring.swagger.middleware/data",
     "params",
     "flash",
     "route-params",
     "headers",
     "async-channel",
     "server-port",
     "content-length",
     "form-params",
     "compojure/route",
     "websocket?",
     "session/key",
     "query-params",
     "content-type",
     "path-info",
     "character-encoding",
     "context",
     "uri",
     "server-name",
     "query-string",
     "body",
     "multipart-params",
     "scheme",
     "request-method",
     "session"]
  (let [bad-params [                        ; these throw errors when json-izing
                    :compojure.api.middleware/options
                    :async-channel
                    ]
        x (apply dissoc (concat [req] bad-params))]
    x))



(defn req->printable [req]
  ;; (clean-all (req-sans-unprintable req)))
  (req-sans-unprintable req))

(defn fempty
  "returns function that replaces nil or empty strings with new values"
  [func & defaults]
  (fn [& fargs]
    (let [args (mapv (fn [value default]
                       (if (or (nil? value) (and (seq? value) (empty? value))) default value))
                     fargs defaults)]
      (apply func args))))
;; ((fempty (fn [& args] (println args)) 3 4 6) nil 5 nil)
;; ((fempty (fn [& args] (println args)) 3 4 nil 'abc) nil 5 nil nil)

(defn base64-encode [s]
  (String. (b64/encode (.getBytes s)) "UTF-8"))

(defn object-id [o]
  (System/identityHashCode o))

(defn select-keys3 [m keys]
  (let [keys (set keys)]
    (into {} (filter (fn [[k v]] (if (contains? keys k) [k v])) m))))

;; (select-keys3 {:a 3 "b" 4 "c" 7} [:a "c"])

(defn get-unique [maps key]
  (map #(% key) maps))

(examples
 (get-unique [{:a 4 :b 5 "d" 12} {:a 6 :c 7}] :a)
 (get-unique [{:a 4 :b 5 "d" 12} {:a 6 :c 7}] "d"))

(examples
 (reset-pp)
 )

(println "done loading com.murphydye.utils.core")
(println "-----------------\n\n")
