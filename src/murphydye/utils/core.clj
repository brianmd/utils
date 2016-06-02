(println "\n\n-----------------")
(println "loading library com.murphydye.utils.core")
(println "-----------------\n\n")

(ns murphydye.utils.core
  (:require
    ;; [clj-http.client :as client]
    ;; [cheshire.core :refer :all]
    ;; [config.core :refer [env]]
    [clj-http.client :as client]
    [clojure.walk :refer :all]
    [clojure.pprint :refer [pprint]]
    [clojure.string :as str]
    [clojure.java.io :as io]

    [cheshire.core :as ches]
    [cheshire.generate :refer [add-encoder encode-str remove-encoder]]
    [taoensso.carmine :as car :refer (wcar)]
    ;[cats.core :as m]
    ;[cats.builtin]
    ;[cats.monad.maybe :as maybe]

    [korma.core :as k]
    [korma.db :as kdb]

    [clojure.java.jdbc :as j]

    [treadstone.config :refer [env]]
    ;; [me.raynes.conch :refer [programs with-programs let-programs] :as sh]
    [com.rpl.specter :as s]
    [net.cgrand.enlive-html :as html]
    ))

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

(def ppout *out*)
(defn reset-pp []
  (def ppout *out*))
;; (reset-pp)

(defn ppn [& args]
  (binding [*out* ppout]
    (println "\n-------------------")
    (doseq [arg args] (pprint arg))))

(defn pp [& args]
  (apply ppn args)
  (last args))

(defn pp->str [obj]
  (with-out-str (pprint obj)))

(defn uuid [] (java.util.UUID/randomUUID))
;; (uuid)


(def dbs (atom {}))
(def ^:dynamic *db* kdb/*current-db*)


(defmacro dselect [& args]
  `(k/select ~@args))

(defmacro ddetect [& args]
  `(first (dselect ~@args)))

(defn new-mysql-connection [m]
  (korma.db/mysql m))

(defn find-db [db-name]
  (if-let [db (db-name @dbs)]
    db
    (when-let [db (new-mysql-connection (-> env :db db-name))]
      (swap! dbs assoc db-name db)
      db)))

(defn jquery* [conn sql-vector]
  (j/query conn sql-vector))

(defn jquery [conn sql-string]
  (jquery* conn [sql-string]))

(defn exec-sql
  ([sql]
   (k/exec-raw sql :results))
  ([conn sql]
   (if (= :default conn)
     (exec-sql sql)
     (let [conn (if (keyword? conn) (find-db conn) conn)]
       (k/exec-raw conn sql :results)))))
;; (exec-sql "select count(*) from customers")
;; (exec-sql :default "select count(*) from customers")
;; (exec-sql :bh-neo "select count(*) from customers")
;; (exec-sql :bh-dev "select count(*) from customers")
;; (exec-sql :mdm-local "select count(*) from idw_manufacturer")

(def protected-write-dbs (atom #{:bh-prod}))

(defn write-sql [conn sql]
  "use for insert/update queries"
  (if (contains? protected-write-dbs conn)
    (throw (Exception. "attempting to write to a protected database"))
    (exec-sql conn sql)))

(def step-input-path (-> env :paths :local :step-input-path))
(def step-output-path (-> env :paths :local :step-output-path))


(defn default-env-setting [key]
  (let [default (-> env :defaults key)]
    (-> env key default)))
;; (default-env-setting :redis)
;; (default-env-setting :db)
;; ((default-env-setting :db) :local)

(println "(an error here indicates you need a profile.clj file with redis settings!)")

;; (def redis-conn {:pool {} :spec (default-env-setting :redis)})
;; (defmacro wcar* [& body] `(car/wcar redis-conn ~@body))

;; (wcar* (car/ping))


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
        s (str/replace skey #"_" "-")
        s (str/replace s #"[^A-Z-][A-Z]" #(str (first %) "-" (second %)))
        s (str/lower-case s)
        ;; s (str/join (map #(if (and (<= 65 (int %)) (<= (int %) 90)) (str \- (str/lower-case %)) %) s))
        k (keyword (if (= \- (first s)) (subs s 1) s))]
      k))

(defn clojurize-keyword
  "convert mixed case to lower case with hyphens. Considers 'ID' as a token.
  Memoized, so can override this function's output with your own."
  [key]
  (let [skey (if (keyword? key) (name key) (str key))]
    (if-let [k (@clojurized-keywords skey)]
      k
      (clojurize-keyword! skey))))

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

(defn clojurize-map-keywords [m]
  (s/transform (s/walker keyword?)
               clojurize-keyword
               m))
(examples
 (clojurize-keyword :ParentID)
 @clojurized-keywords
 (clojurize-map-keywords {:ParentID "394" :SubMap {:TestID ["a" 3 :ParentTrap]}})
 )

(defn save-to-x
  "may be used in a threading macro"
  [obj]
  (def x obj)
  x)

(defn logit [& args]
  (binding [*out* *err*]
    (map pprint args))
  (last args))

(defn logit-plain [& args]
  (apply println args)
  (last args))

(defn first-element-equals? [key coll]
  (and (sequential? coll) (= key (first coll))))

(defn floored [x]
  (java.lang.Math/floor (double x)))

(defn select-ranges [rows & ranges]
  (let [r (vec rows)]
    (mapcat #(subvec r (first %) (second %)) ranges)))
;; (select-ranges [0 1 2 3 4 5 6 7 8 9 10] [0 2] [4 5])

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

(defn as-matnr [string]
  (zero-pad 18 string))

(defn as-document-num [string]
  (zero-pad 10 string))
;; (as-document-num "asdf")

(defn as-short-document-num [string]
  "remove leading zeros"
  (if string (str/replace string #"^0*" "")))
;; (as-short-document-num (as-document-num "00001"))

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

(defn bh_login [email pw]
  (let [cred
        {"customer"
         {"email" email, "password" pw}
         "session"
         {"customer"
          {"email" email, "password" pw}
          }}
        params
        {:body         (ches/generate-string cred)
         :content-type :json
         :accept       :json}
        result (client/post
                "https://www.summit.com/store/customers/sign_in.json"
                params)
        ;; (clojurize-map-keywords
        result (assoc result :body (ches/parse-string (:body result)))
        m (clojurize-map (clojure.walk/keywordize-keys result))]
    (assoc m
           :auth-token (:X-CSRF-Token (:headers m))
           :customer (-> m :body :customers first)
           )))


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
    (pprint result)
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


(defn hselect [parsed v]
  (html/select parsed v))

(defn hdetect [parsed v]
  (first (hselect parsed v)))

(defn html->enlive [html]
  (html/html-resource (java.io.StringReader. html)))

(defn htmlfile->enlive [html]
  (html/html-resource (java.io.FileReader. html)))



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
           (pp
            [(localtime)
             (if (map? req) (req->printable req) req)
             ]))
         :append true)
   req))
;; (do-log-request 3 "requests")
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

(defn object-id [o]
  (System/identityHashCode o))

(examples
 (reset-pp)
 )

(println "done loading com.murphydye.utils.core")
