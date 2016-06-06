(ns murphydye.utils.db)

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
