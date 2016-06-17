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
  (if (contains? @protected-write-dbs conn)
    (throw (Exception. "attempting to write to a protected database"))
    (exec-sql conn sql)))

(defn ->column-name [tbl-name]
  (str/lower-case (->str tbl-name)))
(defn ->table-name [tbl-name]
  (str/lower-case (->str tbl-name)))

(defn select-by-colname
  ([tbl-name colname id] (exec-sql (str "select * from " (->table-name tbl-name) " where " (->column-name colname) "=" id)))
  ([conn tbl-name colname id] (exec-sql conn (str "select * from " (->table-name tbl-name) " where " (->column-name colname) "=" id)))
  )
(defn find-by-colname
  ;; ([tbl-name colname id] (first (exec-sql (str "select * from " (->table-name tbl-name) " where " (->column-name colname) "=" id))))
  ;; ([conn tbl-name colname id] (first (exec-sql conn (str "select * from " (->table-name tbl-name) " where " (->column-name colname) "=" id))))
  ([tbl-name colname id] (first (select-by-colname tbl-name colname id)))
  ([conn tbl-name colname id] (first (select-by-colname conn tbl-name colname id)))
  )
(defn find-by-id
  ([tbl-name id] (find-by-colname tbl-name "id" id))
  ([conn tbl-name id] (find-by-colname tbl-name "id" id))
  )
;; (find-by-colname :customers :id 28)
;; (find-by-id :customers 28)

