(ns murphydye.utils.sap)

(defn as-matnr [string]
  (zero-pad 18 string))

(defn as-document-num [string]
  (zero-pad 10 string))
;; (as-document-num "asdf")

(defn as-short-document-num [string]
  "remove leading zeros"
  (if string (str/replace string #"^0*" "")))
;; (as-short-document-num (as-document-num "00001"))
