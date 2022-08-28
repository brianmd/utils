(ns murphydye.utils.scrape)

(defn is-search-page? [txt]
  (re-find #"Refine By" txt))

(defn content-for-name [coll name]
  (let [section (first (filter #(= name (% "name")) coll))
        ]
    (case (section "type")
      "TABLE" (->> (section "table") second)
      section)
    )
  )
(examples
 (content-for-name saporder "ET_ORDERS_SUMMARY")
 (content-for-name saporder "ET_ORDERS_DETAIL"))

(defn hselect [parsed v]
  (html/select parsed v))

(defn hdetect [parsed v]
  (first (hselect parsed v)))

(defn platt-page-category [page]
  (cond
    (not-empty (hselect page [:span.ProductPriceOrderBox])) :product
    (not-empty (hselect page [:div.no-products-section])) :not-found
    (not-empty (hselect page [:div.refineByHeader])) :search
    :else :unknown
    ))
;(platt-page-category q)

(defn has-platt-download? [search-str]
  (or
    (.exists (io/as-file (str "/Users/bmd/data/crawler/platt/product/" search-str ".html")))
    (.exists (io/as-file (str "/Users/bmd/data/crawler/platt/search/" search-str ".html")))
    (.exists (io/as-file (str "/Users/bmd/data/crawler/platt/not-found/" search-str ".html")))
    (.exists (io/as-file (str "/Users/bmd/data/crawler/platt/unknown/" search-str ".html")))
    ))

(defn platt-url [search-str]
  (str "https://www.platt.com/search.aspx?q=" search-str "&catNavPage=1&navPage=1_16_0")
  )

(defn html->enlive [html]
  (html/html-resource (java.io.StringReader. html)))

(defn htmlfile->enlive [html]
  (html/html-resource (java.io.FileReader. html)))
(defn save-platt-file [search-str html]
  (let [dir "/Users/bmd/data/crawler/platt/"
        content (html->enlive html)
        category (platt-page-category content)
        filename (str search-str ".html")
        full-filename (str dir (name category) "/" filename)
        ]
    (println full-filename)
    (spit full-filename html)
    ))

(defn force-download-platt [search-str]
  ;(let [url (str "https://www.platt.com/search.aspx?q=" search-str "&catNavPage=1&navPage=1_16_0")
  (let [url (platt-url search-str)
        html (slurp (java.net.URL. url))]
    ;(println url)
    (save-platt-file search-str html)
    html))

(defn download-platt [search-str]
  (if (not (has-platt-download? search-str))
    (force-download-platt search-str)))

(defn slow-download-platt [search-str]
  (if (not (has-platt-download? search-str))
    (do
      (force-download-platt search-str)
      (Thread/sleep 3000))))

;(has-platt-download? "045242309825")
;(.exists (io/as-file "/Users/bmd"))
;(spit "/Users/bmd/data/crawler/platt/045242309825" m)

;(def matnr045242309825 (download-platt "45242309825"))

;matnr045242309825
;(def m matnr045242309825)

;(def m (force-download-platt "781810464731"))
;(def l (force-download-platt "045242309719"))
;(platt-page-category l)
;l
;(def n (html/html-resource (java.net.URL. (platt-url "781810464731"))))
;(def l (html/html-resource (java.net.URL. (platt-url "045242309719"))))
;(def q (html/html-resource (java.net.URL. (platt-url "cutter"))))
;(save-platt-file "045242309719" (doseq l))
;(type l)
;045242309719
;(spit "/Users/bmd/data/crawler/platt/78181046473" n)
;n
;m
;(html/select (html/html-resource m) [:script])
;(html/select l [:span.ProductPriceOrderBox])
;(html/select n [:span])
;
;(-> (html/select l [:span.ProductPriceOrderBox]) first :content first)
;span.ProductPrice
; span.ProductPriceOrderBox
; span.ProductPricePerType


