(ns murphydye.utils.fixed-data-table
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r]
            [ajax.core :refer [GET POST]]
            [cljsjs.fixed-data-table]

            [murphydye.utils.core :as utils :refer [ppc]]
            [murphydye.utils.window :as win]
            ))


(defrecord attribute-definition [id keyname title validations children])
(defrecord attribute-validation [validation-definition params])
(defrecord attribute-validation-definition [id keyname title func])
;; specific validation types:
;;   type, regex, one-of|range, required?, multiplicity

(defrecord gui-element [attr-def element-def])
(defrecord gui-element-definition [id keyname])

(defrecord gui-element-properties [visible? width-dim height-dim sort-direction sort-seq filter-val])
(defrecord dimension [value min-value max-value calculattion-fn])




(def Table (r/adapt-react-class js/FixedDataTable.Table))
(def ColumnGroup (r/adapt-react-class js/FixedDataTable.ColumnGroup))
(def Column (r/adapt-react-class js/FixedDataTable.Column))
(def Cell (r/adapt-react-class js/FixedDataTable.Cell))

(defn gen-table
  "Generate `size` rows vector of 4 columns vectors to mock up the table."
  [size]
  (mapv (fn [i] [i                                                   ; Number
                 (rand-int 1000)                                     ; Amount
                 (rand)                                              ; Coeff
                 (rand-nth ["Here" "There" "Nowhere" "Somewhere"])]) ; Store
        (range 1 (inc size))))

;;; using custom :cellDataGetter in column for cljs persistent data structure
;;; is more efficient than converting row to js array in table's :rowGetter
(defn getter [k row] (get row k))

(defn update-width [attr widths]
  (let [id (:id attr)
        col-name id  ;; (name id)
        orig-width (@widths col-name)
        width (or orig-width
                  (if-let [max-len (:max-length attr)] (* max-len 8))
                  150)]
    (if-not (= orig-width width)
      (swap! widths assoc col-name width))
    width))

(defn update-widths [attrs widths]
  (run! #(update-width % widths) attrs))

(defn visible-columns [meta]
  (filter #(:visible? %) @(:attribute-definitions meta)))



;; other than attribute-definitions, each of these is an reagent atom
;; {:attribute-definitions :data :decorator}
;;     attrs:     :validations :max-width
;;     decorator: :filter :width :visible? :sort-direction (map key is id from attribute definition)

(defn prep-table [meta]
  (let [
        visible-data (r/atom (:data meta))
        ]
    ;; (swap! meta assoc :visible-data visible-data)
    (assoc meta :visible-data visible-data)
    ))

(defn show-column [data attr display]
  [Column {:fixed false
           :columnKey :col2
           :header [Cell (:display-name @display)]
           :cell (fn [args]
                   (let [{:keys [columnKey height width rowIndex] :as arg-map} (js->clj args :keywordize-keys true)
                         x (nth (nth data rowIndex) 2)
                         ]
                     (println arg-map)
                     [Cell x]))
           ;; [Cell (str "Row " rowIndex) "."]))
           ;; :cell (fn [{:keys [:columnKey :height :width :rowIndex]}] [Cell rowIndex])
           :isResizable true
           :height 14
           ;; :width (ppc "column 2 width " (or (@widths "col2") 100))
           :width (:width display)
           ;; :flexGrow 1
           }]
  )

(defn show-table [attrs data]
  (let [meta (prep-table meta)
        ;; visible-data (r/atom @data)
        ]
    (fn [attrs data]
      (let [
            ;; attrs (:attribute-definitions meta)
            ;; decorators (:attirubte-definitions meta)
            ;; widths (:widths meta)
            ;; data (:data meta)
            ;; visible-data (:visible-data meta)
            visible-data (r/atom @data)
            table-width (reduce + (map :width (visible-columns meta)))
            ]
        ;; (map #(update-width % decorators) attrs)
        ;; (update-widths attrs widths)
        [Table {
                :groupHeaderHeight 50
                :headerHeight 35
                :rowsCount    (count @visible-data)
                :rowHeight    25
                :height       400
                :width        (min table-width (or (:max-width meta) 9999))
                ;; :rowGetter    #(get table %)
                :isColumnResizing false
                ;; :onColumnResizeEndCallback (fn [width key] (ppc "key/width" (str key " " width)) (swap! widths assoc key width) (ppc "widths" @widths))
                }
         ]
        ))))

(defn combine-table-data [attr-defs decorators data]
  ;; TODO: use data to discover better widths
  (map #(merge % (decorators (:id %))) attr-defs))




(defn column-defaults
  "required: :id :display-name
  optional: :max-length :type :required?"
  [attr-def widths-atom]
  (let [id (:id attr-def)
        col-name id  ;; (name id)
        orig-width (@widths-atom col-name)
        defs {
              :fixed false
              :columnKey id
              :header [Cell (:display-name attr-def)]
              :isResizable true
              :height (or (:height attr-def) 14)
              :width (or orig-width
                         (if-let [max-len (:max-length attr-def)] (* max-len 8))
                         150)
              }]
    (if-not (= orig-width (:width defs))
      (swap! widths-atom assoc col-name (:width defs)))
    defs))

(defn col [data widths attr-def val-fn on-click-fn presentation-def]
  (let [attrs (atom (column-defaults (merge attr-def presentation-def) widths))
        ]
    (swap! attrs assoc
           :cell (fn [args]
                   (let [{:keys [columnKey height width rowIndex] :as arg-map} (js->clj args :keywordize-keys true)
                         x (nth (nth data rowIndex) 2)]
                     (println arg-map)
                     [Cell x]))
           )
    (ppc "attrs 2" @attrs)
    [Column @attrs]))

(defn table-component []
  (let [table  (gen-table 10)
        widths (r/atom {"col1" 100 "col2" 150 "col3" 50})
        ;; widths (r/atom {"col1" 100 "col2" 150 "col3" 150})
        sort-asc? (r/atom true)
        sort-col (r/atom nil)
        ]
    (ppc table)
    (fn []
      [:div
       [Table {
               :width        (reduce + (vals @widths))
               :height       400
               :rowHeight    30
               ;; :rowGetter    #(get table %)
               :rowsCount    (count table)
               :groupHeaderHeight 50
               :onColumnResizeEndCallback (fn [width key] (ppc "key/width" (str key " " width)) (swap! widths assoc key width) (ppc "widths" @widths))
               :isColumnResizing false
               ;; :onColumnResizeEndCallback (fn [width key] (swap! widths assoc key width))
               :headerHeight 50}
        [ColumnGroup {:fixed true
                      :header [Cell "Column Group"]
                      ;; :width 400
                      }
         [Column {:fixed false
                  :columnKey :col1
                  :header [Cell [:a {:on-click (fn [] (win/qgrowl "clicked"))} "Col 1"]]
                  ;; :cell "<Cell>Column 1 static content</Cell>"
                  ;; :cell [Cell "Column 1 static content"]
                  ;; :cell [Cell (str @widths)]
                  :cell (fn [args]
                          (let [{:keys [columnKey height width rowIndex] :as arg-map} (js->clj args :keywordize-keys true)
                                x (second (nth table rowIndex))]
                            [Cell x]))
                  :height 14
                  :width (@widths "col1")
                  ;; :flexGrow 2
                  }]
         [Column {:fixed false
                  :columnKey :col2
                  :header [Cell "Col 2"]
                  :cell (fn [args]
                          (let [{:keys [columnKey height width rowIndex] :as arg-map} (js->clj args :keywordize-keys true)
                                x (nth (nth table rowIndex) 2)]
                            (println arg-map)
                            [Cell x]))
                            ;; [Cell (str "Row " rowIndex) "."]))
                  ;; :cell (fn [{:keys [:columnKey :height :width :rowIndex]}] [Cell rowIndex])
                  :isResizable true
                  :height 14
                  ;; :width (ppc "column 2 width " (or (@widths "col2") 100))
                  :width (@widths "col2")
                  ;; :flexGrow 1
                  }]
         (col table widths {:id 3 :display-name "Col 3"} nil nil nil)
         ;; (defn column [data widths attr-def val-fn on-click-fn presentation-def]
  ;; "required: :id :display-name
  ;; optional: :max-length :type :required?"
         ]
        ]])))



;; want:

;; (examples


;;  (def customer-attributes
;;    [
;;     {:id :email :display-name "Email" :validations {:type :string :max-length 75 :required? true}}
;;     {:id :name :display-name "Name" :validations {:type :group}
;;      :children
;;      [{:id :first-name :display-name "First Name" :type :string :max-length 30 :required? false}
;;       {:id :last-name :display-name "Last Name" :type :string :max-length 30 :required? false}]
;;      }
;;     {:id :service-center-id :display-name "Email" :type :relationship :relationship {:entity :service-center } :required? false}
;;     {:id :address :display-name "Address" :validations {:type :group :required? false}
;;      :relationship
;;      [{:id :street-address :display-name "Street Address" :type :string :max-length 30 :required? false}
;;       {:id :city :display-name "City" :type :string :max-length 30 :required? false}
;;       {:id :zip :display-name "Zip Code" :type :string :max-length 30 :required? false :regex "[0-9]{5}"}]
;;      }
;;     ])
;;  (def customers
;;    [{:first-name "Tommy" :last-name "Smith" :email "tommy@smith.com" :service-center-id 3}
;;     {:first-name "Billy" :last-name "Johns" :email "billy@johns.com"}
;;     ])
;;  (show-table customer-attributes
;;              customers
;;              {:email {:width 200}}
;;              ))

;;  (show-table (combine-attrs-and-decorators
;;                 customer-attributes
;;                 {:email {:width 200}}
;;                 customers)  ;; by passing the data, it can be searched for finding good widths
;;              customers
;;              ))
