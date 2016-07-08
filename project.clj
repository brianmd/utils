(defproject com.murphydye/utils "0.1.1"
  :description "Random, unorganized utility functions"
  :url "https://github.com/brianmd/utils.git"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.taoensso/carmine "2.12.2"]
                 [clj-http "2.1.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [semantic-csv "0.1.0"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/data.xml "0.0.8"]
                 ;; [im.chit/hara.reflect "2.2.17"]

                 [cheshire "5.6.1"]
                 [com.rpl/specter "0.11.0"]
                 [enlive "1.1.6"]
                 [clj-time "0.11.0"]
                 ;; [com.rpl/specter "0.11.1-SNAPSHOT"]
                 [me.raynes/conch "0.8.0"]
                 [potemkin "0.4.3"]
                 [snipsnap "0.1.0" :exclusions [org.clojure/clojure]]
                 ;; [org.clojure/java.jdbc "0.4.2"]  removed because korma requires 0.3.7
                 [org.clojure/java.jdbc "0.3.7"]
                 [korma "0.4.2"]
                 [com.draines/postal "1.11.3"]              ; email support
                 [org.clojure/data.xml "0.0.8"]
                 [com.cemerick/url "0.1.1"]
                 [incanter "1.5.7"]
                 [dk.ative/docjure "1.10.0"]    ; access to excel
                 [potemkin "0.4.3"]

                 ;; cljs libraries
                 [siren "0.2.0"]
                 [cljsjs/fixed-data-table "0.6.0-1"]
                 ]
  :source-paths ["src/clj" "src/cljs"])
