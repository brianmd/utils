(ns murphydye.utils.bh)

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

