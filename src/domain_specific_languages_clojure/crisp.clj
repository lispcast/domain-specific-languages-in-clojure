(ns domain-specific-languages-clojure.crisp)

(def self-evaling?
  (some-fn string? nil? number? boolean?))

(defn crisp-eval [env expr]
  (cond
    (self-evaling? expr)
    expr

    (symbol? expr)
    (if (contains? env expr)
      (get env expr)
      (throw (ex-info "I could not find the symbol in the environment."
               {:symbol expr
                :environment env})))

    (seq? expr)
    (let [[op & args] expr]
      (case op
        if (let [[test then else] args]
             (if (crisp-eval env test)
               (crisp-eval env then)
               (crisp-eval env else)))
        quote (let [[quote-expr] args]
                quote-expr)
        do (last (map #(crisp-eval env %) args))
        let (let [[binding & body] args]
              (cond
                (empty? binding)
                (crisp-eval env (cons 'do body))

                (= 1 (count binding))
                (throw (ex-info "Odd number of binding forms in let."
                         {:bindings binding}))

                :else
                (let [[var val & rst] binding]
                  (crisp-eval (assoc env var val)
                    (cons 'let (cons rst body))))))))

    :else
    (throw (ex-info "I don't know how to evaluate this expression."
             {:expression expr}))))
