(ns domain-specific-languages-clojure.crisp)

(deftype crisp-fn [env arg-list body])

(defn crisp-fn? [x]
  (instance? crisp-fn x))

(def self-evaling?
  (some-fn string? nil? number? boolean? fn?))

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
                (let [[var val & rst] binding
                      evaled-val (crisp-eval env val)]
                  (crisp-eval (assoc env var evaled-val)
                    (cons 'let (cons rst body))))))
        fn (let [[arg-list & body] args]
             (crisp-fn. env arg-list body))
        (let [[f & args] (map #(crisp-eval env %) expr)]
          (cond
            (ifn? f)
            (apply f args)

            (crisp-fn? f)
            (crisp-eval
              (merge (.-env f) (zipmap (.-arg-list f) args))
              (cons 'do (.-body f)))

            :else
            (throw (ex-info "I don't know how to call this as a function."
                     {:expression expr
                      :evaled-value f}))))))

    :else
    (throw (ex-info "I don't know how to evaluate this expression."
             {:expression expr}))))

(defn crisp-compile* [expr]
  (cond
    (self-evaling? expr)
    (fn [env]
      expr)

    (symbol? expr)
    (fn [env]
      (if (contains? env expr)
        (get env expr)
        (throw (ex-info "I could not find the symbol in the environment."
                 {:symbol expr
                  :environment env}))))

    (seq? expr)
    (let [[op & args] expr]
      (case op
        if (let [[testf thenf elsef]
                 (map crisp-compile* args)]
             (fn [env]
               (if (testf env)
                 (thenf env)
                 (elsef env))))
        quote (let [[quote-expr] args]
                (fn [env]
                  quote-expr))
        do (let [bodyfs (map crisp-compile* args)]
             (fn [env]
               (last (map #(% env) bodyfs))))
        let (let [[binding & body] args]
              (cond
                (empty? binding)
                (crisp-compile* (cons 'do body))

                (= 1 (count binding))
                (throw (ex-info "Odd number of binding forms in let."
                         {:bindings binding}))

                :else
                (let [[var val & rst] binding
                      valf (crisp-compile* val)
                      nextf (crisp-compile*
                              (cons 'let (cons rst body)))]
                  (fn [env]
                    (nextf (assoc env var (valf env)))))))
        fn (let [[arg-list & body] args
                 bodyf (crisp-compile* (cons 'do body))]
             (if (empty? arg-list)
               (fn [env]
                 (fn []
                   (bodyf env)))
               (let [nextf (crisp-compile*
                             (cons 'fn (cons (rest arg-list)
                                         body)))
                     argname (first arg-list)]
                 (fn [env]
                   (fn [arg1 & args]
                     (apply
                       (nextf (assoc env argname arg1))
                       args))))))
        (let [[ff & argfs] (map crisp-compile* expr)]
          (fn [env]
            (let [f (ff env)]
              (cond
                (ifn? f)
                (apply f (map #(% env) argfs))

                (crisp-fn? f)
                (crisp-eval
                  (merge (.-env f) (zipmap (.-arg-list f) (map #(% env) argfs)))
                  (cons 'do (.-body f)))

                :else
                (throw (ex-info "I don't know how to call this as a function."
                         {:expression expr
                          :evaled-value f}))))))))

    :else
    (throw (ex-info "I don't know how to evaluate this expression."
             {:expression expr}))))


(defmacro crisp-compile [expr]
  `(sut/crisp-compile* '~expr))
