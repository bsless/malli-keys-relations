(ns com.github.bsless.malli-keys-relations.constraints)

(defprotocol Views
  (-view? [this]))

(defprotocol View
  (-view [this that])
  (-children-views [this])
  (-form [this]))

(let [extend (fn [_protocol this]
               (let [v? (satisfies? View this)]
                 (extend-protocol Views (class this)
                                  (-view? [_] v?))))]
  (extend-protocol Views
    nil
    (-view? [_] false)
    Object,
    (-view? [this] (extend View this) (satisfies? View this))))

(defn -create-form [type children]
  (into [type] children))

(defmethod print-method ::view
  [v ^java.io.Writer w]
  (.write w (pr-str (-form ^View v))))

(defn -const-view
  [x]
  (let [form (-create-form :view/const [x])]
    ^{:type ::view}
    (reify View
      (-view [_ _] x)
      (-children-views [this])
      (-form [_] form))))

(comment
  (def cv (-const-view 1))
  (-view cv 'foo)
  (-children-views cv)
  (-form cv))

(defn- -path-getter
  [path]
  (reduce
   (fn [f e]
     (fn [m]
       (get (f m) e)))
   identity
   path))

(defn -path-view
  [path]
  (let [form (-create-form :view/path path)
        getter (-path-getter path)]
    ^{:type ::view}
    (reify View
      (-view [_ m] (getter m))
      (-children-views [this] path)
      (-form [_] form))))

(comment
  (def pv (-path-view [:a :b]))
  (-view pv {:a {:b 1}})
  (-children-views pv)
  (-form pv))

;; (def -value-where nil)

(defmulti -value-where (fn [args] (if (vector? args) (nth args 0) :view/const)))

(defmethod -value-where :view/const [x] (fn [_] x))

(comment
  (-value-where 1))

(defmethod -value-where :view/path
  [[_ & path]]
  (reduce
   (fn [f e] (fn [m] (get (f m) e)))
   identity
   path))

(comment
  ((-value-where [:view/path :a :b]) {:a {:b 1} :b {:a 2}}))

(defmethod -value-where :view/count
  [[_ coll]]
  (let [f (-value-where coll)]
    (fn [m]
      (count (f m)))))

(comment
  ((-value-where [:view/count [:view/path :a :b]]) {:a {:b [1 2 3]}}))

(defmethod -value-where :view/contains?
  [[_ where what]]
  (let [where' (-value-where where)
        what' (-value-where what)]
    (fn [m]
      (let [where (where' m)
            what (what' m)]
        (boolean
         (cond
           (map? where) (contains? where what)
           (set? where) (contains? where what)
           :else (some #(= what %) where)))))))

(comment
  ((-value-where [:view/contains? [:view/path :a :b] 1]) {:a {:b [1 2 3]}})
  ((-value-where [:view/contains? [:view/path :a :b] -1]) {:a {:b [1 2 3]}})
  ((-value-where [:view/contains? [:view/path :a :b] :c]) {:a {:b {:c 2}}})
  ((-value-where [:view/contains? [:view/path :a :b] :d]) {:a {:b {:c 2}}})
  ((-value-where [:view/contains? [:view/path :a :b] 2]) {:a {:b #{:c 2}}})
  ((-value-where [:view/contains? [:view/path :a :b] :d]) {:a {:b #{:c 2}}}))
