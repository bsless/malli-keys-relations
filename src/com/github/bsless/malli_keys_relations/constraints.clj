(ns com.github.bsless.malli-keys-relations.constraints)

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
