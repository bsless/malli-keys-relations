(ns com.github.bsless.malli-keys-relations
  (:require
   [clojure.test.check.generators :as gen]
   [malli.core :as m]
   #_[malli.util :as mu]
   [malli.registry :as mr]
   [malli.error :as me]
   [malli.generator :as mg]))

;;; Generators

(defn- derive-from-fmap
  [schema options gen]
  (let [props (merge (m/type-properties schema)
                     (m/properties schema))]
    (when-some [fmap (:gen/fmap props)]
      (gen/fmap (m/eval fmap (or options (m/options schema)))
                gen))))

;;; Redefine :and behavior:
;;; If subsequent children return `:gen/fmap`, compose it on top of the generator

(defmethod mg/-schema-generator :and [schema options]
  (let [[h & t] (m/children schema options)
        base-gen (mg/generator h options)
        gen (reduce (fn [gen schema]
                      (or (derive-from-fmap schema options gen) gen))
                    base-gen
                    t)]
    (gen/such-that (m/validator schema options) gen 100)))

(defn -unification-fmap
  [a b]
  (fn [m]
    (let [va (get m a)]
      (assoc m b va))))

(defn -unification-gen
  [a b gen]
  (gen/fmap
   (fn [m]
     (let [va (get m a)]
       (assoc m b va)))
   gen))

#_(def -schema-fmap nil)
(defmulti -schema-fmap (fn [type _children] type) :default ::default)
(defmethod -schema-fmap ::default [_ _] nil)

(defmethod -schema-fmap :keys/=
  [_ [a b]]
  (-unification-fmap a b))

(defmethod -schema-fmap :keys/==
  [_ [a b]]
  (-unification-fmap a b))

(defmulti -child-schema identity :default ::default)
(defmethod -child-schema ::default [_] :any)
(defmethod -child-schema :keys/= [_] :any)
(defmethod -child-schema :keys/== [_] number?)
(defmethod -child-schema :keys/>= [_] number?)
(defmethod -child-schema :keys/>  [_] number?)
(defmethod -child-schema :keys/<= [_] number?)
(defmethod -child-schema :keys/<  [_] number?)
(defmethod -child-schema :keys/not= [_] :any)

(defn- -minimal-schema
  [type children]
  (let [s (-child-schema type)]
    (into [:map] (map (fn [child] [child s])) children)))

(defmulti -schema-gen (fn [type _children] type) :default ::default)
(defmethod -schema-gen ::default [type children] (mg/generator (-minimal-schema type children)))

;;;


;;; Errors

(defn -comparator-err-fn
  [a fa b fb msg]
  (fn [{:keys [value]} _]
    (str
     "value at key "
     a ", "
     (fa value)
     ", should be "
     msg
     " value at key "
     b
     ", "
     (fb value))))

(defmulti -comparator-errfn (fn [type _children] type) :default ::default)
(defmethod -comparator-errfn ::default [_ _] nil)

(defmethod -comparator-errfn :keys/=  [_ [a b]] (-comparator-err-fn a #(get % a) b #(get % b) "equal to"))
(defmethod -comparator-errfn :keys/== [_ [a b]] (-comparator-err-fn a #(get % a) b #(get % b) "equal to"))
(defmethod -comparator-errfn :keys/>= [_ [a b]] (-comparator-err-fn a #(get % a) b #(get % b) "greater than or equal to"))
(defmethod -comparator-errfn :keys/>  [_ [a b]] (-comparator-err-fn a #(get % a) b #(get % b) "greater than"))
(defmethod -comparator-errfn :keys/<= [_ [a b]] (-comparator-err-fn a #(get % a) b #(get % b) "lesser than or equal to"))
(defmethod -comparator-errfn :keys/<  [_ [a b]] (-comparator-err-fn a #(get % a) b #(get % b) "lesser than"))

(defmethod -comparator-errfn :keys/not= [_ [a b]] (-comparator-err-fn a #(get % a) b #(get % b) "different from"))

(def keys-relation-prefix "keys")

(defmulti -comperator-fn identity)
(defmethod -comperator-fn :keys/<= [_] <=)
(defmethod -comperator-fn :keys/<  [_] <)
(defmethod -comperator-fn :keys/>  [_] >)
(defmethod -comperator-fn :keys/>= [_] >=)
(defmethod -comperator-fn :keys/=  [_] =)
(defmethod -comperator-fn :keys/== [_] ==)
(defmethod -comperator-fn :keys/not=  [_] not=)

(defn comparator-relation
  [k]
  (let [f (-comperator-fn k)]
    [k
     (m/-simple-schema
      (fn [_properties [a b :as children]]
        {:type k
         :pred (m/-safe-pred #(f (get % a) (get % b))),
         :type-properties
         {:gen/gen (-schema-gen k children)
          :gen/fmap (-schema-fmap k children)
          :error/fn {:en (-comparator-errfn k children)}}
         :min 2,
         :max 2}))]))

(defn -comparator-relation-schemas
  []
  (into
   {}
   (map comparator-relation)
   [:keys/> :keys/>= :keys/= :keys/== :keys/<= :keys/< :keys/not=]))

(def registry
  (mr/registry
   (merge
    (m/default-schemas)
    (-comparator-relation-schemas))))

(comment
  (me/humanize
   (m/explain
    (m/schema
     [:and
      [:map
       [:x :int]
       [:y :int]]
      [:keys/== :x :y]]
     {:registry registry})
    {:x 2 :y 1}))

  (m/entries
   (m/schema
    [:and
     [:map
      [:x nat-int?]
      [:y nat-int?]]
     [:keys/= :x :y]]
    {:registry registry}))

  (mg/generate
   (m/schema
    [:keys/= :x :y]
    {:registry registry}))

  (mg/generate
   (m/schema
    [:keys/== :x :y]
    {:registry registry}))

  (mg/generate
   (m/schema
    [:and
     [:map
      [:x nat-int?]
      [:y nat-int?]]
     [:keys/= :x :y]]
    {:registry registry}))

  (m/children
   (m/schema
    [:and
     [:map
      [:x nat-int?]
      [:y nat-int?]]
     [:keys/= :x :y]]
    {:registry registry}))

  (mg/generate
   (m/schema
    [:and
     [:map
      [:x nat-int?]
      [:y nat-int?]]
     [:keys/< :x :y]]
    {:registry registry}))

  (mg/generate
   (m/schema
    [:and
     [:map
      [:x nat-int?]
      [:y nat-int?]]
     [:keys/not= :x :y]]
    {:registry registry}))

  (m/validate
   (m/schema
    [:and
     [:map
      [:x nat-int?]
      [:y nat-int?]]
     [:keys/not= :x :y]]
    {:registry registry})
   {:x 1 :y 2})

  (m/validate
   (m/schema
    [:and
     [:map
      [:x nat-int?]
      [:y nat-int?]]
     [:keys/not= :x :y]]
    {:registry registry})
   {:x 1 :y 1})

  (mg/generator
   (m/schema
    [:map
     [:x nat-int?]
     [:y nat-int?]])))

