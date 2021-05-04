# malli-keys-relations

Proof of concept implementation for a discussion on the malli channel on
Clojurians slack of schemas describing relations between keys in a map.

Hopefully it gets integrated with malli in the future.

## Usage

`(require com.github.bsless.malli-keys-relations)`

Either use `-comparator-relation-schemas` to get a map of all current
schemas and add it to your registry, or use the provided `registry` var
which includes them and the default registry.

New schemas:

```clojure
:keys/=
:keys/==
:keys/<
:keys/>
:keys/>=
:keys/<=
:keys/not=
```

Both validation and generation work:

```clojure
(m/validate
   (m/schema
    [:and
     [:map
      [:x nat-int?]
      [:y nat-int?]]
     [:keys/not= :x :y]]
    {:registry registry})
   {:x 1 :y 1})

(mg/generate
   (m/schema
    [:and
     [:map
      [:x nat-int?]
      [:y nat-int?]]
     [:keys/< :x :y]]
    {:registry registry}))
```

## Implementation notes

`:and` generators wouldn't work without overriding the default behavior.

Originally:

```clojure
(defmethod -schema-generator :and [schema options]
  (gen/such-that
   (m/validator schema options)
   (-> schema (m/children options) first (generator options)) 100))
```

Validation is done according to entire schema but generation only by the first child

Now:

```clojure
(defn- derive-from-fmap
  [schema options gen]
  (let [props (merge (m/type-properties schema)
                     (m/properties schema))]
    (when-some [fmap (:gen/fmap props)]
      (gen/fmap (m/eval fmap (or options (m/options schema)))
                gen))))

(defmethod mg/-schema-generator :and [schema options]
  (let [[h & t] (m/children schema options)
        base-gen (mg/generator h options)
        gen (reduce (fn [gen schema]
                      (or (derive-from-fmap schema options gen) gen))
                    base-gen
                    t)]
    (gen/such-that (m/validator schema options) gen 100)))
```

Try to gradually construct a generator from all children if they provide
a `:gen/fmap` and not just a generator.

This could be a general enhancement of `:and` generators.

## License

Copyright © 2021 Ben Sless

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
