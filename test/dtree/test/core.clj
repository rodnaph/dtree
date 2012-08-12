
(ns dtree.test.core
  (:use dtree.core
        midje.sweet))

(def data [
  {:label "yes" :features [1 1]},
  {:label "yes" :features [1 1]},
  {:label "no" :features [1 0]},
  {:label "no" :features [0 1]},
  {:label "no" :features [0 1]}
])

(facts "about shannon entropies"

  (shannon-entropy data) => 0.9709505944546686)

(facts "about splitting datasets"

  (split-dataset data 0 1)
    => [{:label "yes" :features [1]}
        {:label "yes" :features [1]}
        {:label "no" :features [0]}]

  (split-dataset data 0 0)
    => [{:label "no" :features [1]}
        {:label "no" :features [1]}])

