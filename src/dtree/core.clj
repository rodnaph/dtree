(ns dtree.core
  (:use incanter.core))

(defn- ^{:doc "Remove the nth element of the vector."}
  remove-nth [data n]
  (concat (take n data)
          (drop (inc n) data)))

(defn ^{:doc "Allows splitting a dataset along the specified
  axis and value.  This means a new dataset is returned, but
  which only includes items which matched the value on the axis,
  and this axis is also removed from these items."}
  split-dataset [data axis value]
  (let [matches-value #(= value (nth (:features %) axis))
        remove-value #(assoc-in % [:features]
                        (remove-nth (:features %) axis))]
    (->> data
      (filter matches-value)
      (map remove-value))))

(defn ^{:doc "Calculate the Shannon Entroy of a dataset. The data
  should be vector of maps, where each map contains the keys for
  its :label, and its :features vector."}
  shannon-entropy [data]
  (let [log2 #(/ (Math/log %) (Math/log 2))
        total (count data)
        freqs (vals (frequencies (map :label data)))
        to-entropy #(let [prob (/ %2 total)]
                       (- %1 (* prob (log2 prob))))]
    (reduce to-entropy 0 freqs)))

(defn ^{:doc "Calculates the entropy of the dataset after splitting it
  at the specified index and value"}
  to-entropy [data [index value]]
  (let [subdata (split-dataset data index value)
        prob (/ (count subdata) (count data))]
	[index (* prob (shannon-entropy subdata))]))

(defn ^{:doc "Maps over the unique values to split the dataset at their
  indexes and then calculates the entropy for each"}
  to-entropies [data uniques]
  (let [indexed (map vector (range 0 (count uniques)) uniques)]
	(map (partial to-entropy data) indexed)))

(defn ^{:doc "Calculates the information gain at the specified index
  from the base entropy of the dataset"}
  to-info-gain [base [index value]]
  {index (- base value)})

(defn ^{:doc "Examines the dataset to find the 'best' feature
  to split on, which is the one that reduces the Shannon
  Entropy of the dataset the most.  The index of the feature
  is returned."}
  best-feature-to-split [data]
  (let [base-entropy (shannon-entropy data)
        features (trans (map :features data))
        uniques (map distinct features)
        entropies (mapcat (partial to-entropies data) uniques)
        ordered (sort #(> (second %1) (second %2)) entropies)]
	(ffirst ordered)))
