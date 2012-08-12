
(ns dtree.core)

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

