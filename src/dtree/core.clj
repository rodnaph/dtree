
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

