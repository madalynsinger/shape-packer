(ns madalynsinger.shape-packer.qtree)

(def quadrants [:nw :ne :sw :se])

(defn leaf? [node] (#{:in :out} node))

(defn qmap [f & qnodes]
  (into {}
        (map #(apply f (map % qnodes)))
             quadrants))

(defn eventually [f]
  (fn [& trees]
    (fn []
      (apply f (map #(%) trees)))))

(defn collapse [qnode]  
  (loop [checked {}
         qs (shuffle quadrants)
         leaf nil]
    (if-let [q (peek qs)]
      (let [subnode ((q qnode))]
        (if (or (not (leaf? subnode))
                (and leaf (not= leaf subnode)))
          (into checked (select-keys qnode qs))
          (recur (assoc checked q (fn [] subnode))
                 (pop qs)
                 subnode)))
      leaf)))

(defn bool-op [zero one]
  (eventually
    (fn [& nodes]
      (let [nodes* (remove #{one} nodes)]
        (if (some #{zero} nodes)
          zero
          (case (count nodes*)
            0 one
            1 (first nodes*)
            (collapse
              (apply qmap
                     (partial (bool-op zero one))
                     nodes*))))))))

(def invert
  (eventually
    (fn [node]
      (case node
        :in :out
        :out :in
        (qmap invert node)))))

(def union (bool-op :in :out))

(def intersection (bool-op :out :in))

(defn exclusive-union [& trees]
  (let [nodes (remove #{:out} (map #(%) trees))
        node (case (count nodes)
               0 :out
               1 (first nodes)
               (if (some #{:in} nodes)
                 nil
                 (reduce (fn [acc q]
                           (if-let [subnode (apply exclusive-union
                                                   (map q nodes))]
                             (assoc acc q subnode)
                             (reduced nil)))
                         {}
                         (shuffle quadrants))))]
    (when node
      #(if (leaf? node)
         node
         (collapse node)))))

