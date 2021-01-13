(ns lazy-filter (:gen-class))

(defn lazy-filter
    [f arr threads slice] 
    (lazy-cat
        (let
            [slice (take (* threads slice) arr)]
            (reduce concat
                (map deref
                    (doall
                        (map
                            (fn [arr_part]
                                (future
                                    (doall
                                        (filter f arr_part)
                                    )
                                )
                            )
                            (partition-all threads slice)
                        )
                    )
                )
            )
        )
        (lazy-filter
            f
            (drop
                (* threads slice)
                arr
            )
            threads
            slice
        )
    )
)
