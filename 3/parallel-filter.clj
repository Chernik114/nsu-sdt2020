(ns parallel-filter (:gen-class))

(defn parallel-filter
    [f arr threads]
    (reduce concat
        (map deref
            (doall
                (map
                    (fn
                        [arr_part]
                        (future
                            (doall
                                (filter f arr_part)
                            )
                        )
                    )
                    (partition-all
                        (Math/floor
                            (/
                                (count arr)
                                threads
                            )
                        )
                        arr
                    )
                )
            )
        )
    )
)
