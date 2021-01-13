(ns lazy (:gen-class))

(defn trap
    [a b h]
    (/
        (*
            (+ a b)
            h
        )
        2
    )
)

(defn sum
    [f d]
    (map
        first
        (iterate
            (fn
                [[s i]]
                [
                    (+
                        s
                        (trap (f i) (f (- i d)) d)
                    )
                    (+ i d)
                ]
            )
            [0.0 d]
        )
    )
)

(defn negsum
    [f d]
    (map
        first
        (iterate
            (fn
                [[s i]]
                [
                    (+
                        s
                        (trap (f i) (f (+ i d)) d)
                    )
                    (- i d)
                ]
            )
            [0.0 (* -1 d)]
        )
    )
)

(defn integral [f d] (fn
    [b]
    (let
        [dx (- b (* (Math/floor (/ b d)) d))]
        (if (> b 0)
            (+
                (nth
                    (sum f d)
                    (Math/floor (/ b d))
                )
                (trap (f b) (f (- b dx)) dx)
            )
            (*
                (+
                    (nth
                        (negsum f d)
                        (Math/floor (/ (* -1 b) d))
                    )
                    (trap (f b) (f (+ b dx)) dx)
                )
                -1
            )
        )
    )
))
