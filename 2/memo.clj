(ns memo (:gen-class))

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

(def sum (memoize (fn
    [f d x]
    (if (= x 0.0)
        0.0
        (+
            (sum f d (- x d))
            (trap (f x) (f (- x d)) d)
        )
    )
)))

(def negsum (memoize (fn
    [f d x]
    (if (= x 0.0)
        0.0
        (+
            (negsum f d (+ x d))
            (trap (f x) (f (+ x d)) d)
        )
    )
)))

(defn integral [f d] (fn
    [x]
    (let [
        dx (- x (* (Math/floor (/ x d)) d))
        ]
        (if (> x 0)
            (+
                (sum f d (- x dx))
                (trap (f x) (f (- x dx)) dx)
            )
            (*
                (+
                    (negsum f d (+ x dx))
                    (trap (f x) (f (+ x dx)) dx)
                )
                -1
            )
        )
    ))
)
