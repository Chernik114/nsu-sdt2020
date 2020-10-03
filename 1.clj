(defn my-reduce
    [lst agg f]
    (cond
        (= (count lst) 0) agg
        true (recur
            (rest lst)
            (f agg (first lst))
            f
        )
    )
)

(defn task-1-deep
    [chars n lst last_char deep res]
    (cond
        (= n deep) (cons lst res)
        true (my-reduce chars res (fn [l_res l_cur_char]
            (task-1-loop chars n lst last_char l_cur_char deep l_res)
        ))
    )
)

(defn task-1-loop
    [chars n lst last_char cur_char deep res]
    (cond
        (= last_char cur_char) res
        true (task-1-deep chars n (cons cur_char lst) cur_char (inc deep) res)
    )
)

(defn task-1
    [chars n]
    (task-1-deep chars n () nil 0 ())
)
; --------------------------------------------------------
(defn task-1-2
    ([chars n] (task-1-2 chars n () ()))
    ([chars n lst res]
        (cond
            (and (> (count lst) 1) (= (first lst) (first (rest lst))))
                res
            (= (count lst) n)
                (cons lst res)
            true
                (my-reduce chars res #(task-1-2 chars n (cons %2 lst) %1))
        )
    )
)
; ---------------------------------------------------------
(defn task-1-3-w
    [word chars res]
    (cond
        (= (count chars) 0) res
        (= (first word) (first chars)) (recur
            word
            (rest chars)
            res
        )
        true (recur
            word
            (rest chars)
            (cons (cons (first chars) word) res)
        )
    )
)

(defn task-1-3-ws
    [words, chars, res]
    (cond
        (= (count words) 0) res
        true (recur
            (rest words)
            chars
            (task-1-3-w (first words) chars res)
        )
    )
)

(defn task-1-3-a
    [words, chars, i]
    (cond
        (= i 0) words
        true (recur
            (task-1-3-ws words chars ())
            chars
            (dec i)
        )
    )
)

(defn task-1-3
    [chars, i] (task-1-3-a (list ()) chars i)
)
; -----------------------------------------------------
(defn my-map
    [f lst]
    (reduce #(conj %1 (f %2)) () lst)
)

(defn my-filter
    [f lst]
    (reduce #(cond
        (f %2) %1
        true (conj %1 %2)
    ) () lst)
)
; ---------------------------------------------------
(defn task-1-4-w
    [word chars res]
    (concat
        (map #(cons % word)
            (filter #(not (= (first word) %))
                chars
            ))
        res
    )
)

(defn task-1-4-ws
    [words, chars, res]
    (reduce #(task-1-4-w %2 chars %1) res words)
)

(defn task-1-4
    [chars, i]
    (reduce (fn [res, _] (task-1-4-ws res chars ())) (list ()) (range i))
)
; -------------------------------------------------------
(list
    (task-1 (list 1 2 3 4 5) 3)
    (task-1-2 (list 1 2 3 4 5) 3)
    (task-1-3 (list 1 2 3 4 5) 3)
    (task-1-4 (list 1 2 3 4 5) 3)
    (my-map inc (list 1 2 3))
    (my-filter #(> %1 4) (list 1 7 2 8))
)
