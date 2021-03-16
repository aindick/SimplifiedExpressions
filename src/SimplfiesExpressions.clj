(ns SimplfiesExpressions)
(def p1 '(and x (or x (and y (not z)))))
(def p2 '(and (and z false) (or x true)))
(def p3 '(or true a))

(defn andexp [e1 e2] (list 'and e1 e2))
(defn orexp  [e1 e2] (list 'or e1 e2))
(defn notexp [e1] (list 'not e1))

(defn and-simplify [e]
  (cond
    (some false? e) false   ;;returns false
    (some symbol? (rest e))     ;;checks for symbol
    (cond
      (> 2
         (count (remove true? (rest e)))) ;;Less than 2 symbols, return the                                                             list w/o "true"
      (first (remove true? (rest e)))

      (<= 2
          (count (remove true? (rest e)))) ;; 2 or more symbols, return the                                                                 list w/o "true"
      (distinct (remove true? e))     ;;returns list without "true"
      ;;distinct removes any duplicates from a list
      )
    :else true ;;If none of the conditions apply, return "true"
    )
  )

(defn not-simplify [e]
  (cond
    (some true? e) false          ;;Replaces "true" with "false"
    (some false? e) true         ;;Replaces "false" with "true"
    (some symbol? e) e
    )
  )

(defn or-simplify [e]
  (cond
    (some true? e) true                             ;;return "true"
    (some symbol? (rest e))                          ;;check symbol
    (cond
      (> 2
         (count (remove false? (rest e))))           ;;if less than 2, remove "false"
      (first (remove false? (rest e)))          ;;then output the rest of the expression

      (<= 2
          (count (remove false? (rest e))))           ;;if greater than or equal to 2, remove "false"
      (distinct (remove false? e))               ;;then remove any duplicates
      )

    )
  )
(defn simplify [e]
  (let [ e (map (fn [k](if (seq? k)           ;;If "k" is a sequence, simplify it.
                         (simplify k)
                         k))
                e)]
    e
    (cond
      (== 0
          (compare (first e) 'and))                                    ;; Checks for "and"

      (and-simplify e)

      (== 0
          (compare (first e) 'not))                                    ;; Checks for "not"

      (not-simplify e)

      (== 0
          (compare (first e) 'or))                                         ;; Checks for "or"

      (or-simplify e)

      :else true
      )
    )
  )


(defn deep-substitute [a b]
  (map (fn [i]
         (if (seq? i)
           (deep-substitute i b)
           (b i i))) a))

(defn evalexp [bindings e]
  (simplify (deep-substitute bindings e))
  )