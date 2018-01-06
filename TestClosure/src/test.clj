(ns .test)

(defn my_reverse
  [my_list]
  (let [f (fn helper [element]
            (if (list? element)
              (reverse (map helper element))
              element
              )
            )]
    (reverse (map f my_list))
    )
  )

(defn occurence [my_list,element]
  (count (filter #(= % element) (flatten my_list))))

(defn palindrome [string]
  (let [length (count string)
        first_half_end (Math/ceil (/ length 2))
        second_half_start (Math/floor (/ length 2))
        first_half (subs string 0 first_half_end)
        second_half (subs string second_half_start)
        second_half_reverse (apply str (reverse second_half))]
    (= first_half second_half_reverse)
    )
  )

(defn gcd
  [num1,num2]
  (if (zero? num2)
    num1
    (gcd num2 (mod num1 num2))))

(defn f_plus
  [ip1,fp1,ip2,fp2]
  (let [denom (* fp1 fp2)
        numer (+ (* ip1 fp2) (* ip2 fp1))
        gcd_num (gcd (Math/abs numer) (Math/abs denom))]
    (list (/ numer gcd_num) '/ (/ denom gcd_num))))

(defn f_minus
  [ip1,fp1,ip2,fp2]
  (let [denom (* fp1 fp2)
        numer (- (* ip1 fp2) (* ip2 fp1))
        gcd_num (gcd (Math/abs numer) (Math/abs denom))]
    (list (/ numer gcd_num) '/ (/ denom gcd_num))))

(defn f_mult
  [ip1,fp1,ip2,fp2]
  (let [denom (* fp1 fp2)
        numer (* ip1 ip2)
        gcd_num (gcd (Math/abs numer) (Math/abs denom))]
    (list (/ numer gcd_num) '/ (/ denom gcd_num))))

(defn f_div
  [ip1,fp1,ip2,fp2]
  (let [denom (* ip2 fp1)
        numer (* ip1 fp2)
        gcd_num (gcd (Math/abs numer) (Math/abs denom))]
    (list (/ numer gcd_num) '/ (/ denom gcd_num))))

(defn contains_my
  [col el]
  (if (or (nil? col) (empty? col))
    false
    (if (= el (first col))
      true
      (contains_my (rest col) el))))


(defn intersection
  [list1 list2]

  (if (empty? list1)
    '()
    (if (contains_my list2 (first list1))
      (cons (first list1) (intersection (rest list1) list2))
      (intersection (rest list1) list2))
    )
  )

(let [test '((1,2),3,4,5,6)]
  (println (my_reverse test)))

(let [test '((1,2,3,(3)),3,4,5,6)]
  (println (occurence test 3)))

(println (palindrome "ababa"))

(println (intersection '(3) '(1 2 3 4 5)))

(println (f_plus -1 -4 -1 2))

(println (f_minus 1 2 1 4))

(println (f_mult 2 7 3 2))

(println (f_div 1 2 2 3))