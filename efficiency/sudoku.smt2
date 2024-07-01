
(set-option :produce-models true)
(declare-datatypes () ((Val V1 V2 V3 V4 V5 V6 V7 V8 V9)))
(declare-fun board (Int Int) Val)


(define-fun valid_index ((i Int)) Bool
  (and (>= i 0) (< i 9)))

(assert
  (forall ((row Int) (i Int) (j Int))
    (=>
      (and
        (not (= i j))
        (valid_index row)
        (valid_index i)
        (valid_index j))
      (not (= (board row i)
              (board row j))))))

(assert
  (forall ((col Int) (i Int) (j Int))
    (=>
      (and
        (not (= i j))
        (valid_index col)
        (valid_index i)
        (valid_index j))
      (not (= (board i col)
              (board j col))))))

(assert
  (forall ((row1 Int) (col1 Int)
           (row2 Int) (col2 Int))
    (=>
      (and
        (valid_index row1) (valid_index col1)
        (valid_index row2) (valid_index col2)
        (or (not (= row1 row2))
            (not (= col1 col2)))
        (= (div row1 3) (div row2 3))
        (= (div col1 3) (div col2 3)))
      (not (= (board row1 col1)
              (board row2 col2))))))


(assert (= (board 0 1) V6))
(assert (= (board 0 2) V4))
(assert (= (board 0 6) V7))
(assert (= (board 1 4) V2))
(assert (= (board 1 7) V3))
(assert (= (board 1 8) V6))
(assert (= (board 2 2) V1))
(assert (= (board 3 0) V2))
(assert (= (board 3 1) V3))
(assert (= (board 3 4) V8))
(assert (= (board 4 3) V7))
(assert (= (board 4 6) V1))
(assert (= (board 4 8) V4))
(assert (= (board 6 0) V9))
(assert (= (board 7 0) V8))
(assert (= (board 7 7) V2))
(assert (= (board 8 3) V4))


;; placeholder


(check-sat)
(get-model)
