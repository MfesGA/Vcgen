(set-option :print-success true)
(set-logic QF_AUFLIA)
(set-option :produce-models true)
(declare-fun x () Int)
(push 1)
(assert (not (=> (and (and (< 100 x) (<= x 1000)) (not (< x 1000)))
                 (= x 1000))))
(check-sat)
(pop 1)
(get-proof)
