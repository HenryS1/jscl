
;; Paren nesting check for REPLs
(test (= (paren-nesting-level "(((") 3))
(test (= (paren-nesting-level "()") 0))
(test (= (paren-nesting-level "())") -1))
;; If the nesting level becomes negative 
;; the function returns immediately to 
;; indicate imbalanced parentheses
(test (= (paren-nesting-level "())(") -1))

;; Imbalanced parentheses check
(test (not (imbalanced-parensp "()")))
(test (not (imbalanced-parensp "(())")))
(test (imbalanced-parensp ")"))
(test (imbalanced-parensp "())"))
