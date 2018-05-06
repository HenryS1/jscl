;; JSCL is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; JSCL is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with JSCL.  If not, see <http://www.gnu.org/licenses/>.

(/debug "loading repl-node/repl.lisp!")

(defvar *rl*)

(defun in-expressionp (input)
  (> (paren-nesting-level input) 0))

(defun node-init ()
  (setq *standard-output*
        (make-stream
         :write-fn (lambda (string)
                     (#j:process:stdout:write string))))
  (setq *rl* (#j:readline:createInterface #j:process:stdin #j:process:stdout))
  (welcome-message)
  (let ((*root* *rl*)
        (input ""))
    (labels ((set-prompt (prompt-str)
               ((oget *rl* "setPrompt") prompt-str))
             (clear-prompt () (set-prompt ""))
             (append-to-input (line)  
               (setf input (concat input #\newline line)))
             (reset-prompt () 
               (progn 
                 (set-prompt "CL-USER> ")
                 (setf input ""))))
      (reset-prompt)
      (#j:prompt)
      (#j:on "line"
             (lambda (line)
               (%js-try
                (handler-case
                    (progn 
                      (append-to-input line)
                      (cond ((or (= (length line) 0) 
                                 (in-expressionp input))
                             (clear-prompt))
                            ((imbalanced-parensp input)
                             (error "imbalanced parentheses"))
                            (t (let ((results (multiple-value-list
                                               (eval-interactive (read-from-string input)))))
                                 (dolist (result results)
                                   (print result))
                                 (reset-prompt)))))
                  (error (err)
                    (reset-prompt)
                    (format t "ERROR: ")
                    (apply #'format t (!condition-args err))
                    (terpri)))
                (catch (err)
                  (let ((message (or (oget err "message") err)))
                    (reset-prompt)
                    (format t "ERROR[!]: ~a~%" message))))
               ;; Continue
               ((oget *rl* "prompt")))))))


(node-init)
