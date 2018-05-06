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

(/debug "loading repl-web/repl.lisp!")

(defun %write-string (string &optional (escape t))
  (if #j:jqconsole
      (#j:jqconsole:Write string "jqconsole-output" "" escape)
      (#j:console:log string)))

(defun load-history ()
  (let ((raw (#j:localStorage:getItem "jqhist")))
    (unless (js-null-p raw)
      (#j:jqconsole:SetHistory (#j:JSON:parse raw)))))

(defun save-history ()
  (#j:localStorage:setItem "jqhist" (#j:JSON:stringify (#j:jqconsole:GetHistory))))


;;; Decides wheater the input the user has entered is completed or we
;;; should accept one more line.
(defun indent-level (string)
  ;; We should use something based on DEPTH in order to make
  ;; editing nice, but the behaviour is a bit weird with
  ;; jqconsole.
  (if (<= (paren-nesting-level string) 0)
      nil 
      0))

(defun toplevel ()
  (labels ((reset-prompt () 
             (let ((prompt (format nil "~a> " (package-name *package*))))
               (#j:jqconsole:SetPromptLabel prompt "")))
           (clear-prompt ()
             (progn
               (#j:jqconsole:SetPromptLabel "" "")
               (#j:jqconsole:Write "" "jqconsole-prompt")))
           (process-input (line)
             (#j:jqconsole:RegisterMatching "(" ")" "parens")
             ;; Capture unhandled Javascript exceptions. We evaluate the
             ;; form and set successp to T. However, if a non-local exit
             ;; happens, we cancel it, so it is not propagated more.
             (%js-try
              
              ;; Capture unhandled Lisp conditions.
              (handler-case
                  (let ((input line))
                    (cond ((string= input "")
                           (progn 
                             (clear-prompt)
                             (display-repl)))
                          ((imbalanced-parensp input)
                           (error "imbalanced parentheses"))
                          (t (let* ((form (read-from-string input))
                                    (results (multiple-value-list (eval-interactive form))))
                               (reset-prompt)
                               (dolist (x results)
                                 (#j:jqconsole:Write (format nil "~S~%" x) "jqconsole-return"))))))
                (error (err)
                           (#j:jqconsole:Write "ERROR: " "jqconsole-error")
                           (#j:jqconsole:Write (apply #'format nil 
                                                      (!condition-args err)) "jqconsole-error")
                           (#j:jqconsole:Write (string #\newline) "jqconsole-error")))
              
              (catch (err)
                (#j:console:log err)
                (let ((message (or (oget err "message") err)))
                  (#j:jqconsole:Write (format nil "ERROR[!]: ~a~%" message) "jqconsole-error"))))
             
             (save-history)
             (display-repl))
           (display-repl ()
             (#j:jqconsole:Prompt t #'process-input #'indent-level)))
    (reset-prompt)
    (display-repl)))


(defun web-init ()
  (load-history)
  (setq *standard-output*
        (make-stream
         :write-fn (lambda (string) (%write-string string))))
  (welcome-message :html t)
  (#j:window:addEventListener "load" (lambda (&rest args) (toplevel))))

(web-init)
