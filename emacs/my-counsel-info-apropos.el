;; my-counsel-info-apropos.el --- Info narrowing search -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(require 'info)
(require 'generator)
(require 'ivy)
(require 'seq)

(defun my-info-apropos-manuals (buf)
  "Calculate all known info manuals using fresh buffer `BUF'."
  (when (null Info-directory-list) (info-initialize))
  (with-current-buffer buf
    (let ((Info-fontify-maximum-menu-size nil)
          (manuals '()))
      (Info-mode)
      (Info-find-node-2 "dir" "top")
      (goto-char (point-min))
      (re-search-forward "\\* Menu: *\n" nil t)
      (while (re-search-forward "\\*.*: *(\\([^)]+\\))" nil t)
        (cl-pushnew (match-string 1) manuals :test #'equal))
      manuals)))

(iter-defun my-info-apropos-manual-matches (buf manual)
  "Calculate all nodes in `MANUAL' using Info buffer `BUF'."
  (let ((pattern "\n\\* +\\([^\n]*\\(.*?\\)[^\n]*\\):[ \t]+\\([^\n]+\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?")
        (obuf (current-buffer))
        (opoint (point))
        (Info-fontify-maximum-menu-size nil)
        node nodes)
    (set-buffer buf)
    (condition-case err
        (if (setq nodes (Info-index-nodes (Info-find-file manual)))
            (let ((inner-buf (current-buffer))
                  (inner-point (point))
                  (current-node Info-current-node)
                  (current-file Info-current-file))
              (Info-find-node manual (car nodes))
              (while
                  (progn
                    (goto-char (point-min))
                    (while (progn
                             (set-buffer inner-buf)
                             (goto-char inner-point)
                             (re-search-forward pattern nil t))
                      (let ((entry (match-string-no-properties 1))
                            (nodename (match-string-no-properties 3))
                            (line (match-string-no-properties 4)))
                        (add-text-properties
                         (- (match-beginning 2) (match-beginning 1))
                         (- (match-end 2) (match-beginning 1))
                         '(face info-index-match) entry)
                        (setq inner-buf (current-buffer)
                              inner-point (point))
                        (set-buffer obuf)
                        (iter-yield `(,entry ,nodename ,line))))
                    (setq nodes (cdr nodes) node (car nodes)))
                (progn (Info-goto-node node)
                       (setq current-node Info-current-node
                             current-file Info-current-file)))))
      (error
       (set-buffer obuf)
       (goto-char opoint)
       (message "%s" (if (eq (car-safe err) 'error)
                         (nth 1 err) err))
       (sit-for 1 t)))))

(defvar ivy-regex)
(defvar ivy--all-candidates)

(defun counsel-info-apropos--format-candidate (node)
  "Format `NODE' for display in `MY-COUNSEL-INFO-NODE-FOR-MANUAL'."
  (format "%s - %s" (car node) (cadr node)))

(defun counsel-info-apropos--node-match-p (node)
  "Match `NODE' with `RE' or `IVY-REGEX', predicate only."
  (string-match (ivy-re-to-str ivy-regex)
                (counsel-info-apropos--format-candidate node)))

(defvar counsel-info-apropos-node-history '()
  "Stores the history of my-counsel-info-node-for-manual.")

(defstruct counsel-info-apropos-results-state
  "Keeps track of results of searching Info buffers."
  (nodes
   nil
   :type #'list-p
   :documentation "Accumulates results.")
  (user-typing
   nil
   :type #'booleanp
   :documentation "Non-nil when the user is typing."))

(define-error 'counsel-info-apropos-wait
  "thread should wait")

(defun counsel-info-node-apropos-thread (buf manual state-mx state-cond state)
  "Prepare thread to collect info apropos results.

Use buffer `BUF' and look in manual `MANUAL' with mutex
`STATE-MX'.  Collect results and communicate using `STATE'.

Stop the thread by signaling and continue by notifying
`STATE-COND'."
  (lambda ()
    (let* ((node-iter (my-info-apropos-manual-matches buf manual))
           (n 0))
      (iter-do (node node-iter)
        (condition-case msg
            (progn
              (with-mutex state-mx
                (setf (counsel-info-apropos-results-state-nodes state)
                      (cons node
                            (counsel-info-apropos-results-state-nodes state)))
                ;; Put the formatting of the minibuffer after the
                ;; error catching and then setting candidates can be
                ;; idempotent (and done in the signal handler)
                 (ivy--set-candidates
                  (seq-filter #'counsel-info-apropos--node-match-p
                              (counsel-info-apropos-results-state-nodes state)))
                (ivy--insert-minibuffer
                 (ivy--format ivy--all-candidates))
                (when (= n 0) (redisplay))
                (setq n (mod (1+ n) 64)))
              (thread-yield))
          (error
           (pcase msg
             (`(counsel-info-apropos-wait . ,x)
              (progn
                (message "pausing with data: %s" (pp x))
                (with-mutex state-mx
                  (while (counsel-info-apropos-results-state-user-typing state)
                    (condition-wait state-cond))))))))))
    (message "finished searching manual: %s" manual)))

(defun counsel-info-apropos--collect-input (collection-th state-mx state-cond state)
  "Collect and set ivy candidates as results accumulate.

`COLLECTION-TH' is the result collection thread.  Signals the
thread with `STATE-MX' and `STATE-COND' with the `STATE' so that
input is not blocked."
  (lambda (input)
    (if (thread-live-p collection-th)
        (with-mutex state-mx
          (setf (counsel-info-apropos-results-state-user-typing state)
                t)
          (thread-signal collection-th 'counsel-info-apropos-wait `(,input))
          (let ((candidates
                 (seq-filter #'counsel-info-apropos--node-match-p
                             (counsel-info-apropos-results-state-nodes state))))
            (setf (counsel-info-apropos-results-state-user-typing state)
                  nil)
            (condition-notify state-cond)
            candidates))
      (seq-filter #'counsel-info-apropos--node-match-p
                  (counsel-info-apropos-results-state-nodes state)))))

;; TODO: STRONGLY consider using the cli
;; (cl-letf (((symbol-function 'counsel-locate-cmd-default)
;;            (lambda (input)
;;              (format "info -k %s" input))))
;;   (counsel-locate))

(defun my-counsel-info-node-for-manual (buf manual &key &optional unwind)
  "Ivy complete an Info node in `MANUAL' using Info buffer `BUF'.
Send `UNWIND' to `IVY-READ' when done."
  (cl-letf (((symbol-function 'ivy--dynamic-collection-cands)
             (lambda (input)
               (funcall (ivy-state-collection ivy-last) input))))
    (let* ((ivy-dynamic-exhibit-delay-ms 2)
           (state (make-counsel-info-apropos-results-state))
           (state-mx (make-mutex "ivy-info-apropos-results"))
           (state-cond
            (make-condition-variable state-mx
                                     "ivy-info-apropos-results"))
           (collection-th (make-thread
                           (counsel-info-node-apropos-thread
                            buf manual state-mx state-cond state)
                           "ivy-info-apropos-search-results")))
      (ivy-read "Node: " (counsel-info-apropos--collect-input
                          collection-th state-mx state-cond state)
                :dynamic-collection t
                :history counsel-info-apropos-node-history
                :action (lambda (selection)
                          (Info-find-node (Info-find-file manual)
                                          (if (listp selection)
                                              (cadr selection)
                                            "top"))
                          (when (listp selection)
                            (forward-line
                             (string-to-number (caddr selection)))))
                :unwind (lambda ()
                          (when unwind (funcall unwind))
                          (kill-buffer buf))
                :caller 'my-counsel-info-node-for-manual))))

(ivy-configure 'my-counsel-info-node-for-manual
  :display-transformer-fn #'counsel-info-apropos--format-candidate)

(defun my-counsel-info-manual-apropos ()
  "Ivy complete an Info manual then nodes in that manual."
  (interactive)
  (let* ((ohist Info-history)
         (ohist-list Info-history-list)
         (buf (get-buffer-create "*my-info-apropos-search*"))
         (manuals (my-info-apropos-manuals buf))
         (unwind (lambda ()
                   (setq Info-history ohist
                         Info-history-list ohist-list)))
         (action (lambda (manual)
                   (my-counsel-info-node-for-manual
                    buf manual
                    :unwind unwind))))
    (ivy-read "Manual: " manuals
              :action action
              :require-match t
              :caller 'my-counsel-info-manual-apropos)))

(provide 'my-counsel-info-apropos)
;;; my-counsel-info-apropos ends here
