;; counsel-info-apropos.el --- Info narrowing search -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(require 'info)
(require 'generator)
(require 'ivy)
(require 'seq)

(defun counsel-info-apropos--manuals ()
  "Calculate all known info manuals using fresh buffer `BUF'."
  (when (null Info-directory-list) (info-initialize))
  (with-temp-buffer
    (let ((Info-fontify-maximum-menu-size nil)
          (ohist Info-history)
          (ohist-list Info-history-list)
          (manuals '()))
      (Info-mode)
      (Info-find-node-2 "dir" "top")
      (goto-char (point-min))
      (re-search-forward "\\* Menu: *\n" nil t)
      (while (re-search-forward "\\*.*: *(\\([^)]+\\))" nil t)
        (cl-pushnew (match-string 1) manuals :test #'equal))
      (setq Info-history ohist
            Info-history-list ohist-list)
      manuals)))

(iter-defun counsel-info-manual--matches (manual)
  "Calculate all nodes in `MANUAL'."
  (let ((pattern "\n\\* +\\([^\n]*\\(.*?\\)[^\n]*\\):[ \t]+\\([^\n]+\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?")
        (buf (get-buffer-create
              (format "*counsel-info-node-search-%s*" manual)))
        (obuf (current-buffer))
        (opoint (point))
        (ohist Info-history)
        (ohist-list Info-history-list)
        (Info-fontify-maximum-menu-size nil)
        node nodes)
    (set-buffer buf)
    (Info-mode)
    (goto-char (point-min))
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
       (setq Info-history ohist
             Info-history-list ohist-list)
       (set-buffer obuf)
       (goto-char opoint)
       (message "%s" (if (eq (car-safe err) 'error)
                         (nth 1 err) err))
       (sit-for 1 t)))
    (kill-buffer buf)))

(defvar ivy-regex)
(defvar ivy--all-candidates)

(defun counsel-info-apropos--format-candidate (node)
  "Format `NODE' for display in `COUNSEL-INFO-NODE-FOR-MANUAL'."
  (format "%s - %s" (car node) (cadr node)))

(defun counsel-info-apropos--node-match-p (node)
  "Match `NODE' with `RE' or `IVY-REGEX', predicate only."
  (string-match (ivy-re-to-str ivy-regex)
                (counsel-info-apropos--format-candidate node)))

(defvar counsel-info-apropos-timer nil
  "A timer used to collect info nodes in the background.")

(cl-defstruct counsel-info-manual-state
  "Holds the current state of `COUNSEL-INFO-APROPOS'."
  (nodes '())
  iter)

(defun counsel-info-apropos--set-candidates (state candidates)
  "Add `CANDIDATES' to `COUNSEL-INFO-MANUAL-STATE-NODES' nodes in `STATE'.

Setup ivy apropriately."
  (setf (counsel-info-manual-state-nodes state)
        (append (counsel-info-manual-state-nodes state) candidates))
  (ivy--set-candidates
   (seq-filter #'counsel-info-apropos--node-match-p
               (counsel-info-manual-state-nodes state)))
  (ivy--insert-minibuffer (ivy--format ivy--all-candidates)))

(defun counsel-info-apropos--handle-nodes (state n)
  "Collect `N' nodes or until done from `NODE-ITER' and append to `STATE' nodes.

Set the ivy collection accordingly."
  (let ((node-iter (counsel-info-manual-state-iter state))
        (buffer '()))
    (condition-case _
        (let ((node (iter-next node-iter)))
          (while (< (length buffer) n)
            (setq buffer (append buffer (list node)))
            (setq node (iter-next node-iter)))
          (counsel-info-apropos--set-candidates state buffer)
          'continue)
      (iter-end-of-sequence
       (progn
         (counsel-info-apropos--set-candidates state buffer)
         'done)))))

(defun counsel-info-manual--start-timer (state k)
  "Start collecting nodes from `NODE-ITER' into `STATE'.

Run `K' when done."
  (let ((timer-fn
         (lambda ()
           (with-local-quit
             (pcase (counsel-info-apropos--handle-nodes state 50)
               ('continue (counsel-info-manual--start-timer state k))
               ('done (funcall k)))))))
    (setq counsel-info-apropos-timer
          (run-at-time (/ 4 1000.0) nil timer-fn))))

(seq-map (lambda (m) (cons m (make-counsel-info-manual-state)))
         (counsel-info-apropos--manuals))

(defun counsel-info-apropos-for-manual (manual)
  "Search for an Info symbol in `MANUAL'."
  (interactive "sManual: ")
  (unless (seq-contains-p (counsel-info-apropos--manuals) manual #'equal)
    (user-error "Manual not found: %s" manual))
  (cl-letf (((symbol-function 'ivy--dynamic-collection-cands)
             (lambda (input)
               (funcall (ivy-state-collection ivy-last) input))))
    (let* ((ivy-dynamic-exhibit-delay-ms 2)
           (gc-cons-threshold (* 1000 1000 1000 8))
           (state (make-counsel-info-manual-state
                   :iter (counsel-info-manual--matches manual))))
      (counsel-info-manual--start-timer
       state (lambda () (message "Done searching manual: %s" manual)))
      (ivy-read "Node: "
                (lambda (_)
                  (seq-filter
                   #'counsel-info-apropos--node-match-p
                   (counsel-info-manual-state-nodes state)))
                :dynamic-collection t
                :action (lambda (selection)
                          (Info-find-node (Info-find-file manual)
                                          (if (listp selection)
                                              (cadr selection)
                                            "top"))
                          (when (listp selection)
                            (forward-line
                             (string-to-number (caddr selection)))))
                :unwind (lambda ()
                          (when counsel-info-apropos-timer
                            (cancel-timer counsel-info-apropos-timer)))
                :caller 'counsel-info-apropos-for-manual))))

(ivy-configure 'counsel-info-apropos-for-manual
  :display-transformer-fn #'counsel-info-apropos--format-candidate)

(defun counsel-info-manual-apropos ()
  "Ivy complete an Info manual then nodes in that manual."
  (interactive)
  (ivy-read "Manual: " (counsel-info-apropos--manuals)
            :action #'counsel-info-apropos-for-manual
            :require-match t
            :caller 'my-counsel-info-manual-apropos))

(provide 'counsel-info-apropos)
;;; counsel-info-apropos ends here
