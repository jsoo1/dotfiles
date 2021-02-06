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
  "Stores the results of my-counsel-info-node-for-manual.")

(defun my-counsel-info-node-for-manual (buf manual
                                            &key &optional unwind)
  "Ivy complete an Info node in `MANUAL' using Info buffer `BUF'.
Send `UNWIND' to `IVY-READ' when done."
  (let ((ivy-dynamic-exhibit-delay-ms 2)
        (nodes '())
        (nodes-mx (make-mutex "ivy-info-apropos-nodes")))
    (cl-letf (((symbol-function 'ivy--dynamic-collection-cands)
               (lambda (input)
                 (funcall (ivy-state-collection ivy-last) input))))
      (make-thread
       (lambda ()
         (let* ((node-iter
                 (my-info-apropos-manual-matches buf manual))
                (n 0))
           (iter-do (node node-iter)
             (progn
               (with-mutex nodes-mx
                 (setq nodes (cons node nodes))
                 (ivy--set-candidates
                  (seq-filter #'counsel-info-apropos--node-match-p
                              nodes)))
               (ivy--insert-minibuffer
                (ivy--format ivy--all-candidates))
               (when (= n 0) (redisplay))
               (setq n (mod (1+ n) 50))
               (thread-yield)))))
       "invy-info-apropos-search-results")
      (ivy-read "Node: "
                (lambda (_)
                  (with-mutex nodes-mx
                    (seq-filter #'counsel-info-apropos--node-match-p
                                nodes)))
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
