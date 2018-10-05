;;; -*- lexical-binding: t; -*-

(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar smart-scholar-widgets '())
(defvar test-widget)

(defun get-children-widgets (cate)
  (mapcar (lambda (conf-name)
          (plist-get smart-scholar-widgets (intern conf-name)))
        (category->conferences cate)))

(defun get-widget (name)
  (plist-get smart-scholar-widgets (intern name)))

(defun loaded-conferences ()
  ;; FIXME this is very slow, but I don't want to maintain two copies
  ;; of loaded conferences
  (seq-filter (lambda (conf)
                (seq-filter (lambda (bib)
                              (string-match-p (regexp-quote conf) bib))
                            (smart-scholar-loaded-bibs)))
              (smart-scholar-conferences)))

(defun update-widgets-for-loaded ()
  (mapc (lambda (conf)
          (widget-value-set (get-widget conf) t))
        (loaded-conferences))
  (update-widgets))

(defun update-widgets (&optional name)
  "NAME is the most recently changed. If no NAME is provided,
update the category by its children conferences."
  (if (not name)
      (mapc (lambda (cate)
              (let ((cate-widget (get-widget cate))
                    (children-widgets (get-children-widgets cate)))
                (widget-value-set cate-widget
                                  (null (seq-filter (lambda (x)
                                                      (not (widget-value x)))
                                                    children-widgets)))))
            (smart-scholar-categories))
    (if (member name (smart-scholar-categories))
        ;; change all children
        (let ((widget (get-widget name))
              (children-widgets (get-children-widgets name)))
          (mapcar (lambda (w)
                    (widget-value-set w (widget-value widget)))
                  children-widgets))
      ;; change its parent
      (let* ((cate (conference->category name))
             (cate-widget (get-widget cate))
             (children-widgets (get-children-widgets cate)))
        (widget-value-set cate-widget
                          (null (seq-filter (lambda (x)
                                              (not (widget-value x)))
                                            children-widgets)))))))

(defun get-selected-conf ()
  (seq-filter (lambda (conf)
                (widget-value (get-widget conf)))
              (smart-scholar-conferences)))

(defun get-unselected-conf ()
  (seq-filter (lambda (conf)
                (not (widget-value (get-widget conf))))
              (smart-scholar-conferences)))

(defun smart-scholar-reload ()
  "Reload current selection."
  (mapc #'smart-scholar-load-conf (get-selected-conf))
  (mapc #'smart-scholar-unload-conf (get-unselected-conf)))



(defun create-named-checkbox (name)
  "Named checkbox with :name NAME property and update-widgets
callback."
  (let ((w (widget-create
            'checkbox
            :notify (lambda (widget &rest ignore)
                      (update-widgets (widget-get widget ':name)))
            nil)))
    (widget-put w ':name name)
    w))

;;;###autoload
(defun smart-scholar ()
  "Create UI for smart scholar."
  (interactive)
  (let ((buffer (get-buffer-create "*Space Repetition*")))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (make-local-variable 'smart-scholar-widgets)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
      ;; FIXME why I have to put a dummy entry?
      (setq smart-scholar-widgets '(dummy 1))

      (widget-insert "Smart Scholar User Interface.\n\n")

      (mapcar (lambda (cate)
                (let ((confs (category->conferences cate)))

                  (plist-put smart-scholar-widgets
                             (intern cate)
                             (create-named-checkbox cate))
              
                  (widget-insert (concat " " cate "\n"))
                  (mapc (lambda (conf)
                          (widget-insert "  ")
                          (plist-put smart-scholar-widgets
                                     (intern conf)
                                     (create-named-checkbox conf))
                          (widget-insert (concat " " conf "\n")))
                        confs)))
              (smart-scholar-categories))

      (update-widgets-for-loaded)

      (widget-insert "\n")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               ;; reload all the bibs selected in this buffer
                               (smart-scholar-reload)
                               (smart-scholar)
                               (message "Applied"))
                     "Save & Reload")
      (widget-insert "   ")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (smart-scholar))
                     "Reset")
      (widget-insert "\n")
      (widget-insert "\n")
      (widget-insert "Current Loaded bibs:\n")
  
      (widget-insert (string-join (smart-scholar-loaded-bibs) "\n- "))
      ;; refresh using "g"
      ;; TODO other keymaps
      ;; TODO binding for swith checkbox
      (let ((mymap (copy-keymap widget-keymap)))
        (define-key mymap (kbd "g") 'smart-scholar)
        (use-local-map mymap))
      (widget-setup)
      (goto-char (point-min))
      (display-buffer buffer))))

