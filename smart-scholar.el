;;; smart-scholar.el --- A smart scholar package. -*- lexical-binding: t; -*-

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Hebi Li <lihebi.com@gmail.com>
;; Version: 0.1
;; Keywords: Scholar
;; URL: http://example.com/smart-scholar

;;; Commentary:

;; This package provides a minor mode to frobnicate and/or
;; bifurcate any flanges you desire.  To activate it, just type


;; download acm pdf

;;;###autoload
(defun smart-scholar-bibtex-key-at-point ()
  (interactive)
  (bibtex-completion-key-at-point))

;;;###autoload
(defun smart-scholar-bibtex-pdflink-at-point ()
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (re-search-forward "pdflink={\\(.*\\)}" nil 'move)
    (match-string-no-properties 1)))

;;;###autoload
(defun smart-scholar-bibtex-download-pdf-at-point ()
  (interactive)
  (let ((key (smart-scholar-bibtex-key-at-point))
        (pdflink (smart-scholar-bibtex-pdflink-at-point)))
    (let ((conf (second (split-string key "-"))))
      (let* ((dir (concat "~/github/research/pdf/auto/" conf "/"))
             (f (concat dir key ".pdf")))
        (when (not (file-exists-p dir))
          (make-directory dir))
        (when (and (not (file-exists-p f))
                   (not (string= pdflink "#f")))
          (url-copy-file pdflink f))))))
;;;###autoload
(defun doi-utils-get-bibtex-entry-pdf ()
  (smart-scholar-bibtex-download-pdf-at-point))

;;;###autoload
(defun smart-scholar-bibtex-download-all-pdf ()
  (interactive)
  ;; 1. create a buffer
  ;; 2. sleep for 5 sec between each download
  (save-excursion
    (goto-char (point-min))
    (while (smart-scholar-download-next)
      t)))

;;;###autoload
(defun smart-scholar-download-next ()
  (when (re-search-forward "@inproceedings" nil 'move)
    (when (smart-scholar-bibtex-download-pdf-at-point)
      (prin1 "Sleep for 10 sec ..")
      (sleep-for 10))
    t))

(provide 'smart-scholar)
