;;; smart-scholar.el --- A smart scholar package. -*- lexical-binding: t; -*-

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Hebi Li <lihebi.com@gmail.com>
;; Version: 0.1
;; Keywords: Scholar
;; URL: http://example.com/smart-scholar

;;; Commentary:

;; This package provides a minor mode to frobnicate and/or
;; bifurcate any flanges you desire.  To activate it, just type

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bibs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom smart-scholar-html-dir "~/github/smart-scholar-dist/html"
  "Default directory for storing downloaded htmls.")
(defcustom smart-scholar-pdf-dir "~/github/smart-scholar-pdfs"
  "Default directory for storing downloaded pdfs.")
(defcustom smart-scholar-bib-dir "~/github/smart-scholar-dist/bib"
  "Default directory for storing generated bibs.")

(defcustom smart-scholar-category-alist
  '(("AI" . ("NIPS" "ICML" "ACML" "AISTATS" "COLT"
             "IJCAI" "UAI" "AAAI" "JMLR" "ML" "ICCV" "CVPR"))
    ("SE" . ("ASE" "PASTE" "FSE" "ICSE" "ISSTA" "MSR"))
    ("System" . ("OSDI" "SOSP" "KDD" "STOC" "VLDB"))
    ("PL" . ("CGO" "ASPLOS" "Onward"
             "OOPSLA" "PLDI" "SIGPLAN" "POPL"
             "Haskell" "ICFP" "LFP")))
  "Conference by categories.  For easy loading.")

(defcustom smart-scholar-manual-bib-dir
  "~/github/research/bib/"
  "Manual bib dir")

(defun smart-scholar-categories ()
  (mapcar #'car smart-scholar-category-alist))
(defun category->conferences (cate)
  (cdr (assoc cate smart-scholar-category-alist)))
(defun conference->category (conf-name)
  (car (rassoc-if (lambda (x)
                    (member conf-name x))
                  smart-scholar-category-alist)))
(defun smart-scholar-conferences ()
  (apply #'append
         (mapcar #'category->conferences (smart-scholar-categories))))

;; (category->conferences "PL")
;; (conference->category "SOSP")

(defun smart-scholar--add-bibs (bibs)
  (cl-loop for bib in bibs do
           (add-to-list 'reftex-default-bibliography bib)
           (add-to-list 'bibtex-completion-bibliography bib)
           (add-to-list 'org-ref-default-bibliography bib)))
(defun smart-scholar--remove-bibs (bibs)
  (cl-loop for bib in bibs do
           (setq reftex-default-bibliography
                 (remove bib reftex-default-bibliography))
           (setq bibtex-completion-bibliography
                 (remove bib bibtex-completion-bibliography))
           (setq org-ref-default-bibliography
                 (remove bib org-ref-default-bibliography))))

(defun smart-scholar-loaded-bibs ()
  org-ref-default-bibliography)

(defun conf-bib-files (conf-name)
  "Get conference bib files."
  (directory-files
   (concat (file-name-as-directory smart-scholar-bib-dir)
           conf-name)
   t ".*\\.bib"))

(defun smart-scholar-load-conf (conf)
  (smart-scholar--add-bibs (conf-bib-files conf)))

(defun smart-scholar-unload-conf (conf)
  (smart-scholar--remove-bibs (conf-bib-files conf)))


;;;###autoload
(defun smart-scholar-load-bib (load-what)
  "Load bib into org-ref."
  (interactive
   (list
    (completing-read "choose one conf: "
                     (append (smart-scholar-categories)
                             '("manual")))))
  (cond
   ((member load-what (smart-scholar-categories))
    (let ((confs
           (cdr (assoc load-what smart-scholar-category-alist))))
      (smart-scholar--add-bibs
       (apply #'append
              (mapcar #'conf-bib-files
                      confs)))))
   ((member load-what '("manual"))
    (smart-scholar--add-bibs
     (directory-files smart-scholar-manual-bib-dir
                      t ".*\\.bib")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PDF dir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun folder-dirs (folder)
  "find the folders inside another folder, except . and .."
  (when (file-exists-p folder)
    (delete-if-not
     'file-directory-p
     (mapcar (lambda(arg)
               (file-name-as-directory
                (concat (file-name-as-directory folder) arg)))
             (delete-if (lambda (arg)
                          (or (string= ".." arg) (string= "." arg)))
                        (directory-files folder))))))

(defun set-org-ref-pdfdir ()
  "Add pdf dirs to org-ref and bibtex-completion variables."
  (let ((dirs
         (append (folder-dirs "~/github/smart-scholar-pdfs/auto")
                 (folder-dirs "~/github/smart-scholar-pdfs/manual"))))
    (cl-loop for dir in dirs do
             (add-to-list 'org-ref-pdf-directory dir)
             (add-to-list 'bibtex-completion-library-path dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Download pdf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
