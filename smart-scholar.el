;;; smart-scholar.el --- A smart scholar package. -*- lexical-binding: t; -*-

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Hebi Li <lihebi.com@gmail.com>
;; Version: 0.1
;; Keywords: Scholar
;; URL: http://github.com/lihebi/smart-scholar.el

;;; Commentary:

;; This package provides a minor mode to frobnicate and/or
;; bifurcate any flanges you desire.  To activate it, just type

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bibs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom smart-scholar-html-dir "~/.smart-scholar/html"
  "Default directory for storing downloaded htmls."
  :group 'smart-scholar
  :type 'string)
(defcustom smart-scholar-pdf-dir "~/.smart-scholar"
  "Default directory for storing downloaded pdfs."
  :group 'smart-scholar
  :type 'string)
(defcustom smart-scholar-bib-dir "~/.smart-scholar/bib"
  "Default directory for storing generated bibs."
  :group 'smart-scholar
  :type 'string)
(defcustom smart-scholar-manual-bib-dir "~/.smart-scholar/manual-bib/"
  "Manual bib dir."
  :group 'smart-scholar
  :type 'string)

;; This is not technically a configuration
(defcustom smart-scholar-category-alist
  '(("AI" . ("NIPS" "ICML" "ACML" "AISTATS" "COLT"
             "IJCAI" "UAI" "AAAI" "JMLR" "ML" "ICLR"))
    ("CV" . ("ICCV" "CVPR" "ECCV"))
    ("SE" . ("ASE" "PASTE" "FSE" "ICSE" "ISSTA" "MSR"))
    ("System" . ("OSDI" "SOSP" "KDD" "STOC" "VLDB"))
    ("PL" . ("CGO" "ASPLOS" "Onward"
             "OOPSLA" "PLDI" "SIGPLAN" "POPL"
             "Haskell" "ICFP" "LFP"))
    ("NLP" . ("CL" "ACL" "NAACL" "EACL" "EMNLP"))
    ("Robotics" . ("ICRA" "IROS" "TRO")))
  "Conference by categories.  For easy loading."
  :group 'smart-scholar)


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

(defun smart-scholar-manual-bibs ()
  (when (file-exists-p smart-scholar-manual-bib-dir)
    (directory-files smart-scholar-manual-bib-dir
                     t ".*\\.bib")))
(defun smart-scholar-load-manual ()
  (smart-scholar--add-bibs (smart-scholar-manual-bibs)))
(defun smart-scholar-unload-manual ()
  (smart-scholar--remove-bibs (smart-scholar-manual-bibs)))


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

;;;###autoload
(defun set-org-ref-pdfdir ()
  "Add pdf dirs to org-ref and bibtex-completion variables."
  (let ((dirs
         (append (folder-dirs smart-scholar-pdf-dir)
                 ;; FIXME for my convenience
                 (folder-dirs (concat smart-scholar-pdf-dir "/auto"))
                 (folder-dirs (concat smart-scholar-pdf-dir "/manual")))))
    (cl-loop for dir in dirs do
             (add-to-list 'org-ref-pdf-directory dir)
             (add-to-list 'bibtex-completion-library-path dir))))

;; we have so many bib files, thus this function is very costy to
;; execute, about 0.2s. The time consuming part is opening the files
;; and inserting into the temp buffer. Thus, let me create a
;; dedicated buffer for it and load once, and subsequent calls will
;; use this buffer instead of create a new one.

;; TODO add support for turn on such optimization
(defun smart-scholar-load-bibtex-buffer ()
  (interactive)
  (message "Loading hebi-bibtex buffer. This is costy, should
    only do once")
  (with-current-buffer (get-buffer-create "hebi-bibtex")
    (kill-region (point-min)
                 (point-max))
    (mapc #'insert-file-contents
          (seq-filter #'file-exists-p
                      (bibtex-completion-normalize-bibliography 'bibtex)))))

;; with-current-buffer will call save-current-buffer, which although
;; in C code, still cost a lot of time. But this function already
;; saved 100+ times the time spent
;; FIXME will this autoload to overwrite the file?
;;;###autoload
(defun bibtex-completion-get-entry1 (entry-key &optional do-not-find-pdf)
  (when (not (get-buffer "hebi-bibtex"))
    (smart-scholar-load-bibtex-buffer))
  (with-current-buffer "hebi-bibtex"
    (goto-char (point-min))
    (if (re-search-forward (concat "^[ \t]*@\\(" parsebib--bibtex-identifier
                                   "\\)[[:space:]]*[\(\{][[:space:]]*"
                                   (regexp-quote entry-key) "[[:space:]]*,")
                           nil t)
        (let ((entry-type (match-string 1)))
          (reverse (bibtex-completion-prepare-entry
                    (parsebib-read-entry entry-type (point) bibtex-completion-string-hash-table) nil do-not-find-pdf)))
      (progn
        (display-warning :warning (concat "Bibtex-completion couldn't find entry with key \"" entry-key "\"."))
        nil))))


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

(defun link-filter (link)
  ;; (link-filter "https://arxiv.org/abs/1409.0473")
  "Apply all filters for LINK."
  (arxiv-filter
   (ieee-filter-link
    link)))

(defun ieee-filter-link (link)
  ;; <iframe src="https://ieeexplore.ieee.org/ielx2/4535/12853/00593671.pdf?tp=&arnumber=593671&isnumber=12853" frameborder=0>
  (if (string-prefix-p "https://ieeexplore.ieee.org/stamp/stamp.jsp?"
                       link)
      ;; download html
      ;; parse html for the true link
      (with-current-buffer (url-retrieve-synchronously link)
        (goto-char (point-min))
        (when (re-search-forward "<iframe src=\"\\(http[[:ascii:]]*?\\)\"" nil t)
          (match-string 1)))
    link))

(defun arxiv-filter (link)
  ;; (arxiv-filter "http://arxiv.org/abs/1409.0473")
  (if (string-match "^https?://arxiv.org/abs/\\([0-9]+\\.[0-9]+\\)" link)
      (let ((id (match-string-no-properties 1 link)))
        (concat "https://arxiv.org/pdf/" id))
    link))

;;;###autoload
(defun smart-scholar-bibtex-download-pdf-at-point ()
  (interactive)
  (let ((key (smart-scholar-bibtex-key-at-point))
        (pdflink (smart-scholar-bibtex-pdflink-at-point)))
    (let ((conf (second (split-string key "-"))))
      ;; FIXME (auto)
      (let* ((dir (concat smart-scholar-pdf-dir "/auto/" conf "/"))
             (f (concat dir key ".pdf")))
        (when (not (file-exists-p dir))
          (make-directory dir))
        (when (and (not (file-exists-p f))
                   (not (string= pdflink "#f")))
          (url-copy-file
           ;; may contact website, e.g. ieee.org
           (link-filter pdflink) f))))))

;; (url-copy-file "https://arxiv.org/pdf/1409.0473.pdf" "test.pdf")

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
