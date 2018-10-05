;; Download from conference's website


(defun thecvf-html-url (year conf)
  (let* ((url (concat "http://openaccess.thecvf.com/" conf
                      (int-to-string year) ".py")))
    url))

;; http://openaccess.thecvf.com/CVPR2018.py

(defun cvpr-html-url (year)
  "Download cvpr by year"
  (cond
   ((member year '(2013 2014 2015 2016 2017 2018))
    (thecvf-html-url year "CVPR"))
   (t "unsupported")))

(defun iccv-html-url (year)
  (cond
   ((member year '(2013 2015 2017))
    (thecvf-html-url year "ICCV"))
   (t "unsupported")))

(defun smart-scholar-local-path (year conf)
  (let* ((dir (expand-file-name conf smart-scholar-html-dir))
         (filename (expand-file-name
                    (concat conf "-" (int-to-string year) ".html") dir)))
    filename))

(defun download-conf-html! (url local)
  (let ((dir (file-name-directory local)))
    (when (not (file-exists-p dir))
      (make-directory dir)))
  (when (not (file-exists-p local))
    (url-copy-file url local)))

(defvar cvpr-years '(2013 2014 2015 2016 2017 2018))

(defun download-all-cvpr ()
  (cl-loop for year in cvpr-years
           do (download-conf-html!
               (cvpr-html-url year)
               (smart-scholar-local-path year "CVPR"))))

(defvar iccv-years '(2013 2015 2017))

(defun download-all-iccv ()
  (cl-loop for year in iccv-years
           do (download-conf-html!
               (iccv-html-url year)
               (smart-scholar-local-path year "ICCV"))))



;; (download-all-cvpr)
;; (download-all-iccv)


