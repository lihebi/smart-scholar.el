(defconst stop-words '("for" "the" "with" "and" "under" "via" "from" "data"
                       "towards" "approach" "fast" "using"
                       "algorithm" "algorithms" "new" "efficient" "analysis"
                       "beyond" "rank" "time" "without"
                       "high" "point" "word" "risk" "based" "machine" "faster"
                       "method" "object"
                       ;; not so trivial
                       "learning" "networks" "network" "model" "models"
                       "prediction" "estimation"))

(defun count-word-frequency (words)
  ;; return ((hello 128), (word 54))
  (sort
   (remove-if
    (lambda (x)
      (< (second x) 3))
    (let ((unique-words
           (remove-if (lambda (x)
                        (or (< (length x) 3)
                            (member-ignore-case x stop-words)))
                      (remove-duplicates words :test #'string=))))
      (mapcar (lambda (word)
                (list word (count word words :test #'string=)))
              unique-words)))
   (lambda (x y)
     (> (second x)
        (second y)))))

(defun word-frequency->string (freq)
  ;; ((hello 128), (word 54))
  (string-join
   (mapcar (lambda (x)
             (concat (first x) " " (number-to-string (second x))))
           freq)
   "\n"))


;;;###autoload
(defun smart-scholar-cluster-count-word-frequency ()
  ;; in current buffer, count word frequency, return a list 
  (interactive)
  (princ "==============================\n")
  (let ((str (downcase
              (buffer-substring-no-properties (point-min) (point-max)))))
    (let ((words (split-string str)))
      (let ((marked-words (count-word-frequency words)))
        (with-current-buffer "*smart-scholar-cluster*"
          (kill-region (point-min) (point-max))
          (insert (word-frequency->string marked-words)))))))



(defconst topics '("graphical"
                   "causal"
                   "bayesian"
                   "reinforcement"
                   "recurrent"
                   "convolutional"
                   "probabili"
                   "adversarial"
                   "kernel"
                   "fairness"
                   "gradient"
                   "variational"
                   "attention"
                   "random"
                   "sampl"
                   "regression"
                   "submodular"
                   "online"
                   "embedding"
                   "stochastic"
                   "sparse"
                   "bandit"
                   "linear"
                   "cluster"
                   "transfer"
                   "process"
                   ;; uai
                   "inference"
                   "latent"
                   "belief"
                   "influence"
                   "mdp"
                   "dempster-shafer"
                   "evidenc"
                   "markov"
                   "gaussian"
                   "classification"
                   "information"
                   "selection"
                   "search"
                   "constraint"
                   "collaborative"
                   "plan"
                   "video"
                   ;; AAAI common words
                   "social"
                   ;; more general words
                   "adaptive"
                   "decision"
                   "reason"
                   "uncertain"
                   "matrix"
                   "optimal"
                   "generative"
                   "unsupervise"
                   "graph"
                   "optimization"
                   "neural"
                   "deep"))

(defun line-mark-topics (lines)
  "Return ((topic1 (line1 line2)) (topic2 (line3 line4)))"
  (mapcar (lambda (s)
            (list s
                  (first
                   (append
                    (remove-if-not
                     (lambda (x)
                       (string-match-p (regexp-quote x)
                                       s))
                     topics)
                    '("other")))))
          lines))

(defun group-topics (marked-lines)
  (mapcar (lambda (topic)
            (list topic
                  (mapcar #'first
                          (remove-if-not (lambda (x) (string= (second x) topic))
                                         marked-lines))))
          (append topics '("other"))))

(defun lines->org (marked-lines)
  (apply #'concat
         (mapcar (lambda (x)
                   (let ((topic (first x))
                         (lsts (second x)))
                     (concat "* " topic "\n"
                             (string-join lsts "\n")
                             "\n")))
                 marked-lines)))

;;;###autoload
(defun smart-scholar-cluster-auto-group ()
  "scan each line, and according to the topics, group them"
  (interactive)
  (princ "=========================\n")
  (let ((buffer (get-buffer-create "*smart-scholar-cluster*")))
    (let ((min (if (use-region-p)
                   (region-beginning)
                 (point-min)))
          (max (if (use-region-p)
                   (region-end)
                 (point-max))))
      (let ((str (buffer-substring-no-properties min max)))
        (let ((lines
               (remove-if
                (lambda (s) (= (length s) 0))
                (split-string str "\n"))))
          (with-current-buffer buffer
            (kill-region (point-min) (point-max))
            (insert (lines->org (group-topics (line-mark-topics lines)))))))))
  nil)


(defun get-all-titles (str)
  (let ((lines (split-string str "\n")))
    (remove nil
            (mapcar (lambda (s)
                      (when (string-match "\\btitle={\\(.*\\)}" s)
                        (match-string-no-properties 1 s)))
                    lines))))

(defun get-all-ids (str)
  (let ((lines (split-string str "\n")))
    (remove nil
            (mapcar (lambda (s)
                      (when (string-match "inproceedings{\\(.*\\)," s)
                        (match-string-no-properties 1 s)))
                    lines))))


(defun bib->org-str ()
  "from bib file to org lines"
  ;; 1. get all title={}
  ;; 2. get all inproceedings{xxx,
  ;; zip together
  (let ((str (buffer-substring-no-properties (point-min) (point-max))))
    (let ((titles (get-all-titles str))
          (ids (get-all-ids str)))
      (if (= (length titles) (length ids))
          (string-join
           (mapcar* (lambda (title id)
                      (concat "** " title " cite:" id))
                    titles ids)
           "\n")
        "Unmatched title and IDs"))))

;;;###autoload
(defun smart-scholar-cluster-bib->org ()
  "Call in a bib file, will populate the *smart-scholar-cluster*
buffer with org titles for each paper."
  (interactive)
  (let ((str (bib->org-str)))
    (with-current-buffer "*smart-scholar-cluster*"
      (kill-region (point-min) (point-max))
      (insert str))))

(defun get-cite-id (line)
  (string-match "cite:\\([[:digit:]]+-[^-]+\\)*" line)
  (match-string-no-properties 1 line))
(defun get-cite-year (line)
  (string-match "cite:\\([[:digit:]]+\\)-" line)
  (match-string-no-properties 1 line))

(defun get-region-string ()
  (buffer-substring-no-properties (region-beginning)
                                  (region-end)))
(defun kill-region-string ()
  (kill-region (region-beginning)
               (region-end)))

;; (get-cite-id "Sort region of org file, based on cite:2017-NIPS-XXX-XXX")
(defun lines->group-sort->str (lines)
  "group and sort lines, and generate string"
  (string-join
   (mapcar
    (lambda (x)
      (string-join (rest x) "\n"))
    (seq-group-by
     #'get-cite-year
     (sort lines (lambda (x y)
                   (string< (get-cite-id x)
                            (get-cite-id y))))))
   "\n\n"))

;;;###autoload
(defun smart-scholar-cluster-sort-region ()
  "Sort region of org file, based on cite:2017-NIPS-XXX-XXX"
  ;; 1. get all lines
  ;; 2. a function to extract 2017-NIPS
  ;; 3. sort and group based on this id
  (interactive)
  (let ((str (buffer-substring-no-properties (region-beginning)
                                             (region-end))))
    (let ((lines
           (remove "" (mapcar #'string-trim
                              (split-string str "\n")))))
      (let ((res (lines->group-sort->str lines)))
        (kill-region (region-beginning)
                     (region-end))
        (insert "\n")
        (insert res)
        (insert "\n")))))


(provide 'cluster)
;;; cluster.el ends here
