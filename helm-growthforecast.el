(eval-when-compile (require 'cl))
(require 'url-http)
(require 'helm)
(require 'helm-utils)
(require 'json)

(defvar helm-growthforecast-url "")

(defvar helm-growthforecast-cache-file "~/.helm-growthforecast.cache")
(defvar helm-growthforecast-buffer-prefix "HelmGrowthForecast/")

(defvar helm-growthforecast-graph-buffer-list nil)

(defvar helm-growthforecast-graph-url nil)

(defvar helm-growthforecast-timer nil)
(defvar helm-growthforecast-timer-interval 60)

(defvar helm-growthforecast-graph-params '(("t" . "d")))
(defvar helm-growthforecast-complex-params '(("t" . "d")))

(defun helm-growthforecast-cache-file-retrieve ()
  (let ((buffer (url-retrieve-synchronously
                 (concat helm-growthforecast-url "/json/list/all"))))
    (with-current-buffer (get-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (delete-region (point-min) (1+ (point)))
      (write-file helm-growthforecast-cache-file)
      (kill-buffer))))

(defun helm-growthforecast-clear-cache-file ()
  (when (file-exists-p helm-growthforecast-cache-file)
    (delete-file helm-growthforecast-cache-file)))

(defun helm-growthforecast-group-by-service-name (json-list)
  (let (results)
    (loop for graph in json-list
          for service_name = (cdr (assq 'service_name graph))
          for cell = (assoc service_name results)
          if cell do (push graph (cdr cell))
          else collect (list service_name graph) into results
          finally return results)))

(defun helm-growthforecast-read-graph-data ()
  (unless (file-exists-p helm-growthforecast-cache-file)
    (helm-growthforecast-cache-file-retrieve))
  (let ((json-object-type 'alist)
        (json-array-type 'list))
    (helm-growthforecast-group-by-service-name
     (json-read-file helm-growthforecast-cache-file))))

(defun helm-growthforecast-get-graph-name (graph)
  (let ((service-name (cdr (assq 'service_name graph)))
        (section-name (cdr (assq 'section_name graph)))
        (graph-name   (cdr (assq 'graph_name graph))))
    (format "%s.%s.%s" service-name section-name graph-name)))

(defun helm-growthforecast-query-string (params &optional alist)
  (and alist (add-to-list 'params alist))
  (mapconcat (lambda (x) (concat (car x) "=" (cdr x))) params "&"))

(defun helm-growthforecast-make-graph-path (graph)
  (let* ((service-name (cdr (assq 'service_name graph)))
         (section-name (cdr (assq 'section_name graph)))
         (graph-name   (cdr (assq 'graph_name graph)))
         (gmode        (cdr (assq 'gmode graph)))
         (query-string (helm-growthforecast-query-string
                        helm-growthforecast-graph-params `("gmode" . ,gmode))))
    (format "%s/%s/%s?%s" service-name section-name graph-name query-string)))

(defun helm-growthforecast-make-complex-path (graph)
  (let ((complex-data
         (mapconcat
          (lambda (data)
            (let ((type  (cdr (assq 'type data)))
                  (gid   (cdr (assq 'graph_id data)))
                  (gmode (cdr (assq 'gmode data)))
                  (stack (if (eq (cdr (assq 'stack data)) json-false)
                             "0"
                           "1")))
              (format "%s:%s:%s:%s" type gid gmode stack)))
          (cdr (assq 'data graph)) ":"))
        (query-string (helm-growthforecast-query-string
                       helm-growthforecast-complex-params)))
    (format "%s?%s" complex-data query-string)))

(defun helm-growthforecast-make-path (graph)
  (if (eq (cdr (assq 'complex graph)) json-false)
      (helm-growthforecast-make-graph-path graph)
    (helm-growthforecast-make-complex-path graph)))

(defun helm-growthforecast-transform-candidates (candidates)
  (loop for graph in candidates
        collect (cons (helm-growthforecast-get-graph-name graph)
                      (helm-growthforecast-make-path graph))))

(defun helm-growthforecast-create-image (buffer)
  (let ((bin (with-current-buffer buffer (buffer-string))))
    (create-image (substring bin (+ (string-match "\n\n" bin) 2)) 'png t)))

(defun helm-growthforecast-render-graph (buffer image)
  (with-current-buffer buffer
    (save-excursion
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert-image image)
      (insert "\n")
      (insert (format-time-string "Last update: %Y/%m/%d %H:%M:%S" (current-time)))
      (setq buffer-read-only t)
      (unless (eq major-mode 'helm-growthforecast-mode)
        (helm-growthforecast-mode))))
  (add-to-list 'helm-growthforecast-graph-buffer-list buffer t))

(defun helm-growthforecast-start-timer ()
  (setq helm-growthforecast-timer
        (run-at-time (format "%d sec" helm-growthforecast-timer-interval)
                     helm-growthforecast-timer-interval
                     #'helm-growthforecast-timer-action)))

(defun helm-growthforecast-stop-timer ()
  (when helm-growthforecast-timer
    (cancel-timer helm-growthforecast-timer)
    (setq helm-growthforecast-timer nil)))

(defun helm-growthforecast-async-update (buffer)
  (url-retrieve
   (with-current-buffer buffer helm-growthforecast-graph-url)
   (lambda (status buffer)
     (let ((image (helm-growthforecast-create-image (current-buffer))))
       (helm-growthforecast-render-graph buffer image)
       (kill-buffer)))
   `(,buffer)))

(defun helm-growthforecast-timer-action ()
  (loop for buffer in helm-growthforecast-graph-buffer-list
        if (buffer-live-p buffer) do
        (helm-growthforecast-async-update buffer)
        else do
        (setq helm-growthforecast-graph-buffer-list
              (delq buffer helm-growthforecast-graph-buffer-list)))
  (unless helm-growthforecast-graph-buffer-list
    (helm-growthforecast-stop-timer)))

(defun helm-growthforecast-action (candidate)
  (let* ((url (concat helm-growthforecast-url "/graph/" candidate))
         (image-buffer (url-retrieve-synchronously url))
         (image (helm-growthforecast-create-image image-buffer))
         (buffer (get-buffer-create
                  (concat helm-growthforecast-buffer-prefix candidate))))
    (helm-growthforecast-render-graph buffer image)
    (kill-buffer image-buffer)
    (with-current-buffer buffer
      (setq helm-growthforecast-graph-url url))
    (unless helm-growthforecast-timer
      (helm-growthforecast-start-timer))
    (switch-to-buffer buffer)))

(defun helm-growthforecast-action:update-graph-list (path)
  (helm-growthforecast-clear-cache-file)
  (helm-growthforecast-cache-file-retrieve))

(defvar helm-growthforecast-actions
  '(("Select Graph" . helm-growthforecast-action)
    ("Update Graph List" . helm-growthforecast-action:update-graph-list)))

(defun helm-growthforecast-sources ()
  (loop for data in (helm-growthforecast-read-graph-data)
        for name = (car data)
        for candidates = (helm-growthforecast-transform-candidates (cdr data))
        collect `((name . ,name)
                  (candidates . ,candidates)
                  (action . ,helm-growthforecast-actions))))

(defun helm-growthforecast ()
  (interactive)
  (helm :sources (helm-growthforecast-sources)))

;;
;; Major-mode definition
;;

(defvar helm-growthforecast-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'helm-growthforecast-mode-async-update-current-buffer)
    (define-key map (kbd "q") 'helm-growthforecast-mode-quit)
    map))

(define-derived-mode helm-growthforecast-mode nil
  "Helm-Growthforecast"
  "Major mode for Helm Growthforecast."
  (make-local-variable 'helm-growthforecast-graph-url))

(defun helm-growthforecast-mode-async-update-current-buffer ()
  (interactive)
  (helm-growthforecast-async-update (current-buffer)))

(defun helm-growthforecast-mode-quit ()
  (interactive)
  (when (y-or-n-p "Really quit? ")
    (kill-buffer (current-buffer))))

(provide 'helm-growthforecast)
