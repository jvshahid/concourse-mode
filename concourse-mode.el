;;; package --- Summary -*- lexical-binding: t; -*-

;;; concourse-mode.el --- Helper functions to work with Concourse CI

;; Package-Version: 0.0.1

;; Package-Requires: ((hierarchy))

;;; Commentary:

;;; Code:

(require 'json)
(require 'hierarchy)

(defun concourse-get-url (url callback)
  "Retrieves URL and call CALLBACK with the result.)

Either `url-retrieve' or `url-retrieve-synchronously' is used depending on `noninteractive'"
  (let ((buffer (url-retrieve-synchronously url t)))
    (with-current-buffer buffer
      (funcall callback nil))))
  ;; (url-retrieve url callback nil t)

(defun concourse-parse-response (status)
  "Parse HTTP response in the current buffer, where STATUS is the status returned from `concourse-get-url'."
  (unwind-protect
      (progn
        (when (= 0 (buffer-size))
          (error "Received an empty buffer, WTF!!!"))
        (if (plist-get status :error)
            (apply 'signal status))
        (goto-char (point-min))
        (search-forward "\n\n")
        (let ((data (json-read)))
          data))
    (kill-buffer)))

(defun concourse-filter-jobs-by-group (group jobs)
  "Return all jobs in JOBS that belong to the group GROUP."
  (if group
      (cl-remove-if-not (lambda (job)
                          (let ((groups (assoc-default 'groups job)))
                            (cl-find-if (lambda (g) (string-equal g group)) groups)))
                        jobs)
    jobs))

(defun concourse-job-hanging-p (build duration-limit)
  "Return non-nil if the BUILD has been running for more than DURATION-LIMIT seconds."
  (let ((start-time (assoc-default 'start_time build)))
    (when start-time
      (let* ((current-time (round (float-time)))
             (duration (- current-time start-time)))
        (> duration duration-limit)))))

(defun concourse-job-status (duration-limit job)
  "Return an alist containing the name and status of the given JOB.

A JOB is considered hanging if it has been running for more than DURATION-LIMIT seconds"
  (let* ((next-build (assoc-default 'next_build job))
         (previous-build (assoc-default 'finished_build job))
         (name (assoc 'name job))
         (status-from-job (if next-build
                              (if (concourse-job-hanging-p next-build duration-limit)
                                  '(status . hanging)
                                '(status . pending))
                            ;; otherwise, check the previous build
                            (let ((status (assoc-default 'status previous-build)))
                              (if (or (not status) (string-equal status "succeeded"))
                                  '(status . succeeded)
                                '(status . failed))))))
    (list status-from-job name)))

(defun concourse-jobs-status (duration-limit jobs)
  "Return the status of all jobs in JOBS.

Jobs that have been running for more than DURATION-LIMIT are considered `hanging'"
  (cl-map 'list (apply-partially 'concourse-job-status duration-limit) jobs))

(defun concourse-pipeline-summary-from-status (jobs)
  "Return the number of JOBS per status.

A jobs can be in one of the following states, `succeeded', `failed' or `hanging'"
  (cl-labels ((has-status (status) (lambda (x) (eq status
                                                   (assoc-default 'status x))))
              (count-for-status (status list)
                                (length (cl-remove-if-not (has-status status) list))))
    (list (cons 'succeeded (count-for-status 'succeeded jobs))
          (cons 'failed    (count-for-status 'failed jobs))
          (cons 'hanging   (count-for-status 'hanging jobs)))))

(defgroup concourse-mode-configuration nil
  "Customization variables for concourse-mode"
  :group 'emacs)
(defcustom concourse-url "https://diego.ci.cf-app.com"
  "URL of the concourse CI."
  :type 'string
  :group 'concourse-mode-configuration)
(defcustom concourse-pipeline "main"
  "Concourse pipeline name."
  :type 'string
  :group 'concourse-mode-configuration)
(defcustom concourse-team "main"
  "Concourse team name."
  :type 'string
  :group 'concourse-mode-configuration)
(defcustom concourse-group "diego"
  "Concourse group name."
  :type 'string
  :group 'concourse-mode-configuration)
(defcustom concourse-duration-limit (* 60 60)
  "Duration limit in seconds.

Any job that has been running for longer than this value is considered hanging"
  :type 'integer
  :group 'concourse-mode-configuration)

(defvar concourse-pipeline-color nil)

(defun concourse~expand-second (x fs)
  "Insert X as the second argument in the list given by FS.

Internal use only."
  (if (seq-empty-p fs)
      x
    (let ((f (car fs)))
      (if (sequencep f)
          (concourse~expand-second `(,(car f) ,x ,@(cdr f)) (cdr fs))
          (concourse~expand-second `(,f ,x) (cdr fs))))))

(defun concourse~expand-last (x fs)
  "Insert X as the last argument in the list given by FS.

Internal use only."
  (if (seq-empty-p fs)
      x
    (let ((f (car fs)))
      (if (sequencep f)
          (concourse~expand-last `(,@f ,x) (cdr fs))
          (concourse~expand-last `(,f ,x) (cdr fs))))))

(defmacro concourse->> (x &rest fs)
  "Threads X through the forms FS.

Inserts X as the last item in the first form, making a list of it
if it is not a list already.  If there are more forms, inserts the
first form as the last item in second form, etc."
  (concourse~expand-last x fs))

(defmacro concourse-> (x &rest fs)
  "Threads X through the forms FS.

Inserts X as the second item in the first form, making a list of
it if it is not a list already.  If there are more forms, inserts
the first form as the second item in second form, etc."
  (concourse~expand-second x fs))

;;; Emacs mode line update
(defun concourse~mode-line ()
  "Return the text to be used in the mode-line.

Internal use only"
  (concat
   (if concourse-pipeline-color
       (propertize concourse-url 'face (cons 'background-color concourse-pipeline-color))
     (propertize "Updating" 'face (cons 'background-color "orange")))
   " "))

(defun concourse~update-mode-line-from-status (status)
  "Update `concourse-pipeline-color' based on the STATUS.

Internal use only"
  (cond
   ((> (assoc-default 'failed status) 0) (setq concourse-pipeline-color "red"))
   ((> (assoc-default 'hanging status) 0) (setq concourse-pipeline-color "orange"))
   (t (setq concourse-pipeline-color "green")))
  (force-mode-line-update t))

(defun concourse-jobs-path ()
  "Return the url path to the concourse jobs based on `concourse-team' & `concourse-pipeline'."
  (format "/api/v1/teams/%s/pipelines/%s/jobs"
          concourse-team
          concourse-pipeline))

(defun concourse-builds-path (name)
  "Return the url path to the build with the given NAME."
  (format "%s/%s/builds"
          (concourse-jobs-path)
          name))

(defun concourse~update-mode-line ()
  "Retrieve the current pipeline status from concourse and update the mode-line."
  (concourse-get-url
   (format (concat
            concourse-url
            (concourse-jobs-path)))
   (lambda (x)
     (concourse->>
      x
      (concourse-parse-response)
      (concourse-filter-jobs-by-group concourse-group)
      (concourse-jobs-status concourse-duration-limit)
      (concourse-pipeline-summary-from-status)
      (concourse~update-mode-line-from-status)))))

(defvar concourse-timer nil)

(defun concourse~update-mode-line-bg ()
  "Schedule a background update."
  (add-to-list 'mode-line-modes '(:eval (concourse~mode-line)) t)
  (setq concourse-timer (run-at-time 1 60 'concourse~update-mode-line))
  (force-mode-line-update t))

(defun concourse~stop-updating-mode-line ()
  "Stop updating concourse mode-line in the background."
  (delete '(:eval (concourse-mode-line)) mode-line-modes)
  (cancel-timer concourse-timer))

(defvar-local concourse-refresh-func nil
  "the function to use to refresh the current concourse buffer")

(defvar-local concourse-refresh-args nil
  "the args to pass to `concourse-refresh-func")

(defun concourse~colored-name (elem _)
  "Return the name of the passed ELEM.

Color text properties are added based on the element status.  Internal use only."
  (insert " " (cdr (assoc 'name elem)))
  (let* ((build (or (assoc 'finished_build elem)
                    ;; when looking at a job's builds
                    elem))
         (status (cdr (assoc 'status build))))
    (let ((property (cond
                     ((string-equal status "failed") '(:foreground "red"))
                     ((string-equal status "succeeded") '(:foreground "green")))))
      (and property
           (add-face-text-property (point-min)
                                   (point-max)
                                   property)))))

(defun concourse~display-json (root data func buffer)
  "Display the given json DATA as a tree.

ROOT is the name of the root element.  DATA is the json data.
FUNC is the callback function when an element is clicked.  The
tree will be rendered in BUFFER"
  (let ((root `((name . ,root))))
    (let ((h (hierarchy-new)))
      (hierarchy-add-tree h root nil (lambda (n)
                                       (if (equal root n)
                                           (reverse data))))
      (let ((buffer (hierarchy-tree-display h
                                            (hierarchy-labelfn-button
                                             #'concourse~colored-name
                                             (lambda (elem _) (funcall func elem)))
                                            buffer)))
        (with-current-buffer buffer
          (local-set-key (kbd "q") #'kill-current-buffer)
          (local-set-key (kbd "g") #'concourse-refresh)
          buffer)))))

(defun concourse~trigger-job ()
  "Triggers the JOB and refresh the buffer."
  (interactive)
  (let* ((name (cdr (assoc 'name concourse-refresh-args)))
         (builds-path (concourse-builds-path name))
         (url-request-method "POST")
         (token (concourse-get-token "~/.flyrc"))
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " token)))))
    (url-retrieve-synchronously (concat concourse-url builds-path) t))
  (call-interactively #'concourse-refresh))

(defun concourse-refresh ()
  "Refresh the current buffer by calling `concourse-refresh-func'.

`concourse-refresh-func' is passed `concourse-refresh-args' as
args.  The buffer is returned in the same state as before calling
`concourse-refresh', e.g. if the hierarchy was expanded then it
will be left expanded"
  (interactive)
  (let* ((curpoint  (point))
         (open      (concourse-> (point-min)
                                 (widget-at)
                                 (widget-get :parent)
                                 (widget-get :open))))
    (funcall concourse-refresh-func concourse-refresh-args)
    (if open (widget-button-press (point-min)))
    (goto-char curpoint)))

(defun concourse~assoc (x key)
  (alist-get key x))

(defun concourse/get-in (x &rest keys)
  (seq-reduce #'concourse~assoc keys x))

(defun concourse/event-id (event)
  (concourse/get-in event 'data 'data 'origin 'id))

(defun concourse/event-version (event)
  (let ((ver (car (concourse/get-in event 'data 'data 'version))))
    (concat (symbol-name (car ver))
            " "
            (cdr ver))))

(defun concourse/insert-event (event id->name)
  (let ((name (gethash (concourse/event-id event) id->name)))
    (insert (format "%s: %s\n" name (concourse/event-version event)))
    (cl-loop for m across (concourse/get-in event 'data 'data 'metadata)
             do (insert (format "\t%s: %s\n"
                                (concourse~assoc m 'name)
                                (concourse~assoc m 'value))))))

(defun concourse~extract-plan-names (plan &optional tbl)
  (let ((tbl (or tbl (make-hash-table :test 'equal))))
    (let-alist plan
      (cond
       ((or .do .aggregate) (seq-doseq (elem (or .do .aggregate))
                              (concourse~extract-plan-names elem tbl)))

       ((or .get .put)      (puthash .id (or .get.name .put.name) tbl))

       (.ensure             (and (concourse~extract-plan-names .ensure.step   tbl)
                                 (concourse~extract-plan-names .ensure.ensure tbl)))

       (.on_success         (and (concourse~extract-plan-names .on_success.step       tbl)
                                 (concourse~extract-plan-names .on_success.on_success tbl))))
      tbl)))

(defun concourse-build-id->name (build)
  (let* ((url (concourse~assoc build 'api_url))
         (buf (url-retrieve-synchronously (concat concourse-url url "/plan") t)))
    (with-current-buffer buf
      (let-alist (concourse-parse-response nil)
        (concourse~extract-plan-names .plan)))))

(defun concourse-get-token (path)
  (condition-case _
      (with-temp-buffer
        (insert-file-contents path)
        (search-forward (concat "api: " concourse-url))
        (search-forward "type: Bearer")
        (forward-line)
        (search-forward-regexp "value: \\(.*\\)$")
        (buffer-substring (match-beginning 1)
                          (match-end 1)))
    ('search-failed nil)))

(defun concourse-find-file-at-point ()
  "Find file at point in the concourse logs.

This function will replace the build path,
i.e. \"/tmp/build/build-sha\" with \"~/workspace\".  If a line
number is present, e.g. \"foo.go:235\" then jump to that line
number."
  (interactive)
  (let* ((filename (thing-at-point 'filename))
         (lineno   (if (string-match ":\\([0-9]+\\)$" filename)
                       (match-string 1 filename)))
         (filename (replace-regexp-in-string "^/tmp/build/[^/]*\\|:[0-9]+$"
                                             ""
                                             filename)))
    (find-file-other-window (concat "~/workspace/" filename))
    (if lineno
        (goto-line (string-to-number lineno)))))

(defun concourse~kill-build-views ()
  (interactive)
  (kill-buffer "concourse-log-view")
  (kill-buffer "concourse-build-view"))

(defun concourse-view-build (build &optional view-buffer log-buffer)
  (let ((view-buffer (or view-buffer
                         (get-buffer-create "concourse-build-view")))
        (log-buffer (or log-buffer
                        (get-buffer-create "concourse-log-view")))
        (id->name (concourse-build-id->name build)))

    (with-current-buffer view-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (local-set-key (kbd "q") #'concourse~kill-build-views)
      (local-set-key (kbd "s") (lambda ()
                                 (interactive)
                                 (switch-to-buffer log-buffer)))
      (setq buffer-read-only t))

    (with-current-buffer log-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (local-set-key (kbd "q") #'concourse~kill-build-views)
      (local-set-key (kbd "s") (lambda ()
                                 (interactive)
                                 (switch-to-buffer view-buffer)))
      (setq show-trailing-whitespace nil)
      (local-set-key (kbd "f") #'concourse-find-file-at-point)
      (setq buffer-read-only t))

    (let ((buf "")
          id
          event
          data)
      (make-process :name "test-event-stream"
                    :buffer view-buffer
                    :coding 'utf-8
                    :command (append '("curl"
                                       "-k"
                                       "-N"
                                       "--fail"
                                       "--silent")
                                     (if-let (token (concourse-get-token "~/.flyrc"))
                                         (list "-H"
                                               (concat "Authorization: Bearer " token)))
                                     (list (concat concourse-url
                                                   (concourse~assoc build 'api_url)
                                                   "/events")))
                    :connection-type 'pipe
                    :filter (lambda (_ str)
                              (setq buf (concat buf str))
                              (let ((newline? (string-suffix-p "\n" buf))
                                    (lines (split-string buf "\n")))
                                (setq buf "") ;reset buffer
                                (when (not newline?) ; keep the last line in buf if it isn't terminated with \n
                                  (setq buf (car (last lines)))
                                  (setq lines (butlast lines)))
                                (dolist (l lines)
                                  (cond
                                   ((string-prefix-p "id: " l) (setq id (substring l (length "id: "))))
                                   ((string-prefix-p "data: " l) (setq data (substring l (length "data: "))))
                                   ((string-prefix-p "event: " l) (setq event (substring l (length "event: ")))))
                                  (if (and id event data)
                                      (progn
                                        (let ((parsed-data (json-read-from-string data)))
                                          (let-alist parsed-data
                                            (cond
                                             ((string-prefix-p "finish-" .event)
                                              (with-current-buffer view-buffer
                                                (let ((buffer-read-only nil))
                                                  (concourse/insert-event `((id . ,id)
                                                                            (event . ,event)
                                                                            (data . ,parsed-data))
                                                                          id->name))))
                                             ((equal "log" .event)
                                              (with-current-buffer log-buffer
                                                (let ((buffer-read-only nil))
                                                  (save-excursion
                                                    (goto-char (point-max))
                                                    (insert (replace-regexp-in-string
                                                             "\r\\|\e\\[[0-9]+\\(;[0-9]+\\)?m"
                                                             ""
                                                             .data.payload))))))))
                                          (setq id nil
                                                event nil
                                                data nil))))
                                  (if (string-suffix-p "end" event)
                                      (kill-process (get-buffer-process view-buffer))))))
                    :sentinel (lambda (_ str)
                                (unless (string-prefix-p "killed" str)
                                  (message (format "curling Concourse for build events failed: %s" str)))))
      (switch-to-buffer log-buffer))))

(defun concourse-view-job (job &optional buffer)
  (interactive)
  (let ((name (cdr (assoc 'name job))))
    (concourse-get-url (concat concourse-url (concourse-builds-path name))
                       (lambda (x)
                         (let ((data (concourse-parse-response x))
                               (buffer (or buffer
                                           (get-buffer-create "concourse-job-view"))))
                           (concourse~display-json name data #'concourse-view-build buffer)
                           (switch-to-buffer buffer)
                           (local-set-key (kbd "t") #'concourse~trigger-job)
                           (setq-local concourse-refresh-func #'concourse-view-job)
                           (setq-local concourse-refresh-args job))))))

;;;###autoload
(defun concourse-view-pipeline (&optional buffer)
  (interactive)
  (concourse-get-url (concat concourse-url (concourse-jobs-path))
                     (lambda (x)
                       (let ((data (concourse-parse-response x))
                             (buffer (or buffer
                                         (get-buffer-create "concourse-pipeline-view"))))
                         (concourse~display-json concourse-pipeline
                                                 data
                                                 #'concourse-view-job buffer)
                         (switch-to-buffer buffer)
                         (setq-local concourse-refresh-func #'concourse-view-pipeline)))))

;;;###autoload
(defun concourse-mode ()
  (interactive)
  (if concourse-timer
      (progn
        (concourse~stop-updating-mode-line)
        (setq concourse-timer nil)
        (setq concourse-pipeline-color nil))
    (concourse~update-mode-line-bg)))

(provide 'concourse-mode)
;;; concourse-mode.el ends here
