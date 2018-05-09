;;; -*- lexical-binding: t; -*-

;; allow the test to use untrusted CA cert for the test server
(setq network-security-level 'low)

;; TODO: how can i do this ?
(load-file "../../straight/build/hierarchy/hierarchy.el")

(defun fake-server-port ()
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp ":[0-9]")
    (backward-char)
    (if (looking-at "[0-9]+$")
        (buffer-substring (nth 0 (match-data))
                          (nth 1 (match-data)))
      (ert-fail "cannot find server port"))))

(defun -with-fake-server (args body)
  (let* ((proc (start-process "fake-server"
                              "concourse-fake-server"
                              "go"
                              "run" "src/server/main.go"))
         (orig-url concourse-url)
         (port (progn
                 (accept-process-output proc 5)
                 (with-current-buffer "concourse-fake-server"
                   (fake-server-port)))))
    (setq concourse-url (format "https://localhost:%s" port))
    (unwind-protect
        (funcall body)
      (setq concourse-url orig-url)
      (kill-process proc)
      (accept-process-output proc 5)
      (kill-buffer (process-buffer proc)))))


(defmacro with-fake-server (args &rest body)
  `(-with-fake-server ,args
                      (lambda () ,@body)))

(defun -with-flet (fname func body)
  (let ((orig (symbol-function fname)))
    (fset fname func)
    (unwind-protect
        (funcall body)
      (fset fname orig))))

(defmacro with-flet (args &rest body)
  (let ((fname (nth 0 args))
        (fargs (nth 1 args))
        (fbody (nth 2 args)))
    `(-with-flet ',fname (lambda ,fargs ,fbody) (lambda () ,@body))))

(ert-deftest concourse-test-partial ()
  (let ((second (apply-partially 'nth 1))
        (append-to-foo (apply-partially 'append '("foo"))))
    (should (eql (funcall second '(foo bar baz)) 'bar))
    (should (equal (funcall append-to-foo '("bar")) '("foo" "bar")))))

(defun construct-successful-job ()
  ""
  '((name . "successful") (groups . ["diego"]) (finished_build (status . "succeeded"))))

(defun construct-hanging-job ()
  '((name . "hanging") (groups . ["diego"]) (next_build (start_time . 0))))

(defun construct-pending-job ()
  '((name . "hanging") (groups . ["diego"]) (next_build (status . pending))))

(ert-deftest concourse-job-status ()
  (let ((job (construct-successful-job))
        (hanging (construct-hanging-job))
        (pending (construct-pending-job)))
    (should (eql (assoc-default 'status (concourse-job-status 0 job)) 'succeeded))
    (should (eql (assoc-default 'status (concourse-job-status 0 hanging)) 'hanging))
    (should (eql (assoc-default 'status (concourse-job-status 0 pending)) 'pending))))

(ert-deftest concourse-job-hanging-p ()
  (let* ((hanging (construct-hanging-job))
         (hanging-build (assoc-default 'next_build hanging))
         (pending (construct-pending-job))
         (pending-build (assoc-default 'next_build pending)))
    (should (eql (concourse-job-hanging-p hanging-build 60) t))
    (should (eql (concourse-job-hanging-p pending-build 60) nil))))

(ert-deftest concourse-jobs-status ()
  (let* ((jobs (list (construct-successful-job)))
         (status (concourse-jobs-status 0 jobs)))
    (should (eql (length status) 1))))

(ert-deftest pipeline-render-proper-root-name ()
  (with-fake-server nil
                    (with-temp-buffer
                      (let ((concourse-pipeline "somepipeline"))
                        (concourse-view-pipeline (current-buffer))
                        (goto-char (point-min))
                        (should (looking-at "\\[\\+\\] +somepipeline"))
                        (should (equal 1 (count-lines (point-min) (point-max))))
                        (widget-button-press (point-min))
                        (should (looking-at "\\[\\-\\] +somepipeline"))
                        (should (equal 4 (count-lines (point-min) (point-max))))
                        (forward-line)
                        (should (looking-at " +|\\-\\[\\+\\] +first"))
                        (end-of-line)
                        (should (equal (get-text-property (- (point) 1) 'face)
                                       '(:foreground "red")))
                        (beginning-of-line)
                        (forward-line)
                        (should (looking-at " +|\\-\\[\\+\\] +second"))
                        (forward-line)
                        (should (looking-at " +`\\-\\[\\+\\] +third"))))))

(ert-deftest pipeline-render-pipeline-server-request-test ()
  (with-fake-server nil
                    (with-temp-buffer
                      (concourse-view-pipeline (current-buffer))
                      (widget-button-press (point-min))
                      (with-current-buffer "concourse-fake-server"
                        (search-backward "request: ")
                        (looking-at "/api/v1/teams/main/pipelines/main/jobs")))))

(ert-deftest pipeline-render-pipeline-test ()
  (with-fake-server nil
                    (with-temp-buffer
                      (concourse-view-pipeline (current-buffer))
                      (widget-button-press (point-min))
                      (search-forward "first")
                      (backward-word)
                      ;; test button actions by replacing concourse-view-job
                      (let (button-pressed)
                        (with-flet (concourse-view-job (_)
                                                       (setq button-pressed t))
                          (button-activate (button-at (point)))
                          (should button-pressed))))))

(ert-deftest job-render-job-test ()
  (with-fake-server nil
                    (with-temp-buffer
                      (concourse-view-job '((name . "first")) (current-buffer))
                      (should (looking-at "\\[\\+\\] +first"))
                      (widget-button-press (point-min))
                      (should (equal 3 (count-lines (point-min) (point-max))))
                      (forward-line 1)
                      (should (looking-at " +|\\-\\[\\+\\] +1"))
                      (forward-line 1)
                      (should (looking-at " +`\\-\\[\\+\\] +20"))
                      (end-of-line)
                      (should (equal (get-text-property (- (point) 1) 'face)
                                     '(:foreground "red"))))))

(ert-deftest pipeline-render-job-server-request-test ()
  (with-fake-server nil
                    (with-temp-buffer
                      (concourse-view-job '((name . "first")) (current-buffer))
                      (widget-button-press (point-min))
                      (with-current-buffer "concourse-fake-server"
                        (search-backward "request: ")
                        (looking-at "/api/v1/teams/main/pipelines/main/jobs/first/builds")))))

(ert-deftest job-render-build-log-test ()
  (with-fake-server nil
                    (with-temp-buffer
                      (let ((proc (concourse-view-build '((api_url . "/api/v1/builds/1")) nil (current-buffer))))
                        (while (process-live-p proc)
                          (accept-process-output proc)))
                      (should (equal (buffer-string)
                                     " * Starting PostgreSQL 9.4 database server
   ...done.
Running store-dependent test suites against a Postgres database...
[1518030956] Auctioneer Cmd Suite - 35 specs - 7 nodes •••
")))))

(ert-deftest job-render-build-test ()
  (with-fake-server nil
                    (with-temp-buffer
                      (let ((proc (concourse-view-build '((api_url . "/api/v1/builds/1")) (current-buffer))))
                        (while (process-live-p proc)
                          (accept-process-output proc)))
                      (goto-char (point-min))
                      (should (looking-at "diego-release: ref 6b470ccf3fe8212aede328e0825feaffd066153c")))))

(ert-deftest concourse-get-token-test ()
  (let ((conf (make-temp-file "flyrc")))
    (with-temp-file conf
      (insert "
targets:
  ci:
    api: https://some.ci.cf-app.com
    team: main
    token:
      type: Bearer
      value: foobar
"))
    (let ((concourse-url "https://some.ci.cf-app.com"))
      (should (equal (concourse-get-token conf)
                     "foobar")))
    (let ((concourse-url "https://nonexistent.ci.cf-app.com"))
      (should (null (concourse-get-token conf))))))

(ert-deftest build-id->name-test ()
  (with-fake-server nil
                    (with-temp-buffer
                      (let ((plan (concourse-build-id->name '((api_url . "/api/v1/builds/1")))))
                        (should (equal (gethash "5a61226c" plan)
                                       "diego-release"))
                        (should (equal (gethash "5aab7e1d" plan)
                                       "diego-trace-time-rotor-gcp-benchmark-logs"))
                        (should (equal (gethash "5aab7e1c" plan)
                                       "diego-trace-time-rotor-gcp-benchmark-logs"))))))
