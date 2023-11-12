;;; 00-performance.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq gc-cons-threshold (* 1024 1024 100))

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Loading done %s with %d gc."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold (* 1024 1024 20))
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'file-name-handler-alist-original)))

(provide '00-performance)
;;; 00-performance.el ends here
