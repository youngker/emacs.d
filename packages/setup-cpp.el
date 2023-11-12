;;; setup-cpp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package google-c-style
  :preface
  (defun c-mode-common-setup-hook ()
    (google-set-c-style)
    (google-make-newline-indent)
    (c-add-style "my-c-style"
                 '("Google"
                   (tab-width . 4)
                   (c-basic-offset . 4)
                   (indent-tabs-mode . nil)
                   (c-auto-newline . nil)
                   (c-electric-flag . t)
                   (c-offsets-alist . ((access-label . -)
                                       (case-label . 0)
                                       (inlambda . 0)
                                       (statement-case-intro . +)
                                       (statement-case-open . 0)
                                       (member-init-intro . +)))))
    (c-set-style "my-c-style")
    (local-unset-key (kbd "C-c ."))
    (flymake-mode +1)
    (modern-c++-font-lock-mode +1))
  :hook
  (c-mode-common . c-mode-common-setup-hook))

(use-package cmake-mode
  :mode (("CMake[^/\\]*\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package cmake-project
  :diminish
  :preface
  (defun maybe-cmake-project-hook ()
    (if (file-exists-p "CMakeLists.txt")
        (cmake-project-mode)))
  :hook
  ((c-mode c++-mode) . maybe-cmake-project-hook))

(use-package clang-format
  :commands clang-format-buffer
  :bind
  ("C-c c r" . clang-format-buffer))

(use-package modern-cpp-font-lock
  :diminish
  :hook
  (c++-mode . modern-c++-font-lock-mode))

(use-package gdb-mi
  :ensure nil
  :bind
  (("C-c c d" . gdb)
   ("C-c g r" . gdb-restore-windows)
   ("C-c g m" . gdb-display-memory-buffer)
   ("C-c g a" . gdb-display-disassembly-buffer))
  :init
  ;; Force gdb-mi to not dedicate any windows
  (advice-add 'gdb-display-buffer
              :around (lambda (orig-fun &rest r)
                        (let ((window (apply orig-fun r)))
                          (set-window-dedicated-p window nil)
                          window)))
  (advice-add 'gdb-set-window-buffer
              :around (lambda (orig-fun name &optional ignore-dedicated window)
                        (funcall orig-fun name ignore-dedicated window)
                        (set-window-dedicated-p window nil)))
  :config
  (setq gdb-many-windows t
        gdb-non-stop-setting t
        gdb-show-changed-values t
        gdb-show-main t
        gdb-stack-buffer-addresses t
        gdb-stack-buffer-locations t
        gdb-switch-when-another-stopped t
        gdb-thread-buffer-addresses t
        gdb-thread-buffer-arguments t
        gdb-thread-buffer-locations t
        gdb-thread-buffer-verbose-names t
        gdb-use-colon-colon-notation t))

(use-package qml-mode
  :mode ("\\.qml\\'" . qml-mode))

(provide 'setup-cpp)
;;; setup-cpp.el ends here
