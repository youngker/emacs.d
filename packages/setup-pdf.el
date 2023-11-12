;;; setup-pdf.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package pdf-tools
  :disabled t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (use-package pdf-annot :ensure nil)
  (use-package pdf-links :ensure nil)
  (use-package pdf-info :ensure nil)
  (use-package pdf-misc :ensure nil)
  (use-package pdf-sync :ensure nil)
  (use-package pdf-occur :ensure nil)
  (use-package pdf-outline :ensure nil)
  (use-package pdf-history :ensure nil)
  (pdf-tools-install)
  :hook
  (pdf-view-mode . (lambda ()
		     (pdf-misc-size-indication-minor-mode)
		     (pdf-links-minor-mode)
		     (pdf-isearch-minor-mode)
		     (pdf-outline-minor-mode))))

(provide 'setup-pdf)
;;; setup-pdf.el ends here
