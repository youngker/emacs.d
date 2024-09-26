;;; setup-ai.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :defines
  (gptel-model
   gptel-backend)
  :functions
  (gptel-make-ollama)
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (setq gptel-model "llama3.1:latest"
        gptel-backend (gptel-make-ollama "Ollama"
                        :stream t
                        :host "localhost:11434"
                        :models '("llama3.1:latest"
                                  "llama3.2:latest"
                                  "mistral:latest"
                                  "llava:latest"
                                  "phi:latest"
                                  "tinyllama:latest"
                                  "gemma:latest"
                                  "codegemma:latest"))))

(provide 'setup-ai)
;;; setup-ai.el ends here
