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
  (setq litellm-backend
        (gptel-make-openai "litellm"
          :host (plist-get (car (auth-source-search :user "apikey" :max 1)) :host)
          :protocol "https"
          :endpoint "/v1/chat/completions"
          :key 'gptel-api-key-from-auth-source
          :models '(gemini-3.1-flash-lite claude-haiku-4-5 codestral-2 claude-sonnet-4-6)))

  (setq ollama-backend (gptel-make-ollama "ollama"
                         :stream t
                         :host "localhost:11434"
                         :models '(gemma4:latest)))

  (setq gptel-sources (list litellm-backend
                            ollama-backend))
  ;;(setq gptel-track-response nil)
  (setq gptel-backend ollama-backend)
  (setq gptel-model "gemini-3.1-flash-lite"))

(use-package gptel-prompts
  :vc (gptel-prompts :url "https://github.com/jwiegley/gptel-prompts"
                     :branch "main"
                     :rev :newest)
  :after (gptel)
  :demand t
  :config
  (gptel-prompts-update)
  (gptel-prompts-add-update-watchers))

(use-package agent-shell
  :config
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t))
  (setq agent-shell-google-gemini-command
        '("gemini" "--experimental-acp" "-m" "gemini-2.5-pro")))

(provide 'setup-ai)
;;; setup-ai.el ends here
