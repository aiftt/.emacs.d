;;; -*- lexical-binding: t; -*-

(require 'mind-wave)

;; (use-package auth-source-pass
;;   :ensure t
;;   :init
;;   (auth-source-pass-enable)
;;   )

(use-package llm
  :init (slot/vc-install :repo "ahyatt/llm")
  )

;; YOU DON'T NEED NONE OF THIS CODE FOR SIMPLE INSTALL
;; IT IS AN EXAMPLE OF CUSTOMIZATION.
(use-package ellama
  :init
  (slot/vc-install :repo "s-kostyaev/ellama")
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "Chinese")
  (setopt ellama-auto-scroll t)
  (setopt ellama-sessions-directory "~/github/mine/ellama-sessions/")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  ;; (setopt ellama-provider
	;; 	  (make-llm-ollama
	;; 	   :chat-model "codellama" :embedding-model "codellama"))
  ;; (setopt ellama-provider
	;; 	    (make-llm-ollama
	;; 	     ;; this model should be pulled to use it
	;; 	     ;; value should be the same as you print in terminal during pull
	;; 	     :chat-model "llm-ollama"
	;; 	     :embedding-model "llm-ollama"))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  ;; (setopt ellama-providers
	;; 	    '(("zephyr" . (make-llm-ollama
	;; 			   :chat-model "zephyr:7b-beta-q6_K"
	;; 			   :embedding-model "zephyr:7b-beta-q6_K"))
	;; 	      ("mistral" . (make-llm-ollama
	;; 			    :chat-model "mistral:7b-instruct-v0.2-q6_K"
	;; 			    :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
	;; 	      ("mixtral" . (make-llm-ollama
	;; 			    :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
	;; 			    :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
  ;; Naming new sessions with llm
  ;; (setopt ellama-naming-provider
	;;     (make-llm-ollama
	;;      :chat-model "mistral:7b-instruct-v0.2-q6_K"
	;;      :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
  ;; (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; ;; Translation llm provider
  ;; (setopt ellama-translation-provider (make-llm-ollama
	;; 				 :chat-model "sskostyaev/openchat:8k"
	;; 				 :embedding-model "nomic-embed-text"))

  )

(use-package gptel
  :init
  (slot/vc-install :repo "karthink/gptel")
  :config
  (gptel-make-ollama "Ollama"             ;Any name of your choosing
    :host "localhost:11434"               ;Where it's running
    :stream t                             ;Stream responses
    :models '("llama2:13b" "qwen:32b"))          ;List of models
  ;; OPTIONAL configuration
  (setq
   gptel-default-mode 'org-mode
   ;; gptel-model "llama2:13b"
   ;; gptel-backend (gptel-make-ollama "Ollama"
   ;;                 :host "localhost:11434"
   ;;                 :stream t
   ;;                 :models '("llama2:13b"))
   ))


(use-package org-ai
  :init
  (slot/vc-install :repo "rksm/org-ai")
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)
  (setq org-ai-image-model "dall-e-3")
  (setq org-ai-image-default-size "1792x1024")
  (setq org-ai-image-default-count 2)
  (setq org-ai-image-default-style 'vivid)
  (setq org-ai-image-default-quality 'hd)
  (setq org-ai-image-directory (expand-file-name "org-ai-images/" org-directory))
  ) ; if you are using yasnippet and want `ai` snippets



(provide 'init-chat)
