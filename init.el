;; Emacsからの質問をy/nで回答する
(set 'yes-or-no-p 'y-or-n-p)

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; コメントアウトのスタイル
(setq comment-style 'extra-line)

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'alpha-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; フォント設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'default-frame-alist '(font . "Migu 1M-11"))

;;1234;;1234;;1234;;1234;;1234;;
;;漢字;;漢字;;漢字;;漢字;;漢字;;
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  )

(leaf *builtin-ui
  :custom ((line-number-mode . nil)     ; mode-lineに行番号
           (column-number-mode . nil)   ; mode-lineに列番号

           (custom-enabled-themes . '(tango-dark)) ; テーマ
           (blink-cursor-mode . nil)               ; カーソル点滅
           (global-display-line-numbers-mode . t)  ; 左端に行番号
           ))

(leaf *builtin-feature
  :custom ((auto-save-default . nil)    ; 自動保存
           (electric-pair-mode . t)     ; 引用符・括弧補完
           (indent-tabs-mode . nil)     ; インデントをスペースで表現
           (inhibit-startup-screen . t)
           (make-backup-files . nil)    ; バックアップファイルの作成
           (transient-mark-mode . t)    ; 選択範囲のハイライト
           )
  :config
  (leaf *delete-trailing-whitespace
    :hook ((before-save-hook . delete-trailing-whitespace)
           ))

  (leaf *recentf-mode
    :custom ((recentf-max-saved-items . 2000)
             (recentf-mode . t)
             ))

  (leaf *savehist-mode
    :custom ((history-length . 1000)
             (savehist-additional-variables . '(kill-ring))
             (savehist-mode . t)
             ))

  (leaf *show-paren-mode
    :doc "括弧のハイライト"
    :custom ((show-paren-mode . t)
             (show-paren-style . 'expression)
             ))
  )

(leaf *global-set-key
  :bind (("C-h" . delete-backward-char)
         ("C-x C-b" . bs-show)
         ("C-x C-r" . recentf)
         ))

(setq org-directory "~/Documents/org-mode")
(setq org-default-notes-file "notes.org")
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Documents/org-mode/notes.org" "Notes")
         "* %?\nEntered on %U\n %i\n %a")
        ))

(leaf ddskk
  :doc "input method"
  :ensure t
  :custom ((skk-large-jisyo . "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
           (skk-japanese-message-and-error . t)
           (skk-status-indicator . 'minor_mode)
           (default-input-method . "japanese-skk")
           ))

(leaf mwim
  :ensure t
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)
         ))

(leaf vertico
  :ensure t
  :global-minor-mode vertico-mode
  )

(leaf consult
  :ensure t
  :bind (("C-s" . consult-line))
  )

(leaf orderless
  :ensure t
  )
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))
(leaf marginalia
  :ensure t
  :global-minor-mode marginalia-mode)

(leaf embark
  :disabled t
  :bind (("s-a" . embark-act)))



;; (leaf counsel
;;   :ensure t
;;   :bind (("C-s" . swiper-isearch)
;;          ("C-x C-b" . counsel-switch-buffer)
;;          ("C-x C-f" . counsel-find-file)
;;          ("C-x C-r" . counsel-recentf)
;;          ("M-x" . counsel-M-x)
;;          ("M-y" . counsel-yank-pop)
;;          )
;;   :configq
;;   (leaf ivy-rich
;;     :ensure t
;;     :global-minor-mode t
;;     )
;;   )

(leaf company
  :doc "入力補完UI"
  :ensure t
  :global-minor-mode global-company-mode
  :custom (
           (company-idle-delay . 0)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around . t)
           (company-dabbrev-downcase . nil)
           )
  :bind ((:company-active-map
          ("C-h" . nil)
          ))
  :config
  (leaf company-posframe
    :global-minor-mode t
    :ensure t
    )
  (leaf company-quickhelp

    :ensure t
  )
  )

(leaf lsp-mode
  :ensure t
  :hook ((rust-mode-hook . lsp-deferred)
         )
  :custom (lsp-completion-provider . :none)
  :config
  (leaf lsp-ui
    :ensure t
    )
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
                 '(qml-mode . "qmlls6"))

    (message "%s" lsp-language-id-configuration)

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("qmlls6" "--build-dir" "build/debug"))
                      :activation-fn (lsp-activate-on "qmlls6")
                      :server-id 'qmlls))
    )
(setq lsp-clients-clangd-args
    '("--header-insertion=never" "--compile-commands-dir=build/debug"))
  )
(setf lsp-prefer-capf t)
(leaf rust-mode
  :ensure t)

(leaf web-mode
  :ensure t)

(leaf tree-sitter
  :ensure t
  :config
  (leaf tree-sitter-langs
    :ensure t))

(leaf editorconfig
  :ensure t
  :config
  (editorconfig-mode t))

(require 'ansi-color)
(add-hook 'compilation-filter-hook
          '(lambda ()
             (ansi-color-apply-on-region (point-min) (point-max))))

(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)

(yas-global-mode t)
(put 'dired-find-alternate-file 'disabled nil)
