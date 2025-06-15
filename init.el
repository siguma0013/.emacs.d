(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (use-package leaf :ensure t)
  )

(leaf custom-file
  :custom `((custom-file . ,(locate-user-emacs-file ".custom.el"))))

(leaf builtin-ui
  :doc "ビジュアルに関する設定"
  :custom ((line-number-mode . nil)     ; モードラインに行番号
           (column-number-mode . nil)   ; モードラインに列番号

           (custom-enabled-themes . '(tango-dark)) ; テーマ選択
           )
  :config
  (leaf display-line-numbers
    :doc "行番号表示"
    :global-minor-mode global-display-line-numbers-mode
    :custom (display-line-numbers-width . 3) ; 確保する最低幅
    )
  )

(leaf builtin-feature
  :doc "機能に関する設定"
  :custom ((make-backup-files . nil)          ; バックアップファイル
           (auto-save-default . nil)          ; 自動保存
           (auto-save-list-file-prefix . nil) ; 自動保存リスト

           (electric-pair-mode . t)     ; 引用符・括弧補完
           (indent-tabs-mode . nil)
           )
  :config
  (leaf delete-trainig-whitespace
    :doc "行端スペースの削除"
    :hook (before-save-hook . delete-trailing-whitespace)
    )
  (leaf recentf
    :doc "ファイル履歴"
    :global-minor-mode t
    :bind ("C-x C-r" . recentf-open)
    :custom (recentf-max-saved-items . 1000)
    )
  (leaf savehist
    :doc "コマンド履歴"
    :global-minor-mode t
    :custom (savehist-additional-variables . '(kill-ring))
    )
  (leaf show-paren
    :doc "括弧のハイライト"
    :global-minor-mode t
    :custom (show-paren-style . 'mixed)
    )
  )

(leaf global-set-key
  :bind (("C-h" . delete-backward-char)
         ("C-x C-b" . bs-show)
         ))

;; ELPA config

(leaf ddskk
  :doc "インプットメソッド"
  :package t
  :hook ((isearch-mode-hook . skk-isearch-hook-silent)
         (find-file-hook . ddskk-auto-boot))
  :custom ((skk-status-indicator . 'minor-mode)
           (default-input-method . "japanese-skk")
           (skk-large-jisyo . "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
           (skk-japanese-message-and-error . t)
           )
  :config
  (defun skk-isearch-hook-silent ()
    "ddskkのisearch-mode-hook設定推奨メッセージを抑制する為の関数")
  (defun ddskk-auto-boot ()
    "ファイルを開くたびにC-\\しない為の関数"
    (skk-latin-mode t))
  )

(leaf mwim
  :doc "行頭・行端移動の改善"
  :package t
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)
         ))

(leaf *fuzzy-finder
  :config
  (leaf vertico
    :doc "各種コマンドのUI改善"
    :package t
    :global-minor-mode vertico-mode
    )
  (leaf consult
    :package t
    :bind ("C-s" . consult-line)
    )
  (leaf marginalia
    :doc "各種コマンドの注釈追加"
    :package t
    :global-minor-mode marginalia-mode
    )
  (leaf orderless
    :doc "検索等のマッチングの曖昧化"
    :package t
    :config
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides nil)
    ;; (setq completion-styles '(orderless))
    )
  )

(leaf ace-window
  :doc "ウィンドウ移動の改善"
  :package t
  :bind ("C-x o" . ace-window)
  )

(leaf company
  :doc "入力補完"
  :package t
  :disabled t
  :global-minor-mode global-company-mode
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around . t)
           (company-dabbrev-downcase . nil)
           ))

(leaf editorconfig
  :package t
  :global-minor-mode t)

(leaf *git
  :doc "git関連拡張機能"
  :config
  (leaf magit
    :doc "クライアント"
    :package t
    )
  (leaf git-gutter
    :doc "変更箇所のリアルタイム表示"
    :package t
    :global-minor-mode global-git-gutter-mode
    )
  )

(leaf vterm
  :doc "ターミナル"
  :package t
  :bind (("<f2>" . vterm-toggle))
  :custom ((vterm-shell . "/usr/bin/fish")
           (vterm-keymap-exceptions . '("<f2>" "C-c" "C-x" "C-u" "C-g" "C-l" "M-x" "M-o" "C-y" "M-y"))
           )
  :config
  (leaf vterm-toggle
    :doc "ポップアップ表示"
    :package t
    :config
    (add-to-list 'display-buffer-alist
                 '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                   (display-buffer-reuse-window display-buffer-at-bottom)
                   (reusable-frames . visible)
                   (window-height . 0.3)))
    )
  )

(leaf lsp-mode
  :package t
  :config
  (leaf lsp-ui
    :package t)
  (leaf lsp-treemacs
    :package t)

  )

(leaf yasnippet
  :package t
  :config
  (yas-global-mode t))

(leaf web-mode
  :package t
  :mode (("\\.php\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         )
  :custom ((web-mode-block-padding . 0)
           (web-mode-script-padding . 4))
  )

(add-hook 'editorconfig-after-apply-functions
          (lambda (props)
            (setq web-mode-style-padding 0)
            (setq web-mode-script-padding 0)
            ))

(put 'dired-find-alternate-file 'disabled nil)

(leaf treesit
  :config
  (setq treesit-font-lock-level 4)
  )

(setq lsp-bridge-python-command "/usr/bin/python3.11")
(setq lsp-bridge-user-langserver-dir "/data/emacs.d/laugserver/")

  (setq lsp-bridge-lang-server-map
        '((markdown-mode . "marksman")
          (gfm-mode . "marksman")))
(leaf lsp-bridge
  :vc (:url "https://github.com/manateelazycat/lsp-bridge.git")
  :disabled t
  ;; :custom((lsp-bridge-python-command . "/usr/bin/python3.11"))
  :config
  ;; (setq lsp-bridge-python-command "/usr/bin/python3.11")

  (global-lsp-bridge-mode)
  )

;; 開発言語設定

(leaf cmake-mode)

(leaf js-ts-mode
  :mode "\\.js\\'" "\\.mjs\\'"
  )

(leaf typescript-ts-mode
  :mode "\\.ts\\'"
  )
(leaf tsx-ts-mode
  :mode "\\.tsx\\'"
  )


(use-package markdown-mode
  ;; :hook (markdown-mode . lsp)
  :config
  (require 'lsp-marksman)
  (add-hook 'markdown-mode-hook
          (lambda ()
            (setq-local company-backends
                        '((company-files company-capf))))))

;; (leaf markdown-mode
;;   :package t
;;   ;; :mode (("\\.md\\'" . gfm-mode))
;;   :config

;;   (defun my/markdown-resolve-link-at-point ()
;;     "Resolve markdown link at point relative to project root."

;;     (let* ((link (markdown-link-url (markdown-link-at-point)))
;;            (project-root (or (project-root (project-current))
;;                              default-directory))
;;            (full-path (expand-file-name link project-root)))
;;       (when (file-exists-p full-path)
;;         full-path)))

;;   (defun my/markdown-follow-link-with-project-root ()
;;     "Open markdown link, resolving relative to project root if possible."
;;     (interactive)
;;     (let* (
;;            (link (markdown-link-url))
;;            ;; (my-project-current (project-root (project-current)))
;;            (root-directory (or (project-root (project-current)) default-directory))
;;            (full-path
;;             (expand-file-name (if (string< "/" link)
;;                                   (concat "." link)
;;                                 link)
;;                               root-directory))
;;            )

;;       (find-file full-path)
;;       )
;;     )

  ;; (with-eval-after-load 'markdown-mode
  ;;   (define-key markdown-mode-map (kbd "C-c C-o") #'my/markdown-follow-link-with-project-root))
  ;; )

;; (use-package markdown-mode
;;   :hook (markdown-mode . lsp)
;;   :config
;;   (require 'lsp-marksman))

(setq lsp-disabled-clients '((typescript-ts-mode . vue-semantic-server)))

(add-to-list 'lsp-disabled-clients '(js-ts-mode . vue-semantic-server))
