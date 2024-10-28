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
  :custom ((line-number-mode . nil)     ; モードラインに行番号
           (column-number-mode . nil)   ; モードラインに列番号

           (custom-enabled-themes . '(tango-dark)) ; テーマ選択
           ))

(leaf builtin-feature
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
    :custom (show-parent-style . 'mixed)
    )
  )

(leaf global-set-key
  :bind (("C-h" . delete-backward-char)
         ))

;; ELPA config

(leaf ddskk
  :doc "input method"
  :package t
  :hook (isearch-mode-hook . skk-isearch-hook-silent)
  :custom ((skk-status-indicator . 'minor-mode)
           (default-input-method . "japanese-skk")
           (skk-large-jisyo . "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
           (skk-japanese-message-and-error . t)
           )
  :config
  (defun skk-isearch-hook-silent ())
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
    (setq completion-styles '(orderless))
    )
  )

(leaf company
  :doc "入力補完"
  :package t
  :global-minor-mode global-company-mode
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around . t)
           (company-dabbrev-downcase . nil)
           ))

(leaf editorconfig
  :package t
  :global-minor-mode t)

(leaf magit
  :package t)

(leaf vterm
  :package t)
