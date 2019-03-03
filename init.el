;; スタートップメッセージを非表示
(setq inhibit-startup-screen t)
;; Emacsからの質問をy/nで回答する
(set 'yes-or-no-p 'y-or-n-p)

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; show-paren-mode：対応する括弧を強調して表示する
(show-paren-mode t)
(setq show-paren-style 'expression)	;括弧内も強調:expression

;; ミニバッファの履歴を保存する
(savehist-mode t)
(setq history-length 10000)		;履歴数

;; バックアップファイルを作らない
(setq make-backup-files nil)
;; オートセーブファイルを作らない
(setq auto-save-default nil)

;; 選択範囲に色をつける
(transient-mark-mode t)

;; コメントアウトのスタイル
(setq comment-style 'extra-line)

;; カーソルの点滅をやめる
(blink-cursor-mode 0)

;; タブをスペースで表現
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; フレーム関連
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 起動時のフレーム設定
(load-theme 'tango-dark)

;; モードラインに何文字目かも表示
(column-number-mode t)
;; フリンジ(左)に行数を表示する
(global-display-line-numbers-mode t)

;; ツールバー非表示
(tool-bar-mode 0)
;; メニューバー非表示
(menu-bar-mode 0)
;; スクロールバー非表示
(scroll-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; フォント設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'default-frame-alist '(font . "Migu 1M-11"))

;;1234;;1234;;1234;;1234;;1234;;
;;漢字;;漢字;;漢字;;漢字;;漢字;;

;; 無駄なスペースの削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)

(put 'dired-find-alternate-file 'disabled nil)

;; "C-h"をバックスペースにする
(keyboard-translate ?\C-h ?\C-?)

(global-set-key (kbd "C-x C-b") 'bs-show)

(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (recentf-mode 1)
  )

(setq-default header-line-format 'buffer-file-name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SKK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'skk nil t)
  (defvar jisyo (concat (getenv "HOME") "/.emacs.d/SKK-JISYO.L.cdb"))
  (setq skk-cdb-large-jisyo jisyo)

  (setq skk-inhibit-ja-dic-search t)

  ;; 各種メッセージを日本語で通知する
  (setq skk-japanese-message-and-error t)
  (setq skk-status-indicator 'minor_mode)
  (setq default-input-method "japanese-skk")
  )

(when (require 'counsel nil t )
  (ivy-mode 1)
  (counsel-mode 1)
  (setq dumb-jump-selector 'ivy)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  )

(when (require 'company nil t)
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-warp-around t)
  (setq company-tooltip-minimum-width 9999)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-h") nil)
  )

;; (when (require 'lsp-mode nil t)
;;   (with-eval-after-load 'lsp-mode
;;     (require 'lsp-clangd)
;;     (setq lsp-clangd-executable "/usr/bin/clangd")
;;     (add-hook 'c-mode-hook #'lsp-clangd-c-enable)
;;     (add-hook 'c++-mode-hook #'lsp-clangd-c++-enable)
;;     (add-hook 'objc-mode-hook #'lsp-clangd-objc-enable)
;;     )
;;   )

(when (require 'company-lsp nil t)
  (push 'company-lsp company-backends)
  )

(defun test ()
  ""
  (interactive)
  (setq dir buffer-file-name)
  (setq result_a (replace-regexp-in-string "[^/]+/[^/]+$" "" dir))
  (message "%s" result_a)
  )

(defun get-Qt_Project-root ()
  ""

  )

(when (require 'irony nil t)
  (defun company-irony-hook ()
    "my irony hook"
    (when (member major-mode irony-supported-major-modes)
      (irony-mode t)
      )
    (setq project-root-path (replace-regexp-in-string "/[^/]+/[^/]+$" "" buffer-file-name))

    (irony-cdb-json-add-compile-commands-path project-root-path
                        (concat project-root-path "/build/compile_commands.json"))
    (irony-cdb-autosetup-compile-options)
    )

  ;; (add-hook 'c-mode-hook #'company-irony-hook)
  (add-hook 'c++-mode-hook #'company-irony-hook)
  ;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
  )

(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
  (setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
      )
  )

(when (require 'js2-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
    (php-mode ccls lsp-mode lsp-ui lsp-clangd company-lsp flycheck cmake-mode web-mode path-headerline-mode js2-mode ddskk counsel company-irony))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
