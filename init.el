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

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))
  )

(leaf ddskk
  :ensure t
  :custom (
           (skk-large-jisyo . "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
           (skk-japanese-message-and-error . t)
           (skk-status-indicator . 'minor_mode)
           (default-input-method . "japanese-skk")
           )
  )

(leaf mwim
  :ensure t
  :bind(
        ("C-a" . mwim-beginning)
        ("C-e" . mwim-end)
        )
  )


(leaf counsel
  :doc "include ivy swipper"
  :ensure t
  :custom (
           (ivy-wrap . t)
           (ivy-mode . t)
           )
  :bind (
         ("C-s" . swiper)
         ("C-x C-b" . ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         )
  :config
  (leaf ivy-posframe
    :ensure t
    :after ivy posframe
    :custom (
             (ivy-posframe-display-functions-alist . '(
                                                       (counsel-find-file . ivy-posframe-display-at-point)
                                                       (counsel-M-x . ivy-posframe-display-at-point)
                                                       (counsel-recentf . ivy-posframe-display-at-point)
                                                       (counsel-yank-pop . ivy-posframe-display-at-point)

                                                       (ivy-switch-buffer . ivy-posframe-display-at-point)
                                                       (t . ivy-posframe-display-at-frame-bottom-window-center)
                                                       )
                                                   )
             (ivy-posframe-mode . t)
             )
    )
  (leaf ivy-rich
    :ensure t
    :custom (
             (ivy-rich-mode . t)
             )
   )
  )

(leaf company
  :doc ""
  :ensure t
  :custom (
           (global-company-mode . t)
           (company-idle-delay . 0)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around t)
           (company-dabbrev-downcase nil)
           )
  :bind (
         (:company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-h" . nil)
          )
         )
  :config
  (leaf company-posframe
    :ensure t
    :after company posframe
    :custom (
             (require 'subr-x)
             (company-posframe-mode . t)
             (company-posframe-show-indicator . nil)
             (company-posframe-show-metadata . nil)
             (company-posframe-quickhelp-delay nil)
             )
    )
  )


(leaf js2-mode
  :ensure t
  )

(leaf php-mode
  :ensure t
  )

(leaf web-mode
  :ensure t
  )

(leaf cmake-mode
  :ensure t
  :disabled t
  )
