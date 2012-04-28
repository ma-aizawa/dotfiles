;; ~/.emacs.d/elisp ディレクトリをロードパスに追加する
;; ただし、add-to-load-path関数を作成した場合は不要
;; (add-to-list 'load-path' "~/.emacs.d/elisp") 

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))
;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;; C-mにnewline-and-indentを割り当てる。
(global-set-key (kbd "C-m") 'newline-and-indent)

;; "C-t" でウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; Mac OS Xの場合のファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;; カラム番号も表示
(column-number-mode t)
;; ファイルサイズを表示
(size-indication-mode t)
;; 時計を表示
;; (setq display-time-day-and-date t) ;曜日・月・日を表示
(setq display-time-24hr-format t) ;24時間表示
(display-time-mode t)
;; バッテリー残量を表示
(display-battery-mode t)
;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; 行番号を常に表示する
(global-linum-mode t)
(setq linum-format "%4d")

;; TABの表示幅。初期値は8
(setq-default tab-width 4)

;; インデントにタブ文字を使わない
(setq-default indent-tabs-mode nil)

(when (require 'color-theme nil t)
  ;; テーマを読み込むための設定
  (color-theme-initialize)
  ;; テーマをrobin-hoodとりあえず。
  (color-theme-robin-hood))

  ;; solarizedの読み込み
;;  (when (require 'color-theme-solarized)
  ;; solarizedは漢字をちゃんと読める設定に出来たら
;;    (color-theme-solarized-dark))
  
;; asciiフォントをRictyに
(set-face-attribute 'default nil
                    :family "Ricty"
                    :height 160)
;; 日本語フォントもRicty
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty"))
(setq face-font-rescale-alist
      '((".*Ricty.*" . 1.0)))
;;画面サイズの変更
(setq default-frame-alist
      (append
        (list
          '(width   . 80)
          '(height  . 51)
          '(alpha   . 80)
          default-frame-alist)))

(defface my-hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;; 背景がlightならば背景色を緑に
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode 0)

;; paren-mode :対応する括弧を強調して表示する
(setq show-paren-delay 0) ;表示までの秒数 初期値は0.125
(show-paren-mode t)       ;有効化
;; parenのスタイル: expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; バックアップファイルの作成場所をまとめる

