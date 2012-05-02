;; おまじない？
(require 'cl nil t)

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
;;(define-key global-map (kbd "C-t") 'other-window)

;; Mac OS Xの場合のファイル名のエンコード設定
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
                    :height 135)
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
(add-to-list 'backup-directory-alist
      (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; ファイルが#!から始まる場合、+xを付けて保存する
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; emacs-lisp-modeのフックをセット
;; (add-hook 'emacs-lisp-mode-hook
;;          '(lambda ()
;;             (when (require 'eldoc nil t)
;;               (setq eldoc-idle-dela 0.2)
;;               (setq eldoc-echo-area-use-multiline-p t)
;;               (turn-on-eldoc-mode))))
;; emacs-lisp-mode-hook用の関数を定義
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
;; emacs-lisp-modeのフックをセット
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;; auto-installの設定
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する 初期値は~/.emacs.d/auto-install
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))

;; auto-installしたElisp
;; redo+.el                   : redoを追加
;; package.el                 : パッケージ管理を追加

;; redo+の設定
(when (require 'redo+ nil t)
  ;; C-.にリドゥを割り当てる
  (global-set-key (kbd "C-.") 'redo)
  )
;; package.elの設定
(when (require 'package nil t)
  ;; パッケージリポジトリにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  ;; インストールしたパッケージにロードパスを通して読み込む
  (package-initialize))

;;; anything
;; (auto-instlal-batch "anything")
(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.3
   ;; タイプして再描写するまえの時間。デフォルトは0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数。デフォルトは50
   anything-candidate-number-limit 100
   ;; 候補が多い時に体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enalbe-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    ;; root権限でアクションを実行するときのコマンド
    ;; デフォルトは"su"
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    ;; lispシンボルの補完候補の再検索時間
    (require 'anything-migemo nil t))

  (require 'anything-show-complattion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ;; describe-bindingsをanythingに置き換える
    (descbinds-anything-install)))

;; M-yにanything-show-kill-ringを割り当てる
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

;; auto-complateの設定
(when (require 'auto-complate-config nil t)
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;; color-moccurの設定
(when (require 'color-moccur nil t)
  ;; M-oにoccur-by-moccurを割当
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
  ;; スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索のとき除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  ;; Migemoを利用できる環境であればMigemoを使う
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (setq moccur-use-migemo t)))

;; moccur-editの設定
(require 'moccur-edit nil t)

;; wgrepの設定
(require 'wgrep nil t)

;; undohistの設定
(when (require 'undohist nil t)
  (undohist-initialize))

;; undo-treeの設定
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; point-undoの設定
(when (require 'point-undo nil t)
  (define-key global-map (kbd "M-[")'point-undo)
  (define-key global-map (kbd "M-]") 'point-redo)
)

;; ElScreenのプレフィックスキーを変更する（初期値はC-z）
(setq elscreen-prefix-key (kbd "C-t"))
(when (require 'elscreen nil t)
  ;; C-z C-zをタイプした場合にデフォルトのC-zを利用する
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))

;; howmメモ保存の場所
(setq howm-directory (concat user-emacs-directory "howm"))
;; howm-menuの言語を日本語に
(setq howm-menu-lang 'ja)
;; howmメモを1日1ファイルにする場合
;; (setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")
;; howm-modeを読み込む
(when (require 'howm-mode nil t)
  ;; C-c,,でhowm-menuを起動
  (define-key global-map (kbd "C-c ,,") 'howm-menu))
;; howmメモを保存と同時に閉じる
(defun howm-save-buffer-and-kill()
      "howmメモを保存と同時に閉じます。"
      (interactive)
      (when (and (buffer-file-name)
                 (string-match "\\.howm" (buffer-file-name)))
        (save-buffer)
        (kill-buffer nil)))

;; C-c C-cでメモの保存と同時にバッファを閉じる
(define-key howm-mode-map (kbd "C-c C-c") 'howm-save-buffer-and-kill)

;; cua-modeの設定
(cua-mode t) ; cua-modeをオン
(setq cua-emable-cua-key nil) ; CUAキーバインドを無効にする

;; GitフロントエンドEggの設定
(when (executable-find "git")
  (require 'egg nil t))
