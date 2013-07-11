;;;; init.el --- Emacs initialization code

;;; This is where ~/.emacs points to I would hope. It's a monolithic
;;; Emacs configuration, except for when a segment (^L) is too big for
;;; it not to be kept in its own file.

(when (featurep 'xemacs)
  (error "You are using XEmacs, poor you!"))

;;; Package Management

;; setting up melpa stuff
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar melpa-packages
  '(ace-jump-mode
    auctex
    auto-complete
    autopair
    browse-kill-ring
    clojure-mode
    csv-mode
    diminish
    gist
    htmlize
    js2-mode
    js-comint
    magit
    markdown-mode
    nrepl
    org
    paredit
    rainbow-delimiters
    window-number
    yaml-mode
    yasnippet
    zencoding-mode))

(dolist (pkg-name melpa-packages)
  (when (not (package-installed-p pkg-name))
    (let ((pkg-desc (cdr (assq pkg-name package-archive-contents))))
      (when pkg-desc
        (package-install pkg-name)))))

;; setting up el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-sources
      '(el-get
        (:name winring
               :type http
               :url "https://launchpad.net/winring/trunk/4.0.1/+download/winring.el"
               :features winring
               :compile "winring.el")))

(defvar el-get-packages
  '(el-get winring))

(el-get 'sync el-get-packages)

;;; General

(require 'cl)                           ;drinking the kool aid
(require 'misc)                         ;`zap-up-to-char'

(desktop-save-mode 1)                   ;we'll have to think about this
(setq inhibit-splash-screen t)          ;we don't want that anymore

;; set up a friendly greeting
(setq initial-scratch-message ";; Welcome, Master -- happy hacking
")

;; some info about my user
(setq user-full-name "Andreas Scholta")

;; some `woman' customization
(setq woman-use-own-frame nil)
(setq woman-use-topic-at-point t)

(setq x-select-enable-clipboard t)

(setq shell-file-name "/bin/sh")

;;; Look and Feel

;; `ace-jump-mode'

(global-set-key (kbd "C-'") 'ace-jump-mode)

;; no menu-bar, tool-bar, scroll-bar or blinking cursor
(set-scroll-bar-mode nil)

(setq visible-bell t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(column-number-mode 1)                  ;show column number in mode-line
(setq use-dialog-box nil)               ;dialog boxes are flow killers
(setq-default truncate-lines t)         ;never truncate lines
(fset 'yes-or-no-p 'y-or-n-p)           ;saves me some strength

;; fringe stuff!
(set-fringe-mode nil)                   ;8px default fringe both sides

(setq-default indicate-buffer-boundaries t) ;show buffer boundaries
(setq-default indicate-empty-lines nil)     ;show empty lines

(minibuffer-depth-indicate-mode 1)      ;indicate minibuffer depth

;; remember cursor positions in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/places")

;; remember minibuffer history
(setq savehist-file "~/.emacs.d/hist-file")
(savehist-mode 1)

;; clean stale buffers at midnight
(require 'midnight)

(setq clean-buffer-list-delay-general 5)

;;; Buffers

;; make buffernames unique, nicer than default
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; `ido' is buffer and file selection by substring matching

(ido-mode 1)

(setq ido-max-directory-size 3000000)   ;we can handle it
(setq ido-enable-flex-matching t)       ;try fuzzy matching after substring

;; regexes of files to be ignored by ido
(setq ido-ignore-files
      '("~$"                            ;ignore backups
        "\\`\\."                        ;ignore hidden
        "\\`CVS/"
        "\\`#"
        "\\`.#"
        "\\`\\.\\./"
        "\\`\\./"))

;; ignore directories just as files
(setq ido-ignore-directories ido-ignore-files)

(setq ido-create-new-buffer 'always)    ;don't prompt to create new buffer

;; regexes of buffers to be ignored by ido
(setq ido-ignore-buffers '("\\` "))

;; maybe show already visible file/buffer in current window
(setq ido-default-buffer-method 'selected-window)
(setq ido-default-file-method 'selected-window)

(require 'ffap)

(defadvice ido-find-file (around maybe-try-ffap-first (&optional arg) activate)
  "With prefix argument, try `ffap' first."
  (interactive "P")
  (if (and arg (ffap-file-at-point))
      (ffap)
    ad-do-it))

;;; from stackoverflow
(defvar ido-enable-replace-completing-read nil)

(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ;manual override disable ido
          (boundp 'ido-cur-list))     ;avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                                     allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))

;;; Ibuffer

;;; `ibuffer' is also kinda nice for buffer selection and operations
;;; on multiple buffers

(defun ibuffer-ido-find-file ()
  "find file in \"current\" directory in `ibuffer.'"
  (interactive)
  (let ((file-name (buffer-file-name (ibuffer-current-buffer))))
    (ido-find-file-in-dir (if file-name
                              (file-name-directory file-name)
                            "~"))))

;; shadow inferior stuff with superior stuff
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; `ibuffer' is autoloaded
(eval-after-load "ibuffer"
  '(progn
     (define-key ibuffer-mode-map (kbd "C-x C-f") 'ibuffer-ido-find-file)))

;;; windows

;; `window-number-mode' is quite useful
(require 'window-number)
(window-number-meta-mode 1)

(define-key window-number-mode-map (kbd "C-x C-j") nil) ;unwanted crap

;; `winner-mode' has nice undo for windows ;)
(require 'winner)
(winner-mode 1)

;; `winring' has per-frame named window configuration rings
(require 'winring)
(winring-initialize)

(setq winring-show-names t)             ;show window conf name in modeline

(defadvice winring-complete-name (around use-ido activate)
  (let ((ido-enable-replace-completing-read t))
    ad-do-it))

;;; `dired-mode'

(require 'dired)
(require 'dired-x)

(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)

(setq dired-omit-files "^\\.\\|^#\\|\\.$")
(add-hook 'dired-mode-hook 'dired-omit-mode)

;;; editing

(setq require-final-newline nil)        ;no auto final newline
(setq mode-require-final-newline nil)   ;nor in certain modes

;; let's not make backups (that's what we use dvc for)
(setq make-backup-files nil)

;; turn off auto-saving based on input events
(setq auto-save-interval 0)
(setq auto-save-timeout 180)            ;auto-save after 180s idle

(setq-default indent-tabs-mode nil)     ;we don't like tabs for indent

;; show matching parens, always
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
(show-paren-mode 1)

(transient-mark-mode 0)                 ;no transient mark mode please

(browse-kill-ring-default-keybindings)  ;bind m-y to it

;; more useful zapper more conveniently
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

(defun duplicate-line-or-region (arg)
  "Duplicates the current region (transient) ARG times, but at
least twice. If none is active, duplicate the current line
instead."
  (interactive "p")
  (let ((times (if (> arg 1) arg 2)))
    (save-excursion
      (if (use-region-p)
          (kill-region (mark) (point))
        (kill-region (point-at-bol) (point-at-bol 2)))
      (dotimes (i times)
        (yank)))))

(global-set-key (kbd "C-S-d") 'duplicate-line-or-region)

;;; Gists

(setq gist-view-gist t)

(load "~/.emacs.d/github-api" t)

;;; Eshell

(setq eshell-cmpl-cycle-completions nil)
(setq eshell-save-history-on-exit t)
(setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\|_darcs\\)/\\'")

(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)

     (setenv "PAGER" "cat")

     (face-spec-set 'eshell-prompt '((((background dark))       (:foreground "turquoise1"))
                                     (((background light))      (:foreground "black"))))

     (add-hook 'eshell-mode-hook        ;for some reason this needs to be a hook
               '(lambda ()
                  (define-key eshell-mode-map "\C-a" 'eshell-bol)))

     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-visual-commands "sl")

     (add-to-list 'eshell-command-completions-alist
                  '("gunzip" . "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" . "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("evince" . "\\.pdf\\'"))

     (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)))

;;; Whitespace cleanup

(setq whitespace-chars
      '(empty                           ;fix empty lines with ws
        trailing                        ;fix trailing ws
        tabs                            ;display tabs in `whitespace-mode'
        lines))                         ;display lines longer than ..

(setq-default fill-column 80)
(setq whitespace-line-column 80)        ;we don't like using more than 80

;;; `autopair-mode'

(autoload 'autopair-mode "autopair" "autopair mode" t)

(setq autopair-autowrap nil)          ;really bad experiences with this set to T

;;; Magit

(global-set-key (kbd "C-c m") 'magit-status)

;;; Latex

(setq-default TeX-engine 'xetex)

;;; HTML

(require 'zencoding-mode)

(setq zencoding-indentation 2)

(add-hook 'sgml-mode-hook 'zencoding-mode)

;;; JavaScript / NodeJS

(require 'js-comint)

(setq inferior-js-program-command "node -e require('repl').start({ignoreUndefined:\ true})")

(defun node-repl-comint-preoutput-filter (output)
  (when (equal (buffer-name) "*js*")
    (replace-regexp-in-string
     "\\\[0K" ""
     (replace-regexp-in-string
      "\\\[.G" ""
      (replace-regexp-in-string
       "\\\[0J" ""
       (replace-regexp-in-string
        "\\[2C" ""
        (replace-regexp-in-string
         "" "" output)))))))

(defun js-add-comint-bindings ()
  (local-set-key (kbd "C-c C-z") 'run-js)
  (local-set-key (kbd "C-c C-b") 'js-send-buffer)
  (local-set-key (kbd "C-c C-r") 'js-send-region)
  (local-set-key (kbd "C-c C-e") 'js-send-last-sexp))

(add-hook 'js2-mode-hook 'js-add-comint-bindings)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun node-hook ()
  (ansi-color-for-comint-mode-on)
  (add-to-list 'comint-preoutput-filter-functions 'node-repl-comint-preoutput-filter))

(setq inferior-js-mode-hook 'node-hook)

;;; `lisp-mode'

(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'enable-lowlight-parens-mode)

;;; `scheme-mode'

(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'enable-lowlight-parens-mode)

;;; `clojure-mode'

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'enable-lowlight-parens-mode)

;;; `emacs-lisp-mode'

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)   ;symbol documentation
(add-hook 'emacs-lisp-mode-hook 'paredit-mode) ;see above
(add-hook 'emacs-lisp-mode-hook 'enable-lowlight-parens-mode)

(add-hook 'lisp-interaction-mode 'enable-lowlight-parens-mode)

;;; `ruby-mode'

(setq ruby-indent-level 3)              ;aibo prefers this

;;; `python-mode'

(add-to-list 'auto-mode-alist '("\\.boo$" . python-mode))

(setq-default python-indent-offset 3)   ;aibo prefers this

(add-hook 'python-mode-hook 'autopair-mode)

;;; `c-mode'

(setq-default c-basic-offset 3)         ;aibo prefers this

;;; `prolog-mode'

(setq prolog-program-name "swipl")

;;; diminished minor modes

(eval-after-load "paredit" '(diminish 'paredit-mode " pe"))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))

;;; `markdown-mode'

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;;; `org-mode'

(setq org-enforce-todo-dependencies t)
(setq org-enforce-checkbox-dependencies t)

(require 'org)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-completion-use-ido t)

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps t)

(setq org-startup-indented t)

(setq org-hide-leading-stars nil)
(setq org-odd-levels-only nil)

(setq org-default-notes-file nil)

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-include-diary t)
(setq org-agenda-start-on-weekday nil)

;;; `yasnippet'

(require 'yasnippet)

;;; Changing default font size and font

(defun set-font-size (pt-size)
  (interactive (list (read-number "Point size: ")))
  (set-face-attribute 'default nil :height (* pt-size 10)))

(set-font-size 11)               ;we can change buffer font size via C-x C-{=,-}

;;; `rcirc'

(require 'rcirc)

(rcirc-track-minor-mode 1)

(setq rcirc-log-flag t)
(setq rcirc-server-alist
      '(("irc.freenode.net"
         :channels ("##cinema" "#emacs")
         :user-name "brx"
         :full-name "brx")))

(setq rcirc-authenticate-before-join t)

(defun rcirc-omit-mode-on ()
  (unless rcirc-omit-mode (rcirc-omit-mode)))

(defun rcirc-toggle-low-priority-on ()
  (unless rcirc-low-priority-flag (rcirc-toggle-low-priority)))

(setq rcirc-buffer-maximum-lines 200)   ;maximum lines in rcirc buffer

(defun-rcirc-command ghost (nick)
  "Ghost ..."
  (interactive "i")
  (let* ((nick (if (null nick)
                   (read-string "Ghost nick: " (rcirc-nick process))
                 nick))
         (server-name (rcirc-server-name process))
         (auth-info (find server-name rcirc-authinfo
                          :test (lambda (a b)
                                  (and (string-equal nick (caddr b))
                                       (string-match-p (car b) a))))))
    (when auth-info
      (let ((ghost-message
             (concat "ghost " (caddr auth-info) " " (cadddr auth-info))))
        (rcirc-print process (rcirc-buffer-nick) "PRIVMSG" "NICKSERV" ghost-message)
        (rcirc-send-privmsg process "NICKSERV" ghost-message)))))

(defun-rcirc-command clear (n)
  "Clear channel buffer scrollback text, leaving N lines before
prompt."
  (interactive "nHow many lines to leave: ")
  (let ((n (truncate (typecase n
                       (string (string-to-number n))
                       (number n)
                       (t 0)))))
    (save-excursion
      (let ((inhibit-read-only t))
        (kill-region (point-min)
                     (progn
                       (goto-char rcirc-prompt-start-marker)
                       (unless (<= n 0)
                         (move-beginning-of-line (- (- n 1))))
                       (point)))))))

;;; better channel auto joining

(defvar rcirc-channel-join-hooks
  '(("irc.freenode.net"
     ("##cinema" flyspell-mode)
     ("#emacs" rcirc-omit-mode-on flyspell-mode)
     ("#android-dev" rcirc-omit-mode-on rcirc-toggle-low-priority-on))))

(defadvice rcirc-cmd-join (after set-channel-props activate)
  (let ((channel (car (split-string channel)))
        (process (or process (rcirc-buffer-process))))
    (with-current-buffer (rcirc-get-buffer process channel)
      (let ((hooks (cdr (assoc channel
                               (cdr (assoc (process-name process)
                                           rcirc-channel-join-hooks))))))
        (mapc 'funcall hooks)))))

;;; some general stuff..

(defmacro define-toggler (name var)
  `(defun ,name ()
     (interactive)
     (setq ,var (not ,var))
     (if ,var
         (message "Switched %s on!" (quote ,var))
       (message "Switched %s off!" (quote ,var)))))

;;; rcirc doctor stuff

(require 'doctor)

(defvar rcirc-doctor-responses-enabled nil)

(define-toggler rcirc-toggle-doctor-responses
  rcirc-doctor-responses-enabled)

(defvar rcirc-doctor-response-nicks
  '("brxm"))

(defun rcirc-doctor-buffer-name (nick)
  (concat "*rcirc-doctor-" nick "*"))

(defun rcirc-doctor-init (nick)
  (save-excursion
    (let ((buffer-name (rcirc-doctor-buffer-name nick)))
      (unless (buffer-live-p (get-buffer buffer-name))
        (set-buffer (get-buffer-create buffer-name))
        (make-doctor-variables)))))

(defun rcirc-doctor-response (nick msg)
  (save-excursion
    (set-buffer (rcirc-doctor-buffer-name nick))
    (delete-region (point-min) (point-max))
    (dolist (sentence
             (split-string msg
                           "\\([.?!][]\"'”)}]*\\|[。．？！]+\\)[
]*"
                           t))
      (setq doctor--lincount (1+ doctor--lincount))
      (doctor-doc (with-temp-buffer
                    (insert sentence)
                    (text-mode)         ;`doctor-readin' needs this
                    (doctor-readin)))
      (setq doctor--bak sentence))
    (let ((resp (buffer-string)))
      (when (not (zerop (length resp)))
        (concat nick ": "
                (substitute ?\ ?\n (substring resp 0 (1- (length resp)))))))))

(defun rcirc-doctor-maybe-respond (process cmd sender args line)
  (when (and rcirc-doctor-responses-enabled
             (string= cmd "PRIVMSG"))
    (let ((nick (first (split-string sender ":\\|!")))
          (channel (first args))
          (msg (second args)))
      (when (and (member nick rcirc-doctor-response-nicks)
                 (string-match (concat "^" (rcirc-nick process)) msg))
        (rcirc-doctor-init nick)
        (let ((resp (rcirc-doctor-response nick (substring msg (match-end 0)))))
          (when resp
            (rcirc-print process (rcirc-buffer-nick) "PRIVMSG" channel resp)
            (rcirc-send-privmsg process channel resp)))))))

(add-hook 'rcirc-receive-message-hooks 'rcirc-doctor-maybe-respond)

;;; set rcirc process buffer's ignore activity flag after connect

(defadvice rcirc-connect (after ignore-server-activity activate)
  (with-rcirc-process-buffer ad-return-value
    (rcirc-toggle-ignore-buffer-activity)))

;;; rcirc auto reconnect

(defvar rcirc-auto-reconnect t)

(define-toggler rcirc-toggle-auto-reconnect rcirc-auto-reconnect)

(defvar rcirc-auto-reconnect-interval 15)

(defun rcirc-schedule-connect (process)
  (run-with-timer rcirc-auto-reconnect-interval
                  nil
                  'rcirc-reconnect-if-closed
                  process))

(defun rcirc-reconnect-if-closed (process)
  (let* ((server (process-name process))
         (server-plist (cdr
                        (assoc-string server
                                      rcirc-server-alist)))
         (port (number-to-string
                (or (plist-get server-plist :port)
                    rcirc-default-port)))
         (nick (or (plist-get server-plist :nick)
                   rcirc-default-nick))
         (user-name (or (plist-get server-plist :user-name)
                        rcirc-default-user-name))
         (full-name (or (plist-get server-plist :full-name)
                        rcirc-default-full-name))
         (password (plist-get server-plist :password))
         (channels (split-string
                    (mapconcat 'identity
                               (plist-get server-plist
                                          :channels)
                               " ")
                    "[, ]+" t)))
    (let ((p-status (process-status process)))
      (when (or (eq 'failed p-status)
                (eq 'closed p-status))
        (save-window-excursion
          (condition-case err
              (rcirc-connect server port nick user-name full-name
                             channels password)
            (error (when rcirc-auto-reconnect
                     (rcirc-schedule-connect process)))))))))

(defun rcirc-auto-reconnect (process sentinel)
  (let ((p-status (process-status process)))
    (when (and rcirc-auto-reconnect
               (or (eq 'failed p-status)
                   (eq 'closed p-status)))
      (rcirc-schedule-connect process))))

(defvar rcirc-sentinel-hooks nil)	;forward declaration
(add-to-list 'rcirc-sentinel-hooks 'rcirc-auto-reconnect)

(defun ido-switch-rcirc-buffer ()
  "Switch to a rcirc channel buffer using `ido-switch-buffer'"
  (interactive)
  (cl-flet ((not-rcirc-p (b-name)
                         (with-current-buffer b-name
                           (not (eql major-mode 'rcirc-mode)))))
    (let ((ido-ignore-buffers (cons 'not-rcirc-p ido-ignore-buffers)))
      (ido-switch-buffer))))

(defun ido-switch-non-rcirc-buffer ()
  "Switch to another non-rcirc buffer using `ido-switch-buffer'"
  (interactive)
  (cl-flet ((rcirc-p (b-name)
                     (with-current-buffer b-name
                       (eql major-mode 'rcirc-mode))))
    (let ((ido-ignore-buffers (cons 'rcirc-p ido-ignore-buffers)))
      (ido-switch-buffer))))

(global-set-key (kbd "C-c b") 'ido-switch-rcirc-buffer)
(global-set-key (kbd "C-x b") 'ido-switch-non-rcirc-buffer)

(load "~/.emacs.d/rcirc-auth-info" t)

;;; Lowlighting parentheses

(defface lowlighted '((((background dark))      (:foreground "#505050"))
                      (((background light))     (:foreground "#c0c0c0")))
  "Face used for lowlighting text.")

(defvar lowlight-parens-keywords '(("(\\|)" 0 'lowlighted append))
  "Keywords used for call to `font-lock-add-keywords'")

(define-minor-mode lowlight-parens-mode "Minor mode to lowlight parentheses."
  :lighter " llp"
  (if lowlight-parens-mode
      (font-lock-add-keywords nil lowlight-parens-keywords)
    (font-lock-remove-keywords nil lowlight-parens-keywords))
  (font-lock-mode 1))

(defun enable-lowlight-parens-mode () (lowlight-parens-mode 1))

;;; Start editing file as root using tramp and sudo

(defun find-file-as-root ()
  "Find current buffer's file as root using tramp and sudo"
  (interactive)
  (when buffer-file-name
    (find-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; Day- and Night-Time Emacs

(defvar daytime-interval (cons 7 17)
  "Interval of hours considered to make up 'daytime'")

(defun n-in-interval (n iv)
  (and (<= (car iv) n)
       (< n (cdr iv))))

(defvar daytime-hook nil "Hook to be run at start of day.")
(defvar nighttime-hook nil "Hook to be run at start of night.")

(defvar next-day/night-update-timer nil
  "Timer for next `run-day/night-hook-and-reregister' invokation")

(defun run-day/night-hook-and-reregister ()
  (let* ((current-hour (elt (decode-time) 2))
         (day-p (n-in-interval current-hour daytime-interval)))
    (if day-p
        (run-hooks 'daytime-hook)
      (run-hooks 'nighttime-hook))
    (setq next-day/night-update-timer
          (run-at-time (format "%.2d:00"
                               (let ((next-hour (if day-p
                                                    (cdr daytime-interval)
                                                  (car daytime-interval))))
                                 (if (<= next-hour current-hour)
                                     (+ next-hour 24)
                                   next-hour)))
                       nil
                       'run-day/night-hook-and-reregister))))

(defun toggle-day/night-hooks (&optional n)
  "Toggles the timed running of `run-day/night-hook-and-reregister'"
  (interactive "P")
  (cond ((and (not next-day/night-update-timer)
              (or (not n) (> n 0)))
         (run-day/night-hook-and-reregister))
        ((and next-day/night-update-timer
              (or (not n) (<= n 0)))
         (cancel-timer next-day/night-update-timer)
         (setq next-day/night-update-timer nil))))

;; appropriate colors for day and night
(defvar day-fg "black" "Foreground during day")
(defvar day-bg "white" "Background during day")

(defun enable-day-colors ()
  (interactive)
  (set-face-foreground 'default day-fg)
  (set-face-background 'default day-bg)
  (set-face-background 'cursor "black")
  (set-face-foreground 'cursor "white"))

(defun enable-night-colors ()
  (interactive)
  (set-face-foreground 'default day-bg)
  (set-face-background 'default day-fg)
  (set-face-background 'cursor "white")
  (set-face-foreground 'cursor "black"))

(add-hook 'daytime-hook 'enable-day-colors)
(add-hook 'nighttime-hook 'enable-night-colors)

(toggle-day/night-hooks 1)

;;; Custom Settings

(setq custom-file "~/.emacs.d/custom")
(load custom-file t)

;;; Enabled Disabled Commands

(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
