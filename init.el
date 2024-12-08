;;; Commentary:
;; ---------------- -*- mode: emacs-lisp-mode; coding: iso-8859-15; -*-
;; .emacs
;;
;; Actualizado (C-u M-! date):           lun dic  1 11:32:24 CET 2003 (jaja!)
;; Luis
;; Update for Linux version in WS Airbus
;;--------------------------------------------------------------------

;; EMACS package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;--------------------------------------------------------------------
;;  C++ IDE
;;--------------------------------------------------------------------

;; lsp-mode
;; --------
;; Load lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (;; Enable lsp-mode for programming modes
         (python-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (ruby-mode . lsp-deferred))
  :commands lsp-deferred
  :config
  (setq lsp-keymap-prefix "C-c l")   ;; Keymap prefix for lsp-mode commands
  (setq lsp-enable-snippet t)         ;; Enable snippets
  (setq lsp-idle-delay 0.5)           ;; Delay for triggering lsp actions
  )

;; Optional UI enhancements for lsp-mode
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)         ;; Enable inline documentation
  (setq lsp-ui-doc-delay 0.3)        ;; Delay for showing documentation
  (setq lsp-ui-sideline-enable t)    ;; Enable inline hints
  (setq lsp-ui-sideline-delay 0.2))

;; Install lsp-treemacs for project outline
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

;; company
;; -------
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

;; flycheck
(use-package flycheck
  :init (global-flycheck-mode))

;; projectile
;; ----------
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'auto))

;; cmake
;; -----
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-ide
  :hook (c++-mode . cmake-ide-setup))

;; gdb
;; ---
(setq gdb-many-windows t
      gdb-show-main t)


; Carga hs-minor-mode por defecto cuando se edita en C
(add-hook 'c-mode-hook 'hs-minor-mode)

(add-hook 'c-mode-hook 'hide-ifdef-mode)

;; carga which-func-mode si editamos C.
(add-hook 'c-mode-hook 'which-func-mode)

;; Hide the comments too when you do a 'hs-hide-all'
(setq hs-hide-comments nil)

;;--------------------------------------------------------------------
;;  Teclas de atajo simples (no sobreescriben ninguna tecla estándar)
;;--------------------------------------------------------------------

;; Quita el scroll-bar
(scroll-bar-mode -1)

;; Quita menu.
(menu-bar-mode -1)

;; M-i para ir a linea
(global-set-key [(meta i)] 'goto-line)

;; Expresiones Regulares
(global-set-key "\M-+" 'replace-regexp)

;; Compile (C-c c)
(global-set-key (kbd "\C-c c") 'compile)

;; Begin of function
(global-set-key [M-up] 'beginning-of-defun)

;; End of function.
(global-set-key [M-down] 'end-of-defun)

;; C-x C-b para menú de buffers; 
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; C-x M-t para insertar fecha personal
;; (global-set-key (kbd "C-x M-f") 'insert-date-personal)

;; f5 para abrir nueva ventana (C-x 5 2)
;; (global-set-key [f5] 'make-frame-command)

;; f3 y f4, C-f3 y C-f4, para folding con hs-minor-mode
(global-set-key [f3] 'hs-hide-block)
(global-set-key [f4] 'hs-show-block)
(global-set-key [(control f3)] 'hs-hide-all)
(global-set-key [(control f4)] 'hs-show-all)

;; C-_ para dabbrev-expand
(global-set-key (kbd "M-_") 'dabbrev-expand)

;; C-M-_ para dabbrev-expand
(global-set-key (kbd "C-M-_") 'dabbrev-completion)

; SET GEOMETRY
(set-frame-width (selected-frame) 170)   
(set-frame-height (selected-frame) 55)

; Set language environment that allow ñ, and accents.
(set-language-environment "Latin-1")
(set-input-method "latin-1-prefix") ;; or "latin-1-postfix"

;; Line number
(setq column-number-mode t)

;; Backup Directory
;; ----------------
(setq backup-directory (expand-file-name "~/.emacs.d/backups/"))
(unless (file-exists-p backup-directory)
  (make-directory backup-directory t))

;; Configurar Emacs para guardar todos los backups en esa carpeta
(setq backup-directory-alist `(("." . ,backup-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,backup-directory t)))
 
;; ----- LOAD-THEME ------------------
;;(load-theme 'tango-dark t)
(load-theme 'deeper-blue t)

;; ;; org-mode
;; ;; Timestamp when triggering one of the DONE states, set the variable "org-log-done".
;; ;;(require 'org-install)
;; (require 'ob-tangle)
;; (setq org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "INFO" "DONE"))
;;       org-todo-keyword-faces '(("TODO" . "magenta")
;; 			       ("DOING" . "orange")
;; 			       ("BLOCKED" . "red")
;; 			       ("INFO" . "black")
;; 			       ("DONE" . "green")))
;; (setq org-log-done t)
;; ;;(require 'org-exp)
;; ;;(require 'org-confluence)

;; (defun my-yank-org-link (text)
;;   (if (derived-mode-p 'org-mode)
;;       (insert text)
;;     (string-match org-bracket-link-regexp text)
;;     (insert (substring text (match-beginning 1) (match-end 1)))))

;; (defun my-org-retrieve-url-from-point ()
;;   (interactive)
;;   (let* ((link-info (assoc :link (org-context)))
;;          (text (when link-info
;;                  ;; org-context seems to return nil if the current element
;;                  ;; starts at buffer-start or ends at buffer-end
;;                  (buffer-substring-no-properties (or (cadr link-info) (point-min))
;;                                                  (or (caddr link-info) (point-max))))))
;;     (if (not text)
;;         (error "Not in org link")
;;       (add-text-properties 0 (length text) '(yank-handler (my-yank-org-link)) text)
;;       (kill-new text))))

;; ;; Open links in the same Window by default.
;; (setq org-link-frame-setup '((file . find-file)))

;; ;; New function open in other frame.
;; (defun org-open-other-window ()
;;   "Jump to bookmark in another window. See `bookmark-jump' for more."
;;   (interactive)
;;   (let ((org-link-frame-setup (acons 'file 'find-file-other-window org-link-frame-setup)))
;;     (org-open-at-point)))

;; (global-set-key (kbd "C-c M-o") 'org-open-other-window)

;; (defun org-formatted-copy ()
;;   "Export region to HTML, and copy it to the clipboard."
;;   (interactive)
;;   (save-window-excursion
;;     (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
;;            (html (with-current-buffer buf (buffer-string))))
;;       (with-current-buffer buf
;;         (shell-command-on-region
;;          (point-min)
;;          (point-max)
;;          "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
;;       (kill-buffer buf))))


;; ;; LINUX
;; ;; (defun my-org-screenshot ()
;; ;;   "Take a screenshot into a time stamped unique-named file in the
;; ;; same directory as the org-buffer and insert a link to this file."
;; ;;   (interactive)
;; ;;   (setq filename
;; ;;         (concat
;; ;;          (make-temp-name
;; ;;           (concat (buffer-file-name)
;; ;;                   "_"
;; ;;                   (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
;; ;;   (call-process "import" nil nil nil filename)
;; ;;   (insert (concat "[[" filename "]]"))
;; ;;   (org-display-inline-images))

;; ;; WINDOWS
;; ;;
;; ;; My Function for Screenshots
;; ;;


;; ;; (defun my-org-screenshot ()
;; ;;   "Take a screenshot into a time stamped unique-named file in the
;; ;; same directory as the org-buffer and insert a link to this file."
;; ;;   (interactive)
;; ;;   (setq filename
;; ;;         (concat
;; ;;          (make-temp-name
;; ;;           (concat (buffer-file-name)
;; ;;                   "_"
;; ;;                   (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
;; ;;   (shell-command "snippingtool /clip")
;; ;;   (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
;; ;;   (insert (concat "[[file:" filename "]]"))
;; ;;   (org-display-inline-images))

;; ;; (global-set-key "\C-cs" 'my-org-screenshot)
;; ;; (global-set-key (kbd "H-w") 'org-formatted-copy)

;; ;; Global Set Keys.
;; (global-set-key (kbd "C-c 1") 'org-store-link)
;; (global-set-key (kbd "C-c C-1") 'org-insert-link)

;; ;; org assign to hihglight
;; ;; (add-to-list 'org-emphasis-alist '("*" (:foreground "red")))

;; ;; org assign to hihglight
;; ;; (add-to-list 'org-emphasis-alist '("=" (:foreground "yellow")))

;; ;; 
;; ;;(use-package org-bullets
;; ;;:ensure t
;; ;;:init
;; ;;:config
;; ;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Acepta 'y' o 'n' en lugar de 'yes' o 'no' como respuesta a preguntas
(fset 'yes-or-no-p 'y-or-n-p)


;; ;; Set whether isearch opens folded comments, code, or both
;; ;; where x is code, comments, t (both), or nil (neither)
;; (setq hs-isearch-open 'x)

;; ;; Resaltado de sintaxis
;; (global-font-lock-mode t)

;; ;; Muestra el número de columna en la barra de estado
;; (column-number-mode t)

;; ;; Modo inteligente de indicar buffers con C-x b
;; ;; (iswitchb-mode 1)

;; ;; Sustituye la campana sonora por la visual
;; (setq visible-bell t)

;; ;; Evita los pitidos en varias ocasiones
;; (setq ring-bell-function
;;       (lambda ()
;; 	(unless (memq this-command
;; 		      '(isearch-abort
;; 			abort-recursive-edit
;; 			exit-minibuffer
;; 			keyboard-quit))
;; 	  (ding))))

;; Quita la campana del todo
(setq ring-bell-function 'ignore)

;; Quita pantalla de bienvenida
(setq inhibit-startup-message t)

;; Face font size (The value is 1/10pt so 100 is 10pt).
(set-face-attribute 'default nil
		    :family "Monospace"
		    :height 80)

;; ;; Quita barra de herramientas de Emacs 21
(if (fboundp 'tool-bar-mode)
     (tool-bar-mode -1))

;; ;; Scroll vertical línea a línea
;; (progn (setq scroll-step 1)
;;        (setq scroll-preserve-screen-position t)
;;        (setq scroll-conservatively 9999))

;; ; Arrastre de la barra de desplazamiento con el botón izquierdo del ratón
;; (global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

;; ; No añade nuevas líneas al llegar al final del archivo
;; (setq next-line-add-newlines nil)

;; ;; Restringe la actuación de una orden a una región sin confirmación; FIXME ¿?
;; ;(put 'narrow-to-region 'disabled nil)

;; ;; No realiza backups
;; ;(setq auto-save-default nil)

;; ;; Carga elscreen ( C-z es el prefijo )
;; (load "elscreen" "ElScreen" t)

;; ;; Menú de modo shift-bt3 www.emacswiki.org/cgi-bin/wiki.pl?ImenuMode
;; (global-set-key [S-mouse-3] 'imenu)

;; ;; Menú de modo en barra de estado en c-mode
;; (add-hook 'c-mode-hook 'imenu-add-menubar-index)

;; ;; Arranca mmm-mode (multiple major modes)
;; (setq mmm-global-mode 'maybe)

;; ;; CPerl mode en lugar de Perl mode
;; (fset 'perl-mode 'cperl-mode)

;; ;; Activa dired-x, para búsquedas en directorios
;; ;; http://emacswiki.org/cgi-bin/wiki.pl?SearchAndReplaceAcrossMultipleFiles
;; (require 'dired-x)

;; ;; Activa rmoo, cliente de moo
;; ;;(require 'rmoo-autoload)
;; ;; Sólo envía comandos de moo si está en la última línea del buffer
;; ;;(setq rmoo-send-require-last-line t)

;; ;; Estilo de indentado en C, "gnu" o "linux" o "k&r" o "bsd"...
;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
;; 	    (c-set-style "gnu")))

;; ;; Carga csharp-mode
;; (autoload 'csharp-mode "csharp-mode"
;;   "Major mode for editing C# code." t)
;; (setq auto-mode-alist (cons '( "\\.cs\\'" . csharp-mode ) auto-mode-alist ))

;; ;; Habilita función downcase-region C-x C-l
;; (put 'downcase-region 'disabled nil)

;; ;; Graba y el estado de los buffers al terminar y los abre al iniciar
;; ;;(setq desktop-basefilename
;; ;;      (convert-standard-filename ".emacs.desktop"))
;; ;;(if (and (file-exists-p (concat "~/" desktop-basefilename))
;; ;;	 (yes-or-no-p "Load desktop? "))
;; ;;    (progn (load "desktop")
;; ;;	   (desktop-load-default)
;; ;;	   (desktop-read) ))

;; ;; EDIFF Configuraciones chulas (from jdonaire)

;; ;; don't open a new frame for ediff control
;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; ;; split ediff windows horizontally if the frame width is enough
;; (setq ediff-split-window-function
;;       (lambda (&optional arg)
;;     (if (> (frame-width) 160)
;;         (split-window-horizontally arg)
;;       (split-window-vertically arg))))

;; ;; Lanzar ediff directamente desde linea de comandos
;; ;; ediff (bojohan): emacs -diff file1 file2
;; (defun command-line-diff (switch)
;;   (let ((file1 (pop command-line-args-left))
;;     (file2 (pop command-line-args-left)))
;;     (ediff file1 file2)))
;; (add-to-list 'command-switch-alist '("-diff" . command-line-diff)) 

;; (defun hide-ctrl-M ()
;;   "Hides the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
;;   (interactive)
;;   (setq buffer-display-table (make-display-table))
;;   (aset buffer-display-table ?\^M []))

;;--------------------------------------------------------------------
;;  Speedbar
;;--------------------------------------------------------------------
;; Load sr-speedbar
(use-package sr-speedbar
  :ensure t
  :config
  ;; Basic configuration
  (setq sr-speedbar-right-side nil)  ;; Display on the left side
  (setq sr-speedbar-width 30)       ;; Default width of the sidebar
  (setq sr-speedbar-max-width 40)   ;; Maximum width of the sidebar
  (setq speedbar-show-unknown-files t) ;; Show all files, even unknown types
  (setq sr-speedbar-auto-refresh t) ;; Automatically refresh the sidebar
  (setq sr-speedbar-skip-other-window-p t) ;; Avoid selecting other windows

;; Open sr-speedbar at startup
  (sr-speedbar-open)
  )  

;; Keyboard shortcut to toggle the sidebar
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)


;;--------------------------------------------------------------------
;;  Funcioncilla apañás
;;--------------------------------------------------------------------

;; Selecciona todo
(defun select-all ()
  (interactive)
  (set-mark (point-min))
  (goto-char (point-max)))

;; Inserta fecha de la forma: vie 11 oct 2002 19:49
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %e %b %Y %H:%M")))

;; Inserta fecha según ISO 8601: 2002-10-11T20:05:44+0200
(defun insert-date-iso ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%T%z")))

;; Inserta fecha de la forma: 12 de octubre de 2002
(defun insert-date-personal ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%d de %B de %Y")))

;; Convierte de DOS -> UNIX
(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; Convierte de UNIX -> DOS
(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))



;-----------------------------------------------------------
;                   Corretor ortografico
;-----------------------------------------------------------
;; ; Hunspell path
;; (add-to-list 'exec-path "c:/Users/c84160/bin/hunspell/bin")
;; (setq ispell-program-name (locate-file "hunspell"
;; 				       exec-path exec-suffixes 'file-executable-p))

;; (setq ispell-local-dictionary-alist '(
				      
;; 				      (nil
;; 				       "[[:alpha:]]"
;; 				       "[^[:alpha:]]"
;; 				       "[']"
;; 				       t
;; 				       ("-d" "en_US" "-p" "D:\\hunspell\\share\\hunspell\\personal.en")
;; 				       nil
;; 				       iso-8859-1)
				      
;; 				      ("american"
;; 				       "[[:alpha:]]"
;; 				       "[^[:alpha:]]"
;; 				       "[']"
;; 				       t
;; 				       ("-d" "en_US" "-p" "D:\\hunspell\\share\\hunspell\\personal.en")
;; 				       nil
;; 				       iso-8859-1)
;; 				      ))

;; (setq ispell-personal-dictionary "c:/Users/c84160/bin/hunspell/share/hunspell")
;; (require 'ispell)

;; (global-set-key (kbd "<f8>") 'ispell-buffer)
;; (global-set-key (kbd "C-<f8>") 'flyspell-mode)

;; (global-set-key "\C-cb" 'ispell-buffer)
;; (global-set-key "\C-cr" 'ispell-region)
;; (global-set-key "\C-cw" 'ispell-word)


;;--------------------------------------------------------------------
;; Guarda los backups en ~/.emacs.backup/
;;--------------------------------------------------------------------
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.backup/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

;; ------------------------------------------------------------
;; PlantUML
;; ------------------------------------------------------------
;; (setq org-plantuml-jar-path (expand-file-name "~/../bin/plantuml/plantuml.jar"))
;; (setq org-plantuml-jar-path (expand-file-name "~/../bin/plantuml/plantuml-1.2022.0.jar"))
;; (load-library "ob-plantuml.el")

;;--------------------------------------------------------------------
;;  Ciclado complejo por ventanas con C-[TAB] y C-M-[TAB]
;;--------------------------------------------------------------------
(defun crs-delete-these (delete-these from-this-list)
  "Delete DELETE-THESE FROM-THIS-LIST."
  (cond
   ((car delete-these)
    (if (member (car delete-these) from-this-list)
	(crs-delete-these (cdr delete-these) (delete (car delete-these)
						     from-this-list))
      (crs-delete-these (cdr delete-these) from-this-list)))
   (t from-this-list)))

; this is the list of buffers I never want to see
(defvar crs-hated-buffers
  '("KILL" "*Compile-Log*"))

(defun crs-hated-buffers ()
  "List of buffers I never want to see, converted from names to buffers."
  (delete nil
	  (append
	   (mapcar 'get-buffer crs-hated-buffers)
	   (mapcar (lambda (this-buffer)
		     (if (string-match "^ " (buffer-name this-buffer))
			 this-buffer))
		   (buffer-list)))))

; I'm sick of switching buffers only to find KILL right in front of me
(defun crs-bury-buffer (&optional n)
  (interactive)
  (unless n
    (setq n 1))
  (let ((my-buffer-list (crs-delete-these (crs-hated-buffers)
					  (buffer-list (selected-frame)))))
    (switch-to-buffer
     (if (< n 0)
	 (nth (+ (length my-buffer-list) n)
	      my-buffer-list)
       (bury-buffer)
       (nth n my-buffer-list)))))

(global-set-key [(control tab)] 'crs-bury-buffer)
(global-set-key [(control meta tab)] (lambda ()
				       (interactive)
				       (crs-bury-buffer -1)))


;;--------------------------------------------------------------------
;;  Habilita la rueda del ratón
;;--------------------------------------------------------------------
(defcustom mouse-wheel-distance 5
  "*Number of lines, maximum, to scroll the window when you move the mouse =
  wheel."
  :type `integer
  :group `mouse)

(defun mouse-wheelscroll-down ()
  " A function to scroll up or down in response to the mouse wheel."
  (interactive)
  (scroll-down
   (min mouse-wheel-distance
	(max 1 (- (window-height)
		  next-screen-context-lines)))))

(defun mouse-wheelscroll-up ()
  " A function to scroll up or down in response to the mouse wheel."
  (interactive)
  (scroll-up
   (min mouse-wheel-distance
	(max 1 (- (window-height)
		  next-screen-context-lines)))))

(global-set-key [mouse-4] (function mouse-wheelscroll-down))
(global-set-key [mouse-5] (function mouse-wheelscroll-up))

;;--------------------------------------------------------------------
;;  ascii-table: función que muestra la tabla de caracteres ascii
;;--------------------------------------------------------------------
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))



;;--------------------------------------------------------------------
;;  devhelp: con F7 lo arranca y busca la palabra del cursor
;;--------------------------------------------------------------------

;; (defun devhelp-word-at-point ()
;;   "runs devhelp"
;;   (interactive)
;;   (setq w (current-word))
;;   (start-process-shell-command "devhelp" nil "devhelp" "-s" w))
;; (global-set-key [f7] 'devhelp-word-at-point)

;; (setq org-src-fontify-natively t)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(case-fold-search t)
;;  '(current-language-environment "English")
;;  '(global-font-lock-mode t nil (font-lock))
;;  '(org-babel-load-languages (quote ((C . t))))
;;  '(org-tags-column 80)
;;  '(package-selected-packages
;;    (quote
;;     (cmake-ide python company ggtags gnu-elpa-keyring-update inline-crypt grep+ git ## org-jira org sr-speedbar htmlize dired+ auctex)))
;;  '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify))))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

(set-default 'truncate-lines t)
(set-default 'hide-ifdef-toggle-shadowing t)

;; ;; --------------------------------------------------------------------------------
;; ;; Calendar
;; ;; --------------------------------------------------------------------------------
;; (copy-face font-lock-constant-face 'calendar-iso-week-face)
;; (set-face-attribute 'calendar-iso-week-face nil
;;                     :height 0.7)
;; (setq calendar-intermonth-text
;;       '(propertize
;;         (format "%2d"
;;                 (car
;;                  (calendar-iso-from-absolute
;;                   (calendar-absolute-from-gregorian (list month day year)))))
;;         'font-lock-face 'calendar-iso-week-face))

;; (setq calendar-week-start-day 1)

;; (defun today ()
;;    (interactive)
;;    (insert (format-time-string "%Y%m%d")))
;; (global-set-key (kbd "C-c C-.") `today)

;; --------------------------------------------------------------------------------
;; ASK BEFORE CLOSING
;; --------------------------------------------------------------------------------
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-teorminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; --------------------------------------------------------------------------------
;; ;; A400M FFS Project
;; ;; --------------------------------------------------------------------------------
;; (defun insert-a400m-jira-issue ()
;;   "Insert a link to the BiNat JIRA server for the A400M"
;;   (interactive)
;;   (setq a400m_issue (read-string "A400M-"))
;;   (insert "[[https://jira.sim.common.airbusds.corp/browse/A400M-"a400m_issue" ][A400M-"a400m_issue"]]"))
;; (global-set-key (kbd "C-c 1") 'insert-a400m-jira-issue)

;; ;; --------------------------------------------------------------------------------
;; ;; SIM Department Project
;; ;; --------------------------------------------------------------------------------
;; (defun insert-sim-jira-issue ()
;;   "Insert a link to the pforge SIM"
;;   (interactive)
;;   (setq sim_issue (read-string "SIM-"))
;;   (insert "[[https://pforge.corporate.eu.astrium.corp/jira/browse/SIM-"sim_issue" ][SIM-"sim_issue"]]"))
;; (global-set-key (kbd "C-c 2") 'insert-sim-jira-issue)

;; ;; --------------------------------------------------------------------------------
;; ;; VERPAS Department Project
;; ;; --------------------------------------------------------------------------------
;; (defun insert-vejmo-jira-issue ()
;;   "Insert a link to the pforge VERPAS"
;;   (interactive)
;;   (setq issue (read-string "VERPAS-"))
;;   (insert "[[https://pforge.corporate.eu.astrium.corp/jira/browse/VERPAS-"issue" ][VERPAS-"issue"]]"))
;; (global-set-key (kbd "C-c 3") 'insert-vejmo-jira-issue)



;; ;; Autocomplete
;; ;; (require 'auto-complete)

;; ;; Set default browser
;; ;;
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

;; --------------------------------------------------------------------------------
;; Semantic
;; --------------------------------------------------------------------------------
;; (require 'cc-mode)
;; (require 'semantic)

;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)

;; (semantic-mode 1)

;; ;; System locale to use for formatting time values
;; (setq system-time-locale "C") 

;; ;; --------------------------------------------------------------------------------
;; ;; ivy & swipe
;; ;; --------------------------------------------------------------------------------
;; ;; (load-library "ivy.el")
;; ;; (ivy-mode 1)
;; ;; (setq ivy-use-virtual-buffers t)
;; ;; (setq ivy-count-format "(%d/%d) ")
;; (load-library "swiper.el")

;; ;; ivy & swiper shortcut
;; (global-set-key (kbd "M-s") 'swiper-isearch)
;; ;;(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;; ;;(global-set-key (kbd "M-n") 'ivy-next-history-element)
;; ;;(global-set-key (kbd "M-p") 'ivy-previous-history-element)
;; ;;(global-set-key (kbd "C-c v") 'ivy-push-view)
;; ;;(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; ;; ;; --------------------------------------------------------------------------------
;; ;; ;; Os Integration
;; ;; --------------------------------------------------------------------------------
;; ;(use-package exec-path-from-shell)

;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

;; ;;--------------------------- fin de .emacs --------------------------
;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    '(projectile magit lsp-ui lsp-mode cmake-mode use-package swiper rtags flycheck company cmake-ide)))
;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;  )
