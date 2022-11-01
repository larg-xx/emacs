;; ---------------- -*- mode: emacs-lisp-mode; coding: iso-8859-15; -*-
;; .emacs
;;
;; Actualizado (C-u M-! date):           lun dic  1 11:32:24 CET 2003 (jaja!)
;; Luis
;; Update for Linux version in WS Airbus
;;--------------------------------------------------------------------

;; EMACS package
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

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

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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

;;--------------------------------------------------------------------
;;  Configuraciones diversas (TODO: mover carga de ficheros al inicio)
;;--------------------------------------------------------------------
;; Defidimos un load-path
(add-to-list 'load-path "~/.emacs.d/lisp")
;; (add-to-list 'load-path "~/.emacs.d/gtags")
;; (add-to-list 'load-path "~/.emacs.d/magit")
;; (add-to-list 'load-path "~/.emacs.d/python-mode.el-6.1.2")
;; (add-to-list 'load-path "~/.emacs.d/org-20140915")
;; (add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa/ivy")
;; (add-to-list 'load-path "~/.emacs.d/with-editor")
;; (add-to-list 'load-path "~/.emacs.d/transient")
(add-to-list 'load-path "~/.emacs.d/elpa/swiper")

;;--------------------------------------------------------------------
;; GNU GLOBAL
;;--------------------------------------------------------------------
;; (require 'ggtags)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))

;; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

;; (define-key ggtags-mode-map (kbd "M-*") 'pop-tag-mark)
;; (define-key ggtags-mode-map (kbd "C-.") 'ggtags-find-reference)

;; (require 'rtags)
;; (cmake-ide-setup)
;; (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;; (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)

;; (use-package rtags
;;   :ensure t
;;   :hook (c++-mode . rtags-start-process-unless-running)
;;   :config (setq rtags-completions-enabled t
;;   		rtags-path "/home/lramirez/.emacs.d/rtags/src/rtags.el"
;;   		rtags-rc-binary-name "/home/lramirez/bin/rc"
;;   		rtags-rdm-binary-name "/home/lramirez/bin/rdm"
;;   		rtags-use-helm t)
;;   :bind (("C-c E" . rtags-find-symbol)
;;   	 ("C-c e" . rtags-find-symbol-at-point)
;;   	 ("M-."   . rtags-find-symbol-at-point)
;;   	 ("C-c O" . rtags-find-references)
;;   	 ("C-c o" . rtags-find-references-at-point)
;;   	 ("C-."   . rtags-find-references-at-point)
;;   	 ("C-c s" . rtags-find-file)
;;   	 ("C-c v" . rtags-find-virtuals-at-point)
;;   	 ("C-c F" . rtags-fixit)
;;   	 ("C-c f" . rtags-location-stack-forward)
;;   	 ("C-c b" . rtags-location-stack-back)
;;   	 ("M-*"   . rtags-location-stack-back)
;;   	 ("C-c n" . rtags-next-match)
;;   	 ("C-c p" . rtags-previous-match)
;;   	 ("C-c P" . rtags-preprocess-file)
;;   	 ("C-c R" . rtags-rename-symbol)
;;   	 ("C-c x" . rtags-show-rtags-buffer)
;;   	 ("C-c T" . rtags-print-symbol-info)
;;   	 ("C-c t" . rtags-symbol-type)
;;   	 ("C-c I" . rtags-include-file)
;;   	 ("C-c i" . rtags-get-include-file-for-symbol))
;;   )

;; (setq rtags-display-result-backend 'helm)


;;--------------------------------------------------------------------
;; COMPANY
;; Is a text completion framework for Emacs. The name stands for "complete anything".
;; It uses pluggable back-ends and front-ends to retrieve and display completion candidates.
;; M-n M-p
;;--------------------------------------------------------------------
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)


(require 'cc-mode)
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  (kbd "M-1") 'company-complete)
(define-key c++-mode-map (kbd "M-1") 'company-complete)

;; Create dir-locals.el
;; ((nil . ((company-clang-arguments . ("-I/home/<user>/project_root/include1/"
;;                                     "-I/home/<user>/project_root/include2/")))))

;; load python-mode library 
 (autoload 'python-mode "python-mode" "Python Mode." t)
 (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
 (add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; ----------------------------------------------------------------------
;; Backup Directory
;; ----------------------------------------------------------------------
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backups" t)))
 
;; ----- COLOR THEME ------------------
;; This is legacy. Now it is used load-theme
;;(add-to-list 'load-path "~/.emacs.d/color-theme/")
;;(require 'color-theme)
;;(eval-after-load "color-theme"
;;  '(progn
;;     (color-theme-initialize)
;;     (color-theme-classic))) ;; Select here the color-theme
;; ------------------------------------

;; ----- LOAD-THEME ------------------
;;(load-theme 'tango-dark t)
(load-theme 'deeper-blue t)
;;(load-theme 'modus-vivendi t)

;; org-mode
;; Timestamp when triggering one of the DONE states, set the variable "org-log-done".
;;(require 'org-install)
(require 'ob-tangle)
(setq org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "INFO" "DONE"))
      org-todo-keyword-faces '(("TODO" . "magenta")
			       ("DOING" . "orange")
			       ("BLOCKED" . "red")
			       ("INFO" . "black")
			       ("DONE" . "green")))
(setq org-log-done t)
;;(require 'org-exp)
;;(require 'org-confluence)

(defun my-yank-org-link (text)
  (if (derived-mode-p 'org-mode)
      (insert text)
    (string-match org-bracket-link-regexp text)
    (insert (substring text (match-beginning 1) (match-end 1)))))

(defun my-org-retrieve-url-from-point ()
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 ;; org-context seems to return nil if the current element
                 ;; starts at buffer-start or ends at buffer-end
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (if (not text)
        (error "Not in org link")
      (add-text-properties 0 (length text) '(yank-handler (my-yank-org-link)) text)
      (kill-new text))))

;;(define-obsolete-function-alias 'org-define-error 'define-error)
;;(setq org-export-babel-evaluate nil)

;; if first line of file matches, activate org-mode
(add-to-list 'magic-mode-alist '("<!DOCTYPE ORG-MODE .+>" . org-mode) )

;; Open links in the same Window by default.
(setq org-link-frame-setup '((file . find-file)))

;; New function open in other frame.
(defun org-open-other-window ()
  "Jump to bookmark in another window. See `bookmark-jump' for more."
  (interactive)
  (let ((org-link-frame-setup (acons 'file 'find-file-other-window org-link-frame-setup)))
    (org-open-at-point)))

(global-set-key (kbd "C-c M-o") 'org-open-other-window)

(defun org-formatted-copy ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))


;; LINUX
;; (defun my-org-screenshot ()
;;   "Take a screenshot into a time stamped unique-named file in the
;; same directory as the org-buffer and insert a link to this file."
;;   (interactive)
;;   (setq filename
;;         (concat
;;          (make-temp-name
;;           (concat (buffer-file-name)
;;                   "_"
;;                   (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
;;   (call-process "import" nil nil nil filename)
;;   (insert (concat "[[" filename "]]"))
;;   (org-display-inline-images))

;; WINDOWS
;;
;; My Function for Screenshots
;;


;; (defun my-org-screenshot ()
;;   "Take a screenshot into a time stamped unique-named file in the
;; same directory as the org-buffer and insert a link to this file."
;;   (interactive)
;;   (setq filename
;;         (concat
;;          (make-temp-name
;;           (concat (buffer-file-name)
;;                   "_"
;;                   (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
;;   (shell-command "snippingtool /clip")
;;   (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
;;   (insert (concat "[[file:" filename "]]"))
;;   (org-display-inline-images))

;; (global-set-key "\C-cs" 'my-org-screenshot)
;; (global-set-key (kbd "H-w") 'org-formatted-copy)

;; Global Set Keys.
(global-set-key (kbd "C-c 1") 'org-store-link)
(global-set-key (kbd "C-c C-1") 'org-insert-link)

;; org assign to hihglight
;; (add-to-list 'org-emphasis-alist '("*" (:foreground "red")))

;; org assign to hihglight
;; (add-to-list 'org-emphasis-alist '("=" (:foreground "yellow")))

;; 
;;(use-package org-bullets
;;:ensure t
;;:init
;;:config
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Carga libreria calculadora.
;(load-library "~/.emacs.d/20calc-init") 

;; Carga libreria emacs-wiki
;(load-library "~/.emacs.d/emacs-wiki.el")

;; (require 'epa-file)
;; (custom-set-variables
;;  ;; '(epg-gpg-home-directory "c:/Users/MYUSER/AppData/Roaming/gnupg")
;;  '(epg-gpg-program "C:/Users/c84160/bin/gpg4win/bin/gpg.exe")
;;  '(epg-gpgconf-program "C:/Users/c84160/bin/gpg4win/bin/gpgconf.exe")
;; )
;; (epa-file-enable)

;; ----- DICTEM ------------- 
;; Carga libreria dict-emacs
;(load-library "~/.emacs.d/dictem.el")
; Loading dictem functions
;;(require 'dictem)

; Setting the server host and port
;(setq dictem-server "localhost")
;(setq dictem-port   "2628")

; Code necessary to obtain database and strategy list
; from DICT server
;(dictem-initialize)
;; ---------------------------

;(global-set-key (kbd "C-c C-v") 'll-debug-insert-debug-output-statement)

;; Carga gtags-mode por defecto cuando se edita en C
;; (setq c-mode-hook
;;       '(lambda ()
;; 	 (gtags-mode 1) ))

;; Lo mismo para c++
;; (setq c++-mode-hook
;;       '(lambda ()
;; 	 (gtags-mode 1) ))

;; -------- TEX ---------------
;; Carga auctex
;;(load "auctex.el" nil t t)
;;(setq TeX-auto-save t)
;;(setq Tex-parse-left t)
;;(setq-default TeX-master nil)

;; ----------------------------

;; Guarda los buffers entre sesiones
;;(load "desktop")
;;(desktop-load-default)
;;(desktop-read)




;; Acepta 'y' o 'n' en lugar de 'yes' o 'no' como respuesta a preguntas
(fset 'yes-or-no-p 'y-or-n-p)

;; Carga hs-minor-mode por defecto cuando se edita en C
(add-hook 'c-mode-hook 'hs-minor-mode)

(add-hook 'c-mode-hook 'hide-ifdef-mode)

;; carga which-func-mode si editamos C.
(add-hook 'c-mode-hook 'which-func-mode)

;; Carga conf-mode si el fichero es .gpj
(add-to-list 'auto-mode-alist '("\\.gpj\\'" . conf-mode))

;; Hide the comments too when you do a 'hs-hide-all'
(setq hs-hide-comments nil)

;; Set whether isearch opens folded comments, code, or both
;; where x is code, comments, t (both), or nil (neither)
(setq hs-isearch-open 'x)

;; Resaltado de sintaxis
(global-font-lock-mode t)

;; Muestra el número de columna en la barra de estado
(column-number-mode t)

;; Modo inteligente de indicar buffers con C-x b
;; (iswitchb-mode 1)

;; Sustituye la campana sonora por la visual
(setq visible-bell t)

;; Evita los pitidos en varias ocasiones
(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort
			abort-recursive-edit
			exit-minibuffer
			keyboard-quit))
	  (ding))))

;; Quita la campana del todo
;(setq ring-bell-function 'ignore)

;; Quita pantalla de bienvenida
(setq inhibit-startup-message t)

;; Face font size (The value is 1/10pt so 100 is 10pt).
(set-face-attribute 'default nil :height 100)

;; Quita barra de herramientas de Emacs 21
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; Scroll vertical línea a línea
(progn (setq scroll-step 1)
       (setq scroll-preserve-screen-position t)
       (setq scroll-conservatively 9999))

; Arrastre de la barra de desplazamiento con el botón izquierdo del ratón
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

; No añade nuevas líneas al llegar al final del archivo
(setq next-line-add-newlines nil)

;; Restringe la actuación de una orden a una región sin confirmación; FIXME ¿?
;(put 'narrow-to-region 'disabled nil)

;; No realiza backups
;(setq auto-save-default nil)

;; Carga elscreen ( C-z es el prefijo )
(load "elscreen" "ElScreen" t)

;; Menú de modo shift-bt3 www.emacswiki.org/cgi-bin/wiki.pl?ImenuMode
(global-set-key [S-mouse-3] 'imenu)

;; Menú de modo en barra de estado en c-mode
(add-hook 'c-mode-hook 'imenu-add-menubar-index)

;; Arranca mmm-mode (multiple major modes)
(setq mmm-global-mode 'maybe)

;; CPerl mode en lugar de Perl mode
(fset 'perl-mode 'cperl-mode)

;; Activa dired-x, para búsquedas en directorios
;; http://emacswiki.org/cgi-bin/wiki.pl?SearchAndReplaceAcrossMultipleFiles
(require 'dired-x)

;; Activa rmoo, cliente de moo
;;(require 'rmoo-autoload)
;; Sólo envía comandos de moo si está en la última línea del buffer
;;(setq rmoo-send-require-last-line t)

;; Estilo de indentado en C, "gnu" o "linux" o "k&r" o "bsd"...
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-set-style "gnu")))

;; Carga csharp-mode
(autoload 'csharp-mode "csharp-mode"
  "Major mode for editing C# code." t)
(setq auto-mode-alist (cons '( "\\.cs\\'" . csharp-mode ) auto-mode-alist ))

;; Habilita función downcase-region C-x C-l
(put 'downcase-region 'disabled nil)

;; Graba y el estado de los buffers al terminar y los abre al iniciar
;;(setq desktop-basefilename
;;      (convert-standard-filename ".emacs.desktop"))
;;(if (and (file-exists-p (concat "~/" desktop-basefilename))
;;	 (yes-or-no-p "Load desktop? "))
;;    (progn (load "desktop")
;;	   (desktop-load-default)
;;	   (desktop-read) ))

;; EDIFF Configuraciones chulas (from jdonaire)

;; don't open a new frame for ediff control
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; split ediff windows horizontally if the frame width is enough
(setq ediff-split-window-function
      (lambda (&optional arg)
    (if (> (frame-width) 160)
        (split-window-horizontally arg)
      (split-window-vertically arg))))

;; Lanzar ediff directamente desde linea de comandos
;; ediff (bojohan): emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
    (file2 (pop command-line-args-left)))
    (ediff file1 file2)))
(add-to-list 'command-switch-alist '("-diff" . command-line-diff)) 

(defun hide-ctrl-M ()
  "Hides the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;--------------------------------------------------------------------
;;  Libraries 
;;--------------------------------------------------------------------
(require 'sr-speedbar)
(require 'cmake-mode)
(require 'go-mode)
(require 'protobuf-mode)

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

(defun devhelp-word-at-point ()
  "runs devhelp"
  (interactive)
  (setq w (current-word))
  (start-process-shell-command "devhelp" nil "devhelp" "-s" w))
(global-set-key [f7] 'devhelp-word-at-point)

(setq org-src-fontify-natively t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style '((c-mode . "") (c++-mode . "")))
 '(case-fold-search t)
 '(current-language-environment "English")
 '(global-font-lock-mode t nil (font-lock))
 '(org-babel-load-languages '((C . t)))
 '(org-tags-column 80)
 '(package-selected-packages
   '(blamer python-mode flymd markdown-mode helm-rtags cmake-ide python company ggtags gnu-elpa-keyring-update inline-crypt grep+ git ## org-jira org sr-speedbar htmlize dired+ auctex))
 '(text-mode-hook '(turn-on-auto-fill text-mode-hook-identify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#7a88cf" :background nil :height 60 :italic t))))

(set-default 'truncate-lines t)
(set-default 'hide-ifdef-toggle-shadowing t)

;; --------------------------------------------------------------------------------
;; Calendar
;; --------------------------------------------------------------------------------
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

(setq calendar-week-start-day 1)

(defun today ()
   (interactive)
   (insert (format-time-string "%Y%m%d")))
(global-set-key (kbd "C-c C-.") `today)

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
;; A400M FFS Project
;; --------------------------------------------------------------------------------
(defun insert-a400m-jira-issue ()
  "Insert a link to the BiNat JIRA server for the A400M"
  (interactive)
  (setq a400m_issue (read-string "A400M-"))
  (insert "[[https://jira.sim.common.airbusds.corp/browse/A400M-"a400m_issue" ][A400M-"a400m_issue"]]"))
(global-set-key (kbd "C-c 1") 'insert-a400m-jira-issue)

;; --------------------------------------------------------------------------------
;; SIM Department Project
;; --------------------------------------------------------------------------------
(defun insert-sim-jira-issue ()
  "Insert a link to the pforge SIM"
  (interactive)
  (setq sim_issue (read-string "SIM-"))
  (insert "[[https://pforge.corporate.eu.astrium.corp/jira/browse/SIM-"sim_issue" ][SIM-"sim_issue"]]"))
(global-set-key (kbd "C-c 2") 'insert-sim-jira-issue)

;; --------------------------------------------------------------------------------
;; VERPAS Department Project
;; --------------------------------------------------------------------------------
(defun insert-vejmo-jira-issue ()
  "Insert a link to the pforge VERPAS"
  (interactive)
  (setq issue (read-string "VERPAS-"))
  (insert "[[https://pforge.corporate.eu.astrium.corp/jira/browse/VERPAS-"issue" ][VERPAS-"issue"]]"))
(global-set-key (kbd "C-c 3") 'insert-vejmo-jira-issue)



;; Autocomplete
;; (require 'auto-complete)

;; Set default browser
;;
;;(setq browse-url-browser-function 'browse-url-generic
;;      browse-url-generic-program "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

;; --------------------------------------------------------------------------------
;; Semantic
;; --------------------------------------------------------------------------------
(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

;; System locale to use for formatting time values
(setq system-time-locale "C") 

;; --------------------------------------------------------------------------------
;; ivy & swipe
;; --------------------------------------------------------------------------------
;; (load-library "ivy.el")
;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")
(load-library "swiper.el")

;; ivy & swiper shortcut
(global-set-key (kbd "M-s") 'swiper-isearch)
;;(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;;(global-set-key (kbd "M-n") 'ivy-next-history-element)
;;(global-set-key (kbd "M-p") 'ivy-previous-history-element)
;;(global-set-key (kbd "C-c v") 'ivy-push-view)
;;(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; Blamer
(use-package blamer
  :bind (("M-I" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 30)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 60
                    :italic t)))
  :config
  (global-blamer-mode 1))

;; --------------------------------------------------------------------------------
;; Clang stuff
;; --------------------------------------------------------------------------------
(require 'clang-format)
(setq clang-format-style "file")
(setq clang-format-executable "/home/lramirez/bin/clang-format")
(setq company-clang-executable "/usr/bin/clang++-12")

;; Autosave hook
(add-hook 'c++-mode-hook (lambda () (add-hook 'before-save-hook 'clang-format-buffer)))
(add-hook 'c-mode-hook (lambda () (add-hook 'before-save-hook 'clang-format-buffer)))

;; copy from gnu style with modification.
(c-add-style "work" 
	     '((c-basic-offset . 2)
	       (c-comment-only-line-offset 0 . 0)
	       (c-hanging-braces-alist
		(substatement-open before after)
		(arglist-cont-nonempty))
	       (c-offsets-alist
		(statement-block-intro . +)
		(knr-argdecl-intro . 5)
		(substatement-open . 0) ;; + -> 0
		(substatement-label . 0)
		(label . 0)
		(statement-case-open . +)
		(statement-cont . +)
		(arglist-intro . c-lineup-arglist-intro-after-paren)
		(arglist-close . c-lineup-arglist)
		(inline-open . 0)
		(brace-list-open . +)
		(brace-list-intro first c-lineup-2nd-brace-entry-in-arglist c-lineup-class-decl-init-+ +)
		(topmost-intro-cont first c-lineup-topmost-intro-cont c-lineup-gnu-DEFUN-intro-cont))
	       (c-special-indent-hook . c-gnu-impose-minimum)
	       (c-block-comment-prefix . #1="")))

(add-hook 'c++-mode-hook (lambda () (c-set-style "work")))
(add-hook 'c-mode-hook (lambda () (c-set-style "work")))

;; (setq c-default-style
;;       '((java-mode . "java")
;; 	(awk-mode . "awk")
;; 	(c-mode . "work")
;; 	(c++-mode . "work")
;; 	(cc-mode . "work")
;; 	(other . "work")))

;; --------------------------------------------------------------------------------
;; XML
;; --------------------------------------------------------------------------------
(load-library "noxml-fold")
(add-hook 'nxml-mode-hook 'noxml-fold-mode)

;; --------------------------------------------------------------------------------
;; ;; Os Integration
;; --------------------------------------------------------------------------------
;(use-package exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;--------------------------- fin de init.el --------------------------

