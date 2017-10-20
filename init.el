;;acc start-up
(setq gc-cons-threshold 100000000)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (jdee cdlatex auctex flycheck elpy company-jedi nlinum github-theme github-modern-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

 
;;Set scroll setting
(setq scroll-step 1
        scroll-margin 3
        scroll-conservatively 10000)
		
;;Set transparent
(set-frame-parameter (selected-frame) 'alpha (list 90 80))
(add-to-list 'default-frame-alist (cons 'alpha (list 90 80)))
(setq-default cursor-type '(bar . 8))

;;Set paren match
(show-paren-mode 1)

;;set full screen
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;;MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;;Set Github theme
(load-theme 'github-modern t)

;;Set English font
(set-face-attribute
 'default nil :font "Consolas 14") 
 
;;Set Chinese font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "Microsoft Yahei Mono")))	
					
;;Open the recent file
(desktop-save-mode 1)

;;Enable elpy
(elpy-enable)
(add-to-list 'python-shell-completion-native-disabled-interpreters "python")
(setq-default cursor-type 'bar)
(global-linum-mode t)

;;Complete paren
(electric-pair-mode t)

;;Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;ban welcome page
(setq inhibit-startup-message t)

;;Config AUCTex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)
(setq-default LaTeX-command-style '((""
"%(PDF)%(latex)%(extraopts) %S%(PDFout)")
))
(setq preview-gs-command "C:/Program Files (x86)/gs/gs9.22/bin/gswin32c.exe")
(add-hook 'LaTeX-mode-hook 
          (lambda()
             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
             (setq TeX-command-default "XeLaTeX")
             (setq TeX-save-query nil)
             (setq TeX-show-compilation t)))
			 
;;Config CDLatex
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(setq cdlatex-env-alist
      '(("enumerate" "\\begin{enumerate}\n?\n\\end{enumerate}\n" nil)
	    ("itemize" "\\begin{itemize}\n?\n\\end{itemize}\n" nil)
        ("figure" "\\begin{figure}\n\\centering\n\\includegraphics[width=12cm]{}\n\\caption{}\n\label{fig:}\n\\end{figure}\n" nil)))
(setq cdlatex-command-alist
  '(("enu" "Insert enumerate env"   "" cdlatex-environment ("enumerate") t nil)
    ("ite" "Insert itemize env" "" cdlatex-environment ("itemize") t nil)
	("fig" "Insert figure env" "" cdlatex-environment ("figure") t nil)))
