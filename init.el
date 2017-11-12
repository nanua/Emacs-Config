;;acc start-up
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 100000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)))

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
    (jdee matlab-mode cdlatex auctex flycheck elpy company-jedi nlinum github-theme github-modern-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;Smooth mouth scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
 
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
;(when (require 'flycheck nil t)
;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;Ban welcome page
(setq inhibit-startup-message t)

;;Pair paren
(electric-pair-mode 1)

;;Set up Server
(server-start)

;;Resolve Chinese encode problem
(set-language-environment 'Chinese-GB)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq-default pathname-coding-system 'euc-cn)
(setq file-name-coding-system 'euc-cn)
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
;(prefer-coding-system 'utf-16le-with-signature)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)

;;Reread file from disk
(defun refresh-file ()  
  (interactive)  
  (revert-buffer t (not (buffer-modified-p)) t))  
(global-set-key [(control f5)] 'refresh-file)  

;;Enable delete mode
(delete-selection-mode 1)

;;Config AUCTex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)
(setq-default LaTeX-command-style '((""
"%(PDF)%(latex)%(extraopts) %S%(PDFout)")
))
(setq-default fill-column 100)
(setq preview-gs-command "C:/Program Files (x86)/gs/gs9.22/bin/gswin32c.exe")
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 
          (lambda()
             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
             (setq TeX-command-default "XeLaTeX")
             (setq TeX-save-query nil)))
(electric-pair-mode)
(add-hook 'LaTeX-mode-hook
          '(lambda ()
            (define-key LaTeX-mode-map (kbd "(") 'self-insert-command)))
			 
;;Config CDLatex
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(setq cdlatex-env-alist
      '(("enumerate" "\\begin{enumerate}\n?\n\\end{enumerate}" nil)
	    ("itemize" "\\begin{itemize}\n?\n\\end{itemize}" nil)
		("item" "\\item{?}" nil)
		("displaymath" "\\begin{displaymath}\n?\n\\end{displaymath}" nil)
		("align" "\\begin{align*}\n?\n\\end{align*}" nil)
        ("figure" "\\begin{figure}\n\\centering\n\\includegraphics[width=12cm]{?}\n\\caption{}\n\\label{fig:}\n\\end{figure}" nil)
		("table" "\\begin{center}\n\\begin{tabular}\n?\n\\end{tabular}\n\\end{center}" nil)
		("tabularx" "\\begin{table}\n\\begin{tabularx}{\\textwidth}{?}\n\\toprule[1.5pt]\n\n\\midrule[1pt]\n\n\\bottomrule[1.5pt]\n\\end{tabularx}\n\\end{table}" nil)
		("longtable" "\\begin{longtable}{?}\n\\toprule[1.5pt]\n\n\\midrule[1pt]\n\\endfirsthead\n\\toprule[1.5pt]\n\n\\endhead\n\\bottomrule[1.5pt]\n\\endfoot\n\n\\end{longtable}" nil)))
(setq cdlatex-command-alist
  '(("enu" "Insert enumerate env"   "" cdlatex-environment ("enumerate") t nil)
    ("ite" "Insert itemize env" "" cdlatex-environment ("itemize") t nil)
	("it" "Insert item env" "" cdlatex-environment ("item") t nil)
	("fig" "Insert figure env" "" cdlatex-environment ("figure") t nil)
	("tab" "Insert table env" "" cdlatex-environment ("table") t nil)
	("ltab" "Insert longtable env" "" cdlatex-environment ("longtable") t nil)
	("utab" "Insert tabularx env" "" cdlatex-environment ("tabularx") t nil)
	("dismath" "Insert displaymath env" "" cdlatex-environment ("displaymath") t nil)
	("ali" "Insert align env" "" cdlatex-environment ("align") t nil)))

;;Config org-mode
(setq org-startup-indented t)