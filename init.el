;;Smooth mouth scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
 
;;Set scroll setting
(setq scroll-step 1
        scroll-margin 3
        scroll-conservatively 10000)

;;Set paren match
(show-paren-mode 1)

;;Enable line number
(global-linum-mode t)

;;Complete paren
(electric-pair-mode t)

;;Ban welcome page
(setq inhibit-startup-message t)

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

;;Config org-mode
(setq org-startup-indented t)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;;No backup
(setq make-backup-files nil)

;; copy region or whole line
(global-set-key "\M-w"
(lambda ()
  (interactive)
  (if mark-active
      (kill-ring-save (region-beginning)
      (region-end))
    (progn
     (kill-ring-save (line-beginning-position)
     (line-end-position))
     (message "copied line")))))

;; kill region or whole line
(global-set-key "\C-w"
(lambda ()
  (interactive)
  (if mark-active
      (kill-region (region-beginning)
   (region-end))
    (progn
     (kill-region (line-beginning-position)
  (line-end-position))
     (message "killed line")))))

;;disable tool bar
(tool-bar-mode 0)
