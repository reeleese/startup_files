;; Melpa
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
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
    ))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("40670c1a2158e4387407ca1c8a06c03153baca5016e3f9f23d7fb4157e408960" default)))
 '(package-selected-packages
   (quote
    (zenburn-theme circadian magit leuven-theme column-enforce-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; I prefer cmd key for meta
(if(eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
	  mac-command-key-is-meta t
	  mac-command-modifier 'meta
	  mac-option-modifier 'none)
  )

;; Startup stuff
(defun my-startup-hook ()
  (server-start)
  (scroll-bar-mode -1)
  (when (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode))
  )
(add-hook 'emacs-startup-hook 'my-startup-hook)

;; Circadian
(use-package circadian
  :ensure t
  :config
  (setq calendar-latitude 29.6)
  (setq calendar-longitude -98.6)
  (setq circadian-themes '((:sunrise . leuven)
                            (:sunset  . zenburn)))
  (circadian-setup))

;; C customizations
(defun my-c-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq-default c-basic-offset 4)
  (column-enforce-mode)
  )
(add-hook 'c-mode-hook 'my-c-mode-hook)

