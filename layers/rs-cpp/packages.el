;;; packages.el --- rs-cpp Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq rs-cpp-packages
      '(
        cc-mode
        company
        irony
        company-irony
        flycheck
        flycheck-irony
        clang-format
      ))

;; List of packages to exclude.
(setq rs-cpp-excluded-packages '())


(defvar rs/script-directory (if load-file-name
                         (file-name-directory load-file-name)
                       default-directory))
(defun rs/get-projectile-dir()
  (interactive)
  (message "Project dir: %s %s"
           (projectile-project-root)
           rs/script-directory))

(defun rs/copy-clang-format()
  (interactive)
    (copy-file (concat rs/script-directory ".clang-format") (projectile-project-root)))

(defun rs-cpp/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (add-to-list 'auto-mode-alist `("\\.h$" . ,c-c++-default-mode-for-headers))
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1)
      (evil-leader/set-key-for-mode 'c-mode
        "mga" 'projectile-find-other-file
        "mgA" 'projectile-find-other-file-other-window)
      (evil-leader/set-key-for-mode 'c++-mode
        "mga" 'projectile-find-other-file
        "mgA" 'projectile-find-other-file-other-window))))

(defun rs-cpp/init-clang-format ()
  (use-package clang-format))

(defun rs-cpp/init-irony()
  (use-package irony
    :ensure t
    :defer t
    :init
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    :config
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
(message "RS: Irony loaded"))


(defun rs-cpp/post-init-company ()
  (spacemacs|add-company-hook c-mode-common)
  (setq company-idle-delay 0)
  )


(when (configuration-layer/layer-usedp 'auto-completion)
  (defun rs-cpp/init-company-irony ()
    (use-package company-irony
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init (push 'company-irony company-backends-c-mode-common))))


(defun rs-cpp/post-init-flycheck ()
  (spacemacs/add-to-hooks 'flycheck-mode '(c-mode-hook c++-mode-hook))
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(defun rs-cpp/init-flycheck-irony()
  (use-package flycheck-irony
    :defer t))

(defun check-compile-options ()
  (interactive)
  (irony-cdb-json--ensure-project-alist-loaded)
  (irony--aif (irony-cdb-json--locate-db)
      (progn
        (message "I: found compilation database: %s" it)
        (let ((db (irony-cdb-json--load-db it)))
          (irony--aif (irony-cdb-json--exact-flags db)
              (progn
                (message "I: found exact match: %s" it)
                it)
            (let ((dir-cdb (irony-cdb-json--compute-directory-cdb db)))
              (irony--aif (irony-cdb-json--guess-flags dir-cdb)
                  (message "I: found by guessing: %s" it)
                (message "E: guessing failed"))))))
    (message "E: failed to locate compilation database")))

;; For each package, define a function rs-cpp/init-<package-name>
;;
;; (defun rs-cpp/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
