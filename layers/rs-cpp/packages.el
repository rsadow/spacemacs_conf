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
        rtags
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
  (let ((clang-src-file-name (concat rs/script-directory ".clang-format"))
        (clang-dst-file-name (concat (projectile-project-root) ".clang-format")))
    (when (file-newer-than-file-p clang-src-file-name clang-dst-file-name)
      (progn
        (message "Copy file %s to %s" clang-src-file-name (projectile-project-root))
        (copy-file clang-src-file-name (projectile-project-root) t)
      ))))

(defun rs/parse-compilation-db()
  (let ((compile-json-src (concat (projectile-project-root) "/bin/LinuxX86-64_WMP_FCT/compile_commands.json"))
        (compile-json-dst (concat (projectile-project-root) "compile_commands.json")))
    (when (file-exists-p compile-json-src)
      (unless (file-exists-p compile-json-dst)
        (progn
          (message "Reparsing...")
          (copy-file compile-json-src (projectile-project-root) t)
          (shell-command-to-string (format "sed -E -i -e 's/[^ ]+x86_64-pc-linux-gnu-g[+|c][^ ]+|--sysroot[^ ]+|-D__CCS_INLINE__[^ ]+//g' %s" compile-json-dst))
          )))))

(defun rs/init-cpp-project()
  (interactive)
  (rs/copy-clang-format)
  (rs/parse-compilation-db))

(defun rs/reinit-cpp-project()
  (interactive)
  (let ((compile-json-dst (concat (projectile-project-root) "compile_commands.json")))
    (when (file-exists-p compile-json-dst)
      (delete-file compile-json-dst)
      (rs/init-cpp-project))))

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
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))


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


(defun rs-cpp/init-rtags()
  (use-package rtags
    :if (eq major-mode 'c++-mode)
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
