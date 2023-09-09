;;; blood-utils.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 07, 2023
;; Modified: September 07, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
;;-- end header
(message "----- Loading Utils")
;;-- structs

(defconst blood-profile-struct '(:name                 'unquoted-symbol
                                 :source               'string
                                 :default              'bool
                                 :disabled             'bool
                                 :bootstrap            'list
                                 :backend              '(:name straight :activator #'other)
                                 :modules              'list
                                 :constraints          '(:system :emacs-version)
                                 :paths                '(:install :build :modules)
                                 :recipes              'list
                                 :block-compile-of     'list
                                 :debug                'bool
                                 :post-activation      'fn
                                 )
  "Definition of profile struct. Not a defstruct to make it easier to manually construct"
  )

(defconst blood-module-declaration-struct '(:group    'str|sym
                                            :module   'str|sym
                                            :allow    'list
                                            :disallow 'list
                                            )
  )

(defconst blood-package-struct '(:name         'unquoted-symbol
                                 :recipe       'list-or-symbol
                                 :after        'list
                                 :autoloads    'list
                                 :compile      '(byte native nil)
                                 :constraints  '(:profile :package-version :emacs-version :module-version :commit)
                                 :debug        '((:pre :require :post :complete) . (print break))
                                 :pre-load     'fn
                                 :post-load    'fn
                                 ))

(defconst blood-macro-keywords '(:profile :default :disabled :module-from :install-to
                                 :build-to :active-modules :on-system :on-emacs :cache-to :secrets
                                 :block-compile-of :recipes :debug
                                 )
  )

;;-- end structs

(defun blood--call (prog &rest args)
  "Call a program with arguments, returns (retcode . msg)"
    (with-temp-buffer
      (cons (or (format "%d" (apply #'call-process prog nil t nil args))
                -1)
            (string-trim (buffer-string)))
      )
  )

(defun blood-module-from-path (str)
  "create a (:group %s :module %s) declaration from a path string"
  (let* ((parts (split-string (expand-file-name str) "/" t))
         (relevant (member "modules" parts))
         )
    (if (eq (length relevant) 4)
        `(:group ,(intern (cadr relevant))
          :module ,(intern (caddr relevant)))
      `(:bad-source ,str))
    )
  )

(defun blood--args-to-module-decs (lst)
  "Convert the remaining list into a list of (:group %s :module %s :allow () :disallow ())"
  (let ((source (cl-copy-list lst))
        curr res)
    (while source
      (pcase (pop source)
        (:active-modules: nil)
        ((and kw (pred keywordp) (guard (memq kw '(:allow :disallow))))
         (setq curr (append curr (list kw (pop source))))
         )
        ((and kw (pred keywordp))
         (when curr (push curr res) (setq curr nil))
         (setq curr (append curr (list :group (substring (symbol-name kw) 1) :module (symbol-name (pop source)))))
         )
        (other
         (message "Unknown module dec: %s" other)
         )
        )
      )
    (push curr res)
    (list 'quote res)
    )
  )

(defun blood--update-loadpath ()
  (setq load-path (append
                   (directory-files blood-profile--installation-dir 'full)
                   load-path
                   )
        )
  )

(defmacro log! (text &rest args)
  " A Simple, debug message when 'debug-on-error is true"
  `(when debug-on-error
     (let ((inhibit-message t))
       (funcall #'message ,text ,@args)
       )
     )
  )

(defun inhibit! (&rest inhibited)
  "WARNING: will break stuff. Add a feature to `features', so any `require's for it become no-ops. "
  (mapcar #'provide inhibited)
  )

(provide 'blood-utils)
;;; blood-utils.el ends here
