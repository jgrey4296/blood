;;; blood-core-vars.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 01, 2023
;; Modified: September 01, 2023
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

(defgroup blood nil "Blood settings")

;; consts

(defconst blood-version                 "0.1.0" "This Version of blood")

(defconst BLOOD-PROFILE-FILE-PATTERN "profile\\(-.+\\)?.el" "blood will search and load all profiles in files with this name")

(defconst BLOOD-MODULE-FILE-PATTERN "module\\(-.+\\).el"  "blood will search and load all files named this in module directories, to get package specs")

(defconst blood--available-cmds '(batch clean sync run report stub))

(defconst blood--hook-laziness '(;; Not Lazy
                                     :bootstrap    -99
                                     :clean        -90
                                     :sync         -85
                                     :build        -80
                                     :run          -70
                                     :profile-init -60
                                     :module-init  -50
                                     :module-config 25
                                     :user-min      50
                                     :user-max      99
                                     ) ;; Lazy
  )

(defconst blood--bootstrap-defaults (list
                                       #'blood--bootstrap-straight
                                       #'blood--bootstrap-straight-init
                                       ))

;;-- structs

(defconst blood-profile-struct '(:name                 'unquoted-symbol
                                   :source               'string
                                   :default              'bool
                                   :disabled             'bool
                                   :bootstrap            'list
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

(defconst blood-module-declaration-struct '(
                                              :group    'str|sym
                                              :module   'str|sym
                                              :allow    'list
                                              :disallow 'list
                                              )
  )

(defconst blood-package-struct '(
                                   :name         'unquoted-symbol
                                   :recipe       'list-or-symbol
                                   :after        'list
                                   :defer        'bool
                                   :autoloads    'list
                                   :compile      '(byte native nil)
                                   :constraints  '(:profile :package-version :emacs-version :module-version :commit)
                                   :debug        '((:pre :require :post :complete) . (print break))
                                   :pre-load     'fn
                                   :post-load    'fn
                                   ))

(defconst blood-macro-keywords '(:profile :default :disabled :module-from :install-to
                                   :build-to :active-modules :on-system :on-emacs :cache-to :secrets
                                   :block-compile-of :recipes :debug)
  )

;;-- end structs

;; Customs

(defcustom blood-cache-dir (expand-file-name "~/.cache/blood/") "the directory to use as the emacs/straight cache head")

(defcustom blood-config-dir (expand-file-name "~/.config/blood/") "directory for config files")

(defcustom blood-additional-bootstrappers nil "functions to run as part of the bootstrap process" :type 'list)

;; Private

(defvar blood--active-module-specs  nil "The Modules to activate in after-init-hook")

(defvar blood--module-packages (make-hash-table) "Maps Module-name -> package-specs")

(defvar blood--package-declaration-failures nil)

(defvar blood--sync-backend nil "function that takes a list of packages to install, and installs them")

;; Public

(defvar BLOOD-BOOTSTRAPPED nil)

(defvar blood-profile-spec-list     nil "All declared profiles, which can be activated later")

(defvar blood-active-profile-specs nil "The Specs of the active profiles, as a stack. the original profile is last.")

(provide 'blood-core-vars)
;;; blood-core-vars.el ends here
