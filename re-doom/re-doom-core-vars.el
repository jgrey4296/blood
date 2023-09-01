;;; re-doom-core-vars.el -*- lexical-binding: t; no-byte-compile: t; -*-
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

;;-- consts

(defconst re-doom-version                 "0.1.0" "This Version of re-doom")


(defconst REDOOM-PROFILE-FILE-NAME "profile.el" "re-doom will search and load all profiles in files with this name")

(defconst REDOOM-MODULE-FILE-NAME  "module.el"  "re-doom will search and load all files named this in module directories, to get package specs")

(defconst re-doom--available-cmds '(batch clean sync run report stub))

(defconst re-doom--bootstrap-defaults '(
                                        #'re-doom--bootstrap-straight
                                        #'re-doom--init-straight
                                        ))

(defconst re-doom--hook-laziness '(;; Not Lazy
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

;;-- end consts

;; Customs

(defgroup re-doom nil "General Redoom settings")

(defcustom re-doom-cache-dir (expand-file-name "~/.config/re-doom/") "the directory to use as the emacs cache head")

(defcustom re-doom-additional-bootstrappers nil "functions to run as part of the bootstrap process" :type 'list)

;; Private


(defvar re-doom--active-module-specs  nil "The Modules to activate in after-init-hook")

(defvar re-doom--cmd nil)

(defvar re-doom--module-packages (make-hash-table) "Maps Module-name -> package-specs")

(defvar re-doom--package-declaration-failures nil)

;; Public

(defvar RE-DOOM-BOOTSTRAPPED nil)

(defvar re-doom-profile-spec-list     nil "All declared profiles, which can be activated later")

(defvar re-doom-active-profile-specs nil "The Specs of the active profiles, as a stack. the original profile is last.")

(defvar re-doom-profile                 "default" "The current profile")
;;-- structs

(defconst re-doom-profile-struct '(:name                 'unquoted-symbol
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

(defconst re-doom-module-declaration-struct '(
                                              :group    'str|sym
                                              :module   'str|sym
                                              :allow    'list
                                              :disallow 'list
                                              )
  )

(defconst re-doom-package-struct '(
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

(defconst re-doom-macro-keywords '(:profile :default :disabled :module-from :install-to
                                   :build-to :active-modules :on-system :on-emacs :cache-to :secrets
                                   :block-compile-of :recipes :debug)
  )

;;-- end structs

(provide 're-doom-core-vars)
;;; re-doom-core-vars.el ends here
