;;; blood--elpaca.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;; blood functions for bootstrapping and using elpaca
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'blood-structs)

;; https://github.com/progfolio/elpaca/blob/master/doc/manual.md
(defconst blood--bootstrap-straight-backend-default (blood--backend-s :name 'elpaca
                                                                      :require 'elpaca
                                                                      :bootstrap nil
                                                                      :activator nil
                                                                      :sync nil
                                                                      :clean nil
                                                                      )




(provide 'blood--elpaca)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 24, 2024
;; Modified:   July 24, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; blood--elpaca.el ends here
