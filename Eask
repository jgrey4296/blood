;; -*- mode: eask; lexical-binding: t -*-

(package "blood"
         "0.1.0"
         "An emacs framework")

(website-url "https://github.com/jgrey4296/blood")
(keywords "init" "framework")

(package-descriptor "src/blood-pkg.el")
(files "src/blood.el"
       "src/blood-*.el"
       "src/blood--*.el"
       "early-init.el"
       '(:exclude "_related/**")
       '(:exclude "src/__tests/*")
       )

(source "gnu")
(source "melpa")

(depends-on "emacs" "30.2")
(depends-on "cl-lib" "1.5")

(development
 (depends-on "evil")
 )
