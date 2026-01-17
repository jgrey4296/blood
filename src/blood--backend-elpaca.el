;;; blood--elpaca.el -*- lexical-binding: t; no-byte-compile: t; -*-
;; https://github.com/progfolio/elpaca/blob/master/doc/manual.md
;;
(eval-and-compile
  (require 'blood-deferral)
  (require 'blood-log)
  )
(loaded? blood-structs)
(llog! "Blood Backend: Elpaca")


(defconst blood--backend-elpaca-default
  (blood-build-backend 'elpaca
                       :requires 'elpaca
                       :bootstrap nil
                       :activator nil
                       :sync nil
                       :clean nil
                       )
  )

(provide 'blood--backend-elpaca)
;;; blood--elpaca.el ends here
