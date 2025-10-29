;;; blood--elpaca.el -*- lexical-binding: t; no-byte-compile: t; -*-
(loaded? blood-structs)
(llog! "Blood Backend: Elpaca")

;; https://github.com/progfolio/elpaca/blob/master/doc/manual.md
(defconst blood--bootstrap-elpaca-backend-default (blood-build-backend 'elpaca
                                                                      :requires 'elpaca
                                                                      :bootstrap nil
                                                                      :activator nil
                                                                      :sync nil
                                                                      :clean nil
                                                                      )
  )

(provide 'blood--backend-elpaca)
;;; blood--elpaca.el ends here
