;; blood-trace.el -*- mode: elisp; lexical-binding: t; -*-
  (require 'memory-report)

;; Blood Memory growth tracing
;; overall obj memory usage:
;; (cdar (memory-report--garbage-collect))
;; (memory-report--symbol-plist)
;; (memory-report--buffers)
;; (memory-report--largest-variables)

(defvar blood--trace-memory-list nil)
(defvar blood--trace-precount nil)
(blood-register-cache! :trace #'(lambda (data) (string-join (mapcar #'(lambda (x) (format "%-20s : %s" (car x) (cdr x))) data) "\n"))
                       :header "Memory Growth per package,\n reported by `memory-report--garbage-collect's\n taken before and after loading the package"
                       )


(defmacro blood-trace--memory-pre (package)
  (when blood--trace-memory
    `(setq blood--trace-precount (cdar (memory-report--garbage-collect)))
    )
  )

(defmacro blood-trace--memory-post (package)
  (when blood--trace-memory
    `(push (cons ,package (- (cdar (memory-report--garbage-collect)) blood--trace-precount)) blood--trace-memory-list)
    )
  )

(defun blood-trace--memory-report-h ()
  (hlog! "Package Memory Growth: ")
  (let ((growth-list (reverse blood--trace-memory-list)))
    (dolist (pair growth-list)
      (ilog! "%-20s : %s" (car pair) (cdr pair))
      )
    (blood-cache! :trace growth-list)
    )
  )

(defun blood-trace--load-timing-h ()
  (ilog! "TODO: trace the time it takes to load packages")

  )

(defun blood-trace--memory-components ()
  "adapted from `memory-report--garbage-collect'

Elems are entries of (name size used free)
- NAME is a symbol describing the kind of objects this entry represents,
- SIZE is the number of bytes used by each one,
- USED is the number of those objects that were found live in the heap,
- FREE is the number of those objects that are not live but that Emacs is keeping around

Entry Types are:
conses, symbols, strings, string-bytes, vectors, vector-slots, floats, intervals, buffers,
"
  (let ((elems (garbage-collect))
        )
    (memory-report--set-size elems)
    (let ((sizes (list :strings        (memory-report--gc-elem elems 'strings)
                       :string-bytes   (memory-report--gc-elem elems 'string-bytes)
                       :vectors        (memory-report--gc-elem elems 'vectors)
                       :vector-slots   (memory-report--gc-elem elems 'vector-slots)
                       :floats         (memory-report--gc-elem elems 'floats)
                       :conses         (memory-report--gc-elem elems 'conses)
                       :symbols        (memory-report--gc-elem elems 'symbols)
                       :intervals      (memory-report--gc-elem elems 'intervals)
                       :buffer-objects (memory-report--gc-elem elems 'buffers)
                       ))
          (counts (list :strings        (nth 2 (assq 'strings elems))
                        :string-bytes   (nth 2 (assq 'string-bytes elems))
                        :vectors        (nth 2 (assq 'vectors elems))
                        :vector-slots   (nth 2 (assq 'vector-slots elems))
                        :floats         (nth 2 (assq 'floats elems))
                        :conses         (nth 2 (assq 'conses elems))
                        :symbols        (nth 2 (assq 'symbols elems))
                        :intervals      (nth 2 (assq 'intervals elems))
                        :buffer-objects (nth 2 (assq 'buffers elems))
                        ))
          )
      counts
      ))
  )

(provide 'blood-trace)
