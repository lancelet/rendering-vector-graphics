;; Major mode for bookit
;;
;; M-x eval-buffer

(setq
 bookit-highlights
 '(
   ("|(\\|)\\|(\\|)" . font-lock-keyword-face)
   ))

(define-derived-mode
  bookit-mode
  fundamental-mode
  "bookit"
  "major mode for editing bookit documents"
  (setq font-lock-defaults '(bookit-highlights)))
