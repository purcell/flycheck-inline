;;; flycheck-inline.el --- Display flycheck error message with inline popup style.

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "24.4") (cl-lib "0.3") (inline-docs "1.0.1"))
;; Package-Version: 0.1
;; Keywords: tools convenience
;; homepage: https://github.com/stardiviner/flycheck-inline

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
(require 'inline-docs)

(defun flycheck-inline (errors)
  "Display the flycheck message of ERRORS with inline style."
  (when errors
    (if (display-graphic-p)
        (let ((message (mapconcat #'flycheck-error-format-message-and-id
                                  errors "\n\n")))
          (inline-docs message))
      (funcall 'flycheck-display-error-messages)))
  )

(define-minor-mode flycheck-inline-mode
  "This is a minor mode to display flycheck error message in inline way."
  :global t
  (if flycheck-inline-mode
      (with-eval-after-load 'flycheck
        (setq flycheck-display-errors-function 'flycheck-inline))
    (setq flycheck-display-errors-function 'flycheck-display-error-messages)
    ))


;;; ----------------------------------------------------------------------------

(provide 'flycheck-inline)

;;; flycheck-inline.el ends here
