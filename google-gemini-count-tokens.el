;;; google-gemini-count-tokens.el --- Create count tokens request with Google Gemini API  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Create count tokens request with Google Gemini API.
;;

;;; Code:

(require 'google-gemini)

;;
;;; API

;;;###autoload
(cl-defun google-gemini-count-tokens ( text callback
                                       &key
                                       (parameters google-gemini-parameters)
                                       (content-type "application/json")
                                       (key google-gemini-key)
                                       (model "gemini-pro"))
  "Send count tokens request.

Arguments PARAMETERS, CONTENT-TYPE, and KEY are global options;
however, you can overwrite the value by passing it in.

The rest of the arugments are optional, please see Google Gemini API reference
page for more information.  Arguments here refer to MODEL."
  (google-gemini-request (concat google-gemini-generativelanguage-url
                                 "v1beta/models/" model ":countTokens?key="
                                 key)
    :type "POST"
    :params parameters
    :headers (google-gemini--headers content-type)
    :data (google-gemini--json-encode
           `(("contents" . [((parts . [((text . ,text))]))])))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;
;;; Application

;;;###autoload
(defun google-gemini-count-tokens-prompt ()
  "Send request to count tokens."
  (interactive)
  (if-let ((text (read-string "[Count Tokens] Content: ")))
      (google-gemini-count-tokens text
                                  (lambda (data)
                                    (let-alist data
                                      (message "`totalTokens` is %s" .totalTokens))))
    (user-error "Abort, cancel generate content operation")))

(provide 'google-gemini-count-tokens)
;;; google-gemini-count-tokens.el ends here
