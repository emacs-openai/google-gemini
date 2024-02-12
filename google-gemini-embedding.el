;;; google-gemini-embedding.el --- Embedding module  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

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
;; Embedding module.
;;

;;; Code:

(require 'google-gemini)

;;
;;; API

;;;###autoload
(cl-defun google-gemini-embedding ( text callback
                                    &key
                                    (content-type "application/json")
                                    (key google-gemini-key)
                                    (model "embedding-001")
                                    (category "HARM_CATEGORY_DANGEROUS_CONTENT")
                                    (threshold "BLOCK_ONLY_HIGH")
                                    stop-sequences
                                    temperature
                                    max-output-tokens
                                    top-p
                                    top-k)
  "Send generate content request."
  (google-gemini-request (concat google-gemini-generativelanguage-url
                                 "v1beta/models/" model ":embedContent?key="
                                 key)
    :type "POST"
    :headers (google-gemini--headers content-type)
    :data (google-gemini--json-encode
           `(("model" . ,(concat "models/" model))
             ("content" . (("parts" . [(("text" . ,text))])))))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;
;;; Application

;;;###autoload
(defun google-gemini-embedding-prompt ()
  "Ask to embedding from Google Gemini."
  (interactive)
  (if-let ((text (read-string "[Embedding] Content: ")))
      (google-gemini-embedding text (lambda (data) (message "%s" data)))
    (user-error "Abort, cancel embedding operation")))

(provide 'google-gemini-embedding)
;;; google-gemini-embedding.el ends here
