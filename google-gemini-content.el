;;; google-gemini-content.el --- Create generate content with Google Gemini API  -*- lexical-binding: t; -*-

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
;; Create generate content with Google Gemini API.
;;

;;; Code:

(require 'google-gemini)

;;
;;; API

(defun google-gemini-content-make-contents (text)
  "Create a contents value."
  `((parts . [((text . ,text))])))

;;;###autoload
(cl-defun google-gemini-content-generate ( contents callback
                                           &key
                                           (content-type "application/json"))
  "Send generate content request."
  (request (format "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:generateContent?key=%s"
                   google-gemini-key)
    :type "POST"
    :headers (google-gemini--headers content-type)
    :data (google-gemini--json-encode
           `(("contents" . [,contents])))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;
;;; Application

;;;###autoload
(defun google-gemini-content-prompt ()
  "Start making a conversation to Google Gemini."
  (interactive)
  (if-let* ((text (read-string "Content: "))
            (contents (google-gemini-content-make-contents text)))
      (google-gemini-content-generate contents
                                      (lambda (data)
                                        (let-alist data
                                          (let-alist (elt .candidates 0)
                                            (let-alist .content
                                              (let-alist (elt .parts 0)
                                                (message "%s" .text)))))))
    (user-error "Abort, cancel generate content operation")))

(provide 'google-gemini-content)
;;; google-gemini-content.el ends here
