;;; google-gemini.el --- Elisp library for the Google Gemini API  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  JenChieh

;; Author: JenChieh <jcs090218@gmail.com>
;; Maintainer: JenChieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/google-gemini
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (request "0.3.0"))
;; Keywords: comm google gemini

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
;; Elisp library for the Google Gemini API
;;

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'let-alist)
(require 'pcase)
(require 'pp)
(require 'json)

(require 'request)

(defgroup google-gemini nil
  "Elisp library for the Google Gemini API."
  :prefix "google-gemini-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/google-gemini"))

;;
;;; Logger

(defvar google-gemini--show-log nil
  "Get more information from the program.")

(defun google-gemini--log (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when google-gemini--show-log
    (apply 'message fmt args)))

;;
;;; Request

(defvar google-gemini-key ""
  "Variable storing the gemini key or a function name to retrieve it.

The function should take no arguments and return a string containing the key.

A function, `google-gemini-key-auth-source', that retrieves the key from
auth-source is provided for convenience.")

(defcustom google-gemini-base-url "googleapis.com"
  "The base URL for Google Gemini API requests."
  :type 'string
  :group 'google-gemini)

;;;###autoload
(defun google-gemini-key-auth-source (&optional base-url)
  "Retrieve the Google Gemini API key from auth-source given a BASE-URL.
If BASE-URL is not specified, it defaults to `google-gemini-base-url'."
  (if-let ((auth-info
            (auth-source-search :max 1
                                :host (or (url-host (url-generic-parse-url (or base-url google-gemini-base-url)))
                                          google-gemini-base-url)
                                :require '(:user :secret))))
      (funcall (plist-get (car auth-info) :secret))
    (error "Google Gemini API key not found in auth-source")))

(defun google-gemini--alist-omit-null (alist)
  "Omit null value or empty string in ALIST."
  (cl-remove-if (lambda (pair)
                  (let ((value (cdr pair)))
                    (or (null value)          ; ignore null
                        (and (stringp value)  ; ignore empty string
                             (string-empty-p value)))))
                alist))

(defun google-gemini--headers (content-type)
  "Construct request headers.

Arguments CONTENT-TYPE are common request headers."
  (google-gemini--alist-omit-null
   `(("Content-Type" . ,content-type))))

(defun google-gemini--json-encode (object)
  "Wrapper for function `json-encode' but it remove nil value before
constructing JSON data.

The argument OBJECT is an alist that can be construct to JSON data; see function
`json-encode' for the detials."
  (let* ((object (google-gemini--alist-omit-null object))
         (encoded (json-encode object)))
    (google-gemini--log "[ENCODED]: %s" encoded)
    encoded))

(provide 'google-gemini)
;;; google-gemini.el ends here
