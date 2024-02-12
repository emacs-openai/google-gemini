;;; google-gemini-model.el --- More info regarding models  -*- lexical-binding: t; -*-

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
;; More info regarding models.
;;

;;; Code:

(require 'google-gemini)

;;
;;; API

;;;###autoload
(cl-defun google-gemini-model ( model
                                callback
                                &key
                                (parameters google-gemini-parameters)
                                (key google-gemini-key))
  "Send request to get model information.

Arguments MODEL and CALLBACK are required for this type of request.
MODEL is the name of the model.  CALLBACK is the execuation after request
is made.

Arguments PARAMETERS, and KEY are global options; however, you can overwrite the
value by passing it in."
  (google-gemini-request (concat google-gemini-generativelanguage-url
                                 "v1beta/models/" model "?key="
                                 key)
    :type "GET"
    :params parameters
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;;###autoload
(cl-defun google-gemini-models ( callback
                                 &key
                                 (parameters google-gemini-parameters)
                                 (key google-gemini-key))
  "Send request to get a list of supported models.

Arguments CALLBACK is required for this type of request.
CALLBACK is the execuation after request is made.

Arguments PARAMETERS, and KEY are global options; however, you can overwrite the
value by passing it in."
  (google-gemini-request (concat google-gemini-generativelanguage-url
                                 "v1beta/models?key="
                                 key)
    :type "GET"
    :params parameters
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;
;;; Application

;;;###autoload
(defun google-gemini-model-info ()
  "Print a model information."
  (interactive)
  (if-let ((model (read-string "Name of the model: " "gemini-pro")))
      (google-gemini-model model (lambda (data) (message "%s" data)))
    (user-error "Abort, cancel get model info operation")))

(defvar google-gemini-model-entries nil
  "Async models entries.")

(tblui-define
 google-gemini-model
 "Google Gemini Model" "Display models information from Google Gemini."
 (lambda () google-gemini-model-entries)
 [("Name" 30 nil)
  ("Version" 5 nil)
  ("Description" 5 nil)]
 nil)

;;;###autoload
(defun google-gemini-list-models ()
  "List out all supported models."
  (interactive)
  (setq google-gemini-model-entries nil)  ; reset
  (google-gemini-models (lambda (data)
                          (let-alist data
                            (mapc (lambda (model)
                                    (let-alist model
                                      (push (list (length google-gemini-model-entries)
                                                  (vector .name
                                                          .version
                                                          .description))
                                            google-gemini-model-entries)))
                                  .models))
                          (google-gemini-model-goto-ui))))

(provide 'google-gemini-model)
;;; google-gemini-model.el ends here
