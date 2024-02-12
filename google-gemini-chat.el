;;; google-gemini-chat.el --- Chat module  -*- lexical-binding: t; -*-

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
;; Chat module.
;;

;;; Code:

(require 'google-gemini)

;;
;;; API

;;;###autoload
(cl-defun google-gemini-chat ( contents callback
                               &key
                               (content-type "application/json")
                               (parameters google-gemini-parameters)
                               (key google-gemini-key)
                               (model "gemini-pro")
                               (category "HARM_CATEGORY_DANGEROUS_CONTENT")
                               (threshold "BLOCK_ONLY_HIGH")
                               stop-sequences
                               temperature
                               max-output-tokens
                               top-p
                               top-k)
  "Send chat request.

Arguments CONTENTS and CALLBACK are required for this type of request.
CONTENTS is the chat conversation data.  CALLBACK is the execuation after
request is made.

Arguments PARAMETERS, CONTENT-TYPE, and KEY are global options;
however, you can overwrite the value by passing it in.

The rest of the arugments are optional, please see Google Gemini API reference
page for more information.  Arguments here refer to MODEL, TEMPERATURE,
STOP-SEQUENCES, MAX-OUTPUT-TOKENS, TOP-P, and TOP-K."
  (google-gemini-request (concat google-gemini-generativelanguage-url
                                 "v1beta/models/" model ":generateContent?key="
                                 key)
    :type "POST"
    :params parameters
    :headers (google-gemini--headers content-type)
    :data (google-gemini--json-encode
           `(("contents" . ,contents)
             ("safetySettings" . [(("category" . ,category)
                                   ("threshold" . ,threshold))])
             ("generationConfig" .
              (("stopSequences" . ,stop-sequences)
               ("temperature" . ,temperature)
               ("maxOutputTokens" . ,max-output-tokens)
               ("topP" . ,top-p)
               ("topK" . ,top-k)))))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;
;;; Application

;;;###autoload
(defun google-gemini-chat-prompt ()
  "Start the chat conversation to Google Gemini."
  (interactive)
  (if-let* ((user (read-string "What is your name? " "user"))
            (say (read-string "Start the conversation: ")))
      (google-gemini-chat `[(("role" . ,user)
                             ("parts" . [(("text" . ,say))]))]
                          (lambda (data)
                            (let-alist data
                              (let-alist (elt .candidates 0)
                                (let-alist .content
                                  (let-alist (elt .parts 0)
                                    (message "Response: %s" .text)))))))
    (user-error "Abort, cancel generate content operation")))

(provide 'google-gemini-chat)
;;; google-gemini-chat.el ends here
