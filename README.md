<a href="https://gemini.google.com/"><img align="right" src="etc/logo.png"></a>
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/google-gemini.svg)](https://jcs-emacs.github.io/jcs-elpa/#/google-gemini)

# google-gemini
> Elisp library for the Google Gemini API

[![CI](https://github.com/emacs-openai/google-gemini/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-openai/google-gemini/actions/workflows/test.yml)

The [Google Gemini][] Elisp library provides convenient access to the Gemini API from
applications written in the Elisp language. 

*P.S. This package is expected to be used as a library, so there are only a few
interactable commands you can use, and those are mostly examples.*

## 📚 Documentation

- [Google AI for Developers - REST API](https://ai.google.dev/tutorials/rest_quickstart)

## 🔨 Usage

You will need to set up your API key before you can use this library.

```elisp
(setq google-gemini-key "[YOUR API KEY]")
```

Alternatively you can configure a function to retrieve the key from some
external source. A function, `google-gemini-key-auth-source` is provided to
retrieve the key from an auth-source entry under the `:host` key `api.google-gemini.com`

```elisp
(setq google-gemini-key #'google-gemini-key-auth-source)
```

> 💡 Tip
>
> The two variables `google-gemini-key` is the default values for sending requests!
> However, you can still overwrite the value by passing the keywords `:key`!

## 🔰 The simplest example

Here is the simplest example that teaches you how to use this library.
This is a function with a `query` and a callback function.

```elisp
(google-gemini-content-generate "How are you?"
                                (lambda (data)
                                  (message "%s" data)))
```

## 📨 Sending Request

All arguments are exposed in the argument list, so you can send any request in any way you want.

For example, the request function `google-gemini-content-generate` accepts
argument max-tokens. By seeing Google Gemini's references page:

> Max output tokens: Specifies the maximum number of tokens that can be generated
> in the response. A token is approximately four characters. 100 tokens correspond
> to roughly 60-80 words.

```elisp
(google-gemini-content-generate ...
                                ...
                                :max-output-tokens 100)  ; Get roughly 60-80 words!
```

## 🔗 References

- [llm](https://github.com/ahyatt/llm)

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

### 🔬 Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
$ eask install-deps --dev
```

To test the package's installation:

```sh
$ eask package
$ eask install
```

To test compilation:

```sh
$ eask compile
```

**🪧 The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
$ eask lint checkdoc
```

The standard `package` linter:

```sh
$ eask lint package
```

*📝 P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE) for details.


<!-- Links -->

[Google Gemini]: https://gemini.google.com/
