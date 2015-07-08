;;; myanmar3-input-method.el --- Myanmar Emacs Input Method with Myanmar3 Keyboard Layout

;; Copyright (C) 2015 Ye Lin Kyaw
;; Author: Ye Lin Kyaw <yelinkyaw@gmail.com>
;; Created: 06 Jul 2015
;; Last-Updated: 06 Jul 2015
;; Version: 0.0.1
;; Keywords: Myanmar, Unicode, Keyboard
;; Homepage: http://github.com/yelinkyaw/emacs-myanmar-input-methods

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; Version 3.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a Emacs input method for Myanmar with Myanmar3 Layout

;;; Installation:

;; To install, just drop this file into a directory in your
;; `load-path' and (optionally) byte-compile it. And add:
;;
;;    (require 'myanmar-myanmar3)
;;
;; to your .emacs file.

(provide 'myanmar-myanmar3)
(require 'quail)
(quail-define-package
 "myanmar-myanmar3"
 "Myanmar"
 "မြန်"
 nil
 "Myanmar3 Keyboard Layout"
 nil
 t
 t
 t
 t
 nil
 nil
 nil
 nil
 nil
 t
 )


(defconst myanmar-consonants
 '(
   ("u" . "က")
   ("c" . "ခ")
   (":" . "ဂ")
   ("C" . "ဃ")
   ("i" . "င")
   ("p" . "စ")
   ("`" . "ၑ")
   ("q" . "ဆ")
   ("Z" . "ဇ")
   ("Q" . "ဈ")
   ("n" . "ည")
   ("N" . "ဉ")
   ("#" . "ဋ")
   ("X" . "ဌ")
   ("!" . "ဍ")
   ("~" . "ဎ")
   ("P" . "ဏ")
   ("w" . "တ")
   ("x" . "ထ")
   ("K" . "ဒ")
   ("L" . "ဓ")
   ("e" . "န")
   ("y" . "ပ")
   ("z" . "ဖ")
   ("A" . "ဗ")
   ("b" . "ဘ")
   ("r" . "မ")
   ("B" . "ယ")
   ("&" . "ရ")
   ("v" . "လ")
   ("W" . "ဝ")
   ("o" . "သ")
   ("O" . "ဿ")
   ("[" . "ဟ")
   ("V" . "ဠ")))

(defconst myanmar-independent-vowels
 '(
   ("t" . "အ")
   ("E" . "ဣ")
   ("T" . "ဤ")
   ("U" . "ဥ")
   ("M" . "ဦ")
   ("{" . "ဧ")
   ("]" . "ဩ")
   ("}" . "ဪ")))

(defconst myanmar-dependent-vowels
 '(
   ("g" . "ါ")
   ("m" . "ာ")
   ("d" . "ိ")
   ("D" . "ီ")
   ("k" . "ု")
   ("l" . "ူ")
   ("a" . "ေ")
   ("J" . "ဲ")))

(defconst myanmar-various-signs
 '(
   ("H" . "ံ")
   ("h" . "့")
   (";" . "း")
   ("F" . "္")
   ("f" . "်")
   ("Y" . "၌")
   ("I" . "၍")
   ("R" . "၎င်း")
   ("\\" . "၏")))

(defconst myanmar-consonant-signs
 '(
   ("s" . "ျ")
   ("j" . "ြ")
   ("G" . "ွ")
   ("S" . "ှ")))

(defconst myanmar-digits
 '(
   ("0" . "၀")
   ("1" . "၁")
   ("2" . "၂")
   ("3" . "၃")
   ("4" . "၄")
   ("5" . "၅")
   ("6" . "၆")
   ("7" . "၇")
   ("8" . "၈")
   ("9" . "၉")))

(defconst myanmar-punctuations
 '(
   ("<" . "၊")
   (">" . "။")))

(defconst myanmar-custom-rules
 '(
   ("Hk" . "ုံ")
   ("ps" . "ဈ")
   ("oj" . "ဩ")
   ("ojamf" . "ဪ")
   ))

;; Rules Generator
(defun generate-rules(lists)
  (let (rules)
    (while lists
      (quail-defrule (car (car lists)) (vector (cdr (car lists))))
      (setq lists (cdr lists)))
    ))

;; Generate Rules
;; Consonants
(generate-rules myanmar-consonants)

;; Independent Vowels
(generate-rules myanmar-independent-vowels)

;; Dependent Vowels
(generate-rules myanmar-dependent-vowels)

;; Various Signs
(generate-rules myanmar-various-signs)

;; Consonant Signs
(generate-rules myanmar-consonant-signs)

;; Digits
(generate-rules myanmar-digits)

;; Punctuations
(generate-rules myanmar-punctuations)

;; Custom Rules
(generate-rules myanmar-custom-rules)
