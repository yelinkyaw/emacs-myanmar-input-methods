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
   ("က" . "u")
   ("ခ" . "c")
   ("ဂ" . ":")
   ("ဃ" . "C")
   ("င" . "i")
   ("စ" . "p")
   ("ၑ" . "`")
   ("ဆ" . "q")
   ("ဇ" . "Z")
   ("ဈ" . "Q")
   ("ည" . "n")
   ("ဉ" . "N")
   ("ဋ" . "#")
   ("ဌ" . "X")
   ("ဍ" . "!")
   ("ဎ" . "~")
   ("ဏ" . "P")
   ("တ" . "w")
   ("ထ" . "x")
   ("ဒ" . "K")
   ("ဓ" . "L")
   ("န" . "e")
   ("ပ" . "y")
   ("ဖ" . "z")
   ("ဗ" . "A")
   ("ဘ" . "b")
   ("မ" . "r")
   ("ယ" . "B")
   ("ရ" . "&")
   ("လ" . "v")
   ("ဝ" . "W")
   ("သ" . "o")
   ("ဿ" . "O")
   ("ဟ" . "[")
   ("ဠ" . "V")
   ("အ" . "t")))

(defconst myanmar-independent-vowels
 '(
   ("အ" . "t")
   ("ဣ" . "E")
   ("ဤ" . "T")
   ("ဥ" . "U")
   ("ဦ" . "M")
   ("ဧ" . "{")
   ("ဩ" . "]")
   ("ဪ" . "}")))

(defconst myanmar-dependent-vowels
 '(
   ("ါ" . "g")
   ("ာ" . "m")
   ("ိ" . "d")
   ("ီ" . "D")
   ("ု" . "k")
   ("ူ" . "l")
   ("ေ" . "a")
   ("ဲ" . "J")))

(defconst myanmar-various-signs
 '(
   ("ံ" . "H")
   ("့" . "h")
   ("း" . ";")
   ("္" . "F")
   ("်" . "f")
   ("၌" . "Y")
   ("၍" . "I")
   ("၎င်း" . "R")
   ("၏" . "\\")))

(defconst myanmar-consonant-signs
 '(
   ("ျ" . "s")
   ("ြ" . "j")
   ("ွ" . "G")
   ("ှ" . "S")))

(defconst myanmar-digits
 '(
   ("၀" . "0")
   ("၁" . "1")
   ("၂". "2")
   ("၃" . "3")
   ("၄" . "4")
   ("၅" . "5")
   ("၆" . "6")
   ("၇" . "7")
   ("၈" . "8")
   ("၉" . "9")))

(defconst myanmar-punctuations
 '(
   ("၊" . "<")
   ("။" . ">")))

(defconst myanmar-custom-rules
 '(
   ("ုံ" . "Hk")
   ("ဈ" . "ps")
   ("ဩ" . "oj")
   ("ဪ" . "ojamf")
   ))

;; Get Character Item from Myanmar Characters
(defun get-character-item(key)
  (if (assoc key myanmar-consonants)
      (assoc key myanmar-consonants)
    (if (assoc key myanmar-independent-vowels)
	(assoc key myanmar-independent-vowels)
      (if (assoc key myanmar-dependent-vowels)
	  (assoc key myanmar-dependent-vowels)
	(if (assoc key myanmar-various-signs)
	    (assoc key myanmar-various-signs)
	  (if (assoc key myanmar-consonant-signs)
	      (assoc key myanmar-consonant-signs)
	    (if (assoc key myanmar-digits)
		(assoc key myanmar-digits)
	      (if (assoc key myanmar-punctuations)
		  (assoc key myanmar-punctuations)
		nil))))))))

;; Custom Pre to Post Rules Generator
(defun create-pre-to-post-custom-rules(lists pre post)
  ;;Generate Prefix String
  (setq pre_key "")
  (setq pre_value "")
  (dotimes (i (length pre))
    (setq char (aref pre i))
    (setq item (get-character-item char))
    (if item
	(progn
	  (setq pre_key (concat pre_key (car item)))
	  (setq pre_value (concat pre_value (cdr item))))))
  
  ;;Generate Postfix String
  (setq post_key "")
  (setq post_value "")
  (dotimes (i (length post))
    (setq char (aref post i))
    (setq item (get-character-item char))
    (if item
	(progn
	  (setq post_key (concat post_key (car item)))
	  (setq post_value (concat post_value (cdr item))))))

  ;;Create Rules
  (let (rules)
    (while lists
      ;; Get Item
      (setq item (car lists))
      (setq key (car item))
      (setq value (cdr item))
      
      ;; Set Prefix and Postfix
      (setq key (concat key post_key pre_key))
      (setq value (concat pre_value value post_value))

      ;; Create Rules Item
      (add-to-list 'rules `(,key . ,value))
      (setq lists (cdr lists))
      )
    rules
    )
  )

;; Rules Generator
(defun generate-rules(lists)
  (while lists
    (setq item (car lists))
    (setq key (car item))
    (setq value (cdr item))
    (quail-defrule value (vector key))
    (setq lists (cdr lists)))
  )

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

;; Create Custom Rules for "ေ + consonant"
(generate-rules (create-pre-to-post-custom-rules myanmar-consonants ["ေ"] []))

;; Create Custom Rules for "ေ + consonant + ျ"
(generate-rules (create-pre-to-post-custom-rules myanmar-consonants ["ေ"] ["ျ"]))

;; Create Custom Rules for "ေ + consonant + ြ"
(generate-rules (create-pre-to-post-custom-rules myanmar-consonants ["ေ"] ["ြ"]))

;; Create Custom Rules for "ေ + consonant + ွ"
(generate-rules (create-pre-to-post-custom-rules myanmar-consonants ["ေ"] ["ွ"]))

;; Create Custom Rules for "ေ + consonant + ှ"
(generate-rules (create-pre-to-post-custom-rules myanmar-consonants ["ေ"] ["ှ"]))

;; Create Custom Rules for "ေ + consonant + ျ + ွ"
(generate-rules (create-pre-to-post-custom-rules myanmar-consonants ["ေ"] ["ျ" "ွ"]))

;; Create Custom Rules for "ေ + consonant + ြ + ွ"
(generate-rules (create-pre-to-post-custom-rules myanmar-consonants ["ေ"] ["ြ" "ွ"]))

;; Create Custom Rules for "ေ + consonant + ျ + ှ"
(generate-rules (create-pre-to-post-custom-rules myanmar-consonants ["ေ"] ["ျ" "ှ"]))

;; Create Custom Rules for "ေ + consonant + ြ + ှ"
(generate-rules (create-pre-to-post-custom-rules myanmar-consonants ["ေ"] ["ြ" "ှ"]))

;; Create Custom Rules for "ေ + consonant + ွ + ှ"
(generate-rules (create-pre-to-post-custom-rules myanmar-consonants ["ေ"] ["ွ" "ှ"]))

;; Create Custom Rules for "ေ + consonant + ျ + ွ + ှ"
(generate-rules (create-pre-to-post-custom-rules myanmar-consonants ["ေ"] ["ျ" "ွ" "ှ"]))

;; Create Custom Rules for "ေ + consonant + ြ + ွ + ှ"
(generate-rules (create-pre-to-post-custom-rules myanmar-consonants ["ေ"] ["ြ" "ွ" "ှ"]))

;; Custom Rules
(generate-rules myanmar-custom-rules)
