;;; myanmar-input-methods.el --- Emacs Input Method for Myanmar

;; Copyright (C) 2015 Ye Lin Kyaw
;; Author: Ye Lin Kyaw <yelinkyaw@gmail.com>
;; Created: 06 Jul 2015
;; Last-Updated: 16 Jul 2015
;; Version: 0.0.2
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

;; This is an Emacs input methods for Myanmar

;;; Installation:

;; To install, just drop this file into a directory in your
;; `load-path' and (optionally) byte-compile it. And add:
;;
;;    (require 'myanmar)
;;
;; to your .emacs file.

(provide 'myanmar)
(require 'quail)

;; Get Character Item from Characters List
(defun get-character-item(key character-lists)
  (if (car character-lists)
      (if (assoc key (car character-lists))
	  (assoc key (car character-lists))
	(get-character-item key (cdr character-lists)))
    nil))

;; Custom Pre to Post Rules Generator
(defun create-pre-to-post-custom-rules(lists pre post)
  ;;Generate Prefix String
  (setq pre_key "")
  (setq pre_value "")
  (dotimes (i (length pre))
    (setq char (aref pre i))
    (setq item (get-character-item char myansan-characters))
    (if item
	(progn
	  (setq pre_key (concat pre_key (car item)))
	  (setq pre_value (concat pre_value (cdr item))))))
  
  ;; Generate Postfix String
  (setq post_key "")
  (setq post_value "")
  (dotimes (i (length post))
    (setq char (aref post i))
    (setq item (get-character-item char myansan-characters))
    (if item
	(progn
	  (setq post_key (concat post_key (car item)))
	  (setq post_value (concat post_value (cdr item))))))

  ;; Create Rules
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

;; MyanSan Layout
(quail-define-package
 "myanmar-myansan"
 "Myanmar"
 "မြန်စံ"
 nil
 "Myan San Keyboard Layout"
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

(defconst myansan-consonants
 '(
   ("က" . "u")
   ("ခ" . "c")
   ("ဂ" . "*")
   ("ဃ" . "C")
   ("င" . "i")
   ("စ" . "p")
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
   ("ယ" . ",")
   ("ရ" . "&")
   ("လ" . "v")
   ("ဝ" . "W")
   ("သ" . "o")
   ("ဿ" . "O")
   ("ဟ" . "[")
   ("ဠ" . "V")
   ("အ" . "t")))

(defconst myansan-independent-vowels
 '(
   ("အ" . "t")
   ("ဣ" . "E")
   ("ဤ" . "T")
   ("ဥ" . "U")
   ("ဦ" . "M")
   ("ဧ" . "{")
   ("ဩ" . "]")
   ("ဪ" . "}")))

(defconst myansan-dependent-vowels
 '(
   ("ါ" . "g")
   ("ာ" . "m")
   ("ိ" . "d")
   ("ီ" . "D")
   ("ု" . "k")
   ("ူ" . "l")
   ("ေ" . "a")
   ("ဲ" . "J")))

(defconst myansan-various-signs
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

(defconst myansan-consonant-signs
 '(
   ("ျ" . "s")
   ("ြ" . "j")
   ("ွ" . "G")
   ("ှ" . "S")))

(defconst myansan-digits
 '(
   ("၀" . "0")
   ("၁" . "1")
   ("၂" . "2")
   ("၃" . "3")
   ("၄" . "4")
   ("၅" . "5")
   ("၆" . "6")
   ("၇" . "7")
   ("၈" . "8")
   ("၉" . "9")))

(defconst myansan-punctuations
 '(
   ("၊" . "?")
   ("။" . "/")))

(defconst myansan-custom-rules
 '(
   ("" . "B")
   ("/" . "^")
   ("×" . "_")
   ("ဏ္ဍ" . "@")
   ("ုံ" . "Hk")
   ("ဈ" . "ps")
   ("ဩ" . "oj")
   ("ဪ" . "aojmf")
   ))

(defconst myansan-characters
  `(,myansan-consonants
    ,myansan-independent-vowels
    ,myansan-dependent-vowels
    ,myansan-various-signs
    ,myansan-consonant-signs
    ,myansan-digits
    ,myansan-punctuations)
  )



;; Generate Rules
;; Consonants
(generate-rules myansan-consonants)

;; Independent Vowels
(generate-rules myansan-independent-vowels)

;; Dependent Vowels
(generate-rules myansan-dependent-vowels)

;; Various Signs
(generate-rules myansan-various-signs)

;; Consonant Signs
(generate-rules myansan-consonant-signs)

;; Digits
(generate-rules myansan-digits)

;; Punctuations
(generate-rules myansan-punctuations)

;; Create Custom Rules for "ေ + consonant"
(generate-rules (create-pre-to-post-custom-rules myansan-consonants ["ေ"] []))

;; Create Custom Rules for "ေ + consonant + ျ"
(generate-rules (create-pre-to-post-custom-rules myansan-consonants ["ေ"] ["ျ"]))

;; Create Custom Rules for "ေ + consonant + ြ"
(generate-rules (create-pre-to-post-custom-rules myansan-consonants ["ေ"] ["ြ"]))

;; Create Custom Rules for "ေ + consonant + ွ"
(generate-rules (create-pre-to-post-custom-rules myansan-consonants ["ေ"] ["ွ"]))

;; Create Custom Rules for "ေ + consonant + ှ"
(generate-rules (create-pre-to-post-custom-rules myansan-consonants ["ေ"] ["ှ"]))

;; Create Custom Rules for "ေ + consonant + ျ + ွ"
(generate-rules (create-pre-to-post-custom-rules myansan-consonants ["ေ"] ["ျ" "ွ"]))

;; Create Custom Rules for "ေ + consonant + ြ + ွ"
(generate-rules (create-pre-to-post-custom-rules myansan-consonants ["ေ"] ["ြ" "ွ"]))

;; Create Custom Rules for "ေ + consonant + ျ + ှ"
(generate-rules (create-pre-to-post-custom-rules myansan-consonants ["ေ"] ["ျ" "ှ"]))

;; Create Custom Rules for "ေ + consonant + ြ + ှ"
(generate-rules (create-pre-to-post-custom-rules myansan-consonants ["ေ"] ["ြ" "ှ"]))

;; Create Custom Rules for "ေ + consonant + ွ + ှ"
(generate-rules (create-pre-to-post-custom-rules myansan-consonants ["ေ"] ["ွ" "ှ"]))

;; Create Custom Rules for "ေ + consonant + ျ + ွ + ှ"
(generate-rules (create-pre-to-post-custom-rules myansan-consonants ["ေ"] ["ျ" "ွ" "ှ"]))

;; Create Custom Rules for "ေ + consonant + ြ + ွ + ှ"
(generate-rules (create-pre-to-post-custom-rules myansan-consonants ["ေ"] ["ြ" "ွ" "ှ"]))

;; Custom Rules
(generate-rules myansan-custom-rules)

;; Myanmar3 Layout
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


(defconst myanmar3-consonants
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

(defconst myanmar3-independent-vowels
 '(
   ("အ" . "t")
   ("ဣ" . "E")
   ("ဤ" . "T")
   ("ဥ" . "U")
   ("ဦ" . "M")
   ("ဧ" . "{")
   ("ဩ" . "]")
   ("ဪ" . "}")))

(defconst myanmar3-dependent-vowels
 '(
   ("ါ" . "g")
   ("ာ" . "m")
   ("ိ" . "d")
   ("ီ" . "D")
   ("ု" . "k")
   ("ူ" . "l")
   ("ေ" . "a")
   ("ဲ" . "J")))

(defconst myanmar3-various-signs
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

(defconst myanmar3-consonant-signs
 '(
   ("ျ" . "s")
   ("ြ" . "j")
   ("ွ" . "G")
   ("ှ" . "S")))

(defconst myanmar3-digits
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

(defconst myanmar3-punctuations
 '(
   ("၊" . "<")
   ("။" . ">")))

(defconst myanmar3-custom-rules
 '(
   ("ုံ" . "Hk")
   ("ဈ" . "ps")
   ("ဩ" . "oj")
   ("ဪ" . "ojamf")
   ))

;; Generate Rules
;; Consonants
(generate-rules myanmar3-consonants)

;; Independent Vowels
(generate-rules myanmar3-independent-vowels)

;; Dependent Vowels
(generate-rules myanmar3-dependent-vowels)

;; Various Signs
(generate-rules myanmar3-various-signs)

;; Consonant Signs
(generate-rules myanmar3-consonant-signs)

;; Digits
(generate-rules myanmar3-digits)

;; Punctuations
(generate-rules myanmar3-punctuations)

;; Create Custom Rules for "ေ + consonant"
(generate-rules (create-pre-to-post-custom-rules myanmar3-consonants ["ေ"] []))

;; Create Custom Rules for "ေ + consonant + ျ"
(generate-rules (create-pre-to-post-custom-rules myanmar3-consonants ["ေ"] ["ျ"]))

;; Create Custom Rules for "ေ + consonant + ြ"
(generate-rules (create-pre-to-post-custom-rules myanmar3-consonants ["ေ"] ["ြ"]))

;; Create Custom Rules for "ေ + consonant + ွ"
(generate-rules (create-pre-to-post-custom-rules myanmar3-consonants ["ေ"] ["ွ"]))

;; Create Custom Rules for "ေ + consonant + ှ"
(generate-rules (create-pre-to-post-custom-rules myanmar3-consonants ["ေ"] ["ှ"]))

;; Create Custom Rules for "ေ + consonant + ျ + ွ"
(generate-rules (create-pre-to-post-custom-rules myanmar3-consonants ["ေ"] ["ျ" "ွ"]))

;; Create Custom Rules for "ေ + consonant + ြ + ွ"
(generate-rules (create-pre-to-post-custom-rules myanmar3-consonants ["ေ"] ["ြ" "ွ"]))

;; Create Custom Rules for "ေ + consonant + ျ + ှ"
(generate-rules (create-pre-to-post-custom-rules myanmar3-consonants ["ေ"] ["ျ" "ှ"]))

;; Create Custom Rules for "ေ + consonant + ြ + ှ"
(generate-rules (create-pre-to-post-custom-rules myanmar3-consonants ["ေ"] ["ြ" "ှ"]))

;; Create Custom Rules for "ေ + consonant + ွ + ှ"
(generate-rules (create-pre-to-post-custom-rules myanmar3-consonants ["ေ"] ["ွ" "ှ"]))

;; Create Custom Rules for "ေ + consonant + ျ + ွ + ှ"
(generate-rules (create-pre-to-post-custom-rules myanmar3-consonants ["ေ"] ["ျ" "ွ" "ှ"]))

;; Create Custom Rules for "ေ + consonant + ြ + ွ + ှ"
(generate-rules (create-pre-to-post-custom-rules myanmar3-consonants ["ေ"] ["ြ" "ွ" "ှ"]))

;; Custom Rules
(generate-rules myanmar3-custom-rules)
