;;; myansan-input-method.el --- Myanmar Emacs Input Method with Myan San Keyboard Layout

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a Emacs input method for Myanmar with Myan San Layout

;;; Installation:

;; To install, just drop this file into a directory in your
;; `load-path' and (optionally) byte-compile it. And add:
;;
;;    (require 'myanmar-myansan)
;;
;; to your .emacs file.

(provide 'myanmar-myansan)
(require 'quail)
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
(quail-define-rules
 ("1" ["၁"])
 ("2" ["၂"])
 ("3" ["၃"])
 ("4" ["၄"])
 ("5" ["၅"])
 ("6" ["၆"])
 ("7" ["၇"])
 ("8" ["၈"])
 ("9" ["၉"])
 ("0" ["၀"])
 ("q" ["ဆ"])
 ("w" ["တ"])
 ("e" ["န"])
 ("r" ["မ"])
 ("t" ["အ"])
 ("y" ["ပ"])
 ("u" ["က"])
 ("i" ["င"])
 ("o" ["သ"])
 ("p" ["စ"])
 ("[" ["ဟ"])
 ("]" ["ဩ"])
 ("\\" ["၏"])
 ("a" ["ေ"])
 ("s" ["ျ"])
 ("d" ["ိ"])
 ("f" ["်"])
 ("g" ["ါ"])
 ("h" ["့"])
 ("j" ["ြ"])
 ("k" ["ု"])
 ("l" ["ူ"])
 (";" ["း"])
 ("z" ["ဖ"])
 ("x" ["ထ"])
 ("c" ["ခ"])
 ("v" ["လ"])
 ("b" ["ဘ"])
 ("n" ["ည"])
 ("m" ["ာ"])
 ("," ["ယ"])
 ("/" ["။"])
 ("~" ["ဎ"])
 ("!" ["ဍ"])
 ("@" ["ဏ္ဍ"])
 ("#" ["ဋ"])
 ("^" ["/"])
 ("&" ["ရ"])
 ("*" ["ဂ"])
 ("_" ["×"])
 ("Q" ["ဈ"])
 ("W" ["ဝ"])
 ("E" ["ဣ"])
 ("R" ["၎င်း"])
 ("T" ["ဤ"])
 ("Y" ["၌"])
 ("U" ["ဥ"])
 ("I" ["၍"])
 ("O" ["ဿ"])
 ("P" ["ဏ"])
 ("{" ["ဧ"])
 ("}" ["ဪ"])
 ("|" ["ဋ္ဌ"])
 ("A" ["ဗ"])
 ("S" ["ှ"])
 ("D" ["ီ"])
 ("F" ["္"])
 ("G" ["ွ"])
 ("H" ["ံ"])
 ("J" ["ဲ"])
 ("K" ["ဒ"])
 ("L" ["ဓ"])
 ("Z" ["ဇ"])
 ("X" ["ဌ"])
 ("C" ["ဃ"])
 ("V" ["ဠ"])
 ("N" ["ဉ"])
 ("M" ["ဦ"])
 ("?" ["၊"])
)
