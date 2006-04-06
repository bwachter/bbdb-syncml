;;; bbdb-syncml-vcard.el -- VCard routines for the bbdb-syncml package.
;; $Id: bbdb-syncml-vcard.el,v 1.2 2006/04/06 20:37:05 joergenb Exp $

;; Copyright (C) 2003-2004 Jørgen Binningsbø 

;; Author: Jørgen Binningsbø <jb@pvv.org>
;; Maintainer: Jørgen Binningsbø <jb@pvv.org>
;; Version: 
;; Created: Jan 25 2004
;; Keywords: syncml xml network
;; URL: 

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a fork of bbdb-vcard-export located in the contrib/ folder of the bbdb distribution.


(defun bbdb-syncml-vcard-get-bbdb-record-as-vcard-string (record)
  "Returns a VCARD2.1 formatted version of RECORD as a string"
  (let (
	(name (bbdb-record-name record))
	(first-name (elt record 0))
	(last-name (elt record 1))
	(company (elt record 3))
	(notes (bbdb-record-notes record))
	(phones (bbdb-record-phones record))
	(addresses (bbdb-record-addresses record))
	(luid (bbdb-record-getprop record 'luid))
	(net (bbdb-record-net record)))
    (concat 
     "BEGIN:VCARD\n"
     "VERSION:2.1\n"
     (concat "FN:" name "\n")
     (concat "N:" last-name ";" first-name "\n"
	     (if company (concat "ORG:" company "\n"))
	     (if notes (concat "NOTE:" notes "\n")))
     
     (concat "UID:" luid "\n")
     (if phones
	 (let (res)
	   (dolist (phone phones res)
	     (push (concat "TEL;" (bbdb-syncml-vcard-lookup-location-mapping
					(bbdb-phone-location phone))
			   ":"
			   (bbdb-phone-string phone) "\n")
		   res))
	   (mapconcat 'concat (cons "" res) "")))
     
     
     (if addresses
	 (while addresses
	   (concat (bbdb-vcard-export-address-string (car addresses)) "\n")
	   (setq addresses (cdr addresses))))
     (if net
	 (while net
	   (concat "EMAIL;TYPE=internet:" (car net) "\n")
	   (setq net (cdr net))))
     (concat "END:VCARD\n"))))


(defconst bbdb-syncml-vcard-phone-location-mappings
  '(("Mobile" "CELL")
    ("mobile" "CELL")
    ("mobil"  "CELL")
    ("Mobil"  "CELL")
    ("Home"   "HOME")
    ("home"   "HOME")
    ("heime"  "HOME")
    ("Heime"  "HOME")
    ("Voice"  "VOICE")
    ("voice"  "VOICE")
    ("work"   "WORK")
    ))

(defun bbdb-syncml-vcard-lookup-location-mapping (location)
  "Returns the text associated with the BBDB phone location LOCATION."

  (let (tmp (car (cdr (assoc location bbdb-syncml-vcard-phone-location-mappings))))
    (if (null tmp)
	(setq tmp "VOICE"))
    tmp))



(provide 'bbdb-syncml-vcard)	 
		  