;; bbdb-syncml-debug.el: This files contain debug utilities for the bbdb-syncml package.
;; $Id: bbdb-syncml-debug.el,v 1.1 2003/02/06 14:14:22 joergenb Exp $

;; Copyright (C) 2003 Jørgen Binningsbø 

;; Author: Jørgen Binningsbø <jb@pvv.org>
;; Maintainer: Jørgen Binningsbø <jb@pvv.org>
;; Version: 
;; Created: Jan 10 2003
;; Keywords: bbdb syncml xml network
;; URL: http://savannah.nongnu.org/projects/bbdb-syncml

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

;; Commentary: This was mostly taken from the URL package.


(defcustom bbdb-syncml-debug nil
  "*What types of debug messages from the BBDB-SYNCML library to show.
Debug messages are logged to the *BBDB-SYNCML-DEBUG* buffer.

If t, all messages will be logged.
If a number, all messages will be logged, as well shown via `message'.
If a list, it is a list of the types of messages to be logged."
  :type '(choice (const :tag "none" nil)
								 (const :tag "all" t)
								 (checklist :tag "custom"
														(const :tag "HTTP" :value http)
														(const :tag "DAV" :value dav)
														(const :tag "General" :value retrieval)
														(const :tag "Filename handlers" :value handlers)
														(symbol :tag "Other")))
  :group 'bbdb-syncml-hairy)

;;;###autoload
(defun bbdb-syncml-debug (tag &rest args)
  (if quit-flag
      (error "Interrupted!"))
  (if (or (eq bbdb-syncml-debug t)
					(numberp bbdb-syncml-debug)
					(and (listp bbdb-syncml-debug) (memq tag bbdb-syncml-debug)))
      (save-excursion
				(set-buffer (get-buffer-create "*BBDB-SYNCML-DEBUG*"))
				(goto-char (point-max))
				(insert (current-time-string) ": " (symbol-name tag) " -> " (apply 'format args) "\n")
				(if (numberp bbdb-syncml-debug)
						(apply 'message args)))))


(provide 'bbdb-syncml-debug)