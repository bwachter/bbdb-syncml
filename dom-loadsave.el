;; dom-loadsave.el -- An minimalist implementation of the DOM Level 3 
;;                    Load and Save extension.
;; $Id: dom-loadsave.el,v 1.1 2003/02/06 14:14:22 joergenb Exp $

;; Copyright (C) 2003 Jørgen Binningsbø 

;; Author: Jørgen Binningsbø <jb@pvv.org>
;; Maintainer: Jørgen Binningsbø <jb@pvv.org>
;; Version: 
;; Created: Jan 10 2003
;; Keywords: xml
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

;;; Commentary

;; This package require the 'dom.el' file of Alex Schroeder, normally 
;; located at:
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?XmlParser

(require 'dom)

(defun dom-node-write-to-string (node)
  "return a string with the XML-representation of NODE and it's 
child nodes."
	;; check the type of NODE
	(cond 

	 ;; a DOCUMENT node hands processing further to it's ELEMENT 
	 ((dom-document-p node)
		(dom-node-write-to-string (dom-document-element node)))

	 ;; an ELEMENT node basically prints the start tag, processes all children, 
	 ;; and the prints the end tag. 
	 ((dom-element-p node)
		(concat 
		 ;; first, add the tag name
		 "<" 
		 (symbol-name (dom-node-name node))
		 ;; TODO: add attribute support here.
		 ">"
		 ;; if the node has any children, process them first
		 (if (dom-node-has-child-nodes node)
				 (mapconcat 'dom-node-write-to-string (dom-node-child-nodes node) "")
			 "")
		 ;; add the closing tag
		 "</" 
		 (symbol-name (dom-node-name node)) 
		 ">"))
	 
	 ;; a TEXT node simply returns it's value.
	 ((dom-text-p node)
		(dom-node-value node))
	 );; end of cond
	);; end of defun

(provide 'dom-loadsave)