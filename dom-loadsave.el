;; dom-loadsave.el -- An minimalist implementation of the DOM Level 3 
;;                    Load and Save extension.
;; $Id: dom-loadsave.el,v 1.4 2003/11/22 23:42:27 joergenb Exp $

;; Copyright (C) 2003 Jørgen Binningsbø 

;; Author: Jørgen Binningsbø <jb at pvv.org>
;; Maintainer: Jørgen Binningsbø <jb at pvv.org>
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

(defconst dom-loadsave-indent 2)

(defun dom-node-write-to-string (node &optional base-indent-level)
  (setq dom-loadsave-indent-level 1)
  (let* ((dummy (dom-node-write-to-string-inner node)))
    (setq dom-loadsave-indent-level 1)
    dummy))

(defun dom-node-write-to-string-inner (node)
  "return a string with the XML-representation of NODE and it's 
child nodes."
  (cond    
   ;; a DOCUMENT node hands processing further to it's ELEMENT 
   ((dom-document-p node)
    (dom-node-write-to-string-inner (dom-document-element node)))
   
   ;; an ELEMENT node basically prints the start tag, processes all children, 
   ;; and the prints the end tag. 
   ((dom-element-p node)
    (concat 
     ;; first, determine indent
     ;;(number-to-string dom-loadsave-indent-level)
     (make-string (* dom-loadsave-indent-level dom-loadsave-indent) (string-to-char " "))
     ;; add the tag name
     "<" 
     (if (symbolp (dom-node-name node))
	 (symbol-name (dom-node-name node))
       (dom-node-name node))
     ;; check for attributes
     (if (dom-node-has-attributes node)	 
	 (let (res)
	   (dolist (innernode (dom-node-attributes node) res)
	     (if (dom-attr-p innernode)
		 (push (concat (if (symbolp (dom-node-name innernode))
				   (symbol-name (dom-node-name innernode))
				 (dom-node-name innernode))
			       "='" 
			       (if (symbolp (dom-node-value innernode))			
				   (symbol-name (dom-node-value innernode))
				 (dom-node-value innernode))
			       "'")
		       res)))
	   (mapconcat 'concat (cons " " res) " ")))
     ;;	 "yes-attribute")
     ;;	 (dolist (innernode (dom-node-attributes node))
     ;;	   (if (dom-attr-p innernode)
     ;;	       (concat innernode-name "='" innernode-value "'"))))

     (if (dom-node-has-child-nodes node)
	 (progn 
	   (concat 
	    ">"
	    (if (dom-element-p (dom-node-first-child node))
		"\n")
	    ;; if the node has any children, process them first
	    (if (dom-node-has-child-nodes node)
		(progn 
		  (setq dom-loadsave-indent-level (+ 1 dom-loadsave-indent-level))
		  (mapconcat 'dom-node-write-to-string-inner (dom-node-child-nodes node) ""))
	      "")
	    
	    ;; add the closing tag
	    ;; if the node's first child was an element-node, then we're supposed to write the end tag on a new line,
	    ;; and unindented one step according to the level of the children.
	    (if (dom-element-p (dom-node-first-child node))
		(progn
		  ;;	   (setq dom-loadsave-indent-level (1- dom-loadsave-indent-level))
		  (make-string (* (1- dom-loadsave-indent-level) dom-loadsave-indent) (string-to-char " "))))
	    "</" 
	    (progn 
	      (if (>= dom-loadsave-indent-level 1)
		  (setq dom-loadsave-indent-level (1- dom-loadsave-indent-level))
		(setq dom-loadsave 1))
	      (if (symbolp (dom-node-name node))
		  (symbol-name (dom-node-name node))
		(dom-node-name node)))
	    ">\n"))
       "/>\n")))

   ;; a TEXT node simply returns it's value.
   ((dom-text-p node)
    (if (numberp (dom-node-value node))
	(number-to-string (dom-node-value node))
      (dom-node-value node)))
   
   ;; insert other node types here. (none applicable, attribute nodes are dealt with above).
   );; end of cond
  )
  

(provide 'dom-loadsave)