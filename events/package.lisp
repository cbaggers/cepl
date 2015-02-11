;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; package.lisp --- The event package
;;;;
;;;; Copyright (c) 2013, Nikhil Shetty <nikhil.j.shetty@gmail.com>
;;;;   All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;  o Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;;  o Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;  o Neither the name of the author nor the names of the contributors may
;;;;    be used to endorse or promote products derived from this software
;;;;    without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;; ==========================================================================
(in-package #:cl-user)

(defpackage :cepl.events
  (:use :cl :cepl-utils :cells)
  (:nicknames :evt)  
  (:export :event
           :event-cell
           :map-evt
           :merge-evt
           :filter-evt
           :all-events
           :*map-evt*
           :*merge-evt*
           :*filter-evt*
           :*all-events*
           :observe
           :undefobserver
           :def-event-node))

(defpackage :cepl.events.sdl
  (:use :cl :cepl-utils :cepl.events :cells)
  (:nicknames :evt.sdl)
  (:export :pump-events
           :case-events

           :will-quit
           :window
           :mouse-scroll
           :mouse-button
           :mouse-motion
           :key
           :terminal

           :all-events
           :mouse
           :sys
           :window
           :keyboard
           :*all-events*
           :*mouse*
           :*sys*
           :*window*
           :*keyboard*

           :action
           :button
           :clicks
           :delta
           :etype
           :id
           :key
           :pos
           :repeat
           :source-id
           :state
           :timestamp
           :vec
           :data
           
           :button-state
           :key-state))
