;;; Example of use of cl-ofdm-transfer's API to make a server receiving
;;; messages from clients and sending them back in reverse order.
;;;
;;; Copyright 2021 Guillaume LE VAILLANT
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(asdf:load-system "ofdm-transfer")

(defpackage :echo-server
  (:use :cl)
  (:export #:client
           #:server))
(in-package :echo-server)

(defparameter *radio-driver* "driver=hackrf")
(defparameter *sample-rate* 4000000)
(defparameter *transmission-gain* 36)
(defparameter *reception-gain* 60)
(defparameter *frequency-offset* 100000)
(defparameter *bit-rate* 9600)
(defparameter *subcarrier-modulation* "qpsk")
(defparameter *subcarriers* 64)
(defparameter *cyclic-prefix-length* 16)
(defparameter *taper-length* 4)
(defparameter *inner-fec* "rs8")
(defparameter *outer-fec* "rs8")

(defun transmit (data frequency)
  (ofdm-transfer:transmit-buffer data
                                 :radio-driver *radio-driver*
                                 :sample-rate *sample-rate*
                                 :gain *transmission-gain*
                                 :frequency frequency
                                 :frequency-offset *frequency-offset*
                                 :bit-rate *bit-rate*
                                 :subcarrier-modulation *subcarrier-modulation*
                                 :subcarriers *subcarriers*
                                 :cyclic-prefix-length *cyclic-prefix-length*
                                 :taper-length *taper-length*
                                 :inner-fec *inner-fec*
                                 :outer-fec *outer-fec*
                                 :final-delay 1))

(defun receive (callback frequency)
  (ofdm-transfer:receive-callback callback
                                  :radio-driver *radio-driver*
                                  :sample-rate *sample-rate*
                                  :gain *reception-gain*
                                  :frequency frequency
                                  :frequency-offset *frequency-offset*
                                  :bit-rate *bit-rate*
                                  :subcarrier-modulation *subcarrier-modulation*
                                  :subcarriers *subcarriers*
                                  :cyclic-prefix-length *cyclic-prefix-length*
                                  :taper-length *taper-length*
                                  :inner-fec *inner-fec*
                                  :outer-fec *outer-fec*))

(defun receive-1 (frequency)
  (let* ((received nil)
         (callback (lambda (data)
                     (setf received data)
                     (ofdm-transfer:stop-all-transfers))))
    (receive callback frequency)
    received))

(defun process-request (data)
  (reverse data))

(defun server (frequency)
  (unwind-protect
       (loop :do
         (let* ((data (receive-1 frequency))
                (processed (process-request data)))
           (format t "~%Received: ~a~%" data)
           (sleep 1)
           (format t "Sending: ~a~%" processed)
           (transmit processed frequency)))
    (ofdm-transfer:stop-all-transfers)))

(defun client (frequency data)
  (unwind-protect
       (progn
         (format t "~%Sending: ~a~%" data)
         (transmit data frequency)
         (let ((data (receive-1 frequency)))
           (format t "Received: ~a~%" data)
           data))
    (ofdm-transfer:stop-all-transfers)))
