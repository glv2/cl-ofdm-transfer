;;; This file is part of cl-ofdm-transfer
;;; Copyright 2021 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defpackage :ofdm-transfer
  (:use :cl)
  (:import-from :cffi
                #:defcfun
                #:define-foreign-library
                #:null-pointer
                #:null-pointer-p
                #:use-foreign-library)
  (:export #:free-transfer
           #:make-transfer
           #:start-transfer
           #:stop-all-transfers
           #:stop-transfer
           #:transmit
           #:receive
           #:verbosity))

(in-package :ofdm-transfer)


;;;
;;; Bindings to libofdm-transfer
;;;

(define-foreign-library ofdm-transfer
  (:unix (:or "libofdm-transfer.so"
              "libofdm-transfer.so.1"))
  (t (:default "libofdm-transfer")))

(use-foreign-library ofdm-transfer)

(defcfun ("ofdm_transfer_set_verbose" ofdm-transfer-set-verbose) :void
  "Set the verbosity level."
  (v :unsigned-char))

(defcfun ("ofdm_transfer_is_verbose" ofdm-transfer-is-verbose) :unsigned-char
  "Get the verbosity level.")

(defcfun ("ofdm_transfer_create" ofdm-transfer-create) :pointer
  "Initialize a new transfer."
  (radio-driver :string)
  (emit :unsigned-char)
  (file :string)
  (sample-rate :unsigned-long)
  (bit-rate :unsigned-int)
  (frequency :unsigned-long)
  (frequency-offset :long)
  (gain :unsigned-int)
  (ppm :float)
  (subcarrier-modulation :string)
  (subcarriers :unsigned-int)
  (cyclic-prefix-length :unsigned-int)
  (taper-length :unsigned-int)
  (inner-fec :string)
  (outer-fec :string)
  (id :string)
  (dump (:pointer :char)))

(defcfun ("ofdm_transfer_free" ofdm-transfer-free) :void
  "Cleanup after a finished transfer."
  (transfer :pointer))

(defcfun ("ofdm_transfer_start" ofdm-transfer-start) :void
  "Start a transfer and return when finished."
  (transfer :pointer))

(defcfun ("ofdm_transfer_stop" ofdm-transfer-stop) :void
  "Interrupt a transfer."
  (transfer :pointer))

(defcfun ("ofdm_transfer_stop_all" ofdm-transfer-stop-all) :void
  "Interrupt a transfer.")

(defcfun ("ofdm_transfer_print_available_radios"
          ofdm-transfer-print-available-radios)
  :void
  "Print list of detected software defined radios.")

(defcfun ("ofdm_transfer_print_available_subcarrier_modulations"
          ofdm-transfer-print-available-subcarrier-modulations)
  :void
  "Print list of supported subcarrier modulations.")

(defcfun ("ofdm_transfer_print_available_forward_error_codes"
          ofdm-transfer-print-available-forward-error-codes)
  :void
  "Print list of supported forward error codes.")


;;;
;;; Lisp API
;;;

(defun verbosity ()
  "Get the verbosity level."
  (ofdm-transfer-is-verbose))

(defun (setf verbosity) (value)
  "Set the verbosity level."
  (ofdm-transfer-set-verbose value))

(defun make-transfer (file emit
                      &key
                        (radio-driver "") (sample-rate 2000000) (bit-rate 38400)
                        (frequency 434000000) (frequency-offset 0) (gain 0)
                        (ppm 0.0) (subcarrier-modulation "qpsk")
                        (subcarriers 64) (cyclic-prefix-length 16)
                        (taper-length 4) (inner-fec "h128") (outer-fec "none")
                        (id ""))
  "Initialize a transfer."
  (let ((transfer (ofdm-transfer-create radio-driver
                                        (if emit 1 0)
                                        file
                                        sample-rate
                                        bit-rate
                                        frequency
                                        frequency-offset
                                        gain
                                        ppm
                                        subcarrier-modulation
                                        subcarriers
                                        cyclic-prefix-length
                                        taper-length
                                        inner-fec
                                        outer-fec
                                        id
                                        (null-pointer))))
    (if (null-pointer-p transfer)
        (error "Failed to initialize transfer.")
        transfer)))

(defun free-transfer (transfer)
  "Cleanup after a finished transfer."
  (ofdm-transfer-free transfer))

(defun start-transfer (transfer)
  "Start a transfer and return when finished."
  (ofdm-transfer-start transfer))

(defun stop-transfer (transfer)
  "Interrupt a transfer."
  (ofdm-transfer-stop transfer))

(defun stop-all-transfers ()
  "Interrupt all transfers."
  (ofdm-transfer-stop-all))

(defun transmit (file
                 &key
                   (radio-driver "") (sample-rate 2000000) (bit-rate 38400)
                   (frequency 434000000) (frequency-offset 0) (gain 0)
                   (ppm 0.0) (subcarrier-modulation "qpsk")
                   (subcarriers 64) (cyclic-prefix-length 16)
                   (taper-length 4) (inner-fec "h128") (outer-fec "none")
                   (id ""))
  "Transmit the data from FILE."
  (let ((transfer (make-transfer file t
                                 :radio-driver radio-driver
                                 :sample-rate sample-rate
                                 :bit-rate bit-rate
                                 :frequency frequency
                                 :frequency-offset frequency-offset
                                 :gain gain
                                 :ppm ppm
                                 :subcarrier-modulation subcarrier-modulation
                                 :subcarriers subcarriers
                                 :cyclic-prefix-length cyclic-prefix-length
                                 :taper-length taper-length
                                 :inner-fec inner-fec
                                 :outer-fec outer-fec
                                 :id id)))
    (unwind-protect (ofdm-transfer-start transfer)
      (ofdm-transfer-free transfer))
    t))

(defun receive (file
                &key
                  (radio-driver "") (sample-rate 2000000) (bit-rate 38400)
                  (frequency 434000000) (frequency-offset 0) (gain 0)
                  (ppm 0.0) (subcarrier-modulation "qpsk")
                  (subcarriers 64) (cyclic-prefix-length 16)
                  (taper-length 4) (inner-fec "h128") (outer-fec "none")
                  (id ""))
  "Receive data into FILE."
  (let ((transfer (make-transfer file nil
                                 :radio-driver radio-driver
                                 :sample-rate sample-rate
                                 :bit-rate bit-rate
                                 :frequency frequency
                                 :frequency-offset frequency-offset
                                 :gain gain
                                 :ppm ppm
                                 :subcarrier-modulation subcarrier-modulation
                                 :subcarriers subcarriers
                                 :cyclic-prefix-length cyclic-prefix-length
                                 :taper-length taper-length
                                 :inner-fec inner-fec
                                 :outer-fec outer-fec
                                 :id id)))
    (unwind-protect (ofdm-transfer-start transfer)
      (ofdm-transfer-free transfer))
    t))
