;;; This file is part of cl-ofdm-transfer
;;; Copyright 2021 Guillaume LE VAILLANT
;;; Distributed under the GNU GPL v3 or later.
;;; See the file LICENSE for terms of use and distribution.

(defsystem "ofdm-transfer"
  :name "ofdm-transfer"
  :description "Send and receive data with SDRs using OFDM modulation"
  :version "1.0"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("cffi")
  :components ((:file "ofdm-transfer")))
