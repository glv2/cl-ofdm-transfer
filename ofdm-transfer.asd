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
  :depends-on ("cffi" "cl-octet-streams" "float-features")
  :in-order-to ((test-op (test-op "ofdm-transfer/tests")))
  :components ((:file "ofdm-transfer")))

(defsystem "ofdm-transfer/tests"
  :name "ofdm-transfer/tests"
  :description "Tests fot ofdm-transfer"
  :version "1.0"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("fiveam" "ofdm-transfer" "uiop")
  :in-order-to ((test-op (load-op "ofdm-transfer/tests")))
  :perform (test-op (o s)
             (let ((tests (uiop:find-symbol* 'ofdm-transfer-tests
                                             :ofdm-transfer-tests)))
               (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:file "tests")))
