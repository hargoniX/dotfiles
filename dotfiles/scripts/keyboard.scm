#! /bin/sh
# -*- mode: scheme; coding: utf-8 -*-
exec guile -e main -s "$0" "$@"
!#
(use-modules (srfi srfi-1)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 match)
             (ice-9 regex)
             (ice-9 receive))

(define vendor-device-id "0416:0123")

(define (configure-keyboard)
  (format #t "Configuring keyboard...~%")
  (system "setxkbmap us"))
  ;; (system "setxkbmap za -option caps:swapescape"))

(define open-process
  (@@ (ice-9 popen) open-process))

(define (open-udevadm-monitor)
  (open-process OPEN_BOTH
                "udevadm"
                "monitor"
                "--udev"
                "--subsystem-match=hid"))

(define udev-add-regex
  (make-regexp
   (string-append "^UDEV +\\[[0-9a-z:.]+\\] +add +/devices/.+"
                   vendor-device-id
                   "\\..+")))

(define (main args)
  ;; configure once right away on start
  (configure-keyboard)
  (receive (read-port write-port pid)
      (open-udevadm-monitor)
    (let loop ((line (read-line read-port)))
      (unless (eof-object? line)
        (when (regexp-exec udev-add-regex line)
          (configure-keyboard))
        (loop (read-line read-port))))
    (kill pid SIGKILL)))
