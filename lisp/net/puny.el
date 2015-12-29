;;; puny.el --- translate non-ASCII domain names to ASCII

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: mail, net

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Written by looking at
;; http://stackoverflow.com/questions/183485/can-anyone-recommend-a-good-free-javascript-for-punycode-to-unicode-conversion

;;; Code:

(require 'seq)

(defun puny-encode-domain (domain)
  "Encode DOMAIN according to the IDNA/punycode algorith.
For instance, \"fśf.org\" => \"xn--ff-2sa.org\"."
  ;; The vast majority of domain names are not IDNA domain names, so
  ;; add a check first to avoid doing unnecessary work.
  (if (string-match "\\'[[:ascii:]]+\\'" domain)
      domain
    (mapconcat 'puny-encode-string (split-string domain "[.]") ".")))

(defun puny-encode-string (string)
  "Encode STRING according to the IDNA/punycode algorithm.
This is used to encode non-ASCII domain names.
For instance, \"bücher\" => \"xn--bcher-kva\"."
  (let ((ascii (seq-filter (lambda (char)
                             (< char 128))
                           string)))
    (if (= (length ascii) (length string))
        string
      (concat "xn--"
              (if (null ascii)
                  ""
                (concat ascii "-"))
              (puny-encode-complex (length ascii) string)))))

(defun puny-decode-domain (domain)
  "Decode DOMAIN according to the IDNA/punycode algorith.
For instance, \"xn--ff-2sa.org\" => \"fśf.org\"."
  (mapconcat 'puny-decode-string (split-string domain "[.]") "."))

(defun puny-decode-string (string)
  "Decode an IDNA/punycode-encoded string.
For instance \"xn--bcher-kva\" => \"bücher\"."
  (if (string-match "\\`xn--" string)
      (puny-decode-string-internal (substring string 4))
    string))

(defconst puny-initial-n 128)
(defconst puny-initial-bias 72)
(defconst puny-base 36)
(defconst puny-damp 700)
(defconst puny-tmin 1)
(defconst puny-tmax 26)
(defconst puny-skew 28)

;; 0-25  a-z
;; 26-36 0-9
(defun puny-encode-digit (d)
  (if (< d 26)
      (+ ?a d)
    (+ ?0 (- d 26))))

(defun puny-adapt (delta num-points first-time)
  (let ((delta (if first-time
                   (/ delta puny-damp)
                 (/ delta 2)))
        (k 0))
    (setq delta (+ delta (/ delta num-points)))
    (while (> delta (/ (* (- puny-base puny-tmin)
                          puny-tmax)
                       2))
      (setq delta (/ delta (- puny-base puny-tmin))
            k (+ k puny-base)))
    (+ k (/ (* (1+ (- puny-base puny-tmin)) delta)
            (+ delta puny-skew)))))

(defun puny-encode-complex (insertion-points string)
  (let ((n puny-initial-n)
        (delta 0)
        (bias puny-initial-bias)
        (h insertion-points)
        result m ijv q)
    (while (< h (length string))
      (setq ijv (cl-loop for char across string
                         when (>= char n)
                         minimize char))
      (setq m ijv)
      (setq delta (+ delta (* (- m n) (+ h 1)))
            n m)
      (cl-loop for char across string
               when (< char n)
               do (cl-incf delta)
               when (= char ijv)
               do (progn
                    (setq q delta)
                    (cl-loop with k = puny-base
                             for t1 = (cond
                                       ((<= k bias)
                                        puny-tmin)
                                       ((>= k (+ bias puny-tmax))
                                        puny-tmax)
                                       (t
                                        (- k bias)))
                             while (>= q t1)
                             do (push (puny-encode-digit
                                       (+ t1 (mod (- q t1)
                                                  (- puny-base t1))))
                                      result)
                             do (setq q (/ (- q t1) (- puny-base t1))
                                      k (+ k puny-base)))
                    (push (puny-encode-digit q) result)
                    (setq bias (puny-adapt delta (+ h 1) (= h insertion-points))
                          delta 0
                          h (1+ h))))
      (cl-incf delta)
      (cl-incf n))
    (nreverse result)))

(defun puny-decode-digit (cp)
  (cond
   ((<= cp ?9)
    (+ (- cp ?0) 26))
   ((<= cp ?Z)
    (- cp ?A))
   ((<= cp ?z)
    (- cp ?a))
   (t
    puny-base)))

(defun puny-decode-string-internal (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-max))
    (search-backward "-" nil (point-min))
    ;; The encoded chars are after the final dash.
    (let ((encoded (buffer-substring (1+ (point)) (point-max)))
          (ic 0)
          (i 0)
          (bias puny-initial-bias)
          (n puny-initial-n)
          out)
      (delete-region (point) (point-max))
      (while (< ic (length encoded))
        (let ((old-i i)
              (w 1)
              (k puny-base)
              digit t1)
          (cl-loop do (progn
                        (setq digit (puny-decode-digit (aref encoded ic)))
                        (cl-incf ic)
                        (cl-incf i (* digit w))
                        (setq t1 (cond
                                  ((<= k bias)
                                   puny-tmin)
                                  ((>= k (+ bias puny-tmax))
                                   puny-tmax)
                                  (t
                                   (- k bias)))))
                   while (>= digit t1)
                   do (setq w (* w (- puny-base t1))
                            k (+ k puny-base)))
          (setq out (1+ (buffer-size)))
          (setq bias (puny-adapt (- i old-i) out (= old-i 0))))

        (setq n (+ n (/ i out))
              i (mod i out))
        (goto-char (point-min))
        (forward-char i)
        (insert (format "%c" n))
        (cl-incf i)))
    (buffer-string)))

;; http://www.unicode.org/reports/tr39/#Restriction_Level_Detection

(defun puny-highly-restrictive-p (string)
  (let ((scripts
         (seq-uniq
          (seq-map (lambda (char)
                     (aref char-script-table char))
                   string))))
    (or
     ;; Every character uses the same script.
     (= (length scripts) 1)
     (seq-some 'identity
               (mapcar (lambda (list)
                         (seq-every-p (lambda (script)
                                        (memq script list))
                                      scripts))
                       '((latin han hiragana kana)
                         (latin han bopomofo)
                         (latin han hangul)))))))

(provide 'puny)

;;; puny.el ends here
