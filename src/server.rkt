; xyzzygy - a clone of the game Cards Against Humanity
; Copyright (C) 2019  Andy Tockman <andy@tck.mn>
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/templates)

(define (response/html html)
  (response/output
    (λ (out) (write-string html out))))

(define-values (xyzzygy-dispatch xyzzygy-url)
  (dispatch-rules
    [("") homepage]
    [("about") about-page]))

(define (homepage req)
  (response/html (include-template "../templates/home.html")))

(define (about-page req)
  (response/html (include-template "../templates/about.html")))

(serve/servlet xyzzygy-dispatch
               #:command-line? #t
               #:servlet-regexp #rx""
               #:extra-files-paths (list (build-path "static")))
