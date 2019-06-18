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

(require db)

(provide db-conn db-init)

(define db-conn
  (virtual-connection
    (connection-pool
      (λ () (sqlite3-connect #:database "xyzzygy.db"
                             #:mode 'create)))))

(define (db-init)
  (query-exec db-conn
              "CREATE TABLE IF NOT EXISTS users (
                 id INTEGER PRIMARY KEY,
                 username TEXT NOT NULL,
                 password TEXT NOT NULL
               )"))
