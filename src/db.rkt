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

(require db sql)

(provide db-conn db-init
         (struct-out user)
         (all-from-out db) (all-from-out sql))

(define db-conn
  (virtual-connection
    (connection-pool
      (thunk (sqlite3-connect #:database "xyzzygy.db"
                              #:mode 'create)))))

(define (db-init)
  (query-exec db-conn
              "CREATE TABLE IF NOT EXISTS users (
                 id       INTEGER PRIMARY KEY,
                 created  REAL NOT NULL,
                 username TEXT NOT NULL,
                 password TEXT NOT NULL,
                 admin    INTEGER NOT NULL
               )")
  (query-exec db-conn
              "CREATE TABLE IF NOT EXISTS keys (
                 key     TEXT NOT NULL,
                 created REAL NOT NULL
               )")
  (query-exec db-conn
              "CREATE TABLE IF NOT EXISTS decks (
                 id      INTEGER PRIMARY KEY,
                 created REAL NOT NULL,
                 owner   INTEGER NOT NULL,
                 name    TEXT NOT NULL,
                 pinned  INTEGER NOT NULL
               )")
  (query-exec db-conn
              "CREATE TABLE IF NOT EXISTS cards (
                 id      INTEGER PRIMARY KEY,
                 created REAL NOT NULL,
                 deckid  INTEGER NOT NULL,
                 val     TEXT NOT NULL
               );"))

(struct user (id name admin?))
