;; (networking update) Copyright 2020 Casper Rutz

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;; http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

#lang racket/base

(provide make-board-string
         make-board
         connect-to-server)

(require racket/tcp 
         (only-in racket/string
                  string-join)
         (prefix-in b: "../../models/board.rkt")
         (prefix-in r: "../../models/roles.rkt")
         (prefix-in g: "../../models/graveyard.rkt"))

(define (connect-to-server msg [host "localhost"])
  (define-values (in out) (tcp-connect host 6413))
  (display msg out)
  (close-output-port out)
  (define resp-msg (read-line in))
  (close-input-port in)
  resp-msg)

(define role-to-string (hash r:leader "L" r:advisor "V" r:elephant "Z" r:chariot "G" r:horse "S" r:cannon "W" r:pawn "P"))
(define string-to-role (hash "L" r:leader "V" r:advisor "Z" r:elephant "G" r:chariot "S" r:horse "W" r:cannon "P" r:pawn))

(define string-to-player (hash "O" "Orange" "P" "Purple"))
(define player-to-string (hash "Orange" "O" "Purple" "P"))

; MAKE BOARD
; join game function gets ok from server, and calls function to init a game.
; this is how that function creates a board.

(define (make-cell zipped-piece)
  (r:cell (hash-ref string-to-player (cadr zipped-piece)) ; gets second element in a list
        #f
        (hash-ref string-to-role (car zipped-piece)) ; first elem in a list
        #f))

(define (make-board pieces piece-player) ; pieces/piece-player are from server, after return string has been parsed and code/id/name/pwd verified.
  (define zipped-pieces (map list (map string (string->list pieces)) (map string (string->list piece-player))))
  (map make-cell zipped-pieces))

; MAKE BOARD STRING
; called by create-game function to send to server

(define (make-board-string game-name game-pwd board)  ; name/pwd are input from user, board/whos-going-first are from game creation 
  (define piece-string (string-join (map (lambda (cell) (hash-ref role-to-string (r:cell-role cell))) board) ""))
  (define piece-player-string (string-join (map (lambda (cell) (hash-ref player-to-string (r:cell-player cell))) board) ""))
  (string-join (list "E:1" game-name game-pwd piece-string piece-player-string) ":"))
