;; Copyright 2019 Thea Leake

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


(provide single-player
         multi-player
         remote-player-join
         remote-player-create)

(require racket/tcp 
         racket/match
         racket/list
         (only-in racket/string
                  string-join
                  string-split)
         (only-in racket/class
                  send)
         (prefix-in b: "../models/board.rkt")
         (prefix-in r: "../models/roles.rkt")
         (prefix-in g: "../models/graveyard.rkt")
         (prefix-in ai: "ai.rkt")
         (prefix-in t: "../views/tile.rkt")
         (prefix-in i: "../views/images.rkt")
         (prefix-in v: "../views/view.rkt")
         (prefix-in ev: "../views/end_view.rkt")
         (prefix-in nc: "../networking/src/game-converter.rkt"))

(define init-turn
  (g:gen-init-turn "First Necromancer: pick a corpse to raise!"))

(define human-player-channel (make-channel))

(define computer-player-channel (make-channel))

(define remote-player-channel (make-channel))

; KEEPALIVE
(define (start-keepalive game-name game-pwd client-id)
  (define (send-keepalive)
    ;(println "hey I'm here :D")
    (define resp-msg (nc:connect-to-server (string-join (list "A" client-id game-name game-pwd) ":")))
    ; keep track of if server is still responding. Response should just be "A"
    (sleep 10)
    (send-keepalive))
  (send-keepalive))
; END KEEPALIVE

(define tile-list
  (map (lambda (piece coords)
         (t:make-tile v:board-table
                      (lambda ()
                        (channel-put human-player-channel coords))
                      piece
                      coords))
       (g:turn-board init-turn)
       b:board-coordinates))


(define (update-board state)
  (for-each (lambda (tile-piece-coords)
              (t:update-tile state tile-piece-coords))
            (map t:location
                 tile-list
                 (g:turn-board state)
                 b:board-coordinates)))

(define (update-ui state)
  (update-board state)
  (send v:player-display set-label (string-join (list "Current Necromancer:" (g:turn-player state))))
  (send v:player-message set-label (g:turn-message state)))


(define (event-handled state)
  (update-ui state)
  state)


(define (finish-move-message state location-coords)
  (let ([captured-piece (g:turn-captured state)])
    (if (r:cell-empty? captured-piece)
        (g:turn-message state)
        (string-join (list "Captured "
                           (r:cell-player captured-piece)
                           (r:cell-role captured-piece))))))

; SEND UPDATE
(define (exec-send-update game-name game-pwd client-id id name pwd)
  (if (and (equal? game-name name) (equal? game-pwd pwd) (equal? client-id id))
      #t
      (begin (nc:connect-to-server "P") #f))) ; disconnect, end game

(define (update-server move-from move-to game-name game-pwd client-id)
  (println "update-server")
  (define move-from-str (string-join (list (number->string (b:position-column move-from)) (number->string (b:position-row move-from))) ""))
  (define move-to-str (string-join (list (number->string (b:position-column move-to)) (number->string (b:position-row move-to))) ""))
  (define resp-msg (nc:connect-to-server (string-join (list "C" client-id game-name game-pwd move-from-str move-to-str) ":")))
  (println resp-msg)
  (cond
    [(regexp-match-exact? #px"M" resp-msg) null] ; disconnect, end game
    [(regexp-match-exact? #px"P" resp-msg) null] ; disconnect, end game
    [(regexp-match-exact? #px"A" resp-msg) #t]
    [(regexp-match-exact? #px"D:[1 | 2]:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}:[0-8][0-4]:[0-8][0-4]:[1 | 2]" resp-msg)
     (match (regexp-split #px":" resp-msg) [(list code id name pwd move-from move-to who-moved-last) (exec-send-update game-name game-pwd client-id id name pwd)])]
    [else (begin (nc:connect-to-server "M") null)])) ; disconnect, end game

(define (finish-move-turn state location-coords [game-name #f] [game-pwd #f] [client-id #f])
  (println "finish move turn")
  (let* ([updated-game (g:player-move state
                                      location-coords)]
         [message (finish-move-message updated-game
                                       location-coords)]
         [updated-turn (struct-copy g:turn updated-game
                                    [message message]
                                    [src-coords b:none-position])])
    (if game-name
        (update-server (g:turn-src-coords state) location-coords game-name game-pwd client-id)
        null)
    (event-handled updated-turn)))


(define (raise-message state coords)
  (string-join (list
                "Raised a"
                (g:role-at-location coords (g:turn-board state)))))


(define (raise-location state location-coords [game-name #f] [game-pwd #f] [client-id #f])
  (let* ([handled-turn (g:player-flip-location state
                                              location-coords)]
        [updated-turn (struct-copy g:turn handled-turn
                  [message (raise-message state
                                          location-coords )])])
    (update-server location-coords location-coords game-name game-pwd client-id)
    (event-handled updated-turn)))


(define (move-message state location-coords)
  (string-join (list
                (g:player-at-location location-coords (g:turn-board state))
                (g:role-at-location location-coords (g:turn-board state))
                "selected, choose destination")))


(define (move-src-event state location-coords)
  (event-handled (struct-copy g:turn state
                              [src-coords location-coords]
                              [message (move-message state location-coords)])))

(define (wrong-player state)
  (event-handled (struct-copy g:turn state
                              [message "Error! Selected other necromancer's piece."])))


(define (handle-tile-click state location-coords [game-name #f] [game-pwd #f] [client-id #f])
  (println "handle tile click")
  (println location-coords)
  (cond
    ((list? location-coords) (if (equal? (car location-coords) (cadr location-coords))
                                 (handle-tile-click state (car location-coords) game-name game-pwd client-id)
                                 (handle-tile-click (handle-tile-click state (car location-coords) game-name game-pwd client-id)
                                                    (cadr location-coords) game-name game-pwd client-id)))
    ((g:coords-selected? state) (finish-move-turn state location-coords game-name game-pwd client-id))
    ((g:location-hidden? location-coords (g:turn-board state))
     (raise-location state location-coords game-name game-pwd client-id))
    ((eq? (g:turn-player state)
          (g:player-at-location location-coords (g:turn-board state)))
     (move-src-event state location-coords))
    (else (wrong-player state))))


(define (player-won player cleanup-thunk)
  (send ev:end-game-message set-label
        (string-join (list "Player"
                            player
                           "Has Won!")))
  (send ev:end-game-dialog show #t)
  (cleanup-thunk))

(define (clear-player-channel)
  (when (channel-try-get human-player-channel)
    (clear-player-channel)))

(define (get-human-choice)
  (clear-player-channel)
  (channel-get human-player-channel))


(define (get-computer-choice state)
  (channel-put computer-player-channel state)
  (channel-get computer-player-channel))

; REQUEST UPDATE
(define (exec-request-update game-name game-pwd client-id id name pwd move-from move-to)
  (if (and (equal? game-name name) (equal? game-pwd pwd) (equal? client-id id))
      (let ([list-move-from (string-split move-from "")]
            [list-move-to (string-split move-from "")])
        (list (b:position (string->number (cadr list-move-from)) (string->number (caddr list-move-from)))
              (b:position (string->number (cadr list-move-to)) (string->number (caddr list-move-to)))))
      (begin (nc:connect-to-server "P") null))) ; disconnect, end game

(define (get-remote-player-choice game-name game-pwd client-id)
  (define resp-msg (nc:connect-to-server (string-join (list "B" client-id game-name game-pwd) ":")))
  (println "in get-remote-choice")
  (println resp-msg)
  (cond
    [(regexp-match-exact? #px"M" resp-msg) null] ; disconnect, end game
    [(regexp-match-exact? #px"P" resp-msg) null] ; disconnect, end game
    [(regexp-match-exact? #px"D:[1 | 2]:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}:[0-8][0-4]:[0-8][0-4]:[1 | 2]" resp-msg)
     (match (regexp-split #px":" resp-msg) [(list code id name pwd move-from move-to who-moved-last) 
                                            (if (equal? client-id who-moved-last)
                                                (begin (sleep 2) (get-remote-player-choice game-name game-pwd client-id))
                                                (exec-request-update game-name game-pwd client-id id name pwd move-from move-to))])]
    [else (begin (nc:connect-to-server "M") null)])) ; disconnect, end game
; END REQUEST UPDATE

(define (event-loop init-state player-choice-fn [game-name #f] [game-pwd #f] [client-id #f])
  (println "event loop")
  (let loop ([state init-state])
    (println "in event loop loop")
    (cond
      ((g:player-lost? state)
       (r:toggle-player (g:turn-player state)))         ;; toggle to return winning player
      (else
       (loop
        (handle-tile-click state
                           (player-choice-fn state)
                           game-name game-pwd client-id))))))


(define (single-player-init-turn init-state)
  (let* ([second-turn
         (handle-tile-click init-state
                            (channel-get human-player-channel))]
         [second-player (g:turn-player second-turn)])
    (event-loop second-turn
                (lambda (state)
                  (if (eq? (g:turn-player state ) second-player)
                      (get-computer-choice state)
                      (get-human-choice))))))


(define (multi-player-init-turn init-state)
  (let ([event-result
         (handle-tile-click init-state
                            (channel-get human-player-channel))])
    (event-loop event-result
                (lambda (_)
                  (get-human-choice)))))


(define (multi-player)
 (thread
  (lambda ()
    (player-won (multi-player-init-turn init-turn)
                (lambda ()
                  (void))))))

(define (single-player [difficulty 'easy])
  (thread
   (lambda ()
     (ai:start-ai computer-player-channel difficulty)
     (player-won (single-player-init-turn init-turn)
                 (lambda ()
                   (channel-put computer-player-channel
                                #f))))))

; HAS OTHER PLAYER JOINED?
(define (exec-other-player-joined? game-name game-pwd name pwd)
  (if (and (equal? game-name name) (equal? game-pwd pwd))
      #t
      (begin (nc:connect-to-server "P")
             #f)))

(define (other-player-joined? game-name game-pwd client-id)
  (println "joined?")
  (define resp-msg (nc:connect-to-server (string-join (list "K" client-id game-name game-pwd) ":")))
  (println resp-msg)
  (cond
    [(regexp-match-exact? #px"M" resp-msg) #f] ; invalid msg - disconnect, end game
    [(regexp-match-exact? #px"P" resp-msg) #f] ; invalid credentials - disconnect, end game
    [(regexp-match-exact? #px"J" resp-msg) #f] ; forward leave - disconnect, end game
    [(regexp-match-exact? #px"A" resp-msg) (begin (sleep 3) (other-player-joined? game-name game-pwd client-id))] ; forward leave - disconnect, end game
    [(regexp-match-exact? #px"H:1:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}:([A-Z]){32}:([O | P]){32}" resp-msg)
     (match (regexp-split #px":" resp-msg) [(list code id name pwd msg1 msg2) (exec-other-player-joined? game-name game-pwd name pwd)])]
    [else (begin (nc:connect-to-server "M") #f)])) ; disconnect, end game

; REMOTE PLAYER CREATE GAME
(define (exec-create-game game-name game-pwd client-id id name pwd)
  (if (and (equal? name game-name) (equal? pwd game-pwd) (equal? id client-id))
      #t
      (begin (nc:connect-to-server "P")
             #f)))

(define (create-game game-name game-pwd client-id init-state) 
  (println "create-game")
  (define resp-msg (nc:connect-to-server (nc:make-board-string game-name game-pwd (g:turn-board init-state))))
  (println resp-msg)
  (cond
    [(regexp-match-exact? #px"M" resp-msg) #f] ; invalid msg - disconnect, end game
    [(regexp-match-exact? #px"Q" resp-msg) #f] ; name is taken - disconnect, end game
    [(regexp-match-exact? #px"T" resp-msg) #f] ; too many players - disconnect, end game
    [(regexp-match-exact? #px"F:1:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}" resp-msg)
     (match (regexp-split #px":" resp-msg) [(list code id name pwd) (exec-create-game game-name game-pwd client-id id name pwd)])]
    [else (begin (nc:connect-to-server "M") #f)])) ; disconnect, end game

(define (remote-player-create-init-turn init-state game-name game-pwd client-id)
  (println "remote-player-create-init-turn")
  (if (and (create-game game-name game-pwd client-id init-state)
           (other-player-joined? game-name game-pwd client-id))
      (let* ([finished-first-turn
               (handle-tile-click init-state
                                  (channel-get human-player-channel)
                                  game-name
                                  game-pwd
                                  client-id)]
             [remote-player (g:turn-player finished-first-turn)])
        (println "after let")
        (println remote-player)
        (event-loop finished-first-turn 
                    (lambda (state)
                      (if (eq? (g:turn-player state) remote-player)
                          (get-remote-player-choice game-name game-pwd client-id)
                          (get-human-choice)))
                    game-name
                    game-pwd
                    client-id))
  null)) ; disconnect, end game

(define (remote-player-create game-name game-pwd)
  (thread
    (lambda ()
      (start-keepalive game-name game-pwd "1")))
  (thread
    (lambda ()
      (player-won (remote-player-create-init-turn init-turn game-name game-pwd "1")
                  (lambda ()
                    (void))))))

; REMOTE PLAYER JOIN GAME
(define (exec-join-game game-name game-pwd name pwd pieces player-pieces)
  (if (and (equal? name game-name) (equal? pwd game-pwd))
      (g:turn (nc:make-board pieces player-pieces) 
            "Undecided"
            "AAAAAAAAAAAAAAAHHHHHHH"
            #t
            r:none-role
            b:none-position
            #f)
      (begin (nc:connect-to-server "P")
             null)))

(define (join-game game-name game-pwd client-id) 
  (define resp-msg (nc:connect-to-server (string-join (list "G" client-id game-name game-pwd) ":")))
  (cond
    [(regexp-match-exact? #px"M" resp-msg) null] ; invalid code - disconnect, end game
    [(regexp-match-exact? #px"P" resp-msg) null] ; invalid credentials - disconnect, end game
    [(regexp-match-exact? #px"R" resp-msg) null] ; too many players - disconnect, end game
    [(regexp-match-exact? #px"H:2:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}:([A-Z]){32}:([O | P]){32}" resp-msg)
     (match (regexp-split #px":" resp-msg) [(list code id name pwd pieces player-pieces) (exec-join-game game-name game-pwd name pwd pieces player-pieces)])]
    [else (begin (nc:connect-to-server "M") null)])) ; disconnect, end game

(define (remote-player-join-init-turn game-name game-pwd client-id)
  (define init-state (join-game game-name game-pwd client-id))
  (if (not (null? init-state))
      (let* ([finished-first-turn
               (handle-tile-click init-state (get-remote-player-choice game-name game-pwd client-id) game-name game-pwd client-id)]
             [local-player (g:turn-player finished-first-turn)])
        (event-loop finished-first-turn 
                    (lambda (state)
                      (if (eq? (g:turn-player state) local-player)
                          (get-human-choice)
                          (get-remote-player-choice game-name game-pwd client-id)))
                    game-name
                    game-pwd
                    client-id))
      null)) ; tell player game could not be entered

(define (remote-player-join game-name game-pwd)
  (thread
    (lambda ()
      (start-keepalive game-name game-pwd "2")))
  (thread
    (lambda ()
      (player-won (remote-player-join-init-turn game-name game-pwd "2")
                  (lambda ()
                    (void))))))
