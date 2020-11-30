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

(provide start-server
         kill-all-server-threads)

(require racket/tcp
         racket/string
         racket/list
         racket/bool
         racket/match)

; CHANNEL

; messages from clients are passed here, where history picks them up.
(define conn-channel (make-channel))

; HISTORY

; destroys a game. Called upon receiving an erroneous message or when client is inactive.
(define (destroy-game game-data game-name)
  (hash-remove game-data game-name))

; updates time of communication for client.  
(define (set-update-time game-data id name)
  (define game (hash-ref game-data name))
  (hash-set game-data name (hash-set game (string-join (list id "-last-updated") "") (current-seconds))))

; responds to keepalive and updates time of communication for client.
(define (op-keepalive game-data id name pwd resp-msg)
  (resp-msg "A") ; op-keepalive
  (set-update-time game-data id name))

; responds with last game update; updates time of communication for client.
(define (op-update? game-data id name pwd resp-msg)
  (define s-last-move (hash-ref (hash-ref game-data name) "last-move")) ;stored as (list # #)
  (define who-moved-last (hash-ref (hash-ref game-data name) "who-moved-last"))
  (resp-msg (string-join (list "D" id name pwd (car s-last-move) (last s-last-move) who-moved-last) ":")) ; op-forward-update
  (set-update-time game-data id name))

; confirms if a player was able to update a game successfully; updates time of communication for client.
(define (op-update game-data id name pwd move-from move-to resp-msg)
  (define game-to-update (hash-ref game-data name))
  (if (equal? (hash-ref (hash-ref game-data name) "last-move") (list move-from move-to))
      (begin (resp-msg "A")
             (set-update-time game-data id name))
      (begin (resp-msg (string-join (list "D" id name pwd move-from move-to id) ":")) ; op-forward-update
             (set-update-time (hash-set game-data name (hash-set* game-to-update "last-move" (list move-from move-to) "who-moved-last" id)) id name))))

; confirms if a player was able to create a game successfully; if so, updates time of communication for client.
(define (op-create game-data id name pwd pieces piece-to-player resp-msg)
  (println "creating a game...")
  (println game-data)
  (println name)
  (cond
    [(hash-has-key? game-data name) (begin (resp-msg "Q") game-data)] ; err-invalid-name (name already exists)
    [(>= (hash-count game-data) 5) (begin (resp-msg "T") game-data)] ; err-too-many-games
    [else (begin (resp-msg (string-join (list "F" id name pwd) ":")) ; op-game-created
                 (hash-set game-data name (hash "pieces" pieces
                                                "piece-to-player" piece-to-player
                                                "last-move" (list "84" "84")
                                                "who-moved-last" "2"
                                                "pwd" pwd
                                                "1-connected" #t
                                                "2-connected" null
                                                "1-last-updated" (current-seconds)
                                                "2-last-updated" null)))]))

; response confirms if a player was able to join sucessfully; if so, updates time of communication for client.
(define (op-join game-data id name pwd resp-msg)
  (if (null? (hash-ref (hash-ref game-data name) "2-connected"))
      (let ([piece-to-player (hash-ref (hash-ref game-data name) "piece-to-player")]
            [pieces (hash-ref (hash-ref game-data name) "pieces")])
        (resp-msg (string-join (list "H" id name pwd pieces piece-to-player) ":")) ; op-forward-join
        (hash-set game-data name (hash-set* (hash-ref game-data name) "2-connected" #t "2-last-updated" (current-seconds))))
      (begin (resp-msg "R") ; err-3s-a-crowd
             game-data)))

; response confirms if player 2 has joined; updates the time of communication for the client.
(define (op-joined? game-data id name pwd resp-msg)
  (define is-2-connected? (hash-ref (hash-ref game-data name) "2-connected"))
  (cond
    [(null? is-2-connected?) (resp-msg "A")] ; op-keepalive
    [(equal? #t is-2-connected?) (let ([piece-to-player (hash-ref (hash-ref game-data name) "piece-to-player")]
                                       [pieces (hash-ref (hash-ref game-data name) "pieces")])
                                   (resp-msg (string-join (list "H" id name pwd pieces piece-to-player) ":")))] ; op-forward-join
    [(not is-2-connected?) (resp-msg "J")]) ; op-forward-leave
  (set-update-time game-data id name))

; ensures game name refers to an actual game with a password that matches the one provided
(define (credentials-ok? game-data name pwd)
  (println "credentials-ok?")
  (println game-data)
  (println name)
  (println pwd)
  (define game-has-key (and (hash-has-key? game-data name) (equal? (hash-ref (hash-ref game-data name) "pwd") pwd)))
  (println game-has-key)
  game-has-key)

; ensures client's message contains correct credentials, returns updated game data.
(define (exec-opcode game-data code id name pwd msg1 msg2 resp-msg)
  (println "structure correct!")
  (if (or (equal? code "E") (credentials-ok? game-data name pwd)) ; if game is being created, credentials won't be stored yet
      (cond
        [(equal? code "A") (op-keepalive game-data id name pwd resp-msg)]
        [(equal? code "B") (op-update? game-data id name pwd resp-msg)] 
        [(equal? code "C") (op-update game-data id name pwd msg1 msg2 resp-msg)] 
        [(equal? code "E") (op-create game-data id name pwd msg1 msg2 resp-msg)] 
        [(equal? code "G") (op-join game-data id name pwd resp-msg)] 
        [(equal? code "K") (op-joined? game-data id name pwd resp-msg)] 

        [(regexp-match-exact? #px"[M | P]" code) (destroy-game game-data name)])
      (begin (resp-msg "P") ; err-wrong-credentials
             game-data)))

; splits client message into its component parts. 
; Calls exec-opcode to update game data and respond to client. 
(define (match-opcode game-data client-msg resp-msg)
  (cond
    [(string-prefix? client-msg "E") (match (regexp-split #px":" client-msg) 
                                            [(list code id name pwd msg1 msg2) (exec-opcode game-data code id name pwd msg1 msg2 resp-msg)])]
    [(string-prefix? client-msg "C") (match (regexp-split #px":" client-msg) 
                                            [(list code id name pwd msg1 msg2) (exec-opcode game-data code id name pwd msg1 msg2 resp-msg)])]
    [else (match (regexp-split #px":" client-msg) [(list code id name pwd) (exec-opcode game-data code id name pwd null null resp-msg)])]))

; ensures messages are of the correct structure.
(define (correct-structure client-msg)
  ; opcode : id : name : game-password : message
  (println "Checking structure...")
  (let [(regex-match (or (regexp-match-exact? #px"[A | B | M | P]:[1 | 2]:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}" client-msg) 
                         (regexp-match-exact? #px"C:[1 | 2]:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}:[0-8][0-4]:[0-8][0-4]" client-msg)
                         (regexp-match-exact? #px"E:1:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}:[L | V | Z | G | S | W | P]{32}:[O | P]{32}" client-msg)
                         (regexp-match-exact? #px"K:1:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}" client-msg)
                         (regexp-match-exact? #px"G:2:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}" client-msg)))]
    (println regex-match)
    regex-match))

; for garbage collection: if it's more than 10 seconds since a client communicated with
;  server, the server considers that client inacitve and kills the game.
(define (check-timeout current-game)
  (define 1-timeout? (>= (- (current-seconds) (hash-ref current-game "1-last-updated")) 60))
  (define 2-time (hash-ref current-game "2-last-updated"))
  (if (null? 2-time)
      1-timeout?
      (or 1-timeout? (>= (- (current-seconds) 2-time) 60))))

; garbage collection for inactive games.
(define (remove-stale-games game-data name-list)
  (println "Garbage collecting old gams")
  (println name-list)
  (if (empty? name-list)
      game-data
      (let ([current-game (hash-ref game-data (car name-list))])
        (if (check-timeout current-game)
            (remove-stale-games (destroy-game game-data (car name-list)) (cdr name-list))
            (remove-stale-games game-data (cdr name-list))))))

; analyzes message and returns updated game data. Also responds to client.
(define (check-message channel-msg game-data)
  (if (equal? channel-msg "Garbage day!")
      (remove-stale-games game-data (hash-keys game-data))
      (let ([client-msg (hash-ref channel-msg "client-msg")]
            [resp-msg (hash-ref channel-msg "response")])
        (println "checkmsg")
        (println channel-msg)
        (println (hash-ref channel-msg "client-msg"))
        (println (regexp-split #px":" client-msg))
        (if (correct-structure client-msg)
            (match-opcode game-data client-msg resp-msg)
            (begin (resp-msg "M") ; err-invalid-code
                   game-data)))))

; thread to hold game data, analyze messages, and respond appropriately.
(define (history)
  (define orig-game-data (hash))
  (define (loop game-data)
    (define channel-msg (channel-get conn-channel))
    (define new-game-data (check-message channel-msg game-data))
    (loop new-game-data))
  (loop orig-game-data))

; GARGABE COLLECTION

; notifies history thread every 10 seconds to clear out inactive games
(define (garbage-collect)
  (define (garbage-loop)
    (channel-put conn-channel "Garbage day!")
    (sleep 10)
    (garbage-loop))
  (garbage-loop))

; PARENT 

; accepts connections. Puts message and response function on channel.
(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (define (response msg)
    (display msg out)
    (close-input-port in)
    (close-output-port out))
  (channel-put conn-channel (hash "client-msg" (read-line in) "response" response)))


; listens for and accepts connections
(define (parent)
  (define listener (tcp-listen 6413 2 #t)) ;port 6413, max 2 connections waiting
  (define (loop)
    (accept-and-handle listener)
    (println "I got a connextion")
    (loop))
  (thread loop))

; used by client to kill server
(define (kill-all-server-threads thread-list)
  (map kill-thread thread-list))

; used by the client to start up a server for a game
(define (start-server)
  (define parent-thread (parent))
  (define garbage-collect-thread (thread garbage-collect))
  (define history-thread (thread history))
  (list parent-thread garbage-collect-thread history-thread))
