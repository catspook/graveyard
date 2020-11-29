#lang racket/base

(require racket/tcp
         racket/string
         racket/list
         racket/bool
         racket/match)


; CHANNEL

(define conn-channel (make-channel))

; HISTORY

(define (destroy-game game-data game-name)
  (hash-remove game-data game-name))

(define (set-update-time game-data id name)
  (define game (hash-ref game-data name))
  (hash-set game-data name (hash-set game (string-join (list id "-lastl-updated") "") (current-seconds))))

(define (op-keepalive game-data id name pwd resp-msg)
  (resp-msg "A") ; op-keepalive
  (set-update-time game-data id name))

(define (op-update? game-data id name pwd resp-msg)
  (define s-last-move (hash-ref (hash-ref game-data name) "last-move")) ;stored as (list # #)
  (define who-moved-last (hash-ref (hash-ref game-data name) "who-moved-last"))
  (resp-msg (string-join (list "D" id name pwd (number->string (car s-last-move)) (number->string (last s-last-move)) who-moved-last) ":")) ; op-forward-update
  (set-update-time game-data id name))

(define (op-update game-data id name pwd resp-msg move-from move-to)
  (define game-to-update (hash-ref game-data name))
  (resp-msg (string-join (list "D" id name pwd (number->string move-from) (number->string move-to)) ":")) ; op-forward-update
  (set-update-time (hash-set game-data name (hash-set* game-to-update "last-move" (list move-from move-to) "who-moved-last" id)) id name))

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
                                                "last-move" (list 84 84)
                                                "who-moved-last" null
                                                "pwd" pwd
                                                "1-connected" #t
                                                "2-connected" null
                                                "1-last-updated" (current-seconds)
                                                "2-last-updated" null)))]))

(define (op-join game-data id name pwd resp-msg)
  (if (null? (hash-ref (hash-ref game-data name) "2-connected"))
      (let ([piece-to-player (hash-ref (hash-ref game-data name) "piece-to-player")]
            [pieces (hash-ref (hash-ref game-data name) "pieces")])
        (resp-msg (string-join (list "H" id name pwd pieces piece-to-player) ":")) ; op-forward-join
        (hash-set game-data name (hash-set* (hash-ref game-data name) "2-connected" #t "2-last-updated" (current-seconds))))
      (begin (resp-msg "R") ; err-3s-a-crowd
             game-data)))

(define (op-joined? game-data id name pwd resp-msg)
  (define is-2-connected? (hash-ref (hash-ref game-data name) "2-connected"))
  (cond
    [(null? is-2-connected?) (resp-msg (string-join (list "A" id name pwd) ":"))] ; op-keepalive
    [(equal? #t is-2-connected?) (let ([piece-to-player (hash-ref (hash-ref game-data name) "piece-to-player")]
                                       [pieces (hash-ref (hash-ref game-data name) "pieces")])
                                   (resp-msg (string-join (list "H" id name pwd pieces piece-to-player) ":")))] ; op-forward-join
    [(not is-2-connected?) (resp-msg "J")]) ; op-forward-leave
  (set-update-time game-data id name))

(define (credentials-ok? game-data name pwd)
  (println "credentials-ok?")
  (println game-data)
  (println name)
  (println pwd)
  (define stuff (and (hash-has-key? game-data name) (equal? (hash-ref (hash-ref game-data name) "pwd") pwd)))
  (println stuff)
  stuff)

(define (exec-opcode game-data code id name pwd msg1 msg2 msg3 resp-msg)
  (println "structure correct!")
  (if (or (equal? code "E") (credentials-ok? game-data name pwd)) ; if game is being created, credentials won't be stored yet
      (cond
        [(equal? code "A") (op-keepalive game-data id name pwd resp-msg)]
        [(equal? code "B") (op-update? game-data id name pwd resp-msg)] 
        [(equal? code "C") (op-update game-data id name pwd resp-msg (string->number msg1) (string->number msg2))] 
        [(equal? code "E") (op-create game-data id name pwd msg1 msg2 (string->number msg3) resp-msg)] 
        [(equal? code "G") (op-join game-data id name pwd resp-msg)] 
        [(equal? code "K") (op-joined? game-data id name pwd resp-msg)] 

        [(regexp-match-exact? #px"[M | O | P]" code) (destroy-game game-data name)]) ; FIX try putting this in a match, with this as an else
      (begin (resp-msg "P") ; err-wrong-credentials
             game-data)))

(define (match-opcode game-data client-msg resp-msg)
  (cond
    [(or (string-prefix? client-msg "B") (string-prefix? client-msg "C")) 
     (match (regexp-split #px":" client-msg) [(list code id name pwd msg1 msg2) (exec-opcode game-data code id name pwd msg1 msg2 null resp-msg)])]
    [(string-prefix? client-msg "E") (match (regexp-split #px":" client-msg) [(list code id name pwd msg1 msg2 msg3) (exec-opcode game-data code id name pwd msg1 msg2 msg3 resp-msg)])]
    [else (match (regexp-split #px":" client-msg) [(list code id name pwd) (exec-opcode game-data code id name pwd null null null resp-msg)])]))

(define (correct-structure client-msg)
  ; opcode : id : name : game-password : message
  (println "Checking structure...")
  (let [(regex-match (or (regexp-match-exact? #px"[A | B | M | O | P]:[1 | 2]:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}" client-msg) 
                         (regexp-match-exact? #px"C:[1 | 2]:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}:[0-8][0-4]:[0-8][0-4]" client-msg)
                         (regexp-match-exact? #px"E:1:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}:[L | V | Z | G | S | W | P]{32}:[1 | 2]{32}:[1 | 2]" client-msg)
                         (regexp-match-exact? #px"K:1:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}" client-msg)
                         (regexp-match-exact? #px"G:2:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}" client-msg)))]
    (println regex-match)
    regex-match))

(define (check-timeout current-game)
  (define 1-timeout? (>= (- (current-seconds) (hash-ref current-game "1-last-updated")) 60))
  (define 2-time (hash-ref current-game "2-last-updated"))
  (if (null? 2-time)
      1-timeout?
      (or 1-timeout? (>= (- (current-seconds) 2-time) 60))))

(define (remove-stale-games game-data name-list)
  (println "Garbage collecting old gams")
  (println name-list)
  (if (empty? name-list)
      game-data
      (let ([current-game (hash-ref game-data (car name-list))])
        (if (check-timeout current-game)
            (remove-stale-games (destroy-game game-data (car name-list)) (cdr name-list))
            (remove-stale-games game-data (cdr name-list))))))

; returns updated game data
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

(define (history)
  (define orig-game-data (hash))
  (define (loop game-data)
    (define channel-msg (channel-get conn-channel))
    (define new-game-data (check-message channel-msg game-data))
    (loop new-game-data))
  (loop orig-game-data))

(define history-thread (thread history))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (define (response msg)
    (display msg out)
    (close-input-port in)
    (close-output-port out))
  (channel-put conn-channel (hash "client-msg" (read-line in) "response" response)))

; GARGABE COLLECTION

(define (garbage-collect)
  (define (garbage-loop)
    (channel-put conn-channel "Garbage day!")
    (sleep 10)
    (garbage-loop))
  (garbage-loop))

(define garbage-collect-thread (thread garbage-collect))

; SERVER
; add a timeout
(define (parent)
  (define listener (tcp-listen 6413 2 #t)) ;port 6413, max 2 connections waiting
  (define (loop)
    (accept-and-handle listener)
    (println "I got a connextion")
    (loop))
  (thread loop))

(define parent-thread (parent))

(define (kill-all-server-threads)
  (lambda ()
    (kill-thread history-thread)
    (kill-thread parent-thread)))

; as soon as this module is loaded, these start
; something outside of this would call kill-all-server-threads
