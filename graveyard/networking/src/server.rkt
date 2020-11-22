#lang racket/base

(require racket/tcp
         racket/string
         racket/list
         racket/bool
         racket/match)


; CHANNEL

(define conn-channel (make-channel))

; HISTORY

(define (credentials-ok? game-data game-name game-pwd)
  (println "credentials-ok?")
  (println game-data)
  (println game-name)
  (println game-pwd)
  (define stuff (and (hash-has-key? game-data game-name) (equal? (hash-ref (hash-ref game-data game-name) "pwd") game-pwd)))
  (println stuff)
  stuff)

(define (set-update-time game-data client-id game-name)
  (define game (hash-ref game-data game-name))
  (hash-set game-data game-name (hash-set game (string-join (list client-id "-lastl-updated") "") (current-seconds))))

(define (destroy-game game-data game-name)
  ; FIX:
  ; (kill-thread (hash-ref (hash-ref game-data game-name) "controller"))
  (hash-remove game-data game-name))

(define (op-keepalive game-data client-id game-name game-pwd response-template)
  (response-template (string-join (list "A" client-id game-name game-pwd ",") ":")) ; op-keepalive
  (set-update-time game-data client-id game-name))

(define (op-update? game-data client-id game-name game-pwd response-template client-move-from client-move-to)
  (define server-last-move (hash-ref (hash-ref game-data game-name) "last-move")) ;stored as (list # #)
  (define client-last-move (list client-move-from client-move-to))
  (if (not (equal? server-last-move client-last-move))
      (response-template (string-join (list "D" 
                                        client-id 
                                        game-name 
                                        game-pwd 
                                        (string-join (list (number->string (car server-last-move)) 
                                                       "," 
                                                       (number->string (last server-last-move))) "")) ":")) ; op-forward-update
      (response-template (string-join (list "A" client-id game-name game-pwd ",") ":"))) ; op-keepalive
  (set-update-time game-data client-id game-name))

(define (op-update game-data client-id game-name game-pwd response-template move-from move-to)
  (define game-to-update (hash-ref game-data game-name))
  (define game-controller (hash-ref game-to-update "controller"))
  ; FIX: update controller with new move
  (if #t ; FIX: if controller updated successfully
      (begin (response-template (string-join (list "D" 
                                               client-id 
                                               game-name 
                                               game-pwd 
                                               (string-join (list (number->string move-from) 
                                                              "," 
                                                              (number->string move-to)) ""))  ":")) ; op-forward-update
             (hash-set game-data game-name (hash-set game-to-update "last-move" (list move-from move-to))))
      (begin (response-template "O") ; err-invalid-move
             (set-update-time game-data client-id game-name))))

(define (op-create game-data client-id game-name game-pwd response-template)
  (println "creating a game...")
  (println game-data)
  (println game-name)
  (if (not (hash-has-key? game-data game-name))
      (if (< (hash-count game-data) 5)
          (let* ([controller "controller"] ;FIX
                 [game-pieces "VGSL..."]
                 [piece-player "121212..."])
            (response-template (string-join (list "F" client-id game-name game-pwd game-pieces piece-player) ":")) ; op-game-created
            (hash-set game-data game-name (hash "controller" controller
                                                     "last-move" (list 99 99)
                                                     "pwd" game-pwd
                                                     "1-connected" #t
                                                     "2-connected" null
                                                     "1-last-updated" (current-seconds)
                                                     "2-last-updated" null)))
          (begin (response-template "T") ; err-too-many-games
                 game-data))
      (begin (response-template "Q") ; err-invalid-name
             game-data)))

(define (op-join game-data client-id game-name game-pwd response-template)
  (if (null? (hash-ref (hash-ref game-data game-name) "2-connected"))
      (begin (response-template (string-join (list "H" client-id game-name game-pwd ",") ":")) ; op-forward-join
             (hash-set game-data game-name (hash-set* (hash-ref game-data game-name) "2-connected" #t
                                                                                     "2-last-updated" (current-seconds))))
      (begin (response-template "R") ; err-3s-a-crowd
             game-data)))

(define (op-leave game-data client-id game-name game-pwd response-template)
  (response-template (string-join (list "J" client-id game-name game-pwd ",") ":")) ; op-forward-leave
  (hash-set game-data game-name (hash-set* (hash-ref game-data game-name) (string-join (list client-id "-connected") "") #f 
                                                                          (string-join (list client-id "-last-updated") "") null)))

(define (op-joined? game-data client-id game-name game-pwd response-template)
  (define is-2-connected? (hash-ref (hash-ref game-data game-name) "2-connected"))
  (cond
    [(null? is-2-connected?) (response-template (string-join (list "A" client-id game-name game-pwd ",") ":"))] ; op-keepalive
    [(equal? #t is-2-connected?) (response-template (string-join (list "H" client-id game-name game-pwd ",") ":"))] ; op-forward-join
    [(not is-2-connected?) (response-template (string-join (list "J" client-id game-name game-pwd ",") ":"))]) ; op-forward-leave
  (set-update-time game-data client-id game-name))

(define (correct-structure client-msg)
  ; opcode : client-id : game-name : game-password : message
  (println "Checking structure...")
  (let [(regex-match (or (regexp-match-exact? #px"[A | I | M | O | P]:[1 | 2]:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}:," client-msg) 
                         (regexp-match-exact? #px"[B | C]:[1 | 2]:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}:[0-9][0-9]?,[0-9][0-9]?" client-msg)
                         (regexp-match-exact? #px"[E | K]:1:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}:," client-msg)
                         (regexp-match-exact? #px"G:2:([A-Za-z0-9]){1,20}:([A-Za-z0-9]){1,20}:," client-msg)))]
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
        (let ([no-connections (and (false? (hash-ref current-game "1-connected"))
                                   (or (null? (hash-ref current-game "2-connected")) (false? (hash-ref current-game "2-connected"))))])
          (if (or no-connections (check-timeout current-game))
              (remove-stale-games (destroy-game game-data (car name-list)) (cdr name-list))
              (remove-stale-games game-data (cdr name-list)))))))

(define (find-opcode game-data code client-id game-name game-pwd msg response-template)
  (println "structure correct!")
  (let ([moves (map string->number (regexp-split #px"," msg))])
    (if (or (equal? code "E") (credentials-ok? game-data game-name game-pwd)) ; if game is being created, credentials won't be stored yet
        (cond
          [(equal? code "A") (op-keepalive game-data client-id game-name game-pwd response-template)]
          [(equal? code "B") (op-update? game-data client-id game-name game-pwd response-template (first moves) (last moves))] 
          [(equal? code "C") (op-update game-data client-id game-name game-pwd response-template (first moves) (last moves))] 
          [(equal? code "E") (op-create game-data client-id game-name game-pwd response-template)] 
          [(equal? code "G") (op-join game-data client-id game-name game-pwd response-template)] 
          [(equal? code "I") (op-leave game-data client-id game-name game-pwd response-template)] 
          [(equal? code "K") (op-joined? game-data client-id game-name game-pwd response-template)] 
  
          [(regexp-match-exact? #px"[M | O | P]" code) (destroy-game game-data game-name)]) ; FIX try putting this in a match, with this as an else
        (begin (response-template "P") ; err-wrong-credentials
               game-data))))

; returns updated game data
(define (check-message channel-msg game-data)
  (if (equal? channel-msg "Garbage day!")
      (remove-stale-games game-data (hash-keys game-data))
      (let ([client-msg (hash-ref channel-msg "client-msg")]
            [response-template (hash-ref channel-msg "response-template")])
        (println "checkmsg")
        (println channel-msg)
        (println (hash-ref channel-msg "client-msg"))
        (println (regexp-split #px":" client-msg))
        (if (correct-structure client-msg)
            (match (regexp-split #px":" client-msg) [(list code client-id game-name game-pwd msg) (find-opcode game-data code client-id game-name game-pwd msg response-template)])
            (begin (response-template "M") ; err-invalid-code
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
  (channel-put conn-channel (hash "client-msg" (read-line in) "response-template" response)))

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
