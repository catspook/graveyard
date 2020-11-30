;; Copyright 2019 Thea Leake; networking update Copyright 2020 Casper Rutz

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

(provide start-server-dialogue
         set-server-threads!)

(require (only-in racket/class
                  new
                  send)
         (only-in racket/gui/base
                  dialog%
                  button%)
         (prefix-in s: "../networking/src/server.rkt"))


(define start-server-dialogue
  (new dialog%
       [label "Server is running."]
       [parent #f]
       [style '(close-button)]
       [enabled #t]
       [width 400]
       [height 100]))

(define server-threads (list))

(define (set-server-threads! thread-list)
  (set! server-threads thread-list))

(define kill-server-button 
  (new button%
       [parent start-server-dialogue]
       [label "Kill Server"]
       [callback (lambda (button event)
                   (s:kill-all-server-threads server-threads)
                   (send start-server-dialogue show #f))]))

