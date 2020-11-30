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

(provide remote-start-dialog)

(require (only-in racket/class
                  new
                  send)
         (only-in racket/gui/base
                  dialog%
                  button%
                  text-field%)
         (prefix-in v: "view.rkt")
         (prefix-in ctrl: "../controllers/controller.rkt"))


(define remote-start-dialog 
  (new dialog%
       [label "Do you want to create or join a remote game?"]
       [parent #f]
       [style '(close-button)]
       [enabled #t]
       [width 400]
       [height 100]))

(define name-entry-field
  (new text-field%
       [label "Game Name:"]
       [parent remote-start-dialog]))

(define pwd-entry-field
  (new text-field%
       [label "Game Password:"]
       [parent remote-start-dialog]
       [style (list 'password 'single)]))

(define create-button
  (new button%
       [parent remote-start-dialog]
       [label "Create"]
       [callback (lambda (button event)
                   (send remote-start-dialog show #f)
                   (send v:game-window show #t)
                   (ctrl:remote-player-create (send name-entry-field get-value) (send pwd-entry-field get-value)))]))

(define join-button
  (new button%
       [parent remote-start-dialog]
       [label "Join"]
       [callback (lambda (button event)
                   (send remote-start-dialog show #f)
                   (send v:game-window show #t)
                   (ctrl:remote-player-join (send name-entry-field get-value) (send pwd-entry-field get-value)))]))
