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

(provide error-dialog)

(require (only-in racket/class
                  new
                  send)
         (only-in racket/gui/base
                  dialog%
                  button%)
         (prefix-in v: "view.rkt"))


(define error-dialog 
  (new dialog%
       [label "Error! This game is terminated."]
       [parent #f]
       [style '(close-button)]
       [enabled #t]
       [width 400]
       [height 100]))

(define close-button
  (new button%
       [parent error-dialog]
       [label "Okaaaay :("]
       [callback (lambda (button event)
                   (send error-dialog show #f)
                   (send v:game-window show #f)
                   (exit))]))

(define fine-button
  (new button%
       [parent error-dialog]
       [label "That's fine, I guess"]
       [callback (lambda (button event)
                   (send error-dialog show #f)
                   (send v:game-window show #f)
                   (exit))]))

