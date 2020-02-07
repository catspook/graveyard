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

(require rackunit
         rackunit/text-ui
         (only-in racket/list
                  take)
         "../../utils.rkt"
         (prefix-in r: "../../models/roles.rkt")
         (prefix-in b: "../../models/board.rkt")
         )

(define player1 (car r:players))
(define player2 (cdr r:players))


;; Test gen-board - check tests working
(define test-board (b:gen-board))

(define gen-board-tests
  (test-suite "Test board generation"
              ;; board created w/ 32 r:cells
              (check-equal? (length test-board)
                            32)
              ;; all r:cells start out hidden
              (check-equal? (length (filter (lambda (x)
                                             (r:cell-revealed? x))
                                            test-board))
                            0)
              ;; player count split in half
              (check-equal? (length (filter (lambda (x)
                                              (eq? (r:cell-player x)
                                                   player1))
                                            test-board))
                            16)))

(run-tests gen-board-tests)