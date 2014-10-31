(load "load-probscheme")
(load "util")

; Report on Probabilistic Language Scheme by Alexey Radul
; presented by Piotr

; What?
;  - probilistic language
;  - some probscheme specifics

; One already has "random" functions in languages, example, sum of two dice rolls:
(let ((roll1 (+ 1 (random 6)))
      (roll2 (+ 1 (random 6))))
  (+ roll1 roll2))

; The result of the computation "samples" the sum of two independent fair dice rolls.

; (1) But what if I wanted to determine the probability of, say, the result being 6 ?
;  -- could sample many times, and count times 6 occurs, divide by number of samples
;     -- produces an estimate

; (2) What if I wanted to enumerate all possibilities (along with their probabilities)
;   -- can be enumerated and estimated with sampling

; (3) What if I wanted to condition on the sum being at least 6 but otherwise be distributed as before.
;  -- sample many times, except throw away samples that fail the predicate >= 6
;     -- still estimate, danger of predicate being rarely satisfied (even worse estimate)

; (4) What if I wanted to infer something about the dice rolls given the sum? 
;  -- as in, how is roll1 distributed, given that the sum is at least 6
;     -- more estimation by sampling shenanigans



; Lets use a probabilistic language* instead !
;
;                                                                  * actually just scheme, with some help
;
; (1) probability of a certain outcome
;   -- ignore some probscheme details for now

;  equivalent:
;
;   (discrete-select (1 1/6) (2 1/6) (3 1/6) (4 1/6) (5 1/6) (6 1/6))
;
;   (uniform 1 6)

(define make-dice-sum ; ignore
  (lambda ()          ; ignore
    (let ((roll1 (uniform 1 6))
	  (roll2 (uniform 1 6)))
      (+ roll1 roll2))))

(define sum-dist (stochastic-thunk->distribution make-dice-sum)) ; ignore
(distribution/determine! sum-dist)                               ; ignore

(distribution/datum-probability sum-dist 6)
(distribution/datum-probability sum-dist 1)
(distribution/datum-probability sum-dist 7)
(distribution/datum-probability sum-dist 2)
(distribution/datum-probability sum-dist 12)



; (2) enumeration
; 
; (distribution->current-density-alist sumdist) gets you the possibilities along with their
; probabilities

(print-dist sum-dist)
(draw-dist sum-dist)

; (3) conditioning

(define make-cond-dice-sum
  (lambda ()
    (let ((sum (make-dice-sum)))
      (begin
	(observe! (>= sum 6))
	sum))))

(define cond-sum-dist (stochastic-thunk->distribution make-cond-dice-sum))
(distribution/determine! cond-sum-dist)

(print-dist cond-sum-dist)
(draw-dist cond-sum-dist)

; (not normalized)
;

(define cond-sum-dist-normal (distribution/normalize cond-sum-dist))
(distribution/determine! cond-sum-dist-normal)

(print-dist cond-sum-dist-normal)
(draw-dist cond-sum-dist-normal)

; (4) inference
;    -- lets determine how roll1 looks, given that the sum was >= 6

(define (make-make-infer-roll cval)
  (lambda ()
    (let* ((roll1 (uniform 1 6))
	   (roll2 (uniform 1 6))
	   (sum (+ roll1 roll2)))
      (begin
	(observe! (>= sum cval))
	roll1))))

(draw-dist (make-det-and-norm (make-make-infer-roll 6)))

(draw-dist (make-det-and-norm (make-make-infer-roll 2)))

(draw-dist (make-det-and-norm (make-make-infer-roll 12)))

(print-dist (make-det-and-norm (make-make-infer-roll 13)))

; -- Perhaps you can work out all the distributions above by hand easily, if you remember your
;    probability (who does?)



; -- A more interesting example

(define (coin-type)
  (discrete-select ('fair 1/3)
		   ('unfair-tails 1/3)
		   ('unfair-heads 1/3)
))

(define (flip-coin type)
  (cond
   ((equal? type 'fair) (discrete-select ('heads 1/2) ('tails 1/2)))
   ((equal? type 'unfair-tails) (discrete-select ('heads 1/3) ('tails 2/3)))
   ((equal? type 'unfair-heads) (discrete-select ('heads 2/3) ('tails 1/3)))
))

(define (make-fair type)
  (let ((flip1 (flip-coin 'unfair-tails))
	(flip2 (flip-coin 'unfair-tails)))
    (begin
      (observe! (not (equal? flip1 flip2)))
      flip1)))
    
(define (flip-num type amount flipper)
  (cond
   ((= amount 0) '())
   (else (cons (flipper type) (flip-num type (- amount 1) flipper)))))

(draw-dist (make-det-and-norm (lambda () (flip-num 'fair 3 flip-coin))))
(draw-dist (make-det-and-norm (lambda () (flip-num 'unfair-tails 3 make-fair))))
(draw-dist (make-det-and-norm (lambda () (flip-num 'unfair-heads 3 flip-coin))))

(draw-dist (make-det-and-norm (lambda () (flip-num (coin-type) 3 flip-coin))))

; lets try to infer whether a coin is fair or not:

(define (make-make-type-infer obs)
  (lambda ()
    (let* ((type (coin-type))
	   (result (flip-num type (length obs) flip-coin)))
      (begin
	(observe! (equal? result obs))
	type))))

(draw-dist (make-det-and-norm (make-make-type-infer '(heads))))
(draw-dist (make-det-and-norm (make-make-type-infer '(heads heads heads))))
(draw-dist (make-det-and-norm (make-make-type-infer '(heads heads heads heads heads heads heads heads heads heads heads heads))))

(draw-dist (make-det-and-norm (make-make-type-infer '(tails tails tails))))

; whereas without observation, we would know nothing:

(draw-dist (make-det-and-norm (make-make-type-infer '())))



; Privacy example
;
; Let us say we want to query some private information, which includes gender and birthyear.
; We have prior knowledge that gender is uniform between male and female, whereas
; birthyear is 3 times as likely to be between 1990 and 1994 than it is
; between 1985 and 1989, but otherwise uniform (unrealistic, but meh)

; (define (flip p) (discrete-select (#t p) (#f (- 1 p))))

(define (make-secret-belief)
  (let* ((byear (if (flip 1/4) (uniform 1985 1989) (uniform 1990 1994)))
	 (gender (if (flip 1/2) (if (> byear 1988) 'male 'female)
		     'male)))
    (list gender byear)))

(draw-dist (make-det-and-norm make-secret-belief))

; We have some queries, that return 0, 1, or 2 depending on the gender and byear

(define (query1 secret)
  (let ((gender (first secret))
	(byear (second secret)))
    (if (<= byear 1990) 0
	(if (equal? gender 'male) 1 2))))

(define (query2 secret)
  (let ((gender (first secret))
	(byear (second secret)))
    (if (equal? byear 1990) 0 1)))

(define (refine secret-maybe secret query)
  (let* ((out-maybe (query secret-maybe))   ; an uncertain value about the output of the query
	 (output (query secret)))           ; the actual output of the query
    (observe! (equal? out-maybe output))    ; observe the actual output
    secret-maybe))                          ; and return the belief (now consistent with actual output)

; query1

(query1 '(male 1989))

(draw-dist (make-det-and-norm
	    (lambda ()
	      (refine (make-secret-belief) '(male 1989) query1))))

(query1 '(male 1991))

(draw-dist (make-det-and-norm
	    (lambda ()
	      (refine (make-secret-belief) '(male 1991) query1))))

; query2

(query2 '(male 1989))

(draw-dist (make-det-and-norm
	    (lambda ()
	      (refine (make-secret-belief) '(male 1989) query2))))

(query2 '(male 1990))

(draw-dist (make-det-and-norm
	    (lambda ()
	      (refine (make-secret-belief) '(male 1990) query2))))

; -- Can measure information leakage via queries
; -- query2 is in some ways worse than query1 as it is possible for it to reveal the secret down
;    to two possibilities.



; Some details about probscheme
; -- code in scheme just like any other day (a scheme-coding day that is)
; -- create distribution via stochastic-thunk->distribution
;    -- can now reason about the distribution itself and query it
;       -- which is itself also done in scheme



; How?
; -- enumeration
;
;   (discrete-select (1 1/6) (2 1/6) (3 1/6) (4 1/6) (5 1/6) (6 1/6))
;
; -- produces continuation during evaluation by stochastic-thunk->distribution
;    -- results in one of the possibilities along with its probability, the rest are "scheduled"
;    -- more selections can be encountered, and further possibilities returned, and scheduled
;
; -- details unimportant (or I don't know) but effectively we have tree, nodes are selections
;    edges are their possibilities, leafs are results of the computation
;    -- evaluation of a statement that depends on many selects, is evaluated as plain
;       scheme given one possible set of values of the selects, can be done over and over
;       with different selections of random values, and can be exhaustively enumerated
;
; -- detail: how are the probabilities from two different discrete-select combined?
;    -- what is the distribution of sum in terms of the distributions of roll1 and roll2
;    -- all selects are independent, simply products of probabilities
;
;        P[roll1 = x and roll2 = y] = P[roll1 = x] * P[roll2 = y]
;
; -- detail: what about observe! ?
;    -- skip a branch if predicate fails, (also records probability mass of the branch, to discard)
; 
; -- laziness
;    -- use distribution/refine! to take into account another leaf in the tree
;    -- use distribution/determine! to fully explore the tree
;    -- if we can guarantee that low probability events will be explored later than sooner, we can
;       explore large distributions without looking at the small stuff
;       (or even distributions with infinitely many values)
;    -- can look for distribution properties that do not depend on having enumerated it all, done if
;       found early

(define (roll-die)
  (discrete-select (1 1/6) (2 1/6) (3 1/6) (4 1/6) (5 1/6) (6 1/6)))

(define make-dice-sum
  (lambda ()
    (let* ((roll1 (roll-die))
	   (roll2 (roll-die))
	   (sum (+ roll1 roll2)))
      (begin
	(observe! (> sum 3))
	sum))))

(define sum-dist (stochastic-thunk->distribution make-dice-sum))

(distribution/refine! sum-dist) ; repeat
(print-dist sum-dist)           ; repeat

; -- some issues, the probabilities are wrong until fully determined and normalized
;    -- can get estimates, however, can get bounds

(distribution/datum-min-probability sum-dist 4)
(distribution/datum-max-probability sum-dist 4)

(distribution/determine! sum-dist)

; -- what happens (first refinement)
;    - first (roll-die) evaluates to 1, probability set to 1/6, the remaining possibilities
;      are sheculed
;    - second (roll-die) evaluates to 1, probability set to 1/6 * 1/6, remains scheduled
;    - sum is evaluated, just scheme, roll1 = 1, roll2 = 1
;    - observe checks predicate on sum, fails
;      and this tree branch is done, records that 1/6 * 1/6 of probability mass is gone
; -- continued
;    - schedule contains another option for roll2, try it
;    - this time sum = 3, predicate fails again
; -- continued further still
;    - we get to roll2 = 3, now observe predicate holds
;    - the computation results in the value 4, this is recorded, and its mass of 1/36 is noted
; -- continued several times here
; -- another branch
;    - at some point we have roll1 = 3 and roll2 = 1
;    - predicate succeeds
;    - return value would be 4, this was already seen, its probability mass is increased by another 1/36
; -- and so on


; Get
;   -- reasoning about distributions
;      - (exact) probability of an outcome
;      - (exact) enumeration
;      - observation
;      - inference / revision
;  -- all this without having to think (hard) about it!

; Issues
;   -- enumeration
;      -- to get an exact, normalized view, one needs to enumerate all combinations of all possibilities for all
;         the selects (a lot)
;   -- no continuous distributions
;   -- simple
;      -- no exploitation of program stucture whatsoever
