#lang racket
(require 2htdp/image 2htdp/universe test-engine/racket-tests)

;;Put your name and ULID here




(define GAME-HEIGHT 200) ;;Defines the height of the area for editing
(define GAME-WIDTH 200) ;;Same but for width
(define TANK-HEIGHT (- GAME-HEIGHT 15))
(define TANK-START-X (/ GAME-WIDTH 2))
(define UFO-START-HEIGHT 30)
(define UFO-START-X (/ GAME-WIDTH 2))
(define UFO-IMAGE (rectangle 30 30 "solid" "black"))
(define TANK-IMAGE (rectangle 20 20 "solid" "green"))
(define MISSILE-IMAGE (rectangle 5 10 "solid" "green"))

(define BACKGROUND (empty-scene GAME-WIDTH GAME-HEIGHT "white"))

;;point is a structure
;;(point Number Number)
;; Interpretation, it represents a cartesian point
;; of positive values. In rendering higher Y
;; means lower height in the scene.
(struct point [x y] #:transparent)

;; A UFO is a Point
;; interpretation (point x y) is the UFO's location
;; (using the top-down, left-to-right convention)
(define UFO-EX1 (point UFO-START-X UFO-START-HEIGHT))

;;Missiles move 30 units per tick
(define MISSILE-SPEED 30)
;; A Missile is a Point.
;; interpretation (point x y) is the missile's place
(define MISSILE-EX1 (point TANK-START-X (- TANK-HEIGHT 15)))


(define TANK-NEG-VEL -8)
(define TANK-POS-VEL 8)

;; A Tank is a structure:
;; (tank Number Number).
;; interpretation (tank x dx) specifies the position:
;; (x, HEIGHT) and the tank's speed: dx pixels/tick
(struct tank [loc vel] #:transparent)
(define TANK-EX1 (tank TANK-START-X 0))

;; aim is a structure
;; (aim UFO Tank)
;; Interpretation, UFO represents the UFO position
;; Tank represents the Tank position.
(struct aim [ufo tank] #:transparent)
(define AIM-EX1 (aim UFO-EX1 TANK-EX1))

;; aim is a structure
;; (aim UFO Tank)
;; Interpretation, UFO represents the UFO position
;; Tank represents the Tank position.
(struct fired [ufo tank missile] #:transparent)
(define FIRED-EX1 (fired UFO-EX1 TANK-EX1 MISSILE-EX1))

;; A SIGS is one of: 
;; – (aim UFO Tank)
;; – (fired UFO Tank Missile)
;; interpretation represents the complete state of a 
;; space invader game

;; Tank -> Image
;; Draw a tank on the background
(define (draw-tank tank-data)
  (place-image/align
        TANK-IMAGE
        (tank-loc tank-data)
        TANK-HEIGHT
        "center"
        "center" BACKGROUND))

;; UFO Image -> Image
;; After the tank is drawn,
;; draw the ufo into the given image
(define (draw-ufo ufo-data tank-drawn)
  (place-image/align
      UFO-IMAGE
      (point-x ufo-data)
      (point-y ufo-data)
      "center"
      "center"
      tank-drawn))

;; Missile Image -> Image
;; After the UFO and tank are drawn
;; pass the missile data for drawing
;; onto the image with the UFO and tank
(define (draw-missile missile-data ufo-drawn)
  (place-image/align
      MISSILE-IMAGE
      (point-x missile-data)
      (point-y missile-data)
      "center"
      "center"
      ufo-drawn))



;;SIGS -> Image
;;Renders the game by drawing either a tank and a ufo
;;or a tank, ufo, and missile
;;(check-expect (render AIM-EX1) ...)
(define (render game-state)
  (cond
    [(aim? game-state)
     (define tank-data (aim-tank game-state))
     (define ufo-data (aim-ufo game-state))
     (define tank-drawn (draw-tank tank-data))
     (draw-ufo ufo-data tank-drawn)]
    [(fired? game-state)
     (define tank-data (fired-tank game-state))
     (define ufo-data (fired-ufo game-state))
     (define missile-data (fired-missile game-state))
     (define tank-drawn (draw-tank tank-data))
     (define ufo-drawn (draw-ufo ufo-data tank-drawn))
     (draw-missile missile-data ufo-drawn)]))

(define (random-jump) (random -8 9))

;; UFO -> UFO
;; A Functional setter for ufos
;; Updates the x field in a point
(define (ufo-x-set ufo-data new-x) (point new-x (point-y ufo-data)))
(check-expect (ufo-x-set (point 0 0) 1) (point 1 0))

;; UFO -> UFO
;; A Functional setter for ufos
;; Updates the y field in a point
(define (ufo-y-set ufo-data new-y) (point (point-x ufo-data) new-y))
(check-expect (ufo-y-set (point 0 0) 1) (point 0 1))

;; UFO -> UFO
(define (move-ufo ufo-data)
  (ufo-x-set
   (ufo-y-set ufo-data (+ 2 (point-y ufo-data)))
   (+ (random-jump) (point-x ufo-data))))

;; Tank -> Tank
(define (move-tank tank-data)
  (let ([vel (tank-vel tank-data)])
    (tank (+ (tank-loc tank-data) vel) 0)))

;; Tank -> Tank
(define (tank-vel-set tank-data vel)
  (tank (tank-loc tank-data) vel))

;; Missile -> Missile
;; A Functional setter for missiles
;; We can reuse our ufo y-setter
;; since it is really a point setter
(define missile-y-set ufo-y-set)
(check-expect (missile-y-set (point 1 2) 3) (point 1 3))

;; Missile -> Missile
(define (move-missile missile-data) (missile-y-set missile-data (- (point-y missile-data) MISSILE-SPEED)))

;; GameState KeyEvent -> GameState
;; update the tanks velocity or shoot a missile
(define (key-handler game-state ke)
  (match* (game-state ke)
    ;;If the tank is aiming and space is pressed, move to fired and spawn a missile
    ([(aim ufo-data tank-data) " "]
     (fired ufo-data tank-data (point (tank-loc tank-data) (- TANK-HEIGHT 15))))
    ;;If the left arrow is pressed set velocity to -2 so that tank starts moving left
    ([(aim ufo-data tank-data) "left"]
     (aim ufo-data (tank-vel-set tank-data TANK-NEG-VEL)))    
    ([(fired ufo-data tank-data missile-data) "left"]
     (fired ufo-data (tank-vel-set tank-data TANK-NEG-VEL) missile-data))
    ;;If the right arrow is pressed set velocity to -2 so that tank starts moving right
    ([(aim ufo-data tank-data) "right"] 
     (aim ufo-data (tank-vel-set tank-data TANK-POS-VEL)))
    ([(fired ufo-data tank-data missile-data) "right"]
     (fired ufo-data (tank-vel-set tank-data TANK-POS-VEL) missile-data))
    ([g k] game-state)))

;; SIGS -> SIGS
;; Move the ufo and missiles as time goes on
(define (update game-state)
  (cond
    [(aim? game-state)
     (let* ([tank-data (aim-tank game-state)]
            [ufo-data (aim-ufo game-state)]
            [new-tank (move-tank tank-data)]
            [new-ufo (move-ufo ufo-data)])
       (aim  new-ufo new-tank))]
    [(fired? game-state)
     (let* ([tank-data (fired-tank game-state)]
            [ufo-data (fired-ufo game-state)]
            [missile-data (fired-missile game-state)]
            [new-tank (move-tank tank-data)]
            [new-ufo (move-ufo ufo-data)]
            [new-missile (move-missile missile-data)])
       (if (< (point-y new-missile) 0)
           (aim new-ufo new-tank)
           (fired new-ufo new-tank new-missile)))]))

;;UFO Missile -> Bool
;;Check if the missile is in the bounding box
;;of our UFO rectangle
(define (hit-ufo? ufo-data missile-data)
  (define ufo-x (point-x ufo-data))
  (define missile-x (point-x missile-data))
  (and
   (<= missile-x (+ ufo-x 15)) ;;a ufo has width 30 and rendering happens at center
   (>= missile-x (- ufo-x 15)) 
   (<= (point-y missile-data) (- (point-y ufo-data) 15))))

(define (ufo-bottom? ufo-data)
  (>= (+ (point-y ufo-data) 15)  (- TANK-HEIGHT 10))) ;; the tank image has height 20 and is centered

;; SIGS -> Bool
(define (end-game? game-state)
  (match game-state
    [(fired ufo-data tank-data missile-data)
     (or
      (hit-ufo? ufo-data missile-data)
      (ufo-bottom? ufo-data))]
    [(aim ufo-data tank-data)
     (ufo-bottom? ufo-data)]))

(define (run game-state)
  (big-bang game-state
            [on-tick update 0.1]
            [to-draw render]
            [on-key key-handler]
            [stop-when end-game?]))
