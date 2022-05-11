;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 3)
(define MISSILE-SPEED 10)

(define INVADE-RATE 3)      ;0 means - no new invaders per tick; 100 means - new invader every tick 

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit coverfi
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT (image-height TANK))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position
;;         all coordinates represent the position of the bottom-left corner

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1
;;         or stays still if dir 0

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 INVADER-X-SPEED))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT (* -1 INVADER-X-SPEED)))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) INVADER-X-SPEED)) ;> landed, moving right


#;
(define (fn-for-invader i)
  (... (invader-x i) (invader-y i) (invader-dx i)))

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty  (make-tank (- (/ WIDTH 2) (/ (image-width TANK) 2)) 0)))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. list of missiles

(define LOM0 empty) ;no missiles
(define LOM2 (list (make-missile 100 200) (make-missile 200 300))) ;list with 2 missiles

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Missile ListOfMissile)
;; - reference: (first lom) is Missile
;; - self-reference: (rest lom) is ListOfMissile


;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)

(define LOI0 empty) ;no invaders
(define LOI2 (list (make-invader 100 200 (* -1 INVADER-X-SPEED)) (make-invader 200 350 INVADER-X-SPEED))) ;2 invaders

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Invader ListOfInvader)
;; - reference: (first loi) is Invader
;; - self-reference: (rest loi) is ListOfInvader


;; =================
;; Functions:

;; Game -> Game
;; a game of space invaders
;; start the game with (main G0)
(define (main s)
  (big-bang s                    ; Game
    (on-tick   advance-game)     ; Game -> Game
    (to-draw   render)           ; Game -> Image
    (stop-when game-over)        ; Game -> Boolean
    (on-key    handle-key)))     ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game state
;; advance the positions of tanks, missiles, and invaders
;; remove invaders and missiles when they collide from respective lists
;; remove missiles that have left the screen from list of missiles
 
;(define (advance-game s) G0) ;stub

(define (advance-game s)
  (make-game
   (spawn-invaders (advance-invaders (destroy-invaders (game-invaders s) (game-missiles s))))
   (advance-missiles (destroy-missiles (game-missiles s) (game-invaders s)))
   (advance-tank (game-tank s))))


;; Game -> Image
;; render the invaders, missiles, and tank at the appropriate positions on BACKGROUND 
(check-expect (render (make-game empty empty (make-tank 50 1)))
              (place-image/align TANK 50 HEIGHT "left" "bottom" BACKGROUND))
(check-expect (render (make-game empty (list (make-missile 100 200)) (make-tank 50 1)))
              (place-image/align MISSILE 100 200 "left" "bottom"
                                 (place-image/align TANK 50 HEIGHT "left" "bottom" BACKGROUND)))
(check-expect (render (make-game (list (make-invader 150 100 1.5)) (list (make-missile 100 200)) (make-tank 50 1)))
              (place-image/align INVADER 150 100 "left" "bottom"
                                 (place-image/align MISSILE 100 200 "left" "bottom"
                                                    (place-image/align TANK 50 HEIGHT "left" "bottom" BACKGROUND))))

;(define (render s) BACKGROUND) ;stub

(define (render s)
  (render-invaders (game-invaders s)
                   (render-missiles (game-missiles s)
                                    (render-tank (game-tank s)))))


;; Game -> Boolean
;; produce true if an invader lands
(define (game-over s)
  (landed? (game-invaders s)))


;; Game KeyEvent -> Game
(define (handle-key s ke)
  (cond [(key=? ke " ") (make-game (game-invaders s) (fire-missile (game-missiles s) (tank-x (game-tank s))) (game-tank s))]
        [(key=? ke "left") (make-game (game-invaders s) (game-missiles s) (turn-left (game-tank s)))]
        [(key=? ke "right") (make-game (game-invaders s) (game-missiles s) (turn-right (game-tank s)))]
        [else s]))


;; ListOfMissile Number -> ListOfMissile
;; create a missile from appropriate position (align center-x of missile with center-x of tank) and add to list of missiles
;; remember that (x,y) of tanks, invaders, and missiles, represents their bottom-left corner 
(check-expect (fire-missile empty 20)
              (cons (make-missile (- (+ 20 (/ (image-width TANK) 2)) (/ (image-width MISSILE) 2)) (- HEIGHT (image-height TANK))) ;
                    empty))
(check-expect (fire-missile (list (make-missile 100 200) (make-missile 200 300)) 100)
              (cons (make-missile (- (+ 100 (/ (image-width TANK) 2)) (/ (image-width MISSILE) 2)) (- HEIGHT (image-height TANK)))
                    (list (make-missile 100 200) (make-missile 200 300)))) 

;(define (fire-missile lom n) lom) ;stub

(define (fire-missile lom n)
  (cons (make-missile (- (+ n (/ (image-width TANK) 2)) (/ (image-width MISSILE) 2)) (- HEIGHT (image-height TANK))) lom))


;; Tank -> Tank
;; turn tank to the left (dir = -1)
(check-expect (turn-left (make-tank 100 1)) (make-tank 100 -1))
(check-expect (turn-left (make-tank 100 0)) (make-tank 100 -1))
(check-expect (turn-left (make-tank 100 -1)) (make-tank 100 -1))

;(define (turn-left t) t) ;stub

;<use template from Tank>
(define (turn-left t)
  (make-tank (tank-x t) -1))

;; Tank -> Tank
;; turn tank to the right (dir = 1)
(check-expect (turn-right (make-tank 100 1)) (make-tank 100 1))
(check-expect (turn-right (make-tank 100 0)) (make-tank 100 1))
(check-expect (turn-right (make-tank 100 -1)) (make-tank 100 1))

;(define (turn-right t) t) ;stub

;<use template from Tank>
(define (turn-right t)
  (make-tank (tank-x t) 1))

  
;; ListOfInvader -> ListOfInvader
;; advance the movement of all invaders in the list at appropriate speed and direction
(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders LOI2)
              (list (make-invader (+ 100 (* -1 INVADER-X-SPEED))  (+ 200 INVADER-Y-SPEED) (* -1 INVADER-X-SPEED))
                    (make-invader (+ 200 INVADER-X-SPEED) (+ 350 INVADER-Y-SPEED) INVADER-X-SPEED)))
 
;(define (advance-invaders loi) loi) ;stub

;<use template from ListOfInvader>
(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader (first loi))
               (advance-invaders (rest loi)))]))


;; Invader -> Invader
;; move invader in appropriate speed and direction, flip direction when it hits a wall
(check-expect (move-invader (make-invader 150 100 INVADER-X-SPEED))                                                                      ;moving right, not touching right wall
              (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) INVADER-X-SPEED))
(check-expect (move-invader (make-invader (- WIDTH (image-width INVADER)) 100 INVADER-X-SPEED))                                          ;moving right, touching right wall
              (make-invader (+ (- WIDTH (image-width INVADER)) (* -1 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) (* -1 INVADER-X-SPEED)))
(check-expect (move-invader (make-invader 150 100 (* -1 INVADER-X-SPEED)))                                                               ;moving left, not touching left wall
              (make-invader (+ 150 (* -1 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) (* -1 INVADER-X-SPEED)))
(check-expect (move-invader (make-invader 0 100 INVADER-X-SPEED))                                                                        ;moving left, touching left wall
              (make-invader (+ 0 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) INVADER-X-SPEED))

;(define (move-invader i) i) ;stub

;<use template from Invader>
(define (move-invader i)
  (cond [(or
          (and
           (>= (+ (invader-x i) (image-width INVADER)) WIDTH) (> (invader-dx i) 0))
          (and
           (<= (invader-x i) 0) (< (invader-dx i) 0)))
         (make-invader (+ (invader-x i) (* -1 (invader-dx i))) (+ (invader-y i) INVADER-Y-SPEED) (* -1 (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))


;; ListOfInvader -> ListOfInvader
;; spawn new invaders with INVADE-RATE% chance each tick at a random
;; x-coordinate along the top of the screen and with random moving direction

;(define (spawn-invaders loi) loi) ;stub

(define (spawn-invaders loi)
  (cond [(< (random 100) INVADE-RATE)
         (cons (make-invader (random WIDTH) 0 (* (list-ref (list 1 -1) (random 2)) INVADER-X-SPEED)) loi)]
        [else loi]))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; remove invaders who have collided with missiles from list of invaders
(check-expect (destroy-invaders (list (make-invader 100 200 INVADER-X-SPEED) (make-invader 200 300 INVADER-X-SPEED))
                                (list (make-missile 100 190) (make-missile 300 20)))
              (list (make-invader 200 300 INVADER-X-SPEED)))

(check-expect (destroy-invaders (list (make-invader 10 30 INVADER-X-SPEED) (make-invader 100 200 INVADER-X-SPEED) (make-invader 200 300 INVADER-X-SPEED))
                                (list (make-missile 100 190) (make-missile 200 290)))
              (list (make-invader 10 30 INVADER-X-SPEED)))
                                

;(define (destroy-invaders loi lom) loi) ;stub

(define (destroy-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (hit-invader? (first loi) lom)
             (destroy-invaders (rest loi) lom)
             (cons (first loi) (destroy-invaders (rest loi) lom)))]))


;; Invader ListOfMissile -> Boolean
;; return true if invader is hit by any missile in lom
;; invader is considered hit when any of its sides is touched by any of the sides of a missile
;; or their areas overlap
(check-expect (hit-invader? (make-invader 100 200 INVADER-X-SPEED) (list (make-missile 300 20) (make-missile 100 180))) false) ;1 missile miss totally, 1 above invader
(check-expect (hit-invader? (make-invader 100 200 INVADER-X-SPEED) (list (make-missile 300 20) (make-missile  95 215))) true)  ;second missile top touches invader's bottom left
(check-expect (hit-invader? (make-invader 100 200 INVADER-X-SPEED) (list (make-missile 300 20) (make-missile  95 205))) true)  ;missile's right side touches invader's left side
(check-expect (hit-invader? (make-invader 100 200 INVADER-X-SPEED) (list (make-missile 300 20) (make-missile 120 215))) true)  ;missile top touch invader's bottom right
(check-expect (hit-invader? (make-invader 100 200 INVADER-X-SPEED) (list (make-missile 300 20) (make-missile 120 205))) true)  ;missile's left side touches invader's right side
(check-expect (hit-invader? (make-invader 100 200 INVADER-X-SPEED) (list (make-missile 300 20) (make-missile 105 205))) true)  ;area overlap

;(define (hit-invader? i lom) false) ;stub

(define (hit-invader? i lom)
  (cond [(empty? lom) false]
        [else
         (if (and
              (and (>= (+ (missile-x (first lom)) (image-width MISSILE)) (invader-x i))
                   (<= (missile-x (first lom)) (+ (invader-x i) (image-width INVADER))))
              (and (<= (- (missile-y (first lom)) (image-height MISSILE)) (invader-y i))
                   (>= (missile-y (first lom)) (- (invader-y i) (image-height INVADER)))))
             true
             (hit-invader? i (rest lom)))]))


;; ListOfMissile -> ListOfMissile
;; advance the movement of all missiles in the list
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list (make-missile 100 200) (make-missile 200 300)))
              (list (make-missile 100 (- 200 MISSILE-SPEED)) (make-missile 200 (- 300 MISSILE-SPEED))))

;(define (advance-missiles lom) empty) ;stub

;<use template from ListOfMissile>
(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (move-missile (first lom))
               (advance-missiles (rest lom)))]))


;; Missile -> Missile
;; move missile along the y-axis by MISSILE-SPEED px
(check-expect (move-missile (make-missile 150 HEIGHT)) (make-missile 150 (- HEIGHT MISSILE-SPEED)))

;(define (move-missile m) m) ;stub

;<use template from Missile>
(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; ListOfMissile ListOfInvader -> ListOfMissile
;; remove missiles that have hit an invader or have left the screen
(check-expect (destroy-missiles (list (make-missile 100 190) (make-missile 300 20))
                                (list (make-invader 100 200 INVADER-X-SPEED) (make-invader 200 300 INVADER-X-SPEED)))
              (list (make-missile 300 20)))
(check-expect (destroy-missiles (list (make-missile 100 -1) (make-missile 300 20) (make-missile 220 -15))
                                (list (make-invader 100 200 INVADER-X-SPEED) (make-invader 200 300 INVADER-X-SPEED)))
              (list (make-missile 300 20)))

;(define (destroy-invaders loi lom) loi) ;stub

(define (destroy-missiles lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (or (hit-missile? (first lom) loi) (outside? (first lom)))
             (destroy-missiles (rest lom) loi)
             (cons (first lom) (destroy-missiles (rest lom) loi)))]))

;; Missile ListOfInvader -> Boolean
;; return true if missile hits any invader in list
;; same considerations as hit-invader?

(check-expect (hit-missile? (make-missile 100 180) (list (make-invader 300 300 1.5) (make-invader 100 200 1.5))) false) ;missed
(check-expect (hit-missile? (make-missile 95 215) (list (make-invader 300 200 1.5) (make-invader 100 200 1.5))) true)   ;missile top touches second invader's bottom left
(check-expect (hit-missile? (make-missile 95 205) (list (make-invader 300 200 1.5) (make-invader 100 200 1.5))) true)   ;missile's right side touches invader's left side
(check-expect (hit-missile? (make-missile 120 215) (list (make-invader 300 200 1.5) (make-invader 100 200 1.5))) true)  ;missile top touch invader's bottom right
(check-expect (hit-missile? (make-missile 120 205) (list (make-invader 300 200 1.5) (make-invader 100 200 1.5))) true)  ;missile's left side touches invader's right side
(check-expect (hit-missile? (make-missile 105 205) (list (make-invader 300 200 1.5) (make-invader 100 200 1.5))) true)  ;area overlap

;(define (hit-missile? m loi) false) ;stub

(define (hit-missile? m loi)
  (cond [(empty? loi) false]
        [else
         (if (and
              (and (>= (+ (missile-x m) (image-width MISSILE)) (invader-x (first loi)))
                   (<= (missile-x m) (+ (invader-x (first loi)) (image-width INVADER))))
              (and (<= (- (missile-y m) (image-height MISSILE)) (invader-y (first loi)))
                   (>= (missile-y m) (- (invader-y (first loi)) (image-height INVADER)))))
             true
             (hit-missile? m (rest loi)))]))

;; Missile -> Boolean
;; return true if missile leaves the screen (y<0)
(check-expect (outside? (make-missile 100 0)) false)
(check-expect (outside? (make-missile 100 100)) false)
(check-expect (outside? (make-missile 100 -1)) true)

;(define (outside? m) false) ;stub

;<use template from Missile>
(define (outside? m)
  (if (< (missile-y m) 0)
      true
      false))

;; Tank -> Tank
;; advance tank TANK-SPEED px in the tank's current direction, stop at edges
;; when centre-x coordinate of the tank's cannon passes a screen edge
;; bring it back 1px and change the direction to 0
(check-expect (advance-tank (make-tank 100 1)) (make-tank (+ 100 (* 1 TANK-SPEED)) 1))         
(check-expect (advance-tank (make-tank 100 -1)) (make-tank (+ 100 (* -1 TANK-SPEED)) -1))
(check-expect (advance-tank (make-tank 100 0)) (make-tank (+ 100 (* 0 TANK-SPEED)) 0))
(check-expect (advance-tank (make-tank (- (+ WIDTH 1) (/ (image-width TANK) 2)) 1))          ;centre of cannon passed right edge
                            (make-tank (- WIDTH (/ (image-width TANK) 2)) 0))
(check-expect (advance-tank (make-tank (- (- 0 1) (/ (image-width TANK) 2)) -1))             ;centre of cannon passed left edge
                            (make-tank (- 0 (/ (image-width TANK) 2)) 0))

;(define (advance-tank t) t)

;<use template from Tank>
(define (advance-tank t)
  (cond [(>= (+ (tank-x t) (/ (image-width TANK) 2)) (+ WIDTH 1))
         (make-tank (- (tank-x t) 1) 0)]
        [(<= (+ (tank-x t) (/ (image-width TANK) 2)) -1)
         (make-tank (+ (tank-x t) 1) 0)]
        [else
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))

;; Tank -> Image
;; render the tank on BACKGROUND
(check-expect (render-tank (make-tank 50 1))
              (place-image/align TANK 50 HEIGHT "left" "bottom" BACKGROUND))

;(define (render-tank t) BACKGROUND) ;stub

;<use template from Tank>
(define (render-tank t)
  (place-image/align TANK (tank-x t) HEIGHT "left" "bottom" BACKGROUND))


;; ListOfMissile Image -> Image
;; render the missiles on img
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list (make-missile 10 30) (make-missile 40 50) (make-missile 50 60)) BACKGROUND)
              (place-image/align MISSILE 10 30 "left" "bottom"
                                 (place-image/align MISSILE 40 50 "left" "bottom"
                                                    (place-image/align MISSILE 50 60 "left" "bottom" BACKGROUND))))

;(define (render-missiles lom img) BACKGROUND) ;stub

;<use template from ListOfMissile with additional atomic parameter img>
(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (render-missiles (rest lom) (draw-missile (first lom) img))]))


;; Missile Image -> Image
;; render missile on img
(check-expect (draw-missile (make-missile 50 30) BACKGROUND)
              (place-image/align MISSILE 50 30 "left" "bottom" BACKGROUND))

;(define (draw-missile m img) BACKGROUND) ;stub

;<use template from Missile, with additionnal parameter img>
(define (draw-missile m img)
  (place-image/align MISSILE (missile-x m) (missile-y m) "left" "bottom" img))


;; ListOfInvader Image -> Image
;; render the invaders at appropriate place on img
(check-expect (render-invaders (list (make-invader 100 200 1.5) (make-invader 200 300 1.5) (make-invader 150 150 1.5)) BACKGROUND)
              (place-image/align INVADER 100 200 "left" "bottom"
                                 (place-image/align INVADER 200 300 "left" "bottom"
                                                    (place-image/align INVADER 150 150 "left" "bottom" BACKGROUND))))

;(define (render-invaders loi img) BACKGROUND) ;stub

;<use template from ListOfInvader with additional atomic parameter img>
(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (render-invaders (rest loi) (draw-invader (first loi) img))]))


;; Invader Image -> Image
;; render invader at appropriate place on img
(check-expect (draw-invader (make-invader 100 200 1.5) BACKGROUND)
              (place-image/align INVADER 100 200 "left" "bottom" BACKGROUND))

;(define (draw-invader i img) BACKGROUND) ;stub

;<use template from Invader, with additional atomic parameter img>
(define (draw-invader i img)
  (place-image/align INVADER (invader-x i) (invader-y i) "left" "bottom" img))


;; ListOfInvader -> Boolean
;; produce true if any invader has reached the bottom of the screen
(check-expect (landed? empty) false)
(check-expect (landed?
               (list (make-invader 200 300 1.5)
                     (make-invader 100 200 1.5)
                     (make-invader 100 HEIGHT 1.5)
                     (make-invader 2 3 1.5)))
              true)

;(define (landed? loi) false) ;stub

;<use template from ListOfInvader>
(define (landed? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (landed? (rest loi)))]))