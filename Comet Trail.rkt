;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |change into stars in nightsky|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)

;; Comet Debris Trail 

;; =================
;; Constants:
(define WIDTH 400)                                          ; width of scene
(define HEIGHT 200)                                         ; height of scene
(define MTS (rectangle WIDTH HEIGHT "solid" "black"))       ; empty scene
(define SPEED 6)                                           ; speed of decreasing opacity
(define FULL-OPACITY 255)                                   ; opacity starts at 255 and decreases from there
(define DOT-RADIUS (* WIDTH 0.0125))                        ; radius of debris dot
(define DOT-COLOR "white")                                  ; color of debris dot
(define DOT (circle (* WIDTH 0.0125) FULL-OPACITY "white")) ; the trail dots' initial appearance

;; =================
;; Data definitions:

;; ListOfDot represents dots making up comet debris trail

(define-struct dot (x y o))
;; Dot is (make-dot Number[0,WIDTH] Number[0,HEIGHT] Natural[0,FULL-OPACITY])
;; interp.  (make-dot x y o) is the debris dot where:
;; - x is the x coordinate in pixels
;; - y is the y coordinate in pixels
;; - o is the opacity of the dot

(define D1 (make-dot 0 0 FULL-OPACITY))                     ; debris dot in upper left corner with full opacity
(define D2 (make-dot (/ WIDTH 2) (/ HEIGHT 2) 100))         ; debris dot in middle of scene with 100/255 opacity

#;
(define (fn-for-dot d)
  (... (dot-x d)     ; Number[0,WIDTH]
       (dot-y d)     ; Number[0,HEIGHT]
       (dot-o d)))   ; Natural[0, OPACITY]

;; Template Rules Used:
;; - compound: 3 fields
;;  (make-dot Number[0,WIDTH] Number[0,HEIGHT] Natural[0,OPACITY])

;; ListOfDot is one of:
;; - empty
;; - (cons Dot ListOfDot)
;; interp. a list of dots that make up the trail
(define LD1 empty)
(define LD2 (cons (make-dot 0 0 FULL-OPACITY) (cons (make-dot (/ WIDTH 2) (/ HEIGHT 2) 100) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else 
         (... (fn-for-dot (first lod))     ;Dot
              (fn-for-lod (rest lod)))]))  ;ListofDot

;; Template Rules Used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Dot ListofDot)
;; - reference: (first lod) is Dot
;; - self-reference: (rest lod) is ListofDot


;; =================
;; Functions:

;; ListOfDot -> ListOfDot
;; Start the world with (main empty)
;; 
(define (main lod)
  (big-bang lod                             ; ListOfDot
            (on-tick   next-dot)            ; ListOfDot -> ListOfDot
            (to-draw   render-dot)          ; ListOfDot -> Image
            (on-mouse  handle-mouse)))      ; ListOfDot Integer Integer MouseEvent -> ListOfDot

;; ListOfDot -> ListOfDot
;; Produce appearance of the next dot

(check-expect (next-dot empty) empty)
(check-expect (next-dot (cons (make-dot 0 0 FULL-OPACITY) (cons (make-dot (/ WIDTH 2) (/ HEIGHT 2) 100) empty)))
              (cons (make-dot 0 0 (- FULL-OPACITY SPEED)) (cons (make-dot (/ WIDTH 2) (/ HEIGHT 2) (- 100 SPEED)) empty)))


;(define (next-dot lod) lod) ;stub

;; <Used template from ListofDot>

(define (next-dot lod)
  (cond [(empty? lod) empty]
        [else 
         (cons (disappear (first lod))
               (next-dot (rest lod)))]))

;; Dot -> Dot
;; Produces the new opacity of the dot decreasing by SPEED
(check-expect (disappear (make-dot (/ WIDTH 2) (/ HEIGHT 2) 100)) (make-dot (/ WIDTH 2) (/ HEIGHT 2) (- 100 SPEED)))
(check-expect (disappear (make-dot (/ WIDTH 2) (/ HEIGHT 2) 5)) (make-dot (/ WIDTH 2) (/ HEIGHT 2) 0))
;(define (disappear d) d) ;stub

;; <Used template from Dot>
(define (disappear d)                                     ;helper function
  (make-dot (dot-x d)
            (dot-y d)
            (if (>= (- (dot-o d) SPEED) 0) (- (dot-o d) SPEED) 0)))



;; ListOfDot -> Image
;; Render dots onto empty scene 

(check-expect (render-dot empty) MTS)
(check-expect (render-dot (cons (make-dot 0 0 FULL-OPACITY) (cons (make-dot (/ WIDTH 2) (/ HEIGHT 2) 100) empty)))
              (place-image (circle (* WIDTH 0.0125) FULL-OPACITY "white") 0 0
                           (place-image (circle (* WIDTH 0.0125) 100 "white") (/ WIDTH 2) (/ HEIGHT 2) MTS)))

;(define (render-dot lod) MTS) ;stub

;; <Used template from ListofDot>
(define (render-dot lod)
  (cond [(empty? lod) MTS]
        [else 
         (place-dot (first lod) (render-dot (rest lod)))]))

;; Dot Image -> Image
;; Place dot in the position given by d with the opacity given by d on background
(check-expect (place-dot (make-dot (/ WIDTH 2) (/ HEIGHT 2) 100) MTS)
              (place-image (circle (* WIDTH 0.0125) 100 "white") (/ WIDTH 2) (/ HEIGHT 2) MTS))
(check-expect (place-dot (make-dot 0 0 FULL-OPACITY) (place-dot (make-dot (/ WIDTH 2) (/ HEIGHT 2) 100) MTS))
              (place-image (circle (* WIDTH 0.0125) FULL-OPACITY "white") 0 0
                           (place-image (circle (* WIDTH 0.0125) 100 "white") (/ WIDTH 2) (/ HEIGHT 2) MTS)))
;(define (place-dot d img) MTS) ;stub

;; <Used template from Dot>
;; Added extra parameter, img
(define (place-dot d img)                                                              ;helper function
  (place-image (circle (* WIDTH 0.0125) (dot-o d) "white") (dot-x d) (dot-y d) img))

;; ListOfDot Integer Integer MouseEvent -> ListOfDot
;; if me is "button-down", a single new dot appears at that position
;;  - all dots (if any present before "button-down") will be erased
;; when the mouse is dragged, a trail of dots is created at the positions where the mouse dragged across
(check-expect (handle-mouse empty (/ WIDTH 2) (/ HEIGHT 2) "button-down")
              (cons (make-dot (/ WIDTH 2) (/ HEIGHT 2) FULL-OPACITY) empty))
(check-expect (handle-mouse (make-dot (/ WIDTH 2) (/ HEIGHT 2) 100) 0 0 "button-down")
              (cons (make-dot 0 0 FULL-OPACITY) empty))
(check-expect (handle-mouse empty 5 10 "move") empty)
(check-expect (handle-mouse empty 5 10 "drag")
              (cons (make-dot 5 10 FULL-OPACITY) empty))
(check-expect (handle-mouse (cons (make-dot 5 10 FULL-OPACITY) empty) 6 11 "drag")
              (cons (make-dot 6 11 FULL-OPACITY) (cons (make-dot 5 10 FULL-OPACITY) empty)))
;(define (handle-mouse lod x y me) lod) ;stub

(define (handle-mouse lod x y me)
  (cond [(mouse=? me "button-down") (cons (make-dot x y FULL-OPACITY) empty)]
        [(mouse=? me "drag") (cons (make-dot x y FULL-OPACITY) lod)]
        [else lod]))


