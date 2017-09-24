#lang racket
(require gigls/unsafe)

;;; Procedure:
;;;   image-series
;;; Parameters:
;;;   n, a positive integer
;;;   width, a positive integer
;;;   height, a positive integer
;;; Purpose:
;;;   to generate the nth image in a series of 1001 related images
;;; Produces:
;;;   result, an image
;;; Preconditions:
;;;   0 <= n <= 1000
;;; Postconditions:
;;;  for every different value of n a diferent image should be made
;;;  the image should scale in proportion to the values of width and height
;;;  as in a 100x100 image with n 247 should look the same as 1000x1000 with n 247
;;;  except scaled 10 times smaller. Since the brush of the turtle does not scale
;;;  with the image in smaller images the turtle drawings are not the same as in larger images.
(define image-series
  (lambda (n width height)
    (let* ([img (canvas width height n)]
           [k (modulo n 250)] ;k has been set to cycle from values 0-249 over and over again
           [hscale (* width (cos (/ k 178)))]
           [vscale (* height (cos (/ k 178)))]
           [turtle-size (/ (+ (* 0.05 width) (* 0.05 height)) 5)]
           [m (modulo n 8)] ;m has been set to cycle from values 0-7 over and over again
           [z (modulo n 7)] ;z has been set to cycle from values 0-6 over and over again
           [p (+ 3 (modulo n 4))]) ;p has been set to cycle from values 3-6 over and over again
      (map (section copy-trail width height <> 0 img) (iota m))
      (map (section turtle-procedure img width height <> turtle-size p k) (iota m))
      (abstract-object hscale vscale (- width (* .5 hscale)) (- height (* .5 vscale)) img z)
      (rotation img n))))
;;; the values of k,m,z and p are used in some of the codes that follow,
;;; in such code the parameters are given as k,m,z and p to identify them easily.


;;; Procedure:
;;;   canvas
;;; Parameters:
;;;   width, a positive integer
;;;   height, a positive integer
;;;   n, a positive integer
;;; Purpose:
;;;   To produce an image using image-compute depending on the value of n
;;; Produces:
;;;   result, an image
;;; Preconditions:
;;;   0 <= n <= 1000
;;; Postconditions:
;;;  if 0 <= n <= 249 then it should use the func 
;;;  (irgb  n  (- 255 (* col (/ 256 (- width 1))))
;;;  (- 255(* row (/ 256 (- height 1))))) with image-compute
;;;  if 250 <= n <= 499 then it should use the func 
;;;  (irgb (- 255 (* col (/ 256 (- width 1)))) 
;;;  (- n 250) (- 255(* row (/ 256 (- height 1))))) with image-compute
;;;  if 500 <= n <= 749 then it should use the func 
;;;  (irgb (- 255 (* col (/ 256 (- width 1))))  
;;;  (- 255(* row (/ 256 (- height 1)))) (- n 500)) with image-compute
;;;  if 750 <= n <= 1000 then it should use the func 
;;;  (irgb 0 (- 255(* row (/ 256 (- height 1)))) (- n 750)) with image-compute
(define canvas
  (lambda (width height n)
    (cond [(and (> n -1) (< n 250)) (image-compute
                                     (lambda (col row) 
                                       (irgb  n  
                                              (- 255 (* col (/ 256 (- width 1)))) 
                                              (- 255(* row (/ 256 (- height 1)))))) 
                                     width height)]
          [(and (> n 249) (< n 500)) (image-compute
                                      (lambda (col row)
                                        (irgb (- 255 (* col (/ 256 (- width 1))))
                                              (- n 250)
                                              (- 255(* row (/ 256 (- height 1))))))
                                      width height)]
          [(and (> n 499) (< n 750)) (image-compute
                                      (lambda (col row)
                                        (irgb (- 255 (* col (/ 256 (- width 1)))) 
                                              (- 255(* row (/ 256 (- height 1))))
                                              (- n 500))) width height)]
          
          [(and (> n 749) (< n 1001)) (image-compute
                                       (lambda (col row)
                                         (irgb 0
                                               (- 255(* row (/ 256 (- height 1))))
                                               (- n 750))) width height)])))

;;; Procedure:
;;;   rotation
;;; Parameters:
;;;   image, an image
;;;   n, a positive value
;;; Purpose:
;;;   to rotate a given image depending on the value of n
;;; Produces:
;;;   result, an image
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;  if 250 <= n <= 499 then the image should be rotated 90 degrees anti-clockwise
;;;  if 500 <= n <= 749 then the image should be rotated 180 degrees anti-clockwise
;;;  if 750 <= n <= 1000 then the image should be rotated 270 degrees anti-clockwise
(define rotation
  (lambda (image n)
    (cond [(and (> n 249) (< n 500)) (gimp-image-rotate image 2)]
          [(and (> n 499) (< n 750)) (gimp-image-rotate image 1)]
          [(and (> n 749) (< n 1001)) (gimp-image-rotate image 0)])image))

;;; Procedure:
;;;   trail
;;; Parameters:
;;;   trail-width, a positive integer
;;;   height, a positive real number
;;;   n, a positive integer
;;; Purpose:
;;;   to create an image using image-compute depeding on the value of m
;;; Produces:
;;;  result, an image
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;  the image should be of width trail-width
;;;  the image should be of width (round (* height (cos (/ (+ m 1) 5))))
(define trail
  (lambda (trail-width height m)
    (let ([trail-height (round (* height (cos (/ (+ m 1) 5))))])
      (cond 
        [(= m 0)
         (image-compute 
          (lambda (col row)
            (irgb (* 255 (/ row trail-height)) 
                  (* 255 (/ row trail-height)) 
                  (* 255 (/ row trail-height)))) 
          trail-width trail-height)]
        
        [(= m 1)
         (image-compute 
          (lambda (col row)
            (irgb  0 
                   (* 255 (/ row trail-height)) 
                   0))
          trail-width  trail-height)]
        
        [(= m 2)
         (image-compute 
          (lambda (col row)
            (irgb  0
                   0
                   (* 255 (/ row trail-height))))
          trail-width trail-height)]
        
        [(= m 3)
         (image-compute 
          (lambda (col row)
            (irgb  0 
                   (* 255 (/ row trail-height)) 
                   (* 255 (/ row trail-height))))
          trail-width trail-height)]
        
        [(= m 4)
         (image-compute
          (lambda (col row)
            (irgb (* 255 (/ row trail-height))
                  0 
                  0))
          trail-width trail-height)]
        
        [(= m 5)
         (image-compute 
          (lambda (col row)
            (irgb (* 255 (/ row trail-height)) 
                  (* 255 (/ row trail-height)) 
                  0)) 
          trail-width trail-height)]
        
        [(= m 6)
         (image-compute 
          (lambda (col row)
            (irgb  (* 255 (/ row trail-height))
                   0 
                   (* 255 (/ row trail-height))))
          trail-width trail-height)]))))

;;; Procedure:
;;;   copy-trail
;;; Parameters:
;;;   width, a postive integer
;;;   height, a postive integer
;;;   n, a postive integer
;;;   top, a postive integer
;;;   target, an image
;;; Purpose:
;;;   to copy an image onto another image (target)
;;; Produces:
;;;  result, nothing a side-effect
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;  the copied image should have coordinates
;;;  left = (* (/ width 14) (* 2 m))
;;;  top = top
;;;  width = width of img
;;;  height = height of img

(define copy-trail
  (lambda (width height m top target)
    (let ([img (trail (round (/ width 28)) height m)])
      (image-select-all! img)
      (gimp-edit-copy-visible img)
      (image-select-rectangle! target 
                               REPLACE 
                               (* (/ width 14) (* 2 m))
                               top 
                               (car(gimp-image-width img)) 
                               (car (gimp-image-height img)))
      (gimp-edit-paste (image-get-layer target) 1)
      (image-select-nothing! target)
      (gimp-image-flatten target))))

;;; Procedure:
;;;   abstract-object-helper
;;; Parameters:
;;;   width, a positive integer
;;;   height, a positive integer
;;;   x-shift, any real number
;;;   y-shift, any real number
;;;   z, a positive integer
;;; Purpose:
;;;   To produce a drawing depending on the value of z
;;; Produces:
;;;  drawing, a drawing
;;; Preconditions:
;;;   z is between 0 and 6 inclusive.
;;; Postconditions:
;;;  If z=0, the drawing should meet the drawing requirements of the first
;;;  condition
;;;  if z=1, the drawing should meet the drawing requirements of the second 
;;;  condition
;;;  if z=2, the drawing should meet the drawing requirements of the third 
;;;  condition
;;;  if z=3, the drawing should meet the drawing requirements of the fourth 
;;;  condition
;;;  if z=4, the drawing should meet the drawing requirements of the fifth
;;;  condition
;;;  if z=5, the drawing should meet the drawing requirements of the sixth 
;;;  condition
;;;  if z=6, the drawing should meet the drawing requirements of the seventh
;;;  condition

(define abstract-object-helper
  (lambda (width height x-shift y-shift z)
    (let ([size1 (vscale-drawing height 
                                 (hscale-drawing width 
                                                 drawing-unit-circle))]
          
          [size2 (vscale-drawing (/ (* 6 height) 7) 
                                 (hscale-drawing (/ (* 6 width) 7) 
                                                 drawing-unit-circle))]
          
          [size3 (vscale-drawing (/ (* 5 height) 7) 
                                 (hscale-drawing (/ (* 5 width) 7) 
                                                 drawing-unit-circle))]
          
          [size4 (vscale-drawing (/ (* 4 height) 7) 
                                 (hscale-drawing (/ (* 4 width) 7) 
                                                 drawing-unit-circle))]
          
          [size5 (vscale-drawing (/ (* 3 height) 7) 
                                 (hscale-drawing (/ (* 3 width) 7) 
                                                 drawing-unit-circle))]
          
          [size6 (vscale-drawing (/ (* 2 height) 7) 
                                 (hscale-drawing (/ (* 2 width) 7) 
                                                 drawing-unit-circle))]
          
          [size7 (vscale-drawing (/ (* 1 height) 7) 
                                 (hscale-drawing (/ (* 1 width) 7) 
                                                 drawing-unit-circle))])
      (cond [(= 0 z)
             (vshift-drawing y-shift 
                             (hshift-drawing x-shift  
                                             (drawing-group
                                              (recolor-drawing "red" size1)
                                              (recolor-drawing "orange" size2)
                                              (recolor-drawing "yellow" size3)
                                              (recolor-drawing "green" size4)
                                              (recolor-drawing "blue" size5)
                                              (recolor-drawing "indigo" size6)
                                              (recolor-drawing "violet" size7))))]
            [(= 1 z)
             (vshift-drawing y-shift 
                             (hshift-drawing x-shift  
                                             (drawing-group
                                              (recolor-drawing "violet" size1)
                                              (recolor-drawing "green" size2)
                                              (recolor-drawing "yellow" size3)
                                              (recolor-drawing "orange" size4)
                                              (recolor-drawing "blue" size5)
                                              (recolor-drawing "indigo" size6)
                                              (recolor-drawing "red" size7))))]
            [(= 2 z)
             (vshift-drawing y-shift 
                             (hshift-drawing x-shift  
                                             (drawing-group
                                              (recolor-drawing "blue" size1)
                                              (recolor-drawing "orange" size2)
                                              (recolor-drawing "yellow" size3)
                                              (recolor-drawing "green" size4)
                                              (recolor-drawing "red" size5)
                                              (recolor-drawing "indigo" size6)
                                              (recolor-drawing "violet" size7))))]
            [(= 3 z)
             (vshift-drawing y-shift 
                             (hshift-drawing x-shift  
                                             (drawing-group
                                              (recolor-drawing "red" size1)
                                              (recolor-drawing "orange" size2)
                                              (recolor-drawing "yellow" size3)
                                              (recolor-drawing "violet" size4)
                                              (recolor-drawing "green" size5)
                                              (recolor-drawing "indigo" size6)
                                              (recolor-drawing "blue" size7))))]
            [(= 4 z)
             (vshift-drawing y-shift 
                             (hshift-drawing x-shift  
                                             (drawing-group
                                              (recolor-drawing "red" size1)
                                              (recolor-drawing "orange" size2)
                                              (recolor-drawing "blue" size3)
                                              (recolor-drawing "green" size4)
                                              (recolor-drawing "yellow" size5)
                                              (recolor-drawing "indigo" size6)
                                              (recolor-drawing "violet" size7))))]
            [(= 5 z)
             (vshift-drawing y-shift 
                             (hshift-drawing x-shift  
                                             (drawing-group
                                              (recolor-drawing "indigo" size1)
                                              (recolor-drawing "orange" size2)
                                              (recolor-drawing "yellow" size3)
                                              (recolor-drawing "green" size4)
                                              (recolor-drawing "blue" size5)
                                              (recolor-drawing "red" size6)
                                              (recolor-drawing "violet" size7))))]
            [(= 6 z)
             (vshift-drawing y-shift 
                             (hshift-drawing x-shift  
                                             (drawing-group
                                              (recolor-drawing "violet" size1)
                                              (recolor-drawing "orange" size2)
                                              (recolor-drawing "yellow" size3)
                                              (recolor-drawing "green" size4)
                                              (recolor-drawing "blue" size5)
                                              (recolor-drawing "indigo" size6)
                                              (recolor-drawing "red" size7))))]))))

;;; Procedure:
;;;   abstract-object
;;; Parameters:
;;;   width, a positive integer
;;;   height, a positive integer
;;;   x-shift, a positive integer
;;;   y-shift, a positive integer
;;;   target, an image
;;;   z, a positive integer
;;; Purpose:
;;;   To create a drawing on a defined canvas
;;; Produces:
;;;   drawing, a drawing
;;; Preconditions:
;;;   z is between 0 and 6 inclusive
;;; Postconditions:
;;;   It should render an abstract drawing on an target image of a size 
;;;   as determined by the height and width. 

(define abstract-object
  (lambda (width height x-shift y-shift target z)
    (drawing-render! (abstract-object-helper width height x-shift y-shift z) target)))

;;; Procedure:
;;;   turtle-procedure
;;; Parameters:
;;;   image, an image
;;;   width, a positive integer
;;;   height, a positive integer
;;;   m, a positive integer
;;;   size, 
;;;   p, a positive integer
;;;   k, a positive integer
;;; Purpose:
;;;   To create a turtle on the given image and make polygons on the 
;;;   desired places which in shape and color as as m p and k vary
;;; Produces:
;;;  Nothing
;;; Preconditions:
;;;   p is between 3 and 6 inclusive
;;;   k is between 0 and 249 inclusive.
;;; Postconditions:
;;;   If p=5 this procedure should create the shape as determined by 
;;;   turtle-snowflake1! else it should create turtle-snowflake! if p=3, p=4 or
;;;   p=6.
;;;   The turtle should start at the point as determined by turtle-teleport!
;;;   Turtle-set-color should get 250 color variations. The number was determined
;;;   by dividing the the value of irgb 255/255/255 by 250 as we want 250 variations 
;;;   by the orientation of our final image and also by 7 as we have 7 possible trails.
(define turtle-procedure
  (lambda (image width height m size p k)
    (let ([t (turtle-new image)])
      (turtle-set-brush! t "2. Hardness 025")
      (turtle-teleport! t 
                        (* (/ width 14) (* 2 m)) 
                        (round (* height (cos (/ (+ m 1) 5)))))
      (turtle-set-color! t (* k (* 9587 (+ m 1))))
      (cond 
        [(= p 5) (turtle-snowflake1! t size p 2)]
        [else (turtle-snowflake! t size p 2)]))))

;;; Procedure:
;;;   turtle-snowflake1!
;;; Parameters:
;;;   turtle, a turtle
;;;   side-length, any real number
;;;   sides, a positive integer
;;;   depth, a positive integer
;;; Purpose:
;;;  To create a star-shaped polygon of varying depths 
;;; Produces:
;;;  Nothing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;  The turtle will have created a star-shaped polygon of side-length side-length
;;; of sides sides. After the basic star shape has been created, the turtle will 
;;; make another copy of itself if depth > 1. 
(define turtle-snowflake1!
  (lambda (turtle side-length sides depth)
    (cond [(= 0 depth)]
          [else (repeat sides
                        (lambda () 
                          (turtle-forward! turtle side-length)
                          (turtle-snowflake1! turtle 
                                              (* .4 side-length) 
                                              sides 
                                              (- depth 1))
                          (turtle-turn! turtle 
                                        (+ (* (- depth 1) 72) 
                                           (/ 360 sides)))))])))

;;; Procedure:
;;;   turtle-snowflake!
;;; Parameters:
;;;   turtle, a turtle
;;;   side-length, any real number
;;;   sides, a positive integer
;;;   depth, a positive integer
;;; Purpose:
;;;   To create a polygon of varying depths
;;; Produces:
;;;  Nothing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;  The turtle will have moved so that it draws a polygon with side 
;;;  sides of side-length side-length.For each value of depth above one the 
;;;  central polygon will have additional copies of its self at 40% scale  to 
;;;  attached each of its corners. The copies will have another copy if depth  
;;;  is at least 3 and so forth.
;;; Citation
;;;  This code was taken from assignment 6 written by Mujtaba and Clara
(define turtle-snowflake!
  (lambda (turtle side-length sides depth)
    (cond [(= 0 depth)]
          [else (repeat sides
                        (lambda () 
                          (turtle-forward! turtle side-length)
                          (turtle-snowflake! turtle 
                                             (* .4 side-length) 
                                             sides 
                                             (- depth 1))
                          (cond 
                            [(even? depth) (turtle-turn! turtle (/ 360 sides))]
                            [else (turtle-turn! turtle (- 360 (/ 360 sides)))])))])))

