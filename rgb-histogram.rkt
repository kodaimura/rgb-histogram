#!/usr/local/bin/racket
#lang racket/gui

(require racket/draw)
(require plot)

(define img false)


(let ((argv (current-command-line-arguments)))
  (if (zero? (vector-length argv))
      (error "usage: get-rgb image-file")
      (set! img (vector-ref argv 0))))


(define *bm* (make-object bitmap% img))

(define distance
  (lambda (ox oy x y)
    (sqrt (+ (* (- ox x) (- ox x))
             (* (- oy y) (- oy y))))))


;; (A R G B A R G B A R G B A .... )
;; -> (R R R R ... )
(define take-R
  (lambda (ls)
    (map sub1 (filter-not zero?
                          (map * (flatten (make-list (/ (length ls) 4) '(0 1 0 0)))
                               (map add1 ls))))))

(define take-G
  (lambda (ls)
    (map sub1 (filter-not zero?
                          (map * (flatten (make-list (/ (length ls) 4) '(0 0 1 0)))
                               (map add1 ls))))))

(define take-B
  (lambda (ls)
    (map sub1 (filter-not zero?
                          (map * (flatten (make-list (/ (length ls) 4) '(0 0 0 1)))
                               (map add1 ls))))))


; '(35 255 32 56 120 92 255 ...) 
; -> (0の個数 1の個数 .... 255の個数)
(define mltp-aux
  (lambda (ls)
    (define v (make-vector 256))
    (define (aux ls)
      (if (null? ls)
          (vector->list v)
          (begin (vector-set! v (car ls) (+ 1 (vector-ref v (car ls))))
                 (aux (cdr ls)))))
    (aux ls)))

;(A R G B A R G B A R G B ..... )
;(255 130 32 40 255 130 32 40 255 57 120 14 .... )
; -> ('((0 13) (1 20) ... (255 72)) '((0 43) ... (255 19)) '((0 55) ... ))
(define make-list-to-plot-RGB
  (lambda (ls)
    (let ((rs (take-R ls))
          (gs (take-G ls))
          (bs (take-B ls)))
      (let ((r (map cons (range 256) (map list (mltp-aux rs))))
            (g (map cons (range 256) (map list (mltp-aux gs))))
            (b (map cons (range 256) (map list (mltp-aux bs)))))
         (list r g b)))))

; 画像から指定した範囲のピクセルデータを取得
(define get-pixels
  (lambda (ox oy x y)
    (let ((p (make-bytes (* 4 (- x ox) (- y oy)))))
      (send *bm* get-argb-pixels ox oy (- x ox) (- y oy) p)
      p)))




(define blue (bytes 0 0 0 255))
  
(define show-range
  (lambda (ox oy x y)
    (let ((*img* (make-object bitmap% img)))
      (for ((i (range ox x)))
        (send *img* set-argb-pixels i oy 1 1 blue))
      (for ((i (range oy y)))
        (send *img* set-argb-pixels ox i 1 1 blue))
      (for ((i (range ox x)))
        (send *img* set-argb-pixels i y 1 1 blue))
      (for ((i (range oy y)))
        (send *img* set-argb-pixels x i 1 1 blue))
      (let* ((frame2 (new frame% [label img]))
             (canvas2 (new my-canvas% [parent frame2]
                    [min-width (send *img* get-width)]
                    [min-height (send *img* get-height)]
                    [paint-callback
                     (lambda (canvas2 dc)
                       (send dc draw-bitmap *img* 0 0))])))
        (send frame2 show #t)))))

;(plot グラフなど)
;(discrete-histogram プロットデータ #:オプション #:オプション)
;データ: '((x0,y0) (x1,y1) (x2,y2) (x4,y4)...)

;ここでは x-> 0 ~ 255  y-> ピクセルの数
;((1 38) (2 40) (3 0) (4 0) ... (255 73))
  
(define plot-histogram
  (lambda (ox oy x y)
    (plot-new-window? #t)
    (let ((ls (make-list-to-plot-RGB
               (bytes->list (get-pixels ox oy x y)))))
      (plot (list
             (discrete-histogram
               (list-ref ls 0)
               #:skip 1 #:x-min 0
               #:label "R" #:color 1 #:line-color 1)
             (discrete-histogram
               (list-ref ls 1)
               #:skip 1 #:x-min 0.75
               #:label "G" #:color 2 #:line-color 2)
             (discrete-histogram
               (list-ref ls 2)
               #:skip 1 #:x-min 1.5
               #:label "B" #:color 3 #:line-color 3))))))

(define my-canvas%
  (class canvas%
    (inherit refresh)

    (define start_x 0)     ;クリックした時のx座標
    (define start_y 0)     ;クリックした時のy座標
    (define mouse_down #f)  ;マウスが押された状態かどうか

    ;on-eventメソッドを override  (begin (override on-event) (define (on-event) body))の別の記述法
    ;(define (関数名 引数))は (define 関数 (lambda (引数)) body) の別の記述法


    ;; on-eventメソッドをオーバーライド
    ;; 元からあるon-eventメソッドを再定義
    ;; on-eventは自身に対してマウスイベントが生じた時に実行されるメソッド
    
    (define/override (on-event me)
      (cond
        ((eq? 'left-down (send me get-event-type))  
         (begin (set! mouse_down #t)
                (set! start_x (send me get-x))
                (set! start_y (send me get-y))))
        ((eq? 'left-up (send me get-event-type))     
         (begin (plot-histogram start_x
                                start_y
                                (send me get-x)
                                (send me get-y))
                (show-range  start_x
                             start_y
                             (send me get-x)
                             (send me get-y))
                (set! mouse_down #f)
                (set! start_x 0))
                (set! start_y 0))
        (else                     
         (let ((mx (send me get-x))
               (my (send me get-y))
               (pixel (bytes 0 0 0 0)))
           (when (and (positive? mx) (positive? my))
             (send *bm* get-argb-pixels mx my 1 1 pixel)
             (send xy set-value (format "~a, ~a" mx my))
             (send rgb set-value (format "~a, ~a, ~a, ~a"
                                      (bytes-ref pixel 0)
                                      (bytes-ref pixel 1)
                                      (bytes-ref pixel 2)
                                      (bytes-ref pixel 3)))
             (when (eq? mouse_down #t)
               (send dist set-value
                     (number->string
                      (distance start_x start_y mx my)))))))))
    (super-new)))

(define frame (new frame% [label img]))

(define canvas (new my-canvas% [parent frame]
                    [min-width (send *bm* get-width)]
                    [min-height (send *bm* get-height)]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc draw-bitmap *bm* 0 0))]))

(define hp (new horizontal-pane%  [parent frame]))
(define xy (new text-field% [parent hp][label "xy"] ))
(define rgb (new text-field% [parent hp] [label "argb"]))
(define dist (new text-field% [parent hp] [label "distance"]))


(send frame show #t)
