(ns benjamin-schwerdtner.clj-push3-play.arc.grid-impl
  (:require
   [benjamin-schwerdtner.clj-push3-play.prot :refer
    [with-push-type-meta]]
   [libpython-clj2.require :refer [require-python]]
   [libpython-clj2.python :refer [py. py..] :as py]))

(require-python '[torch :as torch]
                '[torch.nn.functional :as F]
                '[builtins :refer [slice]])

(defn get-subgrid
  "Extract a subgrid from the given grid starting at (x, y) with given width and height"
  [grid x y width height]
  (py/get-item grid [(slice y (+ y height)) (slice x (+ x width))]))

(defn count-non-zero
  "Count the number of non-zero cells in a grid"
  [grid]
  (py.. (torch/ne grid 0) (sum) (item)))

(defn set-subgrid!
  "Set a subgrid in the target grid at position (x, y)"
  [target-grid subgrid x y]
  (let [sub-h (py.. subgrid (size 0))
        sub-w (py.. subgrid (size 1))]
    (py/set-item! target-grid
                  [(slice y (+ y sub-h)) (slice x (+ x sub-w))]
                  subgrid)
    target-grid))

(defn grid-count-non-zero
  "Count non-zero cells in the grid"
  [grid]
  (count-non-zero grid))

(def colors (into [] (range 10)))

(defn make-color-gensym
  []
  (let [cnt (atom (count colors))]
    (fn []
      (let [i @cnt]
        (swap! cnt inc)
        i))))

(defonce color-gensym (make-color-gensym))

(defn zeroes [dims]
  (torch/zeros dims :dtype torch/float))

(defn grid [values
            ;; {:keys [device]}
            ]
  (torch/tensor values :dtype torch/float))

(defn fill [grid color]
  (torch/fill grid color))

(defn color-swap
  "Returns a grid where the color `from` is swaped to `to`."
  [grid from to]
  #_(ca-apply-rule
     grid
     (write-rule
      {:kernel [[0 0 0]
                [0 1 0]
                [0 0 0]]
       :mask-color from
       :new-color to
       :old-color from
       :threshold 1}))
  (torch/where (torch/eq grid from) to grid))

(defn get-cell
  "Get the value at position [x y] in the grid"
  [grid x y]
  (py.. (py/get-item grid [y x]) item))

(defn set-cell
  "Set the value at position [x y] in the grid to color"
  [grid x y color]
  (let [new-grid (py.. grid clone)]
    (py.. new-grid (setitem [x y] color))
    new-grid))

(defn count-color
  "Count occurrences of a specific color in the grid"
  [grid color]
  (-> (torch/eq grid color)
      (torch/sum)
      (py.. item)))

(defn color-frequencies
  "Return a map of color -> count for all colors in the grid"
  [grid]
  (let [flat-grid (into [] (py.. grid flatten tolist))]
    (vec (sort-by second (fn [a b] (compare b a)) (map vec (frequencies flat-grid))))))

(defn distinct-colors [grid]
  (into
   []
   (distinct)
   (py.. grid flatten tolist)))

(defn max-color
  "Return the color that appears most frequently in the grid"
  [grid]
  (ffirst (color-frequencies grid)))

(defn max-color-non-zero [grid]
  (ffirst
   (remove
    (comp zero? first)
    (color-frequencies grid))))

(defn positions-of-color
  "Return all [x y] positions where the specified color appears"
  [grid color]
  (let [mask (torch/eq grid color)
        indices (py.. (torch/nonzero mask) tolist)]
    (vec (for [[y x] indices] [x y]))))

(defn positions-of-max-color
  "Return all [x y] positions where the most frequent color appears"
  [grid]
  (positions-of-color grid (max-color grid)))

(defn filter-color [grid color]
  (torch/where
   (torch/eq grid color)
   grid
   0))

(defn rows [grid]
  (vec grid))

(defn columns [grid]
  (vec (torch/t grid)))

(defn window
  ([grid [x y] size] (window grid [x y] size size))
  ([grid [x y] width height]
   (if (or (>= x (py.. grid (size 0)))
           (>= y (py.. grid (size 1)))
           (< x 0)
           (< y 0)
           (<= width 0)
           (<= height 0)
           (< (py.. grid (size 1)) (+ x width))
           (< (py.. grid (size 0)) (+ y height)))
     grid
     (-> grid
         (torch/narrow 1 x width)
         (torch/narrow 0 y height)
         (py.. clone)))))

(defn shape [grid]
  (vec (py.. grid (size))))

(comment
  (shape (grid [0])))

;; ====================================
;;
;; https://www.kaggle.com/code/daviddamario/evolving-cellular-automata-to-solve-arc-tasks
;;

;; 1. A rule gene encoding:

;;
;; Version 1: Encode colors in 5 bits, encode kernel binary
;;
;;
;;
;; +--------+--------+--------------+-------------+-------------+-----------+
;; |        |        |              |             |             |           |
;; |        | 0-8    | 9-13         | 13-17       | 17-21       | 21-25     |
;; +--------+--------+--------------+-------------+-------------+-----------+
;; |        | kernel | mask color   | old color   | new color   | threshold |
;; +--------+--------+--------------+-------------+-------------+-----------+
;; |        | 9 bit  | 5 bit        | 5 bit       | 5 bit       | 5 bit     |
;; +--------+--------+--------------+-------------+-------------+-----------+
;;
;; meta,idx
;;
;;
;; Version 2:
;;
;; - use float32 (torch default)
;;
;;
;; +--------+--------+--------------+-------------+-------------+-----------+
;; |        |        |              |             |             |           |
;; |        | 0-8    | 9            | 10          | 11          | 12        |
;; +--------+--------+--------------+-------------+-------------+-----------+
;; |        | kernel | mask color   | old color   | new color   | threshold |
;; +--------+--------+--------------+-------------+-------------+-----------+
;;
;; meta,idx
;;
;;
;; kernel: 3x3 kernel for convolution, a neighbour activation encoding.
;; mask color: Color the rule applies to, cell activations only come from this color.
;; old color: Color at target cell that rule applies to.
;; new color: Transformed color at target cell.
;; threshold: Threshold for rule to apply, i.e. neighbour count.
;;

;;
;; In version 2: kernel is weighted
;; In version 1: kernel is binary
;;

(defn with-gene-meta [o]
  (with-push-type-meta o :push/ca-rule))

(defn rand-gene
  "
  Returns a random CA rule gene tensor of the following form:


  ;;
  ;; +--------+--------+--------------+-------------+-------------+-----------+
  ;; |        |        |              |             |             |           |
  ;; |        | 0-8    | 9            | 10          | 11          | 12        |
  ;; +--------+--------+--------------+-------------+-------------+-----------+
  ;; |        | kernel | mask color   | old color   | new color   | threshold |
  ;; +--------+--------+--------------+-------------+-------------+-----------+
  ;;

  "
  ([]
   (with-gene-meta
     (torch/cat
      [;; 0-8
       (torch/rand :size [(* 3 3)])
       ;; 9,10,11
       (torch/randint :size [(count [9 10 11])] :low 0 :high 8 :dtype torch/float)
       ;; 12
       ;; --
       ;; 3x3 = 9 is max input
       ;; if kernel weights are within range [0..1].
       ;; --
       ;; Allow 10 for disabling the rule.
       (torch/randint :size [1] :low 0 :high 10 :dtype torch/float)])))
  ([num-genes]
   (with-gene-meta (torch/stack (vec (repeatedly num-genes rand-gene))))))

(comment
  (meta (torch/Tensor [1])))

(defn kernel [rule]
  (->
   (torch/narrow rule -1 0 9)
   (torch/reshape [3 3])))

(defn read-rule
  [rule]
  {:kernel (kernel rule)
   :mask-color (torch/narrow rule -1 9 1)
   :new-color (torch/narrow rule -1 11 1)
   :old-color (torch/narrow rule -1 10 1)
   :threshold (torch/narrow rule -1 12 1)})

(defn write-rule [{:keys [kernel mask-color new-color old-color threshold]}]
  (->
   (torch/cat
    [(py.. (torch/tensor kernel :dtype torch/float) (view 9))
     (torch/tensor [mask-color old-color new-color threshold] :dtype torch/float)])
   (with-gene-meta)))

(comment
  (torch/tensor (torch/tensor 0))

  (write-rule
   {:kernel [[1 0 0]
             [0 0 0]
             [0 0 0]]
    :mask-color 0
    :new-color 0
    :old-color (torch/tensor 0)
    :threshold 2}))

(defn conv
  ([grid kernel] (conv grid kernel 1))
  ([grid kernel padding]
   (py..
    (F/conv2d (py.. grid (unsqueeze 0) (unsqueeze 0))
              (py.. kernel (unsqueeze 0) (unsqueeze 0))
              :padding
              padding)
    (squeeze 0)
    (squeeze 0))))

(defn ca-apply-rule
  "Returns a grid with CA rule (gene) applied.

  This is one time step of the CA.

 (ca-apply-rule
   (grid
    [[5 0 3]
     [5 0 3]
     [0 0 0]])
   (torch/tensor
    [0 0 0
     1 0 0
     0 0 0

     5 0 2

     1.0]))

  ;; tensor([[5., 2., 3.],
  ;;         [5., 2., 3.],
  ;;         [0., 0., 0.]])

  see [[rand-gene]]
  "
  [grid rule-gene]
  (let [{:keys [kernel mask-color new-color old-color threshold]}
        (read-rule rule-gene)
        grid-masked (py.. (torch/eq grid mask-color)
                          (type torch/float32))
        activations (py.. (F/conv2d
                           (py.. grid-masked (unsqueeze 0) (unsqueeze 0))
                           (py.. kernel (unsqueeze 0) (unsqueeze 0))
                           :padding
                           1)
                          (squeeze 0)
                          (squeeze 0))]
    [grid
     grid-masked
     activations
     threshold

     (torch/ge
      activations threshold)

     old-color
     (torch/eq grid old-color)]

    (torch/where (torch/mul (torch/ge activations threshold)
                            (torch/eq grid old-color))
                 new-color
                 grid)))

(defn equal? [g1 g2]
  (torch/equal g1 g2))

(defn reduce-ca
  "Returns a grid with `rule-gene` CA applied multiple times.

  If the CA leads to a stable pattern, terminates and returns that pattern.
  (Class 1 CA).

  In all other cases, including oscillating patterns, this subroutine terminates after `n` steps.
  Default 1000.

  https://en.wikipedia.org/wiki/Cellular_automaton

  "
  ([grid rule-gene] (reduce-ca grid rule-gene (long 1e3)))
  ([grid rule-gene n]
   (reduce
    (fn [grid _]
      (let [out (ca-apply-rule grid rule-gene)]
        (if (torch/equal out grid)
          (ensure-reduced out)
          out)))
    grid
    (range n))))

(defn flood-fill
  "

  Returns a grid where the cell at position plus its neighbours of the same color,
  recursively, are replaced with `color`.

  Imagine that position is a valey in a flood fill algorithm that stops at
  'other color' cells.


 (flood-fill
   (grid
    [[0 0 0 0]
     [1 1 1 1]
     [0 0 0 0]
     [1 1 1 0]])
   [2 2]
   5)

  tensor([[0., 0., 0., 0.],
          [1., 1., 1., 1.],
          [5., 5., 5., 5.],
          [1., 1., 1., 5.]])


  "
  [grid [x y] color]
  (if-not (and (>= x 0)
               (< x (py.. grid (size 0)))
               (>= y 0)
               (< y (py.. grid (size 1))))
    grid
    (let [current-color (py.. (py/get-item grid [x y]) item)]
      (if (= current-color color)
        grid
        (let [marker-color (color-gensym)
              ;; put a marker 'seed' at the position
              grid (torch/index_put
                    grid
                    [(torch/tensor [x]) (torch/tensor [y])]
                    (torch/tensor marker-color :dtype torch/float))]
          (-> grid
              ;; fill marker color growing from seed
              (reduce-ca (write-rule {:kernel [[0 1 0] [1 0 1]
                                               [0 1 0]]
                                      :mask-color marker-color
                                      :new-color marker-color
                                      :old-color current-color
                                      :threshold 1}))
              ;; subst marker to target color
              (color-swap marker-color color)))))))

(comment

  (ca-apply-rule
   (grid
    [[1 1 1]
     [0 0 0]
     [1 1 1]])
   (write-rule
    {:kernel
     [[0 1 0]
      [1 0 1]
      [0 1 0]]
     :mask-color 0
     :new-color 2
     :old-color 0
     :threshold 1}))

  (reduce-ca
   (grid
    [[0 0 0 0]
     [1 1 1 0]
     [0 10 0 0]
     [1 1 1 0]])
   (write-rule
    {:kernel
     [[0 10 0]
      [10 0 10]
      [0 10 0]]
     :mask-color 10
     :new-color 10
     :old-color 0
     :threshold 10}))

  (reduce-ca
   (grid
    [[0 0 0 0]
     [1 1 1 1]
     [0 10 0 0]
     [1 1 1 0]])
   (write-rule
    {:kernel
     [[0 10 0]
      [10 0 10]
      [0 10 0]]
     :mask-color 10
     :new-color 10
     :old-color 0
     :threshold 10}))

  (flood-fill
   (grid
    [[0 0 0 0]
     [1 1 1 1]
     [0 0 0 0]
     [1 1 1 0]])
   [2 2]
   5)

;; tensor([[0., 0., 0., 0.],
  ;;         [1., 1., 1., 1.],
  ;;         [5., 5., 5., 5.],
  ;;         [1., 1., 1., 5.]])

  ((color-gensym))

;; -----------------------
  ;; hm, this is how we might want to say it:
  (def conways-gol
    [;; reproduction rule
     (write-rule {:kernel [[0 1 0]
                           [1 0 1]
                           [0 1 0]]
                  :mask-color 1
                  :new-color 1
                  :old-color 0
                  :threshold 3})
     ;; overpopulation rule
     (write-rule {:kernel [[0 1 0]
                           [1 0 1]
                           [0 1 0]]
                  :mask-color 1
                  :new-color 0
                  :old-color 1
                  :threshold 3})
     ;; underpopulation rule
     (write-rule {:kernel [[0 1 0]
                           [1 0 1]
                           [0 1 0]]
                  :mask-color 0
                  :new-color 0
                  :old-color 1
                  ;; max neighbours 4, if I have only a single or less
                  ;; neighbours, I die.
                  :threshold (- 4 1)})]))

(comment

  (window
   (grid
    [[1 0 1 0]
     [0 1 2 0]
     [1 0 1 0]])
   [1 1]
   3
   2)

  (window
   (grid
    [[1 0 1 0]
     [0 1 2 0]
     [1 0 1 0]])
   [1 1]
   2)

  (window
   (grid
    [[1 0 1 0]
     [0 1 2 0]
     [1 0 1 0]])
   [0 0]
   3))

;; ==================== Rotation Primitives ====================

(defn rotate-90
  "Rotate grid 90 degrees clockwise"
  [grid]
  (-> grid
      (torch/transpose 0 1)
      (torch/flip [1])))

(defn rotate-180
  "Rotate grid 180 degrees"
  [grid]
  (-> grid
      (torch/flip [0])
      (torch/flip [1])))

(defn rotate-270
  "Rotate grid 270 degrees clockwise (90 degrees counter-clockwise)"
  [grid]
  (-> grid
      (torch/transpose 0 1)
      (torch/flip [0])))

(defn rotate
  "Rotate grid by n degrees (must be multiple of 90)"
  [grid degrees]
  (case (mod degrees 360)
    0 grid
    90 (rotate-90 grid)
    180 (rotate-180 grid)
    270 (rotate-270 grid)
    (throw (ex-info "Rotation must be multiple of 90 degrees" {:degrees degrees}))))

;; ==================== Symmetry Primitives ====================

(defn flip-horizontal
  "Flip grid horizontally (left-right)"
  [grid]
  (torch/flip grid [1]))

(defn flip-vertical
  "Flip grid vertically (up-down)"
  [grid]
  (torch/flip grid [0]))

(defn flip-diagonal
  "Flip grid along main diagonal (transpose)"
  [grid]
  (torch/transpose grid 0 1))

(defn flip-antidiagonal
  "Flip grid along anti-diagonal"
  [grid]
  (-> grid
      (rotate-90)
      (flip-horizontal)))

;; ==================== Symmetry Detection ====================

(defn symmetric-horizontal?
  "Check if grid has horizontal symmetry"
  [grid]
  (torch/equal grid (flip-horizontal grid)))

(defn symmetric-vertical?
  "Check if grid has vertical symmetry"
  [grid]
  (torch/equal grid (flip-vertical grid)))

(defn symmetric-diagonal?
  "Check if grid has diagonal symmetry (must be square)"
  [grid]
  (let [[h w] (py.. grid shape)]
    (and (= h w)
         (torch/equal grid (flip-diagonal grid)))))

(defn rotational-symmetry-order
  "Find the order of rotational symmetry (1, 2, or 4)"
  [grid]
  (cond
    (torch/equal grid (rotate-90 grid)) 4
    (torch/equal grid (rotate-180 grid)) 2
    :else 1))

;; ==================== Symmetry Generation ====================

(defn make-symmetric-horizontal
  "Create a horizontally symmetric grid from the left half"
  [half-grid]
  (let [flipped (flip-horizontal half-grid)]
    (torch/cat [half-grid flipped] 1)))

(defn make-symmetric-vertical
  "Create a vertically symmetric grid from the top half"
  [half-grid]
  (let [flipped (flip-vertical half-grid)]
    (torch/cat [half-grid flipped] 0)))

(defn make-symmetric-4fold
  "Create a 4-fold symmetric grid from a quarter"
  [quarter-grid]
  (let [top-right (flip-horizontal quarter-grid)
        top-half (torch/cat [quarter-grid top-right] 1)
        bottom-half (flip-vertical top-half)]
    (torch/cat [top-half bottom-half] 0)))

;; ======================

(defn all-rotations
  "Generate all 4 rotations of a grid"
  [grid]
  [(identity grid)
   (rotate-90 grid)
   (rotate-180 grid)
   (rotate-270 grid)])

(defn all-symmetries
  "Generate all 8 symmetries (D4 group) of a grid"
  [grid]
  (concat (all-rotations grid)
          (all-rotations (flip-horizontal grid))))

(defn resize-grid-center
  "Resize a grid to new dimensions, placing the original grid in the center.
   Fills with background-color (default 0) if new size is larger.
   Crops if new size is smaller."
  ([grid new-height new-width]
   (resize-grid-center grid new-height new-width 0))
  ([grid new-height new-width background-color]
   (let [[old-height old-width] (shape grid)
         ;; Calculate offsets to center the old grid in the new grid
         y-offset (quot (- new-height old-height) 2)
         x-offset (quot (- new-width old-width) 2)
         ;; Create new grid filled with background color
         new-grid (torch/full [new-height new-width] background-color :dtype torch/float)]
     (cond
       ;; If new size is smaller in both dimensions, crop from center
       (and (<= new-height old-height) (<= new-width old-width))
       (let [y-start (quot (- old-height new-height) 2)
             x-start (quot (- old-width new-width) 2)]
         (-> grid
             (torch/narrow 0 y-start new-height)
             (torch/narrow 1 x-start new-width)
             (py.. clone)))

       ;; If new size is larger in both dimensions, place in center
       (and (>= new-height old-height) (>= new-width old-width))
       (do
         (py.. new-grid
               (narrow 0 y-offset old-height)
               (narrow 1 x-offset old-width)
               (copy_ grid))
         new-grid)

       ;; Mixed case: one dimension larger, one smaller
       :else
       (let [;; Determine source region to copy
             src-y-start (if (< new-height old-height)
                           (quot (- old-height new-height) 2)
                           0)
             src-x-start (if (< new-width old-width)
                           (quot (- old-width new-width) 2)
                           0)
             src-height (min old-height new-height)
             src-width (min old-width new-width)
             ;; Determine destination region
             dst-y-start (max 0 y-offset)
             dst-x-start (max 0 x-offset)
             ;; Get source slice
             src-slice (-> grid
                           (torch/narrow 0 src-y-start src-height)
                           (torch/narrow 1 src-x-start src-width))]
         (py.. new-grid
               (narrow 0 dst-y-start src-height)
               (narrow 1 dst-x-start src-width)
               (copy_ src-slice))
         new-grid)))))

(defn resize-grid-top-left
  "Resize a grid to new dimensions, placing the original grid at the top-left.
   Fills with background-color (default 0) if new size is larger.
   Crops from bottom-right if new size is smaller."
  ([grid new-height new-width]
   (resize-grid-top-left grid new-height new-width 0))
  ([grid new-height new-width background-color]
   (let [[old-height old-width] (shape grid)]
     (cond
       ;; If new size is smaller in both dimensions, crop from top-left
       (and (<= new-height old-height) (<= new-width old-width))
       (-> grid
           (torch/narrow 0 0 new-height)
           (torch/narrow 1 0 new-width)
           (py.. clone))

       ;; If new size is larger in both dimensions, place at top-left
       (and (>= new-height old-height) (>= new-width old-width))
       (let [new-grid (torch/full [new-height new-width] background-color :dtype torch/float)]
         (py.. new-grid
               (narrow 0 0 old-height)
               (narrow 1 0 old-width)
               (copy_ grid))
         new-grid)

       ;; Mixed case: one dimension larger, one smaller
       :else
       (let [copy-height (min old-height new-height)
             copy-width (min old-width new-width)
             new-grid (torch/full [new-height new-width] background-color :dtype torch/float)]
         (let [src-slice (-> grid
                             (torch/narrow 0 0 copy-height)
                             (torch/narrow 1 0 copy-width))]
           (py.. new-grid
                 (narrow 0 0 copy-height)
                 (narrow 1 0 copy-width)
                 (copy_ src-slice)))
         new-grid)))))

;; -------------------------------------------------------

(defn max-pool
  ([grid] (max-pool grid [3 3] 3))
  ([grid kernel-size stride]
   (F/max_pool2d (py.. grid (unsqueeze 0))
                 :kernel_size kernel-size
                 :stride stride)))

(defn pack
  ;;
  ;; +-----------------+
  ;; | X  X  X ...     |
  ;; | X               |
  ;; |                 |
  ;; +-----------------+
  ;;
  "Pack pattern into grid from top left to bottom right
  `n` times."
  [grid pattern n]
  (let [patt-height (py.. pattern (size 0))
        patt-width (py.. pattern (size 1))
        grid-height (py.. grid (size 0))
        grid-width (py.. grid (size 1))
        ;; Calculate how many patterns can fit in each dimension
        patterns-per-row (quot grid-width patt-width)
        patterns-per-col (quot grid-height patt-height)
        total-positions (* patterns-per-row patterns-per-col)]
    (if (= n 0)
      grid
      (let [;; Clone grid to avoid mutation
            result-grid (py.. grid clone)
            ;; Pack the requested number of patterns, up to the available space
            patterns-to-pack (min n total-positions)]
        (loop [i 0]
          (if (< i patterns-to-pack)
            (let [;; Calculate position in the grid
                  row (quot i patterns-per-row)
                  col (rem i patterns-per-row)
                  y (* row patt-height)
                  x (* col patt-width)]
              (set-subgrid! result-grid pattern x y)
              (recur (inc i)))
            result-grid))))))

 ;; ==================== Pattern Fill Operations ====================
;;
;; These functions support ARC tasks that involve filling larger grids
;; with patterns according to meta-patterns.
;;
;; Core function:
;; - fill-with-pattern: Fill a grid with a pattern based on a meta-pattern
;;
;; Example usage:
;; (def pattern (grid [[1 2] [3 4]]))
;; (def meta (grid [[1 0] [0 1]]))  ; Place at top-left and bottom-right
;; (def result (fill-with-pattern (zeroes [4 4]) pattern meta))
;; => [[1 2 0 0] [3 4 0 0] [0 0 1 2] [0 0 3 4]]
;;

(defn fill-with-pattern
  "Fill a grid with a pattern according to a meta-pattern.

  Args:
    base-grid: The base grid to fill (determines output dimensions)
    pattern: The subgrid pattern to be inserted (e.g. 3x3)
    meta-pattern: A low-dimensional grid (e.g. 3x3) where non-zero values
                  indicate where to place the pattern in the output grid

  Returns:
    The filled grid (cloned from base-grid)
    Returns base-grid unchanged if:
    - Output dimensions are not divisible by meta-pattern dimensions
    - Pattern is too large to fit in the subgrid regions
    - Any dimension is zero

  Example:
    (fill-with-pattern
      (zeroes [4 4])       ; 4x4 base grid of zeros
      (grid [[1 2] [3 4]]) ; 2x2 pattern
      (grid [[1 0] [0 1]]) ; 2x2 meta-pattern)
    ; Places the pattern at top-left and bottom-right 2x2 regions"
  [base-grid pattern meta-pattern]
  (let [output-grid (py.. base-grid clone)
        [out-height out-width] (shape base-grid)
        [pattern-height pattern-width] (shape pattern)
        [meta-height meta-width] (shape meta-pattern)]
    ;; Validate dimensions
    (cond
      (or
       (not meta-height)
       (not meta-width)
       (not pattern-height)
       (not pattern-width))
      output-grid
      ;; Check for zero dimensions first to avoid division by zero
      (or (= 0 meta-height) (= 0 meta-width)
          (= 0 pattern-height) (= 0 pattern-width)
          (= 0 out-height) (= 0 out-width))
      output-grid

      ;; Check if meta-pattern dimensions divide evenly into output dimensions
      (or (not= 0 (rem out-height meta-height))
          (not= 0 (rem out-width meta-width)))
      output-grid

      ;; Check if pattern fits in the subgrid regions
      :else
      (let [ ;; Calculate the size of each subgrid region
            subgrid-height (quot out-height meta-height)
            subgrid-width (quot out-width meta-width)]
        (cond
          ;; Check if pattern fits in the subgrid regions
          (or (> pattern-height subgrid-height)
              (> pattern-width subgrid-width))
          output-grid

          ;; Valid dimensions - proceed with filling
          :else
          (do
            ;; Iterate through the meta-pattern
            (doseq [meta-y (range meta-height)
                    meta-x (range meta-width)]
              (let [meta-value (py.. (py/get-item meta-pattern [meta-y meta-x]) item)]
                ;; If meta-pattern has non-zero value at this position, place the pattern
                (when (not (zero? meta-value))
                  (let [ ;; Calculate where to place the pattern in the output grid
                        target-y (* meta-y subgrid-height)
                        target-x (* meta-x subgrid-width)]
                    (set-subgrid! output-grid pattern target-x target-y)))))
            output-grid))))))

(comment
  (def pattern (grid [[1 2] [3 4]]))
  (fill-with-pattern
   (zeroes [4 4])
   pattern
   ;; (grid [0])
   (grid [[1 0] [0 0]])))

;; ------------------------------------------------------

(defn clj
  [g]
  (mapv
   (comp vec #(py.. % tolist))
   (py.. g (to :dtype torch/int))))

;; ==================== Tiling Operations ====================

(defn tile
  "Tile a pattern to fill a grid of specified dimensions.
   Pattern is repeated horizontally and vertically."
  [pattern target-height target-width]
  (let [[p-height p-width] (shape pattern)
        ;; Calculate how many times to repeat
        repeat-y (int (Math/ceil (/ target-height p-height)))
        repeat-x (int (Math/ceil (/ target-width p-width)))
        ;; Repeat the pattern
        tiled (torch/tile pattern [repeat-y repeat-x])]
    ;; Crop to exact target dimensions
    (-> tiled
        (torch/narrow 0 0 target-height)
        (torch/narrow 1 0 target-width)
        (py.. clone))))

(defn tile-n
  "Tile a pattern n times in each direction.
   Returns a grid n*height by n*width."
  [pattern n]
  (let [[h w] (shape pattern)]
    (tile pattern (* n h) (* n w))))

;; ==================== Bounding Box / Crop Operations ====================

(defn bounding-box
  "Find the bounding box of non-zero cells in the grid.
   Returns [min-y min-x max-y max-x] or nil if grid is all zeros."
  [grid]
  (let [nonzero-indices (torch/nonzero (torch/ne grid 0))]
    (when (> (py.. nonzero-indices (size 0)) 0)
      (let [ys (py/get-item nonzero-indices [(slice nil nil) 0])
            xs (py/get-item nonzero-indices [(slice nil nil) 1])
            min-y (py.. (torch/min ys) item)
            max-y (py.. (torch/max ys) item)
            min-x (py.. (torch/min xs) item)
            max-x (py.. (torch/max xs) item)]
        [(int min-y) (int min-x) (int max-y) (int max-x)]))))

(defn crop-to-content
  "Crop grid to the minimal bounding box containing non-zero cells.
   Returns the original grid if all zeros."
  [grid]
  (if-let [[min-y min-x max-y max-x] (bounding-box grid)]
    (let [height (inc (- max-y min-y))
          width (inc (- max-x min-x))]
      (-> grid
          (torch/narrow 0 min-y height)
          (torch/narrow 1 min-x width)
          (py.. clone)))
    grid))

;; ==================== Connected Components ====================

(defn connected-components
  "Find connected components of non-zero cells using flood fill.
   Returns a grid where each component has a unique integer label (1, 2, 3...)
   and background remains 0."
  [grid]
  (let [[height width] (shape grid)
        labels (torch/zeros [height width] :dtype torch/float)
        visited (atom #{})
        label-counter (atom 0)
        directions [[0 1] [0 -1] [1 0] [-1 0]]]
    ;; BFS flood fill for each unvisited non-zero cell
    (doseq [y (range height)
            x (range width)]
      (let [val (py.. (py/get-item grid [y x]) item)]
        (when (and (not (zero? val))
                   (not (contains? @visited [y x])))
          ;; Start new component
          (swap! label-counter inc)
          (let [current-label @label-counter
                queue (atom [[y x]])]
            ;; BFS
            (while (seq @queue)
              (let [[cy cx] (first @queue)]
                (swap! queue rest)
                (when (and (>= cy 0) (< cy height)
                           (>= cx 0) (< cx width)
                           (not (contains? @visited [cy cx]))
                           (not (zero? (py.. (py/get-item grid [cy cx]) item))))
                  (swap! visited conj [cy cx])
                  (py/set-item! labels [cy cx] (float current-label))
                  ;; Add neighbors to queue
                  (doseq [[dy dx] directions]
                    (swap! queue conj [(+ cy dy) (+ cx dx)])))))))))
    labels))

(defn extract-objects
  "Extract all connected objects from a grid.
   Returns a vector of {:grid cropped-grid :bbox [min-y min-x max-y max-x] :color dominant-color}."
  [grid]
  (let [labels (connected-components grid)
        num-components (int (py.. (torch/max labels) item))]
    (vec
     (for [label (range 1 (inc num-components))]
       (let [mask (torch/eq labels (float label))
             ;; Get bounding box for this component
             indices (torch/nonzero mask)
             ys (py/get-item indices [(slice nil nil) 0])
             xs (py/get-item indices [(slice nil nil) 1])
             min-y (int (py.. (torch/min ys) item))
             max-y (int (py.. (torch/max ys) item))
             min-x (int (py.. (torch/min xs) item))
             max-x (int (py.. (torch/max xs) item))
             height (inc (- max-y min-y))
             width (inc (- max-x min-x))
             ;; Extract the object (with original colors)
             obj-grid (torch/where mask grid (torch/tensor 0.0 :dtype torch/float))
             cropped (-> obj-grid
                         (torch/narrow 0 min-y height)
                         (torch/narrow 1 min-x width)
                         (py.. clone))]
         {:grid cropped
          :bbox [min-y min-x max-y max-x]
          :color (max-color-non-zero cropped)})))))

;; ==================== Centroid / Spatial Ordering ====================

(defn color-centroid
  "Compute the centroid (average x,y position) for a given color in the grid.
   Returns [x-centroid y-centroid] or nil if color not found."
  [grid color]
  (let [positions (positions-of-color grid color)]
    (when (seq positions)
      (let [n (count positions)
            sum-x (reduce + (map first positions))
            sum-y (reduce + (map second positions))]
        [(/ sum-x n) (/ sum-y n)]))))

(defn colors-by-centroid-x
  "Get distinct colors sorted by their x-centroid (left to right).
   Optionally exclude zero (background)."
  ([grid] (colors-by-centroid-x grid false))
  ([grid exclude-zero?]
   (let [colors (distinct-colors grid)
         colors (if exclude-zero?
                  (remove zero? colors)
                  colors)
         color-centroids (for [c colors
                               :let [centroid (color-centroid grid c)]
                               :when centroid]
                           {:color c :x (first centroid)})]
     (mapv :color (sort-by :x color-centroids)))))

(defn colors-by-centroid-y
  "Get distinct colors sorted by their y-centroid (top to bottom).
   Optionally exclude zero (background)."
  ([grid] (colors-by-centroid-y grid false))
  ([grid exclude-zero?]
   (let [colors (distinct-colors grid)
         colors (if exclude-zero?
                  (remove zero? colors)
                  colors)
         color-centroids (for [c colors
                               :let [centroid (color-centroid grid c)]
                               :when centroid]
                           {:color c :y (second centroid)})]
     (mapv :color (sort-by :y color-centroids)))))

(defn layer-orientation
  "Detect whether colors form horizontal or vertical layers.
   Returns :horizontal if layers span left-right (use y-centroid),
   :vertical if layers span top-bottom (use x-centroid).
   Based on variance of centroids along each axis."
  ([grid] (layer-orientation grid true))
  ([grid exclude-zero?]
   (let [colors (distinct-colors grid)
         colors (if exclude-zero?
                  (remove zero? colors)
                  colors)
         centroids (for [c colors
                         :let [centroid (color-centroid grid c)]
                         :when centroid]
                     centroid)
         xs (map (comp double first) centroids)
         ys (map (comp double second) centroids)
         variance (fn [vals]
                    (when (seq vals)
                      (let [mean (/ (reduce + vals) (count vals))
                            sq-diffs (map #(* (- % mean) (- % mean)) vals)]
                        (double (/ (reduce + sq-diffs) (count vals))))))
         x-var (or (variance xs) 0.0)
         y-var (or (variance ys) 0.0)]
     (if (> x-var y-var)
       :vertical
       :horizontal))))

(defn colors-by-dominant-centroid
  "Get colors sorted by centroid along the dominant axis.
   Automatically detects layer orientation and returns:
   - For vertical layers: colors sorted left-to-right (x-centroid)
   - For horizontal layers: colors sorted top-to-bottom (y-centroid)"
  ([grid] (colors-by-dominant-centroid grid true))
  ([grid exclude-zero?]
   (if (= :vertical (layer-orientation grid exclude-zero?))
     (colors-by-centroid-x grid exclude-zero?)
     (colors-by-centroid-y grid exclude-zero?))))

;; ==================== Grid Overlay / Composite ====================

(defn overlay
  "Overlay grid2 on top of grid1. Non-zero cells in grid2 replace cells in grid1.
   Grids must have the same dimensions."
  [grid1 grid2]
  (torch/where (torch/ne grid2 0) grid2 grid1))

(defn overlay-at
  "Overlay pattern onto grid at position (x, y).
   Pattern cells with value 0 are transparent."
  [grid pattern x y]
  (let [[grid-h grid-w] (shape grid)
        [patt-h patt-w] (shape pattern)
        ;; Calculate visible region
        x-start (max 0 x)
        y-start (max 0 y)
        x-end (min grid-w (+ x patt-w))
        y-end (min grid-h (+ y patt-h))
        ;; Pattern offsets (for when pattern is partially off-grid)
        px-start (- x-start x)
        py-start (- y-start y)]
    (if (or (>= x-start x-end) (>= y-start y-end))
      grid
      (let [result (py.. grid clone)
            visible-h (- y-end y-start)
            visible-w (- x-end x-start)
            ;; Get visible portion of pattern
            patt-slice (-> pattern
                           (torch/narrow 0 py-start visible-h)
                           (torch/narrow 1 px-start visible-w))
            ;; Get corresponding region in result
            grid-slice (-> result
                           (torch/narrow 0 y-start visible-h)
                           (torch/narrow 1 x-start visible-w))
            ;; Overlay: non-zero pattern values replace grid values
            overlaid (torch/where (torch/ne patt-slice 0) patt-slice grid-slice)]
        (py.. result
              (narrow 0 y-start visible-h)
              (narrow 1 x-start visible-w)
              (copy_ overlaid))
        result))))

;; ==================

;; perception primitives:
;; (some retina inspired)

;; 1. center-surround ON/OFF
;; 2. edge detection
;; 3. 'inner' / 'outer' detector
;; 4.
;;

(comment
  (def g (grid
          [[0., 0., 8., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 8., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.],
           [0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
            0., 0., 0.]]))

  (max-pool g)
  (py/import-module "torch.nn.functional")
  (color-frequencies (grid [[0 0] [1 0]]))

  (write-rule
   {:kernel
    [[1 0 0 0 0 0]]
    :mask-color 8
    :new-color 8
    :old-color 8
    :threshold 1})

  (conv g (torch/tensor [[1.0 0 0 0 0 0 0]]) 0)

  (torch/roll g 3)

  (torch/roll g 3)

  (count [0., 0., 0., 0., 0., 8., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
          0., 0., 0.])

  (/ 21 3)

  (pack
   (torch/tensor
    [[0 0 0 0 0 0]
     [0 0 0 0 0 0]
     [0 0 0 0 0 0]
     [0 0 0 0 0 0]
     [0 0 0 0 0 0]
     [0 0 0 0 0 0]])
   (torch/tensor
    [[1 2 3]
     [1 2 3]
     [1 2 3]])
   2)

  (py.. g (size 0))
  (py.. g (size 1))

  (F/conv2d
   (py.. g (unsqueeze 0) (unsqueeze 0))
   (torch/tensor
    [[[[0 0 0 0]
       [0 0 0 0]
       [0 0 0 0]
       [1 0 0 0]]]]
    :dtype
    torch/float)
   :padding
   3)

  (F/conv2d
   (py.. g (unsqueeze 0) (unsqueeze 0))
   (torch/tensor
    [[[[1 0 0 0]]]]
    :dtype
    torch/float)
   :padding
   [0 3])

  (F/conv2d
   (py.. g (unsqueeze 0) (unsqueeze 0))
   (torch/tensor [[[[0 1]]]] :dtype torch/float)
   :padding [0 1]
   :stride 1)

  (max-color g))
