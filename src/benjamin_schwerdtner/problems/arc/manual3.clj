(ns benjamin-schwerdtner.problems.arc.manual3
  (:require
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.problems.arc.data :as d]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [benjamin-schwerdtner.clj-push3-play.arc.grid-impl :as grid]
   [benjamin-schwerdtner.problems.arc.push :as arc]
   [benjamin-schwerdtner.clj-push3-play.hdc.ssp-grid :refer [encode-grid]]
   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as fhrr]
   [benjamin-schwerdtner.clj-push3-play.arc.instructions]
   [libpython-clj2.require :refer [require-python]]
   [benjamin-schwerdtner.clj-push3-play.hdc.spatial-semantic-pointer :as ssp]
   [libpython-clj2.python :refer [py. py..] :as py]))

(require-python '[torch :as torch]
                '[torch.nn.functional :as F])

;; --------------------------------------------

(comment

  (def task (d/read-task "/home/benj/repos/ARC-AGI-2/data/training/d631b094.json"))

  (arc/run-push-on-grid-1
   (-> task
       :test
       first
       :input)
   '(grid_dup
     grid_max_color_nonzero
     grid_count_non_zero
     (1 grid_zeroes)
     (grid_fill)))

  (arc/run-task task
                '(grid_dup
                  grid_max_color_nonzero
                  grid_count_non_zero
                  (1 grid_zeroes)
                  (grid_fill)))

  '{:test-out
    ({:out [[4 4 4 4 4]]
      :pair {:input [[4 4 0] [4 0 4] [0 0 4]]
             :output [[4 4 4 4 4]]}
      :success? true})
    :test-success [true]
    :train-out
    ({:out [[7]]
      :pair {:input [[0 7 0] [0 0 0] [0 0 0]] :output [[7]]}
      :success? true}
     {:out [[2 2 2]]
      :pair {:input [[0 2 0] [2 0 0] [0 2 0]] :output [[2 2 2]]}
      :success? true}
     {:out [[8 8 8 8]]
      :pair {:input [[0 8 0] [8 8 0] [8 0 0]] :output [[8 8 8 8]]}
      :success? true}
     {:out [[1 1]]
      :pair {:input [[0 0 0] [1 0 0] [0 1 0]] :output [[1 1]]}
      :success? true})
    :train-success [true true true true]})

;; ---------------------------------------

(comment

  (def task (d/read-task "/home/benj/repos/ARC-AGI-2/data/training/c3e719e8.json"))

  (arc/run-push-on-grid-1
   (-> task :test first :input)
   '(grid_dup
     grid_dup
     grid_max_color
     grid_filter_color
     ;; grid_dup
     grid_swap
     (3 3 integer_* grid_zeroes_square)
     (grid_fill_with_pattern)))

  (arc/run-task
   task
   '(grid_dup
     grid_dup
     grid_max_color
     grid_filter_color
     (3 3 integer_* grid_zeroes_square)
     (grid_swap grid_fill_with_pattern))))

(comment
  (def task (d/read-task "/home/benj/repos/ARC-AGI-2/data/training/4be741c5.json"))

  ;;
  ;; color correspondance: same colors in input and output
  ;; output: color count == grid element count
  ;;
  ;; objectness: vertical/horizontal, from side to side
  ;;  - object: same color
  ;;  - object: non symetric
  ;;  - object: long/wide 'tall'
  ;;
  ;; output is smaller than input -> pooling?
  ;;
  ;;
  ;; colors in 'layers'.
  ;; the size of a color 'layer' doesn't matter. But they all go from one to the other side.
  ;; Solution to the grid are the layers.
  ;; can be flipped 90degree
  ;;
  ;; - the same alg with a profunctor that flips input-output solves it
  ;;   - how to flip?
  ;;

  ;;
  ;; - color correspondance and color count == output count suggest an 'ordering' concept.
  ;;
  ;; - colors going through the whole grid remind me of 'colums' or 'layers'.
  ;; - (In some ways, the concept of 'layer' is the key to the puzzle)
  ;;
  ;;
  ;; - no symmetry
  ;;

  ;; - implementing a 'contrast scan' would solve the problem
  ;; - 'object topology'
  ;;

  (def grid
    (torch/tensor
     [[3 3 3 3 2 2 2 2 2 1 1 1 8 8]
      [3 3 3 2 2 2 2 2 1 1 1 8 8 8]
      [3 3 3 3 3 2 2 1 1 1 8 8 8 8]
      [3 3 3 3 3 2 2 1 1 1 1 8 8 8]
      [3 3 3 3 2 2 2 2 2 1 1 1 8 8]
      [3 3 3 3 3 2 2 2 2 1 1 1 1 8]
      [3 3 3 2 2 2 2 2 1 1 1 1 8 8]
      [3 3 3 3 2 2 2 2 1 1 1 8 8 8]
      [3 3 3 3 2 2 2 2 1 1 1 1 8 8]
      [3 3 3 2 2 2 2 2 2 1 1 1 8 8]
      [3 3 3 2 2 2 2 2 2 1 1 8 8 8]
      [3 3 3 3 2 2 2 2 1 1 1 1 8 8]
      [3 3 3 3 3 2 2 2 1 1 1 1 8 8]
      [3 3 3 3 3 3 2 2 2 1 1 1 8 8]]
     :dtype torch/float))

  (def color-codebook (fhrr/seed 10))
  (defn color-hdv [color] (py/get-item color-codebook color))

  (arc/run-push-on-grid-1
   (-> task :test first :input)
   ;; --------------------------
   '((0
      grid_distinct_colors
      vector_integer_dup
      vector_integer_length
      1
      integer_swap
      grid_new)
     (vector_integer_pushall)))

  [(ssp/readout (ssp/query-object
                 (encode-grid grid color-codebook [1 1])
                 (color-hdv 3))
                {:height 1 :resolution 0.1 :width 1})
   (ssp/readout (ssp/query-object
                 (encode-grid grid color-codebook [1 1])
                 (color-hdv 2))
                {:height 1 :resolution 0.1 :width 1})
   (ssp/readout (ssp/query-object
                 (encode-grid grid color-codebook [1 1])
                 (color-hdv 1))
                {:height 1 :resolution 0.1 :width 1})
   (ssp/readout (ssp/query-object
                 (encode-grid grid color-codebook [1 1])
                 (color-hdv 8))
                {:height 1 :resolution 0.1 :width 1})]

  [[0.5 0.10000000149011612]
   [0.5 0.4000000059604645]
   [0.5 0.699999988079071]
   [0.5 0.8999999761581421]]

  ;; ==================== Centroid Ordering Solution ====================
  ;;
  ;; Using the new grid_colors_by_centroid_x instruction:
  ;;
  ;; The Push3 program to solve this task:
  ;;
  ;; grid_colors_by_centroid_x_nonzero  ; get [3 2 1 8] - colors sorted by x-centroid
  ;; vector_integer_length              ; get count (4)
  ;; 1                                  ; height = 1
  ;; grid_new                           ; create 1x4 grid
  ;; ... or use vector_integer_pushall to build the output grid
  ;;

  ;; Test the centroid functions directly:
  (grid/color-centroid grid 3)   ;; => left-most centroid
  (grid/color-centroid grid 2)
  (grid/color-centroid grid 1)
  (grid/color-centroid grid 8)   ;; => right-most centroid

  (grid/colors-by-centroid-x grid true) ;; => [3.0 2.0 1.0 8.0] (sorted left to right)

  ;; The Push3 solution:
  (arc/run-push-on-grid-1
   (-> task :train first :input)
   '(grid_colors_by_centroid_x_nonzero
     (vector_integer_length
      1
      integer_swap
      grid_new)
     (vector_integer_pushall)))

  (fhrr/unbind
   (encode-grid grid color-codebook [1 1])
   (ssp/spatial-semantic-pointer [0 0])))
