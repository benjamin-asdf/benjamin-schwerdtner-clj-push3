(ns benjamin-schwerdtner.clj-push3-play.arc.instructions
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl
    :refer [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   [benjamin-schwerdtner.clj-push3-play.arc.grid-impl :as grid]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]))

(register-instruction
 {:sym-name 'grid_zeroes
  :in [:push/integer :push/integer]
  :out :push/grid
  :f (fn [_ width height] (grid/zeroes [width height]))})

(register-instruction
 {:sym-name 'grid_zeroes_square
  :in [:push/integer]
  :out :push/grid
  :f (fn [_ size] (grid/zeroes [size size]))})

(register-instruction
 {:sym-name 'grid_new
  :in [:push/integer :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ width height color]
       (grid/fill (grid/zeroes [height width]) color))
  :doc "Create a new grid with the specified width, height, and fill color."})

(register-instruction
 {:sym-name 'grid_fill
  :in [:push/grid :push/integer]
  :out :push/grid
  :f (fn [_ g color] (grid/fill g color))})

(register-instruction
 {:sym-name 'grid_color_swap
  :in [:push/grid :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g from to] (grid/color-swap g from to))})

(register-instruction
 {:sym-name 'grid_rows
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g] (grid/rows g))})

(register-instruction
 {:sym-name 'grid_columns
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g] (grid/columns g))})

(register-instruction
 {:sym-name 'grid_window
  :in [:push/grid :push/integer :push/integer :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g x y w h] (grid/window g x y w h))})

(register-instruction
 {:sym-name 'grid_equal
  :in [:push/grid :push/grid]
  :out :push/boolean
  :f (fn [_ g1 g2] (grid/equal? g1 g2))})

(register-instruction
 {:sym-name 'grid_flood_fill
  :in [:push/grid :push/integer :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g x y color] (grid/flood-fill g [x y] color))})

(register-instruction
 {:sym-name 'grid_rotate_90
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/rotate-90 g))})

(register-instruction
 {:sym-name 'grid_rotate_180
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/rotate-180 g))})

(register-instruction
 {:sym-name 'grid_rotate_270
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/rotate-270 g))})

(register-instruction
 {:sym-name 'grid_rotate
  :in [:push/grid :push/integer]
  :out :push/grid
  :f (fn [_ g degrees] (grid/rotate g degrees))})

(register-instruction
 {:sym-name 'grid_flip_horizontal
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/flip-horizontal g))})

(register-instruction
 {:sym-name 'grid_flip_vertical
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/flip-vertical g))})

(register-instruction
 {:sym-name 'grid_flip_diagonal
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/flip-diagonal g))})

(register-instruction
 {:sym-name 'grid_flip_antidiagonal
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g] (grid/flip-antidiagonal g))})

(register-instruction
 {:sym-name 'grid_symmetric_horizontal?
  :in [:push/grid]
  :out :push/boolean
  :f (fn [_ g] (grid/symmetric-horizontal? g))})

(register-instruction
 {:sym-name 'grid_symmetric_vertical?
  :in [:push/grid]
  :out :push/boolean
  :f (fn [_ g] (grid/symmetric-vertical? g))})

(register-instruction
 {:sym-name 'grid_symmetric_diagonal?
  :in [:push/grid]
  :out :push/boolean
  :f (fn [_ g] (grid/symmetric-diagonal? g))})

(register-instruction
 {:sym-name 'grid_rotational_symmetry_order
  :in [:push/grid]
  :out :push/integer
  :f (fn [_ g] (grid/rotational-symmetry-order g))})

(register-instruction
 {:sym-name 'grid_make_symmetric_horizontal
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ half-g] (grid/make-symmetric-horizontal half-g))})

(register-instruction
 {:sym-name 'grid_make_symmetric_vertical
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ half-g] (grid/make-symmetric-vertical half-g))})

(register-instruction
 {:sym-name 'grid_make_symmetric_4fold
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ quarter-g] (grid/make-symmetric-4fold quarter-g))})

(register-instruction
 {:sym-name 'grid_all_rotations
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g] (grid/all-rotations g))})

(register-instruction
 {:sym-name 'grid_all_symmetries
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g] (grid/all-symmetries g))})

(register-instruction
 {:sym-name 'grid_resize_center
  :in [:push/grid :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g new-height new-width]
       (grid/resize-grid-center g new-height new-width))})

(register-instruction
 {:sym-name 'grid_resize_center_bg
  :in [:push/grid :push/integer :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g new-height new-width bg-color]
       (grid/resize-grid-center g new-height new-width bg-color))})

(register-instruction
 {:sym-name 'grid_resize_top_left
  :in [:push/grid :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g new-height new-width]
       (grid/resize-grid-top-left g new-height new-width))})

(register-instruction
 {:sym-name 'grid_resize_top_left_bg
  :in [:push/grid :push/integer :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g new-height new-width bg-color]
       (grid/resize-grid-top-left g new-height new-width bg-color))})

(register-instruction
 {:in
  [[:push/vector :push/integer]
   [:push/vector :push/integer]
   [:push/vector :push/integer]

   :push/integer :push/integer
   :push/integer :push/float]
  :f (fn [_
          kernel-c
          kernel-b
          kernel-a

          mask-color
          new-color
          old-color
          threshold]
       (when (every? (comp #(= % 3) count)
                     [kernel-a kernel-b kernel-c])
         (grid/write-rule {:kernel [kernel-a kernel-b kernel-c]
                           :mask-color mask-color
                           :new-color new-color
                           :old-color old-color
                           :threshold threshold})))
  :out :push/ca-rule
  :sym-name 'grid_make_ca})

(register-instruction
 {:sym-name 'grid_ca_apply_rule
  :in [:push/grid :push/ca-rule]
  :out :push/grid
  :f (fn [_ g rule]
       (grid/ca-apply-rule g rule))})

(register-instruction
 {:sym-name 'grid_reduce_ca
  :in [:push/grid :push/code :push/integer]
  :out :push/grid
  :f (fn [_ g rules steps] (grid/reduce-ca g rules steps))})

(register-instruction
 {:sym-name 'ca_rule_rand_gene
  :in []
  :out :push/ca_rule
  :f (fn [_] (grid/rand-gene))})

(register-instruction
 {:sym-name 'grid_shape
  :in [:push/grid]
  :out :push/code
  ;; returns a sequence, code stack is ok.
  :f (fn [_ g] (grid/shape g))})

(register-instruction
 {:sym-name 'grid_get_cell
  :in [:push/grid :push/integer :push/integer]
  :out :push/integer
  :f (fn [_ g x y]
       (grid/get-cell g x y))})

(register-instruction
 {:sym-name 'grid_set_cell
  :in [:push/grid :push/integer :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g x y color]
       (grid/set-cell g x y color))})

(register-instruction
 {:sym-name 'grid_from_code
  :in [:push/code]
  :out :push/grid
  :f (fn [_ values] (grid/grid values))})

(register-instruction
 {:sym-name 'grid_count_color
  :in [:push/grid :push/integer]
  :out :push/integer
  :f (fn [_ g color] (grid/count-color g color))})

;; (register-instruction
;;  {:sym-name 'grid_repeat_diagonal
;;   :in [:push/grid :push/integer :push/integer]
;;   :out :push/grid
;;   :f (fn [_ g window-size num-reps]
;;        (grid/repeat-pattern-diagonal g window-size num-reps))
;;   :doc "Repeat a pattern from top-left corner diagonally. Takes grid, window size, and number of repetitions."})

;; (register-instruction
;;  {:sym-name 'grid_repeat_horizontal
;;   :in [:push/grid :push/integer]
;;   :out :push/grid
;;   :f (fn [_ g num-reps]
;;        (grid/repeat-pattern-horizontal g num-reps))
;;   :doc "Repeat the 3x3 pattern horizontally n times."})

(register-instruction
 {:sym-name 'grid_count_non_zero
  :in [:push/grid]
  :out :push/integer
  :f (fn [_ g] (grid/grid-count-non-zero g))
  :doc "Count the number of non-zero cells in the grid."})

(register-instruction
 {:sym-name 'grid_color_frequencies
  :in [:push/grid]
  :out [:push/vector :push/integer]
  :f (fn [_ g]
       (grid/color-frequencies g))})

(register-instruction
 {:sym-name 'grid_distinct_colors
  :in [:push/grid]
  :out [:push/vector :push/integer]
  :f (fn [_ g]
       (grid/distinct-colors g))})

(register-instruction
 {:sym-name 'grid_max_color
  :in [:push/grid]
  :out :push/integer
  :f (fn [_ g] (grid/max-color g))})

(register-instruction
 {:sym-name 'grid_max_color_nonzero
  :in [:push/grid]
  :out :push/integer
  :f (fn [_ g] (grid/max-color-non-zero g))})

(register-instruction
 {:sym-name 'grid_positions_of_color
  :in [:push/grid :push/integer]
  :out :push/code
  :f (fn [_ g color] (grid/positions-of-color g color))})

(register-instruction
 {:sym-name 'grid_positions_of_max_color
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g] (grid/positions-of-max-color g))})

(register-instruction
 {:sym-name 'grid_color_centroid
  :in [:push/grid :push/integer]
  :out :push/code
  :f (fn [_ g color]
       (or (grid/color-centroid g color) []))
  :doc "Get the centroid [x y] of a color in the grid."})

(register-instruction
 {:sym-name 'grid_colors_by_centroid_x
  :in [:push/grid]
  :out [:push/vector :push/integer]
  :f (fn [_ g] (grid/colors-by-centroid-x g))
  :doc "Get distinct colors sorted by x-centroid (left to right)."})

(register-instruction
 {:sym-name 'grid_colors_by_centroid_x_nonzero
  :in [:push/grid]
  :out [:push/vector :push/integer]
  :f (fn [_ g] (grid/colors-by-centroid-x g true))
  :doc "Get distinct non-zero colors sorted by x-centroid (left to right)."})

(register-instruction
 {:sym-name 'grid_colors_by_centroid_y
  :in [:push/grid]
  :out [:push/vector :push/integer]
  :f (fn [_ g] (grid/colors-by-centroid-y g))
  :doc "Get distinct colors sorted by y-centroid (top to bottom)."})

(register-instruction
 {:sym-name 'grid_colors_by_centroid_y_nonzero
  :in [:push/grid]
  :out [:push/vector :push/integer]
  :f (fn [_ g] (grid/colors-by-centroid-y g true))
  :doc "Get distinct non-zero colors sorted by y-centroid (top to bottom)."})

(register-instruction
 {:sym-name 'grid_from_vector_column
  :in [[:push/vector :push/integer]]
  :out :push/grid
  :f (fn [_ v]
       (when (seq v)
         (grid/grid (mapv vector v))))
  :doc "Create a column grid (Nx1) from a vector of integers."})

(register-instruction
 {:sym-name 'grid_from_vector_row
  :in [[:push/vector :push/integer]]
  :out :push/grid
  :f (fn [_ v]
       (when (seq v)
         (grid/grid [v])))
  :doc "Create a row grid (1xN) from a vector of integers."})

(register-instruction
 {:sym-name 'grid_layer_orientation
  :in [:push/grid]
  :out :push/boolean
  :f (fn [_ g]
       (= :vertical (grid/layer-orientation g true)))
  :doc "Returns true if layers are vertical (use x-centroid), false if horizontal."})

(register-instruction
 {:sym-name 'grid_colors_by_dominant_centroid
  :in [:push/grid]
  :out [:push/vector :push/integer]
  :f (fn [_ g] (grid/colors-by-dominant-centroid g true))
  :doc "Get colors sorted by dominant axis centroid (auto-detects orientation)."})

(register-instruction
 {:sym-name 'grid_layer_scan
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g]
       (let [colors (grid/colors-by-dominant-centroid g true)]
         (when (seq colors)
           (if (= :vertical (grid/layer-orientation g true))
             (grid/grid [colors])           ; row for vertical layers
             (grid/grid (mapv vector colors)))))) ; column for horizontal layers
  :doc "Scan layers and output colors in order. Row for vertical, column for horizontal."})

(register-instruction
 {:sym-name 'grid_max_pool
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g]
       (grid/max-pool g))})

;; ----------------------------

(register-instruction
 {:sym-name 'grid_pack
  :in [:push/grid :push/grid :push/integer]
  :out :push/grid
  :f (fn [_ g p n]
       (grid/pack g p n))})

(register-instruction
 {:sym-name 'grid_fill_with_pattern
  :in [:push/grid :push/grid :push/grid]
  :out :push/grid
  :f (fn [_ g p mp]
       (grid/fill-with-pattern g p mp))})

(register-instruction
 {:sym-name 'grid_filter_color
  :in [:push/grid :push/integer]
  :out :push/grid
  :f (fn [_ g c]
       (grid/filter-color g c))})

;; ==================== Tiling Instructions ====================

(register-instruction
 {:sym-name 'grid_tile
  :in [:push/grid :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ g target-height target-width]
       (grid/tile g target-height target-width))
  :doc "Tile the pattern to fill a grid of target dimensions."})

(register-instruction
 {:sym-name 'grid_tile_n
  :in [:push/grid :push/integer]
  :out :push/grid
  :f (fn [_ g n]
       (grid/tile-n g n))
  :doc "Tile the pattern n times in each direction."})

;; ==================== Bounding Box / Crop Instructions ====================

(register-instruction
 {:sym-name 'grid_bounding_box
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g]
       (or (grid/bounding-box g) []))
  :doc "Get bounding box [min-y min-x max-y max-x] of non-zero cells."})

(register-instruction
 {:sym-name 'grid_crop_to_content
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g]
       (grid/crop-to-content g))
  :doc "Crop grid to minimal bounding box of non-zero cells."})

;; ==================== Connected Components Instructions ====================

(register-instruction
 {:sym-name 'grid_connected_components
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g]
       (grid/connected-components g))
  :doc "Label connected components with unique integers."})

(register-instruction
 {:sym-name 'grid_extract_objects
  :in [:push/grid]
  :out :push/code
  :f (fn [_ g]
       (grid/extract-objects g))
  :doc "Extract all connected objects as a list of {:grid :bbox :color}."})

(register-instruction
 {:sym-name 'grid_first_object
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g]
       (let [objs (grid/extract-objects g)]
         (if (seq objs)
           (:grid (first objs))
           g)))
  :doc "Extract the first (top-left) connected object from the grid."})

(register-instruction
 {:sym-name 'grid_largest_object
  :in [:push/grid]
  :out :push/grid
  :f (fn [_ g]
       (let [objs (grid/extract-objects g)]
         (if (seq objs)
           (:grid (apply max-key #(grid/grid-count-non-zero (:grid %)) objs))
           g)))
  :doc "Extract the largest connected object from the grid."})

;; ==================== Overlay Instructions ====================

(register-instruction
 {:sym-name 'grid_overlay
  :in [:push/grid :push/grid]
  :out :push/grid
  :f (fn [_ g1 g2]
       (if (and (= (grid/shape g1) (grid/shape g2)))
         (grid/overlay g1 g2)
         g1))
  :doc "Overlay g2 on g1 (non-zero cells in g2 replace g1)."})

(register-instruction
 {:sym-name 'grid_overlay_at
  :in [:push/grid :push/grid :push/integer :push/integer]
  :out :push/grid
  :f (fn [_ base pattern x y]
       (grid/overlay-at base pattern x y))
  :doc "Overlay pattern onto base at position (x, y)."})

;; --------------------------------------------

(register-instruction
 {:sym-name 'color_gensym
  :in []
  :out :state
  :f (fn [state]
       ;; color gensym counter is for 1 program execution,
       ;; this way parts of the program can refer to the same color
       (let [i (::color-gensym-counter state (count grid/colors))]
         (-> state
             (assoc ::color-gensym-counter i)
             (stack/push :push/integer i))))})

;; --------------------

(doseq [[sym-name impl]
        [['grid_stackdepth impl/stack-depth-impl]
         ['grid_pop impl/popper]
         ['grid_dup impl/dupper]
         ['grid_flush impl/flusher]
         ['grid_shove impl/shover]
         ['grid_yank impl/yanker]
         ['grid_yankdup impl/yankdupper]
         ['grid_rot impl/rotater]
         ['grid_swap impl/swapper]
         ['grid_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/grid})))
