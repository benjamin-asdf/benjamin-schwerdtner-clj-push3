(ns benjamin-schwerdtner.clj-push3-play.arc.grid-transforms
  "Symmetry, rotation, and retina-like transformations for ARC grids"
  (:require
   [benjamin-schwerdtner.clj-push3-play.arc.grid :as grid]
   [libpython-clj2.require :refer [require-python]]
   [libpython-clj2.python :refer [py. py..] :as py]))

(require-python '[torch :as torch]
                '[torch.nn.functional :as F])

;; ==================== Retina-like Operations ====================

(defn gabor-kernel
  "Create a Gabor filter kernel for edge detection at specific orientations"
  [size sigma theta lambda-wave psi gamma]
  (let [center (/ (dec size) 2.0)
        x-range (torch/arange (- center) (inc center))
        y-range (torch/arange (- center) (inc center))
        [x-grid y-grid] (torch/meshgrid [x-range y-range])

        ;; Rotation
        x-theta (torch/add (torch/mul x-grid (torch/cos theta))
                           (torch/mul y-grid (torch/sin theta)))
        y-theta (torch/sub (torch/mul y-grid (torch/cos theta))
                           (torch/mul x-grid (torch/sin theta)))

        ;; Gaussian envelope
        gaussian (torch/exp (torch/neg
                             (torch/div
                              (torch/add (torch/div (torch/pow x-theta 2)
                                                    (* 2 sigma sigma))
                                         (torch/div (torch/pow y-theta 2)
                                                    (* 2 sigma sigma gamma gamma)))
                              2)))

        ;; Sinusoidal carrier
        sinusoid (torch/cos (torch/add (torch/mul (torch/div (* 2 Math/PI) lambda-wave) x-theta)
                                       psi))]
    (torch/mul gaussian sinusoid)))

(defn edge-detection-bank
  "Create a bank of edge detection kernels at different orientations"
  [size num-orientations]
  (let [angles (map #(* % (/ Math/PI num-orientations)) (range num-orientations))]
    (torch/stack
     (vec (for [theta angles]
            (gabor-kernel size 2.0 theta 4.0 0.0 0.5))))))

(defn retina-convolution
  "Apply retina-like convolution with multiple oriented filters"
  [grid num-orientations]
  (let [kernels (edge-detection-bank 5 num-orientations)
        grid-expanded (py.. grid (unsqueeze 0) (unsqueeze 0))
        kernels-expanded (py.. kernels (unsqueeze 1))
        responses (F/conv2d grid-expanded kernels-expanded :padding 2)]
    ;; Take maximum response across all orientations
    (py.. (torch/max responses 1) values (squeeze 0))))

;; ==================== Center-Surround Operations ====================

(defn center-surround-kernel
  "Create a center-surround kernel (like retinal ganglion cells)"
  [size center-weight surround-weight]
  (let [kernel (torch/full [size size] surround-weight)
        center (/ (dec size) 2)]
    (py/set-item! kernel [center center] center-weight)))

(comment
  (center-surround-kernel 3 0 1)

  ;; tensor([[1, 1, 1],
  ;;        [1, 0, 1],
  ;;        [1, 1, 1]])
  )

(defn on-center-off-surround
  "Simulate ON-center OFF-surround retinal ganglion cells"
  [grid]
  (let [kernel (center-surround-kernel 3 2.0 -0.5)]
    (py.. (F/conv2d (py.. grid (unsqueeze 0) (unsqueeze 0))
                    (py.. kernel (unsqueeze 0) (unsqueeze 0))
                    :padding 1)
      (squeeze 0)
      (squeeze 0))))

(defn off-center-on-surround
  "Simulate OFF-center ON-surround retinal ganglion cells"
  [grid]
  (let [kernel (center-surround-kernel 3 -2.0 0.5)]
    (py.. (F/conv2d (py.. grid (unsqueeze 0) (unsqueeze 0))
                    (py.. kernel (unsqueeze 0) (unsqueeze 0))
                    :padding 1)
          (squeeze 0)
          (squeeze 0))))

;; ==================== Multi-scale Retina Processing ====================

(defn gaussian-pyramid
  "Create a Gaussian pyramid for multi-scale processing"
  [grid levels]
  (loop [pyramid [grid]
         level 1]
    (if (>= level levels)
      pyramid
      (let [blurred (py.. (F/avg_pool2d (py.. (last pyramid) (unsqueeze 0) (unsqueeze 0))
                                        2 :stride 2)
                          (squeeze 0)
                          (squeeze 0))]
        (recur (conj pyramid blurred) (inc level))))))

(defn laplacian-pyramid
  "Create a Laplacian pyramid for edge detection at multiple scales"
  [grid levels]
  (let [gaussian-pyr (gaussian-pyramid grid levels)]
    (vec (for [i (range (dec (count gaussian-pyr)))]
           (let [current (nth gaussian-pyr i)
                 next-upsampled (py.. (F/interpolate
                                       (py.. (nth gaussian-pyr (inc i)) (unsqueeze 0) (unsqueeze 0))
                                       :size (py.. current shape)
                                       :mode "nearest")
                                      (squeeze 0)
                                      (squeeze 0))]
             (torch/sub current next-upsampled))))))

;; ==================== Retina CA Rules ====================

(defn lateral-inhibition-rule
  "CA rule for lateral inhibition (edge enhancement)"
  [active-color inactive-color threshold]
  (grid/write-rule
   {:kernel [[-0.5 -0.5 -0.5]
             [-0.5 4.0 -0.5]
             [-0.5 -0.5 -0.5]]
    :mask-color active-color
    :new-color active-color
    :old-color inactive-color
    :threshold threshold}))

(defn receptive-field-rule
  "CA rule simulating receptive field with center-surround organization"
  [center-color surround-color output-color]
  (grid/write-rule
   {:kernel [[0.2 0.2 0.2]
             [0.2 -1.0 0.2]
             [0.2 0.2 0.2]]
    :mask-color center-color
    :new-color output-color
    :old-color surround-color
    :threshold 0.5}))

(defn orientation-selective-rule
  "CA rule for orientation-selective cells (like V1 simple cells)"
  [orientation]
  (let [kernels {:horizontal [[0.0 0.0 0.0]
                              [1.0 1.0 1.0]
                              [0.0 0.0 0.0]]
                 :vertical [[0.0 1.0 0.0]
                            [0.0 1.0 0.0]
                            [0.0 1.0 0.0]]
                 :diagonal-right [[1.0 0.0 0.0]
                                  [0.0 1.0 0.0]
                                  [0.0 0.0 1.0]]
                 :diagonal-left [[0.0 0.0 1.0]
                                 [0.0 1.0 0.0]
                                 [1.0 0.0 0.0]]}]
    (grid/write-rule
     {:kernel (get kernels orientation (:horizontal kernels))
      :mask-color 1
      :new-color 2
      :old-color 0
      :threshold 2})))

;; ==================== Symmetry-based CA ====================

(defn symmetry-detection-ca
  "CA that detects and propagates symmetry patterns"
  [grid iterations]
  (let [;; Rules for detecting symmetry patterns
        h-sym-rule (grid/write-rule
                    {:kernel [[1.0 0.0 1.0]
                              [0.0 0.0 0.0]
                              [0.0 0.0 0.0]]
                     :mask-color 1
                     :new-color 2
                     :old-color 0
                     :threshold 2})
        v-sym-rule (grid/write-rule
                    {:kernel [[1.0 0.0 0.0]
                              [0.0 0.0 0.0]
                              [1.0 0.0 0.0]]
                     :mask-color 1
                     :new-color 3
                     :old-color 0
                     :threshold 2})]
    (-> grid
        (grid/reduce-ca h-sym-rule iterations)
        (grid/reduce-ca v-sym-rule iterations))))

;; ==================== Helper Functions ====================

(defn apply-transform-sequence
  "Apply a sequence of transformations to a grid"
  [grid transforms]
  (reduce (fn [g transform]
            (transform g))
          grid
          transforms))

(comment

  (make-symmetric-4fold
   (torch/tensor
    [[0 1 0]
     [0 2 0]]))
































  (symmetry-detection-ca (grid/grid [[1 0 1] [0 0 0] [0 0 0] [0 0 0]
                                     [0 0 0] [0 0 0]]))


  (all-symmetries
   (grid/grid [[1 0 2]
               [0 0 0]
               [0 0 0]
               [0 0 0]
               [0 0 0]
               [0 0 0]]))


  (all-rotations
   (grid/grid [[1 0 0]
               [0 0 0]
               [0 0 0]]))



  )
