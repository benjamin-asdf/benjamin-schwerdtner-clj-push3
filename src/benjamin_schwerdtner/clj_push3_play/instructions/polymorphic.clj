(ns benjamin-schwerdtner.clj-push3-play.instructions.polymorphic
  "The stack operators that are the same on every stack.

  Push3 as specified gives each type its own DUP/POP/SWAP/ROT/YANK/SHOVE/... ,
  and this implementation follows that: those live in the per-type files, built
  from the generators in `instructions/impl.clj`. What was missing is the
  handful PushGP added afterwards and that the PSB suites' instruction sets
  include for every stack - `_empty`, `_dup_times`, `_dup_items`, `_deep_dup`.

  They are registered here rather than in the per-type files because there is
  nothing type-specific left to say about them, and because a single list is
  the only way to keep the set the same across eleven types."
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction polymorphic-operators]]
   [benjamin-schwerdtner.clj-push3-play.instructions.input-output :refer
    [type->prefix]]))

(def polymorphic-types
  "Every stack that carries values a program can compute with.

  NAME, BIGNUM, GRID, TASK, CA-RULE and CLJ-OBJECT are deliberately out: they
  are this implementation's own additions rather than part of the PushGP
  vocabulary, and the four operators below are here to complete that
  vocabulary. Adding them would grow the alphabet a random program draws from
  without giving any of these problems anything to draw."
  [:push/boolean :push/char :push/code :push/exec :push/float :push/integer
   :push/string
   [:push/vector :push/boolean]
   [:push/vector :push/char]
   [:push/vector :push/float]
   [:push/vector :push/integer]
   [:push/vector :push/string]])

(doseq [t polymorphic-types
        [suffix generator] polymorphic-operators]
  (register-instruction
   (generator {:sym-name (keyword (str (type->prefix t) suffix))
               :push-type t})))
