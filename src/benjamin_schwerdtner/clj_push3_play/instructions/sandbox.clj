(ns benjamin-schwerdtner.clj-push3-play.instructions.sandbox
  "`:code_run` - the interpreter, reflectively available from inside Push.

  Push3 already has a form of eval: :code_do pushes code onto EXEC and it runs.
  But it runs in the CALLER's state, on the caller's stacks. For a mutation
  operator that wants to *examine* a candidate genome that is useless - the
  candidate would rummage through the very CODE stack that holds the genome
  being mutated. That is contamination, not evaluation.

  :code_run gives a fresh interpreter state, a hard step budget of its own, and
  hands back only a number.

  ---------------------------------------------------------------------------
  Why this is a bigger change than it looks
  ---------------------------------------------------------------------------

  In nature, evaluating a genome means building an organism and running it in
  the world. There is no fast-forward, and the generation time IS the cost of
  evaluation. That is why a mutation operator in biology cannot assess what it
  is mutating - not because it is forbidden, but because assessment is as
  expensive as the thing it would inform.

  (With one striking exception: a pollen tube expresses a substantial part of
  its haploid genome while growing down the style, so the style is a real
  performance test of that genome. In animals the same thing looks actively
  suppressed - sperm chromatin is protamine-compacted and transcriptionally
  quiet, and spermatids share cytoplasm through intercellular bridges, so the
  haploid genotype hides behind the diploid father's products.)

  Once evaluation is cheap, the line between `variation operator` and `search
  algorithm` dissolves. An M with :code_run is no longer a mutation operator,
  it is an evolved local search - a memetic algorithm whose local search is a
  genotype. Expect M to drift toward hill climbing and the outer loop to
  become vestigial; that is the thing to watch for, not a bug to prevent.

  What keeps it honest is that M still does not know the target. It can run a
  program and see a number, but not whether the number is right. So the
  proxies it can actually build are things like: does this candidate produce
  anything at all, does its output move when the input moves, does it behave
  differently from its parent. That last one is novelty search, reachable as a
  genotype rather than installed by hand."
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl
    :refer [register-instruction]]
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]))

(def default-sandbox-executions
  "Step budget for one sandboxed run. Small: M is expected to probe a
  candidate several times, and every one of those steps is charged to M."
  200)

(defn- finite
  [v]
  (when (and v (not (Double/isNaN v)) (not (Double/isInfinite v)))
    v))

(defn sandbox-run
  "Run `code` on input `x` in a fresh state. Returns {:steps n :value v-or-nil}."
  [state code x]
  (try
    (let [budget (get-in state [:parameters :sandbox-max-executions]
                         default-sandbox-executions)
          child (-> (push/setup-state)
                    ;; Inherit the caller's resource limits. Without this the
                    ;; probe runs under the config DEFAULTS, which can be far
                    ;; more permissive than the domain it is probing: on the
                    ;; busy-beaver landscape the organism gets a 10^4 bit
                    ;; BIGNUM budget while a fresh state gets 10^5, so one
                    ;; probe of a squaring organism cost ~45ms - more than
                    ;; evaluating the organism for real. A mutator could then
                    ;; spend more compute looking at a candidate than the loop
                    ;; spends scoring it, and a run that should take a second
                    ;; took over half an hour.
                    (update :parameters merge (:parameters state))
                    (assoc :exceptions-are-warnings? true
                           ;; Blocks nesting - see :code_run below.
                           ::sandboxed? true
                           ;; Deterministic, but not the same stream on every
                           ;; probe: a mutator that runs a stochastic
                           ;; candidate twice should be able to see it differ.
                           :push/random-seed (+ (long (:push/random-seed state 0))
                                                (long (:push/num-executions state 0))))
                    (stack/push :push/float (double x)))
          out (push/execute child code {:max-executions budget})
          fstack (-> out :stacks :push/float)]
      {:steps (:push/num-executions out 0)
       :value (when (seq fstack) (finite (double (peek fstack))))})
    (catch Throwable _
      ;; Evolved code recurses; StackOverflowError is an Error and the
      ;; interpreter's own handler never sees it.
      {:steps 0 :value nil})))

(register-instruction
 {:sym-name :code_run
  :in [:push/code :push/float]
  :out :state
  :doc "Run the top CODE item on the top FLOAT as input, in a fresh
interpreter state, and push what it produced onto FLOAT.

The code goes back on CODE - probing a candidate must not consume it, since
the point is usually to decide whether to mate with it.

Produces nothing only when the candidate leaves the FLOAT stack empty, or
diverges in a way the sandbox cannot absorb.

NOTE the input convention and what it costs. x is pushed onto FLOAT and the
answer is read off FLOAT, matching how hosts are run. So a candidate that
touches nothing hands back x - an inert program IS the identity function, and
a mutator cannot tell 'did nothing' from 'returned its input'. Getting that
distinction would mean delivering x somewhere the candidate has to reach for,
a name binding or an input register, which would change how every host reads
its input too.

The sandbox's steps are charged to the CALLER's execution count, so a mutator
that evaluates pays for what it runs out of its own budget. Without that,
assessment would be free and there would be no way to see whether M had
quietly turned itself into a hill climber."
  :f
  (fn [state code x]
    (if (::sandboxed? state)
      ;; No nesting. M may run a genome; a genome may not run a genome. One
      ;; reflective level, so the cost of a probe stays bounded and knowable.
      (stack/push state :push/code code)
      (let [{:keys [steps value]} (sandbox-run state code x)
            state (-> state
                      (stack/push :push/code code)
                      (update :push/num-executions (fnil + 0) steps))]
        (if value
          (stack/push state :push/float value)
          state))))})

(comment
  (require '[benjamin-schwerdtner.clj-push3-play.mutation.mut :as mut])

  ;; Mate only with candidates whose output moves when the input moves -
  ;; reject constants. Domain independent: it needs no idea of the target.
  (def choosy
    '(:code_other
      1.0 :code_run
      2.0 :code_run
      :float_= :exec_if (:mut_interleave) (:code_pop)))

  (mut/mutate '(1 2 3) choosy {:population ['(:float_dup :float_*)]})
  (mut/mutate '(1 2 3) choosy {:population ['(5.0)]}))
