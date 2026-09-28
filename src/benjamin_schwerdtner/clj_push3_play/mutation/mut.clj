(ns benjamin-schwerdtner.clj-push3-play.mutation.mut
  (:require
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   ;; Registers :code_run. It lives outside instructions/interface.clj because
   ;; it needs the interpreter itself, and interface is what the interpreter
   ;; requires - putting it there would be a cycle. Required here so that
   ;; anything using mutation has the instruction in the table.
   [benjamin-schwerdtner.clj-push3-play.instructions.sandbox]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]))

;; ---------------------------------------------------------------------------------------
;;
;; Mutation design:
;;
;;
;;
;; mutate(mutator-program, input-program, random-seed) -> variations
;;
;;
;; - `input-program` is a PUSH program.
;; - `mutator-program` is a PUSH program.
;; - `random-seed` is a number used for deterministic randomness.
;;
;;
;; mutate:
;;
;; 0. setup Push interpreter with `mutator-program` on the EXEC stack.
;; 1. Push `input-program` as list on the CODE stack.
;; 2. Set `random-seed` as special state for program execution.
;; 3. Execute program (with limits, etc.)
;; 4. Filter the CODE stack for elements of type list, called programs.
;; 5. Each such program counts as 1 potential VARIATION of `input-program`.
;;
;; - proceed by using `n-variations` variations in your EA.
;;
;; Optionally: they are sorted?
;;
;;
;; Future:
;; - translate Push programs in high dimensional vectors and back,
;;   allow 'neuronal' mutation on subsymbolic representations, + have a door into deep learning etc.
;;
;;
;; ---------------------------------------------------------------------------------------
;;
;; Notes on the implementation, ideas/metaevolution.org:
;;
;; REPRODUCTIVE COMPETENCE is free here. `input-program` starts on the CODE
;; stack, so a mutator that does nothing at all still leaves it there and
;; `mutate` returns a clone. Spector's Pushpop had to prop up early
;; generations because random programs had not evolved to use the CHILD
;; instructions yet; the default here is asexual cloning, and every mutator is
;; born fertile. A mutator earns its keep only by doing better than cloning.
;;
;; EPIGENETICS IS METADATA ON THE TREE, not a side register. `M` reads with
;; :code_meta_get and writes with :code_meta_set / :code_meta_inc (see
;; instructions/mutation.clj). Because a marker is attached to a code object
;; rather than pointing at it by name, it survives the region being moved
;; around - and a duplicated subtree hands the marker to both copies, which
;; is the substrate gene duplication needs.
;;
;; Reading is the third order of metaevolution.org - `mut` with dynamic
;; evolvability, e.g. mutate a genome marked 'old' less, raise the rate when
;; the individual was starving. Writing is the leap of faith in that
;; document: signals from `mut` that outlive the individual, since whatever M
;; writes is on the tree the child inherits.
;;
;; There is no propagation policy in this namespace. Whether a counter
;; survives its region being modified is M's business - M is evolved, so the
;; bookkeeping discipline is evolved rather than designed here.
;;
;; ---------------------------------------------------------------------------------------

(def default-max-executions
  "Step budget for one mutator run.

  A mutator is a program in an inner loop of an evolutionary run - it is
  executed once per offspring - so this is deliberately far below the budget
  you would give an organism."
  (long 1e4))

(defn with-signals
  "Merge the caller's signals into `program`'s metadata.

  This is how the loop tells M about the individual it is about to mutate -
  age, stagnation, whether it is starving. It merges rather than replaces, so
  markers a previous mutator wrote survive."
  [program signals]
  (if (and (seq signals) (instance? clojure.lang.IObj program))
    (vary-meta program merge signals)
    program))

(defn setup
  "Interpreter state for running a mutator over `input-program`."
  [input-program {:keys [seed epigenetics alphabet population graveyard
                         parameters]}]
  (-> (push/setup-state)
      (stack/push :push/code (with-signals input-program epigenetics))
      ;; The domain's resource limits, so a mutator - and anything it runs in
      ;; a :code_run sandbox - is bounded the same way the domain is. A
      ;; mutator left on config defaults can be far more expensive than the
      ;; fitness function it is feeding.
      (update :parameters merge parameters)
      ;; What :code_other and :code_elder draw from. A vector of programs, no
      ;; fitness attached - M gets a sample, not a ranking.
      (assoc :push/population (vec population)
             :push/graveyard (vec graveyard))
      (cond->
        ;; :code_rand_atom draws from `:instructions` (see
        ;; instructions/code.clj). By default that is every instruction in the
        ;; table - grid ops, string ops, the lot - so a mutator that inserts a
        ;; random atom almost always inserts one that is meaningless in the
        ;; target domain. Handing in the domain's alphabet is what makes
        ;; insertion a useful move rather than noise.
       alphabet (assoc :instructions alphabet))
      (assoc :push/random-seed (long (or seed 0))
             ;; A mutator is evolved code: it will call instructions on
             ;; stacks that are empty, coerce nonsense, divide by zero. Under
             ;; evolution those are ordinary events, not bugs, so they degrade
             ;; to NOOPs and the program runs on. Without this a single bad
             ;; instruction anywhere would throw away everything the mutator
             ;; had already built.
             :exceptions-are-warnings? true)))

(defn run
  "Runs `mutator-program` over `input-program`. Returns the final state, or nil
  if the run died in a way the interpreter could not absorb."
  [input-program mutator-program {:as opts :keys [max-executions]}]
  (try
    ;; :exceptions-are-warnings? prints each absorbed error. In an
    ;; evolutionary run that is thousands of lines of noise per generation,
    ;; and the errors are expected, so they go nowhere.
    (binding [*err* (java.io.StringWriter.)]
      (push/execute (setup input-program opts)
                    mutator-program
                    {:max-executions (or max-executions default-max-executions)}))
    (catch Throwable _
      ;; StackOverflowError is an Error, not an Exception, so the
      ;; interpreter's own handler does not see it. Evolved code recurses.
      nil)))

(defn variations
  "The variations of `input-program` produced by `mutator-program`.

  Every list left on the CODE stack counts as one variation, ordered top of
  stack first - a mutator that wants to nominate one candidate puts it on top.
  Atoms are discarded: a variation has to be a program.

  Opts:
    `seed`            seed for the mutator's randomness. Distinct seeds are
                      what make repeated calls with the same M and E differ;
                      one seed makes the whole call reproducible.
    `epigenetics`     signals merged into the genome's metadata before the
                      mutator runs, readable with :code_meta_get.
    `alphabet`        the target domain's instructions, which is what
                      :code_rand_atom draws from.
    `population`      other genomes, readable with :code_other - this is what
                      makes recombination possible at all.
    `graveyard`       genomes of individuals selected out, via :code_elder.
    `n-variations`    truncate to this many (from the top).
    `max-executions`  step budget, see [[default-max-executions]].

  Markers are not returned separately - they are on the returned trees."
  ([input-program mutator-program] (variations input-program mutator-program {}))
  ([input-program mutator-program {:as opts :keys [n-variations]}]
   (let [state (run input-program mutator-program opts)
         vs (into []
                  (comp (filter list?)
                        (if n-variations (take n-variations) identity))
                  (reverse (-> state :stacks :push/code)))]
     {:variations vs
      :steps (:push/num-executions state 0)})))

(defn mutate
  "The variations of `input-program` under `mutator-program`, as a vector.

  This is [[variations]] with the step count dropped - `mutate(E, M)` of
  ideas/metaevolution.org."
  ([input-program mutator-program] (mutate input-program mutator-program {}))
  ([input-program mutator-program opts]
   (:variations (variations input-program mutator-program opts))))

(comment
  ;; The null mutator clones - reproductive competence is free.
  (mutate '(1 2 3) '())
  ;; [(1 2 3)]

  ;; A mutator that drops one element, and one that doubles the genome.
  (mutate '(1 2 3 4 5) '(:mut_drop_rand_one) {:seed 7})
  (mutate '(1 2 3) '(:mut_duplicate))

  ;; Distinct seeds, distinct offspring - this is what the loop needs.
  (map #(mutate '(1 2 3 4 5 6 7 8) '(:mut_drop_rand_one) {:seed %})
       (range 4))

  ;; Third order: a mutator that reads a signal and reacts to it. Mutate hard
  ;; when the individual is starving, clone when it is doing fine.
  ;; (:exec_if takes the SECOND exec item when the condition is true.)
  (let [m '(:starving? :code_meta_get :exec_if (:code_noop) (:mut_duplicate))]
    [(mutate '(1 2 3) m {:epigenetics {:starving? true}})
     (mutate '(1 2 3) m {:epigenetics {:starving? false}})])

  ;; A bookkeeping mutator: tally the mutation on the genome, then mutate.
  ;; The counter is one entry in the metadata map, under :mut-count.
  (let [m '(:mut-count :code_meta_inc :mut_drop_rand_one)]
    (->> (iterate (fn [g] (first (mutate g m {:seed (count g)}))) '(1 2 3 4 5 6))
         (take 4)
         (mapv (juxt identity meta))))

  ;; A marker rides along with the subtree it is attached to, through moves
  ;; and through duplication - both copies get it.
  (let [gene (with-meta '(:integer_dup :integer_*) {:conserved true})]
    [(mapv meta (first (mutate (list 3 gene 7) '(1 :mut_rotate) {:seed 3})))
     (mapv meta (first (mutate (list 3 gene 7) '(:code_dup :code_append))))]))
