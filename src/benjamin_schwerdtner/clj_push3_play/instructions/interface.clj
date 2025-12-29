(ns benjamin-schwerdtner.clj-push3-play.instructions.interface
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :as impl]
   [benjamin-schwerdtner.clj-push3-play.instructions.boolean]
   [benjamin-schwerdtner.clj-push3-play.instructions.code]
   [benjamin-schwerdtner.clj-push3-play.instructions.numbers]
   [benjamin-schwerdtner.clj-push3-play.instructions.push-name]
   [benjamin-schwerdtner.clj-push3-play.instructions.mutation]
   [benjamin-schwerdtner.clj-push3-play.instructions.exec]
   [benjamin-schwerdtner.clj-push3-play.instructions.character]
   [benjamin-schwerdtner.clj-push3-play.instructions.string]))

(defn register-instruction [{:keys [sym-name in out f] :as opts}]
  (comment sym-name in out f)
  (impl/register-instruction opts))

(defn instruction [sym] (impl/instruction sym))

(defn execute-instruction [state instr]
  (impl/execute-instruction state instr))

(defn all-instructions [] (impl/all-instructions))
