#!/usr/bin/bb


(ns xrandr
  (:require [clojure.string :as str]
            [clojure.java.shell :as shell]
            [clojure.tools.cli :as tools.cli]))

;;notify-send support
(defn notify-send [summary body]
  (shell/sh "notify-send" summary body))
(comment
  (notify-send "hello" "there"))
(notify-send "testing" "notify-send")
(def screens (filter #(.contains % " connected" ) (-> (shell/sh "xrandr" "-q") :out (str/split #"\n"))))
(def connected-screen-ids (into (sorted-set) (for [screen screens] (first (str/split screen #" ")) )))
(notify-send "testing" "notify-send m0")
(comment
  (prn screens) 
  (prn connected-screen-ids),
         )
(def directions ["left" "right" "above" "below"])

(defn do-it [decision]
  (let [
      prompt (:prompt decision)
      options (keys (:options decision))
      result (shell/sh "dmenu" "-p" prompt :in (str/join "\n" options ))
      choice (-> result :out str/trim)
      next (get-in decision [:options choice] )]
    (next choice)))

(comment
  (do-it {:prompt "Hello"
          :options {"This" #(prn %) "That" #(prn %)}}) 

  ,)




(defn prompts [state] (map :prompt (:choices state)))

(defonce handler-registry (atom {}))
(defn register-handler [key handler]
  (reset! handler-registry (assoc @handler-registry key handler) ))
(defn despatch [key & args] [key args])

(defn res-handler [key]
  (let [resolved (get @handler-registry key)]
    (when (nil? resolved)
      (prn "could not resolve handler for" key "from" (keys @handler-registry)))
    resolved))

(defn do-choices [state]
  (loop [state state]
    (prn "*** top loop got outouts" (:outputs state) )
    (if (empty? (:choices state))
      state
      (let [choices (:choices state)
            decision (first choices)]
        (prn "choice q" (map :prompt choices))
        (prn "got handler-key: " (:on-selection decision))
        (prn "got decision keys" (keys decision))
        (prn "got options-fn: " (:options decision))
        (let  [prompt (:prompt decision)
               options-fn (:options decision)
               options (options-fn state)
               z (prn "calcced options" options)
               result (shell/sh "dmenu" "-p" prompt :in (str/join "\n" options ))
               x (prn "back from dmenu")
               selection (-> result :out str/trim)
               y (prn "got selection" selection)
               despatched (:on-selection decision)
               y (prn "got despatched" despatched)
               handler-key (first despatched)
               handler (res-handler handler-key)
               trimmed-state (assoc state :choices (rest choices))
               new-state (apply handler (concat [selection trimmed-state] (second despatched)))
               ]
          (prn "about to recur")
          (recur new-state)
          ;;(recur ((get @handler-registry (:on-selection decision)) selection  (assoc state :choices (rest choices))) 
        )
        ))
    )
  )

;;Positioning
(defn free-adjacents [s] (let [primary-output (:primary-output s)](-> s :outputs (get primary-output)  :free-adjacents)))

(comment
  (free-adjacents {:primary-output "b" :outputs {"a" {} "b" {:free-adjacents ["left" "right"] } "c" {}} }))

(defn position-selected-handler [selection state screen-id]
  ;;(fn [selection state]
    (prn "posision selected" selection " for " screen-id)
    (let [display (get-in state [:outputs screen-id])]
      (-> state 
          (assoc-in [:outputs screen-id] (assoc display :position selection))
          (assoc-in [:outputs (:primary-output state) :free-adjacents] (disj (free-adjacents state) selection ) )))
  ;;)
  )
(register-handler :position-selected position-selected-handler)


(defn direction-rel-primary-choice [screen-id]
  {
   :prompt (str "Where to position " screen-id " relative to primary?")
   :options free-adjacents
   :on-selection (despatch :position-selected screen-id)})


(comment
  (direction-rel-primary-choice "test") 
  )




(defn pick-primary [s]
  (-> s :outputs (get (:primary-output s))))







;; Choose Others
(defn existing-screens [s]
  (prn "existing screens called with" s)
  (->> s :outputs keys))
(comment
  (existing-screens {:outputs {"this" {}}})
  )

(defn selectable-screens [s]
  (let [existing (existing-screens s)]
    (filter #(not (contains? (set existing) %)) connected-screen-ids)))

(def connected-screen-choice
  {:prompt "Choose additional screen"
   :options selectable-screens
   :on-selection (despatch :screen-selected)})

(register-handler :another-screen(fn [selection state]
                                   (if (= "yes" selection)
                                     (assoc-in state [:choices] (conj (:choices state) connected-screen-choice))
                                     state)))

(def add-another-screen-choice
  {:prompt "Add another screen"
   :options (fn [s] ["yes" "no"])
   :on-selection (despatch :another-screen)})



(defn screen-record [screen-id is-primary]
  (let [screen {:screen-id screen-id }]
    (if is-primary
      (assoc screen :free-adjacents (apply sorted-set directions))
      screen)))

(defn screen-selected-handler [selection state ]
  (prn "screen-selected-handler called with " selection)
  (let [choices (:choices state)
        outputs (:outputs state )
        is-primary false ;;(not (some? (:displays state)))
        additional-choices (concat
                            (if is-primary
                              []
                              [(direction-rel-primary-choice selection)]) 
                            [add-another-screen-choice])
        out-choices (concat additional-choices )]
    (prn "is-primary" is-primary)
    (prn "out choices:" (count out-choices) out-choices)
    (prn "outputs" outputs)
    (-> state
        (assoc-in [:outputs selection] (screen-record selection is-primary )) 
        (assoc-in [:choices] additional-choices)
        )))

(register-handler :screen-selected screen-selected-handler)





;; Choose Primary


(defn primary-selected-handler [selection state ]
  (prn "primary-selected-handler called with " selection)
  (let [choices (:choices state)
        outputs (:outputs state )
        additional-choices (concat [] [add-another-screen-choice])]
    (-> state
        (assoc-in [:outputs selection]  (screen-record selection true )) 
        (assoc :primary-output selection)  
        (assoc :primary-adjacencies directions)
        (assoc-in [:choices] additional-choices)
        )))
(register-handler :primary-selected primary-selected-handler)

;;(defn primary-output [s]
;;  (:primary-output s))



(defn print-selection-handler [selection state] (prn selection))

(register-handler :print-selection print-selection-handler)









(comment
  (screen-selected-handler "myscreen" {}) 

  (prompts
   (screen-selected-handler "my-new-screen"  
                            {:displays [{:screen-id "myscreen"}]}) 
   )
  ,)




;;xrandr --output "$primary" --auto --scale 1.0x1.0 --output "$secondary" --"$secondary_direction_param" "$primary" --auto --scale 1.0x1.0
(defn scale
  ([] (scale 1.0 1.0))
  ([x y] (str "--scale " x "x" y)))


(defn output-group
  ([id positioning]
   (str/join " " (filter #(not (nil? %))["--output" id positioning "--auto" (scale) ])))

  ([id direction reference-output]
   (let [positioning (str direction (if (contains? #{"left" "right"} direction) "-of" ""))]
     (output-group id (str "--" positioning " " reference-output))))

  ([primary-output]
   (output-group primary-output nil)))

(comment
         (output-group "homer" ) 
         (output-group "marge" "right" "homer") 
         (output-group "bart" "below" "homer") 
         (output-group "fred" "dagg" "dog")
         ,)


(defn format-xrandr [{:keys [:outputs :primary-output]}],
  (let [primary (output-group primary-output)
        extensions (flatten (into [] (for [[id {:keys [:position]}] (dissoc outputs primary-output)]
                                      (output-group id position primary-output))))]
    (str/join " " (concat ["xrandr" "--verbose"] [primary]  extensions )))

  )

(comment
  (prn
   (format-xrandr 
    {:outputs {"eDP-1" {:screen-id "eDP-1", :free-adjacents #{"left" "right"}}, "HDMI-1" {:screen-id "HDMI-1", :position "below"}, "fred" {:screen-id "fred", :position "above"}}, :primary-output "eDP-1", :primary-adjacencies ["left" "right" "above" "below"]}) 
   ) 
,) 

(def config (do-choices {:choices [{:prompt "Which screen is primary?"
                                    :options (fn[s] connected-screen-ids)
                                    :on-selection (despatch :primary-selected)}] }))
(prn "got config" config)
(def command (format-xrandr config))
(prn "got command" command)
(def xrandr-result
  (apply shell/sh (vec (str/split command #" "))) 
  )
(prn "xrandr-result" xrandr-result)
(notify-send "xrandr result" (str (:out xrandr-result))) 
