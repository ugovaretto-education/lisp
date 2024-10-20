(defun add-node-from-data (name question yes-case no-case)
  (add-node (make-node :name name
                       :question question
                       :yes-case yes-case
                       :no-case no-case)))

(defun add-nodes ()
  (progn
    (add-node-from-data 'start
              "Does the engine turn over?"
              'engine-turns-over
              'engine-wont-turn-over)
    (add-node-from-data 'engine-turns-over
              "Will the engine run for any period of time?"
              'engine-will-run-briefly
              'engine-wont-run)
    (add-node-from-data 'engine-wont-run
              "Is there gas in the tank?"
              'gas-in-tank
              "Fill the tank and try starting the engine again.")
    (add-node-from-data 'engine-wont-turn-over
              "Do you hear any sound when you turn the key?"
              'sound-when-turn-key
              'no-sound-when-turn-key)
    (add-node-from-data 'no-sound-when-turn-key
              "Is the battery voltage low?"
              "Replace the battery"
              'battery-voltage-ok)
    (add-node-from-data 'battery-voltage-ok
              "Are the battery cables dirty or loose?"
              "Clean the cables and tighten the connections."
              'battery-cables-good))
    nil)
