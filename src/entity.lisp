;;;; Entity functionality

(in-package :cprl)

(defstruct attr
  "Attributes of an entity"
  (con 0 :type integer)  ;; Constitution determines total hit-points
  (bra 0 :type integer)  ;; Brawn determines armour carrying capacity
  (str 0 :type integer)  ;; Strength determines melee weapon ability
  (dex 0 :type integer)  ;; Dexterity determines ranged weapon ability
  (ten 0 :type integer)  ;; Tenacity determines contact-based psychic ability
  (pot 0 :type integer)  ;; Potentiality determines ranged telekinetic ability
  (ada 0 :type integer)  ;; Adaptability determines cybernetic modification capacity
  (att 0 :type integer)  ;; Attunement determines modification effectiveness
  (pen 0 :type integer)  ;; Penetration determines destructive hacking ability
  (sec 0 :type integer)) ;; Security determines defensive anti-hacking ability

(defstruct (entity (:include attr))
  "A game entity"
  (hp 0.0 :type float)         ;; Hit points, i.e health
  (x 1 :type integer)          ;; x-coordinate of entity in its given location
  (y 1 :type integer)          ;; y-coordinate of entity in its given location
  (kind 'entity :type symbol)) ;; The type of entity (how it will be displayed)
