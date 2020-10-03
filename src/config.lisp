;;;; Game configuration

(in-package :config)

(defparameter *major-version* 0)
(defparameter *minor-version* 0)
(defparameter *patch-version* 0)


;;; Globals -------------------------------------------------------------------


(defvar *player*)
(defvar *bazaar*)
(defvar *player-bazaar-x*)
(defvar *player-bazaar-y*)


;;; Colours -------------------------------------------------------------------


(defparameter *normal-fg* #xFFFFFFFF)
(defparameter *normal-bg* #xFF000000)
(defparameter *sel-fg* #xFF000000)
(defparameter *sel-bg* #xFFFFFFFF)

(defparameter *label-fg* #xFFFFBF00)
(defparameter *bazaar-fg* #xFF87CEEB)
(defparameter *cyberspace-fg* #xFF66FF66)
