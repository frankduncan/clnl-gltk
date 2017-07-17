(in-package #:clnl-gltk)

(defun setup ()
 "SETUP => RESULT

ARGUMENTS AND VALUES:

  RESULT: undefined

DESCRIPTION:

  SETUP sets up the clnl-gltk system, calling all the necessary underlying
  functions.

  Calling before the opengl system has been initialized properly may or may
  not work, so calling it after everything else has been initialized is
  recommended."
 (setup-font)
 (setup-button))
