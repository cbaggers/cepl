(in-package :cepl.documentation-functions)

;; These functions are not useful for general day-to-day coding.
;; They are used in place of gl functions in blending & stenciling to allow
;; us to attach documentation to them, to indicate that they have functionality
;; and to allow users to read their the code to see roughly what they equate to

(defn-inline never ((incoming-val number) (stored-val number)) boolean
  "Never passes"
  (declare (ignore incoming-val stored-val))
  nil)

(defn-inline always ((incoming-val number) (stored-val number)) boolean
  "Always passes"
  (declare (ignore incoming-val stored-val))
  t)

(defn-inline keep ((incoming-val number) (stored-val number)) number
  "Always returns the stored-val"
  (declare (ignore incoming-val))
  stored-val)

(defn-inline one ((incoming-val number) (stored-val number)) number
  "Always returns 0"
  (declare (ignore incoming-val stored-val))
  0)

(defn-inline zero ((incoming-val number) (stored-val number)) number
  "Always returns 0"
  (declare (ignore incoming-val stored-val))
  0)

(defn-inline stencil-replace ((incoming-val number) (stored-val number)) number
  "Invert the stored value

   We only show the implementation for 8bit stencil here due to strong
   recommendations from guides to use 8bit stencils. Not an issue
   as this function only exists as documentation anyway"
  (declare (ignore stored-val))
  incoming-val)

(defn-inline stencil-invert ((incoming-val number) (stored-val number)) number
  "Invert the stored value

   We only show the implementation for 8bit stencil here due to strong
   recommendations from guides to use 8bit stencils. Not an issue
   as this function only exists as documentation anyway"
  (declare (ignore incoming-val))
  (- 255 stored-val))

(defn-inline stencil-incf ((incoming-val number) (stored-val number)) number
  "Increment the stored value clamping to maximum

   We only show the implementation for 8bit stencil here due to strong
   recommendations from guides to use 8bit stencils. Not an issue
   as this is only a stand-in function anyway"
  (declare (ignore incoming-val))
  (min (1+ stored-val) 255))

(defn-inline stencil-decf ((incoming-val number) (stored-val number)) number
  "Decrement the stored value clamping to 0

   We only show the implementation for 8bit stencil here due to strong
   recommendations from guides to use 8bit stencils. Not an issue
   as this is only a stand-in function anyway"
  (declare (ignore incoming-val))
  (max 0 (1- stored-val)))

(defn-inline stencil-incf-wrap ((incoming-val number) (stored-val number)) number
  "Increment the stored value wrapping if it overflows

   We only show the implementation for 8bit stencil here due to strong
   recommendations from guides to use 8bit stencils. Not an issue
   as this is only a stand-in function anyway"
  (declare (ignore incoming-val))
  (mod (1+ stored-val) 256))

(defn-inline stencil-decf-wrap ((incoming-val number) (stored-val number)) number
  "Decrement the stored value wrapping if underflows

   We only show the implementation for 8bit stencil here due to strong
   recommendations from guides to use 8bit stencils. Not an issue
   as this is only a stand-in function anyway"
  (declare (ignore incoming-val))
  (mod (1- stored-val) 256))
