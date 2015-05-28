;; smacklisp code for a very agressive brain
(defun something-ahead-p (pred)
  (find-if (lambda (sq)
             (member-if pred sq))
           (vision-ahead)))

(defun feeb-ahead-p ()
  (something-ahead-p #'feeb-image-p))

(defun fireball-ahead-p ()
  (something-ahead-p #'fireball-image-p))

(defun mushroom-p (sq)
  (member :mushroom sq))

(defun mushroom-ahead-p ()
  (something-ahead-p #'mushroom-p))

(defun food-p (thing)
  (or (eq :mushroom thing)
      (eq :carcass thing)))

(defun food-ahead-p ()
  (something-ahead-p #'food-p))

(defun last-move-turn-p ()
  (member (feeb-last-move) '(:turn-right :turn-left :turn-around)))


(defun can-fire-p (min-energy)
  (and (ready-to-fire-p)
       (> (- (feeb-energy)
             (get-parm :flame-energy))
          min-energy)
       (feeb-ahead-p)))

(defun check-for-food-and-eat ()
  (case (and (< (feeb-energy)
                  (get-parm :maximum-energy))
               (find-if #'food-p (current-square)))
    (:mushroom :eat-mushroom)
    (:carcass :eat-carcass)))

(defun can-turn-p (turn)
  (let ((square (case turn
                  (:turn-right (right-square))
                  (:turn-left (left-square))
                  (:turn-around (rear-square)))))
    (and (not (member :rock square))
         (not (find-if #'fireball-image-p square))
         turn)))

(defun can-peek-p (side)
  (let ((vision (case side
                  (:peek-left (vision-left))
                  (:peek-right (vision-right)))))
    (and (> (length vision) 0)
         (not (aref vision 0))
         side)))

(defvar *peek-mode* nil)

(defun vision-feeb-heading ()
  (mod (+ (feeb-heading)
          (case (feeb-peeking)
            (:left 3)
            (:right 1)
            (otherwise 0)))
       4))

(defun looking-at-me-p  (image)
  (= (mod (+ 2 (vision-feeb-heading)) 4)
     (feeb-image-heading image)))

(defun peek-for-victims ()
  (if *peek-mode*
      (if (feeb-peeking)
          (let ((feeb-image (feeb-ahead-p)))
            (if feeb-image
                (if (looking-at-me-p feeb-image)
                    *peek-mode*   ;; peek again.
                    :move-forward ;;git 'em rambo
                    )
                (setf *peek-mode* nil)))
          (progn
            (if (eq *peek-mode* :peek-left)
               :turn-left
               :turn-right)
            (setf *peek-mode* nil)))
      (let ((left (can-peek-p :peek-left))
            (right (can-peek-p :peek-right)))
        (setf *peek-mode*
              (if (and left right)
                  (if (eq 0 (random 2))
                      left
                      right)
                  (or left right))))))


(defun search-for-victims ()
  (let ((here (current-square))
        (leftp (can-turn-p :turn-left))
        (rightp (can-turn-p :turn-right))
        (reversep (can-turn-p :turn-around))
        (dangerp (fireball-ahead-p)))
    (cond ((and (not (last-move-turn-p))
                (or leftp rightp)
                (= 0 (random 2)))
           (if (and leftp (or (not rightp)
                              (= 0 (random 2))))
               :turn-left
               :turn-right)) 
          ((and (> (length (vision-ahead)) 0)
                (not dangerp))
           :move-forward)
          ((and reversep dangerp)
           :turn-around)
          ((or leftp rightp)
           (if (and leftp (or (not rightp)
                              (= 0 (random 2))))
               :turn-left
               :turn-right))
          (reversep
           :turn-around)
          (t :wait))))


(defun brain ()
  (cond ((can-fire-p 10)
         :flame)
        ((check-for-food-and-eat))
        ((peek-for-victims))
        ((search-for-victims))))
