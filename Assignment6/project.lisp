(setq objects '())                      ; creates objects for the game
(setq map '())                          ; creates a map for the game 
(setq object-locations '()) 
(setq location 'dungeon) ; the starting location is the dungeon
(setq allowed-commands '(look walk pickup inventory have run))

;;;;================================================
;;;; The describe-location function uses a location 
;;;; and a map to describe the location

(defun describe-location (location map)
  (second (assoc location map)))

;;;======================================================                     
;;; The describe-path function describes the paths going                      
;;; to and from a location                                                    
;;; e.g. (describe-path '(west door garden))                                   

(defun describe-path (path)
  `(there is a ,(second path) going ,(first path) from here -))

;;;================================================                           
;;; The describe-paths function describes the paths                           
;;; going to and from a given location in the map                             
;;; e.g. (describe-paths 'living-room map)                                     
(defun describe-paths (location map)
  (apply #'append (mapcar #'describe-path (cddr (assoc location map)))))

;;;============================================================
;;;The is-at function describes the location of a given object.
(defun is-at (obj loc obj-loc)
  (eq (second (assoc obj obj-loc)) loc))

;;;================================================================
;;; The describe-floor function describes the objects on the floor.
(defun describe-floor (loc objs obj-loc)
  (apply #'append (mapcar (lambda (x)
                            `(You see a ,x on the floor.))
                          (remove-if-not (lambda (x)
                                           (is-at x loc obj-loc))
                                         objs))))

;;;==========================================================
;;; The look function gives a full description of the current
;;; location
(defun look()
  (append (describe-location location map)
          (describe-paths location map)
          (describe-floor location objects object-locations)))

;;;==============================================================
;;; The walk-direction function allows movement from one location
;;; on the map to another.
(defun walk-direction (direction)
  (let ((next (assoc direction (cddr (assoc location map)))))
    (cond (next (setf location (third next)) (look))
          (t '(You cannot go that way.)))))

(defmacro defspel (&rest rest) 
  `(defmacro ,@rest))

;;;======================================================
;;; The walk SPEL allows movement from one location on the
;;; map to another.
(defspel walk (direction)
  `(walk-direction ',direction))

;;;======================================================
;;; The pickup-object function allows the user to pickup 
;;; an object in the current location.
(defun pickup-object (object)
  (cond ((is-at object location object-locations)
         (push (list object 'body) object-locations)
         `(You are now carrying the ,object))
         (t '(You cannot get that.))))

;;;=====================================================
;;; The pickup SPEL allows the user to pick up an object
;;; in the current location.
(defspel pickup (object)
  `(pickup-object ',object))

;;;=======================================================
;;; The inventory function displays the current inventory.
(defun inventory ()
  (remove-if-not (lambda (x)
                  (is-at x 'body object-locations))
                objects))

;;;=================================================
;;; The have function tells the user whether or not
;;; they have a certain object.
(defun have (object)
  (member object (inventory)))

;;;============================================================
;;; The game-action SPEL allows the user to do certain actions.
(defspel game-action (command subj obj place &rest rest)
  `(defspel ,command (subject object)
    `(cond ((and (eq ',subject ',',subj)
                 (eq ',object ',',obj) 
                 (eq location ',',place)
                 (have ',',subj))
             ,@',rest
             (pushnew ',command allowed-commands))    ;<--------------fix this
           (t '(I cannot ,',command like that.)))))
                 

;;;; The run function allows the user to run away and possibly escape
;;;; the dungeon. In order to escape successfully, the user must have 
;;;; a filled bottle, a made sack, and have one of the two food items.
;;;; If the user does not have everything, then the user will escape
;;;; but not survive.
(defun run ()
  (cond ((and (eq bottle-filled 't) ; checks that the bottle is filled
              (eq sack-made 't) ; checks that the sack is made
              (or (have 'fruits) (have 'turkey-dinner)) ; checks that there is at least one food item in the inventory
              (eq location 'outside)) ; checks that the user is outside
          '(Congratulations! You have escaped!)) ; message
        ((not (eq location 'outside)) ; location is not outside
          '(You cannot escape yet.)) ; cannot escape
        ((not (eq sack-made 't)) ; the sack has not been made
          '(You do not have anything to carry your items. You lost everything ; message
            while you were running away. You did not survive for very long.))
        (t '(You escaped but you did not last long. You need to carry food ; message
             and water to survive you know.))))





(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-eval (sexp)
    (if (member (car sexp) allowed-commands)
        (eval sexp)
        '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))







;;; Macro that easily adds new objects
(defmacro new-object(obj loc)
  `(cond
      ;; Error Checking: Does the object exist?
      ((member ',obj objects)
        '(The object ,obj already exists!))
      ;; Error Checking: Does the location exist?
      ((not(assoc ',loc map))
        '(The location ,loc does not exist!))
      ;; If both object does not exist and location exists, add new object
      (t
        (pushnew ',obj objects)
        (pushnew (list ',obj ',loc) object-locations)) ) )

; help
;; Macro that easily adds new locations
(defmacro new-location(loc &rest descr)
  `(cond
      ;; Error Checking: Does the location exist?
      ((assoc ',loc map)
        '(The location ,loc already exist!))
      ;; If location does not yet exists, add new location
      (t
        (pushnew (list ',loc '(,@descr)) map)) ) )




        ;((nconc (cdr (assoc ',loc map)) (list (list ',@descr))))) ) )

; ;;; Macro that easily adds new paths 
; (defmacro new-path(loc1 loc2 dirc1 path &optional (dirc2 nil))
;   `(cond
;     ;; Error Checking: Does the location1 exist?
;     ((not(assoc ',loc1 locations))
;       '(The location ,loc1 does not exist!))
;     ;; Error Checking: Does the location2 exist?
;     ((not(assoc ',loc2 locations))
;       '(The location ,loc2 does not exist!))
;     ;; Error Checking: Does the path between loc1 -> loc2 exist?
;     ((assoc ',loc2 (cdr (assoc ',loc1 paths)))
;       '(The path ,loc1 -> ,loc2 already exist!))
;     ;; Error Checking: Does the path between loc2 -> loc1 exist and it goes 2-ways?
;     ( (and ',dirc2 (assoc ',loc1 (cdr (assoc ',loc2 *edges*))) )
;       '(The path ,loc2 -> ,loc1 already exist!)) 
;     ;; There exist path(s) so create them
;     (t
;       ;; No matter what loc1 -> loc2 will need to be created
;       (if (assoc ',loc1 *edges*)                                                                ; Does loc1 have an outgoing edge?
;         (pushnew (list ',loc2 ',dirc1 ',path) (cdr (assoc ',loc1 *edges*)))                     ; If so, just add another outgoing edge
;         (pushnew (list ',loc1 (list ',loc2 ',dirc1 ',path)) *edges*))                           ; If not, add it to the edge list
;       ;; If it goes bothways, create a second path, loc2 -> loc1
;       (if ',dirc2
;         (if (assoc ',loc2 *edges*)                                                              ; Does loc2 have an outgoing edge?
;           (pushnew (list ',loc1 ',dirc2 ',path) (cdr (assoc ',loc2 *edges*)))                   ; If so, just add another outgoing edge
;           (pushnew (list ',loc2 (list ',loc1 ',dirc2 ',path)) *edges*)))) ) )                   ; If not, add it to the edge list


(load "add_actions.lisp")
(load "add_objects.lisp")
(load "add_locations.lisp")
;(game-repl)