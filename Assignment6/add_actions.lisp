(setq sack-made nil)                                                    ; sets the sack to not made

(new-action 'combine 'cloth 'stick 'cave)

(game-action combine cloth stick cave                                   ; conditions for the sack to be made
  (cond ((and (have 'stick)                                             ; checks that the stick is in the inventory
              (setq sack-made 't)                                       ; sets the sack to made
              (push (list 'cloth 'gone) object-locations)
              (push (list 'stick 'gone) object-locations))              ; gets rid of material used
          '(You now have a sack. You can use it to carry items.))       ; returns a message
          (t '(You cannot combine those.))))                            ; error message

(setq bottle-filled nil)                                                ; sets the bottle to empty

(new-action 'fill-up 'bottle 'pond 'garden) 

(game-action fill-up bottle pond garden                                 ; conditions to fill up the bottle
  (cond ((and (have 'bottle)                                            ; checks that the bottle is in the inventory
              (setq bottle-filled 't))                                  ; sets the bottle to filled
         '(You now have a bottle full of water.))                       ; returns a message
        (t '(You cannot fill like that.))))                             ; error message

(new-action 'dig 'shovel 'hole 'cave) 

(game-action dig shovel hole cave                                       ; conditions to dig
  (cond ((and (have 'shovel)                                            ; checks that the shovel is in the inventory
          (setq location 'kitchen))                                     ; sets the location to the kitchen
         (append (describe-location location map)                       ;; describes the location
                 (describe-paths location map)
                 (describe-floor location objects object-locations)))
        (t '(You cannot dig like that.))))                              ; error message

(new-action 'light 'lighter 'torch 'dungeon)

(game-action light lighter torch dungeon                                ; conditions to light touch
  (cond ((have 'lighter)                                                ; checks that the lighter is in the inventory 
          '(You lit up the torch. A guard saw a light down in the dungeon
            and gave you a warning. He then extinguished the torch.))
        (t '(You cannot light that.))))                                 ; error message

;(new-action 'eat)

;(game-action eat food nil nil)