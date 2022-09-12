(defun c:pu1 ()
	(VL-CMDF "_.-purge" "_a" "" "_n")
	(VL-CMDF "_.-purge" "_r" "" "_n")
)


(defun pl () (VL-CMDF "._pline"))
(defun C:pl()(pl))


;(setq S::STARTUP (append S::STARTUP zzzod))
;(defun-q zzzod ()(load "Q:/Работа/Полезное для работы/AVTOCAD'z modzzz/Zod.lsp"))

(defun c:ename () (vlax-ename->vla-object (car(entsel))))

(defun c:au () (VL-CMDF "._Audit" "_Y"))

(defun c:XH () (VL-CMDF "._XLINE" "_H"))

(defun c:XV () (VL-CMDF "._XLINE" "_V"))

(defun c:PE () (VL-CMDF "._PEDIT" "_Multiple" ))

(defun c:vv1 () (VL-CMDF "._-VPORTS" "_Single" ))
(defun c:vv2 () (VL-CMDF "._-VPORTS" "_2" "_Vertical"))

(defun c:wi () (VL-CMDF "._wipeout"))

(defun c:FR () (VL-CMDF "._LAYFRZ"))


(defun c:wi1 ()(VL-CMDF "._wipeout" "_frames" "_on")(princ "\nРамки включены"))

(defun c:wi2 ()(VL-CMDF "._wipeout" "_frames" "_off")(princ "\nРамки выключены"))

(defun c:wi3 ()(vl-cmdf "._wipeout" "_frames" "_D")(princ "\nРамки видимы но не печатаются"))

(defun c:bln () (vla-get-EffectiveName (vlax-ename->vla-object (car(entsel)))))

;(defun c:b () (VL-CMDF "._-BOUNDARY" "_A" "_B" "_N"))

(defun c:z1 () (setvar "osmode" 15359)(princ "\n Включены все привязки")(princ))
(defun c:z2 () (setvar "osmode" 14847)(princ "\n Включены все привязки,кроме Nearest")(princ))
(defun c:z3 () (setvar "osmode" 1)(princ "\n Включена только привязка Endpoint ")(princ))

(defun c:ee ( / blk)
(setq blk(vlax-ename->vla-object (car(entsel))))
(princ (strcat "\nget-name............" (vla-get-name blk) "\n"))
(princ (strcat "get-EffectiveName..." (vla-get-EffectiveName blk)"\n"))
(princ)
)





;;---------------------=={ Dump Object }==--------------------;;
;;                                                            ;;
;;  Lists the properties & methods of a supplied VLA-Object   ;;
;;  or VLA-Object equivalent of a supplied ename or DXF list. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  obj - VLA-Object, Entity Name, or Entity DXF List         ;;
;;------------------------------------------------------------;;

(defun c:dump  	nil (LM:dump (car (entsel))		nil))
(defun c:dumpt  nil (LM:dump (car (entsel))		t)	)
(defun c:dumpn 	nil (LM:dump (car (nentsel))	nil))
(defun c:dumptn nil (LM:dump (car (nentsel))	t)	)

(defun LM:dump ( obj flag)
    (cond
        (   (or (= 'ename (type obj))
                (and (listp obj) (= 'ename (type (setq obj (cdr (assoc -1 obj))))))
            )
            (vlax-dump-object (vlax-ename->vla-object obj) flag)
        )
        (   (= 'vla-object (type obj))
            (vlax-dump-object obj flag)
        )
    )
    (princ)
)
(vl-load-com) (princ)


;;---------------------=={ Entity List }==--------------------;;
;;                                                            ;;
;;  Displays the DXF Information for an entity, a variation   ;;
;;  of the program by Michael Puckett of the same name.       ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac - www.lee-mac.com                         ;;
;;------------------------------------------------------------;;
;;  Arguments: entity - entity (ename) / DXF list             ;;
;;------------------------------------------------------------;;

(defun elist ( entity / prin1x princx data xdata )

    (defun prin1x ( x i ) (repeat i (princ "  ")) (prin1 x))
    (defun princx ( x i ) (repeat i (princ "  ")) (princ x))

    (cond
        (   (or
                (and
                    (eq 'ENAME (type entity))
                    (setq data (entget entity))
                )
                (and
                    (listp entity)
                    (setq data entity)
                    (setq entity (cdr (assoc -1 data)))
                )
            )
            (princ "\n\n  (\n")
            (foreach pair data
                (prin1x pair 2)
                (princ "\n")
            )
            (if (setq xdata (assoc -3 (entget entity '("*"))))
                (progn
                    (princx "(" 2)
                    (prin1 (car xdata))
                    (princ "\n")
                    (foreach app (cdr xdata)
                        (princx "(" 3)
                        (princ "\n")
                        (foreach pair app (prin1x pair 4) (princ "\n"))
                        (princx ")" 3)
                        (princ "\n")
                    )
                    (princx ")" 2)
                    (princ "\n")
                )
            )
            (princ "  )")
            (if (= 1 (cdr (assoc 66 data)))
                (while
                    (progn
                        (elist (setq entity (entnext entity)))
                        (not (eq "SEQEND" (cdr (assoc 0 (entget entity)))))
                    )
                )
            )
        )
        (   (print entity)   )
    )
    (princ)
)

(defun c:ent  nil (elist (car (entsel))))
(defun c:nent nil (elist (car (nentsel))))


(defun SS1 (lst)(sssetfirst nil (ssget "_X"		(list lst)))(princ))
(defun SS2 (lst)(sssetfirst nil (ssget		 	(list lst)))(princ))


(defun c:pu5 	()	(_kpblc-autostart-purge 192)	)
(defun c:pu6 	()	(_kpblc-autostart-purge 2007)	)
(defun pu99 	()	(_kpblc-autostart-purge 2047)	)
(defun c:pu99 	()	(_kpblc-autostart-purge 2047)	)


(defun break_point 	( flag01 / ent01 pt01 pt02)
	(setq 	ent01 	(car (entsel "select a line"))
			pt01 	(getpoint "select a point1"))
	(if		(equal 1 flag01)
			(setq pt02 	(getpoint "select a point2"))
	)
	(if		(equal 1 flag01)
			(VL-CMDF "_BREAK"  	ent01 	pt01 	pt02	)
			(VL-CMDF "_BREAK"	ent01	pt01	"@"		)
	)
)


(defun c:br1 	() (break_point 1))
(defun c:br2 	() (break_point 0))



(defun lst_to_stroke ( lst01 / )
(foreach pair lst01 (princ pair)(princ "\n"))
)