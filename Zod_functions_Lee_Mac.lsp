;;; ************************************************************************
;;; * Библиотека DWGruLispLib Copyright ©2008  DWGru Programmers Group
;;; *
;;; * loc:dwgru-get-user-dcl (Кандидат)
;;; *
;;; * Запрос значения у пользователя через диалоговое окно
;;; *
;;; *
;;; * 21/01/2008 Версия 0001. Редакция Владимир Азарко (VVA)
;;; ************************************************************************
 
  (defun loc:dwgru-get-user-dcl
         (zagl info-list multi / fl ret dcl_id msg layouts)
         ;|
* Запрос значения у пользователя через диалоговое окно
* Диалог формируется "налету"
* Параметры вызова:
    zagl - заголовок окна [String]
    info-list - список строковых значений[List of String]
    multi - t - разрешен множественный выбор, nil- нет
 
* Возвращает:
 Список выбранных строк или nil - отмена
* Пример
 (loc:dwgru-get-user-dcl "Укажите вариант" '("Первый" "Второй" "Третий") nil) ->("Первый") 
 (loc:dwgru-get-user-dcl "Укажите вариант" '("Первый" "Второй" "Третий") t) ->("Первый" "Второй") 
|;
    (if (null zagl)
      (setq zagl "Выбор")
    ) ;_ end if
    (setq fl (vl-filename-mktemp "dwgru" nil ".dcl"))
    (setq ret (open fl "w"))
    (mapcar
      '(lambda (x) (write-line x ret))
      (list "dwgru_msg : dialog { "
            (strcat "label=\"" zagl "\";")
            " :list_box {"
            "alignment=top ;"
            (if multi
              "multiple_select = true ;"
              "multiple_select = false ;"
            ) ;_ end of if
            "width=31 ;"
            (if (> (length info-list) 26)
              "height= 26 ;"
              (strcat "height= " (itoa (+ 3 (length info-list))) ";")
            ) ;_ end of if
            "is_tab_stop = false ;"
            "key = \"info\";}"
            "ok_cancel;}"
      ) ;_ end of list
    ) ;_ end of mapcar
    (setq ret (close ret))
    (if (and (null (minusp (setq dcl_id (load_dialog fl))))
             (new_dialog "dwgru_msg" dcl_id)
        ) ;_ end and
      (progn (start_list "info")
             (mapcar 'add_list info-list)
             (end_list)
             (set_tile "info" "0")
             (setq ret "0")
             (action_tile "info" "(setq ret $value)")
             (action_tile "cancel" "(done_dialog 0)")
             (action_tile "accept" " (done_dialog 1)")
             (if (zerop (start_dialog))
               (setq ret nil)
               (setq
                 ret (mapcar (function (lambda (num) (nth num info-list)))
                             (read (strcat "(" ret ")"))
                     ) ;_ end mapcar
               ) ;_ end setq
             ) ;_ end if
             (unload_dialog dcl_id)
      ) ;_ end of progn
    ) ;_ end of if
    (vl-file-delete fl)
    ret
  ) ;_ end of defun



  
  

;функция mip_MTEXT_Unformat взята http://kpblc.blogspot.com/2007_06_01_archive.html
(defun mip_mtext_unformat2 (mtext / text str)
  (setq text "")
  (while (/= mtext "")
    (cond
      ((wcmatch (strcase (setq str (substr mtext 1 2))) "\\[\\{}]")
       (setq mtext (substr mtext 3)
             text  (strcat text str)
             ) ;_ end of setq
       )
      ((wcmatch (substr mtext 1 1) "[{}]") (setq mtext (substr mtext 2)))
      ((wcmatch (strcase (setq str (substr mtext 1 2))) "\\[LO`~]")
       (setq mtext (substr mtext 3))
       )
      ((wcmatch (strcase (substr mtext 1 8)) "\\FSYMBOL")
;;;Add VVA remove Symbol
       (setq mtext (substr mtext (+ 2 (vl-string-search "}" mtext))))
       )
      ((wcmatch (strcase (substr mtext 1 2)) "\\[ACFHQTW]")
       (setq mtext (substr mtext (+ 2 (vl-string-search ";" mtext))))
       )
      ((wcmatch (substr mtext 1 3) "\\p[qxicrjd]")
;;;Add and changed by kpblc
       (setq mtext (substr mtext (+ 2 (vl-string-search ";" mtext))))
       )
      ((wcmatch (strcase (substr mtext 1 2)) "\\S")
       (setq str   (substr mtext 3 (- (vl-string-search ";" mtext) 2))
             text  (strcat text (vl-string-translate "#^\\" "/^\\" str))
             mtext (substr mtext (+ 4 (strlen str)))
             ) ;_ end of setq
       )
      (t
       (setq text  (strcat text (substr mtext 1 1))
             mtext (substr mtext 2)
             ) ;_ end of setq
       )
      ) ;_ end of cond
    ) ;_ end of while
  text
  ) ;_ end of defun


  
(defun sort_and_sum_list (lst / ret tmp)
  (vl-load-com)
  (foreach item
           (mapcar
             '(lambda (x)
                (setq x (reverse x))
                (list  (vl-prin1-to-string (cdr x)) (car x))
              ) ;_ end of lambda
             lst
           ) ;_ end of mapcar
    (if (setq tmp (assoc (car item) ret))
      (setq ret
             (subst (list (car tmp) (+ (cadr tmp) (cadr item))) tmp ret)
      ) ;_ end of setq
      (setq ret (cons item ret))
    ) ;_ end of if
  ) ;_ end of foreach
  (mapcar '(lambda (x) (reverse (cons (cadr x) (read (car x)))))
          ret
  ) ;_ end of mapcar
) ;_ end of defun

 

  

;Перевод числа из 10-й системы счисления в шестнадцатиричную строку
(defun DecToHex (Dec / Hex Value HexChar)

(setq Hex "")
(while (not(equal dec 0))
	(setq 	Value 	(rem Dec 16)
		Dec 	(/ Dec 16)
		HexChar (if 	(< Value 10)
				(itoa Value)
				(cadr (assoc Value '((10 "A")(11 "B")(12 "C")(13 "D") (14 "E")(15 "F"))))
						)
		Hex 	(strcat HexChar Hex)
	)
)

Hex
);_end_of_defun
;Перевод числа из 16-й системы счисления в десятичную
(defun HexToDec (Hex / Dec Koef HexChar Num Value)
(setq 	Dec 	0
      	Koef 	1
       	Num 	(strlen Hex)
)

(while (not(equal Num 0))
	(setq 	HexChar (substr Hex Num 1)
		Value 	(if 	(wcmatch HexChar "#")
         			(atoi HexChar)
         			(cadr (assoc HexChar '(("A" 10)("B" 11)("C" 12)("D" 13) ("E" 14)("F" 15))))
          		)
    		Dec 	(+ Dec (* Value Koef))
    		Koef 	(* Koef 16)
    		Num 	(1- Num)
   	)
)
Dec
);_end_of_defun



;  ! ***************************************************************************
;; !                           copyToclipboard
;; ! ***************************************************************************
;; ! Function : Copy text to clipboard
;; ! Argument : 'str'     - String
;; ! Returns  : nil
;; see http://www.theswamp.org/index.php?topic=21764.new
;; ! ****************************************************************************

(defun copyToclipboard ( text / htmlfile result)
 (setq result
        (vlax-invoke
            (vlax-get
                (vlax-get
                    (setq htmlfile (vlax-create-object "htmlfile"))
                   'ParentWindow
                )
               'ClipBoardData
            )
           'SetData
            "Text"
            text
        )
    )

    (vlax-release-object htmlfile)
    result
)



;  ! ***************************************************************************
;; !                           Getclipboard
;; ! ***************************************************************************
;; ! Function : Return text string from clipboard
;; ! Argument : nil
;; ! Returns  : string
;; see http://www.theswamp.org/index.php?topic=21764.new
;; ! ****************************************************************************

(defun Getclipboard ( / htmlfile result)
;;(Getclipboard)
(setq result
        (vlax-invoke
            (vlax-get
                (vlax-get
                    (setq htmlfile (vlax-create-object "htmlfile"))
                   'ParentWindow
                )
               'ClipBoardData
            )
           'GetData
            "Text"
        )
    )

    (vlax-release-object htmlfile)

    result
)


(defun ROUNDUP ( n m )
    (cond
        ((equal 0.0 (rem n m) 1e-8) n)
        ((< n 0) (- n (rem n m)))
        ((+ n (- m (rem n m))))
    )
)


;; Get Dynamic Block Properties  -  Lee Mac
;; Returns an association list of Dynamic Block properties & values.
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [lst] Association list of ((<prop> . <value>) ... )

(defun LM:getdynprops ( blk )
    (mapcar '(lambda ( x ) (cons (vla-get-propertyname x) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)

;;---------------------=={ Lists Union }==--------------------;;
;;  Returns a list expressing the union of a set of lists     ;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;  Arguments:                                                ;;
;;  l - a list of lists for which to return the union         ;;
;;  Returns:  A list of distinct items in the set of lists    ;;
;;------------------------------------------------------------;;
;;Example Function Call
;;_$ (LM:ListsUnion '((1 2 3 4 5) (1 3 5 7 9) (2 4 6 8)))
;;(1 2 3 4 5 7 9 6 8)

(defun LM:ListsUnion ( l / x u )
  (setq l (apply 'append l))
  (while (setq x (car l)) (setq u (cons x u) l (vl-remove x l)))
  (reverse u)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;взято из эксперсса
(defun bns_count_format2 ( a b mx / )
  (while (<= (strlen a) mx)
;;  (setq a (strcat a "."))
  (setq a (acet-str-format "%1." a))
 );while
;; (setq a (strcat "\n" a b))
 (setq a (acet-str-format "\n%1%2" a b))
);defun bns_count_format


;; List to String  -  Lee Mac
;; Concatenates each string in a supplied list, separated by a given delimiter
;; lst - [lst] List of strings to concatenate
;; del - [str] Delimiter string to separate each item
;; _$ (LM:lst->str '("1" "2" "3" "4" "5") ",")
;; "1,2,3,4,5"
(defun LM:lst->str2 ( lst del )
    (if (cdr lst)
        (strcat (car lst) del (LM:lst->str (cdr lst) del))
        (vl-princ-to-string(car lst))
    )
)



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


;; Print List  -  Lee Mac
;; Prints a supplied list to the command-line or to a given filename,
;; with nested lists displayed in a hierarchical format.
;; l - [lst] List to print
;; f - [str] Optional filename

(defun LM:princl ( l f / _print _princ d r )
    
    (defun _print ( l i )
        (if (and (= 'list (type l)) (vl-list-length l) (vl-some 'vl-consp l))
            (progn
                (_princ (strcat "\n" i "("))
                (foreach x l (_print x (strcat i "    ")))
                (_princ (strcat "\n" i ")"))
            )
            (_princ (strcat "\n" i (vl-prin1-to-string l)))
        )
    )

    (eval
        (list 'defun '_princ '( x )
            (if (and (= 'str (type f)) (setq d (open f "w")))
                (list 'princ 'x d)
               '(princ x)
            )
        )
    )

    (setq r (vl-catch-all-apply '_print (list l "")))
    (if (= 'file (type d))
        (progn
            (setq d (close d))
            (startapp "notepad" f)
        )
    )
    (if (vl-catch-all-error-p r)
        (prompt (vl-catch-all-error-message r))
        l
    )
)

(defun princl ( l ) (LM:princl l nil) (princ))
(defun princf ( l ) (LM:princl l (vl-filename-mktemp "list" (getvar 'dwgprefix) ".txt")) (princ))
(princ)