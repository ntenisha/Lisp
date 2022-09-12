(defun listtoss (list01 / ss1)
(setq ss1 (ssadd))
(foreach x list01
	(ssadd x ss1)
)
ss1
)



;;-------------------=={ UnFormat String }==------------------;;
;;                                                            ;;
;;  Returns a string with all MText formatting codes removed. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  str - String to Process                                   ;;
;;  mtx - MText Flag (T if string is for use in MText)        ;;
;;------------------------------------------------------------;;
;;  Returns:  String with formatting codes removed            ;;
;;------------------------------------------------------------;;
; пример (LM:UnFormat "{\\O\\C1;L\\C256;ee} {\\L\\C2;M\\C256;ac}" nil)
(defun LM:UnFormat ( str mtx / _replace rx )

    (defun _replace ( new old str )
        (vlax-put-property rx 'pattern old)
        (vlax-invoke rx 'replace str new)
    )
    (if (setq rx (vlax-get-or-create-object "VBScript.RegExp"))
        (progn
            (setq str
                (vl-catch-all-apply
                    (function
                        (lambda ( )
                            (vlax-put-property rx 'global     actrue)
                            (vlax-put-property rx 'multiline  actrue)
                            (vlax-put-property rx 'ignorecase acfalse) 
                            (foreach pair
                               '(
                                    ("\032"    . "\\\\\\\\")
                                    (" "       . "\\\\P|\\n|\\t")
                                    ("$1"      . "\\\\(\\\\[ACcFfHLlOopQTW])|\\\\[ACcFfHLlOopQTW][^\\\\;]*;|\\\\[ACcFfHLlOopQTW]")
                                    ("$1$2/$3" . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
                                    ("$1$2"    . "\\\\(\\\\S)|[\\\\](})|}")
                                    ("$1"      . "[\\\\]({)|{")
                                )
                                (setq str (_replace (car pair) (cdr pair) str))
                            )
                            (if mtx
                                (_replace "\\\\" "\032" (_replace "\\$1$2$3" "(\\\\[ACcFfHLlOoPpQSTW])|({)|(})" str))
                                (_replace "\\"   "\032" str)
                            )
                        )
                    )
                )
            )
            (vlax-release-object rx)
            (if (null (vl-catch-all-error-p str))
                str
            )
        )
    )
)
(vl-load-com)


(defun LM:UnFormat2 ( str mtx / _replace rx )

    (defun _replace ( new old str )
        (vlax-put-property rx 'pattern old)
        (vlax-invoke rx 'replace str new)
    )
    (if (setq rx (vlax-get-or-create-object "VBScript.RegExp"))
        (progn
            (setq str
                (vl-catch-all-apply
                    (function
                        (lambda ( )
                            (vlax-put-property rx 'global     actrue)
                            (vlax-put-property rx 'multiline  actrue)
                            (vlax-put-property rx 'ignorecase acfalse) 
                            (foreach pair
                               '(
                                    ("\032"    . "\\\\\\\\")
                                    ("$1"      . "\\\\(\\\\[ACcFfHLlOopQTW])|\\\\[ACcFfHLlOopQTW][^\\\\;]*;|\\\\[ACcFfHLlOopQTW]")
                                    ("$1$2/$3" . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
                                    ("$1$2"    . "\\\\(\\\\S)|[\\\\](})|}")
                                    ("$1"      . "[\\\\]({)|{")
                                )
                                (setq str (_replace (car pair) (cdr pair) str))
                            )
                            (if mtx
                                (_replace "\\\\" "\032" (_replace "\\$1$2$3" "(\\\\[ACcFfHLlOoPpQSTW])|({)|(})" str))
                                (_replace "\\"   "\032" str)
                            )
                        )
                    )
                )
            )
            (vlax-release-object rx)
            (if (null (vl-catch-all-error-p str))
                str
            )
        )
    )
)
(vl-load-com)

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




;функция mip_MTEXT_Unformat взята http://kpblc.blogspot.com/2007_06_01_archive.html
(defun mip_mtext_unformat3 (mtext / text str)
  (setq text "")
  (while (/= mtext "")
    (cond
      ((wcmatch (strcase (substr mtext 1 2)) "\\[F]")
       (setq mtext (substr mtext (+ 2 (vl-string-search ";" mtext))))
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
        (strcat (car lst) del (LM:lst->str2 (cdr lst) del))
        (vl-princ-to-string(car lst))
    )
)


;; List to String  -  Lee Mac
;; Concatenates each string in a supplied list, separated by a given delimiter
;; lst - [lst] List of strings to concatenate
;; del - [str] Delimiter string to separate each item

(defun LM:lst->str ( lst del / str )
    (setq str (car lst))
    (foreach itm (cdr lst) (setq str (strcat str del itm)))
    str
)


;; String to List  -  Lee Mac
;; Separates a string using a given delimiter
;; str - [str] String to process
;; del - [str] Delimiter by which to separate the string
;; Returns: [lst] List of strings

(defun LM:str->lst ( str del / len lst pos )
    (setq len (1+ (strlen del)))
    (while (setq pos (vl-string-search del str))
        (setq lst (cons (substr str 1 pos) lst)
              str (substr str (+ pos len))
        )
    )
    (reverse (cons str lst))
)



;;----------------=={ Add Objects to Block }==----------------;;
;;                                                            ;;
;;  Adds all objects in the provided SelectionSet to the      ;;
;;  definition of the specified block.                        ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  doc   - Document Object in which block resides.           ;;
;;  block - Entity name of reference insert                   ;;
;;  ss    - SelectionSet of objects to add to definition      ;;
;;------------------------------------------------------------;;

(defun LM:AddObjectstoBlock ( doc block ss / lst mat )
  
  (setq lst (LM:ss->vla ss)
        mat (LM:Ref->Def block)
        mat (vlax-tmatrix (append (mapcar 'append (car mat) (mapcar 'list (cadr mat))) '((0. 0. 0. 1.))))
  )
  (foreach obj lst (vla-transformby obj mat))

  (vla-CopyObjects doc (LM:SafearrayVariant vlax-vbobject lst)
    (vla-item (vla-get-Blocks doc) (cdr (assoc 2 (entget block))))
  )
  (foreach obj lst (vla-delete obj))
  (vla-regen doc acAllViewports)
)

;;-----------------=={ Remove From Block }==------------------;;
;;                                                            ;;
;;  Removes an Entity from a Block Definition                 ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  ent - Entity name of Object to Delete from Block [ENAME]  ;;
;;------------------------------------------------------------;;

(defun LM:RemovefromBlock ( doc ent )
  (vla-delete (vlax-ename->vla-object ent))
  (vla-regen doc acAllViewports)
  (princ)
)

;;------------------=={ Safearray Variant }==-----------------;;
;;                                                            ;;
;;  Creates a populated Safearray Variant of a specified      ;;
;;  data type                                                 ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  datatype - variant type enum (eg vlax-vbDouble)           ;;
;;  data     - list of static type data                       ;;
;;------------------------------------------------------------;;
;;  Returns:  VLA Variant Object of type specified            ;;
;;------------------------------------------------------------;;
                         
(defun LM:SafearrayVariant ( datatype data )
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray datatype (cons 0 (1- (length data)))) data
    )    
  )
)

;;------------=={ SelectionSet -> VLA Objects }==-------------;;
;;                                                            ;;
;;  Converts a SelectionSet to a list of VLA Objects          ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  ss - Valid SelectionSet (Pickset)                         ;;
;;------------------------------------------------------------;;
;;  Returns:  List of VLA Objects, else nil                   ;;
;;------------------------------------------------------------;;

(defun LM:ss->vla ( ss / i l )
  (if ss
    (repeat (setq i (sslength ss))
      (setq l (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) l))
    )
  )
)

;;---------------=={ Block Ref -> Block Def }==---------------;;
;;                                                            ;;
;;  Returns the Transformation Matrix and Translation Vector  ;;
;;  for transforming Block Reference Geometry to the Block    ;;
;;  Definiton.                                                ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  e - Block Reference Entity                                ;;
;;------------------------------------------------------------;;
;;  Returns:  List of 3x3 Transformation Matrix, Vector       ;;
;;------------------------------------------------------------;;

(defun LM:Ref->Def ( e / _dxf a l n )

  (defun _dxf ( x l ) (cdr (assoc x l)))

  (setq l (entget e) a (- (_dxf 50 l)) n (_dxf 210 l))
  (
    (lambda ( m )
      (list m
        (mapcar '- (_dxf 10 (tblsearch "BLOCK" (_dxf 2 l)))
          (mxv m
            (trans (_dxf 10 l) n 0)
          )
        )
      )
    )
    (mxm
      (list
        (list (/ 1. (_dxf 41 l)) 0. 0.)
        (list 0. (/ 1. (_dxf 42 l)) 0.)
        (list 0. 0. (/ 1. (_dxf 43 l)))
      )
      (mxm
        (list
          (list (cos a) (sin (- a)) 0.)
          (list (sin a) (cos a)     0.)
          (list    0.        0.     1.)
        )
        (mapcar '(lambda ( e ) (trans e n 0 t))
         '(
            (1. 0. 0.)
            (0. 1. 0.)
            (0. 0. 1.)
          )
        )
      )
    )
  )
)

;; Matrix x Vector  -  Vladimir Nesterovsky
(defun mxv ( m v )
  (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
(defun mxm ( m q )
  (mapcar (function (lambda ( r ) (mxv (trp q) r))) m)
)

;; Matrix Transpose  -  Doug Wilson
(defun trp ( m )
  (apply 'mapcar (cons 'list m))
)

;;---------------------=={ Select if }==----------------------;;
;;                                                            ;;
;;  Provides continuous selection prompts until either a      ;;
;;  predicate function is validated or a keyword is supplied. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  msg  - prompt string                                      ;;
;;  pred - optional predicate function [selection list arg]   ;;
;;  func - selection function to invoke                       ;;
;;  keyw - optional initget argument list                     ;;
;;------------------------------------------------------------;;
;;  Returns:  Entity selection list, keyword, or nil          ;;
;;------------------------------------------------------------;;

(defun LM:SelectIf ( msg pred func keyw / sel ) (setq pred (eval pred))  
  (while
    (progn (setvar 'ERRNO 0) (if keyw (apply 'initget keyw)) (setq sel (func msg))
      (cond
        ( (= 7 (getvar 'ERRNO))

          (princ "\nMissed, Try again.")
        )
        ( (eq 'STR (type sel))

          nil
        )
        ( (vl-consp sel)

          (if (and pred (not (pred sel)))
            (princ "\nInvalid Object Selected.")
          )
        )
      )
    )
  )
  sel
)

;-------------------------------------------------------------;
;                   -- Test Functions --                      ;
;-------------------------------------------------------------;

(defun c:Add2Block ( / *error* _StartUndo _EndUndo acdoc ss e )

  (defun *error* ( msg )
    (if acdoc (_EndUndo acdoc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )
  
  (setq acdoc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (if
    (and (setq ss (ssget "_:L"))
      (setq e
        (LM:SelectIf "\nSelect Block to Add Objects to: "
         '(lambda ( x ) (eq "INSERT" (cdr (assoc 0 (entget (car x)))))) entsel nil
        )
      )
    )
    (progn
      (_StartUndo acdoc) (LM:AddObjectstoBlock acdoc (car e) ss) (_EndUndo acdoc)
    )
  )
  (princ)
)

;-------------------------------------------------------------;

(defun c:Remove ( / *error* _StartUndo _EndUndo acdoc e )

  (defun *error* ( msg )
    (if acdoc (_EndUndo acdoc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )
  
  (setq acdoc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (while (setq e (car (nentsel "\nSelect Object to Remove: ")))
    (_StartUndo acdoc) (LM:RemovefromBlock acdoc e) (_EndUndo acdoc)
  )
  (princ)
)

(vl-load-com) (princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;взято из express tools
;Takes two block names and replaces all references to the first with references to 
;the second.
;returns the number of block references modified
;
(defun acet-block-replace ( bna bna2 / flt ss n j na e1 lst )
   (setq flt (list '(0 . "INSERT")
                    (cons 2 bna)
             )
          ss (ssget "_x" flt)
   );setq
   (setq j 0)
   (if ss
       (progn
        (setq n 0)
        (repeat (sslength ss)
         (setq na (ssname ss n)
               e1 (entget na)
               e1 (subst (cons 2 bna2) (assoc 2 e1) e1)
         );setq
         (if (entmod e1)
             (setq j (+ j 1))
         );if
         (setq n (+ n 1));setq
        );repeat
       );progn then got a selection set of inserts
   );if
   
   ;; get a list of nested block inserts
   (setq lst (bns_blktbl_match flt)) 
   (setq n 0)
   (repeat (length lst)
    (setq na (car (nth n lst))
          e1 (entget na)
          e1 (subst (cons 2 bna2) (assoc 2 e1) e1)
    );setq
    (if (entmod e1)
        (setq j (+ j 1))
    );if
    (setq n (+ n 1));setq
   );repeat       
 
 j
);defun acet-block-replace




 (defun _kpblc-list-dublicates-remove (lst / result)
                                     ;|
*    Функция исключения дубликатов элементов списка 
*    Параметры вызова:
*	lst	обрабатываемый список
*    Возвращаемое значение: список без дубликатов соседних элементов
*    Примеры вызова:
(_kpblc-list-dublicates-remove '((0.0 0.0 0.0) (10.0 0.0 0.0) (10.0 0.0 0.0) (0.0 0.0 0.0)) nil)
((0.0 0.0 0.0) (10.0 0.0 0.0) (0.0 0.0 0.0))
|;
  (foreach x lst
    (if (not (member x result))
      (setq result (cons x result))
      ) ;_ end of if
    ) ;_ end of foreach
  (reverse result)
  ) ;_ end of defun

(defun _kpblc-list-dublicates-stay (lst / res)
                                   ;|
*    Оставляет дубликаты списка
|;
  (if (and lst (= (type lst) 'list))
    (progn
      (foreach item lst
        (if (member item (cdr (member item lst)))
          (setq res (cons item res))
          ) ;_ end of if
        ) ;_ end of foreach
      (setq res (_kpblc-list-dublicates-remove (reverse res)))
      ) ;_ end of progn
    ) ;_ end of if
  res
  )

(defun c:pu2 ()
;;;======================================================
;;; СПИСОК МАСШТАБОВ SCALELIST SCALE
;;;======================================================

(vl-catch-all-apply
  '(lambda ()
     ((lambda (lst / dict dn)
;;; Purge excess scales
;;; gile
;;; http://www.theswamp.org/index.php?topic=29663.0 
;;;lst - шаблон маштабов состоит из списков вида
;;;  (("имя в списке масштабов1" "Масштаб единицы листа1" "Масштаб единицы чертежа1")
;;;   ("имя в списке масштабов2" "Масштаб единицы листа2" "Масштаб единицы чертежа2")
;;;   ...
;;;   )
;;; lst - the pattern scale is made up of lists of species 
;;; (("Name of the Scale 1" Scale_paper_unit_1 Scale_drawing_unit_1) 
;;; ("Name of the Scale 2"  Scale_paper_unit_2 Scale_drawing_unit_2) 
;;; ... 
;;;)
;;; Usage (SetScale)
;;;  (setq pat '(("1:1" 1 1)("1:2" 1 2)("1:10" 1 10) ;_Correct scale here
;;;	      ("1:50" 1 50)("1:100" 1 100)("2:1" 2 1)
;;;	      ))
	(setq dn "A")
        (if (setq dict (dictsearch (namedobjdict) "ACAD_SCALELIST"))
          (progn
            (entmod (vl-remove-if
                      '(lambda (x) (or (= (car x) 3) (= (car x) 350)))
                      dict
                    ) ;_ end of vl-remove-if
            ) ;_ end of entmod
            (setq dict (cdr (assoc -1 dict))
                  n    -1
            ) ;_ end of setq
            (foreach s lst
              (dictadd dict
		       (progn
			 (if (= n 9)
			 (setq dn (chr(1+ (ascii dn)))
			       n -1
			       )
			 )
			 (terpri)
			 (princ
			   (strcat dn (itoa (setq n (1+ n))))
			   )
			 )
                       (entmakex
                         (list
                           '(0 . "SCALE")
                           '(100 . "AcDbScale")
                           (cons 300 (car s))
                           (cons 140 (cadr s))
                           (cons 141 (caddr s))
               		  '(70 . 0) ;_ kpblc http://forum.dwg.ru/showthread.php?t=73416
	                  '(290 . 1);_ kpblc http://forum.dwg.ru/showthread.php?t=73416
                         ) ;_ end of list
                       ) ;_ end of entmakex
              ) ;_ end of dictadd
            ) ;_ end of foreach
          ) ;_ end of progn
        ) ;_ end of if
      )
       '(("1:1" 1 1)
	 ("1:2" 1 2)
	 ("1:10" 1 10)
	 ("1:50" 1 50)
	 ("1:100" 1 100)
	 ("2:1" 2 1)
	)
     )
   )
)

)

;;    Переводит строку в нижний регистр
;;    Параметры вызова:
;;  str    обрабатываемая строка
;;    Примеры вызова:
;;(_kpblc-strcase "ПРОЧИЕ") ; "прочие"
(defun _kpblc-strcase (str)
	(strcase (vl-string-translate "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ" "абвгдеёжзийклмнопрстуфхцчшщъыьэюя" str)
           t
           ) ;_ end of strcase
)
(defun strcase2 (str)(_kpblc-strcase str ))
