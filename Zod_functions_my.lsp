
;измерение времени
(defun time ( / s seconds1 seconds2 i n)
(princ "\n время выполнения \n")
(setq n (getreal "\n Input new value \n"))
(setq s (getvar "DATE"))
(setq seconds1 (* 86400.0 (- s (fix s))))
(setq i 0)
(while (> n 0)
(setq i (1+ i) n (1- n))
)
(setq s (getvar "DATE"))
(setq seconds2 (* 86400.0 (- s (fix s))))
(- seconds2 seconds1)
)


(defun sstolist (ss / i lst); конвертирует набор в список
(setq i 0)
(if ss
(repeat (sslength ss)
(setq lst (append lst (list (ssname ss i))) i (1+ i))
));end of repeat & if
lst
);end of sstolist

(defun enametovla (lst); переводит список ename в vla-object или обратно.
(mapcar '(lambda (obj) (if (= (type obj) 'ename)
(vlax-ename->vla-object obj)
(if (= (type obj) 'vla-object) (vlax-vla-object->ename obj)))
)
lst)
);end of enametovla

;рекурсия
(defun spisok->spisok3 (n)
(if	(listp(car(reverse n)))
	n
	(append (list (list(car n)(cadr n)(caddr n))) (spisok->spisok3(cdddr n)))
)
)


;удаление пробелов
(defun removespacezz (textzz)
(while (wcmatch  textzz  "*  *")
(setq textzz (vl-string-subst " "  "  "   textzz))
)
(setq textzz (vl-string-trim " " textzz))
)


; из текстового примитива получает список из bit количества
(defun ent_to_spisok (ent_name  bit / number  spisok1 text flag)
(if	(vlax-property-available-p (vlax-ename->vla-object ent_name) 'textstring)
		(if (wcmatch (setq text(vl-string-trim " "(vl-list->string(mapcar '(lambda(x)(if(not(or(wcmatch(chr x)"#")(equal (chr x) ".")(equal (chr x) "-")))32 x))(vl-string->list(vl-string-translate "," "."(mip_MTEXT_Unformat (vla-get-textstring(vlax-ename->vla-object ent_name))))))))) "*#*")
			(repeat bit
				(while (null flag)
					(if (not(equal text ""))
						(if	(vl-string-search " " text)
							(if	(wcmatch (substr  text 1 (vl-string-search " " text)) "*#*")
								(setq number (atof text) flag t)
								(setq text (vl-string-trim " "(substr text (+ 2(vl-string-search " " text)) (-(strlen text)(vl-string-search " " text)))))
							)
							(setq flag t number (atof text))
						)
						(setq flag t number 0)
					)
				)
				(setq flag nil spisok1(append spisok1 (list number)))
				(if (not(equal text ""))
					(if	(vl-string-search " " text)
						(setq text (vl-string-trim " "(substr text (+ 2(vl-string-search " " text)) (-(strlen text)(vl-string-search " " text)))))
						(setq text "")
					)	
				)
			)
			(repeat bit(setq spisok1(append spisok1 (list 0))))
		)
	(repeat bit(setq spisok1(append spisok1 (list 0.0))))
)
(setq spisok1 spisok1)
)

; из текстового примитива получает число
(defun ent_to_number (ent_name / )
(if	(vlax-property-available-p (vlax-ename->vla-object ent_name) 'textstring)
	(atof(vl-string-trim " "(vl-list->string(mapcar '(lambda(x)(if(not(or(wcmatch(chr x)"#")(equal (chr x) ".")(equal (chr x) "-")))32 x))(vl-string->list(vl-string-translate "," "."(mip_MTEXT_Unformat (vla-get-textstring(vlax-ename->vla-object ent_name)))))))))
	0))


; из текста получает число
(defun text_to_number (text / )
(atof (setq text(vl-string-trim " "(vl-list->string(mapcar '(lambda(x)(if(not(or(wcmatch(chr x)"#")(equal (chr x) ".")(equal (chr x) "-")))32 x))(vl-string->list(vl-string-translate "," "."(mip_MTEXT_Unformat text))))))))
)

; из текста получает список
(defun text_to_spisok (text  bit / text number  spisok1 flag); из текстового примитива получает список из bit чисел
		(if (wcmatch (setq text(vl-string-trim " "(vl-list->string(mapcar '(lambda(x)(if(not(or(wcmatch(chr x)"#")(equal (chr x) ".")(equal (chr x) "-")))32 x))(vl-string->list(vl-string-translate "," "."(mip_MTEXT_Unformat text))))))) "*#*")
			(repeat bit
				(while (null flag)
					(if (not(equal text ""))
						(if	(vl-string-search " " text)
							(if	(wcmatch (substr  text 1 (vl-string-search " " text)) "*#*")
								(setq number (atof text) flag t)
								(setq text (vl-string-trim " "(substr text (+ 2(vl-string-search " " text)) (-(strlen text)(vl-string-search " " text)))))
							)
							(setq flag t number (atof text))
						)
						(setq flag t number 0)
					)
				)
				(setq flag nil spisok1 (append spisok1 (list number)))
				(if (not(equal text ""))
					(if	(vl-string-search " " text)
						(setq text (vl-string-trim " "(substr text (+ 2(vl-string-search " " text)) (-(strlen text)(vl-string-search " " text)))))
						(setq text "")
					)	
				)
			)
			(repeat bit(setq spisok1(append spisok1 (list 0))))
		)
(setq spisok1 spisok1)
)



(defun get_name_block01 (block01)
(if (vlax-property-available-p block01 'effectivename)
  (vla-get-effectivename block01)
  (vla-get-name block01)
  )
)

;_ проверка координат печати 
(defun fun_get_coord ( / minpt maxpt)
((lambda (/ minpt maxpt)
   (vla-GetWindowToPlot
     (vla-get-layout
       (vla-get-paperspace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       ) ;_ end of vla-get-ModelSpace
     ) ;_ end of vla-get-layout
     'minpt
     'maxpt
   ) ;_ end of vla-GetWindowToPlot
   (mapcar 'vlax-safearray->list (list minpt maxpt))
 ) ;_ end of lambda  
)
)



(defun Utext->text (text)(while (vl-string-search "%%U" text)(setq text(vl-string-subst "" "%%U" text))))

;рекурсия
;(defun Utext->text (text)(if (vl-string-search "%%U" text)(Utext->text(vl-string-subst "" "%%U" text))text))



(defun izvle4enie_atributa (blok_name tag_attr / zna4 Att_s)
(setq tag_attr(strcat "*" (strcase tag_attr) "*"))
(if	(> (vl-catch-all-apply 'length(list(setq Att_s (vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetAttributes (vlax-ename->vla-object blok_name ))))))))0)
		(mapcar	'(lambda (x)
			(if	(wcmatch (strcase (vla-get-TagString x)) (strcase tag_attr))(setq zna4 (mip_MTEXT_Unformat(vla-get-TextString x))))
					)
		Att_s
		)
)
(if zna4 zna4 "")
)

(defun zapis_artributa (blok_name tag_attr zna4 / Att_s) 
(if (numberp zna4)(setq zna4(rtos zna4)))
(if	(> (vl-catch-all-apply 'length(list(setq Att_s (vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetAttributes (vlax-ename->vla-object blok_name ))))))))0)
		(mapcar	'(lambda (x)
			(if	(wcmatch (strcase (vla-get-TagString x)) (strcase tag_attr))(vla-put-TextString x zna4))
				)
		Att_s
		)
)
)

;пример использования (zapis_atributov_iz_spiska (car(entsel))  '(("П1.." "888")))
(defun zapis_atributov_iz_spiska (blok_name att_list / zna4 Att_s)
(setq att_list (mapcar '(lambda(x)(list (strcat "*" (strcase (car x)) "*") (cadr x)))att_list))
(if	(> (vl-catch-all-apply 'length(list(setq Att_s (vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetAttributes (vlax-ename->vla-object blok_name ))))))))0)
		(mapcar	'(lambda (x)
			(mapcar '(lambda (z)
				(if	(wcmatch (strcase (vla-get-TagString x)) (car z))(vla-put-TextString x (cadr z)))
					)
			att_list
			)
				)
		Att_s
		)
)
)

;пример использования (izvle4enie_atributov_iz_spiska (car(entsel))  '("П1.." "888"))
(defun izvle4enie_atributov_iz_spiska (blok_name att_list / zna4 Att_s)
(setq att_list (mapcar '(lambda(x)(list (strcat "*" (strcase x) "*") ""))att_list))
(if	(> (vl-catch-all-apply 'length(list(setq Att_s (vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetAttributes (vlax-ename->vla-object blok_name ))))))))0)
		(mapcar	'(lambda (x)
			(mapcar '(lambda (z)
				(if	(wcmatch (strcase (vla-get-TagString x)) (car z))(setq att_list (subst (list (car z)(vla-get-TextString x)) z att_list)))
					)
			att_list
			)
				)
		Att_s
		)
)
(setq att_list (mapcar '(lambda(x)(list (vl-string-trim "*" (car x))(cadr x)))att_list))
)




(defun izvl_texta_ili_attrib ( tag_attrib / ent1 zna4 flag1)
(while (null flag1)
	(setq ent1 (car(entsel "\n Выберете текст либо блок")))
	(if ent1
		(cond
			(	(wcmatch (cdr(assoc 0(entget ent1)))"TEXT,MTEXT,MULTILEADER")	(setq zna4(mip_MTEXT_Unformat(vla-get-textstring (vlax-ename->vla-object ent1)))flag1 T)		)
			(	(equal(cdr(assoc 0(entget ent1)))"INSERT")						(setq zna4(izvle4enie_atributa ent1 tag_attrib)flag1 T)		)
		)
	)
)
(if zna4 zna4 nil)
)


(defun zapis_texta_ili_attrib (tag_attrib zna4 / ent1 flag1)
(while (null flag1)
	(setq ent1 (car(entsel "\n Выберете итоговый текст либо блок")))
	(if ent1
		(cond
			(	(wcmatch (cdr(assoc 0(entget ent1)))"TEXT,MTEXT,MULTILEADER")	(vla-put-textstring (vlax-ename->vla-object ent1) zna4)(setq flag1 T))
			(	(equal(cdr(assoc 0(entget ent1)))"INSERT")						(zapis_artributa ent1 tag_attrib zna4)(setq flag1 T))
		)
	)
)
)


;переработанная функция выделения динамических блоков по имени из Приложения А.Ривилиса selsim
;ищет имена динамических блоков
;  (SELBLK2 (entget(car(entsel))))
; результат "`*U1604,`*U1605,`*U1610,`*U1611,`*U1612,`*U1620,`*U1615,`*U1616,`*U1617,`*U1608,`*U1618,`*U1619,`*U1613,`*U1614,`*U1609,$_OV_V_obor_ust_prit"
; либо можно так использовать  (sssetfirst nil (ssget "X" (list(cons 2 (SELBLK2 (entget(car(entsel))))))))
(defun selblk2 (ed / bname ss blst n en1 ed1 eo1 result selblk:remwild)
  (defun selblk:remwild	(str / str1 i)
    (setq i 1
	  str1 ""
    ) ;_ end of setq
    (while (<= i (strlen str))
      (if (= "*" (substr str i 1))
	(setq str1 (strcat str1 "`"))
      ) ;_ end of if
      (setq str1 (strcat str1 (substr str i 1)))
      (setq i (1+ i))
    ) ;_ end of while
    str1
  ) ;_ end of defun
    (setq ed1 (vlax-ename->vla-object (cdr (assoc -1 ed))))
     (setq bname (vla-get-EffectiveName ed1)
	   n	 0
	   ss	 (ssget "X" '((0 . "INSERT") (2 . "`*U*")))
	   blst	 (cons bname nil)
     ) ;_ end of setq
     (while (< n (sslength ss))
       (setq en1 (ssname ss n)
	     ed1 (entget en1)
	     eo1 (vlax-ename->vla-object en1)
       ) ;_ end of setq
       (if (and
			 (= bname (vla-get-EffectiveName eo1))
			 (not (member (cdr (assoc 2 ed1)) blst))
			) ;_ end of and
			(setq blst (cons (cdr (assoc 2 ed1)) blst))
       ) ;_ end of if
       (setq n (1+ n))
     ) ;_ end of while
     (setq n 0
	   result ""
     ) ;_ end of setq
     (while (< n (length blst))
       (setq result (strcat result (selblk:remwild (nth n blst))))
       (setq n (1+ n))
       (if (< n (length blst))
	 (setq result (strcat result ","))
       ) ;_ end of if
     ) ;_ end of while
  result
) ;_ end of defun

; извлечение имени и значения свойства динамического блока в виде листов а не ввиде точечных пар, и все в виде текста
(defun LM:getdynprops2 ( blk )
    (mapcar '(lambda ( x ) (list (vl-princ-to-string(vla-get-propertyname x)) (vl-princ-to-string(vlax-get x 'value))))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)


; удаление свойства origin из списка
(defun MM:Remove_origin ( lst )
    (vl-remove-if 
		(function (lambda(y)(equal (car y) "Origin")))
		lst
	)
)

(defun MM:Sep_string ( lst )
(foreach x lst (princ (strcat (vl-prin1-to-string x) "\n")))
(princ)
)


; создание текста
(defun make_text ( z_x z_y z_layer z_textstring / )
(entmake
	(list
		'(0 . "TEXT")
		'(100 . "AcDbText")
		'(100 . "AcDbEntity")
		(cons 8 z_layer)
		(list '10 z_x z_y '0.0)
		'(40 . 250.0)
		(cons 1 z_textstring)
	)
)
(princ)
)



; получить список имен слоев и сортировка в обратном порядке
(defun get_layers_name (  / layer01 layernamelist)
(setq layer01 (tblnext "LAYER" T))
(while layer01
	(setq layernamelist (append layernamelist (list (cdr (assoc 2 layer01)))))
	(setq layer01 (tblnext "LAYER"))
)
(setq layernamelist (vl-sort layernamelist '(lambda(x1 x2)(<  x2 x1))))
)

; включить тихий режим
(defun silent_mod_on ( / )
(if		(getvar "REGENMODE")			(setvar "REGENMODE" 0))
(if		(getvar "CMDECHO")				(setvar "CMDECHO" 0))
(if		(getvar "FILEDIA")				(setvar "FILEDIA" 0))
(if		(getvar "QAFLAGS")				(setvar "QAFLAGS" 5))
)

; выключить тихий режим
(defun silent_mod_off ( / )
(if		(getvar "REGENMODE")			(setvar "REGENMODE" 1))
(if		(getvar "CMDECHO")				(setvar "CMDECHO" 1))
(if		(getvar "FILEDIA")				(setvar "FILEDIA" 1))
(if		(getvar "QAFLAGS")				(setvar "QAFLAGS" 2))
)


; удаляет dxf пару по номеру
(defun remove_dxf_pair ( bit entget01 / )
(vl-remove-if(function (lambda(x)(equal (car x) bit)))entget01)
)

; удаляет dxf пару по номеру
(defun remove_dxf_pair2 ( bitlist01 entget01 / )
(vl-remove-if(function (lambda(x)(member (car x) bitlist01)))entget01)
)

; удаляет dxf пару по номеру кроме 2 у блоков
(defun remove_dxf_pair3 ( bitlist01 entget01 / )
(if	(member '(0 . "INSERT") entget01)
	(setq bitlist01 (vl-remove 2 bitlist01))
)
(vl-remove-if(function (lambda(x)(member (car x) bitlist01)))entget01)
)

; ssget http://lee-mac.com/ssget.html

