(vl-load-com)

;вставляет объект в базовые точки объектов 
(defun c:Zod (/ ss1 ss2 pt1 ww pt)
(command "._undo" "_be")
(princ "\nвставляет объект в базовые точки объектов ")
(princ "\nВыбор элементов для размножения: ")
(setq ss2 (ssget))
(setq pt1 (getpoint "\nБазовая точка: "))
(princ "\nВыбор объектов для вставки ")
(setq ss1 (ssget))
(setq ww (apply
	'append
		(mapcar
			'(lambda (name)
				(mapcar
					'cdr
						(vl-remove-if-not
							'(lambda (x) (= (car x) 10))
								(entget name)
						) ;_ end of vl-remove-if-not
				) ;_ end of mapcar
			) ;_ end of lambda
		(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
		) ;_ end of mapcar
	) ;_ end of apply
);_ end of setq

(setq ww (mapcar '(lambda (pt) (vlax-3d-point pt)) ww) pt1 (vlax-3d-point pt1)); по логике их надо запросить в vla виде,
(mapcar '(lambda (pt) (mapcar '(lambda (obj) (vla-move (vla-copy obj) pt1 pt)) (enametovla (sstolist ss2)))) ww); аналог копирования через vla.
(command "._undo" "_end")
);end defun



;Выравнивание текста по оси Y
(defun c:orty (/ obj ss1 yy)
(princ "\nВыравнивание текста по оси Y ")
(setq obj (vlax-ename->vla-object(car(entsel "Выберете текст"))))
(if	(vl-catch-all-apply '> (list(vl-catch-all-apply'vla-get-Alignment(list obj))0))
	(setq yy(cadr(vlax-safearray->list(vlax-variant-value(vla-get-TextAlignmentPoint obj)))))
	(setq yy(cadr(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint obj)))))
)
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "TEXT")))
(vlax-for x ss1
	(if	(vl-catch-all-apply '> (list(vl-catch-all-apply'vla-get-Alignment(list x))0))
		(vla-put-TextAlignmentPoint x	(vlax-3d-Point(list (car(vlax-safearray->list(vlax-variant-value(vla-get-TextAlignmentPoint x)))) 	yy)))
		(vla-put-InsertionPoint x		(vlax-3d-Point(list (car(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint x))))		yy)))
	)
)
(vla-clear ss1)
)





;Выравнивание текста по оси X
(defun c:ortx (/ obj ss1 yy)
(princ "\nВыравнивание текста по оси X ")
(setq obj (vlax-ename->vla-object(car(entsel "Выберете текст"))))
(if	(vl-catch-all-apply '> (list(vl-catch-all-apply'vla-get-Alignment(list obj))0))
	(setq xx(car(vlax-safearray->list(vlax-variant-value(vla-get-TextAlignmentPoint obj)))))
	(setq xx(car(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint obj)))))
)
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "TEXT")))
(vlax-for x ss1
	(if	(vl-catch-all-apply '> (list(vl-catch-all-apply'vla-get-Alignment(list x))0))
		(vla-put-TextAlignmentPoint x	(vlax-3d-Point(list xx (cadr(vlax-safearray->list(vlax-variant-value(vla-get-TextAlignmentPoint x)))))))
		(vla-put-InsertionPoint x		(vlax-3d-Point(list xx (cadr(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint x)))))))
	)
)
(vla-clear ss1)
)



;Убирает почеркнутый текст
(defun c:Zod2 (/ ss1)
(princ "\nУбирает почеркнутый текст ")
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((1 . "%%U*")))
(vlax-for x ss1 (vla-put-textstring x (Utext->text(vla-get-textstring x))))
(vla-clear ss1)
(princ)
)





;слияние текста
(defun c:Zod3 (/ flag x1 x x_txt la_x old_lock y y_txt x_txt_s y_txt_m)
(princ "\nслияние текста ")
(setq actdoc (vla-get-activedocument(vlax-get-acad-object)))
(vla-startUndoMark actdoc)
(setq 	my_error	*error*
	*error*		Zoderror
)
(while (null flag)
	(if (and (setq x1 (nentsel "\nИсходный текст"))
		(vlax-property-available-p (vlax-ename->vla-object(car x1)) 'textstring)
  	    ) ;_ end of and
	     (if (= (vl-catch-all-apply 'vla-get-isxref (list(vl-catch-all-apply 'vla-item (list(vla-get-blocks (vla-get-activedocument(vlax-get-acad-object))) (vl-catch-all-apply 'vla-get-EffectiveName (list(vl-catch-all-apply 'vlax-ename->vla-object (list(last(last x1)))))))))) :vlax-true)
		(progn		
			(alert "Выбрана внешняя ссылка. Выберете текст, не лежащий во внешней ссылке.")
			(setq flag nil)
		)
		(progn
			(setq 	x (car x1)
				flag T
				x_txt (vla-get-textstring(vlax-ename->vla-object x))
				la_x (tblobjname "layer" (cdr(assoc 8(entget(car x1)))))
			) ;_ end of setq
			(cond 
				(	(member (cdr(assoc 0(entget x))) '("TEXT"))															(setq x_txt_s T)	)
				(	(and (member (cdr(assoc 0(entget x))) '("ATTRIB" "ATTDEF")) (= (vl-catch-all-apply 'vla-get-MTextAttribute(list(vlax-ename->vla-object x))) :vlax-false))	(setq x_txt_s T)	)
			) ;_ end of cond
			(if (= (cdr(assoc 70 (entget la_x))) 4)
				(progn
					(setq old_lock (assoc 70 (entget la_x))
					) ;_ end of setq
					(entmod(subst '(70 . 0)(assoc 70 (entget la_x)) (entget la_x)))
				) ;_ end of progn
			) ;_ end of if
		) ;_ end of progn
	      ) ;_ end of if
	) ;_ end of if
) ;_ end of while

(setvar "errno" 0)
(while (not (equal (getvar "errno") 52))
	
	(setq y (car (nentsel "\nТекст для слияния...")))
	(if (and 
		y
		(not(equal x y))
		(vlax-property-available-p (vlax-ename->vla-object y) 'textstring)
	     )
		(progn
			(setq y_txt (vla-get-textstring(vlax-ename->vla-object y)))
			(cond 
				(	(member (cdr(assoc 0(entget y))) '("MTEXT" "MULTILEADER"))													(setq y_txt_m T)	)
				(	(and (member (cdr(assoc 0(entget y))) '("ATTRIB" "ATTDEF"))(= (vl-catch-all-apply 'vla-get-MTextAttribute(list(vlax-ename->vla-object y)))) :vlax-true)		(setq y_txt_m T)	)
			) ;_ end of cond
			(entdel y)
			(if (and x_txt_s y_txt_m)
			    (setq y_txt (mip_MTEXT_Unformat y_txt))
			) ;_ end of if
       			(vla-put-textstring(vlax-ename->vla-object x)(removespacezz(strcat x_txt " " y_txt)))
       			(setq x_txt (vla-get-textstring(vlax-ename->vla-object x)))
			;(if y_txt_m (setq y_txt_m nil))
			(cond
				(	(= (length x1) 2)	(entupd (car x1))		)
				(	(= (length x1) 4)	(entupd (last(last x1)))	)
			) ;_ end of cond
		) ;_ end of progn
	) ;_ end of if
) ;_ end of while
	
(if old_lock
	(progn
		(entmod(subst old_lock (assoc 70 (entget la_x)) (entget la_x)))
		(cond
			(	(= (length x1) 2)	(entupd (car x1))		)
			(	(= (length x1) 4)	(entupd (last(last x1)))	)
		) ;_ end of cond
	) ;_ end of progn
) ;_ end of if
(vla-EndUndoMark actdoc)
(setq *error* my_error)
(princ)
) ;_ end of defun





(defun Zoderror (msg)
(vl-cmdf)
(vla-EndUndoMark actdoc)
(setq *error* my_error)
(vl-cmdf "Undo" "" )
) ;_ end of defun


;конец ZOD3





;Копирование текста мультивыноски другим мультивыноскам
(defun Zod4 ( FLAG / text1 ss1)
(princ "\nКопирование текста мультивыноски другим мультивыноскам ")
(setq text1 (vla-get-textstring(vlax-ename->vla-object(car(entsel "выберете мультивыноску")))))
(if (equal FLAG 2) (setq text1 (mip_mtext_unformat text1)))
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "MTEXT")))
(vlax-for x ss1 (vla-put-textstring x text1))
(vla-clear ss1 )
)
(defun Zod4 ( FLAG / text1 ss1)
(princ "\nКопирование текста мультивыноски другим мультивыноскам ")
(setq text1 (vla-get-textstring(vlax-ename->vla-object(car(entsel "выберете мультивыноску")))))
(if (equal FLAG 2) (setq text1 (mip_mtext_unformat text1)))
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "MULTILEADER")))
(vlax-for x ss1 (vla-put-textstring x text1))
(vla-clear ss1 )
)
(defun C:Zod4	()(Zod4	1))
(defun C:Zod4z 	()(Zod4	2))





;Меняет текст на исходный после использования align_den
(defun c:Zod5 (/ ss1 l1 number)
(princ "\nМеняет текст на исходный после использования align_den ")
(command "._undo" "_be")
(setq ss1
	(ssget '(
		(-4 . "<OR")
			(-4 . "<AND")(-4 . ">")(40 . 122)(-4 . "<")(40 . 123)(-4 . "AND>")
			(-4 . "<AND")(-4 . ">")(40 . 170)(-4 . "<")(40 . 171)(-4 . "AND>")
			(-4 . "<AND")(-4 . ">")(40 . 239)(-4 . "<")(40 . 240)(-4 . "AND>")
			(-4 . "<AND")(-4 . ">")(40 . 341)(-4 . "<")(40 . 342)(-4 . "AND>")
			(-4 . "<AND")(-4 . ">")(40 . 683)(-4 . "<")(40 . 684)(-4 . "AND>")
		(-4 . "OR>")
		)
	);end ssget
);end setq
(setq number (sslength ss1))
(setq l1 '((122 123 180)(170 171 250)(239 240 350)(341 342 500)(683 684 1000)) )
(while (> number 0)
(foreach p l1 (if
		(and 
			(> (cdr(assoc 40 (entget (ssname ss1 (1- number))))) (car p))
			(< (cdr(assoc 40 (entget (ssname ss1 (1- number))))) (cadr p))
		);end and
		(progn 
			(entmod (subst 
				(cons 40 (caddr p)) 
				(assoc 40 (entget (ssname ss1 (1- number)))) 
				(entget (ssname ss1 (1- number)))
			);end subst	
			);end entmod

			(entmod (subst 
				(cons 41 1) 
				(assoc 41 (entget (ssname ss1 (1- number)))) 
				(entget (ssname ss1 (1- number)))
			);end subst	
			);end entmod
			(entmod (subst 
				(cons 51 (angtof "15")) 
				(assoc 51 (entget (ssname ss1 (1- number)))) 
				(entget (ssname ss1 (1- number)))
			);end subst	
			);end entmod

		);end progn
	       );end if
);end foreach


(ssdel (ssname ss1 (1- number)) ss1)
(setq number (1- number))

);end while

(command "._undo" "_end")

);end defun





;для извлечения нужных DFX пар из примитива
(defun c:Zod6 (/ bit zz)
(princ "\nизвлечение нужных DFX пар из примитива ")
(setq bit (getreal "Значение ключа в DFX паре "))

(setq zz 
	(vl-remove-if-not 
		(function (lambda(x)
			(= (car x) bit)
			   )
		)
	(entget(car(entsel)))
	)
)
);end defun





;поиск выделенного текста с последующей замейной или без нее
(defun c:Zod7 (/ bit ss1 qq ntxt flag)
(princ "\nпоиск выделенного текста с последующей замейной или без нее ")
(while	(null flag)
	(setq bit (car(nentsel "\nВыберите текст - образец")))
	(if	(equal(type(vl-catch-all-apply 'vlax-property-available-p(list(vl-catch-all-apply 'vlax-ename->vla-object (list bit))"textstring")))'SYM)
		(setq	flag	T
			bit	(vla-get-textstring (vlax-ename->vla-object bit)))
	)
)
(setq bit (mip_MTEXT_Unformat bit))
(princ "\nвыберете текст,в котором будет производиться поиск" )
(setq ss1 (ssget))
(mapcar
          '(lambda (name)(if (/= (vl-catch-all-apply 'vla-get-textstring (list(vlax-ename->vla-object name))) bit)
			     (ssdel name ss1)
			     
			 )
	   )
	
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1))) 
)
(princ (strcat "\n*********----  Найдено элементов " (rtos (sslength ss1) 2 0)"----*********") )
(sssetfirst nil ss1)
(princ)
(initget "Да Нет _Yes No")
(setq qq (getkword "\nИзменить текст? [Да/Нет]: "))
(if (= qq "Yes") 
(progn (setq ntxt (getstring "Введите новый текст: "))
;следующие 5 строк заменяют цикл
(mapcar '(lambda (name)
	(vla-put-textstring (vlax-ename->vla-object name) ntxt)
	  );end of lambda
	(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1))))) ;получение списка из номеров примитивов входящих в набор
);end of if
(sssetfirst nil ss1)
(princ)
);end defun






;замена точек на запятые и наоборот
(defun c:Zod8 (/ qq bit1 bit2 ss1)
(princ "\nзамена точек на запятые и наоборот ")
(initget "Запятые Точки _Z T")
(setq qq (getkword "\nЧто заменить в тексте? [Запятые/Точки]: "))
(cond
((= qq "Z")(setq bit1 46 bit2 44))
((= qq "T")(setq bit1 44 bit2 46))
)
(princ "\nВыбор объектов для замены ")
(setq ss1 (ssget))
(mapcar '(lambda (name)
	(if (= (vlax-property-available-p (vlax-ename->vla-object name) 'textstring) T)
	
		(vla-put-textstring (vlax-ename->vla-object name) 
			(vl-list->string 
				(subst bit1 bit2 (vl-string->list (vla-get-textstring (vlax-ename->vla-object name))))
			)
		)
	);end of if
	
	SS1
	);end of lambda
	
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1))) 
);end of mapcar
(princ)
);end of defun



;выравнивание однострочного текста по левому краю
(defun c:Zod9 ( / ss1)
(princ "\nвыравнивание однострочного текста по левому краю ")
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "TEXT")))
(vlax-for x ss1 (vla-put-Alignment x 0))
(vla-clear ss1)
(princ)
)


;аналог ortx
(defun c:ortx2 (/ pt1 x1 x2 x3 cl yy)
(princ "\nаналог ortx ")
(setq pt1 (entget(car(entsel))))
(setq x1 (car(cdr(assoc 10 pt1))))
(setq cl (cdr(assoc 8 pt1)))
(setq x2 (+ x1 200) x3 (- x1 200))
(setq ss1 (ssget "X" 
	(list 
		(cons 8 cl)
		'(-4 . "<AND")

			'(-4 . ">=,*,*")(list 10 x3 10 10)
			'(-4 . "<=,*,*")(list 10 x2 10 10)
		'(-4 . "AND>")
	)
		)
)
(mapcar '(lambda (name)
	(setq yy (caddr(assoc 10 (entget name))))
	(entmod(subst 	(list 10 x1 yy 0)
			(assoc 10 (entget name))
			(entget name)
	 	)
	 )
	 )
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
);end of defun



(defun c:remove-layer (/ del_layer_name z block_name w flag zz)
(princ "\nудаление слоя ")
(vlax-for layer_name (vla-get-layers(vla-get-activedocument(vlax-get-acad-object)))(setq z (append z (vl-remove-if 'null (list(if (snvalid (vla-get-name layer_name))(vla-get-name layer_name)))))))
(setq z (vl-sort z '< ))
(setq del_layer_name (car(loc:dwgru-get-user-dcl "Укажите слой,который хотите удалить" z nil)))

(if 	(equal(vla-get-lock(vla-item(vla-get-layers(vla-get-activedocument(vlax-get-acad-object)))del_layer_name)):vlax-true)
	(vla-put-lock (vla-item(vla-get-layers(vla-get-activedocument(vlax-get-acad-object))) del_layer_name)	0)
);_end_of_if



;удаление из модели и листов
(vlax-for model_obj (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
	(if	(equal (vla-get-Layer model_obj) del_layer_name)
		(vla-erase model_obj)
	);_end_of_if
);_end_of_vlax-for

(vlax-for paper_obj (vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
	(if	(equal (vla-get-Layer paper_obj) del_layer_name)
		(vla-erase paper_obj)
	);_end_of_if
);_end_of_vlax-for

;запись блоков где есть элементы этого слоя
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
	(vlax-for blk_object block_name
		(if 	(and
				(equal (vla-get-Layer blk_object) del_layer_name)
				(not (member (vla-get-name block_name) w))
			)
			(setq w (append w (list(vla-get-name block_name))))
		);_end_of_if
	);_end_of_vlax-for
);_end_of_vlax-for

(while w
(setq flag (loc:dwgru-get-user-dcl "Укажите блок, из которого хотите удалить" w t))
(if 	(null flag)
	(setq w nil)
	(progn
;удаление из блоков
	(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
		(vlax-for blk_object block_name
			(if 	(and	
					(equal (vla-get-Layer blk_object) del_layer_name)
					(member (vla-get-name block_name) flag)
				)
				(vla-erase blk_object)
			);_end_of_if
		);_end_of_vlax-for
	);_end_of_vlax-for
(setq w (vl-remove-if '(lambda(x)(member x flag)) w))
	)
)
)


	
(if 	(equal (vla-get-name(vla-get-ActiveLayer(vla-get-activedocument(vlax-get-acad-object)))) del_layer_name)
	(alert "Удаление активного слоя невозможно!")
(progn
(vl-catch-all-apply 'vla-erase (list(vla-item (vla-get-layers(vla-get-activedocument(vlax-get-acad-object))) del_layer_name)))


(vlax-for layer_name (vla-get-layers(vla-get-activedocument(vlax-get-acad-object)))
	(setq zz (append zz (vl-remove-if 'null (list(if (snvalid (vla-get-name layer_name))(vla-get-name layer_name)))))))
(if 	(member del_layer_name zz)
	(alert "Слой не удален")
(vla-regen (vla-get-activedocument(vlax-get-acad-object)) acallviewports)
)
)
);_end_of_if

);_end_of_defun





;выделение аналогичных динамических блоков , если вручную задавать то долно выглядеть вот так (zod10 '("HEB" "HEA" "IPE"))
(defun zod10 (d_block_name /  ss1 sel sel02)
(princ "выделение аналогичных динамических блоков")
(setq ss1 (ssadd))
(if (equal d_block_name '())
	(progn
		(vla-clear(setq sel02 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
		(pl:obj-filter-select-manual sel02 '((0 . "INSERT")))
		(vlax-for x sel02	(setq d_block_name (append d_block_name (list(get_name_block01 x)))))
	)
)
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual sel '((0 . "INSERT")))
(vlax-for x sel	(if (member (get_name_block01  x)d_block_name)
					(setq ss1(ssadd (vlax-vla-object->ename x)ss1))))
(sssetfirst nil ss1)
(princ)
)
(defun C:zod10 ()(zod10 '()))



;выделение аналогичных динамических блоков во всем чертеже, если вручную задавать то долно выглядеть вот так (zod100 '("HEB" "HEA" "IPE"))
(defun zod100 (d_block_name /  ss1 sel sel02)
(princ "выделение аналогичных динамических блоков")
(setq ss1 (ssadd))
(if (equal d_block_name '())
	(progn
		(vla-clear(setq sel02 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
		(pl:obj-filter-select-manual sel02 '((0 . "INSERT")))
		(vlax-for x sel02	(setq d_block_name (append d_block_name (list(get_name_block01 x)))))
	)
)
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-all sel '((0 . "INSERT")))
(vlax-for x sel	(if (member (get_name_block01  x)d_block_name)
					(setq ss1(ssadd (vlax-vla-object->ename x)ss1))))
(sssetfirst nil ss1)
(princ)
)
(defun C:zod100 ()(zod100 '()))



;обводит видовые экраны в модели
(defun c:zod11 (/ ss1 ss3 VptObj XofSet YofSet VptCen )
(princ "\nобводит видовые экраны в модели ")
(if	(equal (vl-catch-all-apply 'vla-get-MSpace (list(vla-get-activedocument(vlax-get-acad-object)))) :vlax-true)
	(vla-put-MSpace (vla-get-activedocument(vlax-get-acad-object))0)
)
(setq ss1	(ssget "_X" (list '(0 . "VIEWPORT")'(-4 . "<>")'(69 . 1))))
(setq ss1  	(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1))))
;(setq ss1	(vl-remove-if-not'(lambda (obj)(numberp (vl-catch-all-apply 'vla-get-LabelBlockId (list(vlax-ename->vla-object obj))) ))ss1))
(foreach vp ss1
(progn
	(if 	(not  (equal  (vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object))) (vla-item(vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))(cdr(assoc 410(entget vp))))	)	)
		(vla-put-ActiveLayout(vla-get-activedocument(vlax-get-acad-object)) (vla-item(vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))(cdr(assoc 410(entget vp)))))
	)
	(if	(equal	(vla-get-MSpace(vla-get-activedocument(vlax-get-acad-object))) :vlax-true)
		(vla-put-MSpace (vla-get-activedocument(vlax-get-acad-object))0)
	)
	(if	(equal	(vla-get-ViewportOn(vlax-ename->vla-object vp)) :vlax-false)
		(vla-put-ViewportOn(vlax-ename->vla-object vp)1)
	)
	(if 	(not  (equal (vl-catch-all-apply 'vla-get-ActivePViewport (list(vla-get-activedocument(vlax-get-acad-object)))) (vlax-ename->vla-object vp)  ) )
		(progn
			(vla-put-MSpace (vla-get-activedocument(vlax-get-acad-object))1)
			(vla-put-ActivePViewport (vla-get-activedocument(vlax-get-acad-object)) (vlax-ename->vla-object Vp))
		)
	)
	(vla-put-MSpace (vla-get-activedocument(vlax-get-acad-object))0)
	(setq ss3 nil)
	(if
		(assoc 340(entget vp))
		(setq ss3 (mapcar'cdr(vl-remove-if-not'(lambda (x) (= (car x) 10))(entget(cdr(assoc 340(entget vp)))))))
		(progn
			(setq 	VptObj 		(vlax-ename->vla-object Vp)
				XofSet	 	(/ (vla-get-Width VptObj) 2.0)
				YofSet		(/ (vla-get-Height VptObj) 2.0)
				VptCen		(vlax-get VptObj 'Center)
			);_ end_of_setq
			(setq ss3 (list
					(list (- (car VptCen) XofSet) (- (cadr VptCen) YofSet))
					(list (+ (car VptCen) XofSet) (- (cadr VptCen) YofSet))
					(list (+ (car VptCen) XofSet) (+ (cadr VptCen) YofSet))
					(list (- (car VptCen) XofSet) (+ (cadr VptCen) YofSet))
				)
			)

		);_ end_of_progn
	);_ end_of_if
	(setq ss3	(mapcar	'(lambda (pt)
		(setq 	pt (trans pt 3 2)
			pt (trans pt 2 1)
			pt (trans pt 1 0)
		) ;_ end of setq
				) ;_ end of lambda
			ss3
			) ;_ end of mapcar
	) ;_ end of setq
	(setq ss3 (mapcar '(lambda (zz) (mapcar '+ zz '(0 0))) ss3)) ;_Удаляем координату Z
	(setq ss3 (apply 'append ss3))
	(vla-put-closed
		(vla-addlightweightpolyline
			(vla-get-modelspace (vla-get-activedocument(vlax-get-acad-object)))
				(vlax-make-variant
					(vlax-safearray-fill
						(vlax-make-safearray
							vlax-vbdouble
								(cons 0 (- (length ss3) 1))
						) ;_ end of vlax-Make-SafeArray
					ss3
					) ;_ end of vlax-SafeArray-Fill
				) ;_ end of vlax-Make-Variant
		) ;_ end of vla-AddLightWeightPolyline
	:vlax-true
	) ;_ end of vla-Put-Closed
);_ end_of_progn
);_ end_of_foreach
(vla-put-ActiveSpace (vla-get-activedocument(vlax-get-acad-object)) 1)
(vla-Regen (vla-get-activedocument(vlax-get-acad-object)) 1)
(princ)
);_end_of_defun













;выставляет видовые экраны
(defun c:Zod12 (/ flag z w VE1)
(princ "\nвыставляет видовые экраны ")
(vla-put-ActiveSpace (vla-get-activedocument(vlax-get-acad-object)) 1)

(while 	(not flag)
	(if 	(setq z (getpoint (strcat "\nУказано    " (itoa(length w)) "    точек  ")))
		(setq w (append w (list z)))
		(setq flag T)
	)
) ;_ end of while

(while w
	(setvar "CTAB" (car(loc:dwgru-get-user-dcl "Укажите лист" (vl-remove "Model" (ttc-layouts-list)) nil)))
	(if	(equal	(vla-get-MSpace(vla-get-activedocument(vlax-get-acad-object))) :vlax-true)
		(vla-put-MSpace (vla-get-activedocument(vlax-get-acad-object))0)
	)
	(if	(>(length(vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget "_X" (list '(0 . "VIEWPORT")(cons 410 (getvar "CTAB"))))))))1)
		(mapcar '(lambda(x)
			(progn
				(if	(equal (vla-get-ViewportOn(vlax-ename->vla-object x)) :vlax-false)
					(vla-put-ViewportOn(vlax-ename->vla-object x)1)
				)
				(if	(equal (vla-get-DisplayLocked(vlax-ename->vla-object x)) :vlax-true)
					(vla-put-DisplayLocked(vlax-ename->vla-object x)0)
				)
			)
			)
		
		(vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget "_X" (list '(0 . "VIEWPORT")'(-4 . "<>")'(69 . 1)(cons 410 (getvar "CTAB")))))))
		)
	)
	(if	(>(length(vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget "_X" (list '(0 . "VIEWPORT")(cons 410 (getvar "CTAB"))))))))1)
		(progn
			(if	(>(length(vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget "_X" (list '(0 . "VIEWPORT")(cons 410 (getvar "CTAB"))))))))2)
				(progn	
					(while  (/=(length VE1)1)
						(princ "\nВыберите видовой экран, с которым будете сопоставлять модель (…1 шт…) ")
						(setq VE1 (vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget '((0 . "VIEWPORT")))))))
	
					)
					(if 	(not  (equal (vl-catch-all-apply 'vla-get-ActivePViewport (list(vla-get-activedocument(vlax-get-acad-object)))) (vlax-ename->vla-object(car VE1))  ) )
						(progn
							(vla-put-MSpace (vla-get-activedocument(vlax-get-acad-object))1)
							(vla-put-ActivePViewport (vla-get-activedocument(vlax-get-acad-object)) (vlax-ename->vla-object(car VE1)))
							(vla-put-MSpace (vla-get-activedocument(vlax-get-acad-object))0)
							(setq VE1 nil)
						)
					)
				)
			)
		
			(setq p3 (trans (getpoint (strcat "\nОсталось    " (itoa(length w)) "    точек  ")) 3 2))
			(vla-put-MSpace (vla-get-activedocument(vlax-get-acad-object))1)
			(vl-cmdf "_.pan" (car w) p3)
			(vla-put-MSpace (vla-get-activedocument(vlax-get-acad-object))0)
			(setq w (vl-remove (car w) w))
		)
		(alert "В этом листе нет видовых экранов")
	)
)
(princ)
(vla-put-ActiveSpace (vla-get-activedocument(vlax-get-acad-object)) 1)
) ;_ end of defun


(defun ttc-layouts-list ()
  (or doc (setq doc (vla-get-activedocument (vlax-get-acad-object))))
	(mapcar 'vla-get-name
		(vl-sort
			((lambda (/ res)
			   (vlax-for item (vla-get-layouts doc)
				 (setq res (cons item res))
				 ) ;_ end of vlax-for
			   ) ;_ end of lambda
			 )
			'(lambda (a b)
			   (< (vla-get-taborder a) (vla-get-taborder b))
			 ) ;_ end of lambda
		) ;_ end of vl-sort
	)
) ;_ end of defun


(defun c:Zod122 ( / spisok_layots 1_list flag pt1 pt2 pt_z vp_1 ss2 vp_obj_1 pt3)
(princ "\nвыставляет видовые экраны ")
(vla-put-ActiveSpace (vla-get-activedocument(vlax-get-acad-object)) 1)
(setq spisok_layots(cdr(ttc-layouts-list)) 1_list(car(loc:dwgru-get-user-dcl "Укажите лист" spisok_layots nil)))
(while (/= (car spisok_layots) 1_list)(setq spisok_layots (cdr spisok_layots)))
(while 	(null flag)
	(if 	(setq pt_z (getpoint (strcat "\nУкажите точку для листа " (car spisok_layots))))
			(if (setq ss2(ssget "_X" (list '(0 . "VIEWPORT")(cons 410 (car spisok_layots)))))
				(progn
					(vla-put-ActiveLayout (vla-get-activedocument(vlax-get-acad-object))(vla-item 
						(vla-get-layouts(vla-get-activedocument (vlax-get-acad-object))) (car spisok_layots)))
					(setq vp_1 (ssname ss2 0) vp_obj_1 (vlax-ename->vla-object vp_1))
					(vla-GetBoundingBox vp_obj_1 'pt1 'pt2)
					(setq pt1 (vlax-safearray->list pt1) pt2 (vlax-safearray->list pt2) pt3 (trans(list (car pt1)(cadr pt2) 0) 3 2))
					(vla-put-MSpace (vla-get-activedocument(vlax-get-acad-object))1)
					(vl-cmdf "_.pan" pt_z pt3)
					(vla-put-MSpace (vla-get-activedocument(vlax-get-acad-object))0)
					(vla-put-ActiveSpace (vla-get-activedocument(vlax-get-acad-object)) 1)
					(if (cdr spisok_layots)(setq spisok_layots(cdr spisok_layots))(setq flag T))
				)
			)
		(setq flag T)
	)
)
(princ)
)


;сравнивает свойства 2 объектов
(defun c:zod13 (/ lst1 lst2 car_lst1 car_lst2 ign_lst1 ign_lst2 double_lst1 double_lst2 lst11 lst22 x y rev_car_lst1 rev_car_lst2)
(princ "\nсравнивает свойства 2 объектов ")
(setq lst1 (entget(car(entsel))))
(setq lst2 (entget(car(entsel))))
(setq car_lst1 (mapcar 'car lst1))
(setq car_lst2 (mapcar 'car lst2))

(setq x 0)
(repeat (length lst1)
	(if	(not (member (nth x car_lst1) car_lst2))
		(setq ign_lst1(append ign_lst1 (list(nth x lst1))))
	)
	(setq x(1+ x))
)
(setq lst1 (vl-remove-if '(lambda(x1)(member (car x1) (mapcar 'car ign_lst1))) lst1))
(setq y 0)
(repeat (length lst2)
	(if	(not (member (nth y car_lst2) car_lst1))
		(setq ign_lst2(append ign_lst2 (list(nth y lst2))))
	)
	(setq y(1+ y))
)
(setq lst2 (vl-remove-if '(lambda(y1)(member (car y1) (mapcar 'car ign_lst2))) lst2))



(setq rev_car_lst1 (reverse(mapcar 'car lst1)))
(setq rev_car_lst2 (reverse(mapcar 'car lst2)))
(setq x 0)
(repeat (length lst1)
	(if	(member (nth x (reverse(mapcar 'car lst1))) (cdr(member (nth x (reverse(mapcar 'car lst1))) rev_car_lst1)))
		(setq double_lst1(append double_lst1 (list(nth x (reverse lst1)))))
		(setq lst11(append lst11 (list(nth x (reverse lst1)))))
	)
	(setq x(1+ x))
	(setq rev_car_lst1 (cdr rev_car_lst1))
)
(setq y 0)
(repeat (length lst2)
	(if	(member (nth y (reverse(mapcar 'car lst2))) (cdr(member (nth y (reverse(mapcar 'car lst2))) rev_car_lst2)))
		(setq double_lst2(append double_lst2 (list(nth y (reverse lst2)))))
		(setq lst22(append lst22 (list(nth y (reverse lst2)))))
	)
	(setq y(1+ y))
	(setq rev_car_lst2 (cdr rev_car_lst2))
)

(setq lst11 	(vl-sort lst11 		'(lambda(c d)(< (car c) (car d) ))))
(setq lst22 	(vl-sort lst22		'(lambda(c d)(< (car c) (car d) ))))
(setq ign_lst1 	(vl-sort ign_lst1 	'(lambda(c d)(< (car c) (car d) ))))
(setq ign_lst2 	(vl-sort ign_lst2	'(lambda(c d)(< (car c) (car d) ))))
(mapcar	
	'(lambda (a b)
	       (if	(equal (cdr a) (cdr b))
			(progn
				(setq lst11 (vl-remove a lst11))
				(setq lst22 (vl-remove b lst22))
			)
		)
	);_ end of lambda
lst11 lst22
)
(list (mapcar 'list lst11 lst22) "*******" ign_lst1 "*******" ign_lst2  "------" double_lst1 "------" double_lst2)
);_end_of_defun


;сравнивает свойства 2 объектов
(defun zod14 (ename1 ename2 / lst1 lst2 car_lst1 car_lst2 ign_lst1 ign_lst2 double_lst1 double_lst2 lst11 lst22 x y rev_car_lst1 rev_car_lst2)
(princ "\nсравнивает свойства 2 объектов ")
(setq lst1 (entget ename1))
(setq lst2 (entget ename2))
(setq car_lst1 (mapcar 'car lst1))
(setq car_lst2 (mapcar 'car lst2))

(setq x 0)
(repeat (length lst1)
	(if	(not (member (nth x car_lst1) car_lst2))
		(setq ign_lst1(append ign_lst1 (list(nth x lst1))))
	)
	(setq x(1+ x))
)
(setq lst1 (vl-remove-if '(lambda(x1)(member (car x1) (mapcar 'car ign_lst1))) lst1))
(setq y 0)
(repeat (length lst2)
	(if	(not (member (nth y car_lst2) car_lst1))
		(setq ign_lst2(append ign_lst2 (list(nth y lst2))))
	)
	(setq y(1+ y))
)
(setq lst2 (vl-remove-if '(lambda(y1)(member (car y1) (mapcar 'car ign_lst2))) lst2))



(setq rev_car_lst1 (reverse(mapcar 'car lst1)))
(setq rev_car_lst2 (reverse(mapcar 'car lst2)))
(setq x 0)
(repeat (length lst1)
	(if	(member (nth x (reverse(mapcar 'car lst1))) (cdr(member (nth x (reverse(mapcar 'car lst1))) rev_car_lst1)))
		(setq double_lst1(append double_lst1 (list(nth x (reverse lst1)))))
		(setq lst11(append lst11 (list(nth x (reverse lst1)))))
	)
	(setq x(1+ x))
	(setq rev_car_lst1 (cdr rev_car_lst1))
)
(setq y 0)
(repeat (length lst2)
	(if	(member (nth y (reverse(mapcar 'car lst2))) (cdr(member (nth y (reverse(mapcar 'car lst2))) rev_car_lst2)))
		(setq double_lst2(append double_lst2 (list(nth y (reverse lst2)))))
		(setq lst22(append lst22 (list(nth y (reverse lst2)))))
	)
	(setq y(1+ y))
	(setq rev_car_lst2 (cdr rev_car_lst2))
)

(setq lst11 	(vl-sort lst11 		'(lambda(c d)(< (car c) (car d) ))))
(setq lst22 	(vl-sort lst22		'(lambda(c d)(< (car c) (car d) ))))
(setq ign_lst1 	(vl-sort ign_lst1 	'(lambda(c d)(< (car c) (car d) ))))
(setq ign_lst2 	(vl-sort ign_lst2	'(lambda(c d)(< (car c) (car d) ))))
(mapcar	
	'(lambda (a b)
	       (if	(equal (cdr a) (cdr b))
			(progn
				(setq lst11 (vl-remove a lst11))
				(setq lst22 (vl-remove b lst22))
			)
		)
	);_ end of lambda
lst11 lst22
)
(list (mapcar 'list lst11 lst22) "*******" ign_lst1 "*******" ign_lst2  "------" double_lst1 "------" double_lst2)
);_end_of_defun







;аналог мультилидера
(defun c:zod15 (/ e1 n flag p1 p2 p3)
(princ "\nаналог мультилидера ")
(vl-cmdf "_.pasteclip" pause "")

(setq e1 (entlast))
;(setq n (vla-get-count(vla-get-activeselectionset(vla-get-ActiveDocument(vlax-get-acad-object)))))
(setq n 4)
(while 	(not flag)

(if	(vl-catch-all-error-p(vl-catch-all-apply 'vlax-curve-getStartPoint (list (vlax-ename->vla-object e1))))
	(setq e1 (handent(DecToHex(1-(HexToDec(cdr(assoc 5(entget e1))))))))
	(setq flag T)
)
(if	(equal (- (HexToDec(cdr(assoc 5(entget(entlast))))) (1+ n))  (HexToDec(cdr(assoc 5(entget e1)))))
	(setq flag T)
)

) ;_ end of while

(if 	(not(vl-catch-all-error-p(vl-catch-all-apply 'vlax-curve-getStartPoint (list (vlax-ename->vla-object e1)))))
(progn
	(setq p1(getpoint "Укажите первую точку :"))

	(if 	(<
			(DISTANCE (vlax-curve-getStartPoint(vlax-ename->vla-object e1)) p1)
			(DISTANCE (vlax-curve-getEndPoint(vlax-ename->vla-object e1)) p1)
		)
		(setq p3(vlax-curve-getStartPoint(vlax-ename->vla-object e1)))
		(setq p3(vlax-curve-getEndPoint(vlax-ename->vla-object e1)))
	)
	(setq p2(getpoint "Укажите вторую точку :"))
	(vl-cmdf "_.pline" p1 p3 p2 "")
)
	(alert "Среди 2 элементов нету линии")
)
);_end_of_defun


;аналог мультилидера
(defun c:zod155 (/ e1 n flag p1 p2 p3)
(princ "\nаналог мультилидера ")
(vl-cmdf "_.pasteclip" pause "")

(setq e1 (entlast))
;(setq n (vla-get-count(vla-get-activeselectionset(vla-get-ActiveDocument(vlax-get-acad-object)))))
(setq n 4)
(while 	(not flag)

(if	(vl-catch-all-error-p(vl-catch-all-apply 'vlax-curve-getStartPoint (list (vlax-ename->vla-object e1))))
	(setq e1 (handent(DecToHex(1-(HexToDec(cdr(assoc 5(entget e1))))))))
	(setq flag T)
)
(if	(equal (- (HexToDec(cdr(assoc 5(entget(entlast))))) (1+ n))  (HexToDec(cdr(assoc 5(entget e1)))))
	(setq flag T)
)

) ;_ end of while

(if 	(not(vl-catch-all-error-p(vl-catch-all-apply 'vlax-curve-getStartPoint (list (vlax-ename->vla-object e1)))))
(progn
	(setq p1(getpoint "Укажите первую точку :"))

	(if 	(<
			(DISTANCE (vlax-curve-getStartPoint(vlax-ename->vla-object e1)) p1)
			(DISTANCE (vlax-curve-getEndPoint(vlax-ename->vla-object e1)) p1)
		)
		(setq p3(vlax-curve-getStartPoint(vlax-ename->vla-object e1)))
		(setq p3(vlax-curve-getEndPoint(vlax-ename->vla-object e1)))
	)
	(vl-cmdf "_.line" p1 p3 "")
)
	(alert "Среди 2 элементов нету линии")
)
);_end_of_defun







;соединяет линии в полилинию(на аксонометрии из бывших мультилидеров)
(defun c:zod16 (/ e1 ss1 ss2)
(princ "\nсоединяет линии в полилинию(на аксонометрии из бывших мультилидеров) ")
(setq e1 (cdr(assoc 8(entget(car(entsel))))))
(setq ss1 (ssget "_X" (list '(0 . "LINE")(cons 8 e1))))
(setq ss2  	(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1))))
(mapcar
          '(lambda (name)(if 	(or 	(equal (angtos(vla-get-angle (vlax-ename->vla-object name))0 0) "0")
					(equal (angtos(vla-get-angle (vlax-ename->vla-object name))0 0) "180")
				)
			     	(ssdel name ss1)
			     
			 )
	   )
	ss2 
)

(vl-cmdf "_.PEDIT" "m" ss1 "" "_Yes" "_Join" "0" "")
(princ)
)



;выделяет все что длиной меньше bit
(defun c:Zod17 (/ bit ss1 ss2)
(princ "\nвыделяет все что длиной меньше bit ")
(setq bit (getreal "Укажите значение,меньше этой длины будут выделены все отрезки ") ss2 (ssadd))
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 (list (cons 410 (getvar "CTAB"))))
(vlax-for x ss1
(if 
	(equal
		(vl-catch-all-apply '< 
			(list(vl-catch-all-apply 'vlax-curve-getDistAtParam 
				(list	x
					(vl-catch-all-apply 'vlax-curve-getEndParam(list x))))
			bit)
		)
	t)
	(setq ss2(ssadd (vlax-vla-object->ename x)ss2))
)
(sssetfirst nil ss2)
(vla-clear ss1)
(princ (strcat "\n*********----  Найдено элементов " (rtos (sslength ss2) 2 0)"----*********") )
(princ)
) ;_ end of defun
)





;выравнивание однострочного текста по центру
(defun c:Zod18 ( / ss1 pt1)
(princ "\n выравнивание однострочного текста по центру \n")
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "TEXT")))
(vlax-for x ss1
	(setq pt1(vla-get-InsertionPoint x))
	(vla-put-Alignment  x 10)
	(vla-move	x
			(vla-get-InsertionPoint x)
			pt1
	)
)
(vla-clear ss1)
(princ)
);end of defun




(defun c:Zod19 (/ att1 bit ss1 att_text)
(princ "\nИщет в чертеже атрибуты (в таком же блоке) с таким же tag и заменяет")
(setq att1 (car(nentsel)) att_text (cdr(assoc 2(entget att1))))
(setq bit (getstring "Введите новое значение атрибута"))
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-all ss1 (list(assoc 2(entget(cdr(assoc 330(entget att1)))))))
(vlax-for x ss1
	(mapcar '(lambda(attrib1)
		(if	(equal (vla-get-TagString attrib1) att_text)(vla-put-TextString attrib1 bit))
			)
	(vlax-safearray->list(vlax-variant-value(vla-GetAttributes x)))
	)
)
(princ)
)



(defun c:Zod199 (/ att1 bit ss1 att_text)
(princ "\nИщет в чертеже атрибуты (в таком же блоке) с таким же tag и заменяет")
(setq att1 (car(nentsel)) att_text (cdr(assoc 2(entget att1))))
(setq bit (vla-get-textstring(vlax-ename->vla-object (car(entsel)))))
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-all ss1 (list(assoc 2(entget(cdr(assoc 330(entget att1)))))))
(vlax-for x ss1
	(mapcar '(lambda(attrib1)
		(if	(equal (vla-get-TagString attrib1) att_text)(vla-put-TextString attrib1 bit))
			)
	(vlax-safearray->list(vlax-variant-value(vla-GetAttributes x)))
	)
)
(princ)
)

(defun c:Zod1999 (/ att1 bit ss1 att_text)
(princ "\nИщет в чертеже атрибуты (в таком же блоке) с таким же tag и заменяет")
(setq att1 (car(nentsel)) att_text (cdr(assoc 2(entget att1))))
(setq bit (getstring "Введите новое значение атрибута"))
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "INSERT")))
(vlax-for x ss1
	(mapcar '(lambda(attrib1)
		(if	(equal (vla-get-TagString attrib1) att_text)(vla-put-TextString attrib1 bit))
			)
	(vlax-safearray->list(vlax-variant-value(vla-GetAttributes x)))
	)
)
(vla-clear ss1)
(princ)
)



(defun c:zod20 (/ obj ss1 intpt number1 number2 flag)
(princ "\nпродляет или обрезает линии до пересечения с выбранной")
(setq	number1		0
	number2		0)
(while	(null flag)
	(setq obj (car(entsel "\nВыберете отрезок или полилинию")))
	(if	(wcmatch(cdr(assoc 0(entget obj)))"LINE,LWPOLYLINE")
		(setq	flag	T
			obj	(vlax-ename->vla-object obj))
	)
)
(setq ss1(vl-remove-if 'listp (mapcar 'cadr (ssnamex(ssget '((0 . "LINE")))))))
(setq ss1 (vl-remove obj(mapcar 'vlax-ename->vla-object ss1)))
(mapcar '(lambda(name)
	(if	(not	
			(or
				(equal (getvektor (vlax-curve-getStartPoint obj) (vlax-curve-getEndPoint obj)) (getvektor (vlax-curve-getStartPoint name) (vlax-curve-getEndPoint name)))
				(equal (getvektor (vlax-curve-getStartPoint obj) (vlax-curve-getEndPoint obj)) (getvektor (vlax-curve-getEndPoint name) (vlax-curve-getStartPoint name)))				
			)
		)
		(progn
			(setq intpt(vlax-safearray->list(vlax-variant-value(vla-IntersectWith obj name 3))))
			(if
				(and
					(>=(car intpt)(car(getvar "EXTMIN")))
					(<=(car intpt)(car(getvar "EXTMAX")))
					(>=(cadr intpt)(cadr(getvar "EXTMIN")))
					(<=(cadr intpt)(cadr(getvar "EXTMAX")))
				)
				(if	(>
						(DISTANCE (vlax-curve-getStartPoint name) intpt)
						(DISTANCE (vlax-curve-getEndPoint name) intpt)
					)
					(vla-put-EndPoint name (vlax-3d-point intpt))
					(vla-put-StartPoint name (vlax-3d-point intpt))
				)
				(setq number1(1+ number1))
			)
		)
	(setq number2(1+ number2))
	)
	)
ss1)

(if (> number1 0) (princ (strcat "****линии,пересекающиеся за пределами чертежа - " (rtos number1 2 0)) )(princ))
(if (> number2 0) (princ (strcat "****линии,паралелльные с исходной - " (rtos number2 2 0)) )(princ))
(princ)
)

(defun getvektor (pt1 pt2 / tmp); единичный вектор направления заданный точками pt1 pt2.
(setq tmp (mapcar '- pt1 pt2))
(if (not (equal tmp '(0.0 0.0 0.0)))
(mapcar '(lambda (x) (/ (float x) (apply 'max (mapcar 'abs tmp)))) tmp)
);end of if
);end of getvektor




(defun c:Zod21 (/ Layout_name)
(princ "\nвыставляет на всех листах галочку на масштабировать веса линий")
(vlax-for Layout_name (vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))
	(vla-put-ScaleLineweights Layout_name :vlax-true)
);_end_of_vlax-for
)

;|
(defun Zod22 (/ Layout_name)
(princ "\nвыставляет на всех листах галочку на масштабировать веса линий, сохраняет и выходит")
(vlax-for Layout_name (vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))
	(vla-put-ScaleLineweights Layout_name :vlax-false)
);_end_of_vlax-for
(vla-save(vla-get-activedocument(vlax-get-acad-object)))
(vla-quit(vlax-get-acad-object))
)
|;


(defun zod23(/ ss1 layer_name01 )
(setq layer_name01 "_Viewport")
(if	(null(tblobjname "Layer" layer_name01))
	(vla-add(vla-get-Layers(vla-get-activedocument(vlax-get-acad-object)))layer_name01)
)
(if	(/= 0 (cdr(assoc 70(entget(tblobjname "Layer" layer_name01)))))
	(entmod(subst '(70 . 0)(assoc 70 (entget(tblobjname "Layer" layer_name01))) (entget(tblobjname "Layer" layer_name01))))
)
(if	(/= 103 (cdr(assoc 62(entget(tblobjname "Layer" layer_name01)))))
	(entmod(subst '(62 . 103)(assoc 62 (entget(tblobjname "Layer" layer_name01))) (entget(tblobjname "Layer" layer_name01))))
)
(if	(/= 0 (cdr(assoc 290(entget(tblobjname "Layer" layer_name01)))))
	(entmod(subst '(290 . 0)(assoc 290 (entget(tblobjname "Layer" layer_name01))) (entget(tblobjname "Layer" layer_name01))))
)
(setq ss1	(ssget "_X" (list '(0 . "VIEWPORT")'(-4 . "<>")'(69 . 1))))
(if (equal nil ss1)(exit))
(setq ss1  	(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1))))
(mapcar  '(lambda (name)(vla-put-Layer(vlax-ename->vla-object name) layer_name01))ss1)
(princ "\nСоздает новый стой _Viewport и видовые экраны переносит в этот слой")
(princ)
)

(defun C:zod23 ()(zod23))

;(if	(ssget "_X" (list '(0 . "VIEWPORT")'(-4 . "<>")'(69 . 1)))
;	(c:zod23)
;)



(defun var->xy (var)(vlax-3d-point(mapcar '+ '(0 0) (vlax-safearray->list(vlax-variant-value var)))))

(defun xyz->xy0 (coor)(append (mapcar '+ '(0 0) coor)'(0)))

(defun varxyz->varxy0 (safe / spisok1)
(if (null(vl-catch-all-error-p(vl-catch-all-apply  'vlax-safearray->list  (list(vlax-variant-value safe)))))
	(progn
		(setq n 0 spisok1(mapcar '(lambda(x)(progn(setq n(1+ n))(if (zerop(rem n 3)) 0 x)))(vlax-safearray->list(vlax-variant-value safe))))
		(vlax-make-variant
			(vlax-safearray-fill
				(vlax-make-safearray
					vlax-vbdouble
						(cons 0 (- (length spisok1) 1))
				) ;_ end of vlax-Make-SafeArray
			spisok1
			) ;_ end of vlax-SafeArray-Fill
		) ;_ end of vlax-Make-Variant
	)
)
)

(defun c:zod24 (/ ss1)
(princ "\nвыставляет всем объектам координату Z=0")
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(vla-selectonscreen ss1 nil nil)
(vlax-for x ss1	
	(cond	((vlax-property-available-p x 'InsertionPoint)
			(vla-put-InsertionPoint
				x
				(var->xy(vla-get-InsertionPoint x))
			)
		)
		((vlax-property-available-p  x 'center)
			(vla-put-center
				x
				(var->xy(vla-get-center x))
			)
		)
		((vlax-property-available-p  x 'BasePoint)
			(progn
				(vla-put-BasePoint
					x
					(var->xy(vla-get-BasePoint x))
				)
				(vla-put-SecondPoint
					x
					(var->xy(vla-get-SecondPoint x))
				)
			)
		)
		((vlax-property-available-p  x 'StartPoint)
			(progn
				(vla-put-StartPoint
					x
					(var->xy(vla-get-StartPoint x))
				)
				(vla-put-EndPoint
					x
					(var->xy(vla-get-EndPoint x))
				)
			)
		)
		((vlax-property-available-p  x 'elevation)
			(vla-put-elevation x 0)
		)
		((vlax-property-available-p  x 'ControlPoints)
			(progn
				(vla-put-ControlPoints
					x
					(varxyz->varxy0(vla-get-ControlPoints x))
				)
				(if (not(listp (varxyz->varxy0(vla-get-FitPoints x))))
					(vla-put-FitPoints
						x
						(varxyz->varxy0(vla-get-FitPoints x))
					)
				)
			)
		)
	)
	)
(princ)
)

;|

(defun c:l (/ pt1 pt2)
(princ "\nРисует линию с координатами Z=0")
(if	(setq pt1(getpoint))
	(progn	(setq pt1(vlax-3d-point(setvar "LASTPOINT"(mapcar '+ '(0 0) pt1))))
		(setvar "errno" 0)
		(while (equal (getvar "errno") 0)
			(if	(setq pt2(getpoint(getvar "lastpoint")))
				(progn	(setq pt2(vlax-3d-point(mapcar '+ '(0 0)pt2)))
					(vla-addline 
						(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
							(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
							(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
						)
						pt1
						pt2
					)
					(setq pt1(vlax-3d-point(setvar "LASTPOINT" (vlax-safearray->list(vlax-variant-value pt2)))))
				)
				(setvar "errno" 1)
			)
		)
	)
)
(princ)
)

|;


(defun c:addla (/ name_la la_ename)
(princ "\nСоздает слой на основе указанного и делает его активным")
(setq name_la (strcat (cdr(assoc 8(entget(car(entsel))))) "_длины"))
(if	(not (tblobjname "layer" name_la))
	(setq	la_ename	(vla-add	(vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))
				name_la
			)
	)
	(setq	la_ename	(vla-item (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))) name_la))
)
(if	(not(equal 9 (vla-get-Lineweight la_ename)))
	(vla-put-Lineweight la_ename 9)
)
(if	(not(equal 250 (cdr(assoc 62 (entget(vlax-vla-object->ename la_ename))))))
	(entmod(subst '(62 . 250) (assoc 62 (entget(vlax-vla-object->ename la_ename))) (entget(vlax-vla-object->ename la_ename))))
)
(if	(not(equal la_ename (vla-get-ActiveLayer(vla-get-activedocument(vlax-get-acad-object)))))
	(vla-put-ActiveLayer(vla-get-activedocument(vlax-get-acad-object))la_ename)
)
)


;| пояснение 
блок может принадлежать одному слою а его атрибуты другому,при Burst атрибуты лягут на той слой который будет указан в _eattedit -> properties -> layer
это происходит когда блок с атрибутами перетакскивается из одного чертежа и лежат на том слое который нам не нужен ,с помощью этой команды и _filter
перекладываем атрибуты в другой слой и отчищаем чертеж от ненужного слоя
|;
(defun c:zod25 ( / atrib_1 spis_1 ss1)
(princ "\nЗадает всем аттрибутам свойства выделееного атрибута")
(if	(equal	(assoc 0(entget(setq atrib_1 (car(nentsel "\nВыберете объект,слой которго будет присвоен атрибутам блока"))))) 
		'(0 . "ATTRIB")
	)
	(progn
(setq atrib_1(vlax-ename->vla-object atrib_1))
(setq spis_1	(loc:dwgru-get-user-dcl "Укажите свойства атрибута которые хотите передать другим объектам"
		'("Высота" "Слой" "Тип линии" "Толщина линии" "Угол наклона" "Степень сжатия" "Стиль текста" "Угол поворота")
		t)
)
(setq ss1(mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex(ssget "_X" '((0 . "INSERT"))))))))
(mapcar
	'(lambda (name)	(if	(listp(vl-catch-all-apply 'vlax-safearray->list (list(vlax-variant-value(vla-GetAttributes name)))))
				(mapcar	'(lambda (namess2)
(if (member "Высота" 		spis_1) (vla-put-Height		namess2	(vla-get-Height		atrib_1)))
(if (member "Слой" 		spis_1) (vla-put-Layer		namess2	(vla-get-Layer		atrib_1)))
(if (member "Тип линии" 		spis_1) (vla-put-Linetype		namess2	(vla-get-Linetype		atrib_1)))
(if (member "Толщина линии" 	spis_1) (vla-put-Lineweight		namess2	(vla-get-Lineweight	atrib_1)))
(if (member "Угол наклона" 		spis_1) (vla-put-ObliqueAngle	namess2	(vla-get-ObliqueAngle	atrib_1)))
(if (member "Степень сжатия" 	spis_1) (vla-put-ScaleFactor		namess2	(vla-get-ScaleFactor	atrib_1)))
(if (member "Стиль текста" 		spis_1) (vla-put-StyleName		namess2	(vla-get-StyleName		atrib_1)))
(if (member "Угол поворота" 	spis_1) (vla-put-Rotation		namess2	(vla-get-Rotation		atrib_1)))
					)
				(vlax-safearray->list(vlax-variant-value(vla-GetAttributes name)))
				)
			)
	)
	ss1
)
	)
)
(princ)
)






(defun c:zod26 ( / ss1 ss3 )
(princ "\nВыделяет динамические блоки в параметрах которых выставлен Custom")
(setq	ss3 	(ssadd))
(princ "\nВыберете блоки")
(setq ss1(mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex(ssget '((0 . "INSERT"))))))))
(mapcar
	'(lambda (name)	(if	(listp(vl-catch-all-apply 'vlax-safearray->list (list(vlax-variant-value(vla-GetDynamicBlockProperties name)))))
				(mapcar	'(lambda (namess2)
					(if	(equal "Custom" (vlax-variant-value(vla-get-value namess2)))
						(setq ss3 (ssadd (vlax-vla-object->ename name) ss3))
					)
					)
					(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties name)))
				)
			)
	)
	ss1
)
(sssetfirst nil ss3)
(princ)
)



;создание массива из 1 элемента vla-object
(defun sozdat_massiv (vla_object / )(vlax-safearray-fill(vlax-make-safearray vlax-vbObject '(0 . 0)) (list vla_object)))

(defun c:zod27 ( /  ss1 skorost rashod razmeri Att_s a b diametr spis1 ss2)
(princ "\nСчитает скорость в блоках")
(setq	ss2 (ssadd)	spis1	0)
(princ "\nУкажите блоки c атрибутами")
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "INSERT")))
(vlax-for att_name ss1
	(if (= (vla-get-EffectiveName att_name) "_V_dannie_u4astka")
		(if	(= (vl-catch-all-apply 'length(list(setq Att_s (vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetAttributes att_name )))))))3)
			(progn	
				(mapcar	'(lambda (y)
					(cond
						((wcmatch (strcase (vla-get-TagString y)) "СКОРОСТЬ")		(setq skorost  y	))
						((wcmatch (strcase (vla-get-TagString y)) "РАСХОД")			(setq rashod (text_to_number(vla-get-TextString y))))
						((wcmatch (strcase (vla-get-TagString y)) "СЕЧЕНИЕ")		(setq razmeri (text_to_spisok(vla-get-TextString y)2)))
					))
				Att_s)
				(if	(zerop (cadr razmeri))
					(setq diametr (car razmeri ))
					(setq a (car razmeri) b (cadr razmeri))
				)
				(if	diametr 
					(setq zn_skorost (rtos (/ (* 4000 rashod) 3.6 pi diametr diametr)2 2))
					(setq zn_skorost (rtos(/ (* 1000 rashod) 3.6 a b )2 2))
				)
				(if	(equal (vla-get-TextString skorost) zn_skorost)
					(setq spis1 (1+ spis1))
							(progn
								(vla-put-TextString skorost zn_skorost)
								(setq ss2 (ssadd (vlax-vla-object->ename att_name) ss2))
							)
				)
				(setq skorost nil rashod nil razmeri nil a nil b nil diametr nil zn_skorost nil Att_s nil)
			)
		)
	)
)
(vla-clear ss1)
(prompt (strcat "\n*********---- "(rtos spis1  2 0)"     значений скорости верны----*********") )
(sssetfirst nil ss2)
(princ)
)



(defun c:zod28 ( / ish_din w dyn_Property_name ss1 ss2)
(princ "\nИщет нужное значение в динамических свойствах блоков")
(setq ss2	(ssadd))
(if	(vlax-method-applicable-p (setq ish_din(vlax-ename->vla-object(car(entsel))))'GetDynamicBlockProperties)
(if	(listp(setq ish_din(vl-catch-all-apply 'vlax-safearray->list (list(vlax-variant-value(vla-GetDynamicBlockProperties ish_din))))))
(progn	(mapcar '(lambda (att_name)
		(setq w (append w (list(vla-get-PropertyName att_name))))
		)
		ish_din
	)
	(setq dyn_Property_name (car(loc:dwgru-get-user-dcl "Укажите свойство в котором будет производиться поиск" (vl-remove "Origin" w) nil)))
	(setq val (getstring "Искомое значение: "))
	(setq ss1(mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex(ssget '((0 . "INSERT"))))))))
	(mapcar '(lambda (dyn_name)
		(if	(vlax-method-applicable-p dyn_name 'GetDynamicBlockProperties)
			(if	(listp(vl-catch-all-apply 'vlax-safearray->list (list(vlax-variant-value(vla-GetDynamicBlockProperties dyn_name)))))
				(mapcar '(lambda (x)
					(if	(and	(equal (vla-get-PropertyName x) dyn_Property_name)
							(equal (vlax-variant-value(vla-get-Value x)) val)
						)
						(setq ss2 (ssadd (vlax-vla-object->ename dyn_name)ss2))
					)
					)
					(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties dyn_name)))
				)
				
			)
		
		)
		)
		ss1
	)
)))
(sssetfirst nil ss2)
)

(defun c:zod288 ( / ish_din w dyn_Property_name ss1 ss2)
(princ "\nИщет нужное значение в динамических свойствах блоков")
(setq ss2	(ssadd))
(if	(vlax-method-applicable-p (setq ish_din(vlax-ename->vla-object(car(entsel))))'GetDynamicBlockProperties)
(if	(listp(setq ish_din(vl-catch-all-apply 'vlax-safearray->list (list(vlax-variant-value(vla-GetDynamicBlockProperties ish_din))))))
(progn	(mapcar '(lambda (att_name)
		(setq w (append w (list(vla-get-PropertyName att_name))))
		)
		ish_din
	)
	(setq dyn_Property_name (car(loc:dwgru-get-user-dcl "Укажите свойство в котором будет производиться поиск" (vl-remove "Origin" w) nil)))
	(setq val (getreal "Искомое значение: "))
	(setq ss1(mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex(ssget '((0 . "INSERT"))))))))
	(mapcar '(lambda (dyn_name)
		(if	(vlax-method-applicable-p dyn_name 'GetDynamicBlockProperties)
			(if	(listp(vl-catch-all-apply 'vlax-safearray->list (list(vlax-variant-value(vla-GetDynamicBlockProperties dyn_name)))))
				(mapcar '(lambda (x)
					(if	(and	(equal (vla-get-PropertyName x) dyn_Property_name)
							(equal (vlax-variant-value(vla-get-Value x)) val)
						)
						(setq ss2 (ssadd (vlax-vla-object->ename dyn_name)ss2))
					)
					)
					(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties dyn_name)))
				)
				
			)
		
		)
		)
		ss1
	)
)))
(sssetfirst nil ss2)
)


(defun c:zod29 ( / blk_ename bl_ename ent_d ent_z Shirina Visota Diametr minpoint maxpoint p1 p2 p3 line_p1 intpt int_p3 int_p4 spisok1)
(if (null bit_zod29)(setq bit_zod29 (getreal "\n введите высоту текста\n")))
(if	(equal (vla-get-IsDynamicBlock (setq blk_ename(vlax-ename->vla-object(car(setq ent_z(entsel "\nУкажите блок" )))))):vlax-true)
	(progn
(mapcar '(lambda(z) 
	(cond	((equal(vla-get-PropertyName z) "Shirina")		(setq Shirina	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Visota")		(setq Visota	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Diametr")		(setq Diametr	(vlax-variant-value(vla-get-Value z)))		)
			
	)	)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties blk_ename)))
)
(if Shirina 	(if (equal (type Shirina) 'STR)		(setq Shirina(atoi Shirina))))
(if Visota 		(if (equal (type Visota) 'STR)		(setq Visota(atoi Visota))))
(if Diametr 	(if (equal (type Diametr) 'STR)		(setq Diametr1(atoi Diametr))))
(vla-put-ScaleFactor
(setq etext
	(vla-addtext 	(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
				(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
				(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
			)

		(if	Diametr
			(strcat "%%C" (rtos Diametr 2 0))
			(strcat (rtos Shirina  2 0)"x"(rtos Visota 2 0))
		)
		(vlax-3d-point(getpoint "\nУкажите точку вставки текста")) 
		bit_zod29
	)
)
0.8)
(vla-GetBoundingBox etext 'MinPoint 'MaxPoint)
(setq 	minpoint	(vlax-safearray->list minpoint)
	maxpoint	(vlax-safearray->list maxpoint)
	maxpoint	(list(+(car maxpoint) 50)(-(cadr minpoint)(/(* bit_zod29 20)250))(caddr maxpoint))
	minpoint	(list(- (car minpoint) 50)(-(cadr minpoint)(/(* bit_zod29 20)250))(caddr minpoint))
	ent_d	(cadr ent_z)
)
(if 	(<
		(DISTANCE minpoint ent_d)
		(DISTANCE maxpoint ent_d)
	)
	(setq p1 minpoint 	p2 maxpoint)
	(setq p1 maxpoint 	p2 minpoint)
)
(setq line_p1
(vla-addline 
	(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
		(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
		(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
	)
	(vlax-3d-point p1)
	(vlax-3d-point ent_d)
)
)
(vla-addline 
	(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
		(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
		(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
	)
	(vlax-3d-point p1)
	(vlax-3d-point p2)
)
(setq intpt(vlax-safearray->list(vlax-variant-value(vla-IntersectWith line_p1 blk_ename 3))))
(cond	(	(equal (length intpt) 3)	(setq p3 (list(car intpt)(cadr intpt)(caddr intpt)))	)
		(	(equal (length intpt) 6)	(setq int_p3(list(nth 0 intpt)(nth 1 intpt)(nth 2 intpt))	int_p4 (list(nth 3 intpt)(nth 4 intpt)(nth 5 intpt))))
)
(if int_p3
	(if 	(<
			(DISTANCE p1 int_p3)
			(DISTANCE p1 int_p4)
		)
		(setq p3 int_p4)
		(setq p3 int_p3)
	)
)
(if (setq spisok1(ssnamex (ssget "_F" (list p1 (polar p1(angle p1 p3)(* 2(DISTANCE p1 p3)))))))
	(if	(setq spisok1(car(vl-remove-if-not '(lambda(x)(equal(cadr x) (car ent_z)))spisok1)))
		(setq p3(cadr(car(vl-remove-if-not 'listp spisok1))))
	)
)
(vla-put-EndPoint line_p1 (vlax-3d-point p3))
))
(princ)
)




(defun c:zod2999()(setq bit_zod29 (getreal "\n введите высоту текста\n")))


(defun c:zod299( / blk_ename bl_ename ent_d Shirina_v Visota_v minpoint maxpoint p1 p2 p3 line_p1 intpt int_p3 int_p4 Diametr_v )
(if	(vlax-method-applicable-p (setq blk_ename(vlax-ename->vla-object(car(setq ent_d(entsel "\nУкажите блок" )))))'GetDynamicBlockProperties)
(if	(listp(setq bl_ename (vl-catch-all-apply 'vlax-safearray->list (list(vlax-variant-value(vla-GetDynamicBlockProperties blk_ename ))))))
(progn	(mapcar '(lambda (att_name)
		(cond	(	(equal (vla-get-PropertyName att_name) "Shirina")	(if
											(equal (type(vlax-variant-value(vla-get-Value att_name)))(type ""))
											(setq Shirina_v (vlax-variant-value(vla-get-Value att_name)))
											(setq Shirina_v (rtos(vlax-variant-value(vla-get-Value att_name))2 0))
										)
																		)
			(	(equal (vla-get-PropertyName att_name) "Visota")	(if
											(equal (type(vlax-variant-value(vla-get-Value att_name)))(type ""))
											(setq Visota_v (vlax-variant-value(vla-get-Value att_name)))
											(setq Visota_v (rtos(vlax-variant-value(vla-get-Value att_name))2))
										)
																		)
			(	(equal (vla-get-PropertyName att_name) "Diametr")	(if
											(equal (type(vlax-variant-value(vla-get-Value att_name)))(type ""))
											(setq Diametr_v (vlax-variant-value(vla-get-Value att_name)))
											(setq Diametr_v (rtos(vlax-variant-value(vla-get-Value att_name))2))
										)
																		)
		)
		)
		bl_ename 
	)
(vla-put-ScaleFactor
(setq etext
	(vla-addtext 	(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
				(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
				(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
			)

		(if	Diametr_v
			(strcat "%%C" Diametr_v )
			(strcat Shirina_v "x" Visota_v)
		)
		(vlax-3d-point(getpoint "\nУкажите точку вставки текста")) 
		180
	)
)
0.8)
)))
(princ)
)


;Разворот блоков с имнем _Ot_Stoyaki_g на 180 и пернос по оси X на 200 единиц
(defun c:zod30 (/ ss1)
(princ "\nРазворот блоков с имнем _Ot_Stoyaki_g на 180 и пернос по оси X на 200 единиц ")
(setq ss1(mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex(ssget '((0 . "INSERT")(2 . "_Ot_Stoyaki_g"))))))))
(mapcar '(lambda (blk_name)
	(vla-rotate blk_name (vla-get-InsertionPoint blk_name)pi)
	(vla-move	blk_name 
			(vla-get-InsertionPoint blk_name)
			(vlax-3d-point	(list	(+(car(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint blk_name))))200)
						(cadr(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint blk_name))))
					)
			)
	)
	)
ss1
)
)




(defun c:zod31 (/ ss1)
(princ "\nУдаление текста без содержания ")
;|
(if	(setq ss1 (ssget "_X" '((0 . "TEXT,MTEXT")(1 . ", , \\~, \\~ ,\\~ "))))
	(progn  (setq ss1(mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))))
		(mapcar '(lambda(x)(vla-erase x))ss1)
	)
)
(princ)
)
|;
(sssetfirst nil(setq ss1 (ssget "_X" '((0 . "TEXT,MTEXT")(1 . ", , \\~, \\~ ,\\~ ")))))
(if	ss1
	(prompt (strcat "\n*********---- Выделено "(rtos (sslength ss1)  2 0)"    текстов без содержания----*********"))
)
)



(defun c:zod32 ()
(princ "\nСоздание текстового стиля СПДС ")
(if	(null(tblobjname "Style" "spds"))
	(vla-add(vla-get-TextStyles(vla-get-activedocument(vlax-get-acad-object))) "Spds")
)
(vla-put-fontfile(vlax-ename->vla-object(tblobjname "style" "spds"))"spds.shx")
(vla-put-width(vlax-ename->vla-object(tblobjname "style" "spds"))0.85)
(vla-put-ObliqueAngle(vlax-ename->vla-object(tblobjname "style" "spds"))(angtof "15" 4))
(princ)
)


;(c:zod32)


(defun c:zod33 ( / atrib_1 spis_1 ss1)
(princ "\nЗадает всем аттрибутам свойства выделееного атрибута")
(if	(equal	(assoc 0(entget(setq atrib_1 (car(nentsel "\nВыберете объект,слой которго будет присвоен атрибутам блока"))))) 
		'(0 . "ATTRIB")
	)
	(progn
(setq atrib_1(vlax-ename->vla-object atrib_1))
(setq spis_1	(loc:dwgru-get-user-dcl "Укажите свойства атрибута которые хотите передать другим объектам"
		'("Высота" "Слой" "Тип линии" "Толщина линии" "Угол наклона" "Степень сжатия" "Стиль текста" "Угол поворота")
		t)
)
(setq ss1(mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex(ssget "_X" '((0 . "INSERT"))))))))
(mapcar
	'(lambda (name)	(if	(listp(vl-catch-all-apply 'vlax-safearray->list (list(vlax-variant-value(vla-GetAttributes name)))))
				(mapcar	'(lambda (namess2)
(if (member "Высота" 		spis_1) (vla-put-Height		namess2	(vla-get-Height		atrib_1)))
(if (member "Слой" 		spis_1) (vla-put-Layer		namess2	(vla-get-Layer		atrib_1)))
(if (member "Тип линии" 		spis_1) (vla-put-Linetype		namess2	(vla-get-Linetype		atrib_1)))
(if (member "Толщина линии" 	spis_1) (vla-put-Lineweight		namess2	(vla-get-Lineweight	atrib_1)))
(if (member "Угол наклона" 		spis_1) (vla-put-ObliqueAngle	namess2	(vla-get-ObliqueAngle	atrib_1)))
(if (member "Степень сжатия" 	spis_1) (vla-put-ScaleFactor		namess2	(vla-get-ScaleFactor	atrib_1)))
(if (member "Стиль текста" 		spis_1) (vla-put-StyleName		namess2	(vla-get-StyleName		atrib_1)))
(if (member "Угол поворота" 	spis_1) (vla-put-Rotation		namess2	(vla-get-Rotation		atrib_1)))
					)
				(vlax-safearray->list(vlax-variant-value(vla-GetAttributes name)))
				)
			)
	)
	ss1
)
	)
)
(princ)
)




(defun c:zod34 (/ pt1 pt2 MinPoint MaxPoint old_printer)
(princ "\nв текущем лейауте выставлянт нормально лист")
(setq pt1
	(vlax-safearray-fill
		(vlax-make-safearray
			vlax-vbdouble
				(cons 0 (- (length '(0 0)) 1))
		) ;_ end of vlax-Make-SafeArray
	'(0 0)
	) ;_ end of vlax-SafeArray-Fill
)
(vla-put-PlotOrigin(vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object)))pt1)
(setq pt1(mapcar '+ (getvar "EXTMax") '(0 0)))
(vlax-for	ss_prim
	(vla-get-PaperSpace(vla-get-activedocument(vlax-get-acad-object)))
	(vla-GetBoundingBox ss_prim 'MinPoint 'MaxPoint)
	(if	(not	(equal	
				"AcDbViewport" 
				(vla-get-ObjectName ss_prim)
		)	)
		(setq pt1 (list	(min (car pt1)(car(vlax-safearray->list minpoint)))	(min (cadr pt1)(cadr(vlax-safearray->list minpoint)))	))
	)
)
(vlax-for	ss_prim
	(vla-get-PaperSpace(vla-get-activedocument(vlax-get-acad-object)))
	(vla-move ss_prim (vlax-3d-point pt1) (vlax-3d-point '(0 0)))
)
(setq pt2(mapcar '+ (getvar "EXTMin") '(0 0)))
(vlax-for	ss_prim
	(vla-get-PaperSpace(vla-get-activedocument(vlax-get-acad-object)))
	(vla-GetBoundingBox ss_prim 'MinPoint 'MaxPoint)
	(if	(not	(equal	
				"AcDbViewport" 
				(vla-get-ObjectName ss_prim)
		)	)
		(setq pt2 (list	(max (car pt2)(car(vlax-safearray->list maxpoint)))	(max (cadr pt2)(cadr(vlax-safearray->list maxpoint)))	))
	)
)
(setq pt2
	(vlax-safearray-fill
		(vlax-make-safearray
			vlax-vbdouble
				(cons 0 (- (length pt2) 1))
		) ;_ end of vlax-Make-SafeArray
	pt2
	) ;_ end of vlax-SafeArray-Fill
)
(setq pt1
	(vlax-safearray-fill
		(vlax-make-safearray
			vlax-vbdouble
				(cons 0 (- (length '(0 0)) 1))
		) ;_ end of vlax-Make-SafeArray
	'(0 0)
	) ;_ end of vlax-SafeArray-Fill
)
(vla-SetWindowToPlot(vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object))) pt1 pt2)
(if	(not(equal 4(vla-get-PlotType(vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object))))))
	(vla-put-PlotType(vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object))) 4)
)
(vla-put-PlotType(vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object))) 5)
(vla-regen(vla-get-activedocument(vlax-get-acad-object)) acActiveViewport)
(vla-ZoomExtents(vlax-get-acad-object))
)



(defun c:zod35 ( / pt1 pt2)
(princ "\nпроводит линию по граниыам обекта")
(vla-GetBoundingBox (vlax-ename->vla-object (car(entsel))) 'pt1 'pt2)
					(vla-addline 
						(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
							(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
							(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
						)
						pt1
						pt2
					)
)




(defun c:zod36 (/ pt1 pt2 MinPoint MaxPoint old_printer)
(princ "\nВо всех лейаутах  выставлянт нормально лист")
(vlax-for	ss_lay	(vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))
(if	(not(equal (vla-get-name ss_lay) "Model"))
	(progn
(if	(not(equal	(vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object))) ss_lay))
	(vla-put-ActiveLayout(vla-get-activedocument(vlax-get-acad-object)) ss_lay)
)
(if	(not(equal	(vla-get-ConfigName ss_lay)"Adobe PDF.pc3"))
	(progn
		(setq old_printer	(vla-get-ConfigName ss_lay))
		(vla-put-ConfigName ss_lay "Adobe PDF.pc3")
	)
)
(setq pt1
	(vlax-safearray-fill
		(vlax-make-safearray
			vlax-vbdouble
				(cons 0 (- (length '(-3.175 -3.175)) 1))
		) ;_ end of vlax-Make-SafeArray
	'(-3.175 -3.175)
	) ;_ end of vlax-SafeArray-Fill
)
(vla-put-PlotOrigin ss_lay pt1)
(setq pt1(mapcar '+ (getvar "EXTMax") '(0 0)))
(vlax-for	ss_prim
	(vla-get-PaperSpace(vla-get-activedocument(vlax-get-acad-object)))
	(vla-GetBoundingBox ss_prim 'MinPoint 'MaxPoint)
	(if	(not	(equal	
				"AcDbViewport" 
				(vla-get-ObjectName ss_prim)
		)	)
		(setq pt1 (list	(min (car pt1)(car(vlax-safearray->list minpoint)))	(min (cadr pt1)(cadr(vlax-safearray->list minpoint)))	))
	)
)
(vlax-for	ss_prim
	(vla-get-PaperSpace(vla-get-activedocument(vlax-get-acad-object)))
	(vla-move ss_prim (vlax-3d-point pt1) (vlax-3d-point '(0 0)))
)
(setq pt2(mapcar '+ (getvar "EXTMin") '(0 0)))
(vlax-for	ss_prim
	(vla-get-PaperSpace(vla-get-activedocument(vlax-get-acad-object)))
	(vla-GetBoundingBox ss_prim 'MinPoint 'MaxPoint)
	(if	(not	(equal	
				"AcDbViewport" 
				(vla-get-ObjectName ss_prim)
		)	)
		(setq pt2 (list	(max (car pt2)(car(vlax-safearray->list maxpoint)))	(max (cadr pt2)(cadr(vlax-safearray->list maxpoint)))	))
	)
)
(setq pt2
	(vlax-safearray-fill
		(vlax-make-safearray
			vlax-vbdouble
				(cons 0 (- (length pt2) 1))
		) ;_ end of vlax-Make-SafeArray
	pt2
	) ;_ end of vlax-SafeArray-Fill
)
(setq pt1
	(vlax-safearray-fill
		(vlax-make-safearray
			vlax-vbdouble
				(cons 0 (- (length '(0 0)) 1))
		) ;_ end of vlax-Make-SafeArray
	'(0 0)
	) ;_ end of vlax-SafeArray-Fill
)
(vla-SetWindowToPlot ss_lay pt1 pt2)
(if	(not(equal 4(vla-get-PlotType ss_lay)))
	(vla-put-PlotType ss_lay 4)
)
(vla-put-PlotType ss_lay 5)
(if	 old_printer
	(vla-put-ConfigName ss_lay old_printer)
)
(vla-regen(vla-get-activedocument(vlax-get-acad-object)) acActiveViewport)
(vla-ZoomExtents(vlax-get-acad-object))
);end_of_progn
);end_of_if
);end_of_vlax-for
)


(defun c:zod37 ( /   lst)
(princ "\nСписок примитивов на чертеже")
(SETQ ent (entnext))
(while ent
	(if	(not(member (cdr(assoc 0(entget ent))) lst))
		(setq lst (append lst(list(cdr(assoc 0(entget ent))))))
	)
	(SETQ ent (entnext ent))
)
(print (acad_strlsort lst))(princ)
(princ)
)


(defun zod38 (zz / ss1 ss2 i)
(princ "\nВыделение нужно объекта по dxf паре")
(setq	i	0
	ss2 	(ssadd)
	ss1 	(ssget "X")
)
(repeat (sslength ss1)
(if	(equal (cdr(assoc 0(entget(ssname ss1 i)))) zz)
	(ssadd (ssname ss1 i) ss2)
)
(setq i (1+ i))
)
(sssetfirst nil ss2)
(princ)
)



(defun c:zod39 ( / i ss1 ss2 ss3 zz lst)
(princ "\nУдаляет из чертежа определееные типы примитивов")
(setq	i	0
	ss3 	(ssget "_X")
)
(repeat (sslength ss3)
(if	(not(member (cdr(assoc 0(entget(ssname ss3 i)))) lst))
	(setq lst (append lst(list(cdr(assoc 0(entget(ssname ss3 i)))))))
)
(setq i (1+ i))
)
(setq zz(car(loc:dwgru-get-user-dcl "Укажите нужный примитив" (acad_strlsort lst) nil)))
(setq	i	0
	ss2 	(ssadd)
	ss1 	(ssget "_X")
)
(repeat (sslength ss1)
(if	(equal (cdr(assoc 0(entget(ssname ss1 i)))) zz)
	(ssadd (ssname ss1 i) ss2)
)
(setq i (1+ i))
)
(repeat (sslength ss2)
(entdel (ssname ss2 0))
(ssdel (ssname ss2 0) ss2)
(princ)
)
)


(defun zod40 (zz / i ss1 ss2)
(princ "\nИщет текст в атирибутах блоков и выделяет блоки")
(setq	i	0
	ss2 	(ssadd)
	ss1	(ssget "_X" '((0 . "INSERT")))
)
(repeat (sslength ss1)
(if	(numberp(vl-catch-all-apply 'length(list(setq Att_s (vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetAttributes (vlax-ename->vla-object (ssname ss1 i))))))))))
	(progn	(mapcar	'(lambda (name)
			(if	(equal (vla-get-TextString name) zz)
				(ssadd (ssname ss1 i) ss2)
			)
			)
		Att_s)
	)
)
(setq i (1+ i))
)
(sssetfirst nil ss2)
(princ)
)


(defun c:zod41 ()
(/ (getreal "Введите длину ") 5.0)
)

;Берет данные из размеров в виде 1,561 и проставляет путем добавления их к базовой высоте"
(defun c:zod41 ( / visota )
(if (not baz_visota) (setq baz_visota (getreal "Введите базовую высоту ")))
(setq visota (rtos (+ baz_visota (vla-get-Measurement(vlax-ename->vla-object(car(entsel)))))2 2))
(vla-put-TextString(vlax-ename->vla-object(car(entsel))) visota)
(princ)
)

(defun c:zod42 ( / )
(setq baz_visota (getreal "Введите базовую высоту "))
)


(defun c:Zod43 (/ bit1 bit2 ss1)
(princ "\nвыделяет все что длиной больше bit1 и меньше bit2")
(setq bit1 (getreal "Укажите меньшее значение"))
(setq bit2 (getreal "Укажите большее значение"))
(setq ss1 (ssget "_X" '((0 . "LWPOLYLINE")(-4 . "<OR")(8 . "_111")(8 . "_222")(-4 . "OR>"))))
(mapcar
	'(lambda (name)
		(if 
			(not(and
				(<
					(vla-get-Length (vlax-ename->vla-object name))
					bit2
				)
				(>
					(vla-get-Length (vlax-ename->vla-object name))
					bit1
				)
			))
			(ssdel name ss1)
		)
	)
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(sssetfirst nil ss1)
(princ)
)



(defun c:Zod44 (/ bit1 bit2 ss1)
(princ "\nвыделяет блоки которые соответсвуют длине и высоте,точность 0,2")
(setq bit1 (getreal "\nУкажите длину"))
(setq bit2 (getreal "\nУкажите высоту"))
(setq ss1 (ssget "_X" '((0 . "INSERT")(8 . "_111"))))
(mapcar
	'(lambda (name)
		(if 
			(not(and
				(<
					(vlax-variant-value(vla-get-Value(car(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties (vlax-ename->vla-object name)))))))
					(+ bit1 0.2)
				)
				(>
					(vlax-variant-value(vla-get-Value(car(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties (vlax-ename->vla-object name)))))))
					(- bit1 0.2)
				)
				(<
					(vlax-variant-value(vla-get-Value(cadr(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties (vlax-ename->vla-object name)))))))
					(+ bit2 0.2)
				)
				(>
					(vlax-variant-value(vla-get-Value(cadr(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties (vlax-ename->vla-object name)))))))
					(- bit2 0.2)
				)
			))
			(ssdel name ss1)
		)
	)
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(sssetfirst nil ss1)
(princ)
)



(defun c:zod45 ()
(princ "\nВыделете область в которой находятся линии")
(setq ss1 (ssget  '((0 . "LINE,LWPOLYLINE,XLINE,RAY,SPLINE"))))
(princ)
)

(defun c:zod46 ( / Att_s Dlina1 Visota1 flag сс1)
(while (null flag)
	(if(setq cc1(car (entsel "Укажите блок,который будет использоваться ")))
		(if	(equal (assoc 0 (entget cc1)) '(0 . "INSERT"))
			(if	(equal(vla-get-IsDynamicBlock(vlax-ename->vla-object cc1)):vlax-true)
				(if	(numberp(vl-catch-all-apply 'length(list(vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetAttributes (vlax-ename->vla-object cc1))))))))
					(if	(and	(> (vl-catch-all-apply 'length(list(setq Att_s(vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetDynamicBlockProperties (vlax-ename->vla-object cc1))))))))1)
							(mapcar	'(lambda (name)
								(cond
									((equal (vla-get-PropertyName name) "Dlina")			(setq Dlina1 (atof(rtos(vlax-variant-value(vla-get-value name))2 0)))		)
									((equal (vla-get-PropertyName name) "Visota")			(setq Visota1 (atof(rtos(vlax-variant-value(vla-get-value name))2 0)))		)
								)
								)
							Att_s)
							Dlina1
							Visota1
							(setq d_block_name(vl-catch-all-apply 'vla-get-EffectiveName (list(vlax-ename->vla-object cc1))))
						)
	  					(setq flag t)
	  					(alert "Какой то элемент отсутсвует в блоке")
					)
					(alert "Выбран блок без текстового атрибута")
				)
				(alert "Выбран нединамический блок ")
			)
	  		(alert "Выбран не блок ")
		)
	(alert "Ничего не выбрано")	  
	) ;_ end of if
)
(princ)
)


(defun c:zod47 ( / min_dist pt1 pt2 pt3 pt4 din_at)
(setq min_dist (distance (getvar "EXTMAX")(getvar "EXTMin")))
(setq pt (getpoint "Укажите точку"))
(setq	pt1 (cadr (cadddr(car(ssnamex (ssget "_F" (list pt (list (car pt)(+ min_dist(cadr pt))(caddr pt))))))))
		pt2 (cadr (cadddr(car(ssnamex (ssget "_F" (list pt (list (+ min_dist(car pt))(cadr pt)(caddr pt))))))))
		pt3 (cadr (cadddr(car(ssnamex (ssget "_F" (list pt (list (car pt)(-(cadr pt)min_dist)(caddr pt))))))))
		pt4 (cadr (cadddr(car(ssnamex (ssget "_F" (list pt (list (-(car pt)min_dist)(cadr pt)(caddr pt))))))))
		pt2(inters pt1 (list (1+(car pt1))(cadr pt1)(caddr pt1)) pt2 (list (car pt2)(1+(cadr pt2))(caddr pt2)) nil)
		pt1(inters pt3 (list (1+(car pt3))(cadr pt3)(caddr pt3)) pt4 (list (car pt4)(1+(cadr pt4))(caddr pt4)) nil)
		din_at
			(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties
				(vla-InsertBlock	(vla-get-modelspace (vla-get-activedocument(vlax-get-acad-object)))
							(vlax-safearray-fill(vlax-make-safearray vlax-vbdouble(cons 0 (- (length pt1) 1)))pt1)
							d_block_name
							1.0 1.0 1.0 0.0)
			)))
)
(mapcar	'(lambda (x)
			(cond
					((equal (vla-get-Propertyx x) "Dlina")			(vla-put-value x (vlax-make-variant (-(car pt2)(car pt1))))			)
					((equal (vla-get-Propertyx x) "Visota")			(vla-put-value x (vlax-make-variant (-(cadr pt2)(cadr pt1))))			)
			)
		)
din_at)
(princ)
)


;для подсчета панелей,составления их спецификации и проставление нумерации на чертеже
(defun c:zod48 ( / ss1 Dlina1 Visota1 spisok1 spisok2 spisok3 spisok4 spisok6 spisok7 spisok8 f n zz flag d_block_name sum_dlin sum_visot bit)
(setq	n 1	zz 0)
(setq bit (getreal "Введите значение допуска ,для объединения по длине и ширине окон "))
(while (null flag)
	(if(setq cc1(car (entsel "Укажите нужный блок: ")))
		(if	(equal (assoc 0 (entget cc1)) '(0 . "INSERT"))
			(if	(equal(vla-get-IsDynamicBlock(vlax-ename->vla-object cc1)):vlax-true)
				(if	(numberp(vl-catch-all-apply 'length(list(vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetAttributes (vlax-ename->vla-object cc1))))))))
					(if	(and	(> (vl-catch-all-apply 'length(list(setq Att_s(vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetDynamicBlockProperties (vlax-ename->vla-object cc1))))))))1)
							(mapcar	'(lambda (name)
								(cond
									((equal (vla-get-PropertyName name) "Dlina")			(setq Dlina1 (atof(rtos(vlax-variant-value(vla-get-value name))2 0)))		)
									((equal (vla-get-PropertyName name) "Visota")			(setq Visota1 (atof(rtos(vlax-variant-value(vla-get-value name))2 0)))		)
								)
								)
							Att_s)
							Dlina1
							Visota1
							(setq d_block_name(vl-catch-all-apply 'vla-get-EffectiveName (list(vlax-ename->vla-object cc1))))
						)
	  					(setq flag t)
	  					(alert "Какой то элемент отсутсвует в блоке")
					)
					(alert "Выбран блок без текстового атрибута")
				)
				(alert "Выбран нединамический блок ")
			)
	  		(alert "Выбран не блок ")
		)
	(alert "Ничего не выбрано")	  
	) ;_ end of if
) ;_ end of while защита от дурака
(if	cc1
(progn
(setq ss1 (ssget "_X" (list '(0 . "INSERT")(cons 410 (getvar "CTAB"))(assoc 8(entget cc1)))))
(setq Dlina1	nil	Visota1	nil)
(mapcar
          '(lambda (name)
	(if
		(/= 
			(vl-catch-all-apply 'vla-get-EffectiveName (list(vlax-ename->vla-object name)))
			d_block_name
		)
		(ssdel name ss1)
	)
            )
           (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1))) 
);выделение блоков в данном рабочем поле по названию и лежащих с исходным в одинаковом слое
(setq ss1(mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))))
(mapcar '(lambda (att_name)
	(if	(> (vl-catch-all-apply 'length(list(setq Att_s(vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetDynamicBlockProperties att_name)))))))0)
		(progn
			(mapcar	'(lambda (name)
				(cond
					((equal (vla-get-PropertyName name) "Dlina")			(setq Dlina1 (atof(rtos(vlax-variant-value(vla-get-value name))2 0)))		)
					((equal (vla-get-PropertyName name) "Visota")			(setq Visota1 (atof(rtos(vlax-variant-value(vla-get-value name))2 0)))		)
				)
				)
			Att_s
			)
			(setq spisok1(append spisok1 (list(list dlina1 visota1 ))))
		)
	)
	)
ss1
);формирование списка по длине и высоте
(while (>(vl-list-length spisok1)0)
	(setq	sum_dlin	0	sum_visot 0)
	(setq spisok2	(vl-remove-if-not
		(function (lambda(x)
				(and	
					(< (car x) (+(car(nth 0 spisok1))bit))
					(> (car x) (-(car(nth 0 spisok1))bit))
					(< (cadr x) (+(cadr(nth 0 spisok1))bit))
					(> (cadr x) (-(cadr(nth 0 spisok1))bit))
				)
			)
		)
			spisok1)
	)
  	(mapcar '(lambda (x)(setq sum_dlin(+(car x)sum_dlin)))spisok2)
    	(mapcar '(lambda (x)(setq sum_visot(+(cadr x)sum_visot)))spisok2)
	(setq spisok3(append spisok3 (list(append (list(/ sum_dlin (vl-list-length spisok2)) (/ sum_visot (vl-list-length spisok2))) (list(vl-list-length spisok2))))))
	(setq spisok1	(vl-remove-if
		(function (lambda(x)
				(and	
					(< (car x) (+(car(nth 0 spisok1))bit))
					(> (car x) (-(car(nth 0 spisok1))bit))
					(< (cadr x) (+(cadr(nth 0 spisok1))bit))
					(> (cadr x) (-(cadr(nth 0 spisok1))bit))
				)
			)
		)
			spisok1)
	)
)
(setq spisok3	(vl-sort 
			(vl-sort spisok3 
					'(lambda(x1 x2)
						(< (cadr x1)(cadr x2))
					)
			)
			'(lambda(x1 x2)
			(< (car x1)(car x2))
			)
		)
);объединение и сортировка списка
(mapcar '(lambda (x)
	(progn
	(cond	(	(= 1(strlen (rtos n 2 0)))		(setq spisok4(append spisok4(list(append(list(strcat  "П00" (rtos n 2 0)))x))))		)
		(	(= 2(strlen (rtos n 2 0)))		(setq spisok4(append spisok4(list(append(list(strcat  "П0" (rtos n 2 0)))x))))		)
		(	(= 3(strlen (rtos n 2 0)))		(setq spisok4(append spisok4(list(append(list(strcat  "П" (rtos n 2 0)))x))))		)
	)
	(setq n (1+ n))
	)
	)
spisok3);добавляет надпись "П000" к списку
(setq Dlina1	nil	Visota1	nil)
(mapcar '(lambda (att_name)
	(if	(> (vl-catch-all-apply 'length(list(setq Att_s(vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetDynamicBlockProperties att_name)))))))0)
		(progn
			(mapcar	'(lambda (name)
				(cond
					((equal (vla-get-PropertyName name) "Dlina")			(setq Dlina1 (atof(rtos(vlax-variant-value(vla-get-value name))2 0)))		)
					((equal (vla-get-PropertyName name) "Visota")			(setq Visota1 (atof(rtos(vlax-variant-value(vla-get-value name))2 0)))		)
				)
				)
			Att_s
			)
			(mapcar	'(lambda (x)
				(if	(and	(< Dlina1	(+(cadr x) bit))
						(> Dlina1 (- (cadr x) bit))
						(< Visota1 (+ (caddr x) bit))
						(> Visota1 (- (caddr x) bit))
					)
					(vla-put-TextString(car(vlax-safearray->list(vlax-variant-value(vla-GetAttributes att_name))))(car x))
				))
			spisok4)
		)
	))
ss1);проставление нумерации элементов
(mapcar '(lambda (att_name)
	(if	(> (length(list(setq Att_s(vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetAttributes att_name)))))))0)
		(mapcar	'(lambda (name)
			(setq spisok6(append spisok6(list (vla-get-TextString  name))))
			)
		Att_s)
	))
ss1);проверка элементов и прощет количества нумерации
(while (>(vl-list-length spisok6)0)
	(setq spisok7	(vl-remove-if-not
		(function (lambda(x)
				(equal x (car spisok6))
			)
		)
			spisok6)
	)
	(setq spisok8(append spisok8 (list(append (list(nth 0 spisok6)) (list(vl-list-length spisok7))))))
	(setq spisok6	(vl-remove-if
		(function (lambda(x)
				(equal x (car spisok6))
			)
		)
			spisok6)
	)
)
(setq spisok8(vl-sort spisok8 '(lambda(x1 x2)(< (car x1)(car x2)))));объединение одинаковых элементов и сортировка
(setq f (open "c:\\111.txt" "w"))
(write-line (strcat "Наименование" "\t" "длина" "\t" "высота" "\t" "кол" "\t" "недостаток на чертеже")f)
(mapcar '(lambda(x y)
	(if	(equal (cadddr x)(cadr y))
		(write-line (strcat (car x)"\t""\t"(rtos(cadr x)2 0)"\t"(rtos(caddr x)2 0)"\t"(rtos(cadddr x)2 0))f)
		(write-line (strcat (car x)"\t""\t"(rtos(cadr x)2 0)"\t"(rtos(caddr x)2 0)"\t"(rtos(cadddr x)2 0)"\t"(rtos(- (cadddr x)(cadr y)))2 0)f)
	)
	)
spisok4 spisok8)
(mapcar '(lambda(x)(setq zz(+ zz(+(cadddr x)))))spisok4)
(write-line (strcat "Итого" "\t" "\t" "\t" "\t"(rtos zz 2 0))f)
(close f);запись в файл
(princ)
)
(alert "Фигня какая то"))
)


;выделение нужного текста по всему/не всему  чертежу
(defun zod490 ( text_f flag / ss1 ss3)
(if (equal "" text_f ) (setq text_f (strcase(getstring "\n Введите искомый текст  \n"))))
(if (equal flag 1)
	(setq ss1 (ssget "_X" 	'((0 . "TEXT,MTEXT,MULTILEADER"))))
	(setq ss1 (ssget 		'((0 . "TEXT,MTEXT,MULTILEADER"))))
)
(setq	ss3 	(ssadd)
		text_f		(strcat "*" text_f "*")
)
(mapcar '(lambda (x)
		(if (wcmatch(strcase(mip_MTEXT_Unformat(vla-get-TextString(vlax-ename->vla-object x)))) text_f)
			(setq ss3 (ssadd x ss3))
		)
		)	
	(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(sssetfirst nil ss3)
(princ)
)
(defun C:zod490		()(zod490 "" 1))
(defun C:zod490z	()(zod490 "" 2))



;выделение нужного текста
(defun c:zod49 (/ zz1 zz2 )
(setq 	zz1 	(getstring "\n Введите искомый текст  \n")
		zz1		(strcat "*"  zz1 "*")
		zz2		(strcase zz1)	)
(sssetfirst nil (ssget "_X" (list '(-4 . "<OR") (cons 1 zz1) (cons 304 zz1)(cons 1 zz2) (cons 304 zz2) '(-4 . "OR>"))))
(princ)
)

(defun c:zod49999 (/ text_f ent ss3)
(princ "\nПоиск текста в атрибутах блока ")
(setq	ss3 		(ssadd)
		text_f		(strcase(getstring "\n Введите искомый текст  \n"))
		text_f		(strcat "*" text_f "*")
)
(SETQ ent (entnext))
(while ent
	(if (vlax-property-available-p (vlax-ename->vla-object ent) 'textstring)
		(if (wcmatch(strcase(mip_MTEXT_Unformat(vla-get-TextString(vlax-ename->vla-object ent)))) text_f)
			(setq ss3 (ssadd (cdr(assoc 330(entget ent))) ss3))
		)
	)
	(SETQ ent (entnext ent))
)
(sssetfirst nil ss3)
(princ)
)

;извлечение из размера его значение/1000 и умножить на 2,2 ,замена "." на "," и копирование в буфер
(defun c:zod50 (/)
(copyToclipboard (vl-list->string (subst (ascii ",") (ascii ".")(vl-string->list(rtos(*(/(cdr(assoc 42(entget(car(entsel)))))1000)2.2)2 1)))))
(princ)
)


;после указания номера помещения и размерно лини окна открывает файл и добавляет строку с номером и площадью окна
(defun c:zod51 (/ nomer_pomesheniya razmer_okna f flag1 flag2)
(while (null flag1)
	(if (setq nomer_pomesheniya (assoc 1(entget(car(nentsel "\nУкажите номер помещения")))))
		(progn	(setq nomer_pomesheniya (cdr nomer_pomesheniya))
			(setq flag1 t)
		)
	)
)
(while (null flag2)
	(if (equal (assoc 0(setq razmer_okna (entget(car(entsel "\nУкажите размерную линию окна"))))) '(0 . "DIMENSION"))
		(progn	(setq razmer_okna (vl-list->string (subst (ascii ",") (ascii ".")(vl-string->list(rtos(*(/(cdr(assoc 42 razmer_okna))1000)2.2)2 1)))))
			(setq flag2 t)
		)
	)
)
(setq f (open "D:\\111\\111.txt" "a"))
(write-line (strcat nomer_pomesheniya  "\t" razmer_okna )f)
(close f)
(princ)
)


(defun c:zod52 ( / ent1) 
(setq ent1 (vlax-ename->vla-object(car(entsel "Выберете текст"))))
(vla-move (vla-copy ent1)(vla-get-InsertionPoint ent1) (vlax-3d-point (getpoint "\nБазовая точка: ")))
(vla-put-TextString(vlax-ename->vla-object(entlast))(* 60(atoi(vla-get-TextString ent1))))
)

(defun c:zod522 ( / ent1 ent2 ent3) 
(setq ent1 (vlax-ename->vla-object(car(entsel "Выберете текст"))))
(setq ent2 (vlax-ename->vla-object(car(entsel "Выберете текст"))))
(setq ent3 (vlax-ename->vla-object(car(entsel "Выберете итоговый текст"))))
(vla-put-TextString
	ent3
	(+	(atoi(vla-get-TextString ent1))
		(atoi(vla-get-TextString ent2))
	)
)
)


(defun c:zod53 ( / ent1) 
(setq ent1 (vlax-ename->vla-object(car(entsel "Выберете текст"))))
(vla-move (vla-copy ent1)(vla-get-InsertionPoint ent1) (vlax-3d-point (getpoint "\nБазовая точка: ")))
(vla-put-TextString(vlax-ename->vla-object(entlast))(* 1.15 (atoi(vla-get-TextString ent1))))
)

(defun c:zod54 ( / ent1) 
(setq ent1 (vlax-ename->vla-object(car(entsel "Выберете текст"))))
(vla-move (vla-copy ent1)(vla-get-InsertionPoint ent1) (vlax-3d-point (getpoint "\nБазовая точка: ")))
(vla-put-TextString(vlax-ename->vla-object(entlast))(rtos(* 0.86 0.2 (/ 1 3600.0)(atoi(vla-get-TextString ent1)))2 4))
)





(defun c:zod56 ( / zn_1 zn_2)
(setq 	zn_1	(izvl_texta_ili_attrib "Расход")
		zn_2	(izvl_texta_ili_attrib "Расход")
		zn_1 	(rtos(+ (text_to_number zn_2) (text_to_number zn_1))2 0)
)
(zapis_texta_ili_attrib "Расход" zn_1)
)

(defun c:zod566 ( / )(zapis_texta_ili_attrib "Расход" (izvl_texta_ili_attrib "Расход")))

(defun c:zod57 ( / )(zapis_texta_ili_attrib "СЕЧЕНИЕ" (izvl_texta_ili_attrib "СЕЧЕНИЕ")))


(defun c:zod58 (/ ploshad)
(setq ploshad (getreal "Введите площадь "))
(vla-put-TextString(vlax-ename->vla-object (car(entsel "\n Выберете текст  ")))(strcat "L=" (rtos (round(* ploshad 4.75)10) 2 0)" м%%179/час"))
(vla-put-TextString(vlax-ename->vla-object (car(entsel "\n Выберете текст  ")))(strcat "L=" (rtos (round(* ploshad 4.75)10) 2 0)" м%%179/час"))
)

(defun round (value to)
  (if (zerop to) value
    (* (atoi (rtos (/ (float value) to) 2 0)) to)
	)
)



;%%179 - это знак куба
(defun c:Zod59 (/ ss1 )
(setq ss1 (ssget '((0 . "TEXT"))))
(mapcar '(lambda (x)
	(entmod	(subst
				(cons 1 	(strcat "L="(rtos(round(*(/ (atoi(vl-string-right-trim "м?/час"(vl-string-left-trim "L="(cdr(assoc 1 (entget x)))))) 4.75)4.42857)10) 2 0)" м%%179/час"))
				(assoc 1(entget x))
				(entget x)
			)
	)
		)
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(princ)
)

(defun c:zod60 (/ ploshad)
(setq ploshad (getreal "Введите площадь "))
(vla-put-TextString(vlax-ename->vla-object (car(entsel "\n Выберете текст  ")))(strcat "L=" (rtos (round(* ploshad 9.2 30)10) 2 0)" м%%179/час"))
(vla-put-TextString(vlax-ename->vla-object (car(entsel "\n Выберете текст  ")))(strcat "L=" (rtos (round(* ploshad 9.2 30)10) 2 0)" м%%179/час"))
)




(defun c:zod61( / ss1 summa s4et4ik)
(princ "\n Считает суммарную площадь")
(princ "\n Выделите объекты")
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(vla-selectonscreen ss1 nil nil)
(setq	summa 	0.0	s4et4ik	0)
(vlax-for x ss1
	(if (vlax-property-available-p x 'area)
		(setq summa(+ (vla-get-Area x) summa) s4et4ik(1+ s4et4ik))
	)
)
(princ (strcat "\n Количество объектов,у которых есть площадь   " (rtos s4et4ik 2 0) "\n Суммарная площадь    " (rtos (/ summa 1000000.0) ) "  м2"))
(princ)
)

(defun c:zod611( / ss1 summa s4et4ik)
(princ "\n Считает суммарную площадь")
(princ "\n Выделите объекты")
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(vla-selectonscreen ss1 nil nil)
(setq	summa 	0.0	s4et4ik	0.0)
(vlax-for x ss1
	(if (vlax-property-available-p x 'area)
		(setq summa(+ (vla-get-Area x) summa) s4et4ik(1+ s4et4ik))
	)
)
(vla-addtext 	
		(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
			(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
			(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
		)
	(strcat (rtos (/ summa 1000000.0))" м2")
	(vlax-3d-point(getpoint "\nУкажите точку вставки текста")) 
	180
)
(princ)
)













(defun c:zod62( / bit ss1)
(setq bit (getreal "\n Ведите число на которое нужно умножить "))
(setq ss1(ssget  (list '(0 . "TEXT,MTEXT"))))
(mapcar '(lambda (x)
	(vla-put-TextString(vlax-ename->vla-object x)(* bit(atoi(vla-get-TextString(vlax-ename->vla-object x)))))
	 )
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(princ)
)

(defun c:zod63( / bit ss1)
(setq bit (getreal "\n Ведите число на которое нужно умножить "))
(setq ss1(ssget  (list '(0 . "TEXT,MTEXT"))))
(mapcar '(lambda (x)
	(vla-put-TextString(vlax-ename->vla-object x)(round (* bit(atoi(vla-get-TextString(vlax-ename->vla-object x))))10))
	 )
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(princ)
)


(defun c:zod64 (/ ent1 zn_1 zn_2 zn_3 flag1)
(while (null flag1)
	(setq ent1 (car(entsel "\n Выберете блок")))
	(if
		(equal(cdr(assoc 0(entget ent1)))"INSERT")
		(setq zn_1(izvle4enie_se4enia ent1))
	)
	(if zn_1	(setq flag1 T))
)
(setq flag1 nil)
(while (null flag1)
	(setq ent1 (car(entsel "\n Выберете итоговый текст либо блок")))
	(if
		(equal(cdr(assoc 0(entget ent1)))"INSERT")
		(progn(zapis_razmera ent1 zn_1)(setq flag1 T))
	)
)
(princ)
)

(defun izvle4enie_se4enia (blok_name / zna4 Att_s name)
(car(vl-sort 
	(if	(equal (vl-catch-all-apply 'length(list(setq Att_s (vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetAttributes (vlax-ename->vla-object blok_name ))))))))3)
			(mapcar	'(lambda (name)
				(if
					(equal(strcase(vla-get-TagString name)) "СЕЧЕНИЕ")		(setq zna4 (atoi(remove_space(vla-get-TextString name))))
				)
				)
			Att_s
			)
	)
'>))
)
(defun zapis_razmera (blok_name zna4 / Att_s name name) 
	(if	(> (vl-catch-all-apply 'length(list(setq Att_s_2 (vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetAttributes (vlax-ename->vla-object blok_name ))))))))0)
			(mapcar	'(lambda (name)
				(if
					(equal(strcasecase(vla-get-TagString name)) "РАЗМЕР")		(vla-put-TextString name zna4)	
				)
				)
			Att_s_2
			)
	)
(princ)
)












;простановка отметок с учетом уклона
(defun c:zod65( / oldMode oldMode_2 zod65_mode_zapyatie zod65_mode_zvezd sum_dlin tek_dlin flag_1 flag_2 flag_3 cc1 cc2 pt_1 pt_2)
(defun Zoderror (msg)
(vl-cmdf)
(vla-EndUndoMark actdoc)
(setq *error* my_error)
(vl-cmdf "Undo" "" )
)
(setq actdoc (vla-get-activedocument(vlax-get-acad-object)))
(vla-startUndoMark actdoc)
(setq 	my_error	*error*
	*error*		Zoderror
)
(if (or(null zod65_mode_1)(null zod65_Uklon))(zod655))
(setq sum_dlin	0.0 tek_dlin 0.0)
(while (null flag_1)
	(if	(setq cc1(car (entsel "\n Укажите текст/мультвыноску/блок")))
		(if	(member (cdr(assoc 0 (entget cc1)))'("TEXT" "MTEXT" "MULTILEADER" "INSERT"))
			(setq flag_1 t)
		)
	)
)
(if	(equal(assoc 0 (entget cc1))'(0 . "INSERT"))
	(setq cc1(mip_MTEXT_Unformat(vla-get-textstring(car(vlax-safearray->list(vlax-variant-value(vla-GetAttributes (vlax-ename->vla-object cc1 ))))))))
	(setq cc1 (mip_MTEXT_Unformat(vla-get-textstring(vlax-ename->vla-object cc1))))
)
(if (vl-string-search "," cc1)(setq zod65_mode_zapyatie T cc1(vl-string-subst "." "," cc1)))
(if (vl-string-search "*" cc1)(setq zod65_mode_zvezd T cc1(vl-string-subst "." "," cc1)))
(setq cc1 (atof cc1))
(setq pt_1(getpoint "\n Укажите точку"))
(if (null pt_1) (setq tek_dlin 0.0 sum_dlin 0.0)
			(while (null flag_2)
				(princ (strcat "\n Текущая длина " (rtos (/ sum_dlin 1000) 2 3) "  м"))
				(setq pt_2(getpoint "\n Укажите точку" pt_1))
				(if pt_2
					(setq	tek_dlin (DISTANCE pt_2 pt_1) sum_dlin (+ sum_dlin tek_dlin) pt_1 pt_2 tek_dlin 0.0)
					(setq	flag_2 T)
				)
			)
)
(princ (strcat "\n Текущая длина  " (rtos (/ sum_dlin 1000) 2 3) "  м"))
(while (null flag_3)
	(if	(setq cc2(car (entsel "\n Укажите текст/мультвыноску/блок")))
		(if	(member (cdr(assoc 0 (entget cc2)))'("TEXT" "MTEXT" "MULTILEADER" "INSERT"))
			(setq flag_3 t)
		)
	)
)
(if (equal zod65_mode_1 "+")
(setq sum_dlin(rtos (+ cc1 (*(/ sum_dlin 1000)zod65_Uklon)) 2 3 ))
(setq sum_dlin(rtos (- cc1 (*(/ sum_dlin 1000)zod65_Uklon)) 2 3 ))
)
(if (vl-string-search "." sum_dlin)
	(cond	((equal 1 (-(strlen sum_dlin)(vl-string-search "." sum_dlin)1)) (setq sum_dlin(strcat sum_dlin "00")))
			((equal 2 (-(strlen sum_dlin)(vl-string-search "." sum_dlin)1)) (setq sum_dlin(strcat sum_dlin "0")))
	)
	(setq sum_dlin(strcat sum_dlin ".000"))
)
(if zod65_mode_zapyatie (setq sum_dlin(vl-string-subst "," "."  sum_dlin)))
(if zod65_mode_zvezd	(setq sum_dlin(strcat sum_dlin "*")))
(if (not(equal "-"(SUBSTR sum_dlin 1 1)))
	(setq sum_dlin(strcat "+" sum_dlin))
)
(if	(equal(assoc 0 (entget cc2))'(0 . "INSERT"))
	(vla-put-textstring(car(vlax-safearray->list(vlax-variant-value(vla-GetAttributes (vlax-ename->vla-object cc2 )))))sum_dlin)
	(vla-put-textstring(vlax-ename->vla-object cc2)sum_dlin)
)
)

(defun zod655( / oldMode oldMode_2)
(defun Zoderror (msg)
(vl-cmdf)
(vla-EndUndoMark actdoc)
(setq *error* my_error)
(vl-cmdf "Undo" "" )
)
(setq actdoc (vla-get-activedocument(vlax-get-acad-object)))
(vla-startUndoMark actdoc)
(setq 	my_error	*error*
	*error*		Zoderror
)

(if(not zod65_mode_1)(setq zod65_mode_1 "+"))
	(initget "+ -")
    (setq oldMode zod65_mode_1 
    zod65_mode_1 
     (getkword
       (strcat "\n Выберите [+/-] <" zod65_mode_1 ">: "))
     ); end setq
	 (if(null zod65_mode_1)(setq zod65_mode_1 oldMode))
(if (and zod65_Uklon (not(equal 'str (type zod65_Uklon))))(setq zod65_Uklon(rtos zod65_Uklon 2 3)))
(if(not zod65_Uklon)(setq zod65_Uklon "0.003"))
	(initget (strcat zod65_Uklon  " Input"))
    (setq oldMode_2 zod65_Uklon 
    zod65_Uklon 
     (getkword
       (strcat "\nSpecify mode [" zod65_Uklon "/Input] <" zod65_Uklon ">: "))
     ); end setq
	 (if(null zod65_Uklon)(setq zod65_Uklon oldMode_2))
	 (if(equal zod65_Uklon  "Input")(setq zod65_Uklon (getreal "Input new value")))
	 (if(null zod65_Uklon)(setq zod65_Uklon 0.000))
	 (if (equal 'str (type zod65_Uklon))(setq zod65_Uklon(atof zod65_Uklon)))
)

(defun c:zod655 ( / )(zod655))



;суммирует выделенный текст
(defun c:zod66( / ss1 ss2)
(setq ss2	0)
(setq ss1 (ssget '((0 . "TEXT,MTEXT"))))
(mapcar
          '(lambda (x)(setq ss2(+ ss2 (ent_to_number x)))
			 )
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(princ (strcat "\n Сумма    " (rtos ss2 2 2)))
(princ)
)




;суммирует выделенный текст и записывает в текст
(defun c:zod666( / ss1 ss2)
(setq ss2	0)
(setq ss1 (ssget '((0 . "TEXT,MTEXT"))))
(mapcar
          '(lambda (x)(setq ss2(+ ss2 (ent_to_number x)))
			 )
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(vla-move (vla-copy (vlax-ename->vla-object (ssname ss1 0))) (vlax-3d-point(cdr(assoc 10(entget (ssname ss1 0))))) (vlax-3d-point (getpoint)))
(vla-put-textstring(vlax-ename->vla-object (entlast))(rtos ss2 2 0))
(princ)
)

(defun c:zod6666( / ss1 ss2)
(setq ss2	0)
(setq ss1 (ssget '((0 . "TEXT,MTEXT"))))
(mapcar
          '(lambda (x)(setq ss2(+ ss2 (ent_to_number x)))
			 )
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(vla-put-textstring(vlax-ename->vla-object (car(entsel " выберете текст ")))(rtos ss2 2 0))
(princ)
)



;делает число целым
(defun c:zod67( / zz1 )
(vla-put-textstring (vlax-ename->vla-object(setq zz1(car(entsel)))) (atoi(vl-string-subst "." ","(mip_MTEXT_Unformat(cdr(assoc 1(entget zz1)))))))
)


;создает список по n текстам
(defun c:zod68 ( / flag1 zz1 small_Spisok_zod68 n)
(if (null zod68_mode)(zod68))
(setq n zod68_mode)
(while (> n 0)
	(while (null flag1)
		(if (null(setq zz1 (entsel (strcat "\n Укажите " (rtos (1+(- zod68_mode n)) 2 0) " значение \n"))))
			(setq flag1 t zz1 "")
			(if (vlax-property-available-p (vlax-ename->vla-object(car zz1)) 'textstring)
				(setq zz1 (mip_MTEXT_Unformat(vla-get-textstring(vlax-ename->vla-object(car zz1)))) flag1 t)
				(if (vlax-property-available-p (vlax-ename->vla-object(car zz1)) 'Measurement)
					(setq zz1 (mip_MTEXT_Unformat(rtos (vla-get-Measurement(vlax-ename->vla-object(car zz1)))2 0)) flag1 t)
					(setq flag1 t zz1 "")
				)
			)
		)
		(if (equal zz1 "") (princ (strcat "\n получено пустое значение "))(princ (strcat "\n получено значение " zz1)))
		(setq small_Spisok_zod68(append small_Spisok_zod68 (list  zz1)))
	)
	(setq n (1- n) flag1	nil)
)
(setq Spisok_zod68(append Spisok_zod68 (list small_Spisok_zod68)))
(princ)
)




 
;создает текстовый файл по списку из zod68
(defun c:zod688 ( / а y x string1)
(setq Spisok_zod68	
		(vl-sort 
			(vl-sort Spisok_zod68 
					'(lambda(x1 x2)
						(> (cadr x1)(cadr x2))
					)
			)
				'(lambda(x1 x2)
				(> (car x1)(car x2))
				)
		)
)
(setq Spisok_zod68
	(mapcar '(lambda(zzz)
			(append (cdr(reverse  zzz))	(list(if	(equal (type  (car(reverse zzz))) 'STR)
														(atof(car(reverse zzz)))
														(car(reverse zzz))
													))
			)
		)
		Spisok_zod68
	)
)
(setq Spisok_zod68 (sort_and_sum_list Spisok_zod68))
(setq Spisok_zod68
	(mapcar '(lambda(zzz)
			(append (cdr(reverse  zzz))		(list(if	(numberp (car(reverse zzz)))
														(rtos(car(reverse zzz)) 2 2)
														(car(reverse zzz))
													))
			)
		)
		Spisok_zod68
	)
)
(setq f (open "C:\\111\\zod68.txt" "a"))
(mapcar '(lambda(y)(progn
						(setq string1 "")
						(mapcar '(lambda(x)(setq string1 (strcat string1 x "\t")))y)
						(write-line string1 f)
					)
		)
Spisok_zod68
)
(close f)
(princ)
)

(defun zod68 ()
(setq zod68_mode (getint "\n Введите количество данных \n"))
	(if	
		(< zod68_mode 1)
		(setq zod68_mode (getint "\n Введите количество данных \n"))
	)
(princ)
)

;задать нвое количество значений и сброс списка
(defun c:zod6888 ()(setq Spisok_zod68 nil)(zod68))

;показывает последние 3 значения списка Spisok_zod68
(defun c:zod6801 ( / )
(list (caddr (reverse Spisok_zod68))(cadr (reverse Spisok_zod68))(car (reverse Spisok_zod68)))
)

;удаляет последнее значение списка Spisok_zod68
(defun c:zod6802 ( / )
(setq Spisok_zod68(reverse(cdr(reverse Spisok_zod68))))
)

;создает список из текстового файла
(defun c:zod69 ( / a flag q  )
(setq spisok_zod69 nil)
(setq a (open (getfiled "Select a TXT File" (strcat(vla-get-path(vla-get-activedocument(vlax-get-acad-object))) "\\") "txt" 2) "r"))
(while (not flag)
	(if (setq q (read-line a))
		(setq spisok_zod69 (append spisok_zod69 (list(list(substr q 1 (vl-string-search "\t" q))(substr q (+ 2 (vl-string-search "\t" q)) (- (strlen q)(vl-string-search "\t" q)))))))
		(setq flag T)
	)
)
(close a)
)


;в выделенном тексте ищет совпадения среди списка созданного zod69,если находит то делает копию текста со вторым значением из списка,и задает ему цвет 134 
(defun c:zod699( / ss1 z_text la_name)
(setq bit_zod699 (getreal "\n введите высоту текста\n"))
(princ "\n укажите текст,мтекст,мультивыноски ")
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "TEXT,MTEXT,MULTILEADER")))
(if (zerop(vla-get-count ss1))(exit))
(setq la_name(strcat(vla-get-layer(vla-item ss1 0))"_111")) 
(if	(null(tblobjname "Layer" la_name))(vla-add(vla-get-Layers(vla-get-activedocument(vlax-get-acad-object)))la_name))
(vlax-for x ss1
	(setq z_text(mip_MTEXT_Unformat(vla-get-TextString x)))
	(mapcar '(lambda (z)
		(if (wcmatch z_text (strcat "*" (car z) "*"))
			(vla-put-Layer
				(vla-addtext 	
					(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
						(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
						(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
					)
					(cadr z)
					(vlax-3d-point(cdr(assoc 10 (entget(vlax-vla-object->ename x)))))
					bit_zod699
				)
				la_name
			)
		)	
			)
	spisok_zod69
	)
)
(vla-clear ss1)
(princ)
)


;добавляет число ко всем выделенным текстам
(defun c:zod70( / bit ss1 )
(setq bit(getreal "Input number "))
(setq ss1 (ssget '((0 . "TEXT,MTEXT"))))
(mapcar
          '(lambda (x)(vla-put-textstring(vlax-ename->vla-object x)(+ bit(atof(vl-string-subst "." ","(mip_MTEXT_Unformat(cdr(assoc 1(entget x))))))))
			 )
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(princ)
)





;извлекает число из текста,умножает его на 200,и вставляет текст со значением
(defun c:zod722 ( / ent1 layer_name)
(setq ent1(car(entsel "Выделете исходный текст")))
(if	(null(tblobjname "Layer" (setq layer_name(strcat(cdr(assoc 8 (entget ent1))) "_111"))))
	(vla-add(vla-get-Layers(vla-get-activedocument(vlax-get-acad-object)))layer_name)
)
(vla-put-Layer
	(vla-addtext 	
			(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
				(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
				(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
			)
		(round(* bit_zod72 (ent_to_number ent1))bit2_zod72)
		(vlax-3d-point(getpoint "\nУкажите точку вставки текста")) 
		180
	)
layer_name)
(princ)
)

(defun c:zod72 ()
(setq bit_zod72 (getreal "\nвведите число на которое будет умножаться\n"))
(setq bit2_zod72 (getreal "\nвведите число до которого будет округление  0.01,0.1,1,10,100,1000 и т.д.\n"))
)
	
(defun c:zod7222 ( / ent1 layer_name)
(setq ss1 (ssget '((0 . "TEXT,MTEXT"))))
(if	(null(tblobjname "Layer" (setq layer_name(strcat(cdr(assoc 8 (entget (ssname ss1 0)))) "_111"))))
	(vla-add(vla-get-Layers(vla-get-activedocument(vlax-get-acad-object)))layer_name)
)
(mapcar
	'(lambda (x)
(vla-put-Layer
	(vla-addtext 	
			(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
				(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
				(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
			)
		(round(* bit_zod72 (ent_to_number x))bit2_zod72)
		(vlax-3d-point(list (cadr (assoc 10(entget x)))(caddr (assoc 10(entget x)))(cadddr (assoc 10(entget x))))) 
		180
	)
layer_name
)
	)
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(princ)
)



	
(defun c:Zod73 (  )
(setq ent1 nil
)
(setq  ent1 (car(entsel "Укажите нужный блок: ")))
(entmod
	(subst	
		(cons 50 (+ (angtof "180" )(cdr(assoc 50(entget ent1))))) 
		(assoc 50(entget ent1))
		(entget ent1)
	)
)
)

(defun c:Zod74 (  )
(sssetfirst nil (ssget '((0 . "HATCH"))))
)


(defun c:Zod75 ( / dlina_lin vla_text pt1 pt2 pt3 pt4)
(setq dlina_lin (strcat "Длина"(en) " м"))
(setq vla_text
	(vla-addtext 	
			(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
				(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
				(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
			)
		dlina_lin
		(vlax-3d-point(getpoint "\nУкажите точку вставки текста")) 
		1000
	)
)
(vla-GetBoundingBox vla_text 'pt1 'pt2)
(vla-addline 
	(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
		(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
		(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
	)
	pt1
	(setq pt2(vlax-variant-value(vlax-3d-point(list(car(vlax-safearray->list pt2))(cadr(vlax-safearray->list pt1))(caddr(vlax-safearray->list pt2))))))
)
(setq pt3(getpoint "Укажите первую точку :"))
(if	(<	(distance (vlax-safearray->list pt1) pt3)
		(distance (vlax-safearray->list pt2) pt3)
	)
	(setq pt4 pt1)
	(setq pt4 pt2)
)
(vla-addline 
	(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
		(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
		(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
	)
	(vlax-3d-point pt3)
	pt4
)
(princ)
)


(defun en ( / set:entities int:allEntities int:curveEntities int:l rea:length zzz)
  (if (setq set:entities (ssget))
    (progn
      (setq int:allEntities (sslength set:entities)	; количество выбранных примитивов
	    int:curveEntities 0				; счетчик линейных примитивов
	    int:l 0					; счетчик
	    rea:length 0.0				; общая длина линейных примитивов
      ) ;_  setq
      (while (< int:l (sslength set:entities))
	(if (not
	      (vl-catch-all-error-p
		(vl-catch-all-apply
		  'vlax-curve-getStartPoint
		  (list (vlax-ename->vla-object (ssname set:entities int:l)))
		) ;_  vl-catch-all-apply
	      ) ;_  vl-catch-all-error-p
	    ) ;_  not
	  (setq	int:curveEntities (1+ int:curveEntities)
		rea:length	  (+ rea:length
				     (vlax-curve-getDistAtParam
				       (vlax-ename->vla-object (ssname set:entities int:l))
				       (vlax-curve-getEndParam (ssname set:entities int:l))
				     ) ;_  vlax-curve-getDistAtParam
				  ) ;_  +
	  ) ;_  setq
	) ;_  if
	(setq int:l (1+ int:l))
      ) ;_  while
	(setq zzz (rtos (/ rea:length 1000) 2 1))
    ) ;_  progn
  ) ;_  if
) ;_  defun


;разворачивает блоки на 180 градусов относительно центра объекта
(defun c:Zod76 (/ ss1 pt1 pt2 pt3)
(princ "\n разворачивает блоки \n")
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "INSERT")))
(vlax-for x ss1
	(vla-GetBoundingBox x 'pt1 'pt2)
	(setq pt1  (vlax-safearray->list pt1)  pt2 (vlax-safearray->list pt2))
	(setq pt3 (list	(/(+(car pt1 )	(car pt2 ))2)
					(/(+(cadr pt1 )	(cadr pt2 ))2)
					(/(+(caddr pt1 )(caddr pt2 ))2)
				)
	)
	(vla-rotate x (vlax-3d-point pt3) pi)
)
(vla-clear ss1)
(princ)
)


(defun c:Zod766 (/ ss1 pt1 pt2 pt3 )
(princ "\n зеркалит \n")
(setq ss1(mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex(ssget '((0 . "INSERT"))))))))
(mapcar '(lambda(x)
	(progn
	(vla-GetBoundingBox x 'pt1 'pt2)
	(setq pt1  (vlax-safearray->list pt1)  pt2 (vlax-safearray->list pt2))
	(setq pt3 (list	(/(+(car pt1 )	(car pt2 ))2)
					(/(+(cadr pt1 )	(cadr pt2 ))2)
					(/(+(caddr pt1 )(caddr pt2 ))2)
				)
	)
	(vla-mirror x (vlax-3d-point pt3) (vlax-3d-point(list (car pt3)(+ 100(cadr pt3))(caddr pt3) )))
	)
	)
	ss1
)
(mapcar '(lambda(x)(entdel(vlax-vla-object->ename x)))ss1)
(princ)
)


(defun c:Zod7666 (/ ss1 pt1 pt2 pt3 )
(princ "\n зеркалит \n")
(setq ss1(mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex(ssget '((0 . "INSERT"))))))))
(mapcar '(lambda(x)
	(progn
	(vla-GetBoundingBox x 'pt1 'pt2)
	(setq pt1  (vlax-safearray->list pt1)  pt2 (vlax-safearray->list pt2))
	(setq pt3 (list	(/(+(car pt1 )	(car pt2 ))2)
					(/(+(cadr pt1 )	(cadr pt2 ))2)
					(/(+(caddr pt1 )(caddr pt2 ))2)
				)
	)
	(vla-mirror x (vlax-3d-point pt3) (vlax-3d-point(list (+ 100(car pt3))(cadr pt3)(caddr pt3) )))
	)
	)
	ss1
)
(mapcar '(lambda(x)(entdel(vlax-vla-object->ename x)))ss1)
(princ)
)



(defun izmereinie_dlini (/ tek_dlin sum_dlin flag_2 pt1 pt2 pt3)
(setq 
tek_dlin 0.0
sum_dlin	0.0
)
(setq pt_1(getpoint "\n введите 1 точку \n"))
(if (null pt_1) (setq tek_dlin 0.0 sum_dlin 0.0)
			(while (null flag_2)
				(setvar "LASTPOINT" pt_1)
				(setq pt_1 nil tek_dlin 0.0)
				(princ (strcat "\n Текущая длина " (rtos (/ sum_dlin 1000) 2 3) "  m \n"))
				(setq pt_1(getpoint "\n введите следущую точку \n"(getvar "LASTPOINT")))
				(if pt_1
					(setq	tek_dlin(DISTANCE (getvar "LASTPOINT") pt_1)
							sum_dlin (+ sum_dlin tek_dlin))
				(setq flag_2 T)
				)
			)
)
(setq sum_dlin sum_dlin)
)

(defun c:Zod77 () (rtos (/ (izmereinie_dlini) 1000) 2 3 ))

(defun c:zod77 ()
(setq bit_zod77 (getstring "\n введите тект который будет добавлятся к надписи\n"))
(setq bit2_zod77 (getint "\n введите число чисел после запятой \n"))
(setq bit3_zod77 (getint "\n введите высоту текста \n"))
)

(defun c:zod777 ()
	(vla-addtext 	
			(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
				(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
				(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
			)
		(strcat bit_zod77 " "(rtos (/ (izmereinie_dlini) 1000) 2 bit2_zod77 ))
		(vlax-3d-point(getpoint "\nУкажите точку вставки текста")) 
		bit3_zod77
	)
(princ)
)


(defun izvle4enie_razmerov (blok_name / zna4 Att_s name)
(car(vl-sort 
	(if	(equal (vl-catch-all-apply 'length(list(setq Att_s (vl-catch-all-apply 'vlax-safearray->list(list(vlax-variant-value(vla-GetAttributes (vlax-ename->vla-object blok_name ))))))))3)
			(mapcar	'(lambda (name)
				(if
					(equal(strcase(vla-get-TagString name)) "СЕЧЕНИЕ")		(setq zna4 (remove_space(vla-get-TextString name)))
				)
				)
			Att_s
			)
	)
'>))
)

(defun c:zod78 ( / ent1 block_1 spisok1 Diametr Shirina Visota)
(setq ent1 (car(entsel "\n Выберете исходный блок")))
(setq block_1 (vlax-ename->vla-object(car(entsel "\n укажите блок, который нужно изменить "))))
(setq spisok1 (text_to_spisok(izvle4enie_razmerov ent1)2))
(if (= 0(cdr spisok1))(setq Diametr (car spisok1))(setq Shirina (car spisok1) Visota (rtos(cadr spisok1)2 0)))
(mapcar '(lambda(z) 
	(cond	((equal(vla-get-PropertyName z) "Shirina")		(vla-put-Value z Shirina)		)
			((equal(vla-get-PropertyName z) "Visota")		(vla-put-Value z Visota)		)
			((equal(vla-get-PropertyName z) "Diametr")		(vla-put-Value z Diametr)		)

	)
		)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties block_1)))
)
(princ)
)

(defun c:zod788( / block_1 block_3 Shirina1 Visota1 Diametr1 se4enie)
(princ "\n Выставление параметров блоку ")
(setq block_1 (vlax-ename->vla-object(car(entsel "\n укажите 1 блок "))))
(setq block_3 (vlax-ename->vla-object(car(entsel "\n укажите блок в котором нужно изменить значения "))))
(mapcar '(lambda(z) 
	(cond	((equal(vla-get-PropertyName z) "Shirina")		(setq Shirina1	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Visota")		(setq Visota1	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Diametr")		(setq Diametr1	(vlax-variant-value(vla-get-Value z)))		)
	)
		)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties block_1)))
)
(if Shirina1 	(if (numberp Shirina1) 	(setq Shirina1	(rtos Shirina1 2 0))))
(if Visota1 	(if (numberp Visota1) 	(setq Visota1	(rtos Visota1 2 0))))
(if Diametr1 	(if (numberp Diametr1) 	(setq Diametr1	(rtos Diametr1 2 0))))
(if Diametr1 	(setq se4enie Diametr1) (setq se4enie(strcat Shirina1 "x" Visota1 )))
(if	(equal (vl-catch-all-apply 'length(list(setq Att_s (vlax-safearray->list(vlax-variant-value(vla-GetAttributes block_3 ))))))3)
			(mapcar	'(lambda (name)
				(if	(equal(vla-get-TagString name) "СЕЧЕНИЕ")		(vla-put-TextString name	se4enie))
				)
			Att_s
			)
)
(princ)
)


(defun c:zod79( / block_1 block_2 block_3 Shirina1 Visota1 Diametr1  Shirina2 Visota2 Diametr2)
(princ "\n Выставление параметров блоку ")
(setq block_1 (vlax-ename->vla-object(car(entsel "\n укажите 1 блок "))))
(if (setq block_2(car(entsel "\n укажите 2 блок ")))(setq block_2 (vlax-ename->vla-object block_2)))
(setq block_3 (vlax-ename->vla-object(car(entsel "\n укажите блок в котором нужно изменить значения "))))
(mapcar '(lambda(z) 
	(cond	((equal(vla-get-PropertyName z) "Shirina")		(setq Shirina1	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Visota")		(setq Visota1	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Diametr")		(setq Diametr1	(vlax-variant-value(vla-get-Value z)))		)
	)
		)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties block_1)))
)
(if block_2
(mapcar '(lambda(z) 
	(cond	((equal(vla-get-PropertyName z) "Shirina")		(setq Shirina2	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Visota")		(setq Visota2	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Diametr")		(setq Diametr2	(vlax-variant-value(vla-get-Value z)))		)
	)
		)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties block_2)))
)
)
(if Shirina1 	(if (equal (type Shirina1) 'STR)	(setq Shirina1(atof Shirina1))))
(if Visota1 	(if (equal (type Visota1) 'STR)		(setq Visota1(atof Visota1))))
(if Diametr1 	(if (equal (type Diametr1) 'STR)	(setq Diametr1(atof Diametr1))))
(if Shirina2 	(if (equal (type Shirina2) 'STR)	(setq Shirina2(atof Shirina2))))
(if Visota2 	(if (equal (type Visota2) 'STR)		(setq Visota2(atof Visota2))))
(if Diametr2 	(if (equal (type Diametr2) 'STR)	(setq Diametr2(atof Diametr2))))

(if (or(and Shirina1 Diametr2)(and Shirina2 Diametr1))
	(if Diametr2 	(setq Diametr1 Diametr2 Shirina2 Shirina1 Visota2 Visota1)
					(setq Diametr2 Diametr1 Shirina1 Shirina2 Visota1 Visota2)))
(mapcar '(lambda(z) 
	(cond	((equal(vla-get-PropertyName z) "Shirina")		(if (numberp (vlax-variant-value(vla-get-Value z)))	(vla-put-Value z Shirina1)	(vla-put-Value z (rtos Shirina1 2 0)))		)
			((equal(vla-get-PropertyName z) "Visota")		(if (numberp (vlax-variant-value(vla-get-Value z)))	(vla-put-Value z Visota1)	(vla-put-Value z (rtos Visota1 2 0)))		)
			((equal(vla-get-PropertyName z) "Diametr")		(if (numberp (vlax-variant-value(vla-get-Value z)))	(vla-put-Value z Diametr1)	(vla-put-Value z (rtos Diametr1 2 0)))		)
			((equal(vla-get-PropertyName z) "Shirina1")		(if (numberp (vlax-variant-value(vla-get-Value z)))	(vla-put-Value z Shirina2)	(vla-put-Value z (rtos Shirina2 2 0)))		)
			((equal(vla-get-PropertyName z) "Visota1")		(if (numberp (vlax-variant-value(vla-get-Value z)))	(vla-put-Value z Visota2)	(vla-put-Value z (rtos Visota2 2 0)))		)
			((equal(vla-get-PropertyName z) "Diametr1")		(if (numberp (vlax-variant-value(vla-get-Value z)))	(vla-put-Value z Diametr2)	(vla-put-Value z (rtos Diametr2 2 0)))		)
	)
		)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties block_3)))
)
(princ)
)

(defun c:zod799( / block_1 block_3 Shirina1 Visota1 Diametr1 )
(princ "\n Выставление параметров блоку ")
(setq block_1 (vlax-ename->vla-object(car(entsel "\n укажите 1 блок "))))
(setq block_3 (vlax-ename->vla-object(car(entsel "\n укажите блок в котором нужно изменить значения "))))
(mapcar '(lambda(z) 
	(cond	((equal(vla-get-PropertyName z) "Shirina")		(setq Shirina1	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Visota")		(setq Visota1	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Diametr")		(setq Diametr1	(vlax-variant-value(vla-get-Value z)))		)
	)
		)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties block_1)))
)
(if Shirina1 	(if (equal (type Shirina1) 'STR)	(setq Shirina1(atof Shirina1))))
(if Visota1 	(if (equal (type Visota1) 'STR)		(setq Visota1(atof Visota1))))
(if Diametr1 	(if (equal (type Diametr1) 'STR)	(setq Diametr1(atof Diametr1))))
(mapcar '(lambda(z) 
	(cond	((equal(vla-get-PropertyName z) "Shirina")		(if (numberp (vlax-variant-value(vla-get-Value z)))	(vla-put-Value z Shirina1)	(vla-put-Value z (rtos Shirina1 2 0)))		)
			((equal(vla-get-PropertyName z) "Visota")		(if (numberp (vlax-variant-value(vla-get-Value z)))	(vla-put-Value z Visota1)	(vla-put-Value z (rtos Visota1 2 0)))		)
			((equal(vla-get-PropertyName z) "Diametr")		(if (numberp (vlax-variant-value(vla-get-Value z)))	(vla-put-Value z Diametr1)	(vla-put-Value z (rtos Diametr1 2 0)))		)
	)
		)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties block_3)))
)
(princ)
)

(defun c:zod7999( / block_1 block_3 Shirina1 Visota1 Diametr1)
(princ "\n Выставление параметров блоку ")
(setq block_1 (vlax-ename->vla-object(car(entsel "\n укажите 1 блок "))))
(setq block_3 (vlax-ename->vla-object(car(entsel "\n укажите блок в котором нужно изменить значения "))))
(vla-put-Layer block_3 (vla-get-Layer block_1))
(mapcar '(lambda(z) 
	(cond	((equal(vla-get-PropertyName z) "Shirina")		(setq Shirina1	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Visota")		(setq Visota1	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Diametr")		(setq Diametr1	(vlax-variant-value(vla-get-Value z)))		)
	)
		)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties block_1)))
)
(if Shirina1 	(if (equal (type Shirina1) 'STR)	(setq Shirina1(atof Shirina1))))
(if Visota1 	(if (equal (type Visota1) 'STR)		(setq Visota1(atof Visota1))))
(if Diametr1 	(if (equal (type Diametr1) 'STR)	(setq Diametr1(atof Diametr1))))
(mapcar '(lambda(z) 
	(cond	((equal(vla-get-PropertyName z) "Shirina")		(if (numberp (vlax-variant-value(vla-get-Value z)))	(vla-put-Value z Shirina1)	(vla-put-Value z (rtos Shirina1 2 0)))		)
			((equal(vla-get-PropertyName z) "Visota")		(if (numberp (vlax-variant-value(vla-get-Value z)))	(vla-put-Value z Visota1)	(vla-put-Value z (rtos Visota1 2 0)))		)
			((equal(vla-get-PropertyName z) "Diametr")		(if (numberp (vlax-variant-value(vla-get-Value z)))	(vla-put-Value z Diametr1)	(vla-put-Value z (rtos Diametr1 2 0)))		)
	)
		)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties block_3)))
)
(princ)
)



(defun c:zod80 ( / ent_name1 ent_name2 Att_s Se4enie Rashod Skorost)
(princ "\n для блоков акс,копирует значения одного в другой")
(setq ent_name1(vlax-ename->vla-object(car(entsel "\n Укажите исходный блок"))))
(setq ent_name2(vlax-ename->vla-object(car(entsel "\n Укажите изменяемый блок"))))
(if	(equal (vl-catch-all-apply 'length(list(setq Att_s (vlax-safearray->list(vlax-variant-value(vla-GetAttributes ent_name1 ))))))3)
			(mapcar	'(lambda (name)
				(cond
					((equal(vla-get-TagString name) "СЕЧЕНИЕ")		(setq Se4enie	(vla-get-TextString name))	)
					((equal(vla-get-TagString name) "РАСХОД")		(setq Rashod	(vla-get-TextString name))	)
					((equal(vla-get-TagString name) "СКОРОСТЬ")		(setq Skorost	(vla-get-TextString name))	)
				)
				)
			Att_s
			)
)
(if	(equal (vl-catch-all-apply 'length(list(setq Att_s (vlax-safearray->list(vlax-variant-value(vla-GetAttributes ent_name2 ))))))3)
			(mapcar	'(lambda (name)
				(cond
					((equal(vla-get-TagString name) "СЕЧЕНИЕ")		(setq Se4enie	(vla-put-TextString name	Se4enie))	)
					((equal(vla-get-TagString name) "РАСХОД")		(setq Rashod	(vla-put-TextString name	Rashod))	)
					((equal(vla-get-TagString name) "СКОРОСТЬ")		(setq Skorost	(vla-put-TextString name	Skorost))	)
				)
				)
			Att_s
			)
)
(princ)
)




(defun c:zod81 ( / ss2 )
(princ "\n показывает названия динамических свойств в блоке и их значения")
(mapcar '(lambda(z) 
	(setq ss2 (append ss2 (list(cons(vla-get-PropertyName z )(vlax-variant-value(vla-get-Value z))))))
		)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties(vlax-ename->vla-object(car(entsel))))))
)
(setq ss2 ss2)
)

(defun c:zod811 ( / ss2 )
(princ "\n показывает названия динамических свойств в блоке и их значения")
(mapcar '(lambda(z) 
	(setq ss2 (append ss2 (list(cons(vla-get-PropertyName z )(type(vlax-variant-value(vla-get-Value z)))))))
		)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties(vlax-ename->vla-object(car(entsel))))))
)
(setq ss2 ss2)
)





(defun c:zod82 ( / sel ss3)
(setq ss3 (ssadd))
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual sel '((0 . "INSERT")))
(vlax-for x sel(if (equal (vla-get-IsDynamicBlock x):vlax-true)
			(mapcar '(lambda(z) 
				(if	(equal (vlax-variant-value(vla-get-Value z)) "Custom")	(setq ss3	(ssadd  (vlax-vla-object->ename x) ss3))		)
				)
			(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties x)))
			)	)
)
(sssetfirst nil ss3)
(princ)
)


(defun c:zod83 ( / sel ss3)
(princ "\n Выставляет в блоках значение CUSTOM")
(setq ss3 (ssadd))
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual sel '((0 . "INSERT")))
(vlax-for x sel	(if (equal (vla-get-IsDynamicBlock  x):vlax-true)
			(mapcar '(lambda(z) 
				(if	(and	(equal (type(vlax-variant-value(vla-get-Value z))) (type ""))
							(equal (vla-get-Show z) :vlax-true)
					)
					(progn
						(vla-put-value z (vlax-make-variant "Custom" 8))
						(setq ss3	(ssadd  (vlax-vla-object->ename x) ss3))
					)
				)
				)
			(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties x)))
				)
	)			
)
(sssetfirst nil ss3)
(princ)
)


(defun ru-mleader-coords-from-list (list_points / n pt )
(setq pt_array
             (vlax-make-safearray vlax-vbdouble (cons 1 (* 3 (length list_points))))
            n 0
      )
      (while (< n (length list_points))
        (setq pt (nth n list_points))
        (vlax-safearray-put-element pt_array (+ (* n 3) 1) (car pt))
        (vlax-safearray-put-element pt_array (+ (* n 3) 2) (cadr pt))
        (vlax-safearray-put-element pt_array (+ (* n 3) 3) (caddr pt))
        (setq n (1+ n))
      )
  pt_array
 )
 
(defun c:zod844()(setq bit_zod84 (getreal "\n введите высоту текста\n")))
 
(defun c:zod84 ( / vla_mleader layer_name number  ent_pt  pt2  min_dist  pt3  spisok1  ent_z )
(if (null bit_zod84)(setq bit_zod84 (getreal "\n введите высоту текста\n")))
(setq layer_name(vla-get-Layer(vlax-ename->vla-object(setq ent_z(car(setq ent_pt(entsel "\n Укажите обект")))))) number (rtos(car(text_to_spisok layer_name 1))2 0) ent_pt (car(cdr ent_pt)) pt2(getpoint "укажите точку вставки текста"))
(cond	((wcmatch layer_name "*prit*")(setq layer_name (strcat "П" number)))
		((wcmatch layer_name "*Vit*")(setq layer_name (strcat "В" number)))
)
(if (setq spisok1(ssnamex (ssget "_F" (list pt2 (polar pt2(angle pt2 ent_pt)(* 2(distance pt2 ent_pt)))))))
	(if	(setq spisok1(car(vl-remove-if-not '(lambda(x)(equal(cadr x) ent_z))spisok1)))
		(setq pt3(cadr(car(vl-remove-if-not 'listp spisok1))))
	)
)
(if (null pt3) (setq pt3 ent_pt))
(setq vla_mleader
	(vla-addmleader 
		(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
			(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
			(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
		)
		(ru-mleader-coords-from-list(list pt3 pt2))
		0
	)
)
(vla-put-TextString vla_mleader layer_name)
(vla-put-textheight vla_mleader bit_zod84)
)



(defun c:zod85 ( / sel summ)
(princ "\n Считает диаметр гребенки \n")
(setq summ 0.0)
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual sel '((0 . "TEXT,MTEXT,MULTILEADER")))
(vlax-for x sel	(setq summ (+ summ(* (car(text_to_spisok(vla-get-textstring x)1))(car(text_to_spisok(vla-get-textstring x)1))))))
(sqrt summ)
)

(defun c:zod86 ( / sel spisok1)
(princ "\n суммирует диаметры \n")
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual sel '((0 . "TEXT,MTEXT,MULTILEADER")))
(vlax-for x sel	(setq spisok1(append spisok1 (list(list (rtos(car(text_to_spisok(vla-get-textstring x)1))2 0) 1)))))
(vl-sort (sort_and_sum_list spisok1) '(lambda(x1 x2)(< (car x1)(car x2))))
)


(defun c:zod87 ( / flag1  slovo_1		slovo_2		ent_z		bl_name 	string_1  ent_name  vla_mleader  spisok1  pt3  pt2  ent_pt  blk_ename )
(if (null bit_zod87)(setq bit_zod87 (getreal "\n введите высоту текста\n")))
(while (null flag1)
	(if (equal ""(setq slovo_1(getstring "\n Введите искомый текст  \n")))
		(setq flag1 t)
	)
	(setq slovo_2(getstring "\n Введите на что нужно заменить   \n"))
	(if	(null flag1)
		(setq Spisok_zod87(append Spisok_zod87 (list(list slovo_1 slovo_2))))
	)
)
(setq Spisok_zod87(mapcar '(lambda(x)(list (strcat "*"(car x)"*")(cadr x)))Spisok_zod87))
(setq bl_name(vla-get-EffectiveName(setq blk_ename(vlax-ename->vla-object (setq ent_z(car(setq ent_pt(entsel))))))) ent_pt (car(cdr ent_pt)) pt2(getpoint "укажите точку вставки текста"))
(mapcar '(lambda(x)(if(wcmatch bl_name (car x))(setq string_1 (cadr x))))Spisok_zod87)
(mapcar '(lambda(z) 
	(cond	((equal(vla-get-PropertyName z) "Shirina")		(setq Shirina	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Visota")		(setq Visota	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Diametr")		(setq Diametr	(vlax-variant-value(vla-get-Value z)))		)
			
	)	)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties blk_ename)))
)
(if Shirina 	(if (equal (type Shirina) 'STR)		(setq Shirina(atoi Shirina))))
(if Visota 		(if (equal (type Visota) 'STR)		(setq Visota(atoi Visota))))
(if Diametr 	(if (equal (type Diametr) 'STR)		(setq Diametr1(atoi Diametr))))
(if	Diametr
			(setq string_1 (strcat string_1 " %%C" (rtos Diametr 2 0)))
			(setq string_1(strcat string_1 " "(rtos Shirina  2 0)"x"(rtos Visota 2 0)))
)
(setq string_1(vl-string-trim " " string_1))
(if (setq spisok1(ssnamex (ssget "_F" (list pt2 (polar pt2(angle pt2 ent_pt)(* 2(distance pt2 ent_pt)))))))
	(if	(setq spisok1(car(vl-remove-if-not '(lambda(x)(equal(cadr x) ent_z))spisok1)))
		(setq pt3(cadr(car(vl-remove-if-not 'listp spisok1))))
	)
)
(if (null pt3) (setq pt3 ent_pt))
(setq vla_mleader
	(vla-addmleader 
		(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
			(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
			(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
		)
		(ru-mleader-coords-from-list(list pt3 pt2))
		0
	)
)
(vla-put-TextString vla_mleader string_1)
(vla-put-textheight vla_mleader bit_zod87)
)


(defun ru-mleader-coords-from-list (list_points / n pt )
(setq pt_array
             (vlax-make-safearray vlax-vbdouble (cons 1 (* 3 (length list_points))))
            n 0
      )
      (while (< n (length list_points))
        (setq pt (nth n list_points))
        (vlax-safearray-put-element pt_array (+ (* n 3) 1) (car pt))
        (vlax-safearray-put-element pt_array (+ (* n 3) 2) (cadr pt))
        (vlax-safearray-put-element pt_array (+ (* n 3) 3) (caddr pt))
        (setq n (1+ n))
      )
  pt_array
 )
 
 
 
 
 (defun c:zod87 ( / flag1  slovo_1		slovo_2		ent_z		bl_name 	string_1  ent_name  vla_mleader  spisok1  pt3  pt2  ent_pt  blk_ename )
(if (null bit_zod87)(setq bit_zod87 (getreal "\n введите высоту текста\n")))
(while (null flag1)
	(if (equal ""(setq slovo_1(getstring "\n Введите искомый текст  \n")))
		(setq flag1 t)
	)
	(setq slovo_2(getstring "\n Введите на что нужно заменить   \n"))
	(if	(null flag1)
		(setq Spisok_zod87(append Spisok_zod87 (list(list slovo_1 slovo_2))))
	)
)
(setq Spisok_zod87(mapcar '(lambda(x)(list (strcat "*"(car x)"*")(cadr x)))Spisok_zod87))
(setq bl_name(vla-get-EffectiveName(setq blk_ename(vlax-ename->vla-object (setq ent_z(car(setq ent_pt(entsel))))))) ent_pt (car(cdr ent_pt)) pt2(getpoint "укажите точку вставки текста"))
(mapcar '(lambda(x)(if(wcmatch bl_name (car x))(setq string_1 (cadr x))))Spisok_zod87)
(mapcar '(lambda(z) 
	(cond	((equal(vla-get-PropertyName z) "Shirina")		(setq Shirina	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Visota")		(setq Visota	(vlax-variant-value(vla-get-Value z)))		)
			((equal(vla-get-PropertyName z) "Diametr")		(setq Diametr	(vlax-variant-value(vla-get-Value z)))		)
			
	)	)
	(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties blk_ename)))
)
(if Shirina 	(if (equal (type Shirina) 'STR)		(setq Shirina(atoi Shirina))))
(if Visota 		(if (equal (type Visota) 'STR)		(setq Visota(atoi Visota))))
(if Diametr 	(if (equal (type Diametr) 'STR)		(setq Diametr1(atoi Diametr))))
(if	Diametr
			(setq string_1 (strcat string_1 " %%C" (rtos Diametr 2 0)))
			(setq string_1(strcat string_1 " "(rtos Shirina  2 0)"x"(rtos Visota 2 0)))
)
(setq string_1(vl-string-trim " " string_1))
(if (setq spisok1(ssnamex (ssget "_F" (list pt2 (polar pt2(angle pt2 ent_pt)(* 2(distance pt2 ent_pt)))))))
	(if	(setq spisok1(car(vl-remove-if-not '(lambda(x)(equal(cadr x) ent_z))spisok1)))
		(setq pt3(cadr(car(vl-remove-if-not 'listp spisok1))))
	)
)
(if (null pt3) (setq pt3 ent_pt))
(setq vla_mleader
	(vla-addmleader 
		(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
			(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
			(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
		)
		(ru-mleader-coords-from-list(list pt3 pt2))
		0
	)
)
(vla-put-TextString vla_mleader string_1)
(vla-put-textheight vla_mleader bit_zod87)
(princ)
)



(defun c:zod88 ( / ss1 sp_sloev)
(princ "\n Выберете примитивы с нужными слоями ")
(setq ss1 (vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget)))) sp_sloev "")
(foreach ename ss1 
	(if	(not(wcmatch (cdr(assoc 8 (entget ename)))sp_sloev))
		(setq sp_sloev (strcat sp_sloev "," (cdr(assoc 8 (entget ename)))))
	)
)
(setq sp_sloev(vl-string-trim "," sp_sloev))
(princ "\n Выберете примитивы ")
(sssetfirst nil (ssget (list(cons 8  sp_sloev))))
(princ)
)

(defun c:zod888 ( / ss1 sp_sloev)
(if (null sp_sloev_zod88 )
	(progn
		(princ "\n Выберете примитивы с нужными слоями ")
		(setq ss1 (vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget)))) sp_sloev "")
		(foreach ename ss1 
			(if	(not(wcmatch (cdr(assoc 8 (entget ename)))sp_sloev))
				(setq sp_sloev (strcat sp_sloev "," (cdr(assoc 8 (entget ename)))))
			)
		)
		(setq sp_sloev_zod88(vl-string-trim "," sp_sloev))
	)
)
(princ "\n Выберете примитивы ")
(sssetfirst nil (ssget (list(cons 8  sp_sloev_zod88))))
(princ)
)



(defun c:zod8888 (/)
(princ "\n Обнуляет список ")
(setq sp_sloev_zod88 nil)
(princ)
)



(defun c:zod89 ( / pref num1 num2 num3 num4 post num5)
(setq pref (getstring	"\nВведите префикс для названия слоя: "))
(setq num1 (getreal		"\nВведите начало отсчета "))
(setq num2 (getint		"\nВведите количество слоев "))
(setq num3 (getint		"\nВведите шаг "))
(setq num4 (getint		"\nВведите количество чисел в названии 1/01/001/0001(соответсвенно 1/2/3/4) "))
(setq post (getstring	"\nВведите постфикс для названия слоя: "))
(repeat num2
	(setq num5 (rtos num1 2 0))
	(if (<	(strlen num5)
			num4
		)
		(while 	(<(strlen num5)num4)
				(setq num5 (strcat "0" num5))
		)
	)
	(if	(null(tblobjname "Layer" (strcat pref num5 post)))
		(vla-add	(vla-get-Layers(vla-get-activedocument(vlax-get-acad-object)))
					(strcat pref num5 post)
		)
	)
	(setq num1 (+ num1 num3))
)
)


(defun c:zod90 ( / sel m_scale)
(princ "\n масштабирует объекты по центру \n")
(setq m_scale (getreal		"\nВведите масштаб "))
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual sel '((0 . "*")))
(vlax-for x sel
	(vla-GetBoundingBox x 'pt1 'pt2)
	(setq pt1  (vlax-safearray->list pt1)  pt2 (vlax-safearray->list pt2))
	(setq pt3 (list	(/(+(car pt1 )	(car pt2 ))2)
					(/(+(cadr pt1 )	(cadr pt2 ))2)
					(/(+(caddr pt1 )(caddr pt2 ))2)
				)
	)
	(vla-ScaleEntity x (vlax-3d-point pt3) m_scale)
)
(princ)
)


(defun c:zod92 ( / i ss1 lst)
(princ "\nВыделите обекты ,слои которых нужно разблокировать")
(setq	i	0 ss1 (ssget))
(repeat (sslength ss1)
	(if	(not(member (cdr(assoc 8(entget(ssname ss1 i)))) lst))
		(setq lst (append lst(list(cdr(assoc 8(entget(ssname ss1 i)))))))
	)
	(setq i (1+ i))
)
(foreach x lst
	(entmod		
		(subst 	'(70 . 0)
				(assoc 70 (entget(tblobjname "layer" x)))
				(entget(tblobjname "layer" x))
		)
	)
)
(princ)
(vla-regen (vla-get-activedocument(vlax-get-acad-object)) acallviewports)
)


(defun c:zod922 ( / i ss1 lst)
(princ "\nВыделите обекты ,слои которых нужно заблокировать")
(setq	i	0 ss1 (ssget))
(repeat (sslength ss1)
	(if	(not(member (cdr(assoc 8(entget(ssname ss1 i)))) lst))
		(setq lst (append lst(list(cdr(assoc 8(entget(ssname ss1 i)))))))
	)
	(setq i (1+ i))
)
(foreach x lst
	(entmod		
		(subst 	'(70 . 4)
				(assoc 70 (entget(tblobjname "layer" x)))
				(entget(tblobjname "layer" x))
		)
	)
)
(vla-regen (vla-get-activedocument(vlax-get-acad-object)) acallviewports)
(princ)
)

(defun c:zod93 ( / ss1)
(princ "\n не знаю что делает")
(setq	ss1 (mapcar '(lambda (obj)(vlax-ename->vla-object obj))(vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget))))))
(mapcar '(lambda (x)
	(if (vlax-property-available-p x 'textstring)
		(vlax-put-property x 'textstring (iz_texta_4_texta x))
	)
		)
ss1
)
(princ)
)


(defun iz_texta_4_texta (ename_zzz / ish_text text1 text2 text3 text4)
(setq ish_text (vl-string-trim " "(mip_MTEXT_Unformat(vlax-get-property ename_zzz 'textstring))))
(while (vl-string-search "\t" ish_text)
	(setq ish_text(vl-string-subst "" "\t" ish_text))
)
(while (vl-string-search "  " ish_text)
	(setq ish_text(vl-string-subst " " "  " ish_text))
)
(setq text1 (vl-string-trim " "(substr ish_text 1 (1+(vl-string-search " " ish_text)))))
(setq ish_text(vl-string-trim " "(substr ish_text (+ 2(vl-string-search " " ish_text)))))
(setq text2 (vl-string-trim " "(substr ish_text 1 (1+(vl-string-search " " ish_text)))))
(setq ish_text(vl-string-trim " "(substr ish_text (+ 2(vl-string-search " " ish_text)))))
(setq text3 (vl-string-trim " "(substr ish_text 1 (1+(vl-string-search " " ish_text)))))
(setq ish_text(vl-string-trim " "(substr ish_text (+ 2(vl-string-search " " ish_text)))))
(setq text4 (vl-string-trim " " ish_text))
(strcat text1 " n=" text2 "\\P" "L="  text3 "м" " Q=" text4 "Вт")
)

(defun iz_texta_4_spisok (ename_zzz / ish_text spisok1)
(setq ish_text (vl-string-trim " "(mip_MTEXT_Unformat(vlax-get-property ename_zzz 'textstring))))
(if (vl-string-search "\t" ish_text)
	(while (vl-string-search "\t" ish_text)
		(setq ish_text(vl-string-subst "" "\t" ish_text))
	)
)
(if (vl-string-search "\\P" ish_text)
	(while (vl-string-search "\\P" ish_text)
		(setq ish_text(vl-string-subst "" "\\P" ish_text))
	)
)
(if (vl-string-search "  " ish_text)
	(while (vl-string-search "  " ish_text)
		(setq ish_text(vl-string-subst " " "  " ish_text))
	)
)
(setq ish_text(vl-string-trim " " ish_text))
(while (vl-string-search " " ish_text)
		(setq	spisok1 (append spisok1(list(vl-string-trim " "(substr ish_text 1 (1+(vl-string-search " " ish_text))))))
				ish_text(vl-string-trim " "(substr ish_text (+ 2(vl-string-search " " ish_text))))
		)
)
(setq	spisok1 (append spisok1(list ish_text)))
)

(defun iz_spiska_textov_1text (spisok_textov nomer_po_spisku / )
(nth (1- nomer_po_spisku) spisok_textov)
)

(defun c:zod94 ()
(setq bit_zod94 (getreal "\n введите число на которое будет умножаться \n"))
(setq bit2_zod94 (getint "\n введите номер текста, разделенного пробелами \n"))
)
	
(defun c:zod944 ( / ss1 layer_name aaa)
(setq	ss1 (vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget '((0 . "TEXT,MTEXT,MULTILEADER")))))))
(if	(null(tblobjname "Layer" (setq layer_name(strcat(cdr(assoc 8 (entget (nth 0 ss1)))) "_111"))))
	(vla-add(vla-get-Layers(vla-get-activedocument(vlax-get-acad-object)))layer_name)
)
(mapcar
	'(lambda (x)
		(vla-put-Layer
			(vla-addtext 	
				(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
					(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
					(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
				)
				(if (equal (length (iz_texta_4_spisok(vlax-ename->vla-object x))) bit2_zod94)
					(* bit_zod94 (text_to_number(iz_spiska_textov_1text(iz_texta_4_spisok(vlax-ename->vla-object x))bit2_zod94)))
					(setq aaa 999999)
				)
				(vlax-3d-point(list (cadr (assoc 10(entget x)))(caddr (assoc 10(entget x)))(cadddr (assoc 10(entget x))))) 
				180
			)
		layer_name
		)
	)
ss1
)
(princ)
)



(defun c:zod95 ()
(if (or(null zod95_nomer_slova_del)(null zod95_kol_slov))  (zod955))
(princ "\n Выделите обекты  в которых нужно удалить текст \n")
(setq	ss1 (mapcar '(lambda (obj)(vlax-ename->vla-object obj))(vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget))))))
(mapcar '(lambda (x)
	(if (vlax-property-available-p x 'textstring)
		(if (equal zod95_kol_slov (length(iz_stroki_v_spisok(oborabotka_stroki(vlax-get-property x 'textstring)))))
			(vlax-put-property x 'textstring (iz_spiska_v_stoku(udalenie_elementa_po_nomeru zod95_nomer_slova_del (iz_stroki_v_spisok(oborabotka_stroki(vlax-get-property x 'textstring))))))
		)
	)
		)
ss1
)
(princ)
)



(defun c:zod955 ( / )(zod955))

(defun zod955 ( / ent1 flag1 flag2 text1 kol_slov spisok1)
(setq zod95_nomer_slova_del nil zod95_kol_slov nil)
(while (null flag1)
	(if (vlax-property-available-p (setq ent1 (vlax-ename->vla-object(car(entsel "\n Укажите обект, содержащий текст " )))) 'textstring)
		(setq flag1 t spisok1 (iz_stroki_v_spisok(oborabotka_stroki(vlax-get-property ent1 'textstring))) zod95_kol_slov (length spisok1))
	)
)
(princ(strcat " В примитиве  " (rtos (length (iz_texta_4_spisok ent1))2 0) "  текста/ов    " ))
(setq spisok1 spisok1)
(while (null flag2)
	(setq zod95_nomer_slova_del (getreal "\n введите номер текста для удаления \n"))
	(if (and	(< 0 zod95_nomer_slova_del)
				(< zod95_nomer_slova_del (1+ zod95_kol_slov))
		)
		(setq flag2 t)
	)
)
(iz_spiska_v_stoku(udalenie_elementa_po_nomeru zod95_nomer_slova_del spisok1))
)


(defun iz_stroki_v_spisok ( ish_text / spisok1)
(while (vl-string-search " " ish_text)
		(setq	spisok1 (append spisok1(list(vl-string-trim " "(substr ish_text 1 (1+(vl-string-search " " ish_text))))))
				ish_text(vl-string-trim " "(substr ish_text (+ 2(vl-string-search " " ish_text))))
		)
)
(setq	spisok1 (append spisok1(list ish_text)))
)

(defun oborabotka_stroki (ish_text / )
(setq ish_text (vl-string-trim " "(mip_MTEXT_Unformat ish_text)))
(if (vl-string-search "\t" ish_text)
	(while (vl-string-search "\t" ish_text)
		(setq ish_text(vl-string-subst "" "\t" ish_text))
	)
)
(if (vl-string-search "\\P" ish_text)
	(while (vl-string-search "\\P" ish_text)
		(setq ish_text(vl-string-subst "" "\\P" ish_text))
	)
)
(if (vl-string-search "  " ish_text)
	(while (vl-string-search "  " ish_text)
		(setq ish_text(vl-string-subst " " "  " ish_text))
	)
)
(setq ish_text(vl-string-trim " " ish_text))
)

(defun iz_spiska_v_stoku (ish_spisok / stroka)
(setq stroka "")
(mapcar '(lambda(x)(setq stroka (strcat stroka " " x)))ish_spisok)
(setq stroka (vl-string-trim " " stroka))
)

(defun udalenie_elementa_po_nomeru ( nomer ish_spisok / spisok1 kol_elem)
(setq kol_elem 0)
(while (> (-(length ish_spisok)kol_elem) 0)
	(if (equal (1+ kol_elem) nomer)
		(setq spisok1 spisok1)
		(setq spisok1(append spisok1 (list(nth kol_elem ish_spisok))))
	)
	(setq kol_elem(1+ kol_elem))
)
(setq spisok1 spisok1)
)



(defun c:zod966 ( / )(zod966))

(defun zod966 ( / ent1 flag1 flag2 text1 kol_slov qq)
(setq 	zod96_kol_slov nil	zod96_spisok2 nil)
(while (null flag1)
	(if (vlax-property-available-p (setq ent1 (vlax-ename->vla-object(car(entsel "\n Укажите обект, содержащий текст " )))) 'textstring)
		(setq flag1 t spisok1 (iz_stroki_v_spisok(oborabotka_stroki(vlax-get-property ent1 'textstring))) zod96_kol_slov (length spisok1))
	)
)
(princ(strcat " В примитиве  " (rtos (length (iz_texta_4_spisok ent1))2 0) "  текста/ов    " ))
(setq spisok1 spisok1)
(initget "клапаны  диаметры диаметры2 приборы Точки")
(setq qq (getkword "\nЧто вставить [клапаны/диаметры/диаметры2/приборы/Точки]: "))
(cond
((= qq "клапаны")		(setq zod96_spisok2 '("" " n=" " %%C" "")))
((= qq "диаметры")		(setq zod96_spisok2 '("%%C" " Q=" "Вт v=" "м/с")))
((= qq "диаметры2")		(setq zod96_spisok2 '("%%C" "\\PQ=" "Вт\\PG=" "л/с")))
((= qq "приборы")		(setq zod96_spisok2 '("" " n=" "\\PL=" "м Q=" "Вт")))
((= qq "Точки")			(setq zod96_spisok2 '("" "\\P" "")))
)
(soedinenie_2_spiskov_v_stroku zod96_spisok2 spisok1)
)

(defun c:zod96 ( / ss1 )
(if (or(null zod96_kol_slov)(null zod96_spisok2))  (zod966))
(setq	ss1 (mapcar '(lambda (obj)(vlax-ename->vla-object obj))(vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget))))))
(mapcar '(lambda (x)
	(if (vlax-property-available-p x 'textstring)
		(if (equal zod96_kol_slov (length(iz_stroki_v_spisok(oborabotka_stroki(vlax-get-property x 'textstring)))))
			(vlax-put-property x 'textstring (soedinenie_2_spiskov_v_stroku zod96_spisok2 (iz_stroki_v_spisok(oborabotka_stroki(vlax-get-property x 'textstring)))))
		)
	)
		)
ss1
)
(princ)
)


(defun soedinenie_2_spiskov_v_stroku (spisok1 spisok2 / text_1)
(setq spisok2(append spisok2 '("")) text_1 "")
(mapcar '(lambda(x y)
	(setq text_1(strcat text_1 x y))
		)
spisok1 spisok2
)
(setq text_1 text_1)
)
	


(defun c:zod97 ( / i ss1 lst x lst2 stroka cur_layer list_of_layer)
(setq	i	0 ss1 (ssget))
(repeat (sslength ss1)
	(if	(not(member (cdr(assoc 8(entget(ssname ss1 i)))) lst))
		(setq lst (append lst(list(cdr(assoc 8(entget(ssname ss1 i)))))))
	)
	(setq i (1+ i))
)
(setq lst (vl-remove-if-not '(lambda (x) (wcmatch x "DUCT-*")) lst))
(setq lst
	(mapcar '(lambda (x)
		(if
			(vl-string-search "-" x 5)
			(substr x 1 (vl-string-search "-" x 5))
			(setq x x)
		)
			)
	lst)
)
(foreach x lst
	(if (not(member x lst2))
		(setq lst2 (append lst2(list x)))
	)
)
(setq lst2(vl-sort lst2 '<))
(setq stroka "")
(mapcar '(lambda(x)(setq stroka (strcat x  "*," stroka)))lst2)
(setq stroka(vl-string-right-trim "," stroka))
(setvar "CLAYER" (car lst2))
(setq list_of_layer (list (cdr (assoc 2 (tblnext "Layer" t)))))
(while (setq cur_layer (tblnext "Layer"))
	(setq list_of_layer (append list_of_layer (list (cdr (assoc 2 cur_layer)))))
)
(foreach x list_of_layer
	(if	(not(wcmatch x stroka))
		(entmod		
			(subst 	'(70 . 1)
					(assoc 70 (entget(tblobjname "layer" x)))
					(entget(tblobjname "layer" x))
			)
		)
	)
)
(princ)
)


(defun c:zod977 ()
(setvar "cmdecho" 0)
(command "_laythw")
(princ "\nВсе слои включены")
(princ)
)






(defun c:zd102 ( / flag1  	ent_z	string_1  vla_mleader  spisok1  pt3  pt2  ent_pt  )
(if (null bit_zod101)(setq bit_zod101 (getreal "\n введите высоту текста\n")))
(setq ent_z(car(setq ent_pt(entsel "\n Укажите линию"))) ent_pt (car(cdr ent_pt)) pt2(getpoint "\n укажите точку вставки текста"))
(setq string_1(strcat "%%C"(rtos(cdr(assoc 62(entget ent_z)))2 0)))
(if (setq spisok1(ssnamex (ssget "_F" (list pt2 (polar pt2(angle pt2 ent_pt)(* 2(distance pt2 ent_pt)))))))
	(if	(setq spisok1(car(vl-remove-if-not '(lambda(x)(equal(cadr x) ent_z))spisok1)))
		(setq pt3(cadr(car(vl-remove-if-not 'listp spisok1))))
	)
)
(if (null pt3) (setq pt3 ent_pt))
(setq vla_mleader
	(vla-addmleader 
		(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
			(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
			(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
		)
		(ru-mleader-coords-from-list(list pt3 pt2))
		0
	)
)
(vla-put-TextString vla_mleader string_1)
(vla-put-textheight vla_mleader bit_zod101)
)


(defun ru-mleader-coords-from-list (list_points / n pt )
(setq pt_array
             (vlax-make-safearray vlax-vbdouble (cons 1 (* 3 (length list_points))))
            n 0
      )
      (while (< n (length list_points))
        (setq pt (nth n list_points))
        (vlax-safearray-put-element pt_array (+ (* n 3) 1) (car pt))
        (vlax-safearray-put-element pt_array (+ (* n 3) 2) (cadr pt))
        (vlax-safearray-put-element pt_array (+ (* n 3) 3) (caddr pt))
        (setq n (1+ n))
      )
  pt_array
 )

 
(defun c:zd104 (/ spisok_layots adoc n m x y)
(setq spisok_layots(cdr(ttc-layouts-list)) adoc(vla-get-activedocument (vlax-get-acad-object)) n 999 m 1)
(mapcar '(lambda(x)
		(vla-put-name (vla-item(vla-get-layouts adoc) x)(rtos n 2 0))
		(setq n (1- n))
	)
spisok_layots
)
(setq spisok_layots(cdr(ttc-layouts-list)))
(mapcar '(lambda(y)
		(vla-put-name (vla-item(vla-get-layouts adoc) y)(rtos m 2 0))
		(setq m (1+ m))
	)
spisok_layots
)
(princ)
)

создание выноски по 1 или 2 текстам
(defun c:zd105 (/ ent1 ent2 text1 text2 text3 vla_mleader)
(setq 	ent1 (car(entsel)) 
		ent2 (car(entsel))
text1 (mip_MTEXT_Unformat (vla-get-textstring(vlax-ename->vla-object ent1)))
)
(if (null ent2)
	(setq text3 text1)
	(setq	text2 (mip_MTEXT_Unformat (vla-get-textstring(vlax-ename->vla-object ent2)))
			text3 (strcat text1 "\\P" text2)
	)
)
(setq pt1 (getpoint) pt2 (getpoint))
(setq vla_mleader
	(vla-addmleader 
		(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
			(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
			(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
		)
		(ru-mleader-coords-from-list(list pt1 pt2))
		0
	)
)
(vla-put-TextString vla_mleader text3)
(vla-put-textheight vla_mleader 350)
)


;выделение мультитекста и убирание заливки заднего плана
(defun c:zd106 (/ sel x)
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-all sel '((0 . "MTEXT")))
(vlax-for x sel
	(vla-put-BackgroundFill x 0)
)
)


(defun c:zd107 ( / blocks1 zzz spisok1 spisok2 num1 n m flag1 spisok3 sel)
(setq sel nil)
(setq zzz (vl-string-elt "*" 0) num1 3 n 0 m 0 spisok3 "")
(setq blocks1 (vla-get-blocks(vla-get-activedocument (vlax-get-acad-object))))
(vlax-for blk_def blocks1
	(if (equal (vla-get-isxref blk_def) :vlax-false)
			(setq spisok1(append spisok1 (list(get_name_block01 blk_def))))
	  )
)
(setq spisok1(vl-sort (vl-remove-if(function (lambda(x)(= (vl-string-elt x 0) zzz)))spisok1) '<))
(while (< n (length spisok1))
	(while (and (null flag1) (< m (length spisok1)))
		(if 	(and (/= n m)(> (vl-string-mismatch (nth n spisok1) (nth m spisok1) 0 0 T) num1))
				(setq flag1 T 
					spisok2	(append spisok2 (list 
						(vl-string-subst 
							"*" 
							(substr (nth n spisok1)(vl-string-mismatch (nth n spisok1) (nth m spisok1) 0 0 T)) 
							(nth n spisok1)))) 
				)
				(setq m (1+ m))
		)
	)
(setq n (1+ n) flag1 nil m 0)
)
(setq spisok2 (_kpblc-list-dublicates-remove spisok2))
(mapcar '(lambda(x)(setq spisok3 (strcat spisok3 "," x ))) spisok2)
(setq spisok3(vl-string-left-trim "," spisok3))
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-all sel (list '(0 . "insert") (cons 2 spisok3)))
(vlax-for x sel
	(vla-Explode x )
)
(vlax-for x sel
	(vla-erase x )
)
(vla-clear sel)
(vla-erase sel)
(princ)
)




(defun _kpblc-list-dublicates-remove (lst / result)
(foreach x lst
	(if (not (member x result))
		(setq result (cons x result))
	)
)
(reverse result)
)




(defun c:zd108 ( / sel ss1 ss2)
(princ "\n извлекает название динамического блока, названия динамических свойств в блоке и их значения")
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual sel '((0 . "INSERT")))
(vlax-for x sel
	(if (equal (vla-get-IsDynamicBlock x) :vlax-true)
		(progn
			(mapcar '(lambda(z) 
				(setq ss2 (append  ss2(list(list (vla-get-PropertyName z ) (if (numberp(vlax-variant-value(vla-get-Value z)))
																				(rtos (vlax-variant-value(vla-get-Value z)))
																				(vlax-variant-value(vla-get-Value z))
																			)
											)
										)
							)
					)
				)
				(vlax-safearray->list(vlax-variant-value(vla-GetDynamicBlockProperties x)))
			)
			(setq ss2 (vl-remove-if '(lambda(y)(equal (car y) "Origin")) ss2))
			(setq ss2 (append (list(list "название блока"(vla-get-EffectiveName x)))ss2))
			(setq ss1 (append ss1 (list ss2)))
			(setq ss2 nil)
		)
	)
)
(princ)
(setq ss1 ss1)
)






(defun SetScale( / lst pat tmp)
;;; pat - шаблон маштабов состоит из списков вида
;;;  (("имя в списке масштабов1" Масштаб_единицы_листа1 Масштаб_единицы_чертежа1)
;;;   ("имя в списке масштабов2"  Масштаб_единицы_листа2 Масштаб_единицы_чертежа2)
;;;   ...
;;;   )
  (setq pat '(("1:1" 1 1)("1:2" 1 2)("1:10" 1 10)
	      ("1:50" 1 50)("1:100" 1 100)("2:1" 2 1)
	      ("1:11" 1 11)("Мой" 21.5 8.133)))
  ;;;Удаляем не входящие в шаблон масштаб
  (setq tmp (mapcar 'car pat))
(if (GETCNAME "_SCALELISTEDIT")
  (progn
    (COMMAND "_-SCALELISTEDIT" "_R" "_Y" "_E")
    (setq lst nil)
    (foreach item (dictsearch (namedobjdict) "ACAD_SCALELIST")
      (if (= 350 (car item))
     (setq lst (cons (cdr(assoc 300 (entget(cdr item)))) lst))
    ) ;_ end of if
  )
  (while (> (getvar "CMDACTIVE") 0) (command))
    (command "_.-SCALELISTEDIT")
  (foreach item lst
    (command "_D" item)
    )
    (command "_E")
   (setq lst nil) 
  ;;;Список оставшихся масштабов
   (foreach item (dictsearch (namedobjdict) "ACAD_SCALELIST")
    (if	(= 350 (car item))
      (setq lst (cons (cdr(assoc 300 (entget (cdr item)))) lst))
    ) ;_ end of if
  ) ;_ end of foreach
)
  )
    
  ;;;Список не созданных масштабов из шаблона pat
   (if (and lst (setq pat (vl-remove-if '(lambda(x)(member (car x) lst)) pat)))
     (progn
       (while (> (getvar "CMDACTIVE") 0)(command))
       (command "_.-scalelistedit")
       (foreach item pat
	 (command "_Add" (car item) (strcat (rtos (cadr item)) ":" (rtos (caddr item))))
	 ) ;_ end of foreach
       (command "_Exit")
       (while (> (getvar "CMDACTIVE") 0)(command))
       )
     )
  (princ)
   )
(defun C:SetScale ()(SetScale))

;;;извлекает текст из 2 текстов и заменяет в 3 добавляя перенос строки
(defun zd109 ( / )
(princ "\n извлекает текст из 2 текстов и заменяет в 3 добавляя перенос строки")
(vla-put-textstring (vlax-ename->vla-object (car(entsel "\nВыделите итоговый текст"))) (Strcat (mip_MTEXT_Unformat2(zod_get_text "\nВыделите 1 текст")) "\\P" (mip_MTEXT_Unformat2(zod_get_text "\nВыделите 2 текст"))))
(princ)
)
(defun C:zd109 ()(zd109))

;;;извлекает текст из примитива если у него нет свойства textstring то дает пустой текст ""
(defun zod_get_text (bolvanka / zent ztext)
(setq ztext "")
(if	(setq zent(car(entsel bolvanka)))
	(if (vlax-property-available-p (vlax-ename->vla-object zent) 'textstring)
		(setq ztext(vla-get-textstring(vlax-ename->vla-object zent)))
	)
)
(setq ztext ztext)
)




;;;переносит содержимое всех видовых экранов в текущем виде на 
(defun zd110 ( / )
(princ "\n переносит содержимое всех видовых экранов в текущем виде ")
(if (or (null perenosxzd110) (null perenosyzd110) )
	(setq 	perenosxzd110 (getreal "\n Введите чило для переноса по оси Х ")
			perenosyzd110 (getreal "\n Введите чило для переноса по оси Y ")
	)
)
(vl-cmdf "_pan" (list perenosxzd110 perenosyzd110 0) '(0 0 0))
(vla-put-MSpace (vla-get-activedocument(vlax-get-acad-object))0)
(princ)
)
(defun C:zd110 ()(zd110))
;(defun C:zz ()(zd110))

(defun zd1100 ( / )
(setq 	perenosxzd110 (getreal "\n Введите чило для переноса по оси Х ")
		perenosyzd110 (getreal "\n Введите чило для переноса по оси Y ")
)
(princ)
)
(defun C:zd1100 ()(zd1100))



;;;выделяет полилинии длина которых 2300
(defun c:zd111 ( / ss2 dlinazz)
(setq 	dlinazz (getreal "\n Введите искомую длину"))
(setq ss2(ssadd))
(mapcar
          '(lambda (x)(if 	(and 	(< (vla-get-length (vlax-ename->vla-object x)) (1+ dlinazz))
									(> (vla-get-length (vlax-ename->vla-object x)) (1- dlinazz))
							)
							(setq ss2(ssadd  x ss2))
			     	)
			 )
	(vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget '((0 . "LWPOLYLINE"))))))		 
)
(sssetfirst nil ss2)
(princ)
)

;;;в мультивыносках делает текст по слою
(defun c:zd112 ( / ss1)
(princ "\n в мультивыносках делает текст по слою")
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "MULTILEADER")))
(vlax-for x ss1 (vla-put-textstring x (strcat "{\\C256;" (mip_mtext_unformat2(vla-get-textstring x)) "}")))
(vla-clear ss1)
(princ)
)

;;;выделяет все видовые экраны в чертеже
(defun zd113 ( / )
(princ "\n выделяет все видовые экраны в чертеже")
(sssetfirst nil(ssget "_X" (list '(0 . "VIEWPORT")'(-4 . "<>")'(69 . 1))))
)
(defun C:zd113 ()(zd113))

;;; блокирует или разблокирует все экраны
(defun zd1133 ( flagzz / ss1)
(setq ss1(ssget "_X" (list '(0 . "VIEWPORT")'(-4 . "<>")'(69 . 1))))
(mapcar  
	'(lambda (zz)   
		(cond	
			(	(equal flagzz 1)		(vla-put-DisplayLocked (vlax-ename->vla-object zz)	:vlax-true)			)
			(	(equal flagzz 2)		(vla-put-DisplayLocked (vlax-ename->vla-object zz)	:vlax-false)		)
			(	(equal flagzz 3)		(vla-put-ViewportOn (vlax-ename->vla-object zz) 	:vlax-true)			)
			(	(equal flagzz 4)		(vla-put-ViewportOn (vlax-ename->vla-object zz)		:vlax-false)		)
		)
	)           
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(cond
			(	(equal flagzz 1)		(princ "\n все экраны заблокированы \n")		)
			(	(equal flagzz 2)		(princ "\n все экраны разблокированы \n")		)
			(	(equal flagzz 3)		(princ "\n все экраны включены \n")		)
			(	(equal flagzz 4)		(princ "\n все экраны выключены \n")		)
)
(princ)
)
(defun C:zd1131 ()(zd1133 1))
(defun C:zd1132 ()(zd1133 2))
(defun C:zd1133 ()(zd1133 3))
(defun C:zd1134 ()(zd1133 4))


;;;выделяет все видовые экраны на текущем виде
(defun zd114 ( / )
(princ "\n выделяет все видовые экраны на текущем виде")
(sssetfirst nil(ssget "_X" (list '(0 . "VIEWPORT")'(-4 . "<>")'(69 . 1)(cons 410 (getvar "CTAB")))))
)
(defun C:zd114 ()(zd114))

(defun zd1144 ( flagzz / ss1)
(silent_mod_on )
(setq ss1(ssget "_X" (list '(0 . "VIEWPORT")'(-4 . "<>")'(69 . 1)(cons 410 (getvar "CTAB")))))
(mapcar  
	'(lambda (zz)   
		(cond	
			(	(equal flagzz 3)		(vla-put-ViewportOn (vlax-ename->vla-object zz) 	:vlax-true)			)
			(	(equal flagzz 4)		(vla-put-ViewportOn (vlax-ename->vla-object zz)		:vlax-false)		)
		)
	)           
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(cond
			(	(equal flagzz 3)		(princ "\n все экраны включены \n")		)
			(	(equal flagzz 4)		(princ "\n все экраны выключены \n")		)
)
(silent_mod_off)
(princ)
)
(defun C:zd1143 ()(zd1144 3))
(defun C:zd1144 ()(zd1144 4))




;;;удаляет все листы имя которых не совпадает с именем файла
(defun Zd115 ( / file_name Layout_name)
(princ "\n удаляет все листы имя которых не совпадает с именем файла")
(setvar "regenmode" 0)
(setq file_name(vl-string-subst "" ".dwg" (vla-get-name(vla-get-activedocument(vlax-get-acad-object)))))
(setvar "ctab" file_name)
(vlax-for Layout_name (vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))
	(if		(not	(or 	(equal (vla-get-name Layout_name) file_name)
							(equal (vla-get-name Layout_name) "Model")
					)
			)
			(vla-delete Layout_name)
	)
);_end_of_vlax-for
(setvar "regenmode" 1)
(vla-save(vla-get-activedocument(vlax-get-acad-object)))
;(vla-quit(vlax-get-acad-object))
(VL-CMDF "._close" "_y")
(princ)
)
;(defun C:Zd115 ()(Zd115))

;;;удаляет все листы имя которых не совпадает с именем файла
(defun Zd1155 ( / file_name Layout_name )
(princ "\n удаляет все листы имя которых не совпадает с именем файла")
(setvar "regenmode" 0)
(setq file_name(vl-string-subst "" ".dwg" (vla-get-name(vla-get-activedocument(vlax-get-acad-object)))))
(setvar "ctab" file_name)
(vlax-for Layout_name (vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))
	(if		(not	(or 	(equal (vla-get-name Layout_name) file_name)
							(equal (vla-get-name Layout_name) "Model")
					)
			)
			(vla-delete Layout_name)
	)
);_end_of_vlax-for
;(vla-put-ActiveSpace (vla-get-activedocument(vlax-get-acad-object)) 0)
(setvar "regenmode" 1)
(vla-save(vla-get-activedocument(vlax-get-acad-object)))
;(vla-quit(vlax-get-acad-object))
(VL-CMDF "._close" "_y")
(princ)
)
;(defun C:Zd1155 ()(Zd1155))


(defun Zd116 ( / )
(VL-CMDF "_-VPORTS" "_SI")
(princ)
)
(defun C:Zd116 ()(Zd116))

(defun Zd117 ( / )
(VL-CMDF "_-VPORTS" "2" "_Vertical")
(princ)
)
(defun C:Zd117 ()(Zd117))

;;;печать текущего листа по выставденным настройкам
(defun Zd118 ( / doc lstLayouts arSize arLayouts)
(setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
(setq lstLayouts nil
          arSize 0)
(setq lstLayouts (list (getvar "CTAB")))
(setq arLayouts (vlax-make-safearray vlax-vbString (cons 0 (1- (length lstLayouts)))))
(foreach item lstLayouts
	(progn
		(vlax-safearray-put-element arLayouts arSize item)
		(setq arSize (1+ arSize))
	)
)
(vla-SetLayoutsToPlot (vla-get-Plot doc) arLayouts)
(vla-PlotToDevice (vla-get-Plot doc))
(princ)
)
(defun C:Zd118 ()(Zd118))

;;; переименовывает блок
(defun Zd119 ( newnameblock / newnameblock)
(princ "\n переименовывает блок \n")
(if (equal "" newnameblock) (setq newnameblock (getstring "Введите новое имя блока: ")))
(vla-put-Name
  (vla-item
    (vla-get-Blocks
      (vla-get-ActiveDocument
        (vlax-get-acad-object)
      )
    )
  (vla-get-EffectiveName(vlax-ename->vla-object(car (entsel "Укажите нужный блок: "))))
	) ;;;<-- old block name
newnameblock) ;;;< -- new block name
(princ)
)
(defun C:Zd119 ()(Zd119 ""))

;Добавляет префикс ко всем блокам в чертеже
(defun Zd120 ( newnameblock / newnameblock block_name)
(princ "\n Добавляет префикс ко всем блокам в чертеже")
(if (equal "" newnameblock) (setq newnameblock (getstring "Введите префикс для всех блоков блока: ")))
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
	(vl-catch-all-apply 'vla-put-Name(list block_name (strcat newnameblock (vla-get-name block_name))))
);_end_of_vlax-for
(princ)
)
(defun C:Zd120 ()(Zd120 ""))


;Добавляет постфикс ко всем блокам в чертеже
(defun Zd1201 ( newnameblock / newnameblock block_name)
(princ "\n Добавляет суффикс ко всем блокам в чертеже")
(if (equal "" newnameblock) (setq newnameblock (getstring "Введите префикс для всех блоков блока: ")))
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
	(vl-catch-all-apply 'vla-put-Name(list block_name (strcat  (vla-get-name block_name)newnameblock)))
);_end_of_vlax-for
(princ)
)
(defun C:Zd1201 ()(Zd1201 ""))


;в мультитексте делает текст с заданой шириной и сжатием символов
(defun zd121 ( / ss1)
(princ "\n в мультитексте делает текст с заданой шириной и сжатием символов")
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "MULTILEADER,MTEXT")))
(vlax-for x ss1 (vla-put-textstring x (strcat "{\\W0.778;\\T1;" (mip_mtext_unformat2(vla-get-textstring x)) "}")))
(vla-clear ss1)
(princ)
)
(defun C:Zd121 ()(Zd121))


 ;делит текст на число и вставляет новый текст
(defun zd122( / numberzz kolpribzz01 entzz pointzz)
(princ "\n делит текст на число и вставляет новый текст")
(if (null kolpribzz01) (setq kolpribzz01 (* 1.0 (getreal "\n Введите чило на которое нужно делить: "))))
(Setq numberzz (rtos (/ (ent_to_number (setq entzz(car(entsel)))) kolpribzz01) 2 0))
(setq pointzz
	(list 
	(car(cdr(assoc 10 (entget entzz))))
	(-(cadr(cdr(assoc 10 (entget entzz))))500)
	(caddr(cdr(assoc 10 (entget entzz))))	
	)
)
(vla-addtext 	(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
			(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
			(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
		)
	numberzz
	(vlax-3d-point pointzz) 
	500
)
(princ)
)
(defun C:zd122 ()(zd122))





;Добавляет префикс ко всем слоям в чертеже
(defun Zd123 ( newnamelayer flag01 / newnamelayer layer_name)
(princ "\n Добавляет префикс/суффикс ко всем блокам в чертеже")
(if (equal "" newnamelayer) (setq newnamelayer (getstring "Введите префикс для всех слоев: ")))
(vlax-for layer_name (vla-get-layers(vla-get-activedocument(vlax-get-acad-object)))
	(if flag01
		(vl-catch-all-apply 'vla-put-Name(list layer_name (strcat newnamelayer (vla-get-name layer_name))))
		(vl-catch-all-apply 'vla-put-Name(list layer_name (strcat (vla-get-name layer_name) newnamelayer)))
	)
);_end_of_vlax-for
(princ)
)
(defun C:Zd1231 ()(Zd123 "" t))
(defun C:Zd1232 ()(Zd123 "" nil))

;Убирает слово во всем слоям в чертеже
(defun Zd1233 ( newnamelayer / newnamelayer layer_name)
(princ "\n Убирает слово во всем слоям в чертеже")
(if (equal "" newnamelayer) (setq newnamelayer (getstring "Введите слово для поиска во всех именах слоев: ")))
(vlax-for layer_name (vla-get-layers(vla-get-activedocument(vlax-get-acad-object)))
	(vl-catch-all-apply 'vla-put-Name(list layer_name (vl-string-subst  "" newnamelayer   (vla-get-name layer_name))))
);_end_of_vlax-for
(princ)
)
(defun C:Zd1233 ()(Zd1233 ""))


;Выделяет все примитивы  лежащих на слое
(defun Zd124 (  /  )
(princ "\n Выделяет все примитивы  лежащих на слое")
(sssetfirst nil(ssget "_X"(list(assoc 8 (entget(car(entsel  "\n  Выберете примитив ")))))))
(princ)
)
(defun C:Zd124 ()(Zd124))

(defun Zd1244 (  /  )
(princ "\n Выделяет все примитивы  лежащих на слое")
(sssetfirst nil(ssget (list(assoc 8 (entget(car(entsel  "\n  Выберете примитив ")))))))
(princ)
)
(defun C:Zd1244 ()(Zd1244))

;Разблокирует все слои
(defun Zd125 (  /  )
(princ "\n Разблокирует все слои")
(VL-CMDF "-layer" "_Unlock" "*" "")
(princ)
)
(defun C:Zd125 ()(Zd125))


;Включает и размораживает все слои
(defun Zd126 (  /  )
(princ "\n Включает и размораживает все слои")
(VL-CMDF "-layer" "_On" "*" "")
(VL-CMDF "-layer" "_Thaw" "*" "")
(princ)
)
(defun C:Zd126 ()(Zd126))


;Разблокирует ,включает и размораживает все слои 
(defun Zd127 (  /   )
(princ "\n Разблокирует ,включает и размораживает все слои")
(setvar "regenmode" 0)
(VL-CMDF "-layer" "_Unlock" "*" "")
(VL-CMDF "-layer" "_On" "*" "")
(VL-CMDF "-layer" "_Thaw" "*" "")
(setvar "regenmode" 1)
(VL-CMDF "_regen")
(princ)
)
(defun C:Zd127 ()(Zd127))


(defun Zd128 (  /  zzz_ang zzz_ang1 zzz_ang2 ss1 sel zz_dopusk)
(princ "\n Выделяет линии лежащие под углом")
(setq zzz_ang (getreal "\n Ведите угол \n"))
(setq 	zz_dopusk 1
		zzz_ang1 (- zzz_ang zz_dopusk)
		zzz_ang2 (+ zzz_ang zz_dopusk)
		ss1 (ssadd)			)
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual sel '((0 . "LINE")))
(vlax-for x sel
	(if 
		(or	
			(and
				(< zzz_ang1 (atof(angtos (angle (vlax-safearray->list(vlax-variant-value(vla-get-startpoint x))) (vlax-safearray->list(vlax-variant-value(vla-get-endpoint x)))) 0 2)))
				(<  (atof(angtos (angle (vlax-safearray->list(vlax-variant-value(vla-get-startpoint x))) (vlax-safearray->list(vlax-variant-value(vla-get-endpoint x)))) 0 2))zzz_ang2)
			)
			(and
				(< zzz_ang1 (atof(angtos (angle (vlax-safearray->list(vlax-variant-value(vla-get-endpoint x))) (vlax-safearray->list(vlax-variant-value(vla-get-startpoint x)))) 0 2)))
				(<  (atof(angtos (angle (vlax-safearray->list(vlax-variant-value(vla-get-endpoint x))) (vlax-safearray->list(vlax-variant-value(vla-get-startpoint x)))) 0 2))zzz_ang2)
			)
		)
		(setq ss1(ssadd (vlax-vla-object->ename x)ss1))
	)
)
(princ)
(sssetfirst nil ss1)
)
(defun C:Zd128 ()(Zd128))



(defun Zd129 (  /  )
(VL-CMDF "-SCALELISTEDIT" "_R" "_Y" "_E"))
(defun C:Zd129 ()(Zd129))




(defun Zd130 ()
(if (wcmatch  (getvar "DWGNAME") "*dwg*")
	(vla-SaveAs (vla-get-activedocument (vlax-get-acad-object)) (vl-string-subst "" ".dxf"(vl-string-subst "" ".dwg" (getvar "DWGNAME"))) ac2013_dxf)
	(vla-SaveAs (vla-get-activedocument (vlax-get-acad-object)) (vl-string-subst "" ".dxf"(vl-string-subst "" ".dwg" (getvar "DWGNAME"))) ac2013_dwg)
)
(princ)
)
(defun C:Zd130 ()(Zd130))




;Убирает в названиях слоев все слева до символов $0$

;(defun Zd131 (  / newnamelayer  posit_1 poisk_simv layer_name02 flag01 usercmd)
;(princ "\n Убирает в названиях слоев все слева до символов $0$")
;(setq usercmd (getvar "CMDECHO"))
;(setvar "CMDECHO" 0)
;(setvar "regenmode" 0)
;(setq poisk_simv "$0$" )
;(vlax-for layer_name01 (vla-get-layers(vla-get-activedocument(vlax-get-acad-object)))
;	(if	(setq posit_1(vl-string-search poisk_simv (vla-get-name layer_name01)))
;		(if (vl-catch-all-error-p(vl-catch-all-apply 'vla-item(list (vla-get-layers(vla-get-activedocument(vlax-get-acad-object))) (setq layer_name02(substr (vla-get-name layer_name01) (+ 4 posit_1))))))
;			(vl-catch-all-apply 'vla-put-Name(list layer_name01 layer_name02))
;			(VL-CMDF "_.-LAYMRG" "_Name"(vla-get-name layer_name01)"" "_Name" layer_name02 "_Yes")
;		)
;	)
;);_end_of_vlax-for
;(setvar "CMDECHO" usercmd)
;(setvar "regenmode" 1)
;(princ)
;)

(defun Zd131 (  / newnamelayer  posit_1 poisk_simv layer_name02 usercmd  ss1 ent)
(princ "\n Убирает в названиях слоев все слева до символов $0$")
(setq usercmd (getvar "CMDECHO"))
(setvar "CMDECHO" 0)
(setvar "regenmode" 0)
(setq poisk_simv "$0$" )
(vlax-for layer_name01 (vla-get-layers(vla-get-activedocument(vlax-get-acad-object)))
	(if	(setq posit_1(vl-string-search poisk_simv (vla-get-name layer_name01)))
		(if (vl-catch-all-error-p(vl-catch-all-apply 'vla-item(list (vla-get-layers(vla-get-activedocument(vlax-get-acad-object))) (setq layer_name02(substr (vla-get-name layer_name01) (+ 4 posit_1))))))
			(vl-catch-all-apply 'vla-put-Name(list layer_name01 layer_name02))
			(setq ss1 (append ss1 (list (vla-get-name layer_name01))))
		)
	)
);_end_of_vlax-for
(if (null ss1) (exit))
(defun subst_ent_lay ( ent / ent_lay)
	(setq ent_lay (cdr (assoc 8 (entget ent))))
	(cons 8 (substr ent_lay (+ 4 (vl-string-search poisk_simv ent_lay))))
)
(setq ent (entnext))
(while ent
	(if (member  (assoc 8 (entget ent))  ss1)
		(entmod		
			(subst 	(subst_ent_lay	ent)
					(assoc 8 (entget ent))
					(entget ent)
			)
		)
	)
	(setq ent (entnext ent))
)
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
	(vlax-for vlog_block_name block_name
		(if (vlax-property-available-p vlog_block_name 'Layer)
			(if
				(member  (vla-get-Layer vlog_block_name) ss1)
				(vla-put-Layer vlog_block_name (substr (vla-get-Layer vlog_block_name) (+ 4 (vl-string-search poisk_simv (vla-get-Layer vlog_block_name)))))
			)
		);_end_of_if
	);_end_of_vlax-for
)
(setvar "CMDECHO" usercmd)
(setvar "regenmode" 1)
(princ)
)

(defun C:Zd131()(Zd131))


;Вычисляет левый и правый углы примитивов и выставляет окно печати по ним
(defun Zd132 (  / x1 x2 y1 y2 pt1 pt2 pt3 pt4 objItem sel01)
(princ "\n Вычисляет левый и правый углы примитивов и выставляет окно печати по ним ")
(vla-clear(setq sel01 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-all sel01 (list(cons 410 (getvar "CTAB")) '(-4 . "<NOT") '(-4 . "<OR") '(0 . "TEXT") '(0 . "MTEXT")  '(0 . "MULTILEADER")  '(0 . "VIEWPORT") '(-4 . "OR>") '(-4 . "NOT>")))
(setq x1	(car(getvar "EXTMax"))	x2	(car(getvar "EXTMin"))	y1	(cadr(getvar "EXTMax"))	y2	(cadr(getvar "EXTMin")))
(vlax-for objItem  sel01
	(vla-GetBoundingBox objItem  'pt1 'pt2)
	(if	(< (car (vlax-safearray->list pt1)) x1) 	(setq x1 (car (vlax-safearray->list pt1))))
	(if	(< (cadr (vlax-safearray->list pt1)) y1) 	(setq y1 (cadr (vlax-safearray->list pt1))))
	(if	(> (car (vlax-safearray->list pt2)) x2) 	(setq x2 (car (vlax-safearray->list pt2))))
	(if	(> (cadr (vlax-safearray->list pt2)) y2) 	(setq y2 (cadr (vlax-safearray->list pt2))))
)
(setq 	pt3 	(vlax-safearray-fill(vlax-make-safearray vlax-vbDouble '(0 . 1))(list x1 y1 )) 
		pt4 	(vlax-safearray-fill(vlax-make-safearray vlax-vbDouble '(0 . 1))(list x2 y2 )))
(vla-SetWindowToPlot(vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object))) pt3 pt4)
(vla-Regen (vla-get-activedocument(vlax-get-acad-object)) 1)
(princ)
)
(defun C:Zd132()(Zd132))

;выставляет окно печати по формату
(defun Zd1322 (  / na4alo_koor_X na4alo_koor_Y formatA1L_X formatA1L_Y pt3 pt4)
(princ "\n выставляет окно печати по формату")
(setq na4alo_koor_X 0 na4alo_koor_Y 0)
(setq formatA1L_X 841 formatA1L_Y 594)
(setq 	pt3 	(vlax-safearray-fill(vlax-make-safearray vlax-vbDouble '(0 . 1))(list na4alo_koor_X na4alo_koor_Y )) 
		pt4 	(vlax-safearray-fill(vlax-make-safearray vlax-vbDouble '(0 . 1))(list formatA1L_X formatA1L_Y )))
(vla-SetWindowToPlot(vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object))) pt3 pt4)
;(vla-Regen (vla-get-activedocument(vlax-get-acad-object)) 1)
(princ)
)
(defun C:Zd1322()(Zd1322))

;выставляет окно печати по формату A)
(defun Zd1323 (  / na4alo_koor_X na4alo_koor_Y formatA1L_X formatA1L_Y pt3 pt4)
(princ "\n выставляет окно печати по формату")
(setq na4alo_koor_X 0 na4alo_koor_Y 0)
(setq formatA1L_X 1189 formatA1L_Y 841)
(setq 	pt3 	(vlax-safearray-fill(vlax-make-safearray vlax-vbDouble '(0 . 1))(list na4alo_koor_X na4alo_koor_Y )) 
		pt4 	(vlax-safearray-fill(vlax-make-safearray vlax-vbDouble '(0 . 1))(list formatA1L_X formatA1L_Y )))
(vla-SetWindowToPlot(vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object))) pt3 pt4)
;(vla-Regen (vla-get-activedocument(vlax-get-acad-object)) 1)
(princ)
)
(defun C:Zd1323()(Zd1323))




(defun Zd1330 ( flag01 / act_lay )
(princ "\n выставляет окно печати по формату")
(princ "\n Zd1331 - PDF установка заново листа")
(princ "\n Zd1332 - PDF как было")
(princ "\n Zd1341 - ПЕЧАТЬ установка заново листа")
(princ "\n Zd1342 - ПЕЧАТЬ как было")
(setq act_lay (vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object))))
(vl-catch-all-apply '(lambda () (vla-put-PlotType 				act_lay 	"4"					)))
(cond
	(	(or (equal flag01 1)(equal flag01 2))		(progn
			(vl-catch-all-apply '(lambda () (vla-put-ConfigName 			act_lay 	"DWG To PDF.pc3"	)))
			(vl-catch-all-apply '(lambda () (vla-put-CanonicalMediaName		act_lay 	"ISO_full_bleed_A1_(841.00_x_594.00_MM)")))
													) 
	)
	(	(or (equal flag01 3)(equal flag01 4))		(progn
			(vl-catch-all-apply '(lambda () (vla-put-ConfigName 			act_lay 	"\\\\Rumoscfp01\\RUMOSCPR010"	)))
			(vl-catch-all-apply '(lambda () (vla-put-CanonicalMediaName		act_lay 	"A3")))
													)
	)
)
(vl-catch-all-apply '(lambda () (vla-put-CenterPlot				act_lay 	"-1"				)))
(vl-catch-all-apply '(lambda () (vla-put-PaperUnits				act_lay 	"1"					)))
(vl-catch-all-apply '(lambda () (vla-put-PlotHidden				act_lay 	"0"					)))
(cond
	(	(or (equal flag01 1)(equal flag01 2))	(vl-catch-all-apply '(lambda () (vla-put-PlotRotation	act_lay "0"	))))
	(	(or (equal flag01 3)(equal flag01 4))	(vl-catch-all-apply '(lambda () (vla-put-PlotRotation	act_lay "1"	))))
)
(vl-catch-all-apply '(lambda () (vla-put-PlotViewportBorders	act_lay 	"0"					)))
(vl-catch-all-apply '(lambda () (vla-put-PlotViewportsFirst		act_lay 	"-1"				)))
(vl-catch-all-apply '(lambda () (vla-put-PlotWithLineweights	act_lay 	"-1"				)))
(vl-catch-all-apply '(lambda () (vla-put-PlotWithPlotStyles		act_lay 	"-1"				)))
(vl-catch-all-apply '(lambda () (vla-put-ScaleLineweights		act_lay 	"0"					)))
(vl-catch-all-apply '(lambda () (vla-put-ShowPlotStyles			act_lay 	"0"					)))
(vl-catch-all-apply '(lambda () (vla-put-StandardScale			act_lay 	"0"					)))
(vl-catch-all-apply '(lambda () (vla-put-StyleSheet				act_lay 	"Master.ctb"		)))
(vl-catch-all-apply '(lambda () (vla-put-UseStandardScale		act_lay 	"-1"				)))
(cond
	((or (equal flag01 1)(equal flag01 3))		(Zd132)													)
	((or (equal flag01 2)(equal flag01 4))		(vl-catch-all-apply '(lambda () (vla-Regen	act_lay "1"	)))		)
)
(princ)
)
(defun C:Zd1331()(Zd1330 1))
(defun C:Zd1332()(Zd1330 2))
(defun C:Zd1341()(Zd1330 3))
(defun C:Zd1342()(Zd1330 4))


; заменяет текст на мульти выноску
(defun Zd135 (  /  vla_mleader vla_text pt1 pt2)
(setq vla_text(vlax-ename->vla-object (car(entsel "\n Выберете текст  "))))
(setq pt1 (getpoint "укажите точку вставки текста"))
(setq pt2 (getpoint "укажите точку привязки линии"))
(setq vla_mleader
	(vla-addmleader 
		(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
			(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
			(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
		)
		(ru-mleader-coords-from-list(list pt2 pt1))
		0
	)
)
(vla-put-TextString vla_mleader (vla-get-textstring vla_text))
(vla-put-TextHeight vla_mleader 2.50)
;(vla-put-TextHeight vla_mleader (vla-get-Height vla_text))
(vla-put-Layer vla_mleader (vla-get-Layer vla_text))
(princ)
)
(defun C:Zd135()(Zd135))



;Убирает в названиях блоков все слева до символов $0$
(defun Zd138 (  /  posit_1 poisk_simv block_name02 )
(princ "\n Убирает в названиях блоков все слева до символов $0$")
(setq poisk_simv "$0$" )
(vlax-for block_object01 (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
	(if	(setq posit_1(vl-string-search poisk_simv (vla-get-name block_object01)))
		(if (vl-catch-all-error-p(vl-catch-all-apply 'vla-item(list (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object))) (setq block_name02(substr (vla-get-name block_object01) (+ 4 posit_1))))))
			(vl-catch-all-apply 'vla-put-Name(list block_object01 block_name02))					
			(acet-block-replace (vla-get-name block_object01) block_name02)
		)
	)
);_end_of_vlax-for
(princ)
)
(defun C:Zd138()(Zd138))




		


; если точки вставки блоков далеко надходятся, то лисп их переделает
(defun Zd140 (  / pt1 pt2 pt3 sel insertion_point_01 lst22 vopros)
(setq pt1 (getpoint "\n Задайте левый нижний угол "))
(setq pt2 (getpoint "\n Задайте правый верхний угол  "))
(setq pt3 (getpoint "\n Задайте новую точку для точки вставки блоков  "))
(setq lst22 '(nil))
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(initget "выбратьвсе рамка")
(setq vopros (getkword "\n Выбор блоков [выбратьвсе / рамка]: "))
(if		(equal vopros "выбратьвсе")		
		(pl:obj-filter-select-all sel '((0 . "INSERT")))
		(pl:obj-filter-select-manual sel '((0 . "INSERT")))
)
(vlax-for x sel	
	(if (setq insertion_point_01(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint x))))
		(if 	
			(not(and
				(> 		(car insertion_point_01) 		(car pt1 )		)
				(> 		(cadr insertion_point_01) 		(cadr pt1 )		)
				(< 		(car insertion_point_01) 		(car pt2 )		)
				(< 		(cadr insertion_point_01) 		(cadr pt2 )		)
			))
			(if (vl-member-if-not '(lambda (y) (equal (car y) (vla-get-EffectiveName x ))) lst22)
				(setq lst22(append lst22 (list(list(vla-get-EffectiveName x ) (vlax-vla-object->ename x)))))
			)
		)
	)
	(setq insertion_point_01 nil)
)
(setq lst22(vl-remove-if 'null lst22))
(mapcar  
	'(lambda (z)   
			(LM:changeblockbasepoint_02 t (cadr z ) pt3)       
	)           
lst22
)
(princ (strcat "\n Исправлено " (rtos(length lst22) 2 0) " блоков ") )
(princ)
)
(defun C:Zd140()(Zd140))


(defun LM:changeblockbasepoint_02 ( flg blk_ename new_point / *error* bln cmd ent lck mat nbp vec )

    (defun *error* ( msg )
        (foreach lay lck (vla-put-lock lay :vlax-true))
        (if (= 'int (type cmd)) (setvar 'cmdecho cmd))
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (while
        (progn (setvar 'errno 0) (setq ent blk_ename)
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (= 'ename (type ent))
                    (if (/= "INSERT" (cdr (assoc 0 (entget ent))))
                        (princ "\nSelected object is not a block.")
                    )
                )
            )
        )
    )
    (if (and (= 'ename (type ent)) (setq nbp new_point))
        (progn
            (setq mat (car (revrefgeom ent))
                  vec (mxv mat (mapcar '- (trans nbp 1 0) (trans (cdr (assoc 10 (entget ent))) ent 0)))
                  bln (LM:blockname (vlax-ename->vla-object ent))
            )
            (LM:startundo (LM:acdoc))
            (vlax-for lay (vla-get-layers (LM:acdoc))
                (if (= :vlax-true (vla-get-lock lay))
                    (progn
                        (vla-put-lock lay :vlax-false)
                        (setq lck (cons lay lck))
                    )
                )
            )
            (vlax-for obj (vla-item (vla-get-blocks (LM:acdoc)) bln)
                 (vlax-invoke obj 'move vec '(0.0 0.0 0.0))
            )
            (if flg
                (vlax-for blk (vla-get-blocks (LM:acdoc))
                    (if (= :vlax-false (vla-get-isxref blk))
                        (vlax-for obj blk
                            (if
                                (and
                                    (= "AcDbBlockReference" (vla-get-objectname obj))
                                    (= bln (LM:blockname obj))
                                    (vlax-write-enabled-p obj)
                                )
                                (vlax-invoke obj 'move '(0.0 0.0 0.0) (mxv (car (refgeom (vlax-vla-object->ename obj))) vec))
                            )
                        )
                    )
                )
            )
            (if (= 1 (cdr (assoc 66 (entget ent))))
                (progn
                    (setq cmd (getvar 'cmdecho))
                    (setvar 'cmdecho 0)
                    (vl-cmdf "_.attsync" "_N" bln)
                    (setvar 'cmdecho cmd)
                )
            )
            (foreach lay lck (vla-put-lock lay :vlax-true))
            (vla-regen  (LM:acdoc) acallviewports)
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

; выделение анотативных блоков
(defun zd141 (  /  ss1 ss2)
(setq ss1 (ssget "_X" '((0 . "INSERT") (2 . "`*U*,TITLEBLOCK"))))
(setq ss2 (ssadd))
(mapcar
	'(lambda (xx)
		(if
			(equal
				(vla-get-effectivename(vlax-ename->vla-object xx))
				(vla-get-name(vlax-ename->vla-object xx))
			)
			(setq ss2 (ssadd  xx ss2))
		)
	)
	(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(princ)
(sssetfirst nil ss2)
)
(defun C:zd141()(zd141))


; взрыв меджикадовских Magicad блоков 
(defun zd142 (  /  zflag ss1 ss2)
(if
	(setq ss1 (ssget "_X" '((0 . "INSERT") (2 . "`*U*,TITLEBLOCK"))))
	(progn
		(setq ss2 (ssadd))
		(mapcar
			'(lambda (xx)
				(if
					(equal
						(vla-get-effectivename(vlax-ename->vla-object xx))
						(vla-get-name(vlax-ename->vla-object xx))
					)
					(setq ss2 (ssadd  xx ss2))
				)
			)
			(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
		)
		(if (> (sslength ss2) 0)
			(progn
				(setq zflag (getvar "qaflags"))
				(setvar "qaflags" 5) 
				(VL-CMDF "_explode" ss2 "" "")
				(setvar "qaflags" zflag)
			)
		)	
	)
)
(princ)
)
(defun C:zd142()(zd142))


; взрыв меджикадовских Magicad блоков 
(defun zd141 (  /  )
(VL-CMDF "._Magiexplode"  (ssget "_X" ) "" ) 
(princ)
)
(defun C:zd141()(zd141))

; взрыв меджикадовских Magicad блоков 
(defun zd142 (  /  zflag ss1 ss2)
(if
	(setq ss1 (ssget "_X" '((0 . "INSERT") (2 . "`*U*,TITLEBLOCK"))))
	(progn
		(setq ss2 (ssadd))
		(mapcar
			'(lambda (xx)
				(if
					(equal
						(vla-get-effectivename(vlax-ename->vla-object xx))
						(vla-get-name(vlax-ename->vla-object xx))
					)
					(setq ss2 (ssadd  xx ss2))
				)
			)
			(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
		)
		(if (> (sslength ss2) 0)
			(progn
				(setq zflag (getvar "qaflags"))
				(setvar "qaflags" 5) 
				(VL-CMDF "_explode" ss2 "" "")
				(setvar "qaflags" zflag)
			)
		)	
	)
)
(princ)
)
(defun C:zd142()(zd142))

; взрыв меджикадовских Magicad блоков 
(defun zd143 (  /  )(zd141)(zd142)(princ))
(defun C:zd143()(zd143))



; взрыв блока начало имени которого указывается
(defun zd144 ( ish_name  /  )
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
	(vlax-for vlog_block_name block_name
		(if (vlax-property-available-p vlog_block_name 'EffectiveName)
			(if
				(wcmatch  (vla-get-EffectiveName vlog_block_name) (strcat "*" ish_name "*"))
				(if
					(null(vl-catch-all-error-p(vl-catch-all-apply '(lambda () (vla-explode vlog_block_name)))))
					(vl-catch-all-apply '(lambda () (vla-erase vlog_block_name)))
				)
			)
		);_end_of_if
	);_end_of_vlax-for
);_end_of_vlax-for
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
		(if (vlax-property-available-p block_name 'EffectiveName)
			(if
				(wcmatch  (vla-get-EffectiveName block_name) (strcat "*" ish_name "*"))
				(if
					(null(vl-catch-all-error-p(vl-catch-all-apply '(lambda () (vla-explode block_name)))))
					(vl-catch-all-apply '(lambda () (vla-erase block_name)))
				)
			)
		);_end_of_if
);_end_of_vlax-for
(princ)
)
(defun C:zd144()(zd144))


;нормализация текста для печати
(defun zd145 (  /  ss1  ss210 ss50 ss51)
(setq ss1 (ssget "_X" '((0 . "TEXT,MTEXT,MULTILEADER,ATTRIB"))))
(setq ss210 '(210 0.0 0.0 1.0))
(setq ss51 '(51 . 0.0))
(mapcar
	'(lambda (xx)
			(setq ss50 (assoc 50 (entget xx)))
			(entmod		
				(subst 	(list 10 (cadr(assoc 10 (entget xx))) (caddr(assoc 10 (entget xx))) 0.0)
						(assoc 10 (entget xx))
						(entget xx)
				)
			)
			(entmod		
				(subst 	ss210
						(assoc 210 (entget xx))
						(entget xx)
				)
			)
			(entmod		
				(subst 	ss51
						(assoc 51 (entget xx))
						(entget xx)
				)
			)
			(entmod		
				(subst 	ss50
						(assoc 50 (entget xx))
						(entget xx)
				)
			)
	)
	(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(if (setq ss1 (ssget "_X" (list '(0 . "TEXT,MTEXT,MULTILEADER,ATTRIB") (cons 410 (getvar "CTAB")))))
	(progn
		(command "_.UCS" "")
		(command "_.move" ss1 "" '(0 0 1e99) ""       "_.move" "_p" "" '(0 0 -1e99) "")
	)
)
(princ)
)
(defun C:zd145()(zd145))

;выделение по dxf группе
(defun zd146 ( usl1 /  ss1  ss2 ss3 ss4 ss5 ss6)
(cond		(	(equal usl1 1)	(princ "\n выделить все во всем чертеже" 	)	)
			(	(equal usl1 2)	(princ "\n выделить все в данном листе" 	)	)
			(	(equal usl1 3)	(princ "\n выделить вручную " 				)	)
			(	(equal usl1 4)	(princ "\n выделить вручную в данном листе" )	)
)
(setq 	spisok1 	'(5 100 300 301 302 303 305)
		ss1 		(entget(car(entsel)))
		ss1 		(vl-sort ss1 '(lambda(x1 x2)(< (car x1)(car x2))))
		ss1 		(vl-remove-if '(lambda (x) (equal 'ename (type(cdr x))))ss1)
		ss1 		(vl-remove-if '(lambda (x) (member (car x) spisok1))ss1)
		ss1			(_kpblc-list-dublicates-remove ss1)
		ss2 		(mapcar '(lambda (xx)(vl-prin1-to-string xx))ss1)
		ss3 		(car(list(loc:dwgru-get-user-dcl "Укажите dxf группу по которой искать" ss2 t)))
		ss4 		(mapcar '(lambda (xx)(read (eval xx))) ss3)
		ss6			(list(cons 410 (getvar "CTAB")) (car ss4))
)
(if ss4
	(cond	(	(equal usl1 1)	(setq 	ss5 		(ssget "_X" ss4))	)
			(	(equal usl1 2)	(setq 	ss5 		(ssget "_X"	ss6))	)
			(	(equal usl1 3)	(setq 	ss5 		(ssget 		ss4))	)
			(	(equal usl1 4)	(setq 	ss5 		(ssget 		ss6))	)
	)
	(exit)
)
(princ (strcat "\n найдено " (rtos (sslength ss5)2 0) ))
(sssetfirst nil ss5)
(princ)
)
(defun C:zd1461()	(zd146 	1))
(defun C:zd1462()	(zd146 	2))
(defun C:zd1463()	(zd146 	3))
(defun C:zd1464()	(zd146 	4))

;все dxf в списке
(defun zd147 (  /  ss1  ss2  ss3 ss4)
(setq 	ss1 		(entget(car(entsel)))
		ss1 		(vl-sort ss1 '(lambda(x1 x2)(< (car x1)(car x2))))
		ss2 		(mapcar '(lambda (xx)(vl-prin1-to-string xx))ss1)
		ss3 		(car(list(loc:dwgru-get-user-dcl "Укажите dxf группу" ss2 t)))
		ss4 		(mapcar '(lambda (xx)(read (eval xx))) ss3)
)
(princ)
(setq ss4 ss4)
)
(defun C:zd147()(zd147))

;копирование в буфер текста из подложки
(defun zd148 nil
(copyToclipboard (vla-get-textstring(vlax-ename->vla-object (car(nentsel)))))
(princ)
)
(defun C:zd148()(zd148))


;переименование блоков
(defun zd149 ( name01  /  ent01  )
(vla-put-Name
  (vla-item
    (vla-get-Blocks
      (vla-get-ActiveDocument
        (vlax-get-acad-object)
      )
    )
  (vla-get-EffectiveName (vlax-ename->vla-object (car(entsel))))) ;;;<-- old block name
name01)
(princ)
)
(defun C:zd149()(zd149))

(defun zd150 ( flag01 / Layout_nugnuii)
(princ "\n удаляет все листы кроме выделенного")
(setq Layout_nugnuii(car(loc:dwgru-get-user-dcl "Укажите лист" (vl-remove "Model" (ttc-layouts-list)) nil)))
(setvar "CTAB" Layout_nugnuii)
(vlax-for Layout_name (vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))
	(if		(not	(equal (vla-get-name Layout_name) Layout_nugnuii)
			)
			(vl-catch-all-apply '(lambda () (vla-delete Layout_name)))
	)
);_end_of_vlax-for
(if	(equal flag01 0)
	(progn
		(vla-save(vla-get-activedocument(vlax-get-acad-object)))
		(VL-CMDF "_close")
	)
)
(princ)
)

(defun C:zd150()(zd150 1))
(defun C:zd1500()(zd150 0))


(defun zd151 (  flag01 /  ent ss3 file_name )
(princ "\n удаляет все листы кроме выделенного")
(setq file_name(vl-string-subst "" ".dwg" (vla-get-name(vla-get-activedocument(vlax-get-acad-object)))))
(setvar "CTAB" file_name)
(vlax-for Layout_name (vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))
	(if		(not	(equal (vla-get-name Layout_name) file_name))
			(vl-catch-all-apply '(lambda () (vla-delete Layout_name)))
	)
);_end_of_vlax-for
(if	(equal flag01 0)
	(progn
		(vla-save(vla-get-activedocument(vlax-get-acad-object)))
		(VL-CMDF "_close")
	)
)
(princ)
)
(defun C:zd151()(zd151 0))



;Перенос элементов из слоя 0 в 0_temp
(defun zd152 ( / ent )
(princ "\n Перенос элементов из слоя 0 в 0_temp ")
(if	(null(tblobjname "Layer" "0_temp"))(vla-add(vla-get-Layers(vla-get-activedocument(vlax-get-acad-object)))"0_temp"))
(SETQ ent (entnext))
(while ent
	(if (equal  (assoc 8 (entget ent))  '(8 . "0"))
		(entmod		
			(subst 	'(8 . "0_temp")
					'(8 . "0")
					(entget ent)
			)
		)
	)
	(SETQ ent (entnext ent))
)
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
	(vlax-for vlog_block_name block_name
		(if (vlax-property-available-p vlog_block_name 'Layer)
			(if
				(equal  (vla-get-Layer vlog_block_name) "0")
				(vla-put-Layer vlog_block_name "0_temp")
			)
		);_end_of_if
	);_end_of_vlax-for
);_end_of_vlax-for
(princ)
)


;выделяет все видовые экраны с указанным масштабом
(defun zd153 ( / ss1 custom_scale_v01 ss2)
(princ "\n выделяет все видовые экраны с указанным масштабом")
(setq ss1(ssget "_X" (list '(0 . "VIEWPORT")'(-4 . "<>")'(69 . 1)))
		ss2 (ssadd)
)
(setq custom_scale_v01 (vl-princ-to-string(vla-get-CustomScale(vlax-ename->vla-object (car(entsel "\n укажите исходный вьюпорт"))))))
(mapcar
          '(lambda (name)(if (equal (vl-princ-to-string(vl-catch-all-apply 'vla-get-CustomScale (list(vlax-ename->vla-object name)))) custom_scale_v01 )
			     (ssadd name ss2)
			     
			 )
	   )
	
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1))) 
)
(sssetfirst nil ss2)
)
(defun C:zd153 ()(zd153))

(defun zd154 ( / )
(princ "\n Сохраняет в dxf \n")
(setvar "FILEDIA" 0)
(VL-CMDF "._dxfout" "" "")
(setvar "FILEDIA" 1)
(princ)
)
(defun C:zd154 ()(zd154))

;(vl-string-subst ".dxf" ".dwg" (strcat(vla-get-path(vla-get-activedocument(vlax-get-acad-object))) "\\" (vla-get-name(vla-get-activedocument(vlax-get-acad-object)))))


(defun zd155 ( / )
(princ "\n Сохраняет в dwg \n")
(setvar "FILEDIA" 0)
(VL-CMDF "._qsave" "" "")
(setvar "FILEDIA" 1)
(princ)
)
(defun C:zd155 ()(zd155))

(defun zd156 ( / CurrDwgName Fname name)
(princ "\n Сохраняет в wblock \n")
(setq CurrDwgName (getvar "dwgname"))
(setq Fname (substr CurrDwgName 1 (- (strlen CurrDwgName) 4)))
(setq name (strcat  (getvar "DWGPREFIX") "_wblock_" Fname ".dwg"))
(setvar "FILEDIA" 0)
(VL-CMDF "._-wblock" name "*")
(setvar "FILEDIA" 1)
(princ)
)
(defun C:zd156 ()(zd156))



;пример вызова (poisk_i_zamena_frazi_wild "SUPPLY" "ПРИТОК" "SUPPLY/ПРИТОК" ename01)
;было "?250-SUPPLY  ?250-ПРИТОК" станет "?250-SUPPLY/ПРИТОК"
(defun poisk_i_zamena_frazi_wild ( slovo_01 slovo_02 slovo_03 ename01 / ishodnii_text_01 kone4nii_text)
(if (vlax-property-available-p ename01 'textstring)
	(progn
		(setq ishodnii_text_01 (vla-get-textstring ename01))
		(if (vl-string-search slovo_01 ishodnii_text_01)
			(if	(vl-string-search slovo_02 ishodnii_text_01 pos01)
				(progn 
					(setq pos01(1+(vl-string-search slovo_01 ishodnii_text_01)))
					(setq pos02(- (+ 1 (strlen slovo_02) (vl-string-search slovo_02 ishodnii_text_01 pos01)) pos01 ))
					(setq kone4nii_text (vl-string-subst slovo_03 (substr ishodnii_text_01 pos01 pos02) ishodnii_text_01))
					(vla-put-textstring ename01 kone4nii_text)
				)
			)
		)
	)
)
)

; пример (zd157 "RETURN" "ВЫТЯЖКА" "RETURN/ВЫТЯЖКА" )
(defun zd157 (slovo_01 slovo_02 slovo_03  / ss1 )
(setq ss1 (ssget '((0 . "TEXT,MTEXT,MULTILEADER"))))
(mapcar '(lambda (x)
	(poisk_i_zamena_frazi_wild slovo_01 slovo_02 slovo_03 (vlax-ename->vla-object x))
		)
	(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(princ)
)


; (make_layer "001_Urban_Areas" 6 "Continuous")
(defun make_layer ( name_layer color_number type_line / )
(entmake
	(list
		(cons 0 "LAYER")
		(cons 100 "AcDbSymbolTableRecord")
		(cons 100 "AcDbLayerTableRecord")
		(cons 2  name_layer)
		'(70 . 0)
		(cons 62  color_number)
		(cons 6  type_line)
	)
)
(princ)
)


;290 это видимость слоя , 1 видимый 0 невиддимый
(defun zd158 (  / layer_name01 layer_name02)
(setq layer_name01 "_Revision" layer_name02 "_Signs")
	(defun layer_put_property ( layer_name dxf_number dxf_value / )
		(entmod		
			(subst 	(cons dxf_number  dxf_value)
					(assoc dxf_number (entget(tblobjname "layer" layer_name)))
					(entget(tblobjname "layer" layer_name))
			)
		)
		(princ)
	)

	(defun layer_get_property ( layer_name dxf_number / )
		(cdr(assoc dxf_number (entget(tblobjname "layer" layer_name))))
	)

(if  	(null (tblobjname "layer" layer_name01))
		(make_layer layer_name01 2 "Continuous")
)
(if  	(null (tblobjname "layer" layer_name02))
		(make_layer layer_name02 2 "Continuous")
)
(if	(null (assoc 420 (entget(tblobjname "Layer" layer_name02))))
	(entmod	
		(append (entget(tblobjname "Layer" layer_name02)) 
		(list '(420 . 1252076)))
	)
)

(if (not(equal 0 	(layer_get_property layer_name01 290))) 	(layer_put_property layer_name01 290 0)		)
(if (not(equal 30 	(layer_get_property layer_name01 370))) 	(layer_put_property layer_name01 370 30)	)
(if (not(equal 2 	(layer_get_property layer_name01 62))) 		(layer_put_property layer_name01 62 2)		)
(princ)
)



;создает листы согласно списка в файл 111.txt
(defun zd159 ( / spisok_zod159 pathfolfer a flag q  printname) 
(setvar "filedia" 0)
(vl-mkdir (strcat(getvar "DWGPREFIX") "tempfolder333"))
(setq pathfolfer (strcat(getvar "DWGPREFIX") "tempfolder333\\"))
(setq a (open  (strcat(getvar "DWGPREFIX") "111.txt") "r"))
(while (not flag)
	(if (setq q (read-line a))
		(setq spisok_zod159 (append spisok_zod159 (list(list(substr q 1 (vl-string-search "\t" q))(substr q (+ 2 (vl-string-search "\t" q)) (- (strlen q)(vl-string-search "\t" q)))))))
		(setq flag T)
	)
)
(close a)
(foreach x spisok_zod159 (command "saveas" "2013" (strcat pathfolfer (car x))))
(vla-save(vla-get-activedocument(vlax-get-acad-object)))
(setvar "filedia" 1)
(VL-CMDF "._close" "_y")
(princ)
)
(defun C:zd159 ()(zd159))


;аналог bcount для дин блоков
(defun zd160 ( / en_blk  blk   ss1   dyn_prop  temp01 )
(if (vlax-method-applicable-p (setq blk (vlax-ename->vla-object (setq en_blk(car(entsel " выберете дин блок"))))) 'getdynamicblockproperties)
	(setq dyn_Property_name 
		(loc:dwgru-get-user-dcl " выберете свойства "  
			(mapcar '(lambda(x)(car x))(MM:Remove_origin(LM:getdynprops blk))) 
			t
		)					
	)
	(exit)
)
(princ "выберете блоки")
(setq 	ss1 (ssadd en_blk)
		ss1 (ssget (list(cons 2 (SELBLK2 (entget en_blk)))))
)	
(mapcar 
	'(lambda (x) 
		(mapcar 
			'(lambda (y) 
				(if (member (vl-princ-to-string(car y)) dyn_Property_name)
					(setq temp01(append temp01 (list (append (list(vl-princ-to-string(car y)) (vl-princ-to-string(cadr y)))))))
				)
			)
			(MM:Remove_origin(LM:getdynprops2 (vlax-ename->vla-object x)))
		)
		(setq 	dyn_prop(append dyn_prop (list(append (LM:ListsUnion temp01) '(1))))
				temp01 nil
		)
	)
	(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(setq dyn_prop(vl-sort (sort_and_sum_list dyn_prop) '(lambda(x1 x2)(< (car x1)(car x2)))))
(foreach x dyn_prop(princ (bns_count_format2 (vl-princ-to-string (reverse(cdr(reverse x)))) (vl-princ-to-string(car(reverse x))) 100)))
(princ)
)
(defun C:zd160 ()(zd160))






;подсчет только динамических блоков с учетом всех дин свойств
(defun zd161 ( / en_blk  blk   ss1   dyn_prop  max_length )
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "INSERT")))
(vlax-for block_name ss1
		(if 	(LM:getdynprops2 block_name)
				(setq dyn_prop (append dyn_prop (list (append (list(vla-get-EffectiveName block_name)) (LM:ListsUnion(MM:Remove_origin(LM:getdynprops2 block_name))) '(1)))))
		);_end_of_if
);_end_of_vlax-for
(setq dyn_prop(vl-sort (sort_and_sum_list dyn_prop) '(lambda(x1 x2)(< (car x1)(car x2)))))
(setq max_length 0)
(mapcar '(lambda(x)(if (>(strlen (LM:lst->str2 x " "))max_length)(setq max_length (strlen (LM:lst->str2 x " ")))))dyn_prop)
(setq max_length (+ 10 max_length))
(foreach x dyn_prop(princ (bns_count_format2 (vl-princ-to-string (reverse(cdr(reverse x)))) (vl-princ-to-string(car(reverse x))) max_length)))
(vla-clear ss1)
(princ)
)
(defun C:zd161 ()(zd161))

;поиск wipeout
(defun zd162 ( / lst lst1 lst2)
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
	(vlax-for v_name block_name
		(if (equal (vla-get-ObjectName v_name) "AcDbWipeout")
			(if	(equal (vla-get-IsDynamicBlock block_name) :vlax-true)
				(setq lst1(append lst1 (list (vla-get-Name block_name))))
				(setq lst2(append lst2 (list (vla-get-Name block_name))))
			)
		);_end_of_if
	);_end_of_vlax-for
);_end_of_vlax-for
(setq lst (append lst1 lst2))
(princ "\n WIPEOUT есть в следующих блоках \n")
(foreach x lst (princ (strcat  x "\n")))
(princ)
)
(defun C:zd162 ()(zd162))

;делает оффсет offset для большого количества объектов и перемещает в слой
(defun zd163 ( / ss1 dist01 lay01)  
(setq 	dist01	50.0
		lay01	"0"
)
(vla-clear(setq ss1 (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual ss1 '((0 . "ARC,CIRCLE,ELLIPSE,*LINE")))
(vlax-for x ss1
	(vla-put-layer(car(vlax-safearray->list(vlax-variant-value(vla-offset x dist01))))lay01)
)
(vla-clear ss1)
(princ)
)
(defun C:zd163 ()(zd163))



;создает тексты с каждым слоем и его названием
(defun zd164 ( / y_coor )
(setq y_coor 0.0)
(foreach x (get_layers_name) 
	(progn 
		(make_text 0.0 y_coor x x)
		(setq y_coor (+ 500 y_coor))
	)
)
(princ)
)
(defun C:zd164 ()(zd164))

; преобразовывает в облака различные примитивы
(defun zd165 (/ ss i)
(princ "\n преобразовывает в облака различные примитивы \n")
  (if (setq ss (ssget '((0 . "*LINE,CIRCLE,ELLIPSE,ARC"))))
    (repeat (setq i (sslength ss))
      (command "_.revcloud" "" (ssname ss (setq i (1- i))) "")
    )
  )
  (princ)
)
(defun C:zd165 ()(zd165))


;переделанный cline leemac , проставляет сентры всем кружкам
(defun cline2 ( / )
(setq ss1 (ssget '((0 . "CIRCLE"))))
(mapcar '(lambda(ent)
	(if
        (and
            (setq pt1 (trans (cdr (assoc 10 (entget ent))) ent 1))
            (setq pt2 (list (car pt1) (+ 100 (cadr pt1)) 0.0 ))
            (setq ang (angle pt1 pt2)
                  dis (distance pt1 pt2)
            )
        )
        (repeat 2
            (entmake
                (list
                   '(0 . "LINE")
                    (cons 10 (trans (polar pt1 ang dis) 1 0))
                    (cons 11 (trans (polar pt1 ang (- dis)) 1 0))
                )
            )
            (setq ang (+ ang (/ pi 2.0))
                  dis (- dis)
            )
        )
    )
	)
(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(princ)
)
(defun C:cline2 ()(cline2))



;по центру объекта рисует точку
(defun zd166 ( / sel pt1 pt2 pt3 activespace01)
(princ "\n масштабирует объекты по центру \n")
(setq activespace01 (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object))))
(vla-clear(setq sel (vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object)))))
(pl:obj-filter-select-manual sel '((0 . "*")))
(vlax-for x sel
	(vla-GetBoundingBox x 'pt1 'pt2)
	(setq pt1  (vlax-safearray->list pt1)  pt2 (vlax-safearray->list pt2))
	(setq pt3 (list	(/(+(car pt1 )	(car pt2 ))2)
					(/(+(cadr pt1 )	(cadr pt2 ))2)
					(/(+(caddr pt1 )(caddr pt2 ))2)
				)
	)
	(vla-AddPoint  activespace01 (vlax-3d-point pt3))
)
(if		(getvar "PDMODE")	(if		(not (equal 3	(getvar "PDMODE")))				(setvar "PDMODE" 3		)))
(if		(getvar "PDSIZE")	(if		(not (equal 500	(getvar "PDSIZE")))				(setvar "PDSIZE" 500	)))
(princ)
)
(defun C:zd166 ()(zd166))


;замораживает несколько слоев на выбранных экранах
;   (zd167 '("$_OV_blocki_obsh_01" "$_OV_blocki_obsh_02" "$_OV_blocki_obsh_03" "$_OV_blocki_obsh_04" "$_OV_blocki_obsh_05") )
(defun zd167 ( list01 / ss1 )
(princ "\n замораживает несколько слоев на выбранном экране \n")
(setq ss1 (ssget '((0 . "VIEWPORT"))))
(mapcar
	'(lambda (x)
		(mapcar
			'(lambda (y)
				(WS-VpLayFreeze y x)
			) ;_ end of mapcar
			list01
		)
	) ;_ end of lambda
	(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(princ)
)



; аналог overkill
;самый быстрый
(defun zd168 ( / ss1  ss2 ss200 ss3 ss4 zzz1 zzz2)
(setq zzz1 (getvar "DATE"))
(princ "\n аналог overkill \n")
(setq 	ss1 	(ssget) 
		ss1 	(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
		ss1		(vl-remove-if-not (FUNCTION (LAMBDA (x)(assoc 8(entget x))))ss1)
		ss1 	(vl-sort ss1 '(lambda(c d)(< (cdr(assoc 5 (entget c))) (cdr(assoc 5 (entget d))) )))
		ss3		(cdr ss1)
		ss1		(reverse(cdr(reverse ss1)))
		ss4		ss3
)
(while (car ss1)
	(while (car ss4)
		(if (equal (remove_dxf_pair3 '(-1 2 5 360)(entget (car ss1))) (remove_dxf_pair3 '(-1 2 5 360)(entget (car ss4))))
			(setq ss2 (append ss2 (list(car ss4))))
		)
		(setq ss4 (cdr ss4))
	)
	(foreach x ss2 (setq ss3 (vl-remove x ss3)))
	(foreach x ss2 (setq ss1 (vl-remove x ss1)))
	(setq 	ss1 	(cdr ss1)
			ss3		(cdr ss3)
			ss4		ss3
			ss200	(append ss200 ss2)
			ss2		nil
	)
)
(if	ss200	
		(progn
				(setq ss200(_kpblc-list-dublicates-remove ss200))
				(mapcar '(lambda (y)(entdel y))ss200)
				(princ (strcat "\n удалено      "  (rtos(length ss200)2 0) "    объектов \n"))
		)
		(princ (strcat "\n нет      дубликатов \n"))
)
(setq zzz2 (getvar "DATE"))
(* 86400.0(- zzz2 zzz1))
)
(defun C:zd168 ()(zd168))







; выставляет масштабирование и заперт на взырыв блока, либо убирает
(defun zd169 ( flag_01 / block_name_01 )
(Setq block_name_01
	(vla-item
		(vla-get-Blocks
			(vla-get-ActiveDocument
				(vlax-get-acad-object)
			)
		)
		(vla-get-EffectiveName(vlax-ename->vla-object(car (entsel "Укажите нужный блок: "))))
	)
)
(cond 
	(	(equal flag_01 1)	(progn (vla-put-BlockScaling 	block_name_01 1)	
									(vla-put-Explodable	 	block_name_01 :vlax-false)
							)
	)
	(	(equal flag_01 2)	(progn (vla-put-BlockScaling 	block_name_01 0)	
									(vla-put-Explodable	 	block_name_01 :vlax-true)
							)
	)
)
(princ)
)
(defun C:zd1691 ()(zd169 1))
(defun C:zd1692 ()(zd169 2))


; получает имя блоки и имена вложенных
(defun bln ( flag01 / block01 listblockname01 objectname01) 
(if
	(vlax-property-available-p (setq block01 (vlax-ename->vla-object (car(entsel)))) 'EffectiveName)
	(setq listblockname01 (append listblockname01 (list(vla-get-EffectiveName block01 ))))
	(progn (princ " это не блок \n") (exit))
)
(setq objectname01
	(vla-item
		(vla-get-Blocks (vla-get-ActiveDocument(vlax-get-acad-object)))
		(car listblockname01)
	)
)
(vlax-for itemobjectname objectname01
	(if (vlax-property-available-p itemobjectname 'EffectiveName)
		(setq listblockname01 (append listblockname01 (list(vla-get-EffectiveName itemobjectname ))))
	);_end_of_if
)
(setq listblockname01(_kpblc-list-dublicates-remove listblockname01))

(princ (strcat "\n Название блока                 " (car listblockname01)))
(if (equal flag01 2)
	(foreach x (cdr listblockname01)
		(princ (strcat "\n Название вложенного блока      " x))
	)
)
(princ)
)
(defun C:bln ()(bln 1))
(defun C:bln2 ()(bln 2))





;разбивает равномерно шахту для труб, указывая центры точками
(defun zd170 (  / pt01  pt02    pt03   number01   list01   dist01   dist02   dist03  )
(Setq 	pt01	(getpoint "\n укажите левый нижний угол")
		pt02	(getpoint "\n укажите верхний нижний угол")
		dist02	(getdist "\n укажите 2 точки диамтера")
		number01	(getreal "\n сколько будет труб")
)
(setq 	dist01	(/(-(abs(-(car pt01)(car pt02)))(* dist02 number01)) (+ 1.0 number01))
		dist02	(/ dist02 2.0)
		dist03	(abs(/(-(cadr  pt01)(cadr  pt02))2.0))
		pt03	(list (+(car pt01)dist01 dist02) (+(cadr pt01)	dist03))
)
(repeat (atoi(rtos number01 2 0 ))
	(setq 	list01	(append list01 (list pt03))
			pt03	(list (+(car pt03)	dist01 (* 2.0 dist02))(cadr pt03))
			
	)
)
(if		(getvar "PDMODE")	(if (not (equal (getvar "PDMODE") 3))		(SETVAR "PDMODE" 3)))
(if		(getvar "PDSIZE")	(if (not (equal (getvar "PDSIZE") 5000))		(SETVAR "PDSIZE" 5000)))
(mapcar '(lambda(x)(command	"_point" x))list01)
(princ)
)
(defun C:zd170 ()(zd170))


(defun zd171 ( flag01 / Layout_name)
(princ "\n выставляет/убирает отображение как при печати")
(vlax-for Layout_name (vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))
	(if flag01
		(vl-catch-all-apply '(lambda () (vla-put-ShowPlotStyles Layout_name :vlax-true)))
		(vl-catch-all-apply '(lambda () (vla-put-ShowPlotStyles Layout_name :vlax-false)))
	)
);_end_of_vlax-for
)
(defun C:zd1711 ()(zd171 t))
(defun C:zd1712 ()(zd171 nil))





; восстанавливает сохраненые наборы слоев
(defun lay_state_restore ( name_state_01 /  )
(if		(layerstate-has name_state_01)
		(progn		(layerstate-restore name_state_01)
					(vla-regen (vla-get-activedocument(vlax-get-acad-object)) acallviewports)
		)
		(princ "нет такого набора")
)
(princ)
)
(defun C:la1 ()(lay_state_restore "normal"))
(defun C:la2 ()(lay_state_restore "unhide"))

; удалить параметризацию
(defun del_parametrizaciu( / )
(princ "\n удалить параметризацию \n")
(if (dictsearch (namedobjdict) "acad_assocnetwork")
  (entdel (cdr (assoc -1 (dictsearch (namedobjdict) "acad_assocnetwork"))))
)
)


(defun pstyleDOshow ( flag01 / doc layoutobj)
(princ "\n выставляет на  текущем листе свойство показать стиль печати")
  (setq	doc	  (vla-get-activedocument
		    (vlax-get-acad-object)
		  )
	layoutobj (vla-get-activelayout doc)
  )
  (if flag01
	(vla-put-showplotstyles layoutobj :vlax-true)
	(vla-put-showplotstyles layoutobj :vlax-false)
  )
  (vla-regen doc acAllViewports)
  (mapcar 'vlax-release-object (list layoutobj doc))
  (princ)
)
(defun C:zd172 	()(pstyleDOshow t))
(defun C:zd1722 ()(pstyleDOshow nil))


(defun zd173 ( flag01 / )
(princ "\n выставляет на  всех лситах свойство показать стиль печати")
(vlax-for Layout_name (vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))
	(if flag01
		(vla-put-showplotstyles Layout_name :vlax-true)
		(vla-put-showplotstyles Layout_name :vlax-false)
	)
);_end_of_vlax-for
)
(defun C:zd1731 	()(zd173 t))
(defun C:zd1732		()(zd173 nil))


(defun Zd174 ( newnamelayer / newnamelayer ss1 )
(princ "\n Добавляет суффикс к слою объектов")
(if (equal "" newnamelayer) (setq newnamelayer (getstring "Введите префикс для всех слоев: ")))
(princ "\n выделите объекты \n ")
(setq ss1 (vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget )))))
(mapcar '(lambda (x) 
			(entmod
				(subst  (cons 8 (strcat (cdr(assoc 8 (entget x)))newnamelayer))
						(assoc 8 (entget x)) 
						(entget x)
				)
			)
		)
ss1
)
(princ)
)
(defun C:Zd174 ()(Zd174 ""))




(defun Zd175 (  /   zd175_mode numb01 la_x ent_coor ent01)
(princ "\n выставляет отметку согласно тексту ")
(initget "новая предидущ  _New01 Prev01")
(setq zd175_mode
	(getkword  (strcat "\n Выберите [новая/предидущ]: <новая>: " ))
)
(if (equal zd175_mode "Prev01")
	(setq numb01 zd175_coord)
	(progn 	(setq ent01 (car(entsel "\n укажите текст или полилинию")))
			(cond 
				((wcmatch (cdr(assoc 0(entget ent01))) "TEXT,MTEXT,MULTILEADER") 	(setq numb01 (ent_to_number ent01)))
				((wcmatch (cdr(assoc 0(entget ent01))) "LWPOLYLINE") 				(setq numb01 (cdr(assoc 38(entget ent01)))))
			)
	)
)
(setq 	ent01 		(entsel "\n укажите полилинию")
		la_x 		(car ent01)
		ent_coor 	(reverse(cdr(reverse(car(cdr ent01)))))
)
(entmod(subst (cons 38 numb01) 	(assoc 38 (entget la_x)) 	(entget la_x)))
(entmod(subst '(62 . 6) 	(assoc 62 (entget la_x))	 (entget la_x)))
(vla-put-Layer
	(vla-addtext 	
		(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
			(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
			(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
		)
		(rtos numb01 2 2)
		(vlax-3d-point ent_coor)
		1
	)
	(getvar "clayer")
)
(setq zd175_coord numb01)
(princ)
)
(defun C:Zd175 ()(Zd175))



(defun Zd176 (  /   ent01 circe01 text01 )
(princ "\n выставляет отметку согласно тексту ")
(setq ent01 (car(entsel "\n укажите текст или круг")))
(cond
	((equal "CIRCLE"	(cdr(assoc 0 (entget ent01))))(setq circe01 ent01))
	((equal "TEXT"		(cdr(assoc 0 (entget ent01))))(setq text01 ent01))
)
(setq ent01 (car(entsel "\n укажите текст или круг")))
(cond
	((equal "CIRCLE"	(cdr(assoc 0 (entget ent01))))(setq circe01 ent01))
	((equal "TEXT"		(cdr(assoc 0 (entget ent01))))(setq text01 ent01))
)
(entmod(subst '(73 . 0) 	(assoc 73 (entget text01))	 (entget text01)))
(entmod(subst (assoc 10 (entget circe01)) 	(assoc 10 (entget text01))	 (entget text01)))
(entmod(subst '(62 . 6) 	(assoc 62 (entget text01))	 (entget text01)))
(princ)
)
(defun C:Zd176 ()(Zd176))


(defun Zd178 ( / text01 ent02)
(setq text01 (mip_mtext_unformat(cdr(assoc 1(entget(car(nentsel)))))))
(setq ent02 (car(nentsel)))
(entmod(subst (cons 1 text01) 	(assoc 1 (entget ent02))	 (entget ent02)))
(princ)
)
(defun C:Zd178 ()(Zd176))




; Если не работает smt
(defun zd179 (  / ss1 )
(setq ss1 (ssget 		'((0 . "TEXT,MTEXT,MULTILEADER"))))
(mapcar '(lambda (x)
		(vla-put-TextString (vlax-ename->vla-object x)(mip_mtext_unformat3(vla-get-TextString(vlax-ename->vla-object x))))
		)
	(vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
)
(princ)
)
(defun C:zd179		()(zd179))



; Извлекает нужно значение динамеческого блока и записывает текст
;(zd180 "Length" 500)  (defun C:zd180		()(zd180 "Length" 250))
(defun zd180 (  name_dyn_prop heiht_text /  text_01)
(setq text_01 (rtos(LM:GETDYNPROPVALUE (vlax-ename->vla-object(car(entsel))) name_dyn_prop) 2 0))

(vla-addtext 	
	(if	(equal 1 (vla-get-activespace(vla-get-activedocument(vlax-get-acad-object))))
				(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
				(vla-get-paperspace(vla-get-activedocument(vlax-get-acad-object)))
	)
	text_01
	(vlax-3d-point(getpoint "\nУкажите точку вставки текста")) 
	heiht_text
)
(princ)
)




(defun dwgp1 (  /  )
(princ "\nСоздает список из dwg properties ")
(setq f (open (strcat  (getvar "DWGPREFIX") "dwgp.txt") "a"))
(mapcar '(lambda(y)
			(progn
				(setq string1 "")
				(mapcar '(lambda(x)(setq string1 (strcat string1 x "\t")))y)
				(write-line string1 f)
			)
		)
	(getdwgprops)
)
(close f)
(princ)
)
(defun C:dwgp1	()(dwgp1))

(defun dwgp2 ( / a flag q  )
(princ "\nЗагружает из txt-файлf в текущий dwg properties ")
(setq a (open (getfiled "Select a TXT File" (strcat(vla-get-path(vla-get-activedocument(vlax-get-acad-object))) "\\") "txt" 2) "r"))
(while (not flag)
	(if (setq q (read-line a))
		(setq spisok_dwgp (append spisok_dwgp (list(list(substr q 1 (vl-string-search "\t" q))(substr q (+ 2 (vl-string-search "\t" q)) (- (strlen q)(vl-string-search "\t" q)))))))
		(setq flag T)
	)
)
(close a)
(setdwgprops spisok_dwgp)
)
(defun C:dwgp2	()(dwgp2))


;; Get Dynamic Block Property Value  -  Lee Mac
;; Returns the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)

(defun LM:getdynpropvalue ( blk prp )
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)

(defun c:colorchange (/ c1 c2 )
(setq c1 (getint " What is the original color: ") c2 (getint " What is the new color: "))
(vlax-for layer 
	(vla-get-Layers (vla-get-ActiveDocument (vlax-get-Acad-Object)))
	(if (= c1 (vla-get-Color layer))(vla-put-Color layer c2))
)
(prin1)
)

(defun c:widthchange (/ c1 c2 )
(setq c1 (getint " What is the original width: ") c2 (getint " What is the new width: "))
(vlax-for layer 
	(vla-get-Layers (vla-get-ActiveDocument (vlax-get-Acad-Object)))
	(if (> c1 (vla-get-lineweight layer))(vla-put-lineweight layer c2))
)
(prin1)
)

(defun colorchange (c1 c2 / )
(vlax-for layer 
	(vla-get-Layers (vla-get-ActiveDocument (vlax-get-Acad-Object)))
	(if (= c1 (vla-get-Color layer))(vla-put-Color layer c2))
)
(prin1)
)

(defun widthchange (c1 c2 /  )
(vlax-for layer 
	(vla-get-Layers (vla-get-ActiveDocument (vlax-get-Acad-Object)))
	(if (< c1 (vla-get-lineweight layer))
		(vla-put-lineweight layer c2)
	)
)
(prin1)
)

(defun colorchange2 (c1 c2 / )
(vlax-for layer 
	(vla-get-Layers (vla-get-ActiveDocument (vlax-get-Acad-Object)))
	(if (vl-string-search (strcase c1) (strcase(vla-get-name layer)))
		(vla-put-Color layer c2)
	)
)
(prin1)
)

(defun widthchange2 (c1 c2 /  )
(vlax-for layer 
	(vla-get-Layers (vla-get-ActiveDocument (vlax-get-Acad-Object)))
	(if (vl-string-search (strcase c1)
		(strcase(vla-get-name layer)))(vla-put-lineweight layer c2)
	)
)
(prin1)
)

;меняет цвет слоев на белый (7) и ширину слоев более 0,35 делает 35
(defun zd181 (  /  )
;(colorchange	6		7	)
;(colorchange	191		7	)
;(colorchange	200		7	)
;(colorchange	211		7	)
;(colorchange	231		7	)
(colorchange2	"ANNO"	7	)
(colorchange2	"A-DETL-THIN"	7	)
(colorchange2	"Размеры"	7	)
(colorchange2	"Выноски"	7	)
(colorchange2	"GRID"	7	)
(widthchange	35		35	)
(widthchange2	"GRID"	9	)
(colorchange2	"DETL"	9	)
(zod23)
(zd158)
(princ)
)

(defun C:Zd181 ()(Zd181))

(defun zd182 ( text01 / ent01 )
(setq ent01 (vlax-ename->vla-object(car(entsel))))
(vla-put-textstring 
	ent01
	(strcat (vla-get-textstring ent01) text01)
)
(princ)
)

(defun C:Z1 ()(zd182 "-Л"))
(defun C:Z2 ()(zd182 "-П"))

(defun zd183 (  /  )
(sssetfirst nil (ssget "_X" '((0 . "*TEXT") (-4 . "<OR") (1 . "*500*")(1 . "*300*")(-4 . "OR>"))))
)

(defun zd1831 (  /  )
(sssetfirst nil (ssget "_X" '((0 . "*TEXT") (-4 . "<OR") (1 . "*500*")(1 . "*300*")(1 . "*эл*")(-4 . "OR>"))))
)

(defun C:zd183 ()(zd183))

;Выделение обычных блоков содержащих текст в имени
(defun zd184 (  blk_name01 / ss1 )
(setq blk_name01 (strcat "*" blk_name01 "*"))
(sssetfirst nil (ssget "_X" (list '(0 . "INSERT") (cons 2 blk_name01 ))))
(if	
	(setq ss1 (ssget "_X" (list '(0 . "INSERT") (cons 2 blk_name01 ))))
	(sssetfirst nil ss1)
	(princ "\n Не найдены блоки с таким именем \n")
)
)

(defun C:zd184 ()(zd184 "ннотация"))

;Выделение  блоков из 1 элемента
; princl - это Lee Mac
(defun zd185 ( / list01)
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
		(if (< (vla-get-count block_name) 2)
			(setq list01 (append list01 (list (cons 2 (vla-get-name block_name)))))
		);_end_of_if
);_end_of_vlax-for
(princl (vl-sort list01 (function (lambda (a b) (< (cdr a) (cdr b))))))
)

;Выделение  блоков из 1 элемента через меню
(defun zd1852 ( / list01)
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
		(if (< (vla-get-count block_name) 2)
			(setq list01 (append list01 (list (vla-get-name block_name))))
		);_end_of_if
);_end_of_vlax-for
(setq list01 	(vl-sort list01 '<))
(setq list01 	(loc:dwgru-get-user-dcl "Укажите блоки,которые хотите выделить" list01 t))
(setq list01	(mapcar '(lambda (name) (cons 2 name))list01))
(setq list01 	(append (list '(-4 . "<OR"))  list01))
(setq list01 	(append  list01 (list '(-4 . "OR>"))))
(sssetfirst nil (ssget "_X" list01))
)

;Взрыв  блоков из 1 элемента
(defun zd186 ( / list01)
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
		(if (< (vla-get-count block_name) 2)
			(setq list01 (append list01 (list (cons 2 (vla-get-name block_name)))))
		);_end_of_if
);_end_of_vlax-for
(setq list01 (append (list '(-4 . "<OR"))  list01))
(setq list01 (append  list01 (list '(-4 . "OR>"))))
(sssetfirst nil (ssget "_X" list01))
)

(defun C:zd185 ()(zd185))
(defun C:zd186 ()(zd186))
(defun C:zd1852 ()(zd1852))

;Выделение элементов толще 35
(defun zd187 ( / )
(sssetfirst nil (ssget "_X" '((-4 . ">")(370 . 35))))
)

(defun C:zd187 ()(zd187))

;Поиск в блоках элементов тощиной более указанной и замена на указанную
(defun zd188 ( old_Lineweight01 new_Lineweight01 / block_name entity_name)
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
	(vlax-for entity_name block_name
		(if (vlax-property-available-p entity_name 'Lineweight)
			(if
				(>  (vla-get-Lineweight entity_name) old_Lineweight01)
				(vla-put-Lineweight entity_name new_Lineweight01)
			)
		);_end_of_if
	);_end_of_vlax-for
);_end_of_vlax-for
(princ)
)

(defun C:zd188 ()(zd188 35 35))

;поиск листов среди укзанных в списке и устновка листа и принтера
(defun zd189 ( / poogreshnost spis_listov p1 p2 deltaX deltaY long01 short01 format01 act_lay Numerator Denominator pogreshnost02 pt1 pt2)
(setq 	pogreshnost 	25
		pogreshnost02 	0.1
		printer_name	"DWG To PDF.pc3"
		act_lay 		(vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object)))
		spis_listov
	'(
		(	"ISO_full_bleed_A0_(841.00_x_1189.00_MM)"		1189		841		)
		(	"ISO_full_bleed_A1_(594.00_x_841.00_MM)"		841			594		)
		(	"ISO_full_bleed_A2_(420.00_x_594.00_MM)"		594			420		)
		(	"ISO_full_bleed_A3_(297.00_x_420.00_MM)"		420			297		)
		(	"ISO_full_bleed_A4_(210.00_x_297.00_MM)"		297			210		)
	)
)
(setq p1 (getpoint 		"\n Укажите 1 точку : "))
(setq p2 (getcorner p1 	"\n Укажите 2 точку : "))
(setq 	deltaX (abs (- (car p1) (car p2)))
		deltaY (abs (- (cadr p1) (cadr p2)))
)
(if (> deltaX deltaY)
	(setq long01	deltaX	short01	deltaY)
	(setq short01	deltaX	long01	deltaY)	
)
(mapcar '(lambda (name)
	(if	(and
			(> pogreshnost (abs (- long01	 (cadr	name))))
			(> pogreshnost (abs (- short01	 (caddr	name))))
		)
		(setq format01 (car name))
	)
	)
	spis_listov
)
(if (not format01)
	(progn
		(princ (strcat "\n Лист со сторонами  " (rtos long01 2 2) "x" (rtos short01 2 2) " не найден"))
		(princ)
		(exit)
	)
)
(vl-catch-all-apply '(lambda () (vla-put-ConfigName 			act_lay 	printer_name	)))
(vl-catch-all-apply '(lambda () (vla-put-CanonicalMediaName		act_lay 	format01		)))
(vla-GetCustomScale act_lay 'Numerator 'Denominator)
;разбить на 2 условия для вертикальных не работает
(if	(or
		(> pogreshnost02 (abs (- Numerator	 	1)))
		(> pogreshnost02 (abs (- Denominator	1)))
	)
	(vl-catch-all-apply '(lambda () (vla-put-PlotRotation	act_lay "1"	)))
	(vl-catch-all-apply '(lambda () (vla-put-PlotRotation	act_lay "0"	)))
)
(setq 	pt1 	(vlax-safearray-fill(vlax-make-safearray vlax-vbDouble '(0 . 1))(list (min (car p1)(car p2)) (min (cadr p1)(cadr p2)) )) 
		pt2 	(vlax-safearray-fill(vlax-make-safearray vlax-vbDouble '(0 . 1))(list (max (car p1)(car p2)) (max (cadr p1)(cadr p2)) ))
)
(vla-SetWindowToPlot act_lay pt1 pt2)
(vl-catch-all-apply '(lambda () (vla-put-PlotType	act_lay acWindow)))
(vla-regen (vla-get-activedocument(vlax-get-acad-object)) acallviewports)
(princ (strcat "\n Установлен принтер " printer_name))
(princ (strcat "\n Установлен лист " format01))
(princ)
)


(defun C:zd189 ()(zd189))




;1 это начало 2 это конец
(defun MM:divide_string ( char01 string01 na4_kon / flag01 list01 list02 string01 fin01)
(setq 	char01 		(car(vl-string->list char01))
		string01 	(vl-string->list string01))
(mapcar '(lambda(x)
	(if (equal flag01 nil)
		(if	(equal x char01)
			(setq 	list01 (append list01 (list x))
					flag01 T				)
			(setq 	list01 (append list01 (list x)))
		)
		(setq 	list02 (append list02 (list x)))
	)
	)
string01
)
(if	(equal 1 na4_kon)
	(setq fin01 (vl-list->string list01))
	(setq fin01 (vl-list->string list02))
)
)



;поиск в тексте буквы бразбитие строки на 2 части до буквы и после б вставка по середине строки
(defun zd190 ( text01 char01 / ent01 )
(if 	(null (vlax-property-available-p (vlax-ename->vla-object(setq ent01 (car (entsel)))) 'textstring))
		(exit)
		(setq ent01 (vlax-ename->vla-object ent01))
)
(vla-put-textstring 
	ent01
	(strcat 	(MM:divide_string char01 (vla-get-textstring ent01) 1) 
				text01 
				(MM:divide_string char01 (vla-get-textstring ent01) 2)
	)
)
(princ)
)

(defun C:Z1 ()(zd190 "(л)" "к"))
(defun C:Z2 ()(zd190 "(п)" "к"))


(defun MM:copy_layer_with_new_color ( / ent col )
(setq ent (entget(car(entsel))))
(setq col (cdr (assoc 62 ent)))
(if (/= col nil)
  (if (and (> col 0)(< col 256))
	(progn
	  (setq lay (strcat (cdr(assoc 8 ent)) "_" (itoa col)))
	  (if (= (tblsearch "LAYER" lay) nil)
		(entmake
			(list
				(cons 0 "LAYER")
				(cons 100 "AcDbSymbolTableRecord")
				(cons 100 "AcDbLayerTableRecord")
				(cons 2  lay)
				'(70 . 0)
				(cons 62  col)
				(assoc 6(tblsearch "LAYER" (cdr(assoc 8 ent))))
				(assoc 370 (entget(tblobjname "LAYER" (cdr(assoc 8 ent)))))
			)
		)
	  )
	)
  )
)
(princ)
)

(defun C:zd191 ()(MM:copy_layer_with_new_color))

(defun Zd192 ( style01 / Layout_name)
(princ "\nвыставляет на всех листах нужный стиль печати")
(vlax-for Layout_name (vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))
	(if (not(equal (vla-get-Name Layout_name) "Model"))
		(vla-put-StyleSheet Layout_name style01)
	)
);_end_of_vlax-for
)

(defun c:Zd192 () (Zd192 "monochrome.ctb"))

(defun Zd193 ( / ss3 )
(princ "\n Выделяет блоки с аттрибутами и нединамические")
(setq	ss3 	(ssadd))
(princ "\nВыберете блоки")
(mapcar
	'(lambda (name)	(if	(and	(vlax-invoke name 'getattributes)
								(null (vlax-invoke name 'GetDynamicBlockProperties))
						)
					(setq ss3 (ssadd (vlax-vla-object->ename name) ss3))
					)
	)
	(mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex(ssget '((0 . "INSERT")))))))
)
(sssetfirst nil ss3)
(princ)
)

(defun c:Zd193 () (Zd193))

;выделение примитиво с цветом не by layer
(defun Zd194 ( / i )
(sssetfirst nil (ssget  '((-4 . "<>")(62 . 256))))
)

(defun c:Zd194 () (Zd194))


(defun getsavever ( / zz5 )
(setq zz5 (atoi(substr (ver) 13)))
(cond	
	(	(>= zz5 2018)	(setq  zz5	2018)		)
	(	(>= zz5 2013)	(setq  zz5	2013)		)
	(	(>= zz5 2010)	(setq  zz5	2010)		)
	(	(>= zz5 2007)	(setq  zz5	2007)		)
	(	(>= zz5 2004)	(setq  zz5	2004)		)
	(	(>= zz5 2000)	(setq  zz5	2000)		)
)
)

;разбивка строки на несколько строк split string
(defun SplitStr ( str01 del01 / pos )
  (if (setq pos (vl-string-search del01 str01))
    (cons (substr str01 1 pos) (SplitStr (substr str01 (+ pos 1 (strlen del01))) del01)) (list str01)))

;разбивка строки на две строки split string once
(defun SplitStrOnce ( str01 del01 / pos )
	(if	(setq pos (vl-string-search del01 str01))
		(cons 
			(substr str01 1 pos) 
			(substr str01 (+ pos 1 (strlen del01)))
		)
		(list str01)
	)
)

(defun zd195 ( / ent1 tex01) 
(setq 	ent1 	(vlax-ename->vla-object(car(entsel "Выберете текст"))))
(vla-move 
	(vla-copy ent1)
	(vla-get-InsertionPoint ent1) 
	(vlax-3d-point 
		(getpoint "\nБазовая точка: " 
			(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint ent1))))))
(vla-put-TextString		(vlax-ename->vla-object(entlast))	(cdr(SplitStrOnce(vla-get-TextString ent1) " ")))
(vla-put-TextString		ent1								(car(SplitStrOnce(vla-get-TextString ent1) " ")))
)

(defun c:zd195 () (zd195))

;удаляет все dxf пары кроме требуемой
(defun MM:remove_a_e_one ( ent01 bit / )
(vl-remove-if-not 
		(function (lambda(x)
			(= (car x) bit)
			   )
		)
	(entget ent01)
)
)

;выделяет полилинии из cur_points точек для последующего взрыва 1 - во всем чертеже 0 - в указанной области
(defun MM:select_pl_with_cur_points ( cur_points flag01 / ss1 ss2 )
(if	(equal flag01 1)
	(setq ss1 (vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget "_X" 	'((0 . "LWPOLYLINE")))))) ss2 	(ssadd))
	(setq ss1 (vl-remove-if 'listp (mapcar 'cadr (ssnamex (ssget 		'((0 . "LWPOLYLINE")))))) ss2 	(ssadd))
)
	(mapcar '(lambda (x)
		(if (equal cur_points (length (MM:remove_a_e_one x 10)))
			(ssadd x ss2)
		)
		  );end of lambda
		ss1
	)
	(setq ss2 ss2)
)


;выделяет полилинии из 2 точек для последующего взрыва 1 - во всем чертеже 0 - в указанной области
(defun zd196 ( flag01 / ss1 ss2 )
	(sssetfirst nil (MM:select_pl_with_cur_points 2 flag01))
)

(defun c:zd196 () 	(zd196 	1	))
(defun c:zd1962 () 	(zd196 	0	))






(defun LM:grouplayerfilters nil
   (   (lambda ( foo ) (foo (entget (cdr (assoc 330 (entget (tblobjname "layer" "0")))))))
       (lambda ( enx / dic itm rtn )
           (and (setq dic (cdr (assoc 360 (member '(102 . "{ACAD_XDICTIONARY") enx))))
                (setq dic (cdr (assoc -1  (dictsearch dic "aclydictionary"))))
                (while (setq itm (dictnext dic (not itm)))
                    (if (= "AcLyLayerGroup" (cdr (assoc 1 itm)))
                        (setq rtn (cons (cons (cdr (assoc 300 itm)) (foo itm)) rtn))
                    )
                )
           )
           (reverse rtn)
       )
   )
)

(defun Zd197 (  / Layout_name)
(princ "\nвыставляет отбражение/скрытие стиля печати")
(if	(equal (vla-get-ShowPlotStyles(vla-get-ActiveLayout(vla-get-activedocument(vlax-get-acad-object)))) :vlax-true)
	(vlax-for Layout_name (vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))
		(vla-put-ShowPlotStyles Layout_name ':vlax-false)
	)
	(vlax-for Layout_name (vla-get-Layouts(vla-get-activedocument(vlax-get-acad-object)))
		(vla-put-ShowPlotStyles Layout_name ':vlax-true)
	)
)
(vla-Regen (vla-get-activedocument(vlax-get-acad-object)) 1)
)

(defun c:Zd197 () 	(Zd197 	))


(defun Zd198 (  / view01 ss01)
(setvar "REGENMODE" 0)
(setq 	view01 (car(entsel))
		ss01 (vl-remove-if 'listp (mapcar 'cadr (ssnamex(ssget '((0 . "LWPOLYLINE")))))))
(mapcar '(lambda (x)
	(VL-CMDF "_VPCLIP" (vlax-vla-object->ename(vla-copy (vlax-ename->vla-object view01))) x)
	(VL-CMDF)
	)
ss01
)
(setvar "REGENMODE" 1)
)

(defun c:Zd198 () 	(Zd198 	))


(defun find_or_create_lay ( layer_name01 / )
	(if	(null(tblobjname "Layer" layer_name01))
		(vla-add(vla-get-Layers(vla-get-activedocument(vlax-get-acad-object)))layer_name01)
	)
)

(defun match-layer-prop (src01 trg01 / source target)
(setq 	source	(entget (tblobjname "layer" src01))
		target	(entget (tblobjname "layer" trg01))
)
(entmod
	(append (vl-remove-if
		'(lambda (x) (member (car x) '(70 62 6 290 370 390 347)))
		target
		)
		(vl-remove-if-not
			'(lambda (x) (member (car x) '(70 62 6 290 370 390 347)))
			source
		)
	)
)
(entupd (cdr (assoc -1 target)))
(princ)
)

(defun create-lay-match-layer-prop (new01 old01 / )
	(if (find_or_create_lay new01)
		(match-layer-prop old01 new01)
	)
)

;разбивает слой чтобы каждый тип примитва был в отдельном слое
(defun Zd199 ( layer_name01 / usercmd ent)
(setq usercmd (getvar "CMDECHO"))
(setvar "CMDECHO" 0)
(setvar "regenmode" 0)
(setq ent (entnext))
(while ent
	(if (equal  (cdr(assoc 8(entget ent))) layer_name01 )
		(progn
			(create-lay-match-layer-prop (strcat	layer_name01 "_" (cdr(assoc 0(entget ent)))) layer_name01)
			(entmod		
				(subst 	(cons 8 (strcat	layer_name01 "_" (cdr(assoc 0(entget ent)))))
						(assoc 8 (entget ent))
						(entget ent)
				)
			)
		)
	)
	(setq ent (entnext ent))
)
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
		(if (vlax-property-available-p block_name 'Layer)
			(if
				(equal  (vla-get-Layer block_name) layer_name01)
				(progn
					(create-lay-match-layer-prop 	(strcat	layer_name01 "_" (cdr(assoc 0(entget(vlax-vla-object->ename block_name))))) layer_name01)
					(vla-put-Layer block_name 	(strcat	layer_name01 "_" (cdr(assoc 0(entget(vlax-vla-object->ename vlog_block_name))))))
				)
			)
		);_end_of_if
)
(vlax-for block_name (vla-get-blocks(vla-get-activedocument(vlax-get-acad-object)))
	(vlax-for vlog_block_name block_name
		(if (vlax-property-available-p vlog_block_name 'Layer)
			(if
				(equal  (vla-get-Layer vlog_block_name) layer_name01)
				(progn
					(create-lay-match-layer-prop 	(strcat	layer_name01 "_" (cdr(assoc 0(entget(vlax-vla-object->ename vlog_block_name))))) layer_name01)
					(vla-put-Layer vlog_block_name 	(strcat	layer_name01 "_" (cdr(assoc 0(entget(vlax-vla-object->ename vlog_block_name))))))
				)
			)
		);_end_of_if
	);_end_of_vlax-for
)
(setvar "CMDECHO" usercmd)
(setvar "regenmode" 1)
(princ)
)


;аналог синего в rgb 19,26,236
;(dictSEARCH (namedobjdict) "ACAD_MILLER_DICTIONARY")
;http://www.lee-mac.com/ssget.html полезное про ssget
;(entdel (handent "5154"))






