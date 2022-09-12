;   _.filetab  вкладки файлов Команды _.filetab, _.filetabclose. Системная переменная filetabstate.

;CONVERTPSTYLES конвертирование stb в ctb



(defun setcurvar (varname varvalue / ) 
	(if 	(getvar varname)
			(if 	(not(equal (getvar varname) varvalue))
					(setvar varname varvalue)
			)
	)
(princ)
)
	
(defun setcurenv (varname varvalue / ) 
	(if 	(getenv varname)
			(if 	(not(equal (getenv varname) varvalue))
					(setenv varname varvalue)
			)
	)
(princ)
)



;  не открывается пустой файл при запуске
;(setcurvar "startup" 2)

;Чтобы убрать вкладку «Начало» в AutoCAD 2016 или 2017 версии нужно воспользоваться системной переменной: STARTMODE =0. Если же задать значение «1», то вкладка будет присутствовать постоянно.Для более ранних версий AutoCAD 2014-2015 действует другая системная переменная: NEWTABMODE. Для отключения вкладки «Начало работы» нужно присвоить значение «0».
(setcurvar 		"STARTMODE" 			0		)


(setcurvar 		"VISRETAIN" 			1		)
(setcurvar 		"FILEDIA" 				1		)
(setcurvar 		"EDGEMODE" 				0		)
(setcurvar 		"PEDITACCEPT" 			1		)
(setcurvar 		"gridmode" 				0		)
(setcurvar 		"SELECTIONPREVIEW" 		3		)
(setcurvar 		"LWDEFAULT" 			0		)
(setcurvar 		"elevation" 			0		)
(setcurvar 		"OSNAPZ" 				0		)
(setcurvar		"CMDECHO" 				1		)

;для нормальной работы stripmtext
(setcurvar 		"LISPSYS" 				0		)
;динамический ввод + размеры при наведении
(setcurvar 		"DYNMODE" 				3		)
;отключение меню при выборе
(setcurvar 		"SELECTIONCYCLING" 		0		)
;чтобы панель снизу не скакала
(setcurvar 		"STATUSBARAUTOWRAP" 	0		)


;выключение монитора аннотаций
(setcurvar 		"ANNOMONITOR" 			-2		)
;выключить сетку
(setcurvar 		"SNAPMODE" 				0		)
;включить отображение толщин линий
(setcurvar   	"LWDISPLAY"     		1		)
;объектная привязка 16384 - все включено
(setcurvar 		"OSMODE" 				16383	)
;Указывает значение в единицах чертежа для автоматического масштабирования блоков, изображений или вставленных или прикрепленных к чертежу внешних ссылок 4 - милиметры
(setcurvar 		"INSUNITS" 				4		)
;Устанавливает единицы измерения исходного содержимого, если значение переменной INSUNITS равно 0. 
(setcurvar 		"INSUNITSDEFSOURCE" 	4		)
;определение и загрузка arx объектов
(setcurvar 		"DEMANDLOAD" 			2		)
;показывать proxy
(setcurvar 		"PROXYSHOW" 			1		)
;уведомление о прокси
(setcurvar 		"PROXYNOTICE" 			0		)
(setcurvar 		"PREVIEWEFFECT" 		2		)
;Установка веса линий для новых объектов.  Устанавливает для веса линий значение ПОСЛОЮ
(setcurvar 		"CELWEIGHT" 			-1		)
;Установка привязки ,все включено
(setcurvar 		"AUTOSNAP" 				63		)
;задание отслеживания угла 15 градусов
(setcurvar 		"POLARANG" 				(* pi (/ 15 180.0))		)
;Установка отслеживания углов и объектов
(setcurvar 		"TRACKPATH" 			0		)
;Не разворачивать тект во ремя зеркаличания
(setcurvar 		"MIRRTEXT" 				0		)
;если не работает клавиша Delete
(setcurvar 		"PICKFIRST" 			1		)
;если не работает Предварительный выбор
(setcurvar 		"QAFLAGS" 				2		)
;Отмена автонанесения параметрических зависимостей
(setcurvar 		"CONSTRAINTINFER" 		0		)
;файлы в одном окне
(setcurvar 		"SDI" 					0		)
;количество объектов для которых отображаются ручки
(setcurvar 		"GRIPOBJLIMIT" 			5000	)

;размер ручек для выделения
(setcurvar 		"PICKBOX" 				8		)
(setcurvar 		"GRIPSIZE" 				8		)
(setcurenv		"AutoSnapSize" 			"8"		)


;отключение предпросмотра листов при наведении вкладку файла
(setcurvar 		"FILETABPREVIEW" 			0		)
(setcurvar 		"FILETABTHUMBHOVER" 		0		)



;Отображение аннтативных объектов в том масштабе в каком они есть
;(if		(getvar "ANNOALLVISIBLE")				(setvar "ANNOALLVISIBLE" 1))
;(if		(getvar "ANNOAUTOSCALE")				(setvar "ANNOAUTOSCALE" -4))


