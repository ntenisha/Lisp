(vl-load-com)

;(if		(getvar "CMDECHO")				(setvar "CMDECHO" 0))
;(if		(getvar "FILEDIA")				(setvar "FILEDIA" 0))
;(if		(getvar "QAFLAGS")				(setvar "QAFLAGS" 5))

(defun load_lisp ( file_name / )
	(if 
		(vl-catch-all-error-p(vl-catch-all-apply '(lambda () (load file_name))))
		(princ (strcat "\n внутри файла " file_name " ошибка \n"))
	)
(princ)
)

;уточнить версию и подгрузить только 1 раз
;(loadarx "SelSim2019x64.arx")
;(loadarx "ExplodeProxy2019x64.arx")
;(loadarx "GetDwgProps2019x64.arx")

(defun loadother ( / )
(load  "help-paper.VLX")
(load  "APEL.FAS")
(load "Zod_commands.lsp")
(progn
  (princ "\n Комманда (apel-com '(apel-draw-find_analog_objects)) \n")
  (princ "\n Комманда AD \n")
)
)

(defun loaddvb ( dvbfile /  )(command "_vbaload" dvbfile)(princ))

(defun loadarx ( arxfile /  )
(setvar "FILEDIA" 0)
(command "_ARX" "_Load" arxfile)
(setvar "FILEDIA" 1)
(princ)
)


(load "Annotative Tools.lsp")
(load "Align_Den.lsp")
(load "AF.lsp")
(load "flatten1.lsp")
(load "bgtools 3.12a.lsp")
(load "BFindV2-0.lsp")
(load "Break All or Some .LSP")
(load "break-total.LSP")
(load "ChangeBlockBasePointV1-5.lsp")
(load "cpsll.lsp")
(load "en.lsp")
(load "pl_obj-filter-select-all.lsp")
(load "pl_obj-filter-select-manual.lsp")
(load "quickdraw.LSP")
(load "remove-block.lsp")
(load "StripMtext v5-0c.lsp")
(load "TextCalc.LSP")
(load "ttc.LSP")
(load "VpFreezedLays.lsp")
(load "SSD_Dynamic_block_filter.lsp")
(load "Purge_plus.lsp")
(load "kpblc-autostart-purge.lsp")
(load "dr(dictionaryremove).lsp")
(load "tabsort_ru-2.2.lsp")
(load "addlay.lsp")

;(loaddvb "Number x64.dvb")
(load "RunNumber.LSP")


(load "Zod_commands.lsp")
(load "Zod_functions_my.lsp")
(load "Zod_functions_not_my.lsp")
(load "Zod_functions_Lee_Mac.lsp")
(load "Zod_lisp")
(load "Zod_variables")


(load "Zod_lastload")


(progn(dictadd (namedobjdict) "ACAD_MILLER_DICTIONARY" (entmakex (list '(0 . "DICTIONARY") '(100 . "AcDbDictionary"))))(princ))

;(dictsearch (namedobjdict) "ACAD_MILLER_DICTIONARY")

(princ "\n файл Zod_lisp успешно загружен \n")
