;   _.filetab  ������� ������ ������� _.filetab, _.filetabclose. ��������� ���������� filetabstate.

;CONVERTPSTYLES ��������������� stb � ctb



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



;  �� ����������� ������ ���� ��� �������
;(setcurvar "startup" 2)

;����� ������ ������� ������� � AutoCAD 2016 ��� 2017 ������ ����� ��������������� ��������� ����������: STARTMODE =0. ���� �� ������ �������� �1�, �� ������� ����� �������������� ���������.��� ����� ������ ������ AutoCAD 2014-2015 ��������� ������ ��������� ����������: NEWTABMODE. ��� ���������� ������� ������� ������� ����� ��������� �������� �0�.
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

;��� ���������� ������ stripmtext
(setcurvar 		"LISPSYS" 				0		)
;������������ ���� + ������� ��� ���������
(setcurvar 		"DYNMODE" 				3		)
;���������� ���� ��� ������
(setcurvar 		"SELECTIONCYCLING" 		0		)
;����� ������ ����� �� �������
(setcurvar 		"STATUSBARAUTOWRAP" 	0		)


;���������� �������� ���������
(setcurvar 		"ANNOMONITOR" 			-2		)
;��������� �����
(setcurvar 		"SNAPMODE" 				0		)
;�������� ����������� ������ �����
(setcurvar   	"LWDISPLAY"     		1		)
;��������� �������� 16384 - ��� ��������
(setcurvar 		"OSMODE" 				16383	)
;��������� �������� � �������� ������� ��� ��������������� ��������������� ������, ����������� ��� ����������� ��� ������������� � ������� ������� ������ 4 - ���������
(setcurvar 		"INSUNITS" 				4		)
;������������� ������� ��������� ��������� �����������, ���� �������� ���������� INSUNITS ����� 0. 
(setcurvar 		"INSUNITSDEFSOURCE" 	4		)
;����������� � �������� arx ��������
(setcurvar 		"DEMANDLOAD" 			2		)
;���������� proxy
(setcurvar 		"PROXYSHOW" 			1		)
;����������� � ������
(setcurvar 		"PROXYNOTICE" 			0		)
(setcurvar 		"PREVIEWEFFECT" 		2		)
;��������� ���� ����� ��� ����� ��������.  ������������� ��� ���� ����� �������� ������
(setcurvar 		"CELWEIGHT" 			-1		)
;��������� �������� ,��� ��������
(setcurvar 		"AUTOSNAP" 				63		)
;������� ������������ ���� 15 ��������
(setcurvar 		"POLARANG" 				(* pi (/ 15 180.0))		)
;��������� ������������ ����� � ��������
(setcurvar 		"TRACKPATH" 			0		)
;�� ������������� ���� �� ���� ������������
(setcurvar 		"MIRRTEXT" 				0		)
;���� �� �������� ������� Delete
(setcurvar 		"PICKFIRST" 			1		)
;���� �� �������� ��������������� �����
(setcurvar 		"QAFLAGS" 				2		)
;������ ������������� ��������������� ������������
(setcurvar 		"CONSTRAINTINFER" 		0		)
;����� � ����� ����
(setcurvar 		"SDI" 					0		)
;���������� �������� ��� ������� ������������ �����
(setcurvar 		"GRIPOBJLIMIT" 			5000	)

;������ ����� ��� ���������
(setcurvar 		"PICKBOX" 				8		)
(setcurvar 		"GRIPSIZE" 				8		)
(setcurenv		"AutoSnapSize" 			"8"		)


;���������� ������������� ������ ��� ��������� ������� �����
(setcurvar 		"FILETABPREVIEW" 			0		)
(setcurvar 		"FILETABTHUMBHOVER" 		0		)



;����������� ����������� �������� � ��� �������� � ����� ��� ����
;(if		(getvar "ANNOALLVISIBLE")				(setvar "ANNOALLVISIBLE" 1))
;(if		(getvar "ANNOAUTOSCALE")				(setvar "ANNOAUTOSCALE" -4))


