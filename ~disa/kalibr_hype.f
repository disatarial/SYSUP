
\ �������� ������ ����������  ������ ������� - �������,  "��������� ��������"
\ ������ ������� �  �������������� ������ !!!!!
\ ������������� ������ -  ������� ��������������� 


\ tabl_kalibr NEW "���"  = �������� ����� �������
\ "���-�� �����"  "���-�� ��������" "���" LoadDatas:   = ������������� �������� �������  "���"
\ 
\  LoadData: �1 �2 ...  \ �������� ������ ������� 1
\ �1 �2 ...�4 LoadData   \ �������� ������ ������� 2 

\ "���" SeeDatas   -���������� ������� ������
\  n  TakeData 
\ freq	n "���"  TakeData \ �������� ������ �� ������� n  � �������� freq �� ������� "���" 

\ -----------------------------------
\  � ����� ����   "LoadDatas: " � "�������������" ����������� ��� �������� �������� �������� �� �������� �����
\ "���"  "���-�� �����"  "���-�� ��������" LoadDatas:   = ������������� �������� �������  "���"
\ -----------------------------------


 REQUIRE  N_S_PickUpWord ~disa\dopoln.f
 REQUIRE  F. lib\include\float2.f
REQUIRE  HYPE ~day\hype3\hype3.f

0 VALUE tekdata \ ������������ �����! ?? 

CLASS tabl_kalibr

CELL DEFS  datas \ ������ �� ������
CELL DEFS num_datas  \ ���������� ����������� �����
CELL DEFS max_datas  \ ����� �����
CELL DEFS num_datas_in_string   \ ���������� ������ �  ������


:  adr_data_in_number    || D:  N_write || N_write ! num_datas_in_string @ FLOATS  * datas @ +  N_write @ FLOATS +   ;


: take_freq_in_number \ �������  ������ � �������� ��������
\   num_datas_in_string  @ FLOATS * datas @ + SF@
    0  adr_data_in_number  F@ 
    ;
 
:  take_data_in_number  (  n_freq , n_write  -- )
\  || D:  N_write ||
\ N_write ! num_datas_in_string @ FLOATS  * datas @ +  N_write @ FLOATS +
  adr_data_in_number  F@ ;


: NextFreq ( freq_old -- freq_new )
|| D:  num   D: n F: freq   ||
 num ! \ ����� ����� 
 freq F!  0 n !
 num_datas @ \ ���������� �����
 0  DO
 I  take_freq_in_number    freq F@  F<  IF I  n !  THEN 
  LOOP
  n @   take_freq_in_number 
;

: TakeData \ ( freq  num  -- data )
|| D:  num   D: n F: freq   ||
num ! \ ����� ����� 
 freq F!  0 n !
num_datas @ \ ���������� �����
 0  DO
 I  take_freq_in_number    freq F@  F<  IF I  n !  THEN 
  LOOP
 n @  num_datas @ 1- = IF num_datas @  2 -  n ! THEN
   \ �������� ������������
  \ (�1-1024 �2)/(�1-�2)*�1 +�1
  n @ 1+ num @ take_data_in_number  n @ num @ take_data_in_number  F-  \ (�k-�n)  
  n @ 1+  take_freq_in_number  n @  take_freq_in_number  F-  \ (�k-�n)
 F/  ( ***/***)
 freq F@
 n @  take_freq_in_number    \ (***) *�1
  F- F* \ (***) *�1
   \  n 1+ ������������������������������    FSWAP F- \ (***) + �1
  n @ num @ take_data_in_number     F+ 
\ CR n @ . num_datas @ . num @ .  F.
 ;


 : LoadData 
 num_datas @ 1  adr_data_in_number F!
 num_datas @ 0  adr_data_in_number F!
 num_datas @ 1 + num_datas   !
;

\ : :SetData  datas ! ;
 :  LoadDatas:   (   adr_data,   n_Data )
 SELF TO tekdata 
 num_datas_in_string ! 0  num_datas ! \ :SetData  
 num_datas_in_string @ FLOATS *  CELL + ALLOCATE THROW   datas !
 ;

 : SeeDatas \ { adr \ --  }
 \ || D: adr || adr !
\ num_datas @  . num_datas_in_string @  . CR
CR
num_datas @ 0 DO
I take_freq_in_number F.

num_datas_in_string @ 1 DO
		J I take_data_in_number F. 
	LOOP
CR
LOOP 
;



 : LoadData: ( num -- )
\ num_datas @ . num_datas_in_string @ .  ." |"
 num_datas_in_string @  0 DO   
 NextWord  \ 2DUP TYPE SPACE
  STR>S  S>FLOAT  DROP \ F.
 num_datas @ I adr_data_in_number F!
 LOOP
\ num_datas @ 1  adr_data_in_number F!
\ num_datas @ 0  adr_data_in_number F!
 num_datas @ 1 + num_datas   !
;

 : dispose datas @ FREE THROW  ;

;CLASS

: LoadDatas: ROT ^ LoadDatas: ; ( obj line col -- )
: LoadData tekdata  ^ LoadData ;  ( data1 .. dataN -- )
: LoadData: tekdata  ^ LoadData: ;   ( data1 .. dataN -- )
: SeeDatas   ^ SeeDatas ;



\EOF

 tabl_kalibr NEW kalibrovka 

 tabl_kalibr NEW k12 
\ kalibrovka  .
\  kalibrovka  see tabl_kalibr NEW kalibrovka 

\ 0 , HERE  1024  ALLOT VALUE kalibrovka

CR
10 2 kalibrovka   LoadDatas:

 80.000e6	-20.6E	LoadData
 300.000e6	-9.9E	LoadData
600.259e6	-14.4E	LoadData
1.000e9        -1.1E	LoadData
2.000e9        -1.1E	LoadData
3.000e9        -1.1E	LoadData
4.000e9        -1.1E	LoadData
\ EOF
kalibrovka SeeDatas
CR
\  0 , HERE  1024  ALLOT VALUE k12
10 2 k12  LoadDatas:
 1.000e6	 1.E	LoadData
 2.000e6	 2.E	LoadData
 3.000e6	 3.e	LoadData
 4.000e6	 4.E	LoadData
 5.000e6	 5.E	LoadData
 6.000e6	 6.E	LoadData
 7.000e6	 7.E	LoadData
k12 SeeDatas

: z 
4.5000e6	1 k12 TakeData  F.
 0.000e6		1 k12 TakeData F. 
 8.000e6		1 k12 TakeData  F. 
; 
CR

tabl_kalibr NEW k0 

30 3 k0  LoadDatas:
 LoadData:   1.000e6  1.2E	10.5E
 LoadData:   2.000e6  3.E	10.6E
 LoadData:   3.000e6  4.E	10.7E
 k0 SeeDatas
  1.000e6 1 k0 TakeData  F.
  