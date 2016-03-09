STARTLOG
\ версия 1.0
\ + возможность запуска без графики 
\ + передача между потоками единой структурой, с возможностью останова, и разбором ошибок 
\ -+ проверка ответов оборудования
\ -+ проверка правильности настроек
\ + таблица по частоте
\ - определение повторов
\ + сохранение коментария
\ - возможность в приборы добавлять калибровки, комбинированные приборы
\ + все в UTF8
\ - проврка перезаписи файлов

 REQUIRE  CASE  lib/ext/case.f

\ сравнение строки и маски, для  проверки ответа оборудования
 REQUIRE WildCMP-U ~pinka/lib/mask.f
 REQUIRE  objLocalsSupport ~day/hype3/locals.f
 REQUIRE  tabl_kalibr  ~disa/kalibr_hype.f
 REQUIRE  dBuV->V  ~disa/algoritm.f

\ передача аргументов для запуска оборудования
0 , HERE 128 ALLOT VALUE NAME_INTERFACE \ тип интерфейса
0 , HERE 128 ALLOT VALUE NUMB_INTERFACE \ адрес внутри интерфейса
: TO_NAME_INTERFACE DUP NAME_INTERFACE C! NAME_INTERFACE 1+ SWAP CMOVE ;
: TO_NUMB_INTERFACE DUP NUMB_INTERFACE C! NUMB_INTERFACE 1+ SWAP CMOVE ;

0 CONSTANT NoError
\ коды управления
1 CONSTANT work_Execute   \ нормальный режим работы
2 CONSTANT work_Stop   \ останов при нормальным режиме работы
3 CONSTANT work_Pause   \ пауза
\ коды ошибок
1001 CONSTANT error_begin_problem
1002 CONSTANT error_end_problem
1003 CONSTANT error_cicle_problem
1004 CONSTANT error_metod_file
1005 CONSTANT error_nastr_file

2001 CONSTANT error_out_file  \ ошибки подключаемых файлов

3001  CONSTANT  error_face \ ошибка данных в окне


\ минимальный абор для управления программой
 0
256 -- file_metod_buf		\ метод.   из него берутся остальные насторйки
256 -- file_nastr_buf		\ имя файла настроек
256  -- file_result_buf		\ буфер имени файла результата
CELL -- STEP_PAUSE \ начальная данные для межшаговой паузы
CELL -- STEP_TIME \ начальная дянные для длительности стоянки в точке
CELL -- prog_result
CELL -- Len_Coment_buffer \ размер данных в буфере для коментария настроек
1024 -- Coment_buffer \ буфер  для коментария настроек
CELL -- Len_Coment_metod_buffer \ размер данных в буфере для коментария метода
1024 -- Coment_metod_buffer \ буфер  для коментария метода
CELL -- Len_opisanie_buffer \ размер данных в буфере для коментария метода
1024 -- Coment_opisanie_buffer \ буфер  для коментария метода
1024 -- Data_buffer \ буфер  для служебных выводимых на экран
256  -- _file_name_bufer_ \ буфер  передачи имени между потоками
256  -- ERROR_PROG_BUFER \ буфер  передачи ошибки
CELL -- ERROR_PROG \ номер ошибки
CELL -- work_thread \ запуск потока
CELL -- tid-thead \ -  физическое управление потоком
CELL -- -upravl_thead-  \ логический ключ для процедуры  запуска/останова  испытаний.. все удаления нужно делать в том же потоке, в котором и запускались
CELL -- refresh  \ обновление
VALUE sysup-work-info  

0 VALUE work-info \ минимальный набор дла запуска программы
0 VALUE metod-info  \ дополнительные настроки программы, определяются в методе

 HERE DUP >R sysup-work-info DUP ALLOT ERASE TO work-info


: COMMENT( { \ num  adr len  -- }
  BEGIN
    [CHAR] ) DUP PARSE   -> len -> adr 
	\    \ тут нужна защита от переполнения 
    adr  work-info Coment_buffer  num + len   CMOVE \ скопировали все в буфер
	num len + -> num		\ установили новый его размер
    adr len + C@ = 0=
  WHILE
    REFILL 0= IF EXIT THEN
  REPEAT      
   work-info Coment_buffer   num + 1+  0!
   num work-info  Len_Coment_buffer  !
;

: COMMENT_METOD( { \ num  adr len  -- }
  BEGIN
    [CHAR] ) DUP PARSE   -> len -> adr 
	\    \ тут нужна защита от переполнения 
    adr  work-info Coment_metod_buffer  num + len   CMOVE \ скопировали все в буфер
	num len + -> num		\ установили новый его размер
    adr len + C@ = 0=
  WHILE
    REFILL 0= IF EXIT THEN
  REPEAT      
   work-info Coment_metod_buffer   num + 1+  0!
   num work-info  Len_Coment_metod_buffer  !
;



: To_Data_buffer   { adr u \  -- }
work-info Data_buffer  @ 1 < IF 1024  0 DO 0 I work-info Data_buffer  + C! LOOP THEN \ очищение буфера
work-info Data_buffer @ u + 1024 CELL -  > \ ограничение на буфер
IF
 1024 CELL - work-info Data_buffer @  -  -> u
THEN
adr work-info Data_buffer CELL+  work-info Data_buffer @ + u CMOVE 
work-info Data_buffer @ u + work-info Data_buffer !
 \ ."  ->" u . work-info Data_buffer @ . ." <- "
;

: Str_Data_buffer  ( -- s )
work-info Data_buffer CELL+ work-info Data_buffer @ 
0 work-info Data_buffer  !
;

VECT BEGIN:
VECT WORK:
VECT END:
VECT  NewStep \ Функция вычисления частоты следующего шага
VECT WORK_BEGIN: \ "первый" шаг
VECT WORK_END:	\ "последний" шаг
VECT IF_END:	\ условие окончания
VECT  see 
VECT  SeeOne \ для вывода в различные окошки
VECT  SAVE_NASTR_DATA \ сохранение настроек  начальных данных
 
: NOP ;
' NOP TO BEGIN:
' NOP TO  WORK:
' NOP TO  END:
' NOP   TO  NewStep 
' NOP  TO  WORK_BEGIN:
' NOP  TO  WORK_END:
' NOP  TO  SAVE_NASTR_DATA
:NONAME DROP 0 ; TO SeeOne 
:NONAME  0 ; TO  IF_END:
\ управляемая пауза


: LOAD_TO_BUFER { s-adr adr \ u   -- }
s-adr STR@  TYPE CR
s-adr STR@  DUP 255 > IF DROP 255 THEN -> u
adr  1+ u CMOVE 
s-adr STRFREE
u adr  C!
;


: LOAD_TO_ERR_BUFER { s-adr adr \ u    -- }
s-adr STR@ NIP  adr  C@ + 255 > IF 255  adr  C@ - ELSE  s-adr STR@ NIP  THEN -> u
 s-adr STR@ DROP  adr  1 + adr  C@ +  u CMOVE  
 s-adr STRFREE
 adr  C@   u + adr  C!
;


: TYPE_BUFER { bufer } 
bufer  1+ 
bufer  C@   DUP 0 > IF TYPE ELSE 2DROP ."  no info in buffer "  THEN 
;

: CLEAR_ERROR_BUFER 255 0 DO 0 work-info ERROR_PROG_BUFER   I + C!  LOOP ;
: TO_ERROR_PROG_BUFER    work-info ERROR_PROG_BUFER  LOAD_TO_ERR_BUFER ;  ( s-adr  ) \ ошибку в буфер
: TYPE_ERROR_PROG_BUFER  work-info ERROR_PROG_BUFER   TYPE_BUFER ;
\ отработка ошибки
: :ERROR  { s-adr-err n-err  } 
   s-adr-err  TO_ERROR_PROG_BUFER  n-err work-info  ERROR_PROG ! TYPE_ERROR_PROG_BUFER   -1 ;

: PAUSE-PROG 
  DUP 50 < IF  PAUSE  ELSE 50 / 0 DO work-info -upravl_thead- @  work_Execute = IF  50 PAUSE THEN    LOOP ELSE DROP THEN 
\ 1000
\ PAUSE
 ;


: WorkStep
work_Execute work-info -upravl_thead- !
\  work-info FREQ_BEGIN  F@ work-info  FREQ F!
\ WORK_BEGIN:

  BEGIN 
 \ ."  -1- "
  \ завершение работы, штатное/принудительное
   work-info  -upravl_thead- @ work_Execute = IF IF_END:    ELSE  0 THEN     
  WHILE
  \ рабочий цикл
  WORK:  
   \ вычисляем и запоминаем новый шаг (новую частоту?) 
     NewStep  \ work-info  FREQ  F!
  	work-info  STEP_PAUSE @ PAUSE-PROG  
REPEAT
  \ "последний" шаг
 work-info -upravl_thead- @ work_Execute = IF   WORK_END: WORK:      THEN     
;



: WorkCicle 
\ обнулили ошибки при запуске
CLEAR_ERROR_BUFER 
NoError work-info ERROR_PROG !
\ подготовка оборудования к запуску, инициализация приборов
work_Execute work-info -upravl_thead- !
  [']  BEGIN: CATCH  
  IF \ CR  ." word BEGIN problem  " CR error_begin_problem work-info  ERROR_PROG !
  " word BEGIN problem  " error_end_problem  :ERROR  DROP
  ELSE \ если инициализация прошла 
  CR ." word BEGIN  - ok  " CR
 	['] WorkStep CATCH  
	IF   " word WorkCicle problem "  error_cicle_problem  :ERROR  DROP
\	  error_cicle_problem work-info  ERROR_PROG ! 
	ELSE  CR ." word WorkCicle -ok  "  CR THEN
  THEN	
	['] END:  CATCH 	
	IF  \ CR ." word END problem. "  CR error_end_problem work-info  ERROR_PROG ! 
	" word END problem "  error_end_problem  :ERROR  DROP
	ELSE  CR ." word END - ok  " CR THEN	
\	out_file outFileClose
\	[']  FreePribor CATCH 0=  IF CR ." word FreePribor problem. " THEN	 
	work-info -upravl_thead- @ 0= \ если не было ошибок 
  IF  work_Stop  work-info -upravl_thead- ! \  то "нормальный" останов
  THEN
\ 0 TO -work_thead-
CR ." WorkCicle STOP" CR
work_Stop  work-info -upravl_thead- !
\ TERMINATE 
;

\ ----------дополнительные навороты---------

: --nast  NextWord work-info  file_nastr_buf 1 + SWAP DUP >R CMOVE R> work-info  file_nastr_buf C! ; IMMEDIATE \ файл настроек
: --metod NextWord work-info  file_metod_buf 1 + SWAP DUP >R CMOVE R> work-info  file_metod_buf C! ; IMMEDIATE \ файл метода
: --file  NextWord work-info  file_result_buf 1 + SWAP DUP >R CMOVE R> work-info  file_result_buf C! ; IMMEDIATE \ файл результата

: file_metod_reqiured work-info  file_metod_buf 1+ work-info  file_metod_buf C@  	INCLUDE-PROBE   ;
: file_nastr_reqiured work-info  file_nastr_buf 1+ work-info  file_nastr_buf C@  	INCLUDE-PROBE   ;

: INCLUDE-PROBE ." include file: " 2DUP TYPE CR INCLUDE-PROBE   ;   

\ : metod: ['] --metod EXECUTE file_metod_reqiured ; \ файл метода




\  S" metod/virtual.metod"  DUP file_metod_buf 4 - ! file_metod_buf SWAP CMOVE
  	
\ /metod/virtual.metod
:NONAME 
." -----------------------------------"  CR
." file_metod_buf: " work-info  file_metod_buf  1+ work-info  file_metod_buf  C@ TYPE CR 
." file_nastr_buf: " work-info  file_nastr_buf  1+ work-info  file_nastr_buf  C@ TYPE CR  
." STEP_TIME: "	     work-info	STEP_TIME	@ .  CR
." STEP_PAUSE: "     work-info	STEP_PAUSE	@ . CR
." COMMENT: "        work-info Coment_buffer work-info  Len_Coment_buffer  @ TYPE CR
CR
; TO see 


: r file_metod_reqiured  DROP file_nastr_reqiured DROP see ;
: e work-info  ERROR_PROG @ .  CR TYPE_ERROR_PROG_BUFER CR ;
: s WorkCicle  ;
: a 
\ S" -metod metod/virtual.metod -nast nast/test.nast" EVALUATE
\ S" --nast nast/prohod.nast" EVALUATE
\ S" --metod metod/vir_prohod.metod" EVALUATE  
 S" --nast nast/prohod.nast" EVALUATE
 S" --metod metod/prohod.metod" EVALUATE
\ S" --nast nast/kalibrovka.nast" EVALUATE
\ S" --metod metod/kalibr_2_kan_NRVD.metod" EVALUATE    
\ S"  --file test.sce" EVALUATE   
;
: w  
a
 file_metod_reqiured IF     " Error: load metod problem  "   error_metod_file :ERROR    ELSE 0 THEN IF EXIT THEN
 file_nastr_reqiured IF     " Error: load nastr problem  "   error_nastr_file :ERROR    ELSE 0 THEN IF EXIT THEN

\		WorkCicle 
;

: ST
 \ work_Execute work-info -upravl_thead- !
 ['] w TASK work-info work_thread !
 work-info work_thread @ START work-info  tid-thead  !
; 

\EOF
: zzz
 20 0  DO
  I SeeOne   
DEPTH . DUP . DUP 0 >  
  IF
   TYPE  CASE 1 OF @ . ENDOF 2 OF F@ F. ENDOF DROP ."  --- "  ENDCASE CR
  ELSE DROP  THEN
  LOOP
;

