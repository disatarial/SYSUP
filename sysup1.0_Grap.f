 REQUIRE CAPI: lib/win/api-call/capi.f


\ WIN ONLY
 REQUIRE  _SYSTIME ~pi/lib/systime.f 
\ будильник
WINAPI: Beep Kernel32.dll

REQUIRE WorkCicle sysup1.0.f

\ пользовательский интерфейс

CVAPI: gtk_init					libgtk-3-0.dll
CVAPI: gtk_widget_destroy			libgtk-3-0.dll
CVAPI: gtk_widget_hide				libgtk-3-0.dll
CVAPI: gtk_widget_show				libgtk-3-0.dll
CVAPI: gtk_widget_set_sensitive			libgtk-3-0.dll

CVAPI: gtk_builder_new				libgtk-3-0.dll
CVAPI: gtk_builder_add_from_file		libgtk-3-0.dll
CVAPI: gtk_builder_get_object			libgtk-3-0.dll

CVAPI: gtk_main					libgtk-3-0.dll
CVAPI: gtk_main_quit				libgtk-3-0.dll
CVAPI: gtk_toggle_button_set_active		libgtk-3-0.dll
CVAPI: gtk_switch_get_active			libgtk-3-0.dll

CVAPI: g_signal_connect_data			libgobject-2.0-0.dll
CVAPI: g_object_get_data			libgobject-2.0-0.dll
CVAPI: g_object_unref				libgobject-2.0-0.dll
CVAPI: g_timeout_add				libglib-2.0-0.dll

CVAPI: gtk_entry_set_text			libgtk-3-0.dll
CVAPI: gtk_entry_get_text			libgtk-3-0.dll
CVAPI: gtk_entry_get_text_length		libgtk-3-0.dll
CVAPI: gtk_switch_set_active			libgtk-3-0.dll
CVAPI: gtk_text_view_get_buffer			libgtk-3-0.dll
CVAPI: gtk_text_buffer_set_text			libgtk-3-0.dll

CVAPI: gtk_file_chooser_set_filename		libgtk-3-0.dll
CVAPI: gtk_file_chooser_get_filename		libgtk-3-0.dll
CVAPI: gtk_file_chooser_get_current_folder	libgtk-3-0.dll
CVAPI: gtk_file_chooser_select_filename		libgtk-3-0.dll
CVAPI: gtk_file_chooser_set_action		libgtk-3-0.dll
\ CVAPI: g_locale_from_utf8			libglib-2.0-0.dll
\ CVAPI: g_convert				libglib-2.0-0.dll
CVAPI: gtk_text_buffer_get_end_iter		libgtk-3-0.dll
CVAPI: gtk_text_buffer_get_start_iter		libgtk-3-0.dll
CVAPI: gtk_text_buffer_get_text			libgtk-3-0.dll
CVAPI: gtk_text_buffer_insert_at_cursor		libgtk-3-0.dll
CVAPI: gtk_text_iter_get_line			libgtk-3-0.dll

CVAPI: gtk_list_store_append			libgtk-3-0.dll
CVAPI: gtk_list_store_set			libgtk-3-0.dll
CVAPI: gtk_list_store_insert			libgtk-3-0.dll
CVAPI: gtk_list_store_move_after		libgtk-3-0.dll
CVAPI: gtk_list_store_move_before 		libgtk-3-0.dll
CVAPI: gtk_list_store_remove			libgtk-3-0.dll 
CVAPI: gtk_list_store_clear			libgtk-3-0.dll 

CVAPI: gtk_tree_view_get_model			libgtk-3-0.dll
CVAPI: gtk_tree_model_get_iter_from_string	libgtk-3-0.dll
CVAPI: gtk_tree_model_get_iter			libgtk-3-0.dll
CVAPI: gtk_tree_view_get_cursor			libgtk-3-0.dll 
CVAPI: gtk_tree_view_get_dest_row_at_pos	libgtk-3-0.dll 
CVAPI: gtk_tree_view_get_selection		libgtk-3-0.dll 
CVAPI: gtk_tree_selection_get_user_data		libgtk-3-0.dll 
CVAPI: gtk_tree_path_to_string			libgtk-3-0.dll 
CVAPI: gtk_tree_model_get_string_from_iter	libgtk-3-0.dll 

CVAPI: gtk_label_set_text			libgtk-3-0.dll 
CVAPI: gtk_file_chooser_set_current_folder	libgtk-3-0.dll 

0
CELL -- domain
CELL -- code \ 
CELL -- message
CONSTANT GError
HERE DUP >R GError  DUP ALLOT ERASE VALUE GtkError

\ оболочка для пользователя
VARIABLE pargv
VARIABLE pargs
VARIABLE window
VARIABLE builder
\ кнопки
VARIABLE button_load_metod \ загрулить файл метода
VARIABLE button_load_data \ загрулить файл настроек
VARIABLE button_save_data \ _Coment_buffer_сохранить  файл  настроек
VARIABLE button_start \ начать работу
VARIABLE button_stop   \ остановить  работу
VARIABLE button_pause  \ пауза в работе
\ выбор файлов 
VARIABLE filechooserbutton_file_nastr  \ настройки
VARIABLE filechooserbutton_metod  \ методика работы


\ переменные
VARIABLE entry_result    \ имя файла результата
VARIABLE entry_time_on \ время воздействия
VARIABLE entry_time_stop \ пауза между воздействиями

\ данные для коментария   и ссылка на его буфер
\ настройки
VARIABLE textview_comment
VARIABLE textview_comment_bufer
\ метод
VARIABLE textview_comment_metod
VARIABLE textview_comment_metod_bufer
\ описание текущее
VARIABLE textview_opisenie
VARIABLE textview_opisenie_bufer


\ окно результата
VARIABLE textview_result
VARIABLE textview_result_bufer 

VARIABLE treeview_data \ список переменных (..частота, шаг....)
VARIABLE treeview_file
VARIABLE liststore_text
VARIABLE liststore_file
VARIABLE tree_view_selection

VARIABLE button_norma
VARIABLE button_error
\ 0 VALUE 
VARIABLE dialog
VARIABLE dialog_entry
VARIABLE dialog_label
VARIABLE dialog_filechooserbutton
VARIABLE num_data \ номер последних/текущих данных в списке  gtk_tree... только для передачи внутри граф. интерфейса

\ итератор для текстового буфера... размер неизвестен
 0 , HERE  64 ALLOT  VALUE iter_n
 0 , HERE  64 ALLOT  VALUE iter_k
 0 , HERE  64 ALLOT  VALUE iter_store_text
 0 , HERE  64 ALLOT  VALUE iter_store_file

VARIABLE error  \ сюда скидывать номер ошибки 


: act_on_window_destroy    CR ." Exit " 0 gtk_main_quit DROP CR    ;
: act_timer_ticket  { \ s }
  Str_Data_buffer STR>S -> s
  s STR@ NIP IF  \ ."  >"  s STR@ TYPE ." <   " \ TYPE
\  S"  + " 
 s STR@   SWAP  textview_result_bufer @ 3 gtk_text_buffer_insert_at_cursor DROP 
  THEN
  s STRFREE
\ вывод ошибки 
work-info  ERROR_PROG @  NoError <> 
IF 
	work-info ERROR_PROG_BUFER C@   work-info ERROR_PROG_BUFER 1+ textview_result_bufer @ 3 gtk_text_buffer_insert_at_cursor DROP  
	 " {CRLF}" DUP  >R STR@ SWAP textview_result_bufer @ 3 gtk_text_buffer_insert_at_cursor DROP R> STRFREE
THEN
;
: Refresh_list { \ adr u I flag  s  data-adr }
 liststore_text  @ 1 gtk_list_store_clear			DROP
0 -> I
 BEGIN
   I SeeOne    
 DUP  0 >  
 WHILE
      -> u -> adr   -> flag      -> data-adr  
	I iter_store_text  liststore_text  @  3 gtk_list_store_insert DROP
   	\ -1  adr u STR>S DUP >R STR@ DROP 1 iter_store_text liststore_text  @ 5 gtk_list_store_set DROP R> STRFREE
   flag 1 = IF data-adr  @ >NUM  STR>S -> s THEN
   flag 2 = IF data-adr F@ >FNUM STR>S -> s THEN
   flag 2 > flag 5 < AND IF data-adr 1+ data-adr C@ STR>S -> s THEN
   flag 4 > flag 1 < OR IF "  закончено" -> s THEN
	 -1 adr u STR>S DUP >R STR@ DROP 0  s DUP >R STR@ DROP 1  I 2 iter_store_text liststore_text  @ 9 gtk_list_store_set DROP R> STRFREE  R> STRFREE  
   I 1 + -> I
REPEAT
DROP
;

: act_button_load_metod_click 
   \ грузим имя метода
   filechooserbutton_metod       @ 1 gtk_file_chooser_get_filename DUP IF  ASCIIZ> STR>S work-info  file_metod_buf  LOAD_TO_BUFER 0 ELSE DROP "  Erкor: metod not found "   error_metod_file :ERROR   THEN  IF EXIT THEN
\ грузим метод
	file_metod_reqiured    
  IF     " Error: load metod problem  "   error_metod_file :ERROR    ELSE 0 THEN IF EXIT THEN
 work-info STEP_PAUSE @ DS>F >FNUM  STR>S >R R@ STR@ DROP entry_time_stop  @ 2 gtk_entry_set_text DROP R> STRFREE
 work-info STEP_TIME  @ DS>F >FNUM  STR>S >R R@ STR@ DROP entry_time_on    @ 2 gtk_entry_set_text DROP R> STRFREE
see CR
 work-info  Len_Coment_metod_buffer @ work-info Coment_metod_buffer    textview_comment_metod_bufer @ 3 gtk_text_buffer_set_text DROP
Refresh_list
;

:  act_button_load_data_click  
 0 work-info  ERROR_PROG  !	\ сбраcываем ошибки
\ грузим имя настроек
   filechooserbutton_file_nastr  @ 1 gtk_file_chooser_get_filename DUP IF  ASCIIZ> STR>S work-info  file_nastr_buf  LOAD_TO_BUFER 0 ELSE DROP "  Erкor: nastr not found "   error_metod_file :ERROR   THEN  IF EXIT THEN
\ грузим настройки
	file_nastr_reqiured 
  IF     " Error: load nastr problem  "   error_metod_file :ERROR    ELSE 0 THEN IF EXIT THEN
see CR
 work-info STEP_PAUSE @ DS>F >FNUM  STR>S >R R@ STR@ DROP entry_time_stop  @ 2 gtk_entry_set_text DROP R> STRFREE
 work-info STEP_TIME  @ DS>F >FNUM  STR>S >R R@ STR@ DROP entry_time_on    @ 2 gtk_entry_set_text DROP R> STRFREE

 \ комментарий 
 work-info  Len_Coment_buffer @ work-info Coment_buffer    textview_comment_bufer @ 3 gtk_text_buffer_set_text DROP
 work-info file_result_buf   1 + entry_result @ 2 gtk_entry_set_text DROP  
 Refresh_list
 ;

: load_data_out_form 
   	0 work-info  ERROR_PROG  !	\ сбраcываем ошибки
\ время между шагами
    entry_time_stop  @ 1 gtk_entry_get_text_length   DUP 0 > 
    IF 
	entry_time_stop  @ 1 gtk_entry_get_text    SWAP
	STR>FLOAT IF F>D D>S  work-info  STEP_PAUSE ! 0 ELSE "  Eroor: entry_time_stop"   error_face :ERROR  THEN IF EXIT THEN
   0 ELSE DROP "  Eroor: entry_time_stop empty "   error_face :ERROR   THEN  IF EXIT THEN
\ время воздействия
    entry_time_on    @ 1 gtk_entry_get_text_length   DUP 0 > 
    IF 
	entry_time_on  @ 1 gtk_entry_get_text    SWAP
	STR>FLOAT IF F>D D>S  work-info  STEP_TIME  ! 0 ELSE "  Eroor: entry_time_on  "   error_face :ERROR  THEN IF EXIT THEN
   0 ELSE DROP "  Eroor: entry_time_on  empty "   error_face :ERROR   THEN  IF EXIT THEN
\ имя результата

    entry_result @ 1 gtk_entry_get_text_length   DUP 0 > 
    IF 
	entry_result  @ 1 gtk_entry_get_text    SWAP
	STR>S work-info file_result_buf LOAD_TO_BUFER	
   0 ELSE DROP "  Error: entry_result  empty "   error_face :ERROR   THEN  IF EXIT THEN

\ work-info file_result_buf   1 + entry_result @ 

\ синхронизация буфера настроек
	iter_n  textview_comment_bufer  @ 2 gtk_text_buffer_get_start_iter DROP
	iter_k   textview_comment_bufer  @ 2 gtk_text_buffer_get_end_iter DROP
	0 iter_k   iter_n    textview_comment_bufer  @ 4 gtk_text_buffer_get_text  ASCIIZ>   
	DUP work-info  Len_Coment_buffer ! \ запомнить длинну
	work-info Coment_buffer SWAP CMOVE \ процесс перемещения 
\ синхронизация буфера метода
	iter_n  textview_comment_metod_bufer  @ 2 gtk_text_buffer_get_start_iter DROP
	iter_k   textview_comment_metod_bufer  @ 2 gtk_text_buffer_get_end_iter DROP
	0 iter_k   iter_n    textview_comment_metod_bufer  @ 4 gtk_text_buffer_get_text  ASCIIZ>   
	DUP work-info     Len_Coment_metod_buffer ! \ запомнить длинну
	work-info Coment_metod_buffer SWAP CMOVE \ процесс перемещения 

\ синхронизация буфера описания
	iter_n  textview_opisenie_bufer  @ 2 gtk_text_buffer_get_start_iter DROP
	iter_k   textview_opisenie_bufer  @ 2 gtk_text_buffer_get_end_iter DROP
	0 iter_k   iter_n    textview_opisenie_bufer  @ 4 gtk_text_buffer_get_text  ASCIIZ>   
	DUP work-info     Len_opisanie_buffer ! \ запомнить длинну
	work-info Coment_opisanie_buffer SWAP CMOVE \ процесс перемещения 



;
  
    
: save_nastr_file { name \ save_file }
 0 work-info  ERROR_PROG  !	\ сбраcываем ошибки
 S" " SWAP textview_result_bufer @  3 gtk_text_buffer_set_text DROP \ обнуляем окно
load_data_out_form
work-info  ERROR_PROG @ 0= \ ошибок не было
IF
	name outFileCreate ->  save_file
	" COMMENT( " save_file StoFile     
\	iter_n  textview_comment_bufer  @ 2 gtk_text_buffer_get_start_iter DROP
\	iter_k   textview_comment_bufer  @ 2 gtk_text_buffer_get_end_iter DROP
\	0 iter_k   iter_n    textview_comment_bufer  @ 4 gtk_text_buffer_get_text  ASCIIZ>   
	work-info Coment_buffer  work-info  Len_Coment_buffer @  \ данные по буферу
	STR>S  save_file StoFile   
	" )" save_file StoFile save_file CRtoFile
	"  {''} " save_file StoFile work-info   file_result_buf 1+ work-info   file_result_buf  C@    	STR>S save_file StoFile  " {''} work-info file_result_buf	LOAD_TO_BUFER " save_file StoFile  save_file CRtoFile
	save_file SAVE_NASTR_DATA 	
\	see
	save_file outFileClose
THEN
;

 : act_but_start_click 
 work_Execute work-info -upravl_thead- @ <> \  и запущенность программ, дабы не запускать 2 копии
IF
	0 work-info  ERROR_PROG  !	\ сбраcываем ошибки
	S" " SWAP textview_result_bufer @  3 gtk_text_buffer_set_text DROP \ обнуляем окно
	load_data_out_form		\ подгружаем из формы данные
	work-info  ERROR_PROG @ 0=  \ проверяем на ошибки формы 
	IF	
		['] WorkCicle  TASK work-info work_thread !
		work-info work_thread @ START 
	THEN
 THEN

 ;

:NONAME act_on_window_destroy  BYE 
	window @ ;  1 CELLS  CALLBACK: on_window_destroy 
:NONAME  act_but_start_click 
	window @ ;  1 CELLS CALLBACK: but_start_click 
:NONAME  
 	0 work-info  ERROR_PROG  !	\ сбраcываем ошибки
	work_Stop work-info -upravl_thead- ! 	
	 window @ ;  1 CELLS CALLBACK: but_stop_click 
	 
:NONAME 
  work-info -upravl_thead- @  work_Execute =  \ работает?
  IF 
	 work-info tid-thead   @ SUSPEND  work_Pause   work-info -upravl_thead- !
  ELSE \ в паузе
	work_Execute  work-info -upravl_thead- !   work-info tid-thead  @ RESUME  
  \ -upravl_thead- work_Pause = IF -work_thead-   RESUME  work_Execute  TO -upravl_thead-   ELSE  -work_thead-  SUSPEND  work_Pause  TO -upravl_thead- THEN 
  THEN	window @ ;  1 CELLS  CALLBACK: but_pause_click 
  
:NONAME   filechooserbutton_file_nastr @ 1 gtk_file_chooser_get_filename DUP 0<> IF ASCIIZ> STR>S    save_nastr_file    CR   ELSE DROP THEN 
	window @ ;  1 CELLS 	CALLBACK: button_save_data_click 


:NONAME  act_button_load_data_click  
	window @ ;  1 CELLS  CALLBACK: button_load_data_click 

:NONAME act_timer_ticket 
	window @  ;  1 CELLS CALLBACK:  timer_ticket 

:NONAME  act_button_load_metod_click   
	window @ ;  1 CELLS  CALLBACK: button_load_metod_click  


\ -----кнопки принять /отклонить  изменение списка -------------
:NONAME  ."  -1- " dialog  @ 1 gtk_widget_hide DROP  dialog  @ ; 1 CELLS CALLBACK:  button_error_click   
:NONAME  ."  -2- "  { \ flag  adr_str  adr u }
 num_data @ SeeOne \ адрес данных,тип данных,  название данных   длинна названия
  2DROP -> flag  -> adr_str  	
\ выясняем какой тип  данных 
flag  1 =  flag  2 =  OR \ действительное или  целое
	IF  dialog_entry  @ 1 gtk_entry_get_text_length   DUP 0 > 
		IF 
			dialog_entry  @ 1 gtk_entry_get_text    -> adr  -> u \ adr u 	
			adr u  STR>FLOAT   
			IF
				flag  2 =  IF	 adr_str F!  ELSE  F>D D>S  adr_str ! THEN \ действительное или целое
			ELSE -1 -> flag THEN 
		ELSE -1 -> flag
		THEN
	THEN
flag  3 =  \ строка
	IF DROP
	THEN
flag  4 =  \ файл
	IF
		dialog_filechooserbutton @ 1 gtk_file_chooser_get_filename \  взяли имя
		DUP 
		IF ASCIIZ> 2DUP ?STR_FILE			\ проверили наличие файла
			IF  STR>S adr_str LOAD_TO_BUFER		\
			ELSE  -1 -> flag 
			THEN  
		ELSE -1 -> flag 
		THEN
	THEN

 flag -1 <> IF dialog  @ 1 gtk_widget_hide DROP Refresh_list THEN 
 dialog  @ ; 1 CELLS CALLBACK:  button_norma_click  

\ почему-то глючит кнопка выбора файлов
: Dialog-activate/deactivate  ( 1 / 0 )
IF	
dialog_entry @   1 gtk_widget_hide DROP
\	dialog_filechooserbutton @  1 gtk_widget_show DROP
ELSE 
	dialog_entry @  1 gtk_widget_show DROP
\	dialog_filechooserbutton @  1 gtk_widget_hide DROP
THEN 
;
\ ---- нажатие на список -------------------------
:NONAME   {  column path tree_view \ model  flag  adr u }
\ выделенная строчка 
  tree_view  1 gtk_tree_view_get_model   -> model 
  path iter_store_text model 3   gtk_tree_model_get_iter DROP \ (model, &iter, path_string)
  iter_store_text model 2 gtk_tree_model_get_string_from_iter    ASCIIZ> STR>S  \ STYPE ."  "
 S>FLOAT
\ Временно 
 IF F>D D>S  num_data ! \ запомнить номер строчки с данными
	num_data @ SeeOne \ адрес данных,тип данных,  название данных   длинна названия
	-> u -> adr  -> flag
	adr u STR>S DUP >R  STR@ DROP dialog_label @ 2 gtk_label_set_text DROP R> STRFREE
	flag 1 = IF @  >NUM  STR>S  0 Dialog-activate/deactivate   THEN	 \ целое
	flag 2 = IF F@ >FNUM  STR>S 0 Dialog-activate/deactivate  THEN      \ действительное
	flag 3 = IF DUP 1+ SWAP C@ STR>S 0 Dialog-activate/deactivate  THEN \ просто текст
	flag 4 = IF  -1 Dialog-activate/deactivate			\ имя файла
		DUP 1+ SWAP C@ STR>S 
		 DUP STR@ DROP dialog_filechooserbutton @ 2 gtk_file_chooser_set_filename DROP    
		THEN
	flag 4 > flag 1 < OR IF " ОШИБКА " THEN
	DUP >R  STR@ DROP dialog_entry    @ 2 gtk_entry_set_text DROP R> STRFREE
  dialog  @ 1 gtk_widget_show DROP
THEN
column path tree_view  	window @  ;  3 CELLS CALLBACK:  treeview_data_click


: work_windows
 pargv pargs  2  gtk_init  DROP \ 2DROP 
  0 gtk_builder_new   builder !
  error  " sysup_07_2015.glade"  >R R@ STR@  DROP  builder @ 3 gtk_builder_add_from_file DROP   R> STRFREE \ 2DROP 
  " window1"  >R R@ STR@  DROP builder @ 2 gtk_builder_get_object window !  R> STRFREE \ 2DROP
 window @  1 gtk_widget_show DROP \ DROP
   \ ДЕЙСТВО ЗАКРЫТИЕ ПРОГРАММЫ
 " destroy"  >R 0 0 0  ['] on_window_destroy  R@ STR@ DROP window @ 6 g_signal_connect_data   R> STRFREE  DROP \ 2DROP 2DROP 2DROP
   \ кнопка старт
 " button_start" >R  R@ STR@  DROP builder @ 2 gtk_builder_get_object button_start !    R> STRFREE \ 2DROP
  " clicked"  >R 0 0 0  ['] but_start_click   R@ STR@ DROP button_start @ 6 g_signal_connect_data   R> STRFREE  DROP \ 2DROP 2DROP 2DROP
   \ кнопка стоп
 " button_stop" >R  R@ STR@  DROP builder @  2 gtk_builder_get_object button_stop !    R> STRFREE \ 2DROP
 " clicked"  >R 0 0 0  ['] but_stop_click   R@ STR@ DROP button_stop @ 6 g_signal_connect_data   R> STRFREE  DROP \ 2DROP 2DROP 2DROP
   \ кнопка пауза
 " button_pause" >R  R@ STR@  DROP builder @  2 gtk_builder_get_object button_pause !    R> STRFREE \ 2DROP
 " clicked"  >R 0 0 0  ['] but_pause_click   R@ STR@ DROP button_pause @ 6 g_signal_connect_data   R> STRFREE  DROP \ 2DROP 2DROP 2DROP
   \ кнопка загрузить настройки
  " button_load_data" >R  R@ STR@  DROP builder @  2 gtk_builder_get_object button_load_data !    R> STRFREE \ 2DROP
 " clicked"  >R 0 0 0  ['] button_load_data_click   R@ STR@ DROP button_load_data @ 6 g_signal_connect_data   R> STRFREE  DROP \ 2DROP 2DROP 2DROP
  \ кнопка сохранить настройки
  " button_save_data" >R  R@ STR@  DROP builder @  2 gtk_builder_get_object button_save_data !    R> STRFREE \ 2DROP
 " clicked"  >R 0 0 0  ['] button_save_data_click   R@ STR@ DROP button_save_data @ 6 g_signal_connect_data   R> STRFREE  DROP \ 2DROP 2DROP 2DROP
\  кнопка загрузить метод
  " button_load_metod" >R  R@ STR@  DROP builder @  2 gtk_builder_get_object button_load_metod !    R> STRFREE \ 2DROP
 " clicked"  >R 0 0 0  ['] button_load_metod_click   R@ STR@ DROP button_load_metod @ 6 g_signal_connect_data   R> STRFREE  DROP \ 2DROP 2DROP 2DROP

 " filechooserbutton_metod"     >R  R@ STR@  DROP builder @  2 gtk_builder_get_object filechooserbutton_metod !    R> STRFREE \ 2DROP
 " filechooserbutton_file_nastr" >R  R@ STR@  DROP builder @  2 gtk_builder_get_object filechooserbutton_file_nastr !    R> STRFREE \ 2DROP
 \ " file-set"  >R 0 0 0  ['] button_load_data_click   R@ STR@ DROP filechooserbutton_file_nastr @ 6 g_signal_connect_data   R> STRFREE  DROP \ 2DROP 2DROP 2DROP
 \ коментарий наксктоек
  " textview_comment" >R  R@ STR@  DROP builder @  2 gtk_builder_get_object textview_comment !    R> STRFREE 
  textview_comment @ 1 gtk_text_view_get_buffer textview_comment_bufer ! 
\ коментарий метода
  " textview_comment_metod" >R  R@ STR@  DROP builder @  2 gtk_builder_get_object textview_comment_metod !    R> STRFREE 
  textview_comment_metod @ 1 gtk_text_view_get_buffer textview_comment_metod_bufer ! 
\ коментарий текущей работы
  " textview_opisenie" >R  R@ STR@  DROP builder @  2 gtk_builder_get_object textview_opisenie !    R> STRFREE 
  textview_opisenie @ 1 gtk_text_view_get_buffer textview_opisenie_bufer ! 


" entry_time_on"   >R  R@ STR@  DROP builder @ 2 gtk_builder_get_object entry_time_on !     R> STRFREE 
" entry_time_stop" >R  R@ STR@  DROP builder @ 2 gtk_builder_get_object entry_time_stop !   R> STRFREE 
\ куда сохранятся
  " entry_result" >R  R@ STR@  DROP builder @ 2 gtk_builder_get_object entry_result !    R> STRFREE \ 2DROP
 
  " textview_result" >R  R@ STR@  DROP builder @  2 gtk_builder_get_object  textview_result !    R> STRFREE 
   textview_result @ 1 gtk_text_view_get_buffer  textview_result_bufer ! 

    " treeview_data" >R  R@ STR@  DROP builder @ 2 gtk_builder_get_object  treeview_data  !    R> STRFREE
    " treeview_file" >R  R@ STR@  DROP builder @ 2 gtk_builder_get_object  treeview_file  !    R> STRFREE

    " liststore_text" >R R@ STR@ DROP builder @ 2 gtk_builder_get_object liststore_text ! R> STRFREE  
    " liststore_file" >R R@ STR@ DROP builder @ 2 gtk_builder_get_object liststore_file ! R> STRFREE  
  
  iter_store_text liststore_text  @ 2 gtk_list_store_append DROP
  " row-activated"  >R 0 0 0  ['] treeview_data_click   R@ STR@ DROP treeview_data @ 6 g_signal_connect_data   R> STRFREE  DROP \ 2DROP 2DROP 2DROP

\ диалог ввода данных
 " dialog"  DUP >R STR@  DROP builder @ 2 gtk_builder_get_object  dialog !  R> STRFREE \ 2DROP
 " button_norma" >R  R@ STR@  DROP builder @  2 gtk_builder_get_object button_norma !    R> STRFREE \ 2DROP
 " clicked"  >R 0 0 0  ['] button_norma_click   R@ STR@ DROP button_norma @ 6 g_signal_connect_data   R> STRFREE  DROP \ 2DROP 2DROP 2DROP
 " button_error" >R  R@ STR@  DROP builder @  2 gtk_builder_get_object button_error !    R> STRFREE \ 2DROP
 " clicked"  >R 0 0 0  ['] button_error_click   R@ STR@ DROP button_error @ 6 g_signal_connect_data   R> STRFREE  DROP \ 2DROP 2DROP 2DROP
 " dialog_entry" >R  R@ STR@  DROP builder @ 2 gtk_builder_get_object dialog_entry !    R> STRFREE \ 2DROP
 
  " dialog_filechooserbutton"     >R  R@ STR@  DROP builder @  2 gtk_builder_get_object dialog_filechooserbutton !    R> STRFREE \ 2DROP
 \ " file-activated"  >R 0 0 0  ['] dialog_filechooserbutton_click   R@ STR@ DROP dialog_filechooserbutton @ 6 g_signal_connect_data  R> STRFREE   DROP \ 2DROP 2DROP 2DROP
 

 
 " dialog_label" >R  R@ STR@  DROP builder @ 2 gtk_builder_get_object dialog_label !    R> STRFREE \ изменяемая надпись
 

\   dialog @  1 gtk_widget_show DROP \ DROP
\ диалог ввода файлов

(
/* Добавляем новую строку в модель */
      gtk_list_store_append list_store, &iter;
      gtk_list_store_set list_store, &iter,
                          COLUMN_STRING, some_data,
                          COLUMN_INT, i,
                          COLUMN_BOOLEAN,  FALSE,
                          -1
)
 
 0 ['] timer_ticket  1500 3 g_timeout_add DROP
\ button_start @ gtk_widget_grab_default  DROP

  0 gtk_main  DROP 
;


 : StoFile { s-adr   file }
\  отправляю строку формата ~ас  в файл 
   s-adr STR@ \  ." -> " TYPE ." <-"  \ SWAP textview_result_bufer @ 3 gtk_text_buffer_set_text DROP
 To_Data_buffer  
\        SWAP  textview_comment_bufer @ 3 gtk_text_buffer_set_text DROP
	s-adr   file  StoFile  ;

 : FtoFile  { file }  "  " file StoFile  >FNUM  STR>S file StoFile   "  " file StoFile ;  \ флоат число в выходной файл
 : DtoFile  { file }  "  " file StoFile  >NUM   STR>S file StoFile   "  " file StoFile  ;  \ целое число в выходной файл
 : CRtoFile " {CRLF}" SWAP StoFile ;


0 VALUE runthread
: start
 ['] work_windows TASK TO runthread
  runthread START
; 
 
start

