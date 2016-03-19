REQUIRE STR@         ~ac/lib/str5.f
REQUIRE  F. lib\include\float2.f
REQUIRE  objLocalsSupport ~day/hype3/locals.f

\ запись данных в файл

: StoFile { sadr file \ --  } 
\  отправляю строку формата ~ас  в файл 
sadr STR@ file WRITE-FILE THROW
sadr STRFREE
;
: FtoFile { file } "  " file StoFile  >FNUM   file WRITE-FILE  THROW "  " file StoFile  ; \ флоат число в выходной файл
: DtoFile { file } "  " file StoFile   >NUM   file  WRITE-FILE THROW "  " file StoFile  ; \ целое число в выходной файл


: outFileCreate { sadr \ --  file } 
sadr STR@
 W/O CREATE-FILE THROW  
 sadr STRFREE
 ;
 
 : CRtoFile " {CRLF}" SWAP StoFile ;


: outFileClose  { file \ -- }
file FLUSH-FILE THROW
file  CLOSE-FILE THROW
;


: dBuV->V 20e F/ 10E FSWAP F** 1e6 F/ ; 
: V->dBuV 1e6 F* FLOG 20e F*  ;
: W->dBm  1e3 F* FLOG 10e F*  ;





\ формула для вычисления подаваемого сигнала 
0
1 FLOATS -- Data_Kalib  \ данные калибровки
1 FLOATS -- Sig_Gener   \ сигнал генератора
1 FLOATS -- Sign_Izmer  \ измеренный сигнал
1 FLOATS -- setsGenMaxStep 
1 FLOATS -- setsGenMax
1 FLOATS -- ResultGen
 CONSTANT nach_formula
 HERE DUP >R nach_formula DUP ALLOT ERASE VALUE Data_nach_formula
 : nach_formula_see  { x \ -- }
CR ." nach_formula_see :  " CR
x Data_Kalib  ." Data_Kalib = "  F@ F. CR \ данные калибровки
x Sig_Gener   ." Sig_Gener  = "  F@ F. CR \ сигнал генератора
x Sign_Izmer  ." Sign_Izmer = "  F@ F. CR \ измеренный сигнал
x setsGenMaxStep ." setsGenMaxStep = " F@ F. CR  
x setsGenMax ." setsGenMax = " F@  F. CR
x ResultGen ." ResultGen = " F@ F. CR
  ;

: Vyhod_Na_Amplitudu   \ 
|| R: nach_data  F: result D: sig ||
\ nach_formula_see
 \ nach_data ! 
\ ." -- " 
 \ nach_data @ nach_formula_see 
nach_data @ Data_Kalib F@
 nach_data @ Sign_Izmer F@
F- result F!   
\ result F@ F. SPACE
 result F@ 0e F< IF -1 ELSE 0 THEN sig !
\ result F@ F.	
\ ограничение на шаг
result F@ FABS nach_data @ setsGenMaxStep F@ F>  IF nach_data @ setsGenMaxStep  F@  result F! sig @ IF result F@ -1e F* result F! THEN THEN  
\ result F@ F.	
\ ." dela: " result F@  F. CR
 result F@  nach_data @  Sig_Gener F@ 
 F+ result F! 
\  result F@ F. SPACE 
	 result F@ nach_data @  setsGenMax F@ F> IF nach_data @  setsGenMax  F@ result F!  THEN  
 \ result F@ F. SPACE
	result F@  nach_data @ ResultGen F!
 \ nach_data @ nach_formula_see 
\ result F@  F. CR
 \ DEPTH . 
;