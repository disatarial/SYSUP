\  работа с  сокетами, на основе рсокет  ~ac
\  все лежит в отдельном словаре
 
 REQUIRE  F. ~disa\dopoln.f
 
\ REQUIRE  { lib\ext\locals.f
REQUIRE  HYPE ~day\hype3\hype3.f
CLASS test_port

\ : init ." init  " CR ;
: open  ." open: "   .  ."  " STYPE  CR ;
: close  ." close: "  CR ;
: write  ." write: "  STYPE  ."  " ;
: read  " read: test_port "      ;
;CLASS

(
0 VALUE smb100
: xxx
 socket_port NewObj TO smb100
  " 192.168.0.19" 5025 smb100 ^   open
   "  *IDN?" smb100 ^ write
\ " FREQ                20000000"        smb100 write
  smb100  ^ read STYPE
 smb100 ^ close
 smb100 ^ dispose
;
)