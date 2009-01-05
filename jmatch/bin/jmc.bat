@ECHO OFF
SET TOP=d:\docs\code\jmatch
SET ext=jmatch

SET args=
SET verbose=
SET extra_cp=
SET vmargs=
SET ext_cp=
SET extargs =

:Loop
IF "%1"=="" GOTO Continue
IF "%1"=="-v" GOTO Verbose
IF "%1"=="-classpath" GOTO Classpath
IF "%1"=="-profile" GOTO Profile
IF "%1"=="-ext" GOTO Ext
IF "%1"=="-j" GOTO J
SET args=%args% %1
SHIFT
GOTO Loop
:Verbose
SET verbose=1
SHIFT
GOTO Loop
:Classpath
SHIFT
SET extra_cp=%1
SHIFT
GOTO Loop
:Profile
SET vmargs=%vmargs% -Xrunhprof:cpu=samples
SHIFT
GOTO Loop
:Ext
SHIFT
SET ext=%1
SHIFT
GOTO Loop
:J
SHIFT
SET vmargs=%vmargs% '%1'
SHIFT
GOTO Loop
:Continue

IF DEFINED extra_cp SET extra_cp=";%extra_cp%"
IF DEFINED ext SET ext_cp=;%TOP%\lib\%ext%.jar
IF DEFINED ext SET extargs=-ext %ext%

SET classpath="%TOP%\classes;%TOP%\lib\polyglot.jar;%TOP%\lib\java_cup.jar;%ext_cp% %extra_cp%"
SET command=java %vmargs% -classpath %classpath% polyglot.main.Main %extargs% %args%

IF DEFINED verbose echo %command%

CMD /C %command%
