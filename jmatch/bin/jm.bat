@ECHO OFF
SET TOP=d:\docs\code\jmatch

SET args=
SET verbose=
SET hprof=
SET extra_cp=

:Loop
IF "%1"=="" GOTO Continue
IF "%1"=="-v" GOTO Verbose
IF "%1"=="-profile" GOTO Profile
IF "%1"=="-classpath" GOTO Classpath
IF "%1"=="-cp" GOTO Classpath
SET args=%args% %1
SHIFT
GOTO Loop
:Verbose
SET verbose=1
SHIFT
GOTO Loop
:Profile
SHIFT
SET hprof=-Xrunhprof:cpu=samples,file=%1,depth=10
SHIFT
GOTO Loop
:Classpath
SHIFT
SET extra_cp=%1
SHIFT
GOTO Loop
:Continue

IF DEFINED extra_cp SET extra_cp=";%extra_cp%"

SET classpath="%TOP%\classes;%TOP%\lib\jmatch-runtime.jar;%extra_cp%"
SET command=java %hprof% -classpath %classpath% %args%

IF DEFINED verbose echo %command%

CMD /C %command%
