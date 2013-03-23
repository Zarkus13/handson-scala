set SCRIPT_DIR=%~dp0
set SBT=sbt-launch-0.12.2.jar
java -Dsbt.ivy.home=.\ivyrepo -Dsbt.boot.directory=.\sbtboot -Xmx512M -jar -Dfile.encoding=UTF8 -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m "%SCRIPT_DIR%\%SBT%" %*
