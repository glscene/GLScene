rem @echo off
cls

set LANG=C

dxgettext -b source --delphi --useignorepo -r

pushd locale\ru\LC_MESSAGES
copy default.po default-backup.po
ren default.po default-old.po

echo Merging
msgmergedx default-old.po ..\..\..\default.po -o default.po
del default-old.po
del default-backup.po

copy default.po glsviewer.po
popd

del default.po

pause

