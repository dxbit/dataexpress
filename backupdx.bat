set src=d:\myprojects\dataexpress
set lib=d:\lazcomponents
set dest=%userprofile%\cloud@mail.ru\dataexpress\backdx-%date%.zip

md d:\backupdx
md d:\backupdx\src
md d:\backupdx\src\_test
md d:\backupdx\src\_test\languages
md d:\backupdx\src\_test\languages\en
md d:\backupdx\src\_test\languages\ru
md d:\backupdx\src\_test\languages\ru\help
md d:\backupdx\components
md d:\backupdx\components\jvdesign
md d:\backupdx\components\dbctrlsex
md d:\backupdx\images

copy "%src%\*" d:\backupdx\src\
copy "%src%\_test\languages\en\*" d:\backupdx\src\_test\languages\en\
copy "%src%\_test\languages\ru\*" d:\backupdx\src\_test\languages\ru\
copy "%src%\_test\languages\ru\help\*" d:\backupdx\src\_test\languages\ru\help\
copy "%lib%\jvdesign\*" d:\backupdx\components\jvdesign\
copy "%lib%\dbctrlsex\*" d:\backupdx\components\dbctrlsex\
copy "%src%\images\*" d:\backupdx\images

"c:\program files\7-zip\7z.exe" a -r "%dest%" d:\backupdx\*

rd /s /q d:\backupdx