{-------------------------------------------------------------------------------

    Copyright 2015-2024 Pavel Duborkin ( mydataexpress@mail.ru )

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

-------------------------------------------------------------------------------}

unit MetaManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMetaManager }

  TMetaManager = class
  public
    procedure SaveToCache;
  end;

implementation

uses
  dbengine, dxmains, formmanager, reportmanager, scriptmanager, imagemanager,
  Zipper, LazFileUtils;

{ TMetaManager }

procedure TMetaManager.SaveToCache;
var
  i: Integer;
  TmpDir: String;
begin
 { TmpDir := GetTempDir + ExtractFileNameOnly(DBase.Database) + '.tmp';
  if not CreateDir(TmpDir) then
  begin
    ShowMessage('Can not create temporary directory ' + TmpDir);
    Exit;
  end;

  DXMain.SaveToDir(TmpDir);
  FormMan.SaveToDir(TmpDir);
  ReportMan.SaveToDir(TmpDir);
  ImageMan.SaveToDir(TmpDir);  }
end;

end.

