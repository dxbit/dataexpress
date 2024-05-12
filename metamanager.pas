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

