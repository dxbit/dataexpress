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

unit UpdateManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, DB, SQLDb, FileUtil, dbengine, SAX,
  SAXBaseReader, ComCtrls, strconsts;

type
  PUpdateFileInfo = ^TUpdateFileInfo;
  TUpdateFileInfo = record
    FileName: String;
    Time, Size: LongInt;
  end;

  { TUpdateFileList }

  TUpdateFileList = class(TList)
  private
    function GetFiles(Index: Integer): TUpdateFileInfo;
  public
    function AddFile(const FileName: String; FileTime, ASize: LongInt): Integer;
    procedure Clear; override;
    procedure CopyFrom(L: TUpdateFileList);
    property Files[Index: Integer]: TUpdateFileInfo read GetFiles; default;
  end;

  { TUpdateMan }

  TUpdateMan = class
  private
    FDB: TDBEngine;
    FFiles: TUpdateFileList;
    FUserMsg: String;
    FVersion: String;
    procedure CreateUpdateDatabase;
    function FilesToXml: String;
  public
    constructor Create;
    destructor Destroy; override;
    function ConnectOrCreate: Boolean;
    function Connect: Boolean;
    procedure LoadFromDb;
    procedure SaveToDb;
    procedure UploadFiles(const BaseDir: String; AFileList: TStrings);
    procedure DownloadFiles;
    function CheckUpdates: Boolean;
    property DB: TDBEngine read FDB;
    property Version: String read FVersion write FVersion;
    property UserMsg: String read FUserMsg write FUserMsg;
    property Files: TUpdateFileList read FFiles;
  end;

  { TFileDataReader }

  TFileDataReader = class(TSAXBaseReader)
  public
    UMan: TUpdateMan;
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
  end;

implementation

uses
  zipper, myzipper, apputils, appsettings;

{ TUpdateFileList }

function TUpdateFileList.GetFiles(Index: Integer): TUpdateFileInfo;
begin
  Result := PUpdateFileInfo(Items[Index])^;
end;

function TUpdateFileList.AddFile(const FileName: String; FileTime,
  ASize: LongInt): Integer;
var
  pUfi: PUpdateFileInfo;
begin
  New(pUfi);
  pUfi^.FileName := FileName;
  pUfi^.Time := FileTime;
  pUfi^.Size := ASize;
  Result := Add(pUfi);
end;

procedure TUpdateFileList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(PUpdateFileInfo(Items[i]));
  inherited Clear;
end;

procedure TUpdateFileList.CopyFrom(L: TUpdateFileList);
var
  i: Integer;
  F: TUpdateFileInfo;
begin
  Clear;
  for i := 0 to L.Count - 1 do
  begin
    F := L[i];
    AddFile(F.FileName, F.Time, F.Size);
  end;
end;

{ TFileDataReader }

procedure TFileDataReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  Path: String;
  Tm, Sz: Integer;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  if LocalName = 'file' then
  begin
    Path := XmlToStr(GetStr(Atts, 'path'));
    Tm := GetInt(Atts, 'time');
    Sz := GetInt(Atts, 'size');
    UMan.Files.AddFile(Path, Tm, Sz);
  end
  else if LocalName = 'data' then
  begin
    UMan.Version := GetStr(Atts, 'version');
    UMan.UserMsg := XmlToStr(GetStr(Atts, 'msg'));
  end;
end;

{ TUpdateMan }

procedure TUpdateMan.CreateUpdateDatabase;
begin
  FDB.Execute('CREATE TABLE DX_VERSION (VERSION INTEGER);' +
    'CREATE TABLE DX_FILES (ID INTEGER,' +
    'DATA BLOB SUB_TYPE 1 SEGMENT SIZE 80, ' +
    'FILES BLOB SUB_TYPE 0 SEGMENT SIZE 512);' +
    'COMMIT;' +
    'INSERT INTO DX_VERSION (VERSION) VALUES (1);');
end;

function TUpdateMan.FilesToXml: String;
var
  i: Integer;
  F: TUpdateFileInfo;
begin
  Result := '<data version="' + BuildDateToStr + '" msg="' + StrToXml(FUserMsg) +
    '"><files>';
  for i := 0 to FFiles.Count - 1 do
  begin
    F := FFiles[i];
    Result := Result + '<file path="' + StrToXml(F.FileName) + '" time="' + IntToStr(F.Time) +
      '" size="' + IntToStr(F.Size) + '"/>';
  end;
  Result := Result + '</files></data>';
end;

constructor TUpdateMan.Create;
begin
  FDB := TDBEngine.Create;
  FDB.Database := AppConfig.UpdatesDBPath;
  FDB.Pwd := AppConfig.UpdatesDBPwd;
  if FDB.Pwd = '' then FDB.Pwd := 'masterkey';
  FFiles := TUpdateFileList.Create;
end;

destructor TUpdateMan.Destroy;
begin
  FFiles.Free;
  FDB.Free;
  inherited Destroy;
end;

function TUpdateMan.ConnectOrCreate: Boolean;
var
  NeedCreate: Boolean;
begin
  Result := False;
  NeedCreate := False;
  try
    FDB.Connect(False);
    Result := True;
  except
    on E: Exception do
    begin
      //if E is EIBDatabaseError then
      //  Debug(EIBDatabaseError(E).ErrorCode);
      NeedCreate := (E is EIBDatabaseError) and (EIBDatabaseError(E).ErrorCode = 335544344);
      if not NeedCreate then
        ErrMsg(Format(rsErrConnectUpdatesDB, [FDB.Database]) + ExceptionToString(E, True, False));
    end;
  end;

  if not NeedCreate then Exit;

  try
    FDB.Conn.CreateDB;
    CreateUpdateDatabase;
    Result := Connect;
  except
    on E: Exception do
      ErrMsg(Format(rsErrCreateUpdatesDB, [FDB.Database]) + ExceptionToString(E, True, False));
  end;
end;

function TUpdateMan.Connect: Boolean;
var
  Msg: String;
begin
  Result := False;
  try
    FDB.Connect;
    Result := True;
  except
    on E: Exception do
    begin
      Msg := Format(rsErrConnectUpdatesDB, [FDB.Database]);
      ErrMsg(Msg + ExceptionToString(E, True, False));
    end;
  end;
end;

procedure TUpdateMan.LoadFromDb;
var
  St: TStream;
  Reader: TFileDataReader;
begin
  Reader := nil;
  with FDB.OpenDataSet('select data from dx_files where id=1') do
  try
    St := CreateBlobStream(Fields[0], bmRead);
    if St <> nil then
    begin
      Reader := TFileDataReader.Create;
      Reader.UMan := Self;
      Reader.ParseStream(St);
    end;
  finally
    FreeAndNil(Reader);
    FreeAndNil(St);
    Free;
  end;
end;

procedure TUpdateMan.SaveToDb;
var
  DS: TSQLQuery;
begin
  DS := FDB.OpenDataSet('select id, data from dx_files where id=1');
  try
    if DS.RecordCount = 0 then
    begin
      DS.Append;
      DS.Fields[0].AsInteger := 1;
    end
    else
      DS.Edit;
    DS.Fields[1].AsString := FilesToXml;
    DS.Post;
    FDB.ApplyDataSet(DS);
    FDB.Commit;
  finally
    DS.Free;
  end;
end;

procedure TUpdateMan.UploadFiles(const BaseDir: String; AFileList: TStrings);
var
  ZipFlNm, FlNm: String;
  i: Integer;
  DS: TSQLQuery;
  FS: TFileStream;
  St: TStream;
  FlTm, FlSz: LongInt;
  OldList: TUpdateFileList;
begin
  ZipFlNm := GetTempFileName(GetTempDir, 'dxupdates');
  OldList := TUpdateFileList.Create;
  OldList.CopyFrom(FFiles);
  FFiles.Clear;

  try

  with TMyZipper.Create do
  try
    for i := 0 to AFileList.Count - 1 do
    begin
      FlNm := AFileList[i];
      FlTm := FileAge(FlNm);
      FlSz := FileSize(FlNm);
      Delete(FlNm, 1, Length(BaseDir));
      FFiles.AddFile(FlNm, FlTm, FlSz);
      FlNm := StringReplace(FlNm, '\', '/', [rfReplaceAll]);
      Entries.AddFileEntry(AFileList[i], FlNm);
    end;
    FileName := ZipFlNm;
    ZipAllFiles;
  finally
    Free;
  end;

  DS := FDB.OpenDataSet('select id, data, files from dx_files where id=1');
  FS := TFileStream.Create(ZipFlNm, fmOpenRead + fmShareDenyNone);
  try
    if DS.RecordCount = 0 then
    begin
      DS.Append;
      DS.Fields[0].AsInteger := 1;
    end
    else
      DS.Edit;
    DS.Fields[1].AsString := FilesToXml;
    St := DS.CreateBlobStream(DS.Fields[2], bmWrite);
    St.CopyFrom(FS, FS.Size);
    FDB.ApplyDataSet(DS);
    FDB.Commit;
  finally
    St.Free;
    DS.Free;
    FS.Free;
    DeleteFile(ZipFlNm);
  end;

  OldList.Free;

  except
    FFiles.CopyFrom(OldList);
    OldList.Free;
    raise;
  end;
end;

procedure TUpdateMan.DownloadFiles;
var
  DS: TSQLQuery;
  St: TStream;
  TmpDir, ZipFlNm: String;
begin
  TmpDir := GetTempDir + 'dxupdates.tmp' + PathDelim;
  ZipFlNm := GetTempFileName(GetTempDir, 'dxupdates');
  if DirectoryExists(TmpDir) then
  begin
    if not DeleteDirectory(TmpDir, True) then
      raise Exception.Create(Format(rsCanNotCleanDir, [TmpDir]));
  end
  else if not CreateDir(TmpDir) then
    raise Exception.Create(Format(rsCanNotCreateTempDir, [TmpDir]));

  DS := FDB.OpenDataSet('select files from dx_files where id=1');
  try

    St := DS.CreateBlobStream(DS.Fields[0], bmRead);

    if St <> nil then
    begin
      with TFileStream.Create(ZipFlNm, fmCreate) do
      try
        CopyFrom(St, St.Size);
      finally
        Free;
      end;

      with TUnZipper.Create do
      try
        FileName := ZipFlNm;
        OutputPath := TmpDir;
        UnZipAllFiles;
      finally
        Free;
      end;

      DeleteFile(ZipFlNm);
    end;

  finally
    FreeAndNil(St);
    DS.Free;
  end;
end;

function TUpdateMan.CheckUpdates: Boolean;
var
  i: Integer;
  S: String;
  F: TUpdateFileInfo;
begin
  Result := False;
  LoadFromDb;
  for i := 0 to FFiles.Count - 1 do
  begin
    F := FFiles[i];
    if CompareText(F.FileName, 'dataexpress.cfg') = 0 then Continue;
    S := AppPath + F.FileName;
    //Debug(S);
    //Debug(IntToStr(FileAge(S)) + ' - ' + IntToStr(F.Time));
    if not FileExists(S) or (FileAge(S) <> F.Time) then Exit(True);
  end;
end;


end.

