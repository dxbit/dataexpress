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

unit ReportManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, dxreports, Forms, mytypes;

type

  { TReportManager }

  TReportManager = class
  private
    FReports: TList;
    FAdded, FDeleted: TIntegerList;
    function GetReports(Index: Integer): TReportData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromDB;
    procedure SaveToDB;
    procedure SaveToDir(const aDir: String);
    procedure LoadFromDir(const aDir: String);
    procedure LoadFromCache(const aDir: String);
    function LoadFromFile(const FileName: String): TReportData;
    function CreateReport(Id: Integer): TReportData;
    function CreateNewReport: TReportData;
    procedure AddReport(RD: TReportData);
    function AddCopyReport(aRD: TReportData): TReportData;
    procedure ReplaceReport(OldRD, NewRD: TReportData);
    procedure DeleteReport(RD: TReportData);
    function ReportCount: Integer;
    function FindReport(aId: Integer): TReportData;
    function FindQueryByName(const aName: String): TReportData;
    function FindByName(const aName: String): TReportData;
    procedure GetReports(L: TStrings);
    procedure CorrectMaxId;
    function CloneManager: TReportManager;
    function MakeUniqueName(const AnyName: String): String;
    property Reports[Index: Integer]: TReportData read GetReports;
  end;

var
  ReportMan: TReportManager;

implementation

uses
  apputils, dbengine, Db, LazUtf8, FileUtil, LazFileUtils;


{ TReportManager }

function TReportManager.GetReports(Index: Integer): TReportData;
begin
  Result := TReportData(FReports[Index]);
end;

constructor TReportManager.Create;
begin
  FReports := TList.Create;
  FAdded := TIntegerList.Create;
  FDeleted := TIntegerList.Create;
end;

destructor TReportManager.Destroy;
begin
  FAdded.Free;
  FDeleted.Free;
  ClearList(FReports);
  FReports.Free;
  inherited Destroy;
end;

procedure TReportManager.Clear;
begin
  ClearList(FReports);
  FDeleted.Clear;
  FAdded.Clear;
end;

procedure TReportManager.LoadFromDB;
var
  DS: TSQLQuery;
  MS: TMemoryStream;
  RD: TReportData;
begin
  Clear;
  DS := DBase.OpenDataSet('select id, data, lastmodified from dx_reports');
  MS := TMemoryStream.Create;
  try
    while DS.EOF = False do
    begin
      MS.Size:=0;
      TBlobField(DS.Fields[1]).SaveToStream(MS);
      MS.Position := 0;
      RD := TReportData.Create;
      RD.LoadFromStream(MS);
      RD.LastModified := DS.Fields[2].AsDateTime;
      FReports.Add(RD);

      if DBase.IsRemote then
        Application.ProcessMessages;
      DS.Next;
    end;
  finally
    MS.Free;
    DS.Free;
  end;
end;

procedure TReportManager.SaveToDB;
var
  Wh: String;
  i: Integer;
  MS: TMemoryStream;
  DS: TSQLQuery;
  RD: TReportData;
begin
  // Удаление

  //Debug('Удаление отчетов');

  Wh := '';
  for i := 0 to FDeleted.Count - 1 do
  begin
    Wh := Wh + IntToStr(FDeleted[i]) + ',';
    //Debug(FDeleted[i]);
  end;
  if Wh <> '' then
  begin
    SetLength(Wh, Length(Wh) - 1);
    DS := DBase.CreateQuery('delete from dx_reports where id in (' + Wh + ')');
    try
      DBase.ExecuteQuery(DS);
    finally
      DS.Free;
    end;
  end;
  //Debug('');

  // Добавление новых

  //Debug('Добавление отчетов');

  MS := TMemoryStream.Create;
  DS := DBase.CreateQuery('insert into dx_reports (id, data, lastmodified) values (:id, :data, :lastmodified)');
  try
    DS.Prepare;

    for i := 0 to FAdded.Count - 1 do
    begin
      RD := FindReport(FAdded[i]);

      MS.Size:=0;
      RD.SaveToStream(MS);
      MS.Position:=0;

      DS.Params[0].AsInteger := RD.Id;
      DS.Params[1].LoadFromStream(MS, ftMemo);
      DS.Params[2].AsDateTime := RD.LastModified;

      DBase.ExecuteQuery(DS);
      RD.ResetReportChanged;

      //Debug(RD.Name);
    end;
  finally
    DS.Free;
    MS.Free;
  end;
  //Debug('');

  // Замена существующих

  //Debug('Замена отчетов');

  MS := TMemoryStream.Create;
  DS := DBase.CreateQuery('update dx_reports set data=:data, lastmodified=:lastmodified where id=:id');
  try
    DS.Prepare;

    for i := 0 to ReportCount - 1 do
    begin
      RD := Reports[i];
      if (FAdded.FindValue(RD.Id) >= 0) or not RD.ReportChanged then Continue;

      MS.Size:=0;
      RD.SaveToStream(MS);
      MS.Position:=0;

      DS.Params[0].LoadFromStream(MS, ftMemo);
      DS.Params[1].AsDateTime := RD.LastModified;
      DS.Params[2].AsInteger := RD.Id;

      DBase.ExecuteQuery(DS);
      RD.ResetReportChanged;

      //Debug(RD.Name);
    end;
  finally
    DS.Free;
    MS.Free;
  end;
  //Debug('');

  FAdded.Clear;
  FDeleted.Clear;
end;

{procedure TReportManager.SaveToDB;
var
  DS: TSQLQuery;
  MS: TMemoryStream;
  i: Integer;
  RD: TReportData;
begin
  DS := DBase.OpenDataSet('select id, data from dx_reports');
  MS := TMemoryStream.Create;
  try
  	while not DS.Eof do
    	DS.Delete;

    for i := 0 to FReports.Count - 1 do
    begin
      RD := Reports[i];
      MS.Size:=0;
      RD.SaveToStream(MS);
      MS.Position:=0;

      DS.Append;
      DS.Fields[0].Value:=RD.Id;
      TBlobField(DS.Fields[1]).LoadFromStream(MS);
      DS.Post;
    end;
    DBase.ApplyDataset(DS);
  finally
    MS.Free;
    DS.Free;
  end;
end;}

procedure TReportManager.SaveToDir(const aDir: String);
var
  i: Integer;
  RD: TReportData;
  FS: TFileStream;
begin
  for i := 0 to ReportCount - 1 do
  begin
    RD := Reports[i];
    FS := TFileStream.Create(aDir + IntToStr(RD.Id) + '.rpt', fmCreate + fmOpenWrite);
    try
      RD.SaveToStream(FS);
      SetFileDateTime(FS.Handle, RD.LastModified);
    finally
      FS.Free;
    end;
  end;
end;

procedure TReportManager.LoadFromDir(const aDir: String);
var
  SL: TStringList;
  i, id: Integer;
  RD: TReportData;
  FS: TFileStream;
begin
  Clear;
  SL := TStringList.Create;
  try

  FindAllFiles(SL, aDir, '*.rpt', False);
  for i := 0 to SL.Count - 1 do
  begin
    if not TryStrToInt(ChangeFileExt(ExtractFileName(SL[i]), ''), id) then
      Continue;
    RD := TReportData.Create;
    FS := TFileStream.Create(SL[i], fmOpenRead + fmShareDenyNone);
    try
      RD.LoadFromStream(FS);
      FReports.Add(RD);
    finally
      FS.Free;
    end;
    RD.LastModified:=GetFileDateTime(SL[i]);
  end;

  finally
    SL.Free;
  end;
end;

procedure TReportManager.LoadFromCache(const aDir: String);
var
  DS: TSQLQuery;
  RD: TReportData;
  BS: TStream;
  FlNm: String;
  FS: TFileStream;
  i, RDId: Integer;
  SL: TStringList;
  LastModified: TDateTime;
begin
  Clear;
  DS := DBase.OpenDataSet('select id, data, lastmodified from dx_reports');
  try
    while not DS.EOF do
    begin
      FlNm := aDir + DS.Fields[0].AsString + '.rpt';
      LastModified := DS.Fields[2].AsDateTime;
      if not FileExists(FlNm) or not SameFileDateTime(FlNm, LastModified) then
      begin
        FS := nil;
        BS := DS.CreateBlobStream(DS.Fields[1], bmRead);
        try
          RD := TReportData.Create;
          RD.LoadFromStream(BS);

          FS := TFileStream.Create(FlNm, fmCreate);
          FS.CopyFrom(BS, 0);
          SetFileDateTime(FS.Handle, LastModified);
        finally
          BS.Free;
          FreeAndNil(FS);
        end;

        if DBase.IsRemote then
          Application.ProcessMessages;
      end
      else
        RD := LoadFromFile(FlNm);

      RD.LastModified := LastModified;
      FReports.Add(RD);

      DS.Next;
    end;
  finally
    DS.Free;
  end;

  SL := TStringList.Create;
  try
    FindAllFiles(SL, aDir, '*.rpt', False);
    for i := 0 to SL.Count - 1 do
    begin
      if TryStrToInt( ExtractFileNameOnly(SL[i]), RDId ) then
      begin
        if FindReport(RDId) = nil then
          DeleteFile(SL[i]);
      end;
    end;
  finally
    SL.Free;
  end;
end;

function TReportManager.LoadFromFile(const FileName: String): TReportData;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
  Result := TReportData.Create;
  try
    Result.LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

function TReportManager.CreateReport(Id: Integer): TReportData;
var
  RD: TReportData;
  MS: TMemoryStream;
begin
  Result := nil;
  RD := FindReport(Id);
  if RD = nil then Exit;

  Result := TReportData.Create;
  MS := TMemoryStream.Create;
  RD.SaveToStream(MS);
  MS.Position:=0;
  Result.LoadFromStream(MS);
  MS.Free;
end;

function TReportManager.CreateNewReport: TReportData;
begin
  Result := TReportData.Create;
  Result.Id:=DBase.GenId('gen_rid');
  Result.Version:=2;
  FReports.Add(Result);
  FAdded.AddValue(Result.Id);
end;

procedure TReportManager.AddReport(RD: TReportData);
begin
  FReports.Add(RD);
  FAdded.AddValue(Rd.Id);
end;

function TReportManager.AddCopyReport(aRD: TReportData): TReportData;
var
  MS: TMemoryStream;
begin
  Result := TReportData.Create;
  FReports.Add(Result);
  MS := TMemoryStream.Create;
  aRD.SaveToStream(MS);
  MS.Position:=0;
  Result.LoadFromStream(MS);
  MS.Free;
  FAdded.AddValue(aRD.Id);
end;

procedure TReportManager.ReplaceReport(OldRD, NewRD: TReportData);
var
  i: Integer;
begin
  i := FReports.IndexOf(OldRD);
  FReports[i] := NewRD;
  OldRD.Free;
end;

procedure TReportManager.DeleteReport(RD: TReportData);
begin
  if not FAdded.DeleteValue(RD.Id) then
    FDeleted.AddValue(RD.Id);

  FReports.Remove(RD);
  RD.Free;
end;

function TReportManager.ReportCount: Integer;
begin
  Result := FReports.Count;
end;

function TReportManager.FindReport(aId: Integer): TReportData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to ReportCount - 1 do
    if Reports[i].Id = aId then Exit(Reports[i]);
end;

function TReportManager.FindQueryByName(const aName: String): TReportData;
var
  i: Integer;
  RD: TReportData;
begin
  Result := nil;
  for i := 0 to ReportCount - 1 do
  begin
    RD := Reports[i];
    if (RD.Kind = rkQuery) and (MyUtf8CompareText(aName, RD.Name) = 0) then
      Exit(RD);
  end;
end;

function TReportManager.FindByName(const aName: String): TReportData;
var
  i: Integer;
  RD: TReportData;
begin
  Result := nil;
  for i := 0 to ReportCount - 1 do
  begin
    RD := Reports[i];
    if MyUtf8CompareText(aName, RD.Name) = 0 then
      Exit(RD);
  end;
end;

procedure TReportManager.GetReports(L: TStrings);
var
  SL: TStringListUtf8;
  RD: TReportData;
  i: Integer;
begin
  SL := TStringListUtf8.Create;
  for i := 0 to ReportCount - 1 do
  begin
    RD := Reports[i];
    if RD.Kind <> rkReport then Continue;
    SL.AddObject(RD.Name, RD);
  end;
  SL.Sort;
  L.Assign(SL);
  SL.Free;
end;

procedure TReportManager.CorrectMaxId;
var
  i, MaxId: Integer;
  RD: TReportData;
begin
  MaxId := DBase.GetCurrentId('gen_rid');
  for i := 0 to ReportCount - 1 do
  begin
    RD := Reports[i];
    if RD.Id > MaxId then MaxId := RD.Id;
  end;
  DBase.ChangeId('gen_rid', MaxId + 1);
end;

function TReportManager.CloneManager: TReportManager;
var
  i: Integer;
  RD: TReportData;
begin
  Result := TReportManager.Create;
  for i := 0 to ReportCount - 1 do
  begin
    RD := CreateReport(GetReports(i).Id);
    Result.FReports.Add(RD);
  end;
end;

function TReportManager.MakeUniqueName(const AnyName: String): String;
var
  n: Integer;
  S: String;
begin
  SplitComponentName(AnyName, S, n);
  Inc(n);
  while FindByName(S + IntToStr(n)) <> nil do
    Inc(n);
  Result := S + IntToStr(n);  end;

end.

