{
Copyright © 2015-2017 Pavel Duborkin
Author: Pavel Duborkin
E-Mail: 7bit@list.ru, mydataexpress@mail.ru

This file is part of DataExpress.

DataExpress is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

DataExpress is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with DataExpress.  If not, see <http://www.gnu.org/licenses/>.
}
unit ReportManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, DXReports;

type

  { TReportManager }

  TReportManager = class
  private
    FReports: TList;
    function GetReports(Index: Integer): TReportData;
    function ReportsExists: Boolean;
    procedure CreateReports;
    procedure CheckReport(RD: TReportData);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure CheckReports;
    procedure LoadFromDB;
    procedure SaveToDB;
    procedure SaveToDir(const aDir: String);
    procedure LoadFromDir(const aDir: String);
    function CreateReport(Id: Integer): TReportData;
    function CreateNewReport: TReportData;
    function AddCopyReport(aRD: TReportData): TReportData;
    procedure DeleteReport(RD: TReportData);
    function ReportCount: Integer;
    function FindReport(aId: Integer): TReportData;
    function FindQueryByName(const aName: String): TReportData;
    function FindByName(const aName: String): TReportData;
    procedure GetReports(L: TStrings);
    procedure CurrentMaxId;
    property Reports[Index: Integer]: TReportData read GetReports;
  end;

var
  ReportMan: TReportManager;

implementation

uses
  apputils, dbengine, Db, LazUtf8, dxctrls, FileUtil, mytypes;

{ TReportManager }

function TReportManager.GetReports(Index: Integer): TReportData;
begin
  Result := TReportData(FReports[Index]);
end;

function TReportManager.ReportsExists: Boolean;
var
  DS: TSQLQuery;
begin
  try
    DS := DBase.OpenDataSet('select id from dx_reports');
    Result := True;
    DS.Free;
  except
    Result := False;
  end;
end;

procedure TReportManager.CreateReports;
begin
  DBase.Execute('CREATE TABLE DX_REPORTS (ID INTEGER, DATA BLOB SUB_TYPE 1 SEGMENT SIZE 80);' +
    'CREATE SEQUENCE GEN_RID;');
end;

// Начиная с версии 1.9.5, сборка после 25.08.2016, изменена идентификация
// полей запроса. Теперь поле определяется не по порядковому номеру в списке,
// а по уникальному идентификатору. ИД формируется как максимальный ИД + 1.
// Это позволит избежать запутывания столбков в гриде при изменении состава
// или порядка полей. Эта функция проверяет все ID полей, если 0, то
// присваивается порядковый номер.
procedure TReportManager.CheckReport(RD: TReportData);
var
  i, j: Integer;
  pSrc: PRpSource;
  pF: PRpField;
  pCF: PRpCalcField;
  CD: TRpColoringData;
  Col: TRpGridColumn;
begin
  if RD.Version = 2 then Exit;
  for i := 0 to RD.Sources.Count - 1 do
  begin
    pSrc := RD.Sources[i];
    for j := 0 to pSrc^.Fields.Count - 1 do
    begin
      pF := pSrc^.Fields[j];
      pF^.Id := j;
    end;
  end;
  for i := 0 to RD.CalcFields.Count - 1 do
  begin
    pCF := RD.CalcFields[i];
    pCF^.Id := i;
  end;
  // Меняем заголовок столбца на имя поля набора.
  for i := 0 to RD.Coloring.Count - 1 do
  begin
    CD := RD.Coloring[i];
    Col := RD.Grid.FindColumnByTitle(CD.FieldName);
    if Col <> nil then
      CD.FieldName:=Col.FieldName;
  end;
  RD.Version := 2;
end;

constructor TReportManager.Create;
begin
  FReports := TList.Create;
end;

destructor TReportManager.Destroy;
begin
  ClearList(FReports);
  FReports.Free;
  inherited Destroy;
end;

procedure TReportManager.Clear;
begin
  ClearList(FReports);
end;

procedure TReportManager.CheckReports;
begin
  if not ReportsExists then
    CreateReports;
end;

procedure TReportManager.LoadFromDB;
var
  DS: TSQLQuery;
  MS: TMemoryStream;
  RD: TReportData;
begin
  Clear;
  DS := DBase.OpenDataSet('select id, data from dx_reports');
  MS := TMemoryStream.Create;
  try
    while DS.EOF = False do
    begin
      MS.Size:=0;
      TBlobField(DS.Fields[1]).SaveToStream(MS);
      MS.Position := 0;
      RD := TReportData.Create;
      RD.LoadFromStream(MS);
      CheckReport(RD);
      FReports.Add(RD);

      DS.Next;
    end;
  finally
    MS.Free;
    DS.Free;
  end;
end;

procedure TReportManager.SaveToDB;
var
  DS: TSQLQuery;
  MS: TMemoryStream;
  i: Integer;
  RD: TReportData;
begin
  //DBase.Execute('delete from dx_reports;');

  DS := DBase.OpenSysTable('select id, data from dx_reports', 'dx_reports');
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
    DBase.Commit;
  finally
    MS.Free;
    DS.Free;
  end;
end;

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
    FS := TFileStream.Create(SL[i], fmOpenRead);
    try
      RD.LoadFromStream(FS);
      CheckReport(RD);
      FReports.Add(RD);
    finally
      FS.Free;
    end;
  end;

  finally
    SL.Free;
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
end;

procedure TReportManager.DeleteReport(RD: TReportData);
begin
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
    if (RD.Kind = rkQuery) and (Utf8CompareText(aName, RD.Name) = 0) then
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
    if Utf8CompareText(aName, RD.Name) = 0 then
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

procedure TReportManager.CurrentMaxId;
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
  DBase.ChangeId('gen_rid', MaxId);
end;

end.

