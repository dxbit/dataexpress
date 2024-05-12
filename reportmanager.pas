unit ReportManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, dxreports, Forms;

type

  { TReportManager }

  TReportManager = class
  private
    FReports: TList;
    function GetReports(Index: Integer): TReportData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
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
    procedure CorrectMaxId;
    function CloneManager: TReportManager;
    function MakeUniqueName(const AnyName: String): String;
    property Reports[Index: Integer]: TReportData read GetReports;
  end;

var
  ReportMan: TReportManager;

implementation

uses
  apputils, dbengine, Db, LazUtf8, FileUtil, mytypes;


{ TReportManager }

function TReportManager.GetReports(Index: Integer): TReportData;
begin
  Result := TReportData(FReports[Index]);
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
  DS: TSQLQuery;
  MS: TMemoryStream;
  i: Integer;
  RD: TReportData;
begin
  //DBase.Execute('delete from dx_reports;');

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
    //DBase.Commit;
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
    FS := TFileStream.Create(SL[i], fmOpenRead + fmShareDenyNone);
    try
      RD.LoadFromStream(FS);
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

