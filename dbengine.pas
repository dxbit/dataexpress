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

unit DbEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, ibase60dyn, SQLDb, SQLDbLib, Db, Forms,
  Controls, LazFileUtils, dxctrls, strconsts, dxreports;

const
  DX_VERSION = 35;

type

  TDBEngine = class;

  { TdxConnection }

  TdxConnection = class(TIBConnection)
  protected
    function ConstructUpdateSQL(Query: TCustomSQLQuery; var ReturningClause: Boolean
      ): string; override;
    procedure UpdateIndexDefs(IndexDefs: TIndexDefs; TableName: string); override;
  end;

  { TdxDataSet }

  TdxDataSet = class(TSQLQuery)
  //protected
  //  procedure SetActive(Value: Boolean); override;
  private
    FForm: TdxForm;
    FRD: TReportData;
  protected
    procedure InternalInitFieldDefs; override;
    function LoadField(FieldDef: TFieldDef; buffer: pointer; out CreateBlob: boolean
      ): boolean; override;
  public
    procedure Delete; override;
    property Form: TdxForm read FForm write FForm;
    property RD: TReportData read FRD write FRD;
  end;

  { TFBLoader }

  TFBLoader = class(TSQLDBLibraryLoader)
  private
    function GetFirebird25Lib: String;
    function GetFirebird5Lib: String;
    procedure ForceUnloadLibrary;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFirebirdLibrary(DB: TDBEngine);
    function IsFirebird25Lib: Boolean;
    function IsFirebird5Lib: Boolean;
    function IsLibraryLoaded: Boolean;
  end;

  { TDBEngine }

  TDBEngine = class
  private
    //FLoader: TSQLDBLibraryLoader;
    FConn: TIBConnection;
    FDatabase: String;
    FDBVersion: Integer;
    FPwd: String;
    FTrans: TSQLTransaction;
    FUpdateTrans: TSQLTransaction;
    procedure ConnLog(Sender: TSQLConnection; EventType: TDBEventType;
      const Msg: String);
    procedure SetDatabase(AValue: String);
    //procedure LoadFirebirdLibrary;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect(ReplaceLib: Boolean = True);
    procedure Disconnect;
    procedure AttachDataSet(DS: TSQLQuery);
    //procedure AttachForm(aForm: TComponent);
    procedure DettachDataSet(DS: TSQLQuery);
    procedure ApplyDataSet(DS: TSQLQuery);
    procedure Commit;
    //procedure SafeCommit;
    procedure ReadCommit;
    procedure StartReadTrans;
    //procedure Rollback;
    function Connected: Boolean;
    function OpenDataSet(const SQL: String): TSQLQuery;
    //function OpenSysTable(const SQL, Table: String): TSQLQuery;
    procedure CreateDatabase;
    procedure Execute(const aSQL: String);
    procedure ExecuteWithoutCommit(const aSQL: String);
    function GenId(const S: String; N: Integer = 1): Integer;
    procedure ChangeId(const S: String; Value: Integer);
    function GetCurrentId(const S: String): Variant;
    function DatabaseExists: Boolean;
    function CheckVersion: Integer;
    procedure UpdateVersion(NewVersion: Integer);
    function IsRemote: Boolean;
    property Database: String read FDatabase write SetDatabase;
    property Pwd: String read FPwd write FPwd;
    property DBVersion: Integer read FDBVersion write FDBVersion;
    property Conn: TIBConnection read FConn;
  end;

var
  DBase: TDBEngine;
  FBLoader: TFBLoader;

implementation

uses
  apputils, LazUtf8, Dialogs, sqlgen, appsettings;

{ TdxConnection }

function TdxConnection.ConstructUpdateSQL(Query: TCustomSQLQuery;
  var ReturningClause: Boolean): string;
begin
  ReturningClause:=False;
  if (Query is TdxDataSet) and (TdxDataSet(Query).Form <> nil) then
    Result := SqlUpdateStatementOnlyChanged(TdxDataSet(Query).Form)
  else
    Result := inherited ConstructUpdateSQL(Query, ReturningClause);
end;

procedure TdxConnection.UpdateIndexDefs(IndexDefs: TIndexDefs; TableName: string
  );
begin

end;

{ TdxDataSet }

function RpFieldTypeToFieldType(Tp: TRpFieldType): TFieldType;
begin
  case Tp of
    flText: Result := ftString;
    flNumber: Result := ftFloat;
    flDate: Result := ftDate;
    flTime: Result := ftTime;
    else raise Exception.Create('RpFieldTypeToFieldType: unsupported field type.');
  end;
end;

procedure TdxDataSet.InternalInitFieldDefs;
var
  i: Integer;
  CF: PRpCalcField;
begin
  inherited InternalInitFieldDefs;
  if FRD = nil then Exit;
  for i := 0 to FRD.CalcFields.Count - 1 do
  begin
    CF := FRD.CalcFields[i];
    FieldDefs.Add('cf' + IntToStr(CF^.Id), RpFieldTypeToFieldType(CF^.Tp), CF^.Size,
      -1, False, False, FieldDefs.Count + 1, CP_UTF8);
  end;
end;

function GetNonCalcFieldsCount(RD: TReportData): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to RD.GetRpSQLFieldCount - 1 do
  begin
    if RD.GetFieldType(i) in [flFile, flImage] then Inc(Result, 4)
    else Inc(Result);
  end;
end;

function TdxDataSet.LoadField(FieldDef: TFieldDef; buffer: pointer; out
  CreateBlob: boolean): boolean;
var
  delta: Integer;
begin
  if FRD <> nil then
  begin
    if FRD.IsSimple then
    begin
      if FRD.HasParentIdField then delta := 2
      else delta := 1;
    end
    else
      delta := 0;
    if FieldDef.FieldNo > GetNonCalcFieldsCount(FRD) + delta then Exit(True);
  end;
  Result:=inherited LoadField(FieldDef, buffer, CreateBlob);
end;

procedure TdxDataSet.Delete;
begin
  inherited Delete;
  // Обход ошибки от 12.03.2022: на печать выводится последняя запись отфильтрованного
  // запроса (выходной фильтр), когда в запросе на самом деле нет записей (запрос пустой).
  // Получается, что внутренний буфер не очищается после удаления записи и
  // обращение к полю вытягивает старые данные из буфера.
  if IsEmpty then InitRecord(ActiveBuffer);
end;

{ TFBLoader }

constructor TFBLoader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ConnectionType := 'firebird';
end;

procedure TFBLoader.LoadFirebirdLibrary(DB: TDBEngine);
begin
  if not AppConfig.SupportDXDB then
  begin
    if IsLibraryLoaded and IsFirebird5Lib then
      ForceUnloadLibrary;

    if not Enabled then
    begin
      LibraryName := GetFirebird25Lib;
      Enabled := True;
    end;
    Exit;
  end;


  if Enabled then
  begin
    if (CompareText(ExtractFileExt(DB.Database), '.FDB') = 0) and
      not IsFirebird25Lib then ForceUnloadLibrary
    else if (CompareText(ExtractFileExt(DB.Database), '.FDB') <> 0) and
      not IsFirebird5Lib then ForceUnloadLibrary
  end;

  if not Enabled then
  begin
    if CompareText(ExtractFileExt(DB.Database), '.FDB') = 0 then
      LibraryName := GetFirebird25Lib
    else
      LibraryName := GetFirebird5Lib;
    Enabled := True;
  end;
end;

function TFBLoader.GetFirebird25Lib: String;
begin
  Result := AppPath + 'fbclientd.dll';
end;

function TFBLoader.GetFirebird5Lib: String;
begin
  Result := AppPath + 'fb5\fbclient.dll';
end;

function TFBLoader.IsFirebird25Lib: Boolean;
begin
  Result := LibraryName = AppPath + 'fbclientd.dll';
end;

function TFBLoader.IsFirebird5Lib: Boolean;
begin
  Result := LibraryName = AppPath + 'fb5\fbclient.dll';
end;

function TFBLoader.IsLibraryLoaded: Boolean;
begin
  Result := SqlDb.GetConnectionDef(ConnectionType).LoadedLibraryName <> '';
end;

procedure TFBLoader.ForceUnloadLibrary;
var
  LibName: String;
  i: Integer;
begin
  for i := 1 to 3 do
  begin
    UnloadLibrary;
    LibName := SqlDb.GetConnectionDef(ConnectionType).LoadedLibraryName;
    if LibName = '' then Break;
    Sleep(500);
  end;
  if LibName <> '' then
    raise Exception.CreateFmt(rsFbLibNotUnloadMsg, [LibName]);
end;

{ TDBEngine }

procedure TDBEngine.SetDatabase(AValue: String);
begin
  FDatabase := AValue;
  if not IsRemote and not FileNameIsAbsolute(FDatabase) then
    FDatabase := AppPath + FDatabase;
  {$ifdef windows}
  FConn.DatabaseName:=Utf8ToWinCP(FDatabase);
  {$else}
  FConn.DatabaseName := AValue;
  {$endif}
end;

{procedure TDBEngine.LoadFirebirdLibrary;
var
  LibName: String;
begin
  if FLoader.Enabled then
  begin
    if (CompareText(ExtractFileExt(FDatabase), '.FDB') = 0) and
      (FLoader.LibraryName <> AppPath + 'fbclientd.dll') then
      FLoader.Enabled := False
    else if (CompareText(ExtractFileExt(FDatabase), '.DXDB') = 0) and
      (FLoader.LibraryName <> AppPath + 'fb5\fbclient.dll') then
      FLoader.Enabled := False;
  end;

  if not FLoader.Enabled then
  begin
    LibName := SqlDb.GetConnectionDef(FLoader.ConnectionType).LoadedLibraryName;
    if LibName <> '' then
      raise Exception.CreateFmt(rsFbLibNotUnloadMsg, [LibName]);

    if CompareText(ExtractFileExt(FDatabase), '.FDB') = 0 then
      FLoader.LibraryName := AppPath + 'fbclientd.dll'
    else //if CompareText(ExtractFileExt(FDatabase), '.DXDB') = 0 then
      FLoader.LibraryName := AppPath + 'fb5\fbclient.dll';
    FLoader.Enabled := True;
  end;
end;   }

procedure TDBEngine.ConnLog(Sender: TSQLConnection; EventType: TDBEventType;
  const Msg: String);
var
  S: String;
begin
  case EventType of
    detCustom: S := 'Custom';
    detPrepare: S := 'Prepare';
    detExecute: S := 'Execute';
    detFetch: S := 'Fetch';
    detCommit: S := 'Commit';
    detRollback: S := 'Rollback';
    detParamValue: S := 'Param value';
    detActualSQL: S := 'Actual SQL';
    else S := '';
  end;
  S := '[' + TimeToStr(Time) + '] ' + S + ': ' + Msg;
  Debug(S);
end;

constructor TDBEngine.Create;
begin
  FConn := TdxConnection.Create(nil);
  FConn.UserName:='sysdba';
  FConn.CharSet:='UTF8';
  //FConn.LogEvents := [detCustom, detPrepare, detExecute, detFetch, detCommit, detRollBack, detParamValue, detActualSQL];
  //FConn.OnLog:=@ConnLog;
  FTrans := TSQLTransaction.Create(nil);
  FTrans.Params.AddStrings(['isc_tpb_read', 'isc_tpb_read_committed',
    'isc_tpb_nowait', 'isc_tpb_rec_version']);
  FTrans.DataBase := FConn;
  FUpdateTrans := TSQLTransaction.Create(nil);
  FUpdateTrans.Params.AddStrings(['isc_tpb_write', 'isc_tpb_read_committed',
      'isc_tpb_nowait', 'isc_tpb_rec_version']);
  FUpdateTrans.DataBase := FConn;
  (*FLoader := TSQLDBLibraryLoader.Create(nil);
  FLoader.ConnectionType := 'Firebird';
  {$IFDEF WINDOWS}
  FLoader.LibraryName := AppPath + 'fb5\fbclient.dll';
  {$ELSE}
  FLoader.LibraryName := AppPath + 'libfbclient.so.2.5.9';
  {$ENDIF}
  FLoader.Enabled := True;*)
end;

destructor TDBEngine.Destroy;
begin
  FreeAndNil(FUpdateTrans);
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  //FreeAndNil(FLoader);
  inherited Destroy;
end;

procedure TDBEngine.Connect(ReplaceLib: Boolean);
begin
  if (FPwd = '') or not IsRemote then FPwd := 'masterkey';
  FConn.Password := FPwd;
  if not FBLoader.IsLibraryLoaded or ReplaceLib then
    FBLoader.LoadFirebirdLibrary(Self);
  FConn.Open;
end;

procedure TDBEngine.Disconnect;
begin
  if not FConn.Connected then Exit;
  FConn.Close(True);
end;

procedure TDBEngine.AttachDataSet(DS: TSQLQuery);
begin
  DS.DataBase := FConn;
  DS.Transaction := FTrans;
  DS.UpdateTransaction := FUpdateTrans;
end;

{procedure TDBEngine.AttachForm(aForm: TComponent);
var
  i: Integer;
  C: TComponent;
begin
  for i := 0 to aForm.ComponentCount - 1 do
  begin
    C := aForm.Components[i];
    if C is TSQLQuery then
      AttachDataSet(TSQLQuery(C));
  end;
end; }

procedure TDBEngine.DettachDataSet(DS: TSQLQuery);
begin
  DS.Database := nil;
  DS.Transaction := nil;
end;

procedure TDBEngine.ApplyDataSet(DS: TSQLQuery);
var
  n: Integer;
begin
  n := 0;
  while True do
    try
      if DS.Active then DS.ApplyUpdates;
      Break;
    except
      on E: EDatabaseError do
      begin
        FUpdateTrans.Rollback;
        if Pos('lock conflict on no wait transaction', E.Message) > 0 then
        begin
          Sleep(50);
          Inc(n);
          if n < 3 then Continue
          else if MessageDlg(rsWarning, rsFailedToWriteData, mtWarning,
            [mbRetry, mbAbort], 0) = mrRetry then
          begin
            n := 0;
            Continue;
          end
          else raise;
        end
        else
          raise;
      end;
    end;
end;

procedure TDBEngine.Commit;
begin
  try
    FUpdateTrans.Commit;
  except
    on E: Exception do
    begin
      FUpdateTrans.Rollback;
      raise;
    end;
  end;
end;

{procedure TDBEngine.SafeCommit;
begin
  try
    FLockTrans.Commit;
  except
    on E: Exception do
    begin
      FLockTrans.Rollback;
      raise;
    end;
  end;
end;}

procedure TDBEngine.ReadCommit;
begin
  FTrans.Commit;
end;

// Запускаем читающую транзакцию, чтобы не вызывать AV при чтении блобов ImageMan.
procedure TDBEngine.StartReadTrans;
begin
  if not FTrans.Active then FTrans.StartTransaction;
end;

{procedure TDBEngine.Rollback;
begin
  if FUpdateTrans.Active then
	  FUpdateTrans.Rollback;
end;}

function TDBEngine.Connected: Boolean;
begin
  Result := FConn.Connected;
end;

function TDBEngine.OpenDataSet(const SQL: String): TSQLQuery;
var
  KeyField: TField;
begin
  //DebugStr(SQL);
  Result := TdxDataSet.Create(nil);
  Result.Transaction := FTrans;
  Result.UpdateTransaction := FUpdateTrans;
  Result.DataBase := FConn;
  Result.SQL.Text := SQL;
  try
    Result.Open;
    KeyField := Result.FindField('id');
    if KeyField <> nil then KeyField.ProviderFlags := KeyField.ProviderFlags + [pfInKey];
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

// Используется для записи в таблицы DX_FORMS, DX_REPORTS, DX_USERS, DX_SCRIPTS, DX_MAINS.
{function TDBEngine.OpenSysTable(const SQL, Table: String): TSQLQuery;
begin
  Result := TdxDataSet.Create(nil);
  Result.Transaction := FTrans;
  Result.UpdateTransaction := FUpdateTrans;
  Result.DataBase := FConn;
  Result.SQL.Text := SQL;
  Result.DeleteSQL.Text := 'delete from ' + Table + ' where id=:ID';
  try
    Result.Open;
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
end; }

procedure TDBEngine.CreateDatabase;
begin
  FBLoader.LoadFirebirdLibrary(Self);
  FConn.CreateDB;
  Execute('CREATE TABLE DX_FORMS (ID INTEGER, FORM BLOB SUB_TYPE 1 SEGMENT SIZE 80);' +
    'CREATE SEQUENCE GEN_TID;' +
    'CREATE SEQUENCE GEN_FID;' +
    'CREATE TABLE DX_REPORTS (ID INTEGER, DATA BLOB SUB_TYPE 1 SEGMENT SIZE 80);' +
    'CREATE SEQUENCE GEN_RID;' +
    'CREATE TABLE DX_USERS (ID INTEGER, USERS BLOB SUB_TYPE 1 SEGMENT SIZE 80,' +
    'ROLES BLOB SUB_TYPE 1 SEGMENT SIZE 80, INTFS BLOB SUB_TYPE 1 SEGMENT SIZE 80);' +
    'CREATE TABLE DX_CONN (ID INTEGER, UID INTEGER, MODE INTEGER, DTIME TIMESTAMP);' +
    'CREATE TABLE DX_LOCK (CID INTEGER, UID INTEGER, FMID INTEGER, RECID INTEGER, DTIME TIMESTAMP);' +
    'CREATE TABLE DX_SCRIPTS (ID INTEGER, ' +
    'SCRIPT BLOB SUB_TYPE 1 SEGMENT SIZE 80, EXTRA BLOB SUB_TYPE 1 SEGMENT SIZE 80, ' +
    'FMID INTEGER, NAME VARCHAR(255), KIND SMALLINT);' +
    'CREATE TABLE DX_MAIN (ID INTEGER, ACTIONS BLOB SUB_TYPE 1 SEGMENT SIZE 80, ' +
      'SETTINGS BLOB SUB_TYPE 1 SEGMENT SIZE 80, ' +
      'LASTMODIFIED TIMESTAMP);' +
    'CREATE TABLE DX_IMAGES (ID INTEGER, NAME VARCHAR(50), ' +
      'IMG_100 BLOB SUB_TYPE 0 SEGMENT SIZE 512, ' +
      'IMG_150 BLOB SUB_TYPE 0 SEGMENT SIZE 512, ' +
      'IMG_200 BLOB SUB_TYPE 0 SEGMENT SIZE 512);' +
    'CREATE TABLE DX_VERSION (VERSION INTEGER);' +
    'COMMIT;' +
    'INSERT INTO DX_VERSION (VERSION) VALUES (' + IntToStr(DX_VERSION) + ');');
end;

procedure TDBEngine.Execute(const aSQL: String);
var
  Trans: TSQLTransaction;
begin
  if aSQL = '' then Exit;
  Trans := FUpdateTrans;

  with TSQLScript.Create(nil) do
    try try
      DataBase := FConn;
      Transaction := Trans;
      Terminator:=';';
      Script.Text := aSQL;
      ExecuteScript;
      Trans.Commit;
    except
      on E: EDatabaseError do
      begin
        Trans.Rollback;
        raise;
      end;
    end;
    finally
      Free;
    end;
end;

procedure TDBEngine.ExecuteWithoutCommit(const aSQL: String);
begin
  with TSQLScript.Create(nil) do
    try try
      DataBase := FConn;
      Transaction := FUpdateTrans;
      Terminator:=';';
      Script.Text := aSQL;
      ExecuteScript;
    except
      on E: EIBDatabaseError do
      begin
        FUpdateTrans.Rollback;
        raise;
      end;
    end;
    finally
      Free;
    end;
end;

function TDBEngine.GenId(const S: String; N: Integer): Integer;
begin
  with OpenDataSet(Format('select gen_id(%s, %d) from rdb$database', [S, N])) do
    try
      Result := Fields[0].AsInteger;
    finally
      Free;
    end;
end;

procedure TDBEngine.ChangeId(const S: String; Value: Integer);
begin
  Execute('alter sequence ' + S + ' restart with ' + IntToStr(Value) + ';');
end;

function TDBEngine.GetCurrentId(const S: String): Variant;
begin
  with OpenDataSet('select gen_id(' + S + ',0) from rdb$database') do
    try
      Result := Fields[0].Value;
    finally
      Free;
    end;
end;

function TDBEngine.DatabaseExists: Boolean;
begin
  Result := FileExists(FDatabase);
end;

function TDBEngine.CheckVersion: Integer;
begin
  try
    with OpenDataSet('select version from dx_version') do
    try
	 	  Result := Fields[0].AsInteger;
      FDBVersion := Result;
    finally
      Free;
    end;
  except
    Result := 0;
  end;
end;

procedure TDBEngine.UpdateVersion(NewVersion: Integer);
begin
  Execute(Format('UPDATE DX_VERSION SET VERSION=%d;', [NewVersion]));
  FDBVersion := NewVersion;
end;

function TDBEngine.IsRemote: Boolean;
begin
  Result := IsRemoteDatabase(FDatabase);
end;

end.

