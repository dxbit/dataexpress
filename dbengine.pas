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
unit DbEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, MySQLDbLib, SQLDb, Db, Forms,
  Controls;

type

  { TDBEngine }

  TDBEngine = class
  private
    FConn: TIBConnection;
    FDatabase: String;
    FPwd: String;
    FRemote: Boolean;
    FTrans: TSQLTransaction;
    FUpdateTrans: TSQLTransaction;
    FLockTrans: TSQLTransaction;
    FLoader: TMySQLDbLibraryLoader;
    FUserName: String;
    procedure SetDatabase(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetLocalDB;
    procedure Connect;
    procedure Disconnect;
    procedure AttachDataSet(DS: TSQLQuery);
    procedure AttachForm(aForm: TComponent);
    procedure DettachDataSet(DS: TSQLQuery);
    procedure ApplyDataSet(DS: TSQLQuery);
    procedure Commit;
    procedure SafeCommit;
    procedure ReadCommit;
    procedure Rollback;
    function Connected: Boolean;
    function OpenDataSet(const SQL: String; const aSafeTable: String = ''): TSQLQuery;
    function OpenSysTable(const SQL, Table: String): TSQLQuery;
    procedure CreateDatabase;
    procedure Execute(const aSQL: String; aSafeTable: String = '');
    procedure ExecuteDirect(const aSQL: String);
    function GenId(const S: String): Variant;
    procedure ChangeId(const S: String; Value: Integer);
    function GetCurrentId(const S: String): Variant;
    function DatabaseExists: Boolean;
    function CheckVersion: Integer;
    procedure AddVersion;
    procedure UpdateVersion;
    property Database: String read FDatabase write SetDatabase;
    property Remote: Boolean read FRemote write FRemote;
    property UserName: String read FUserName write FUserName;
    property Pwd: String read FPwd write FPwd;
  end;

var
  DBase: TDBEngine;

implementation

uses
  apputils, LazUtf8, Dialogs;

{ TDBEngine }

procedure TDBEngine.SetDatabase(AValue: String);
begin
  if (AValue <> '') and (not FRemote) and (ExtractFilePath(AValue) = '')  then
    AValue := AppPath + AValue;
  FDatabase := AValue;
  {$ifdef windows}
  FConn.DatabaseName:=Utf8ToWinCP(AValue);
  {$else}
  FConn.DatabaseName := AValue;
  {$endif}
end;

constructor TDBEngine.Create;
begin
  FConn := TIBConnection.Create(nil);
  FConn.UserName:='sysdba';
  //FConn.Password:='masterkey';
  FConn.CharSet:='UTF8';;
  FTrans := TSQLTransaction.Create(nil);
  FTrans.Params.AddStrings(['isc_tpb_read', 'isc_tpb_read_committed',
    'isc_rec_version', 'isc_tpb_nowait']);
  FTrans.DataBase := FConn;
  FUpdateTrans := TSQLTransaction.Create(nil);
  FUpdateTrans.Params.AddStrings(['isc_tpb_write', 'isc_tpb_read_committed',
      'isc_rec_version', 'isc_tpb_nowait']);
  FUpdateTrans.DataBase := FConn;
  FLockTrans := TSQLTransaction.Create(nil);
  FLockTrans.Params.AddStrings(['isc_tpb_consistency', 'isc_tpb_lock_read=%0:s',
    'isc_tpb_lock_write=%0:s', 'isc_tpb_exclusive']);
  FLockTrans.Database := FConn;
  FLoader := TMySQLDBLibraryLoader.Create(nil);
  FLoader.ConnectionType:='Firebird';
end;

destructor TDBEngine.Destroy;
begin
  FreeAndNil(FLockTrans);
  FreeAndNil(FUpdateTrans);
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  FreeAndNil(FLoader);
  inherited Destroy;
end;

procedure TDBEngine.SetLocalDB;
begin
  FRemote := False;
  FPwd := 'masterkey';
end;

procedure TDBEngine.Connect;
var
  D: TConnectionDef;
  L: TLibraryUnLoadFunction;
begin
  if not FLoader.Enabled then
  begin
    {$ifdef windows}
    if FRemote then
      FLoader.LibraryName:=AppPath + 'fbclient.dll'
    else
      FLoader.LibraryName:=AppPath + 'fbclientd.dll';
    {$else}
    if FRemote then
      FLoader.LibraryName:='libfbclient.so.2'
    else
      FLoader.LibraryName:='libfbembed.so.2.5';
    {$endif}
    FLoader.Enabled:=True;
  end;
  FConn.Password:=FPwd;
  try
    FConn.Open;
  except
    // Если при подключении произошла ошибка, то
    // библиотека не выгружается (счетчик ссылок не уменьшается). Вызов
    // метода Close не дает результата. Подглядел реализацию Loader-а.
    on E: Exception do
    begin
      D := SqlDb.GetConnectionDef(FLoader.ConnectionType);
      L := D.UnLoadFunction;
      if L <> nil then L;
      raise;
    end;
  end;
end;

procedure TDBEngine.Disconnect;
begin
  FConn.Close(True);
  FLoader.Enabled:=False;
end;

procedure TDBEngine.AttachDataSet(DS: TSQLQuery);
begin
  DS.DataBase := FConn;
  DS.Transaction := FTrans;
  DS.UpdateTransaction := FUpdateTrans;
end;

procedure TDBEngine.AttachForm(aForm: TComponent);
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
end;

procedure TDBEngine.DettachDataSet(DS: TSQLQuery);
begin
  DS.Database := nil;
  DS.Transaction := nil;
end;

procedure TDBEngine.ApplyDataSet(DS: TSQLQuery);
begin
  try
    if DS.Active then DS.ApplyUpdates;
  except
    {on E: EIBDatabaseError do
    begin

    end; }
    on E: EDatabaseError do
    begin
      {if FTrans.Active then
	      FTrans.RollbackRetaining;}
      if FUpdateTrans.Active then
      	FUpdateTrans.Rollback;
      raise;
    end;
  end;
end;

procedure TDBEngine.Commit;
begin
  FUpdateTrans.Commit;
end;

procedure TDBEngine.SafeCommit;
begin
  FLockTrans.Commit;
end;

procedure TDBEngine.ReadCommit;
begin
  FTrans.Commit;
end;

procedure TDBEngine.Rollback;
begin
  if FUpdateTrans.Active then
	  FUpdateTrans.Rollback;
end;

function TDBEngine.Connected: Boolean;
begin
  Result := FConn.Connected;
end;

function TDBEngine.OpenDataSet(const SQL: String; const aSafeTable: String
  ): TSQLQuery;
begin
  //DebugStr(SQL);
  Result := TSQLQuery.Create(nil);
  if aSafeTable = '' then
  begin
    Result.Transaction := FTrans;
    Result.UpdateTransaction := FUpdateTrans;
  end
  else
  begin
    FLockTrans.Params.Text := Format(FLockTrans.Params.Text, [aSafeTable]);
    Result.Transaction := FLockTrans;
    Result.UpdateTransaction := FLockTrans;
  end;
  Result.DataBase := FConn;
  Result.SQL.Text := SQL;
  try
    Result.Open;
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

// Используется для записи в таблицы DX_FORMS, DX_REPORTS, DX_USERS, DX_SCRIPTS.
function TDBEngine.OpenSysTable(const SQL, Table: String): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
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
end;

procedure TDBEngine.CreateDatabase;
begin
  {$ifdef windows}
  FLoader.LibraryName:=AppPath + 'fbclientd.dll';
  {$else}
  FLoader.LibraryName := 'libfbembed.so.2.5';
  {$endif}
  FLoader.Enabled:=True;
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
    'CREATE TABLE DX_VERSION (VERSION INTEGER);' +
    'COMMIT;' +
    'INSERT INTO DX_VERSION (VERSION) VALUES (30);');
end;

procedure TDBEngine.Execute(const aSQL: String; aSafeTable: String);
var
  Trans: TSQLTransaction;
begin
  if aSQL = '' then Exit;
  if aSafeTable <> '' then
  begin
    Trans := FLockTrans;
    Trans.Params.Text := Format(Trans.Params.Text, [aSafeTable]);
  end
  else
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

procedure TDBEngine.ExecuteDirect(const aSQL: String);
begin
  FConn.ExecuteDirect(aSQL, FLockTrans);
end;

function TDBEngine.GenId(const S: String): Variant;
begin
  with OpenDataSet('select gen_id(' + S + ',1) from rdb$database') do
    try
      Result := Fields[0].Value;
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
    finally
      Free;
    end;
  except
    Result := 0;
  end;
end;

procedure TDBEngine.AddVersion;
begin
  Execute('CREATE TABLE DX_VERSION (VERSION INTEGER);');
  Execute('INSERT INTO DX_VERSION (VERSION) VALUES (30);');
end;

procedure TDBEngine.UpdateVersion;
begin
  Execute('UPDATE DX_VERSION SET VERSION=?;');
end;

end.

