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

unit DXMains;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, sqldb, Db, Forms, strconsts;

type
  TProjectChangeContext = (pccDesigner, pccReports, pccSaveReports, pccUsers, pccExt);

  { TDXMain }

  TDXMain = class
  private
    FActions: String;
    FDesignTimePPI: Integer;
    //FCanProjectChange, FClearCounter: Boolean;
    FLastModified: TDateTime;
    procedure LoadSettings(St: TStream);
    procedure SaveSettings(St: TStream);
    procedure LoadActions(St: TStream);
    procedure SaveActions(St: TStream);
    procedure AppendOrEdit(DS: TDataSet);
    function GetLastModified: TDateTime;
  public
    constructor Create;
    procedure LoadFromDb;
    procedure SaveToDb;
    procedure LoadFromDir(const Dir: String);
    procedure SaveToDir(const Dir: String);
    procedure Clear;
    procedure CreateMain;
    procedure UpdateMain;
    procedure UpdateMain2;
    procedure RunActions;
    function AllowDynamicForms: Boolean;
    procedure SetLastModified;
    function CanProjectChange: Boolean;
    function CanProjectSave: Boolean;
    property Actions: String read FActions write FActions;
    property DesignTimePPI: Integer read FDesignTimePPI write FDesignTimePPI;
    property LastModified: TDateTime read FLastModified;
  end;

var
  DXMain: TDXMain;

implementation

uses
  dbengine, dxactions, datasetprocessor, SAX, saxbasereader, dxusers,
  apputils, warningform, appsettings;

type

  { TSettingsReader }

  TSettingsReader = class(TSaxBaseReader)
  private
    FMain: TdxMain;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
  public
    property Main: TdxMain read FMain write FMain;
  end;

{ TSettingsReader }

procedure TSettingsReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
begin
  if LocalName = 'designer' then
  begin
    FMain.DesignTimePPI := GetInt(Atts, 'designtimeppi');
  end
end;

{ TDXMain }

procedure TDXMain.CreateMain;
begin
  DBase.Execute('CREATE TABLE DX_MAIN (ID INTEGER, ' +
    'ACTIONS BLOB SUB_TYPE 1 SEGMENT SIZE 80);');
    {, ' +
    'SETTINGS BLOB SUB_TYPE 1 SEGMENT SIZE 80, ' +
    'FLAGS BLOB SUB_TYPE 1 SEGMENT SIZE 80);');}
end;

procedure TDXMain.UpdateMain;
begin
  DBase.Execute('ALTER TABLE DX_MAIN ADD SETTINGS BLOB SUB_TYPE 1 SEGMENT SIZE 80;');
end;

procedure TDXMain.UpdateMain2;
begin
  DBase.Execute('ALTER TABLE DX_MAIN ADD LASTMODIFIED TIMESTAMP;');
end;

procedure TDXMain.RunActions;
var
  DSP: TDataSetProcessor;
begin
  if FActions = '' then Exit;

  DSP := TDataSetProcessor.Create;
  DSP.BindDummyForm;

  with TActionRunner.Create do
  try
    DSProc := DSP;
    DSRi := 0;
    Load(FActions);
    Run;
  finally
    DSP.ClearDummyForm;
    DSP.Free;
    Free;
  end;
end;

function TDXMain.AllowDynamicForms: Boolean;
begin
  //Result := UserMan.IsUser and not UserMan.CurrentUser.WasDeveloper and
  //  not AppConfig.Caching;
  Result := False;
end;

procedure TDXMain.SetLastModified;
begin
  DBase.Execute('merge into DX_MAIN m ' +
    'using (select 1 as id from rdb$database) m2 ' +
    'on (m.id=m2.id) ' +
    'when matched then update set lastmodified=CURRENT_TIMESTAMP '+
    'when not matched then insert (id, lastmodified) values (1, CURRENT_TIMESTAMP);');
  FLastModified := GetLastModified;
end;

function TDXMain.GetLastModified: TDateTime;
begin
  with DBase.OpenDataSet('select lastmodified from DX_MAIN where id=1') do
  begin
    Result := Fields[0].AsDateTime;
    Free;
  end;
end;

function TDXMain.CanProjectChange: Boolean;
begin
  Result := GetLastModified = FLastModified;
  if not Result then ShowWarnInfo(rsChangeProjectProhibited, rsChangeProhibitedDetails);
end;

function TDXMain.CanProjectSave: Boolean;
begin
  Result := GetLastModified = FLastModified;
  if not Result then ShowWarnInfo(rsSaveProjectProhibited, rsChangeProhibitedDetails);
end;

procedure TDXMain.LoadSettings(St: TStream);
begin
  if St = nil then Exit;
  with TSettingsReader.Create do
  try
    Main := Self;
    ParseStream(St);
  finally
    Free;
  end;
end;

procedure TDXMain.SaveSettings(St: TStream);
var
  S: String;
begin
  FDesignTimePPI := Screen.PixelsPerInch;
  S := '<settings><designer designtimeppi="' + IntToStr(Screen.PixelsPerInch) +
    '"/></settings>';
  St.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TDXMain.LoadActions(St: TStream);
begin
  if St = nil then Exit;
  SetLength(FActions, St.Size);
  St.ReadBuffer(Pointer(FActions)^, St.Size);
end;

procedure TDXMain.SaveActions(St: TStream);
var
  S: String;
begin
  S := FActions;
  St.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TDXMain.AppendOrEdit(DS: TDataSet);
begin
  if DS.RecordCount = 0 then
  begin
    DS.Append;
    DS.Fields[0].AsInteger := 1;
  end
  else DS.Edit;
end;

constructor TDXMain.Create;
begin
  FDesignTimePPI := 96;
end;

procedure TDXMain.LoadFromDb;
var
  St: TStream;
  DS: TSQLQuery;
begin
  Clear;
  DS := DBase.OpenDataSet('select actions, settings, lastmodified from dx_main where id=1');
  St := nil;
  try
    St := DS.CreateBlobStream(DS.Fields[0], bmRead);
    LoadActions(St);
    FreeAndNil(St);
    St := DS.CreateBlobStream(DS.Fields[1], bmRead);
    LoadSettings(St);
    FLastModified := DS.Fields[2].AsDateTime;
  finally
    FreeAndNil(St);
    DS.Free;
  end;
end;

procedure TDXMain.SaveToDb;
var
  DS: TSQLQuery;
  St: TStream;
begin
  St := nil;
  DS := DBase.OpenDataSet('select id, actions, settings from dx_main where id=1');
  try
    {while not DS.EOF do
      DS.Delete;}
    AppendOrEdit(DS);
    St := DS.CreateBlobStream(DS.Fields[1], bmWrite);
    SaveActions(St);
    FreeAndNil(St);
    St := DS.CreateBlobStream(DS.Fields[2], bmWrite);
    SaveSettings(St);
    DS.Post;
    DBase.ApplyDataSet(DS);
    //DBase.Commit;
  finally
    FreeAndNil(St);
    DS.Free;
  end;
end;

procedure TDXMain.LoadFromDir(const Dir: String);
var
  FS: TFileStream;
begin
  Clear;
  if FileExists(Dir + 'actions.main') then
  begin
    FS := TFileStream.Create(Dir + 'actions.main', fmOpenRead + fmShareDenyNone);
    try
      LoadActions(FS);
    finally
      FS.Free;
    end;
  end;
  if FileExists(Dir + 'settings.main') then
  begin
    FS := TFileStream.Create(Dir + 'settings.main', fmOpenRead + fmShareDenyNone);
    try
      LoadSettings(FS);
    finally
      FS.Free;
    end;
  end;
end;

procedure TDXMain.SaveToDir(const Dir: String);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(Dir + 'actions.main', fmCreate);
  try
    SaveActions(FS);
  finally
    FS.Free;
  end;

  FS := TFileStream.Create(Dir + 'settings.main', fmCreate);
  try
    SaveSettings(FS);
  finally
    FS.Free;
  end;
end;

procedure TDXMain.Clear;
begin
  FActions := '';
  FDesignTimePPI := 96;
end;

end.

