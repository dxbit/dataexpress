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
unit DxActions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, strconsts, mytypes, sqldb;

type
  TdxActionType = (actNone, actGotoForm, actPrint, actMassCalc, actOpenReport,
    actSaveChanges, actUserMonitor, actCallFunc, actClearFields);

  TBasicActionClass = class of TBasicAction;

  { TBasicAction }

  TBasicAction = class
  private
    FConfirmMsg: String;
    FDSProc: TObject;
    FDSRi: Integer;
    FExecCond: String;
    FMsg: String;
    FSaveRecord: Boolean;
  protected
    procedure InnerExecute; virtual;
  public
    constructor Create; virtual;
    procedure Load(const Xml: String); virtual;
    procedure Execute;
    property ExecCond: String read FExecCond write FExecCond;
    property ConfirmMsg: String read FConfirmMsg write FConfirmMsg;
    property Msg: String read FMsg write FMsg;
    property SaveRecord: Boolean read FSaveRecord write FSaveRecord;
    property DSProc: TObject read FDSProc write FDSProc;
    property DSRi: Integer read FDSRi write FDSRi;
  end;

  { TGotoFormAction }

  TGotoFormAction = class(TBasicAction)
  private
    FFormId: Integer;
  protected
    procedure InnerExecute; override;
  public
    property FormId: Integer read FFormId write FFormId;
  end;

  { TPrintAction }

  TPrintAction = class(TBasicAction)
  private
    FExpression: String;
    FTemplateFile: String;
  protected
    procedure InnerExecute; override;
  public
    property TemplateFile: String read FTemplateFile write FTemplateFile;
    property Expression: String read FExpression write FExpression;
  end;

  { TMassCalcAction }

  TMassCalcAction = class(TBasicAction)
  private
    FExpression: String;
    FFieldId: Integer;
    FFilter: String;
    FFormId: Integer;
  protected
    procedure InnerExecute; override;
  public
    property FormId: Integer read FFormId write FFormId;
    property Filter: String read FFilter write FFilter;
    property FieldId: Integer read FFieldId write FFieldId;
    property Expression: String read FExpression write FExpression;
  end;

  { TOpenReportAction }

  TOpenReportAction = class(TBasicAction)
  private
    FRpId: Integer;
  protected
    procedure InnerExecute; override;
  public
    property RpId: Integer read FRpId write FRpId;
  end;

  TSaveChangesAction = class(TBasicAction);

  { TUserMonitorAction }

  TUserMonitorAction = class(TBasicAction)
  protected
    procedure InnerExecute; override;
  end;

  { TCallFuncAction }

  TCallFuncAction = class(TBasicAction)
  private
    FExpr: String;
  protected
    procedure InnerExecute; override;
  public
    property Expression: String read FExpr write FExpr;
  end;

  { TClearFieldsAction }

  TClearFieldsAction = class(TBasicAction)
  private
    FFields: TIntList;
  protected
    procedure InnerExecute; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  	property Fields: TIntList read FFields;
  end;

function CreateAction(act: TdxActionType): TBasicAction;

implementation

uses
  SAX, saxbasereader, datasetprocessor, dxctrls, mainframe, apputils,
  expressions, variants, formmanager, sqlgen, Dialogs, dxusers, reportmanager,
  ReportWindow, Db, monitorform, dximages, dxfiles;

type

  { TActionReader }

  TActionReader = class(TSAXBaseReader)
  private
    FAction: TBasicAction;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
  public
    property Action: TBasicAction read FAction write FAction;
    procedure ParseXml(const Xml: String);
  end;

function CreateAction(act: TdxActionType): TBasicAction;
begin
  Result := nil;
  case act of
    actGotoForm: Result := TGotoFormAction.Create;
    actPrint: Result := TPrintAction.Create;
    actMassCalc: Result := TMassCalcAction.Create;
    actOpenReport: Result := TOpenReportAction.Create;
    actSaveChanges: Result := TSaveChangesAction.Create;
    actUserMonitor: Result := TUserMonitorAction.Create;
    actCallFunc: Result := TCallFuncAction.Create;
    actClearFields: Result := TClearFieldsAction.Create;
  end;
end;

function CalcExpr(const Expr: String; DSP: TDataSetProcessor; DSRi: Integer; var V: Variant): Boolean;
var
  DSR: TDataSetRec;
  Ex: TExpression;
begin
  Result := False;
  DSR := DSP.DataSets[DSRi]^;
  Ex := nil;
  with TExpressionBuilder.Create do
  try try
    SkipLabels:=True;
    Form := DSR.Form;
    ParentForm := DSP.Form;
    DataSet := DSR.DataSet;
    Ex := Build(Expr);
    if Ex <> nil then
      V := Ex.Calc;
    Result := True;
  except
    on E: Exception do
      ErrMsg(Format(rsErrorInExpr, [LineEnding + LineEnding + E.Message]));
  end;
  finally
    Free;
    FreeAndNil(Ex);
  end;
end;

{ TClearFieldsAction }

procedure TClearFieldsAction.InnerExecute;
var
  Fm: TdxForm;
  DS: TSQLQuery;
  i: Integer;
  C: TComponent;
begin
  inherited InnerExecute;
  with TDataSetProcessor(DSProc).DataSets[DSRi]^ do
  begin
    Fm := Form; DS := DataSet;
  end;

  if not (DS.State in [dsInsert, dsEdit]) then Exit;

  for i := 0 to FFields.Count - 1 do
  begin
    C := FindById(Fm, FFields[i]);
    if C = nil then Continue;
    if C is TdxDBImage then TdxDBImage(C).Clear
    else if C is TdxFile then TdxFile(C).Clear
    else if C is TdxLookupComboBox then TdxLookupComboBox(C).Clear
    else DS.FieldByName(FieldStr(C)).SetData(nil);
  end;
end;

constructor TClearFieldsAction.Create;
begin
  inherited Create;
  FFields := TIntList.Create;
end;

destructor TClearFieldsAction.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

{ TCallFuncAction }

procedure TCallFuncAction.InnerExecute;
var
  DSP: TDataSetProcessor;
  V: Variant;
begin
  inherited InnerExecute;
  DSP := TDataSetProcessor(FDSProc);
  if Trim(FExpr) <> '' then
    CalcExpr(FExpr, DSP, DSRi, V);
end;

{ TUserMonitorAction }

procedure TUserMonitorAction.InnerExecute;
begin
  inherited InnerExecute;
  MonitorFm.ShowForm;
end;

{ TOpenReportAction }

procedure TOpenReportAction.InnerExecute;
begin
  if not UserMan.CheckRpVisible(FRpId) then Exit;
  if ReportMan.FindReport(FRpId) <> nil then ShowReportWindow(FRpId);
end;

{ TMassCalcAction }

procedure TMassCalcAction.InnerExecute;
var
  DSP: TDataSetProcessor;
  Fm: TdxForm;
  DSR: TDataSetRec;
  Wh: String;
begin
  Fm := FormMan.FindForm(FFormId);
  if Fm = nil then Exit;
  if FindById(Fm, FieldId) = nil then Exit;
  DSP := TDataSetProcessor(FDSProc);
  DSR := DSP.DataSets[FDSRi]^;
  Wh := SqlSelCondFilter2(Fm, FFilter, DSR.Form, DSP.Form, DSR.DataSet);
  with TDataSetProcessor.Create do
  try try
    BindForm(Fm.Id, False, vtDefault);
    OpenWhere(Wh);
    Recalculate(FFormId, FFieldId, FExpression);
  except
    on E: Exception do
      ErrMsg(E.Message);
  end;
  finally
    Free;
  end;
end;

{ TPrintAction }

procedure TPrintAction.InnerExecute;
var
  DSP: TDataSetProcessor;
  S: String;
  V: Variant;
begin
  if FDSRi > 0 then Exit;
  DSP := TDataSetProcessor(FDSProc);
  S := Trim(FTemplateFile);
  if (S = '') and (Trim(FExpression) <> '') then
  begin
    if CalcExpr(FExpression, DSP, DSRi, V) then
      S := VarToStr(V);
  end;
  if S <> '' then
  begin
    DSP.Print(GetTemplatesDir + S);
  end;
end;

{ TGotoFormAction }

procedure TGotoFormAction.InnerExecute;
var
  DSP: TDataSetProcessor;
begin
  DSP := TDataSetProcessor(FDSProc);
  if (DSP.GotoEnable) and (DSP.Form.ViewType <> vtGridOnly) and (FormMan.FindForm(FFormId) <> nil) then
  begin
    MainFr.GotoRec(FFormId, 0);
  end;
end;

{ TActionReader }

procedure TActionReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  SL: TStringList;
  i: Integer;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  with FAction do
  begin
    ExecCond := XmlToStr(Atts.GetValue('', 'execcond'));
    ConfirmMsg := XmlToStr(Atts.GetValue('', 'confirm'));
    Msg := XmlToStr(Atts.GetValue('', 'msg'));
    SaveRecord:=Str2Bool(Atts.GetValue('', 'saverecord'));
  end;
  if FAction is TGotoFormAction then
    with TGotoFormAction(FAction) do
    begin
      FormId := GetInt(Atts, 'formid');
    end
  else if FAction is TPrintAction then
    with TPrintAction(FAction) do
    begin
      TemplateFile := Atts.GetValue('', 'template');
      Expression := XmlToStr(Atts.GetValue('', 'expression'));
    end
  else if FAction is TMassCalcAction then
    with TMassCalcAction(FAction) do
    begin
      FormId := GetInt(Atts, 'formid');
      Filter := XmlToStr(Atts.GetValue('', 'filter'));
      FieldId := GetInt(Atts, 'fieldid');
      Expression := XmlToStr(Atts.GetValue('', 'expression'));
    end
  else if FAction is TOpenReportAction then
    with TOpenReportAction(FAction) do
    begin
      RpId := GetInt(Atts, 'rpid');
    end
  else if FAction is TCallFuncAction then
    with TCallFuncAction(FAction) do
    begin
      Expression := XmlToStr(Atts.GetValue('', 'expression'))
    end
	else if FAction is TClearFieldsAction then
    with TClearFieldsAction(FAction) do
    begin
      SL := TStringList.Create;
      SplitStr(Atts.GetValue('', 'fields'), ';', SL);
      for i := 0 to SL.Count - 1 do
	      Fields.AddValue(StrToInt(SL[i]));
      SL.Free;
    end;
end;

procedure TActionReader.ParseXml(const Xml: String);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(Xml);
  ParseStream(SS);
  SS.Free;
end;

{ TBasicAction }

procedure TBasicAction.InnerExecute;
begin

end;

constructor TBasicAction.Create;
begin

end;

procedure TBasicAction.Load(const Xml: String);
begin
  with TActionReader.Create do
  begin
    Action := Self;
    ParseXml(Xml);
    Free;
  end;
end;

procedure TBasicAction.Execute;
var
  DSP: TDataSetProcessor;
  V: Variant;
  S: String;
begin
  DSP := TDataSetProcessor(FDSProc);
  if Trim(FExecCond) <> '' then
  begin
    if CalcExpr(FExecCond, DSP, DSRi, V) then
    begin
      S := VarToStr(V);
      if S <> '' then
      begin
        MessageDlg(rsWarning, S, mtWarning, [mbOk], 0);
        Exit;
      end;
    end
    else Exit;
  end;

  if FConfirmMsg <> '' then
  begin
    if MessageDlg(rsWarning, FConfirmMsg, mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit;
  end;

  if (DSRi = 0) and FSaveRecord then
  begin
    if DSP.MasterSet.State in [dsInsert, dsEdit] then
    begin
      if not DSP.Validate(DSRi) then Exit;
      DSP.Post;
      DSP.MasterSet.Edit;
      if DSP.MasterSet.State <> dsEdit then Exit;
    end;
  end;

  try
    InnerExecute;

    if FMsg <> '' then
      MessageDlg(rsWarning, FMsg, mtInformation, [mbOk], 0)
  except
    on E: Exception do
      ErrMsg(E.Message);
  end;
end;

end.

