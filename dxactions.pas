unit DxActions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, strconsts, mytypes, sqldb, Graphics,
  uPSDebugger, Dialogs;

type
  TdxActionType = (actNone, actGotoForm, actPrint, actMassCalc, actOpenReport,
    actSaveChanges, actUserMonitor, actCallFunc, actClearFields, actCustom, actShowMessage);

  //T2DVariant = array of array of Variant;

  { TActionProp }

  TActionProp = class
  public
    Name: String;
    Value: String;
    //Values: T2DVariant;
  end;

  { TActionProps }

  TActionProps = class(TList)
  private
    function GetProps(Index: Integer): TActionProp;
  public
    function AddProp: TActionProp;
    function Find(const AName: String): TActionProp;
    procedure Clear; override;
    property Props[Index: Integer]: TActionProp read GetProps; default;
  end;

  TBaseActionClass = class of TBaseAction;

  { TBaseAction }

  TBaseAction = class
  private
    FActionType: TdxActionType;
    FDisabled: Boolean;
    //FConfirmMsg: String;
    FDSProc: TObject;
    FDSRi: Integer;
    //FExecCond: String;
    //FTmpForm: TObject;
    //FMsg: String;
    //FSaveRecord: Boolean;
  protected
    function GetActionName: String; virtual;
    function InnerExecute: Variant; virtual;
  public
    constructor Create; virtual;
    //procedure Load(const Xml: String); virtual;
    function Execute: Variant;
    procedure RenameForm(const OldName, NewName: String); virtual;
    procedure RenameField(CurFm: TObject; const FormName, OldName, NewName: String); virtual;
    procedure RenameComponent(CurFm: TObject; const FormName, OldName, NewName: String); virtual;
    procedure RenameQuery(const OldName, NewName: String); virtual;
    procedure RenameReport(const OldName, NewName: String); virtual;
    procedure RenameRpField(RD: TObject; const OldName, NewName: String); virtual;
    procedure RenameImage(const OldName, NewName: String); virtual;
    function FormExists(aName: String): Boolean; virtual;
    function FieldExists(CurFm: TObject; const FormName, FieldName: String): Boolean; virtual;
    function ObjectExists(CurFm: TObject; const FormName, FieldName: String): Boolean; virtual;
    function ComponentExists(CurFm: TObject; const FormName, CmpName: String): Boolean; virtual;
    function QueryExists(aName: String): Boolean; virtual;
    function ReportExists(aName: String): Boolean; virtual;
    function RpFieldExists(RD: TObject; const FieldName: String): Boolean; virtual;
    function TemplateExists(const FileName: String): Boolean; virtual;
    function ImageExists(const ImageName: String): Boolean; virtual;
    function ValueExists(AValue: String): Boolean; virtual;
    property ActionName: String read GetActionName;
    property ActionType: TdxActionType read FActionType;
    //property ExecCond: String read FExecCond write FExecCond;
    //property ConfirmMsg: String read FConfirmMsg write FConfirmMsg;
    //property Msg: String read FMsg write FMsg;
    //property SaveRecord: Boolean read FSaveRecord write FSaveRecord;
    property DSProc: TObject read FDSProc write FDSProc;
    property DSRi: Integer read FDSRi write FDSRi;
    //property TmpForm: TObject read FTmpForm write FTmpForm;
    property Disabled: Boolean read FDisabled write FDisabled;
  end;

  { TGotoFormAction }

  TGotoFormAction = class(TBaseAction)
  private
    //FFormId: Integer;
    FFormName: String;
  protected
    function InnerExecute: Variant; override;
  public
    //property FormId: Integer read FFormId write FFormId;
    procedure RenameForm(const OldName, NewName: String); override;
    function FormExists(aName: String): Boolean; override;
    function ValueExists(AValue: String): Boolean; override;
    property FormName: String read FFormName write FFormName;
  end;

  { TPrintAction }

  TPrintFileAction = (pfaNone, pfaOpen, pfaPrint);

  TPrintAction = class(TBaseAction)
  private
    FExpression: String;
    FFileAction: TPrintFileAction;
    FOutFile: String;
    FSaveRecord: Boolean;
    FTemplateFile: String;
  protected
    function InnerExecute: Variant; override;
  public
    constructor Create; override;
    function TemplateExists(const FileName: String): Boolean; override;
    function ValueExists(AValue: String): Boolean; override;
    property TemplateFile: String read FTemplateFile write FTemplateFile;
    property Expression: String read FExpression write FExpression;
    property OutFile: String read FOutFile write FOutFile;
    property SaveRecord: Boolean read FSaveRecord write FSaveRecord;
    property FileAction: TPrintFileAction read FFileAction write FFileAction;
  end;

  { TMassCalcAction }

  TMassCalcAction = class(TBaseAction)
  private
    FExpression: String;
    FFieldName: String;
    FFilter: String;
    FFormName: String;
    FTableName: String;
  protected
    function InnerExecute: Variant; override;
  public
    procedure RenameForm(const OldName, NewName: String); override;
    procedure RenameField(CurFm: TObject; const aFormName, OldName, NewName: String);
      override;
    function FormExists(aName: String): Boolean; override;
    function FieldExists(CurFm: TObject; const aFormName, aFieldName: String): Boolean;
      override;
    function ValueExists(AValue: String): Boolean; override;
    property FormName: String read FFormName write FFormName;
    property Filter: String read FFilter write FFilter;
    property TableName: String read FTableName write FTableName;
    property FieldName: String read FFieldName write FFieldName;
    property Expression: String read FExpression write FExpression;
  end;

  { TOpenReportAction }

  TOpenReportAction = class(TBaseAction)
  private
    FRpName: String;
  protected
    function InnerExecute: Variant; override;
  public
    procedure RenameReport(const OldName, NewName: String); override;
    function ReportExists(aName: String): Boolean; override;
    function ValueExists(AValue: String): Boolean; override;
    property RpName: String read FRpName write FRpName;
  end;

  { TSaveChangesAction }

  TSaveChangesAction = class(TBaseAction)
  protected
  	function InnerExecute: Variant; override;
  end;

  { TUserMonitorAction }

  TUserMonitorAction = class(TBaseAction)
  protected
    function InnerExecute: Variant; override;
  end;

  { TCallFuncAction }

  TCallFuncAction = class(TBaseAction)
  private
    FExpr: String;
  protected
    function InnerExecute: Variant; override;
  public
    function ValueExists(AValue: String): Boolean; override;
    property Expression: String read FExpr write FExpr;
  end;

  { TClearFieldsAction }

  TClearFieldsAction = class(TBaseAction)
  private
    FFields: TStringList;
  protected
    function InnerExecute: Variant; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RenameField(CurFm: TObject; const FormName, OldName, NewName: String);
      override;
    function FieldExists(CurFm: TObject; const FormName, FieldName: String): Boolean;
      override;
    function ValueExists(AValue: String): Boolean; override;
  	property Fields: TStringList read FFields;
  end;

  { TShowMessageAction }

  TShowMessageAction = class(TBaseAction)
  private
    FButtons: TMsgDlgButtons;
    FExprMsg: String;
    FMessage: String;
    FTitle: String;
    FMsgType: TMsgDlgType;
  protected
    function InnerExecute: Variant; override;
  public
    constructor Create; override;
    function ValueExists(AValue: String): Boolean; override;
  	property Title: String read FTitle write FTitle;
    property Message: String read FMessage write FMessage;
    property ExprMsg: String read FExprMsg write FExprMsg;
    property MsgType: TMsgDlgType read FMsgType write FMsgType;
    property Buttons: TMsgDlgButtons read FButtons write FButtons;
  end;

  { TActionCustom }

  TActionCustom = class(TBaseAction)
  private
    FActionId: String;
  	FProps: TActionProps;
  protected
    function GetActionName: String; override;
    function InnerExecute: Variant; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RenameForm(const OldName, NewName: String); override;
    procedure RenameField(CurFm: TObject; const FormName, OldName, NewName: String); override;
    procedure RenameComponent(CurFm: TObject; const FormName, OldName, NewName: String); override;
    procedure RenameQuery(const OldName, NewName: String); override;
    procedure RenameReport(const OldName, NewName: String); override;
    procedure RenameRpField(RD: TObject; const OldName, NewName: String); override;
    procedure RenameImage(const OldName, NewName: String); override;
    function FormExists(aName: String): Boolean; override;
    function FieldExists(CurFm: TObject; const FormName, FieldName: String): Boolean; override;
    function ObjectExists(CurFm: TObject; const FormName, FieldName: String): Boolean; override;
    function ComponentExists(CurFm: TObject; const FormName, CmpName: String): Boolean; override;
    function QueryExists(aName: String): Boolean; override;
    function ReportExists(aName: String): Boolean; override;
    function RpFieldExists(RD: TObject; const FieldName: String): Boolean; override;
    function TemplateExists(const FileName: String): Boolean; override;
    function ImageExists(const ImageName: String): Boolean; override;
    function ValueExists(AValue: String): Boolean; override;
    property Props: TActionProps read FProps;
    property ActionId: String read FActionId write FActionId;
  end;

  TActionLineKind = (alkNone, alkAction, alkIf, alkElseIf, alkElse, alkComment);

  TActionLines = class;

  { TActionLine }

  TActionLine = class
  public
    Kind: TActionLineKind;
    Action: TBaseAction;
    Cond, Text: String;
    Lines: TActionLines;
    constructor Create;
    destructor Destroy; override;
  end;

  { TActionLines }

  TActionLines = class(TList)
  private
    function GetLines(Index: Integer): TActionLine;
  public
  	function AddLine(NextLine: Pointer = nil): TActionLine;
    procedure DeleteLine(L: TActionLine);
    procedure Clear; override;
    property Lines[Index: Integer]: TActionLine read GetLines; default;
  end;

  { TActionRunner }

  TActionRunner = class
  private
    FDSProc: TObject;
    FDSRi: Integer;
  	FLines: TActionLines;
    function SaveAction(A: TBaseAction): String;
    function SaveLine(ALine: TActionLine): String;
    function SaveLines(Lines: TActionLines): String;
    procedure RunLines(ALines: TActionLines);
  public
    constructor Create;
    destructor Destroy; override;
  	procedure Load(const Xml: String);
    procedure Save(var Xml: String);
    function Run: Variant;
    property Lines: TActionLines read FLines;
    property DSProc: TObject read FDSProc write FDSProc;
    property DSRi: Integer read FDSRi write FDSRi;
  end;

function CreateAction(act: TdxActionType): TBaseAction;
function ActionTypeToStr(act: TdxActionType): String;

implementation

uses
  SAX, saxbasereader, datasetprocessor, dxctrls, mainframe, apputils,
  expressions, variants, formmanager, sqlgen, dxusers, reportmanager,
  ReportWindow, Db, monitorform, dximages, dxfiles,
  uPSRuntime, uPSUtils, Math, dxreports, LazUtf8, scriptmanager, errorsform,
  LazFileUtils;

type

  { TActionReader }

  {TActionReader = class(TSAXBaseReader)
  private
    FAction: TBaseAction;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
  public
    property Action: TBaseAction read FAction write FAction;
    procedure ParseXml(const Xml: String);
  end;    }

  { TActionLinesReader }

  TActionLinesReader = class(TSAXBaseReader)
  private
    FActionRunner: TActionRunner;
    FLines: TActionLines;
    FStack: TList;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
    procedure DoEndElement(const NamespaceURI, LocalName, QName: SAXString); override;
  public
    property ActionRunner: TActionRunner read FActionRunner write FActionRunner;
    procedure ParseXml(const Xml: String);
  end;

function CreateAction(act: TdxActionType): TBaseAction;
begin
  Result := nil;
  case act of
    actCustom: Result := TActionCustom.Create;
    actGotoForm: Result := TGotoFormAction.Create;
    actPrint: Result := TPrintAction.Create;
    actMassCalc: Result := TMassCalcAction.Create;
    actOpenReport: Result := TOpenReportAction.Create;
    actSaveChanges: Result := TSaveChangesAction.Create;
    actUserMonitor: Result := TUserMonitorAction.Create;
    actCallFunc: Result := TCallFuncAction.Create;
    actClearFields: Result := TClearFieldsAction.Create;
    actShowMessage: Result := TShowMessageAction.Create;
  end;
  Result.FActionType := act;
end;

function CalcExpression(const Expr: String; DSP: TDataSetProcessor; DSRi: Integer; var V: Variant): Boolean;
var
  DSR: TDataSetRec;
  Ex: TExpression;
begin
  Result := False;
  DSR := DSP.DataSets[DSRi]^;
  Ex := nil;
  with TExpressionBuilder.Create do
  try
    SkipLabels:=True;
    Form := DSR.Form;
    ParentForm := DSP.Form;
    DataSet := DSR.DataSet;
    Ex := Build(Expr);
    if Ex <> nil then
      V := Ex.Calc;
    Result := True;
  finally
    Free;
    FreeAndNil(Ex);
  end;
end;

function ActionTypeToStr(act: TdxActionType): String;
var
  S: String;
begin
  S := '';
  case act of
    actGotoForm: S := rsGotoForm;
    actPrint: S := rsPrint;
    actMassCalc: S := rsMassCalc;
    actOpenReport: S := rsOpenReport;
    actSaveChanges: S := rsSaveChanges;
    actUserMonitor: S := rsUserMonitor;
    actCallFunc: S := rsCallFunction;
    actClearFields: S := rsClearFields;
    actShowMessage: S := rsShowMessage;
  end;
  Result := S;
end;

{ TShowMessageAction }

function TShowMessageAction.InnerExecute: Variant;
var
  S: String;
  V: Variant;
begin
  if Trim(FExprMsg) = '' then S := FMessage
  else
  begin
    CalcExpression(FExprMsg, TDataSetProcessor(FDSProc), FDSRi, V);
    S := VarToStr(V);
  end;
  Result := MessageDlg(FTitle, S, FMsgType, FButtons, 0);
end;

constructor TShowMessageAction.Create;
begin
  inherited Create;
  FMsgType := mtWarning;
  FButtons := [mbOk];
end;

function TShowMessageAction.ValueExists(AValue: String): Boolean;
begin
  AValue := Utf8LowerCase(AValue);
  Result := (Utf8Pos(AValue, Utf8LowerCase(FTitle), 1) > 0) or
    (Utf8Pos(AValue, Utf8LowerCase(FMessage), 1) > 0) or
    (Utf8Pos(AValue, Utf8LowerCase(FExprMsg), 1) > 0);
end;

{ TActionLinesReader }

procedure TActionLinesReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  SL: TStringList;
  i: Integer;
  AttName: SAXString;
  Pm: TActionProp;
  TagName: String;
  act: TdxActionType;
  A: TBaseAction;
  Line: TActionLine;
  B: TMsgDlgButtons;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  TagName := AnsiLowerCase(LocalName);
  if TagName = 'action' then
  begin
    act := TdxActionType(GetInt(Atts, 'type'));
    if act = actNone then Exit;
    A := CreateAction(act);
    A.Disabled := GetBool(Atts, 'disabled');
    A.DSProc := FActionRunner.DSProc;
    A.DSRi := FActionRunner.DSRi;
    Line := FLines.AddLine;
    Line.Kind:=alkAction;
    Line.Action := A;
  end
  else if TagName = 'if' then
  begin
    Line := FLines.AddLine;
    Line.Kind := alkIf;
    Line.Cond := XmlToStr(GetStr(Atts, 'cond'));
    FStack.Add(FLines);
    FLines := Line.Lines;
  end
  else if TagName = 'elseif' then
  begin
    Line := FLines.AddLine;
    Line.Kind := alkElseIf;
    Line.Cond := XmlToStr(GetStr(Atts, 'cond'));
    FStack.Add(FLines);
    FLines := Line.Lines;
  end
  else if TagName = 'else' then
  begin
    Line := FLines.AddLine;
    Line.Kind := alkElse;
    FStack.Add(FLines);
    FLines := Line.Lines;
  end
  else if TagName = 'comment' then
  begin
    Line := FLines.AddLine;
    Line.Kind := alkComment;
    Line.Text := XmlToStr(GetStr(Atts, 'text'));
  end;

  if TagName <> 'action' then Exit;

  if A is TGotoFormAction then
    with TGotoFormAction(A) do
    begin
      FormName := XmlToStr(GetStr(Atts, 'form'));
    end
  else if A is TPrintAction then
    with TPrintAction(A) do
    begin
      TemplateFile := XmlToStr(Atts.GetValue('', 'template'));
      Expression := XmlToStr(Atts.GetValue('', 'expression'));
      OutFile := XmlToStr(Atts.GetValue('', 'outfile'));
      if AttrExists(Atts, 'fileaction') then
        FileAction := TPrintFileAction(GetInt(Atts, 'fileaction'));
      SaveRecord := GetBool(Atts, 'saverecord');
    end
  else if A is TMassCalcAction then
    with TMassCalcAction(A) do
    begin
      FormName := XmlToStr(GetStr(Atts, 'form'));
      Filter := XmlToStr(Atts.GetValue('', 'filter'));
      TableName := XmlToStr(GetStr(Atts, 'table'));
      FieldName := XmlToStr(GetStr(Atts, 'field'));
      Expression := XmlToStr(Atts.GetValue('', 'expression'));
    end
  else if A is TOpenReportAction then
    with TOpenReportAction(A) do
    begin
      RpName := XmlToStr(GetStr(Atts, 'rp'));
    end
  else if A is TCallFuncAction then
    with TCallFuncAction(A) do
    begin
      Expression := XmlToStr(Atts.GetValue('', 'expression'))
    end
	else if A is TClearFieldsAction then
    with TClearFieldsAction(A) do
    begin
      SplitStr(XmlToStr(Atts.GetValue('', 'fields')), ';', Fields);
    end
  else if A is TShowMessageAction then
  	with TShowMessageAction(A) do
    begin
      Title := XmlToStr(GetStr(Atts, 'title'));
      Message := XmlToStr(GetStr(Atts, 'message'));
      ExprMsg := XmlToStr(GetStr(Atts, 'exprmsg'));
      MsgType := TMsgDlgType(GetInt(Atts, 'msgtype'));
      SL := TStringList.Create;
      SplitStr(GetStr(Atts, 'buttons'), ';', SL);
      B := [];
      if SL[0] = '1' then Include(B, mbOk);
      if SL[1] = '1' then Include(B, mbCancel);
      if SL[2] = '1' then Include(B, mbAbort);
      if SL[3] = '1' then Include(B, mbRetry);
      if SL[4] = '1' then Include(B, mbIgnore);
      if SL[5] = '1' then Include(B, mbYes);
      if SL[6] = '1' then Include(B, mbNo);
      if SL[7] = '1' then Include(B, mbAll);
      if SL[8] = '1' then Include(B, mbNoToAll);
      if SL[9] = '1' then Include(B, mbYesToAll);
      if SL[10] = '1' then Include(B, mbClose);
      SL.Free;
      Buttons := B;
    end
  else if A is TActionCustom then
  begin
  	TActionCustom(A).ActionId := GetStr(Atts, 'id');
    for i := 0 to Atts.Length - 1 do
    begin
      AttName := Atts.GetLocalName(i);
      if (AttName = 'type') or (AttName = 'id') then Continue;
      Pm := TActionCustom(A).Props.AddProp;
      Pm.Name := AttName;
      Pm.Value := XmlToStr(Atts.GetValue(i));
    end;
  end;
end;

procedure TActionLinesReader.DoEndElement(const NamespaceURI, LocalName,
  QName: SAXString);
var
  TagName: String;
  i: Integer;
begin
  inherited DoEndElement(NamespaceURI, LocalName, QName);
  TagName := AnsiLowerCase(LocalName);
  if (TagName = 'if') or (TagName = 'elseif') or (TagName = 'else') then
    if FStack.Count > 0 then
    begin
      i := FStack.Count - 1;
	    FLines := TActionLines(FStack[i]);
      FStack.Delete(i);
    end;
end;

procedure TActionLinesReader.ParseXml(const Xml: String);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(Xml);
  FLines := FActionRunner.Lines;
  FStack := TList.Create;
  try
	  ParseStream(SS);
  finally
  	FStack.Free;
    SS.Free;
  end;
end;

{ TActionLine }

constructor TActionLine.Create;
begin
  Lines := TActionLines.Create;
end;

destructor TActionLine.Destroy;
begin
  if Action <> nil then Action.Free;
  Lines.Free;
  inherited Destroy;
end;

{ TActionLines }

function TActionLines.GetLines(Index: Integer): TActionLine;
begin
  Result := TActionLine(Items[Index]);
end;

function TActionLines.AddLine(NextLine: Pointer): TActionLine;
var
  i: Integer;
begin
	Result := TActionLine.Create;
  if NextLine = nil then
  	Add(Result)
  else
  begin
	  i := IndexOf(NextLine);
  	Insert(i, Result);
  end;
end;

procedure TActionLines.DeleteLine(L: TActionLine);
begin
  Remove(L);
  L.Free;
end;

procedure TActionLines.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Lines[i].Free;
  inherited Clear;
end;

{ TActionRunner }

function SaveGotoFormAction(A: TGotoFormAction): String;
begin
  Result := 'form="' +  StrToXml(A.FormName) + '"';
end;

function SavePrintAction(A: TPrintAction): String;
begin
  Result:='template="' + StrToXml(A.TemplateFile) +
    '" expression="' + StrToXml(A.Expression) +
    '" outfile="' + StrToXml(A.OutFile) +
    '" fileaction="' + IntToStr(Ord(A.FileAction)) +
    '" saverecord="' + Bool2Str(A.SaveRecord) + '"';
end;

function SaveMassCalcAction(A: TMassCalcAction): String;
begin
  Result := 'form="' + StrToXml(A.FormName) + '" filter="' +
    StrToXml(A.Filter) + '" table="' + StrToXml(A.TableName) + '" field="' +
    StrToXml(A.FieldName) + '" expression="' + StrToXml(A.Expression) + '"';
end;

function SaveOpenReportAction(A: TOpenReportAction): String;
begin
  Result := 'rp="' + StrToXml(A.RpName) + '"';
end;

function SaveCallFuncAction(A: TCallFuncAction): String;
begin
  Result:='expression="' + StrToXml(A.Expression) + '"';
end;

function SaveClearFieldsAction(A: TClearFieldsAction): String;
var
  i: Integer;
  S: String;
begin
  S := '';
  for i := 0 to A.Fields.Count - 1 do
  begin
    S := S + A.Fields[i];
    if i < A.Fields.Count - 1 then
    	S := S + ';'
  end;
  Result := 'fields="' + StrToXml(S) + '"';
end;

function SaveShowMessageAction(A: TShowMessageAction): String;
var
  S, B: String;
begin
  S := 'title="' + StrToXml(A.Title) + '" message="' + StrToXml(A.Message) +
  	'" msgtype="' + IntToStr(Ord(A.MsgType)) + '" exprmsg="' +
    StrToXml(A.ExprMsg) + '" buttons="';
  B := '';
  if mbOk in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbCancel in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbAbort in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbRetry in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbIgnore in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbYes in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbNo in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbAll in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbNoToAll in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbYesToAll in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbClose in A.Buttons then B := B + '1' else B := B + '0';
  Result := S + B + '"';
end;

function SaveCustomAction(A: TACtionCustom): String;
var
  i: Integer;
  P: TActionProp;
  S: String;
begin
  S := 'id="' + A.ActionId + '" ';
  for i := 0 to A.Props.Count - 1 do
  begin
    P := A.Props[i];
    S := S + AnsiLowerCase(P.Name) + '="' + StrToXml(P.Value) + '" ';
  end;
  Result := S;
end;

function TActionRunner.SaveAction(A: TBaseAction): String;
var
  S: String;
begin
  S := '';
  case A.ActionType of
		actGotoForm: S := SaveGotoFormAction(TGotoFormAction(A));
    actPrint: S := SavePrintAction(TPrintAction(A));
    actMassCalc: S := SaveMassCalcAction(TMassCalcAction(A));
    actOpenReport: S := SaveOpenReportAction(TOpenReportAction(A));
    actCallFunc: S := SaveCallFuncAction(TCallFuncAction(A));
    actClearFields: S := SaveClearFieldsAction(TClearFieldsAction(A));
    actShowMessage: S := SaveShowMessageAction(TShowMessageAction(A));
    actCustom: S := SaveCustomAction(TActionCustom(A));
  end;
  Result := '<action ';
  if A.Disabled then Result := Result + 'disabled="1" ';
  Result := Result + 'type="' + IntToStr(Ord(A.ActionType)) + '" ' + S + '/>';
end;

function TActionRunner.SaveLine(ALine: TActionLine): String;
begin
  case ALine.Kind of
    alkAction: Result := SaveAction(ALine.Action);
    alkIf: Result := '<if cond="' + StrToXml(ALine.Cond) + '">' + SaveLines(ALine.Lines) +
  		'</if>';
    alkElseIf: Result := '<elseif cond="' + StrToXml(ALine.Cond) + '">' + SaveLines(ALine.Lines) +
  		'</elseif>';
    alkElse: Result := '<else>' + SaveLines(ALine.Lines) + '</else>';
    alkComment: Result := '<comment text="' + StrToXml(ALine.Text) + '"/>';
  end;
end;

function TActionRunner.SaveLines(Lines: TActionLines): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Lines.Count - 1 do
  	Result := Result + SaveLine(Lines[i]);
end;

procedure TActionRunner.RunLines(ALines: TActionLines);
var
  i: Integer;
  L: TActionLine;
  Cond: Variant;
  SkipCond: Boolean;
  DSP: TDataSetProcessor;
  Fm: TdxForm;
begin
  DSP := TDataSetProcessor(FDSProc);
  Fm := DSP.DataSets[FDSRi]^.Form;
  for i := 0 to ALines.Count - 1 do
  begin
    L := ALines[i];

    if not (L.Kind in [alkElseIf, alkElse]) then SkipCond := False;

    if L.Kind = alkAction then
    begin
      if not L.Action.Disabled then
        Fm.ActionResult := L.Action.Execute;
    end
    else if (L.Kind in [alkIf, alkElseIf]) and (not SkipCond) then
    begin
      if CalcExpression(L.Cond, DSP, FDSRi, Cond) and VarIsBool(Cond) and
      	(Cond = True) then
      begin
        SkipCond := True;
        RunLines(L.Lines);
      end;
    end
    else if (L.Kind = alkElse) and (not SkipCond) then
    	RunLines(L.Lines);
  end;

end;

constructor TActionRunner.Create;
begin
  FLines := TActionLines.Create;
end;

destructor TActionRunner.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TActionRunner.Load(const Xml: String);
begin
	Lines.Clear;
  with TActionLinesReader.Create do
  try
     ActionRunner := Self;
     ParseXml(Xml);
  finally
    Free;
  end;
end;

procedure TActionRunner.Save(var Xml: String);
var
  S: String;
begin
  Xml := '';
  S := SaveLines(FLines);
  if S <> '' then
	  Xml := '<actions>' + S + '</actions>';
end;

function TActionRunner.Run: Variant;
var
  Fm: TdxForm;
  OldResult: Variant;
begin
  Fm := TDataSetProcessor(FDSProc).DataSets[FDSRi]^.Form;
  OldResult := Fm.ActionResult;
  Fm.ActionResult := Null;

  try try
    RunLines(FLines);
    Result := Fm.ActionResult;
  except
    on E: Exception do
      ErrMsg(E.Message, True, 'ActionRun');
  end;
  finally
    Fm.ActionResult := OldResult;
  end;
end;

{ TSaveChangesAction }

function TSaveChangesAction.InnerExecute: Variant;
var
  DSP: TDataSetProcessor;
  DSR: TDataSetRec;
begin
  Result := False;
  DSP := TDataSetProcessor(FDSProc);
  DSR := DSP.DataSets[DSRi]^;
  if DSR.DataSet.State in [dsInsert, dsEdit] then
  begin
    if not DSP.Validate(DSRi) then Exit;
    if DSRi = 0 then
    begin
	    DSP.Post;
  	  if DSP.InnerEdit(0, True, True, False) <> asOk then Exit;
    end
    else
    begin
      DSR.DataSet.Post;
      DSR.DataSet.Edit;
    end;
    Result := True;
  end;
end;

{ TActionCustom }

function TActionCustom.GetActionName: String;
var
  EA: TExprAction;
begin
  Result := '';
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then Result := EA.Name;
end;

function TActionCustom.InnerExecute: Variant;
var
  EAction: TExprAction;
  Params: TPSList;
  i, j, z, MaxCol, idx, n: Integer;
  EAC: TEAControl;
  P: PPSVariant;
  S: string;
  Prop: TActionProp;
  Rows, Cols, Titles: TStringList;
  Ex: TPSDebugExec;
  Tmp: PIFTypeRec;
  pV: PIFVariant;
  OldSelf: TObject;
  ProcNo: Cardinal;
  Proc: TPSInternalProcRec;
  Res: TbtString;
  Num: Longint;
  SD: TScriptData;
  Fm: TdxForm;
begin
  inherited InnerExecute;
  EAction := ScriptMan.Actions.FindAction(FActionId);
  if EAction = nil then Exit;

  Params := TIfList.Create;

  try //try

  // Сохраняем текущее значение Self
  SD := ScriptMan.Scripts[EAction.SDi];
  Ex := ExtRunMan.GetExec(SD);

  //pV := ExprModule.Exec.GetVar2('Self');
  pV := Ex.GetVar2('Self');
  if pV = nil then raise ECalcError.CreateFmt(rsExecActionFailedMsg, [EAction.Name, SD.Name]);
	OldSelf := PSGetObject(@PPSVariantData(pV)^.Data, pV^.FType);
  Fm := TDataSetProcessor(DSProc).DataSets[DSRi]^.Form;
  if Fm.Id > 0 then
    SetVariantToClass(pV, Fm)
  // Dummy form для Main
  else
    SetVariantToClass(pV, nil);
  //

  //Ex := ExprModule.Exec;
  n := EAction.Controls.GetParamCount;
  for i := EAction.Controls.Count - 1 downto 0 do
  begin
    EAC := EAction.Controls[i];
    if EAC.ControlType = eacDivider then Continue;
    Prop := FProps.Find(EAC.Name);
    if Prop <> nil then S := Prop.Value
    else S := '';

    case EAC.ControlType of
      eacNumber, eacCheckBox:
        begin
	      	P := CreateHeapVariant(Ex.FindType2(btS32));
          if TryStrToInt(S, Num) then
          	VSetInt(P, Num);
        end;
      eacColor:
        begin
      	  P := CreateHeapVariant(Ex.FindType2(btS32));
          if not TryStrToColor(S, Num) then Num := clNone;
      	  VSetInt(P, Num);
        end;
      eacGrid:
        begin
          Tmp := Ex.GetTypeNo(Ex.GetType('TVARIANTARRAY2D'));
          //if Tmp = nil then raise Exception.CreateFmt(rsParamNoTVariantArray2d,
          //	[EAction.OrigName, n]);
          if Tmp <> nil then
  	      	P := CreateHeapVariant(Tmp)
          else
            P := CreateHeapVariant(Ex.FindType2(btS32));

          if (S <> '') and (Tmp <> nil) then
          begin

            Rows := TStringList.Create;
            Cols := TStringList.Create;
            Titles := TStringList.Create;

            SplitStr(S, '|', Rows);
            PSDynArraySetLength(PPSVariantDynamicArray(P)^.Data,
          	  PPSVariantDynamicArray(P)^.VI.FType, Rows.Count - 1);

            SplitStr(Rows[0], ';', Titles);
            Rows.Delete(0);	// Удаляем заголовок

            // При заполнении массива учитывает случаи, когда количество
            // и порядок столбцов меняется.
            MaxCol := Min(EAC.Controls.Count, Titles.Count);

            for j := 0 to Rows.Count - 1 do
            begin
              SplitStr(Rows[j], ';', Cols);
              // В случае с одной колонкой и пустой строкой надо добавлять один
              // элемент, чтобы не было ошибки index of bounds.
              if Cols.Count = 0 then Cols.Add('');

              SetLength(TVariantArray2d(PPSVariantDynamicArray(P)^.Data)[j], EAC.Controls.Count);

              // Если не будут проинициализированы ВСЕ элементы, то
              // в скрипте массив будет пустым.
              for z := 0 to EAC.Controls.Count - 1 do
              	TVariantArray2d(PPSVariantDynamicArray(P)^.Data)[j][z] := Null;

						  for z := 0 to MaxCol - 1 do
              begin
                idx := EAC.Controls.FindIndexByName(Titles[z]);
                if idx >= 0 then
                begin
                  S := DecodeCellText(Cols[z]);
                  if EAC.Controls[idx].ControlType in [eacNumber, eacCheckBox] then
                  begin
                    if TryStrToInt(S, Num) then
                      TVariantArray2d(PPSVariantDynamicArray(P)^.Data)[j][idx] := Num;
                  end
                  else if EAC.Controls[idx].ControlType = eacColor then
                  begin
                    if not TryStrToColor(S, Num) then Num := clNone;
                    TVariantArray2d(PPSVariantDynamicArray(P)^.Data)[j][idx] := Num;
                  end
                  else
	                  TVariantArray2d(PPSVariantDynamicArray(P)^.Data)[j][idx] := S;
                end;
              end;
            end;

            Cols.Free;
            Titles.Free;
            Rows.Free;

          end;
        end;
      else
      begin
        P := CreateHeapVariant(Ex.FindType2(btString));
        VSetString(P, S);
      end;
    end;

    Params.Add(P);
    Dec(n);
  end;

  ProcNo := Ex.GetProc(EAction.OrigName);
  Proc := Ex.GetProcNo(ProcNo) as TPSInternalProcRec;
  if Proc = nil then Exit;

  // Добавляем Result
  S := Proc.ExportDecl;
  Res := grfw(S);
  if Res <> '-1' then
  begin
    P := CreateHeapVariant(Ex.GetTypeNo(StrToInt(Res)));
    Params.Add(P);
  end
  else P := nil;
  //

  {with ExprModule.Exec do
		RunProc(Params, ProcNo);
  ExprModule.Exec.RaiseCurrentException;      }
	Ex.RunProc(Params, ProcNo);
  Ex.RaiseCurrentException;

  Result := Null;
  if P <> nil then PIFVariantToVariant(P, Result);

  {except
    on Err: Exception do
    begin
    	ErrMsgFmt(rsActionExecError, [EAction.Name, Err.Message]);
      raise;
    end;
  end;   }
  finally
  	FreePIFVariantList(Params);
    SetVariantToClass(pV, OldSelf);
  end;
end;

constructor TActionCustom.Create;
begin
  inherited Create;
  FProps := TActionProps.Create;
end;

destructor TActionCustom.Destroy;
begin
  FProps.Free;
  inherited Destroy;
end;

function _RenameFormInGrid(A: TActionCustom; Controls: TEAControls; ControlTypes: TEAControlTypes;
  var GridPropValue: String; const OldName, NewName: String; OnlyCheckExists: Boolean): Boolean;
var
  Rows, Titles, Cols: TStringList;
  i, j, idx: Integer;
  EAC: TEAControl;
  S: String;
begin
  Result := False;
  Rows := TStringList.Create;
  Titles := TStringList.Create;
  Cols := TStringList.Create;

  try

  SplitStr(GridPropValue, '|', Rows);
  SplitStr(Rows[0], ';', Titles);

  for i := 1 to Rows.Count - 1 do
  begin
    SplitStr(Rows[i], ';', Cols);
    for j := 0 to Controls.Count - 1 do
    begin
      EAC := Controls[j];
      idx := Titles.IndexOf(EAC.Name);
      if (idx < 0) or (idx > Cols.Count - 1) then Continue;
      if EAC.ControlType in ControlTypes then
        if MyUtf8CompareText(OldName, Cols[idx]) = 0 then
        begin
          Result := True;
          if OnlyCheckExists then Exit;
          Cols[idx] := NewName;
        end;
    end;
    S := '';
    for j := 0 to Cols.Count - 1 do
    begin
      S := S + Cols[j];
      if j < Cols.Count - 1 then S := S + ';';
    end;
    Rows[i] := S;
  end;

  if Result then
  begin
    S := '';
    for i := 0 to Rows.Count - 1 do
    begin
      S := S + Rows[i];
      if i < Rows.Count - 1 then S := S + '|';
    end;
    GridPropValue := S;
  end;

  finally
    Rows.Free;
    Titles.Free;
    Cols.Free;
  end;
end;

function _RenameForm(A: TActionCustom; Controls: TEAControls; ControlTypes: TEAControlTypes;
  const OldName, NewName: String; OnlyCheckExists: Boolean): Boolean;
var
  i: Integer;
  EAC: TEAControl;
  P: TActionProp;
  GridPropValue: String;
begin
  Result := False;
  for i := 0 to Controls.Count - 1 do
  begin
    EAC := Controls[i];
    if EAC.ControlType in ControlTypes then
    begin
      P := A.Props.Find(EAC.Name);
      if (P <> nil) and (MyUtf8CompareText(P.Value, OldName) = 0) then
      begin
        Result := True;
        if OnlyCheckExists then Exit;
        P.Value := NewName;
      end;
    end
    else if EAC.ControlType = eacGrid then
    begin
      P := A.Props.Find(EAC.Name);
      if P <> nil then
      begin
        GridPropValue := P.Value;
        if _RenameFormInGrid(A, EAC.Controls, ControlTypes, GridPropValue, OldName, NewName,
          OnlyCheckExists) then
        begin
          Result := True;
          if OnlyCheckExists then Exit;
          P.Value := GridPropValue;
        end;
      end;
    end;
  end;
end;

procedure TActionCustom.RenameForm(const OldName, NewName: String);
var
  EA: TExprAction;
begin
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    _RenameForm(Self, EA.Controls, [eacForm, eacChildForm], OldName, NewName, False);
end;

function _SourceExists(A: TActionCustom; Controls: TEAControls; CurFm: TdxForm;
  const SourceName, FormName: String): Boolean;
var
  EACSrc: TEAControl;
  P, SrcP: TActionProp;
  ObjName: String;
  Fm: TdxForm;
  Cmp: TComponent;
begin
  Result := False;
  EACSrc := Controls.FindByName(SourceName);
  if EACSrc = nil then Exit;

  // Источник является формой
  if EACSrc.ControlType in [eacForm, eacChildForm] then
  begin
    P := A.Props.Find(EACSrc.Name);
    if (P <> nil) and (MyUtf8CompareText(P.Value, FormName) = 0) then Exit(True);
  end
  // Источник является объектом
  else if EACSrc.ControlType = eacObject then
  begin
    SrcP := A.Props.Find(EACSrc.Name);
    if SrcP <> nil then
    begin
      ObjName := SrcP.Value;
      if EACSrc.Source = '' then Fm := CurFm
      else Fm := nil;
      // Объект может принадлежать другой форме
      if EACSrc.Source <> '' then
      begin
        EACSrc := Controls.FindByName(EACSrc.Source);
        if EACSrc <> nil then
        begin
          if EACSrc.ControlType in [eacForm, eacChildForm] then
          begin
            SrcP := A.Props.Find(EACSrc.Name);
            if SrcP <> nil then
              Fm := FormMan.FindFormByName(SrcP.Value);
          end;
        end;
      end;
      if Fm <> nil then
      begin
        // Ищем компонент объекта
        Cmp := FindComponentByFieldName(Fm, ObjName);
        if (Cmp <> nil) and (Cmp is TdxLookupComboBox) then
        begin
          // Наконец-то теперь мы можем узнать имя формы
          Fm := FormMan.FindForm(GetSourceTId(Cmp));
          if (Fm <> nil) and (MyUtf8CompareText(FormName, Fm.FormCaption) = 0) then Exit(True);
        end;
      end;
    end;
  end;
end;

function _RenameFieldInGrid(A: TActionCustom; ParentControls, Controls: TEAControls;
  ControlTypes: TEAControlTypes; var GridPropValue: String; CurFm: TdxForm;
  const FormName, OldName, NewName: String; OnlyCheckExists: Boolean): Boolean;
var
  Rows, Titles, Cols: TStringList;
  i, j, n, idx: Integer;
  EAC, EACSrc: TEAControl;
  P: TActionProp;
  S, ObjName: String;
  Cmp: TComponent;
  Fm: TdxForm;
begin
  Result := False;
  Rows := TStringList.Create;
  Titles := TStringList.Create;
  Cols := TStringList.Create;

  try

  SplitStr(GridPropValue, '|', Rows);
  SplitStr(Rows[0], ';', Titles);

  for i := 1 to Rows.Count - 1 do
  begin
    SplitStr(Rows[i], ';', Cols);
    for j := 0 to Controls.Count - 1 do
    begin
      EAC := Controls[j];
      // Таблица может быть пустая или столбцов может быть больше (добавили
      // ui-компонент, а действие не пересохраняли).
      //if j = Cols.Count then Break;
      n := Titles.IndexOf(EAC.Name);
      if (n < 0) or (n > Cols.Count - 1) then Continue;

      if EAC.ControlType in ControlTypes then
      begin
        if MyUtf8CompareText(OldName, Cols[n]) = 0 then
        begin
          // В таблице поля текущей формы
          if (EAC.Source = '') and (CurFm <> nil) and (CurFm.FormCaption = FormName) then
          begin
            Result := True;
            if OnlyCheckExists then Exit;
            Cols[n] := NewName;
          end
          // Поле имеет какой-то источник
          else if EAC.Source <> '' then
          begin
            idx := Titles.IndexOf(EAC.Source);

            // Источник находится в таблице
            if idx >= 0 then
            begin
              EACSrc := Controls.FindByName(EAC.Source);
              if EACSrc <> nil then
              begin
                // Источник является формой
                if EACSrc.ControlType in [eacForm, eacChildForm] then
                begin
                  if MyUtf8CompareText(Cols[idx], FormName) = 0 then
                  begin
                    Result := True;
                    if OnlyCheckExists then Exit;
                    Cols[n] := NewName;
                  end;
                end
                // Источником является объектом
                else if EACSrc.ControlType = eacObject then
                begin
                  if EACSrc.Source = '' then Fm := CurFm
                  else Fm := nil;
                  ObjName := Cols[idx];
                  // Объект принадлежит какой-то форме
                  if EACSrc.Source <> '' then
                  begin
                    idx := Titles.IndexOf(EACSrc.Source);
                    // Источник объекта находится в таблице
                    if idx >= 0 then
                    begin
                      if Controls[idx].ControlType in [eacForm, eacChildForm] then
                        Fm := FormMan.FindFormByName(Cols[idx]);
                    end
                    // Источник где-то наверху
                    else
                    begin
                      EACSrc := ParentControls.FindByName(EACSrc.Source);
                      if EACSrc <> nil then
                      begin
                        if EACSrc.ControlType in [eacForm, eacChildForm] then
                        begin
                          P := A.Props.Find(EACSrc.Name);
                          if P <> nil then
                            Fm := FormMan.FindFormByName(P.Value);
                        end;
                      end;
                    end;
                  end;
                  if Fm <> nil then
                  begin
                    // Наконец-то ищем наш объект и определяем его форму
                    Cmp := FindComponentByFieldName(Fm, ObjName);
                    if (Cmp <> nil) and (Cmp is TdxLookupComboBox) then
                    begin
                      Fm := FormMan.FindForm(GetSourceTId(Cmp));
                      if (Fm <> nil) and (MyUtf8CompareText(Fm.FormCaption, FormName) = 0) then
                      begin
                        Result := True;
                        if OnlyCheckExists then Exit;
                        Cols[n] := NewName;
                      end;
                    end;
                  end;
                end;
              end;
            end
            // Источник где-то наверху
            else
              if _SourceExists(A, ParentControls, CurFm, EAC.Source, FormName) then
              begin
                Result := True;
                if OnlyCheckExists then Exit;
                Cols[n] := NewName;
              end;
          end;
        end;
      end;
    end;

    S := '';
    for j := 0 to Cols.Count - 1 do
    begin
      S := S + Cols[j];
      if j < Cols.Count - 1 then S := S + ';';
    end;
    Rows[i] := S;
  end;

  if Result then
  begin
    S := '';
    for i := 0 to Rows.Count - 1 do
    begin
      S := S + Rows[i];
      if i < Rows.Count - 1 then S := S + '|';
    end;
    GridPropValue := S;
  end;

  finally
    Cols.Free;
    Titles.Free;
    Rows.Free;
  end;
end;

function _RenameField(A: TActionCustom; Controls: TEAControls; ControlTypes: TEAControlTypes;
  CurFm: TdxForm; const FormName, OldName, NewName: String; OnlyCheckExists: Boolean): Boolean;
var
  i: Integer;
  EAC: TEAControl;
  P: TActionProp;
  GridPropValue: String;
begin
  Result := False;
  for i := 0 to Controls.Count - 1 do
  begin
    EAC := Controls[i];
    if EAC.ControlType in ControlTypes then
    begin
      P := A.Props.Find(EAC.Name);
      if (P <> nil) and (MyUtf8CompareText(P.Value, OldName) = 0) then
      begin
        // источником поля является текущая форма
        if (EAC.Source = '') and (CurFm <> nil) and (CurFm.FormCaption = FormName) then
        begin
          Result := True;
          if OnlyCheckExists then Exit;
          P.Value := NewName;
        end
        // Поле имеет источник
        else if EAC.Source <> '' then
        begin
          if _SourceExists(A, Controls, CurFm, EAC.Source, FormName) then
          begin
            Result := True;
            if OnlyCheckExists then Exit;
            P.Value := NewName;
          end;
        end;
      end;
    end
    else if EAC.ControlType = eacGrid then
    begin
      P := A.Props.Find(EAC.Name);
      if P <> nil then
      begin
        GridPropValue := P.Value;
        if _RenameFieldInGrid(A, Controls, EAC.Controls, ControlTypes, GridPropValue, CurFm,
          FormName, OldName, NewName, OnlyCheckExists) then
        begin
          Result := True;
          if OnlyCheckExists then Exit;
          P.Value := GridPropValue;
        end;
      end;
    end;
  end;
end;

procedure TActionCustom.RenameField(CurFm: TObject; const FormName, OldName,
  NewName: String);
var
  EA: TExprAction;
begin
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    _RenameField(Self, EA.Controls, [eacField, eacObject], TdxForm(CurFm),
      FormName, OldName, NewName, False)
end;

procedure TActionCustom.RenameComponent(CurFm: TObject; const FormName,
  OldName, NewName: String);
var
  EA: TExprAction;
begin
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    _RenameField(Self, EA.Controls, [eacComponent], TdxForm(CurFm),
      FormName, OldName, NewName, False)
end;

procedure TActionCustom.RenameQuery(const OldName, NewName: String);
var
  EA: TExprAction;
begin
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    _RenameForm(Self, EA.Controls, [eacQuery], OldName, NewName, False);
end;

procedure TActionCustom.RenameReport(const OldName, NewName: String);
var
  EA: TExprAction;
begin
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    _RenameForm(Self, EA.Controls, [eacReport], OldName, NewName, False);
end;

function _RenameRpFieldInGrid(A: TActionCustom; ParentControls, Controls: TEAControls;
  ControlTypes: TEAControlTypes; var GridPropValue: String;
  RD: TReportData; const OldName, NewName: String; OnlyCheckExists: Boolean): Boolean;
var
  Rows, Titles, Cols: TStringList;
  i, j, n, idx: Integer;
  EAC, SrcEAC: TEAControl;
  S: String;
  SrcType: TEAControlType;
  SrcP: TActionProp;
begin
  Result := False;
  if RD.Kind = rkReport then SrcType := eacReport
  else SrcType := eacQuery;

  Rows := TStringList.Create;
  Titles := TStringList.Create;
  Cols := TStringList.Create;

  try

  SplitStr(GridPropValue, '|', Rows);
  SplitStr(Rows[0], ';', Titles);

  for i := 1 to Rows.Count - 1 do
  begin
    SplitStr(Rows[i], ';', Cols);
    for j := 0 to Controls.Count - 1 do
    begin
      EAC := Controls[j];
      if (EAC.ControlType in ControlTypes) and (EAC.Source <> '') then
      begin
        n := Titles.IndexOf(EAC.Name);
        if (n < 0) or (n > Controls.Count - 1) or (Utf8CompareText(OldName, Cols[n]) <> 0) then Continue;
        idx := Titles.IndexOf(EAC.Source);
        // Источник в таблице
        if idx >= 0 then
        begin
          SrcEAC := Controls.FindByName(EAC.Source);
          if (SrcEAC <> nil) and (SrcEAC.ControlType = SrcType) and (MyUtf8CompareText(Cols[idx], RD.Name) = 0) then
          begin
            Result := True;
            if OnlyCheckExists then Exit;
            Cols[n] := NewName;
          end;
        end
        // Источник где-то наверху
        else
        begin
          SrcEAC := ParentControls.FindByName(EAC.Source);
          if (SrcEAC <> nil) and (SrcEAC.ControlType = SrcType) then
          begin
            SrcP := A.Props.Find(SrcEAC.Name);
            if (SrcP <> nil) and (MyUtf8CompareText(SrcP.Value, RD.Name) = 0) then
            begin
              Result := True;
              if OnlyCheckExists then Exit;
              Cols[n] := NewName;
            end;
          end;
        end;
      end;
    end;
    S := '';
    for j := 0 to Cols.Count - 1 do
    begin
      S := S + Cols[j];
      if j < Cols.Count - 1 then S := S + ';';
    end;
    Rows[i] := S;
  end;

  if Result then
  begin
    S := '';
    for i := 0 to Rows.Count - 1 do
    begin
      S := S + Rows[i];
      if i < Rows.Count - 1 then S := S + '|';
    end;
    GridPropValue := S;
  end;

  finally
    Rows.Free;
    Titles.Free;
    Cols.Free;
  end;
end;

function _RenameRpField(A: TActionCustom; Controls: TEAControls; ControlTypes: TEAControlTypes;
  RD: TReportData; const OldName, NewName: String; OnlyCheckExists: Boolean): Boolean;
var
  i: Integer;
  EAC, EACSrc: TEAControl;
  P, SrcP: TActionProp;
  SrcType: TEAControlType;
  S: String;
begin
  Result := False;
  if RD.Kind = rkReport then SrcType := eacReport
  else SrcType := eacQuery;

  for i := 0 to Controls.Count - 1 do
  begin
    EAC := Controls[i];
    if (EAC.ControlType in ControlTypes) and (EAC.Source <> '') then
    begin
      P := A.Props.Find(EAC.Name);
      if (P <> nil) and (MyUtf8CompareText(OldName, P.Value) = 0) then
      begin
        EACSrc := Controls.FindByName(EAC.Source);
        if (EACSrc <> nil) and (EACSrc.ControlType = SrcType) then
        begin
          SrcP := A.Props.Find(EACSrc.Name);
          if (SrcP <> nil) and (MyUtf8CompareText(SrcP.Value, RD.Name) = 0) then
          begin
            Result := True;
            if OnlyCheckExists then Exit;
            P.Value := NewName;
          end;
        end;
      end;
    end
    else if EAC.ControlType = eacGrid then
    begin
      P := A.Props.Find(EAC.Name);
      if P <> nil then
      begin
        S := P.Value;
        if _RenameRpFieldInGrid(A, Controls, EAC.Controls, ControlTypes, S,
          RD, OldName, NewName, OnlyCheckExists) then
        begin
          Result := True;
          if OnlyCheckExists then Exit;
          P.Value := S;
        end;
      end;
    end;
  end;
end;

procedure TActionCustom.RenameRpField(RD: TObject; const OldName,
  NewName: String);
var
  EA: TExprAction;
begin
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    _RenameRpField(Self, EA.Controls, [eacField], TReportData(RD),
      OldName, NewName, False);
end;

procedure TActionCustom.RenameImage(const OldName, NewName: String);
var
  EA: TExprAction;
begin
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    _RenameForm(Self, EA.Controls, [eacImage], OldName, NewName, False);
end;

function TActionCustom.FormExists(aName: String): Boolean;
var
  EA: TExprAction;
begin
  Result := False;
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    Result := _RenameForm(Self, EA.Controls, [eacForm, eacChildForm], aName, '', True);
end;

function TActionCustom.FieldExists(CurFm: TObject; const FormName,
  FieldName: String): Boolean;
var
  EA: TExprAction;
begin
  Result := False;
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    Result := _RenameField(Self, EA.Controls, [eacField, eacObject], TdxForm(CurFm),
      FormName, FieldName, '', True);
end;

function TActionCustom.ObjectExists(CurFm: TObject; const FormName,
  FieldName: String): Boolean;
var
  EA: TExprAction;
begin
  Result := False;
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    Result := _RenameField(Self, EA.Controls, [eacObject], TdxForm(CurFm),
      FormName, FieldName, '', True);
end;

function TActionCustom.ComponentExists(CurFm: TObject; const FormName,
  CmpName: String): Boolean;
var
  EA: TExprAction;
begin
  Result := False;
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    Result := _RenameField(Self, EA.Controls, [eacComponent], TdxForm(CurFm),
      FormName, CmpName, '', True)
end;

function TActionCustom.QueryExists(aName: String): Boolean;
var
  EA: TExprAction;
begin
  Result := False;
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    Result := _RenameForm(Self, EA.Controls, [eacQuery], aName, '', True);
end;

function TActionCustom.ReportExists(aName: String): Boolean;
var
  EA: TExprAction;
begin
  Result := False;
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    Result := _RenameForm(Self, EA.Controls, [eacReport], aName, '', True);
end;

function TActionCustom.RpFieldExists(RD: TObject; const FieldName: String
  ): Boolean;
var
  EA: TExprAction;
begin
  Result := False;
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    Result := _RenameRpField(Self, EA.Controls, [eacField], TReportData(RD),
      FieldName, '', True);
end;

function TActionCustom.TemplateExists(const FileName: String): Boolean;
var
  EA: TExprAction;
begin
  Result := False;
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    // Функция подходит и для поиска шаблонов.
    Result := _RenameForm(Self, EA.Controls, [eacTemplate], FileName, '', True);
end;

function TActionCustom.ImageExists(const ImageName: String): Boolean;
var
  EA: TExprAction;
begin
  Result := False;
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    // Функция подходит и для поиска изображений.
    Result := _RenameForm(Self, EA.Controls, [eacImage], ImageName, '', True);
end;

function TActionCustom.ValueExists(AValue: String): Boolean;

  function _ValueExists(Controls: TEAControls): Boolean;
  var
    i, j, z: Integer;
    EAC: TEAControl;
    P: TActionProp;
    Rows, Cols: TStringList;
  begin
    Result := False;
    Rows := TStringList.Create;
    Cols := TStringList.Create;

    for i := 0 to Controls.Count - 1 do
    begin
      EAC := Controls[i];
      P := Props.Find(EAC.Name);
      if P = nil then Continue;

      if EAC.ControlType = eacGrid then
      begin
        SplitStr(P.Value, '|', Rows);

        for j := 1 to Rows.Count - 1 do
        begin
          SplitStr(Rows[j], ';', Cols);
          for z := 0 to Cols.Count - 1 do
            if Utf8Pos(AValue, Utf8LowerCase(Cols[z])) > 0 then
            begin
              Result := True;
              Break;
            end;
          if Result then Break;
        end;
      end
      else
      begin
        if Utf8Pos(AValue, Utf8LowerCase(P.Value)) > 0 then Result := True;
      end;
      if Result then Break;
    end;
    Rows.Free;
    Cols.Free;
  end;

var
  EA: TExprAction;
begin
  Result := False;
  AValue := Utf8LowerCase(AValue);
  EA := ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then
    Result := _ValueExists(EA.Controls);
end;

{ TActionProps }

function TActionProps.GetProps(Index: Integer): TActionProp;
begin
  Result := TActionProp(Items[Index]);
end;

function TActionProps.AddProp: TActionProp;
begin
	Result := TActionProp.Create;
  Add(Result);
end;

function TActionProps.Find(const AName: String): TActionProp;
var
  i: Integer;
  P: TActionProp;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    P := Props[i];
    if CompareText(AName, P.Name) = 0 then Exit(P);
  end;
end;

procedure TActionProps.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Props[i].Free;
  inherited Clear;
end;

{ TClearFieldsAction }

function TClearFieldsAction.InnerExecute: Variant;
var
  Fm: TdxForm;
  DS: TSQLQuery;
  i: Integer;
  C: TComponent;
begin
  inherited InnerExecute;
  Result := False;
  with TDataSetProcessor(DSProc).DataSets[DSRi]^ do
  begin
    Fm := Form; DS := DataSet;
  end;

  if not (DS.State in [dsInsert, dsEdit]) then Exit;

  for i := 0 to FFields.Count - 1 do
  begin
    C := FindComponentByFieldName(Fm, FFields[i]);
    if C = nil then Continue;
    if C is TdxDBImage then TdxDBImage(C).Clear
    else if C is TdxFile then TdxFile(C).Clear
    else if C is TdxLookupComboBox then TdxLookupComboBox(C).ClearData
    else if C is TdxCheckBox then DS.FieldByName(FieldStr(C)).AsInteger := 0
    else DS.FieldByName(FieldStr(C)).SetData(nil);
  end;
  Result := True;
end;

constructor TClearFieldsAction.Create;
begin
  inherited Create;
  FFields := TStringListUtf8.Create;
end;

destructor TClearFieldsAction.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

procedure TClearFieldsAction.RenameField(CurFm: TObject; const FormName,
  OldName, NewName: String);
var
  i: Integer;
  S: String;
begin
  if (CurFm = nil) or (MyUtf8CompareText(TdxForm(CurFm).FormCaption, FormName) <> 0) then Exit;
  for i := 0 to FFields.Count - 1 do
  begin
    S := FFields[i];
    if MyUtf8CompareText(OldName, S) = 0 then FFields[i] := NewName;
  end;
end;

function TClearFieldsAction.FieldExists(CurFm: TObject; const FormName,
  FieldName: String): Boolean;
begin
  Result := (CurFm <> nil) and (MyUtf8CompareText(TdxForm(CurFm).FormCaption, FormName) = 0) and
    (FFields.IndexOf(FieldName) >= 0);
end;

function TClearFieldsAction.ValueExists(AValue: String): Boolean;
var
  i: Integer;
  S: String;
begin
  Result := False;
  AValue := Utf8LowerCase(AValue);
  for i := 0 to FFields.Count - 1 do
  begin
    S := Utf8LowerCase(FFields[i]);
    if Utf8Pos(AValue, S, 1) > 0 then Exit(True);
  end;
end;

{ TCallFuncAction }

function TCallFuncAction.InnerExecute: Variant;
var
  DSP: TDataSetProcessor;
  V: Variant;
begin
  inherited InnerExecute;
  DSP := TDataSetProcessor(FDSProc);
  if Trim(FExpr) <> '' then
  begin
    CalcExpression(FExpr, DSP, DSRi, V);
    Result := V;
  end;
end;

function TCallFuncAction.ValueExists(AValue: String): Boolean;
begin
  Result := Utf8Pos(Utf8LowerCase(AValue), Utf8LowerCase(FExpr), 1) > 0;
end;

{ TUserMonitorAction }

function TUserMonitorAction.InnerExecute: Variant;
begin
  inherited InnerExecute;
  ShowMonitorForm;
  Result := True;
end;

{ TOpenReportAction }

function TOpenReportAction.InnerExecute: Variant;
var
  RD: TReportData;
begin
  Result := False;
  RD := ReportMan.FindByName(FRpName);
  if (RD = nil) or (not UserMan.CheckRpVisible(RD.Id)) then Exit;
  ShowReportWindow(RD.Id);
  Result := True;
end;

procedure TOpenReportAction.RenameReport(const OldName, NewName: String);
begin
  if MyUtf8CompareText(OldName, FRpName) = 0 then FRpName := NewName;
end;

function TOpenReportAction.ReportExists(aName: String): Boolean;
begin
  Result := MyUtf8CompareText(aName, FRpName) = 0;
end;

function TOpenReportAction.ValueExists(AValue: String): Boolean;
begin
  Result := Utf8Pos(Utf8LowerCase(AValue), Utf8LowerCase(FRpName), 1) > 0;
end;

{ TMassCalcAction }

function TMassCalcAction.InnerExecute: Variant;
var
  DSP: TDataSetProcessor;
  Fm, Tbl: TdxForm;
  DSR: TDataSetRec;
  C: TComponent;
begin
  Result := False;
  Fm := FormMan.FindFormByName(FFormName);
  if Fm = nil then Exit;
  Tbl := FormMan.FindFormByName(FTableName);
  if Tbl <> nil then
    C := FindComponentByFieldName(Tbl, FFieldName)
  else
    C := FindComponentByFieldName(Fm, FFieldName);
  if C = nil then Exit;
  DSP := TDataSetProcessor(FDSProc);
  DSR := DSP.DataSets[FDSRi]^;
  with TDataSetProcessor.Create do
  try
    BindForm(Fm.Id, False, vtGridOnly);
    Form.OpenRecords(FFilter, DSR.Form, False);
    //_Open2(FFilter, False, DSR.Form);
    if Tbl <> nil then
      Recalculate(Tbl.Id, GetId(C), FExpression, False)
    else
      Recalculate(Fm.Id, GetId(C), FExpression, False);
    Result := True;
  finally
    Free;
  end;
end;

procedure TMassCalcAction.RenameForm(const OldName, NewName: String);
begin
  if MyUtf8CompareText(OldName, FFormName) = 0 then FFormName := NewName
  else if MyUtf8CompareText(OldName, FTableName) = 0 then FTableName := NewName;
end;

procedure TMassCalcAction.RenameField(CurFm: TObject; const aFormName, OldName,
  NewName: String);
begin
  if ((MyUtf8CompareText(aFormName, FFormName) = 0) or
    (MyUtf8CompareText(aFormName, FTableName) = 0)) and
    (MyUtf8CompareText(OldName, FFieldName) = 0) then FFieldName := NewName;
end;

function TMassCalcAction.FormExists(aName: String): Boolean;
begin
  Result := (MyUtf8CompareText(aName, FFormName) = 0) or
    (MyUtf8CompareText(aName, FTableName) = 0);
end;

function TMassCalcAction.FieldExists(CurFm: TObject; const aFormName,
  aFieldName: String): Boolean;
begin
  Result := ((MyUtf8CompareText(aFormName, FFormName) = 0) or
    (MyUtf8CompareText(aFormName, FTableName) = 0)) and
    (MyUtf8CompareText(aFieldName, FFieldName) = 0);
end;

function TMassCalcAction.ValueExists(AValue: String): Boolean;
begin
  AValue := Utf8LowerCase(AValue);
  Result := (Utf8Pos(AValue, Utf8LowerCase(FFormName), 1) > 0) or
    (Utf8Pos(AValue, Utf8LowerCase(FFilter), 1) > 0) or
    (Utf8Pos(AValue, Utf8LowerCase(FTableName), 1) > 0) or
    (Utf8Pos(AValue, Utf8LowerCase(FFieldName), 1) > 0) or
    (Utf8Pos(AValue, Utf8LowerCase(FExpression), 1) > 0);
end;

{ TPrintAction }

function TrySaveRecord(DSP: TDataSetProcessor): Boolean;
begin
  Result := True;
  if DSP.MasterSet.State in [dsInsert, dsEdit] then
  begin
    if DSP.Validate(0, True) then
    begin
    	DSP.Post;
      DSP.InnerEdit(0, True, True, False);
    end
    else Result := False;
  end;
end;

function TPrintAction.InnerExecute: Variant;
var
  DSP: TDataSetProcessor;
  TemplateName, OutName, Errs: String;
  V: Variant;
begin
  Result := False;
  if FDSRi > 0 then Exit;
  DSP := TDataSetProcessor(FDSProc);
  if FSaveRecord and (not TrySaveRecord(DSP)) then Exit;

  TemplateName := Trim(FTemplateFile);
  if (TemplateName = '') and (Trim(FExpression) <> '') then
  begin
    if CalcExpression(FExpression, DSP, DSRi, V) then
      TemplateName := VarToStr(V);
  end;
  if TemplateName = '' then Exit;
  TemplateName := GetTemplatesDir + TemplateName;
  if (Trim(FOutFile) <> '') and CalcExpression(FOutFile, DSP, DSRi, V) then
    OutName := VarToStr(V)
  else
    OutName := GetOutputDir + ExtractFileName(TemplateName);
  //OutName := GetOutputFileName(OutName);
  if not FilenameIsAbsolute(OutName) then OutName := AppPath + OutName;

  {$ifdef windows}
  TemplateName := StringReplace(TemplateName, '/', DirectorySeparator, [rfReplaceAll]);
  OutName := StringReplace(OutName, '/', DirectorySeparator, [rfReplaceAll]);
  {$else}
  TemplateName := StringReplace(TemplateName, '\', DirectorySeparator, [rfReplaceAll]);
  OutName := StringReplace(OutName, '\', DirectorySeparator, [rfReplaceAll]);
  {$endif}

  if not FileExists(TemplateName) then
    raise Exception.CreateFmt(rsTemplateFileNotFound, [TemplateName]);

  DSP.InnerPrint(TemplateName, OutName, Errs, FFileAction = pfaOpen, True);
  if (FFileAction = pfaPrint) and (FileExists(OutName)) then
    ShellExec('print', OutName, '', '', 0);
  Result := True;
  if Errs <> '' then ShowErrorsForm(Errs);
end;

constructor TPrintAction.Create;
begin
  inherited Create;
  FFileAction := pfaOpen;
end;

function TPrintAction.TemplateExists(const FileName: String): Boolean;
begin
  Result := MyUtf8CompareText(FileName, FTemplateFile) = 0;
end;

function TPrintAction.ValueExists(AValue: String): Boolean;
begin
  AValue := Utf8LowerCase(AValue);
  Result := (Utf8Pos(AValue, Utf8LowerCase(FTemplateFile), 1) > 0) or
    (Utf8Pos(AValue, Utf8LowerCase(FExpression), 1) > 0) or
    (Utf8Pos(AValue, Utf8LowerCase(FOutFile), 1) > 0);
end;

{ TGotoFormAction }

function TGotoFormAction.InnerExecute: Variant;
var
  DSP: TDataSetProcessor;
  Fm: TdxForm;
begin
  Result := False;
  DSP := TDataSetProcessor(FDSProc);
  if (DSP.GotoEnable) and (DSP.Form.ViewType <> vtGridOnly) then
  begin
    Fm := FormMan.FindFormByName(FFormName);
    if Fm <> nil then
	    Result := MainFr.GotoRec(Fm.Id, 0);
  end;
end;

procedure TGotoFormAction.RenameForm(const OldName, NewName: String);
begin
  if MyUtf8CompareText(OldName, FFormName) = 0 then FFormName := NewName;
end;

function TGotoFormAction.FormExists(aName: String): Boolean;
begin
  Result := MyUtf8CompareText(aName, FFormName) = 0;
end;

function TGotoFormAction.ValueExists(AValue: String): Boolean;
begin
  Result := Utf8Pos(Utf8LowerCase(AValue), Utf8LowerCase(FFormName), 1) > 0;
end;

{ TActionReader }

(*procedure TActionReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  SL: TStringList;
  i: Integer;
  AttName: SAXString;
  Pm: TActionProp;
  Fm: TdxForm;
  C: TComponent;
  RD: TReportData;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  {with FAction do
  begin
    ExecCond := XmlToStr(Atts.GetValue('', 'execcond'));
    ConfirmMsg := XmlToStr(Atts.GetValue('', 'confirm'));
    Msg := XmlToStr(Atts.GetValue('', 'msg'));
  end; }
  if FAction is TGotoFormAction then
    with TGotoFormAction(FAction) do
    begin
      Fm := FormMan.FindForm(GetInt(Atts, 'formid'));
      if Fm <> nil then FormName := Fm.FormCaption;
    end
  else if FAction is TPrintAction then
    with TPrintAction(FAction) do
    begin
      TemplateFile := Atts.GetValue('', 'template');
      Expression := XmlToStr(Atts.GetValue('', 'expression'));
      OutFile := XmlToStr(Atts.GetValue('', 'outfile'));
      SaveRecord := GetBool(Atts, 'saverecord');
      //ToPrinter := GetBool(Atts, 'toprinter');
    end
  else if FAction is TMassCalcAction then
    with TMassCalcAction(FAction) do
    begin
      Fm := FormMan.FindForm(GetInt(Atts, 'formid'));
      if Fm <> nil then FormName := Fm.FormCaption;
      Filter := XmlToStr(Atts.GetValue('', 'filter'));
      if Fm <> nil then
      begin
	      C := FindById(Fm, GetInt(Atts, 'fieldid'));
        if C <> nil then FieldName := GetFieldName(C);
      end;
      Expression := XmlToStr(Atts.GetValue('', 'expression'));
    end
  else if FAction is TOpenReportAction then
    with TOpenReportAction(FAction) do
    begin
      RD := ReportMan.FindReport(GetInt(Atts, 'rpid'));
      if RD <> nil then RpName := RD.Name;
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
      Fm := TdxForm(FTmpForm);
      for i := 0 to SL.Count - 1 do
      begin
        C := FindById(Fm, StrToInt(SL[i]));
        if C <> nil then
        	Fields.Add(GetFieldName(C));
      end;
      SL.Free;
    end
	else if FAction is TActionCustom then
  begin
    for i := 0 to Atts.Length - 1 do
    begin
      AttName := Atts.GetLocalName(i);
      Pm := TActionCustom(FAction).Props.AddProp;
      Pm.Name := AttName;
      Pm.Value := XmlToStr(Atts.GetValue(i));
    end;
  end;
end;

procedure TActionReader.ParseXml(const Xml: String);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(Xml);
  ParseStream(SS);
  SS.Free;
end;     *)

{ TBaseAction }

function TBaseAction.GetActionName: String;
begin
  Result := ActionTypeToStr(FActionType);
end;

function TBaseAction.InnerExecute: Variant;
begin
  Result := Null;
end;

constructor TBaseAction.Create;
begin

end;

{procedure TBaseAction.Load(const Xml: String);
begin
  with TActionReader.Create do
  begin
    Action := Self;
    ParseXml(Xml);
    Free;
  end;
end;   }

function TBaseAction.Execute: Variant;
{var
  S: String;}
begin
  try
    Result := InnerExecute;
  except
    on E: Exception do
    begin
      {if ScriptLastError.ExObj = E then S := LineEnding + ScriptLastErrorToString
      else if E is EPSException then S := LineEnding + EPSExceptionToString(EPSException(E))
      else S := E.Message;
      raise Exception.CreateFmt(rsActionExecError, [ActionName, S]); }
      raise Exception.CreateFmt(rsActionExecError, [ActionName, ExceptionToString(E, True, False)]);
    end;
  end;
end;

procedure TBaseAction.RenameForm(const OldName, NewName: String);
begin

end;

procedure TBaseAction.RenameField(CurFm: TObject; const FormName, OldName,
  NewName: String);
begin

end;

procedure TBaseAction.RenameComponent(CurFm: TObject; const FormName, OldName,
  NewName: String);
begin

end;

procedure TBaseAction.RenameQuery(const OldName, NewName: String);
begin

end;

procedure TBaseAction.RenameReport(const OldName, NewName: String);
begin

end;

procedure TBaseAction.RenameRpField(RD: TObject; const OldName, NewName: String
  );
begin

end;

procedure TBaseAction.RenameImage(const OldName, NewName: String);
begin

end;

function TBaseAction.FormExists(aName: String): Boolean;
begin
  Result := False;
end;

function TBaseAction.FieldExists(CurFm: TObject; const FormName,
  FieldName: String): Boolean;
begin
  Result := False;
end;

function TBaseAction.ObjectExists(CurFm: TObject; const FormName,
  FieldName: String): Boolean;
begin
  Result := False;
end;

function TBaseAction.ComponentExists(CurFm: TObject; const FormName,
  CmpName: String): Boolean;
begin
  Result := False;
end;

function TBaseAction.QueryExists(aName: String): Boolean;
begin
  Result := False;
end;

function TBaseAction.ReportExists(aName: String): Boolean;
begin
  Result := False;
end;

function TBaseAction.RpFieldExists(RD: TObject; const FieldName: String
  ): Boolean;
begin
  Result := False;
end;

function TBaseAction.TemplateExists(const FileName: String): Boolean;
begin
  Result := False;
end;

function TBaseAction.ImageExists(const ImageName: String): Boolean;
begin
  Result := False;
end;

function TBaseAction.ValueExists(AValue: String): Boolean;
begin
  Result := False;
end;

end.

