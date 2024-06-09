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

unit ExprForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynMemo, Forms, Controls, Graphics,
  Dialogs, ButtonPanel, ComCtrls, StdCtrls, Menus, strconsts, dxctrls,
  expressions, dxreports, LclType, SynEditTypes, mydialogs,
  exprhighlighter;

type

 // ELoopException = class(Exception);

  { TMyHighlighter }

  {TMyHighlighter = class(TSynAnySyn)
  public
    constructor Create(AOwner: TComponent); override;
    function IsKeyword(const AKeyword: string): boolean; override;
  end;   }

  TExprType = (etFieldExpr, etDefaultValue, etCheck, etColoring, etFormCalcField,
    etRpCalcField, etListFilter, etSourceFilter, etOutputFilter, etSelCond,
    etEditCond, etDelCond, etSetValue, etSourceTableFilter, etFilter,
    etActionExpr, etLogicalExpr, etDBFuncFilter);

  { TExprFm }

  TExprFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Memo1: TSynMemo;
    PopupMenu1: TPopupMenu;
    ToolsMnu: TPopupMenu;
    ToolBar2: TToolBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Memo1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure ToolsMnuPopup(Sender: TObject);
  private
    FSelSourceFieldForm, FSelFieldForm: TSelectFieldForm;
    FSelQFieldForm: TSelectQFieldForm;
    FExprType: TExprType;
    FForm, FParForm, FSrcForm, FParSrcForm: TdxForm;
    FExpr: TExpressionBuilder;
    FCmp: TComponent;
    FRD: TReportData;
    //FHelpName: String;
    FErrMsg: String;
    FErrPos: Integer;
    FHigh: TdxExprHighlighter;
    FLoopDetected: Boolean;
    procedure ShowSourceFields;
    procedure ShowFormFields;
    function AddBn(const aCaption, aHint: String; ImgIdx, aTag: Integer): TToolButton;
    procedure AddDiv;
    //procedure FillQueryFields(L: TStrings);
    //procedure FillFields(aFm: TdxForm; L: TStrings; const Pfx: String; AddLabels: Boolean);
    function CheckFilterExpr: Boolean;
    function CheckExpr: Boolean;
    procedure ClearButtons;
    procedure LoadFuncs;
    procedure LoadFormFields(Fm: TdxForm; IsSourceForm: Boolean);
    procedure LoadQueryFields(RD: TReportData);
    procedure ShowFunctionsForm;
    function DoCheck(ShowMsg: Boolean): Boolean;
    function CorrectErrPos(P: Integer): Integer;
    procedure CommentText;
    procedure CommentText2;
    procedure QuoteText(Ch: String);
    procedure BracketText;
    procedure BracketText2;
    //procedure LoopDetect(AOnlyPrefix: Boolean);
    {private declarations }
  public
    { public declarations }
    function ShowForm(ExprType: TExprType; C: TComponent; var aExpr: String;
      Form, SrcForm, ParSrcForm: TdxForm; aRD: TReportData): Integer;
  end;

var
  ExprFm: TExprFm;

function ShowExprForm(ExprType: TExprType; C: TComponent; var aExpr: String;
    Form, SrcForm, ParSrcForm: TdxForm; aRD: TReportData): Integer;

implementation

uses
  funcsform, totalform, apputils, helpmanager,
  sqlgen, formmanager, LazUtf8, scriptmanager, appsettings, dbfuncform,
  formfieldvalueform, queryfieldvalueform, reportmanager;

const
  ChainChars = '->';

type

  { TFilterExprParser }

  TFilterExprParser = class(TSQLFilterParser)
  private
    FParSrcform: TdxForm;
    FSrcForm: TdxForm;
  protected
    function FieldNameParse(const FieldName: String): String; override;
  public
    property SrcForm: TdxForm read FSrcForm write FSrcForm;
    property ParSrcForm: TdxForm read FParSrcForm write FParSrcForm;
  end;

{$R *.lfm}

{function ShowSelectFieldForm(Fm: TdxForm; aShowLabels, CurFmPfx: Boolean
  ): String;
var
  PFm: TdxForm;
begin
  Result := '';
  PFm := nil;
  if Fm.PId > 0 then
  	PFm := FormMan.FindForm(Fm.PId);
  with TSelectFieldForm.CreateNew(nil) do
  try
    Caption := rsSelectFormField;
  	ShowLabels:=aShowLabels;
    ShowCurFormPrefix:=CurFmPfx;
    ShowParFormPrefix := True;
    ShowFieldsOfObject := True;
    ShowObjectFields := True;
    if ShowForm(Fm, PFm) = mrOk then
    	Result := '[' + FieldName + ']';
  finally
  	Free;
  end;
end;

function ShowSelectFieldSource(Fm, ParFm: TdxForm): String;
begin
  Result := '';
  with TSelectFieldForm.CreateNew(nil) do
  try
    Caption := rsSelectSourceField;
    FirstParentForm:=ParFm <> nil;
    ShowFieldsOfObject := True;//ParFm <> nil;
    ShowParFormPrefix := True;
    if ShowForm(Fm, ParFm) = mrOk then
    	Result := '[' + FieldName + ']';
  finally
  	Free;
  end;
end;

function ShowSelectQFieldForm(RD: TReportData): String;
begin
  Result := '';
  with TSelectQFieldForm.CreateNew(nil) do
  try
    Caption := rsSelectQueryField;
    if ShowForm(RD) = mrOk then
      Result := '[' + FieldName + ']';
  finally
    Free;
  end;
end;     }

function ShowExprForm(ExprType: TExprType; C: TComponent; var aExpr: String;
  Form, SrcForm, ParSrcForm: TdxForm; aRD: TReportData): Integer;
begin
  if ExprFm = nil then
    ExprFm := TExprFm.Create(Application);
  Result := ExprFm.ShowForm(ExprType, C, aExpr, Form, SrcForm, ParSrcForm,
  	aRD);
end;

function GetFlName(C: TComponent): String;
begin
  if C is TdxLabel then Result := TdxLabel(C).FieldName
  else Result := GetFieldName(C);
end;

{ TMyHighlighter }

{constructor TMyHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CommentAttri.Foreground:=clGray;
  Comments := [csCStyle];
  NumberAttri.Foreground:=clBlue;
  StringAttri.Foreground:=clGreen;
  KeyAttri.Style := [fsBold];
  IdentifierChars:='!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
end;

function TMyHighlighter.IsKeyword(const AKeyword: string): boolean;
begin
  Result := Keywords.IndexOf(AKeyword) >= 0;
end;  }

{ TFilterExprParser }

function TFilterExprParser.FieldNameParse(const FieldName: String): String;
var
  S: String;
  C: TComponent;
  Fm: TdxForm;
begin
  Result := FieldName;
  S := FieldName;
  if (Length(S) > 0) and (S[1] = '?') then
    Delete(S, 1, 1);
  Fm := FSrcForm;
  if (Length(S) > 0) and (S[1] = '!') then
  begin
    Delete(S, 1, 1);
    Fm := FParSrcForm;
  end;
  if S = '' then DoFilterParserError(Format(rsFPSrcFldIsEmpty, [S]), FOldPos);
  if Fm = nil then DoFilterParserError(Format(rsFPSrcFldNotFound, [S]), FOldPos);

  C := LookupComponent(Fm, S);
  if C = nil then DoFilterParserError(Format(rsFPSrcFldNotFoundIn,
    [S, Fm.FormCaption]), FOldPos);
  if C is TdxObjectField then
    DoFilterParserError(Format(rsFPSrcFldObjFieldNotValid, [S]), FOldPos);
end;

{ TExprFm }

procedure TExprFm.FormCreate(Sender: TObject);
var
  R: TRect;
begin
  Caption := rsExpression;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  MenuItem1.Caption := rsCut;
  SetMenuItemImage(MenuItem1, 'cut16');
  MenuItem2.Caption := rsCopy;
  SetMenuItemImage(MenuItem2, 'copy16');
  MenuItem3.Caption := rsPaste;
  SetMenuItemImage(MenuItem3, 'paste16');
  MenuItem6.Caption := rsUndo;
  SetMenuItemImage(MenuItem6, 'undo16');
  MenuItem7.Caption := rsRedo;
  SetMenuItemImage(MenuItem7, 'goto16');
  MenuItem5.Caption := rsSelectAll;
  Width := AppConfig.ExprFormWidth;
  Height := AppConfig.ExprFormHeight;
  FHigh := TdxExprHighlighter.Create(Self);
  Memo1.Highlighter := FHigh;
  Memo1.Font.Quality:=fqDefault;

  MenuItem9.Caption := rsTotalFunc;
  MenuItem10.Caption := rsDBFunction;
  MenuItem11.Caption := rsValueFromForm;
  MenuItem12.Caption := rsFunctionGET;
  MenuItem13.Caption := rsIDCurrentRec;
  MenuItem14.Caption := rsCondition;

  // Открывается второй редактор выражений
  if ExprFm <> nil then
  begin
    Position:=poDesigned;
    R := ExprFm.BoundsRect;
    SetBounds(R.Left + 50, R.Top + 50, 850, 300);
    LoadFuncs;
  end;

  FSelSourceFieldForm := TSelectFieldForm.CreateNew(Self);
  with FSelSourceFieldForm do
  begin
    Caption := rsSelectSourceField;
    ShowFieldsOfObject := True;
    ShowParFormPrefix := True;
  end;

  FSelFieldForm := TSelectFieldForm.CreateNew(Self);
  with FSelFieldForm do
  begin
    Caption := rsSelectFormField;
    ShowParFormPrefix := True;
    ShowFieldsOfObject := True;
    ShowObjectFields := True;
  end;

  FSelQFieldForm := TSelectQFieldForm.CreateNew(Self);
  FSelQFieldForm.Caption := rsSelectQueryField;
end;

procedure TExprFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult <> mrOk then
  begin
    if Memo1.Modified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
    Exit;
  end;
  if not DoCheck(False) then
  begin
    if FLoopDetected then
    begin
      ErrMsg(FErrMsg);
      CanClose := False;
    end
    else
      CanClose := MessageDlg(rsError,
        Format(rsErrorInExprFix,
          [LineEnding + LineEnding + FErrMsg + LineEnding + LineEnding]), mtError,
          [mbYes, mbNo], 0) = mrNo;
    if not CanClose then
    begin
      Memo1.SetFocus;
      if FErrPos > 0 then
        Memo1.SelStart := FErrPos;
    end;
  end;
end;

procedure TExprFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Запоминаем настройки только основного редактора
  if Self = ExprFm then
  begin
    AppConfig.ExprFormWidth:=ScaleTo96(Width);
    AppConfig.ExprFormHeight:=ScaleTo96(Height);
  end;
end;

procedure TExprFm.FormDestroy(Sender: TObject);
begin
  //FreeAndNil(FExpr);
end;

procedure TExprFm.FormShow(Sender: TObject);
begin
  //UpdateMemoScrollBars(Memo1);
  Memo1.SetFocus;
end;

procedure TExprFm.HelpButtonClick(Sender: TObject);
var
  S: String;
begin
  case FExprType of
    //etFieldExpr, etFormCalcField, etRpCalcField,
    //    etSetValue, etActionExpr, etLogicalExpr: S := 'expressions';
    etFieldExpr: S := 'expression';
    etDefaultValue: S := 'defaultvalue';
    etCheck: S := 'checkvalue';
    etColoring:
      if FRD = nil then S := 'coloring'
      else S := 'rpqrycoloring';
    etListFilter: S := 'listfilter';
    etSourceFilter, etDBFuncFilter: S := 'sourcefilter';
    etOutputFilter: S := 'outfilter';
    etSelCond, etFilter: S := 'selcond';
    etEditCond: S := 'editcond';
    etDelCond: S := 'delcond';
    etSourceTableFilter: S := 'filltablefilter';
    else S := 'expressions';
  end;

  OpenHelp(S);
end;

procedure TExprFm.Memo1Change(Sender: TObject);
begin
  if FHigh.Kind = ekFilter then FHigh.ScanAllRanges;
end;

procedure TExprFm.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F2 then
    ShowFunctionsForm
  else if Key in [VK_F5, VK_F9] then
    DoCheck(True)
  else if Key = VK_ESCAPE then
    ModalResult := mrCancel
  else if (Key = VK_RETURN) and ([ssCtrl] = Shift) then
    ModalResult := mrOk;
end;

procedure TExprFm.Memo1StatusChange(Sender: TObject; Changes: TSynStatusChanges
  );
var
  S: String;
begin
  S := IntToStr(Memo1.CaretY) + ': ' + IntToStr(Memo1.CaretX);
  with ToolBar2 do
		Buttons[ButtonCount - 1].Caption:=S;
end;

procedure TExprFm.MenuItem10Click(Sender: TObject);
var
  S: String;
begin
  with TDBFuncFm.Create(nil) do
  try
    if ShowForm(FForm, S) = mrOk then
    	Memo1.SelText := S;
  finally
    Free;
  end;
end;

procedure TExprFm.MenuItem11Click(Sender: TObject);
var
  S: String;
begin
  if ShowFormFieldValueForm(S) = mrOk then
  	Memo1.SelText := S;
end;

procedure TExprFm.MenuItem12Click(Sender: TObject);
var
  S: String;
begin
  if ShowQueryFieldValueForm(FForm, S) = mrOk then
  	Memo1.SelText := S;
end;

procedure TExprFm.MenuItem13Click(Sender: TObject);
begin
  Memo1.SelText := 'RECID(''' + FForm.FormCaption + ''')';
end;

procedure TExprFm.MenuItem14Click(Sender: TObject);
begin
  Memo1.SelText := 'IIF(, , )';
  Memo1.SelStart := Memo1.SelStart - 5;
end;

procedure TExprFm.MenuItem1Click(Sender: TObject);
begin
  Memo1.CutToClipboard;
end;

procedure TExprFm.MenuItem2Click(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;

procedure TExprFm.MenuItem3Click(Sender: TObject);
begin
  Memo1.PasteFromClipboard;
end;

procedure TExprFm.MenuItem5Click(Sender: TObject);
begin
  Memo1.SelectAll;
end;

procedure TExprFm.MenuItem6Click(Sender: TObject);
begin
  Memo1.Undo;
end;

procedure TExprFm.MenuItem7Click(Sender: TObject);
begin
  Memo1.Redo;
end;

procedure TExprFm.MenuItem9Click(Sender: TObject);
var
  S: String;
begin
	if ShowTotalForm(FForm, S) = mrOk then
	  Memo1.SelText := S;
end;

procedure TExprFm.ToolButtonClick(Sender: TObject);
var
  S: String;
begin
  S := TToolButton(Sender).Caption;
  case TComponent(Sender).Tag of
    0: ShowSourceFields;
    1: ShowFormFields;
    2: ToolsMnu.Popup;  //Memo1.SelText := ShowTotalForm(FForm);
    3: ShowFunctionsForm;
    4:
      begin
        if S = '&&' then S := '&';
        Memo1.SelText := S;
      end;
    5: DoCheck(True);
    6: CommentText;
    7: CommentText2;
    8: QuoteText(S);
    9: BracketText;
    10: BracketText2;
  end;
end;

procedure TExprFm.ToolsMnuPopup(Sender: TObject);
begin
  MenuItem9.Enabled := (Self = ExprFm) and ((FCmp <> nil) or (FForm <> nil));
  MenuItem10.Enabled := Self = ExprFm;
  MenuItem12.Enabled := MenuItem9.Enabled;
  MenuItem13.Enabled := MenuItem9.Enabled;
  	//((FCmp = nil) and (FSrcForm = nil) and (FParSrcForm = nil) and (FForm <> nil));
end;

function TExprFm.AddBn(const aCaption, aHint: String; ImgIdx, aTag: Integer
  ): TToolButton;
var
  Bn: TToolButton;
begin
  Bn := TToolButton.Create(Self);
  Bn.Caption := aCaption;
  Bn.Hint:=aHint;
  Bn.ImageIndex:=ImgIdx;
  Bn.ShowHint:=True;
  Bn.Tag := aTag;
  Bn.OnClick:=@ToolButtonClick;
  Bn.Parent := ToolBar2;
  Result := Bn;
end;

procedure TExprFm.AddDiv;
var
  Bn: TToolButton;
begin
  Bn := TToolButton.Create(Self);
  Bn.Style:=tbsDivider;
  Bn.Parent := ToolBar2;
end;

function TExprFm.CheckFilterExpr: Boolean;
var
  P: TFilterExprParser;
begin
  Result := True;
  P := TFilterExprParser.Create;
  P.ParSrcForm := FParSrcForm;
  P.SrcForm := FSrcForm;
  if FParSrcForm = nil then P.ParSrcForm := FSrcForm;
  P.ExprBuilder := FExpr;
  P.DisableCalcExpr:=True;
  try
    P.Parse(Memo1.Text);
    //if FCmp is TdxQueryGrid then LoopDetect(False);
    if FCmp is TdxQueryGrid then LoopDetect(Memo1.Text, FCmp, FForm, FRD, False);
  except
    on E: EFilterParserError do
    begin
      FErrMsg := E.Message;
      FErrPos := CorrectErrPos(E.Position);
      Result := False;
    end;
    on E: ELoopException do
    begin
      FErrMsg := E.Message;
      FErrPos := 0;
      Result := False;
      FLoopDetected := True;
    end;
  end;
  P.Free;
end;

procedure DoFilterParserError(const Msg: String; P: Integer);
begin
  raise EFilterParserError.Create(Msg, P);
end;

procedure TExprFm.ShowSourceFields;
begin
  if FRD = nil then
    with FSelSourceFieldForm do
    begin
      if ShowForm(FSrcForm, FParSrcForm) = mrOk then
    	  Memo1.SelText := '[' + FieldName + ']';
    end
  else
    with FSelQFieldForm do
    begin
      if ShowForm(FRD) = mrOk then
        Memo1.SelText := '[' + FieldName + ']';
    end;
end;

procedure TExprFm.ShowFormFields;
var
  PFm: TdxForm;
begin
  if FForm.PId > 0 then
  	PFm := FormMan.FindForm(FForm.PId)
  else
    PFm := nil;
  with FSelFieldForm do
  begin
    if ShowForm(FForm, PFm) = mrOk then
    	Memo1.SelText := '[' + FieldName + ']';
  end;
end;

function TExprFm.CheckExpr: Boolean;
var
  Ex: TExpression;
begin
  Result := True;
  Ex := nil;
  try
    Ex := FExpr.Build(Memo1.Lines.Text);
    if ((FExprType = etFieldExpr) and (Ex <> nil)) or (FCmp is TdxQueryGrid) then
      LoopDetect(Memo1.Text, FCmp, FForm, FRD, FExprType in [etRpCalcField, etOutputFilter]);
      //LoopDetect(FExprType in [etRpCalcField, etOutputFilter]);

  except
    on E: ECalcError do
    begin
      FErrMsg := E.Message;
      FErrPos := CorrectErrPos(E.Position);
      Result := False;
    end;
    on E: ELoopException do
    begin
      FErrMsg := E.Message;
      FErrPos := 0;
      Result := False;
      FLoopDetected := True;
    end;
  end;
  FreeAndNil(Ex);
end;

procedure TExprFm.ClearButtons;
var
  i: Integer;
begin
  for i := ToolBar2.ButtonCount - 1 downto 0 do
    ToolBar2.Buttons[i].Free;
end;

procedure TExprFm.LoadFuncs;
var
  FL: THelpList;
  i: Integer;
  F: TExprFunc;
begin
  FL := THelpList.Create;
  try try
    FL.LoadFromFile;
    for i := 0 to FL.Count - 1 do
      FHigh.AddFunc(FL[i].Name);
  except
    ;
  end;
  finally
    FL.Free;
  end;
  // Дополнительные функции в модулях выражений
  for i := 0 to ScriptMan.Funcs.Count - 1 do
  begin
    F := ScriptMan.Funcs[i];
    FHigh.AddFunc(F.Name);
  end;
end;

procedure TExprFm.LoadFormFields(Fm: TdxForm; IsSourceForm: Boolean);

  procedure _LoadFormFields(Fm: TdxForm; const ObjectFieldName: String; Depth: Byte);
  var
    i: Integer;
    C: TComponent;
    FNm: String;
    LCbx: TdxLookupComboBox;
    SFm: TdxForm;
  begin
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if (Depth = 0) and not FExpr.SkipLabels and not IsSourceForm and
        (C is TdxLabel) and (Trim(GetExpression(C)) > '') then
      begin
        FNm := TdxLabel(C).FieldName;
        if Fm.PId = 0 then FNm := '!' + FNm;
        FHigh.AddLabel(FNm);
        Continue;
      end;
      if not HasFId(C) then Continue;
      FNm := ObjectFieldName + GetFieldName(C);
      if (Fm.PId = 0) and (ObjectFieldName = '') then FNm := '!' + FNm;
      if IsSourceForm then FHigh.AddSourceField(FNm)
      else FHigh.AddField(FNm);
      if C is TdxLookupComboBox then
      begin
        // Для избежания зацикливания (форма может ссылаться на себя или
        // формы могут иметь перекрестные ссылки).
        if Depth < 4 then
        begin
          LCbx := TdxLookupComboBox(C);
          if LCbx.SourceTId > 0 then
          begin
            SFm := FormMan.FindForm(LCbx.SourceTId);
            TestNil(SFm, 'TExprFm.LoadFromFields nil');
            _LoadFormFields(SFm, FNm + '|', Depth + 1);
          end;
        end;
      end;
    end;
  end;

begin
  if Fm <> nil then
    _LoadFormFields(Fm, '', 0);
end;

procedure TExprFm.LoadQueryFields(RD: TReportData);
var
  i: Integer;
  {F: TRpField;
  CF: TRpCalcField; }
begin
  if (RD = nil) or RD.IsEmpty then Exit;
  for i := 0 to RD.GetFieldCount - 1 do
    FHigh.AddQueryField(RD.GetFieldName(i));

  {for i := 0 to RD.Sources[0]^.Fields.Count - 1 do
  begin
    F := RD.Sources[0]^.Fields[i]^;
    FHigh.AddQueryField(F.Name);
  end;
  for i := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[i]^;
    FHigh.AddQueryField(CF.Name);
  end;  }
end;

function GetFuncName(const S: String; P: Integer): String;
var
  p1, p2, Len: Integer;
begin
  Result := '';
	Len := Length(S);
  if Len = 0 then Exit;
  p2 := P;
  while (P >= 1) and (S[P] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
		Dec(P);
  p1 := P+1;
  while (p2 <= Len) and (S[p2] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
		Inc(p2);
  Result := Copy(S, p1, p2 - p1);
end;

procedure TExprFm.ShowFunctionsForm;
var
  S: String;
begin
  S := GetFuncName(Memo1.Text, Memo1.SelStart);
  if ShowFuncsForm(S) = mrOk then
    Memo1.SelText := FuncsFm.FuncName;
end;

function TExprFm.DoCheck(ShowMsg: Boolean): Boolean;
begin
  Result := True;
  FLoopDetected := False;
  if Trim(Memo1.Text) <> '' then
  begin
    if (FSrcForm = nil) and (FParSrcForm = nil) then Result := CheckExpr
    else Result := CheckFilterExpr;
    if ShowMsg then
    begin
      if Result then
        MessageDlg(rsExprCheck2, rsSuccess, mtInformation, [mbOk], 0)
      else
      begin
        ErrMsg(FErrMsg);
        Memo1.SetFocus;
        if FErrPos > 0 then
          Memo1.SelStart:=FErrPos;
      end;
    end;
  end;
end;

function TExprFm.CorrectErrPos(P: Integer): Integer;
var
  Len: Integer;
begin
  Result := P;
  Len := Length(Memo1.Text);
  if P > Len then Result := Len;
end;

procedure TExprFm.CommentText;
begin
  if Memo1.SelAvail then
  	Memo1.SelText := '/*' + Memo1.SelText + '*/'
  else
  begin
  	Memo1.SelText := '/*  */';
    Memo1.SelStart := Memo1.SelStart - 3;
  end;
end;

procedure TExprFm.CommentText2;
begin
  Memo1.SelText := '// ' + Memo1.SelText;
end;

procedure TExprFm.QuoteText(Ch: String);
begin
  if Memo1.SelAvail then
  	Memo1.SelText := Ch + Memo1.SelText + Ch
  else
  begin
  	Memo1.SelText := Ch + Ch;
    Memo1.SelStart := Memo1.SelStart - 1;
  end;
end;

procedure TExprFm.BracketText;
begin
  if Memo1.SelAvail then
  	Memo1.SelText := '[' + Memo1.SelText + ']'
  else
  begin
  	Memo1.SelText := '[]';
    Memo1.SelStart := Memo1.SelStart - 1;
  end;
end;

procedure TExprFm.BracketText2;
begin
  if Memo1.SelAvail then
  	Memo1.SelText := '{' + Memo1.SelText + '}'
  else
  begin
  	Memo1.SelText := '{}';
    Memo1.SelStart := Memo1.SelStart - 1;
  end;
end;

// Проходим по цепочке зависимых полей и ищем циклическую ссылку на текущее поле
(*procedure TExprFm.LoopDetect(AOnlyPrefix: Boolean);
var
  FieldsChain, S: String;
  CL: TList;

  // Продвигаеся по цепочке полей и запросов. Если в итоге возвращаемся на
  // текущий компонент, значит есть зацикливание. Компоненты могут
  // ссылаться на самих себя и чтобы не было зацикливания добавлена
  // проверка на Level=0 и добавлен параметр CurC. OnlyPrefix - если
  // выражение является выражением выходного фильтра или вычисляемого поля,
  // где поля формы обозначаются префиксами.
  function _LoopDetect(CurC: TComponent; const Expr, Chain: String; Level: Integer; OnlyPrefix: Boolean): Boolean;
  var
    i, j: Integer;
    C: TComponent;
    E: TExpression;
    S, FlNm: String;
    RD: TReportData;
    b: Boolean;
  begin
    Result := False;
    if CurC <> nil then CL.Add(CurC);
    for i := 0 to FForm.ComponentCount - 1 do
    begin
      C := FForm.Components[i];
      // В выражении поля надписи не поддерживаются
      if (C is TdxLabel) and FExpr.SkipLabels then Continue
      // Бывает, что поле (или запрос) в цепочке ссылается на само себя и чтобы не было зацикливания
      // в проверке на зацикливание в функцию было добавлено это условие
      else if C = CurC then Continue
      // Т. к. проверка основывается на том, что при зацикливании мы вернемся
      // по цепочке на текущий же компонент, но в то же время компонент
      // может ссылаться на самого себя, мы допускаем совпадение компонентов
      // в начале цепочки.
      else if (C = FCmp) and (Level = 0) then Continue
      // Запрос
      else if C is TdxQueryGrid then
      begin
        if FCmp is TdxLabel then Continue;

        RD := ReportMan.FindReport(GetId(C));
        if FormExists(RD.Name, Expr) then
        begin
          if CL.IndexOf(C) >= 0 then
          begin
            FieldsChain := Chain + ChainChars + RD.Name;
            Exit(True);
          end;

          for j := 0 to RD.Sources.Count - 1 do
            if _LoopDetect(C, RD.Sources[j]^.Filter, Chain + ChainChars +
              Format(rsLoopDetectSourceFilter, [RD.Name, j+1]),
              Level + 1, False) then Exit(True);
          if _LoopDetect(C, RD.SQL, Chain + ChainChars +
            Format(rsLoopDetectSQL, [RD.Name]), Level + 1, True) then Exit(True);
          if _LoopDetect(C, RD.Filter, Chain + ChainChars +
            Format(rsLoopDetectOutputFilter, [RD.Name]),
            Level + 1, True) then Exit(True);
          for j := 0 to RD.CalcFields.Count - 1 do
            if _LoopDetect(C, RD.CalcFields[j]^.Expr, Chain + ChainChars +
              Format(rsLoopDetectCalcField, [RD.Name, RD.CalcFields[j]^.Name]),
              Level + 1, True) then Exit(True);
        end;
        Continue;
      end
      else if not HasExpression(C) then Continue;

      FlNm := GetFlName(C);

      if not OnlyPrefix then
        b := FieldExists(FForm.PId, FlNm, Expr)
      // В выражениях выходного фильтра и вычисляемых полях запроса поля
      // формы идут только с префиксом.
      else
        b := FieldExistsForQuery(FlNm, Expr);

      if b then
      begin
        // Мы вернулись по цепочке на текущий же компонент. Значит это зацикливание.
        if CL.IndexOf(C) >= 0 then
        begin
          FieldsChain := Chain + ChainChars + BrStr(FlNm);
          Exit(True);
        end;

        S := GetExpression(C);
        if Trim(S) <> '' then
        begin
          try
            E := FExpr.Build(S);
          except
            E := nil;
          end;
          if E <> nil then
          begin
            FreeAndNil(E);
            if _LoopDetect(C, S, Chain + ChainChars + BrStr(FlNm), Level + 1, False) then Exit(True);
          end;
        end;
      end;
    end;

    if CurC <> nil then CL.Remove(CurC);
  end;

begin
  CL := TList.Create;
  CL.Add(FCmp);
  S := GetComponentName(FCmp);
  if not (FCmp is TdxQueryGrid) then S := BrStr(S);
  if _LoopDetect(nil, Memo1.Lines.Text, S, 0, AOnlyPrefix) then
  begin
    CL.Free;
    raise ELoopException.CreateFmt(rsLoopDetected, [FieldsChain]);
  end;
  CL.Free;
end;  *)

function TExprFm.ShowForm(ExprType: TExprType; C: TComponent;
  var aExpr: String; Form, SrcForm, ParSrcForm: TdxForm; aRD: TReportData
  ): Integer;
var
  S: TCaption;
begin
  Result := mrCancel;
  if C is TdxLabel then
  begin
    S := TdxLabel(C).Caption;
    if (not CheckDuplicateFieldName(S, C, True) or not CheckFieldName(S)) then Exit;
    if (Pos(#10, S) > 0) or (Pos(#13, S) > 0) then
    begin
      ErrMsg(rsMultilineLabelsNotAllow);
      Exit;
    end;
  end;

  FExprType := ExprType;

  case ExprType of
    etFieldExpr: Caption := rsExpression;
    etDefaultValue: Caption := rsDefaultValue;
    etCheck: Caption := rsCheckValue;
    etFormCalcField, etRpCalcField, etColoring,
      etSetValue, etActionExpr: Caption := rsExpression;
    etSourceFilter: Caption := rsSourceFilter;
    etListFilter: Caption := rsListFilter;
    etOutputFilter: Caption := rsOutputFilter + ': ' + aRD.Name;
    etSelCond: Caption := rsSelCond + ': ' + SrcForm.FormCaption;
    etEditCond: Caption := rsEditCond + ': ' + Form.FormCaption;
    etDelCond: Caption := rsDelCond + ': ' + Form.FormCaption;
    etSourceTableFilter: Caption := rsSourceTableFilter;
    etFilter, etDBFuncFilter: Caption := rsFilter;
    etLogicalExpr: Caption := rsLogicalExpr;
  end;

  FForm := Form;
  FParForm := nil;
  if FForm <> nil then
    FParForm := FormMan.FindForm(Form.PId);
  FSrcForm := SrcForm;
  FParSrcForm := ParSrcForm;
  FCmp := C;
  FRD := aRD;

  ClearButtons;
  if (FSrcForm <> nil) or (FParSrcForm <> nil) then
    AddBn('', rsInsertSourceField, 4, 0)
  else if FRD <> nil then
		AddBn('', rsInsertQueryField, 4, 0);
  if FForm <> nil then
    AddBn('', rsInsertFormField, 2, 1);
  AddBn('', rsFunctionsBn, 1, 3);
  AddBn('', rsInsertMore, 0, 2);
  AddBn('', rsExprCheck, 3, 5);
  AddBn('+', rsAddition, -1, 4);
  AddBn('-', rsSubtraction, -1, 4);
  AddBn('*', rsMultiplication, -1, 4);
  AddBn('/', rsDivision, -1, 4);
  AddBn('=', rsEqual, -1, 4);
  AddBn('<>', rsNotEqual, -1, 4);
  AddBn('<', rsLess, -1, 4);
  AddBn('<=', rsLessOrEqual, -1, 4);
  AddBn('>', rsGreat, -1, 4);
  AddBn('>=', rsGreatOrEqual, -1, 4);
  if (FSrcForm <> nil) or (FParSrcForm <> nil) then
  begin
	  AddBn('==', rsContainingText, -1, 4);
  	AddBn('#', rsNotContainingText, -1, 4);
    AddBn('in', rsContainedInList, -1, 4);
    AddBn('notin', rsNotContainedInList, -1, 4);
    //AddBn('frags', rsContainingTextFrags, -1, 4);
  end;
  AddBn('&&', rsLogicalAND, -1, 4);
  AddBn('|', rsLogicalOR, -1, 4);
  AddBn('/* */', rsMultiLineComment, -1, 6);
  AddBn('//', rsLineComment, -1, 7);
  AddBn('''', rsTakeSelectionSingleQuotes, -1, 8);
  AddBn('"', rsTakeSelectionDoubleQuotes, -1, 8);
  AddBn('[]', rsTakeSelectionSquareBr, -1, 9);
  if (FSrcForm <> nil) or (FParSrcForm <> nil) then
	  AddBn('{}', rsTakeSelectionCurlyBr, -1, 10);
  AddDiv;
  AddBn('1:1', rsCursorPos, -1, -1).Enabled:=False;

  FExpr := TExpressionBuilder.Create;
  FExpr.Form := FForm;
  FExpr.ParentForm := FParForm;
  FExpr.SkipLabels:=(C <> nil) and (not (C is TdxLabel));
  FExpr.RD := FRD;

  FHigh.FieldAsParentField := (FForm <> nil) and (FForm.PId = 0);
  FHigh.SourceFieldAsParentSourceField := ExprType in [etListFilter, etSelCond, etFilter] ;
  if (FSrcForm <> nil) or (FParSrcForm <> nil) then FHigh.Kind := ekFilter
  else if FRD <> nil then FHigh.Kind := ekQuery
  else FHigh.Kind := ekExpr;

  if ScriptMan.NeedUpdateFuncs then
  begin
    FHigh.ClearFuncs;
  	LoadFuncs;
    if FuncsFm <> nil then FuncsFm.UpdateHelp;
  end;
  ScriptMan.NeedUpdateFuncs := False;
  FHigh.ClearFields;
  LoadFormFields(FForm, False);
  LoadFormFields(FParForm, False);
  LoadFormFields(FSrcForm, True);
  LoadFormFields(FParSrcForm, True);
  LoadQueryFields(FRD);

  with FSelSourceFieldForm do
  begin
    ForgetForms;
    FirstParentForm := ParSrcForm <> nil;
  end;
  with FSelFieldForm do
  begin
    ForgetForms;
    ShowLabels := C is TdxLabel;
    ShowCurFormPrefix := FRD <> nil;
  end;
  FSelQFieldForm.ForgetReport;

  Memo1.Text := aExpr;
  Memo1.ShowHint := True;
  Memo1.Modified := False;
  Result := ShowModal;
  if Result = mrOk then
    aExpr := TrimRight(Memo1.Text);
  FExpr.Free;
end;

end.

