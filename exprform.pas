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
unit ExprForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynMemo, SynHighlighterAny, Forms,
  Controls, Graphics, Dialogs, ButtonPanel, ComCtrls, StdCtrls, Menus,
  strconsts, dxctrls, expressions, dxreports, LclType, SynEditTypes, mydialogs;

type

  { TExprFm }

  TExprFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MyHigh: TSynAnySyn;
    Memo1: TSynMemo;
    PopupMenu1: TPopupMenu;
    ToolBar2: TToolBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Memo1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Memo1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
  private
    FForm, FParForm, FSrcForm, FParSrcForm: TdxForm;
    FExpr: TExpressionBuilder;
    FCmp: TComponent;
    FRD: TReportData;
    FHelpName: String;
    FErrMsg: String;
    FErrPos: Integer;
    procedure ShowSourceFields;
    procedure ShowFormFields;
    function AddBn(const aCaption, aHint: String; ImgIdx, aTag: Integer): TToolButton;
    procedure AddDiv;
    //procedure FillQueryFields(L: TStrings);
    //procedure FillFields(aFm: TdxForm; L: TStrings; const Pfx: String; AddLabels: Boolean);
    function CheckFilterExpr: Boolean;
    function CheckExpr: Boolean;
    procedure ClearButtons;
    procedure LoadKeywords;
    procedure ShowFuncsForm;
    function DoCheck(ShowMsg: Boolean): Boolean;
    function CorrectErrPos(P: Integer): Integer;
    procedure CommentText;
    procedure CommentText2;
    {private declarations }
  public
    { public declarations }
    function ShowForm(const aCaption: String; C: TComponent; var aExpr: String;
      Form, SrcForm, ParSrcForm: TdxForm; aRD: TReportData; const aHelp: String): Boolean;
  end;

var
  ExprFm: TExprFm;

implementation

uses
  funcsform, {SelectForm, }dximages, totalform, apputils, helpform, dxfiles,
  sqlgen, formmanager, LazUtf8, scriptmanager, appsettings;

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

function ShowSelectFieldForm(Fm: TdxForm; aShowLabels, CurFmPfx: Boolean
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
    ShowFieldsOfObject := ParFm <> nil;
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
end;

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
  if S = '' then DoFilterParserError(Format(rsFPSrcFldNotAccept, [S]), Position);
  if Fm = nil then DoFilterParserError(Format(rsFPSrcFldNotFound, [S]), Position);

  C := LookupComponent(Fm, S);
  if C = nil then DoFilterParserError(Format(rsFPSrcFldNotFoundIn,
    [S, Fm.FormCaption]), Position);
  if (C is TdxFile) or (C is TdxDBImage) or (C is TdxObjectField) then
    DoFilterParserError(Format(rsFPSrcFldNotAcceptPar, [S]), Position);
end;

{ TExprFm }

procedure TExprFm.FormCreate(Sender: TObject);
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
  //LoadKeywords;
end;

procedure TExprFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult <> mrOk then Exit;
  if not DoCheck(False) then
  begin
    CanClose := MessageDlg(rsError,
      Format(rsErrorInExprFix,
        [LineEnding + LineEnding + FErrMsg + LineEnding + LineEnding]), mtError,
        [mbYes, mbNo], 0) = mrNo;
    if not CanClose then Memo1.SelStart := FErrPos;
  end;
end;

procedure TExprFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  AppConfig.ExprFormWidth:=Width;
  AppConfig.ExprFormHeight:=Height;
end;

procedure TExprFm.FormDestroy(Sender: TObject);
begin
  //FreeAndNil(FExpr);
end;

procedure TExprFm.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

procedure TExprFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp(FHelpName);
end;

procedure TExprFm.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F2 then
    ShowFuncsForm
  else if Key = VK_F5 then
    DoCheck(True);
end;

procedure TExprFm.Memo1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  P: TPoint;
begin
  {P := Memo1.PixelsToRowColumn(Point(X, Y));
  Memo1.Hint := IntToStr(P.y) + ': ' + IntToStr(P.x);}
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

procedure TExprFm.ToolButtonClick(Sender: TObject);
var
  S: String;
begin
  case TComponent(Sender).Tag of
    0: ShowSourceFields;
    1: ShowFormFields;
    2: Memo1.SelText := TotalFm.ShowForm(FForm);
    3: ShowFuncsForm;
    4:
      begin
        S := TToolButton(Sender).Caption;
        if S = '&&' then S := '&';
        Memo1.SelText := S;
      end;
    5: DoCheck(True);
    6: CommentText;
    7: CommentText2;
  end;
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
  P.ExprBuilder := FExpr;
  P.DisableCalcExpr:=True;
  try
    P.Parse(Memo1.Text);
    //MessageDlg(rsExprCheck2, rsSuccess, mtInformation, [mbOk], 0);
  except
    on E: EFilterParserError do
    begin
      FErrMsg := E.Message;
      FErrPos := CorrectErrPos(E.Position);
      Result := False;
    end;
  end;
  P.Free;
end;

procedure DoFilterParserError(const Msg: String; P: Integer);
begin
  raise EFilterParserError.Create(Msg, P);
end;

procedure TExprFm.ShowSourceFields;
var
  S: String;
begin
  if FRD = nil then
	  S := ShowSelectFieldSource(FSrcForm, FParSrcForm)
  else
  	S := ShowSelectQFieldForm(FRD);
  if S <> '' then Memo1.SelText := S;
end;

procedure TExprFm.ShowFormFields;
var
  S: String;
begin
  S := ShowSelectFieldForm(FForm, FCmp is TdxLabel, FRD <> nil);
  if S <> '' then Memo1.SelText := S;
end;

function TExprFm.CheckExpr: Boolean;
var
  Ex: TExpression;
begin
  Result := True;
  Ex := nil;
  try
    Ex := FExpr.Build(Memo1.Lines.Text);
  except
    on E: ECalcError do
    begin
      FErrMsg := E.Message;
      FErrPos := CorrectErrPos(E.Position);
      Result := False;
    end
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

procedure TExprFm.LoadKeywords;
var
  FL: THelpList;
  i, j: Integer;
  F: TExprFunc;
begin
  MyHigh.Keywords.Clear;
  FL := THelpList.Create;
  try try
    FL.LoadFromFile;
    for i := 0 to FL.Count - 1 do
      MyHigh.KeyWords.Add(FL[i].Name);
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
    MyHigh.Keywords.Add(F.Name);
  end;
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

procedure TExprFm.ShowFuncsForm;
var
  S: String;
begin
  S := GetFuncName(Memo1.Text, Memo1.SelStart);
  if FuncsFm.ShowForm(S) = mrOk then
    Memo1.SelText := FuncsFm.FuncName;
end;

function TExprFm.DoCheck(ShowMsg: Boolean): Boolean;
begin
  Result := True;
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
  Memo1.SelText := '// ';
end;

function TExprFm.ShowForm(const aCaption: String; C: TComponent;
  var aExpr: String; Form, SrcForm, ParSrcForm: TdxForm; aRD: TReportData;
  const aHelp: String): Boolean;
begin
  if C is TdxLabel then
  begin
    if (Trim(TdxLabel(C).Expression) = '') and
    	(not (CheckName2(TdxLabel(C).Caption) and CheckDuplicateLabel2(TdxLabel(C), TdxLabel(C).Caption))) then Exit;
  end;

  Caption := aCaption;
  FHelpName := aHelp;

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
  if ((C <> nil) and ((C is TdxLabel) or (C is TdxCalcEdit))) or
    ((C = nil) and (SrcForm = nil) and (ParSrcForm = nil) and (Form <> nil)) then
    AddBn('', rsInsertTotalFunc, 0, 2);
  AddBn('', rsFunctionsBn, 1, 3);
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
  end;
  AddBn('&&', rsLogicalAND, -1, 4);
  AddBn('|', rsLogicalOR, -1, 4);
  AddBn('/* */', rsMultiLineComment, -1, 6);
  AddBn('//', rsLineComment, -1, 7);
  AddBn('', rsExprCheck, 3, 5);
  AddDiv;
  AddBn('1:1', rsCursorPos, -1, -1).Enabled:=False;

  FExpr := TExpressionBuilder.Create;
  FExpr.Form := FForm;
  FExpr.ParentForm := FParForm;
  FExpr.SkipLabels:=(C <> nil) and (not (C is TdxLabel));
  FExpr.RD := FRD;

  if ScriptMan.NeedUpdateFuncs then
  begin
  	LoadKeywords;
    FuncsFm.UpdateHelp;
  end;
  ScriptMan.NeedUpdateFuncs := False;

  Memo1.Text := aExpr;
  Memo1.ShowHint := True;
  Result := ShowModal = mrOk;
  if Result then
    aExpr := TrimRight(Memo1.Text);
  FExpr.Free;
end;

end.

