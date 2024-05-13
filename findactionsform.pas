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

unit FindActionsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  Buttons, LazUtf8, strconsts, scriptmanager, dxactions, dxctrls, dxmains,
  LCLType, ExtCtrls;

type

  { TFindActionsFm }

  TFindActionsFm = class(TForm)
    IfChk: TCheckBox;
    KindCbx: TComboBox;
    FindBn: TBitBtn;
    Label5: TLabel;
    ModuleCbx: TComboBox;
    ActionCbx: TComboBox;
    NothingPan: TPanel;
    ValueEd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Grid: TStringGrid;
    procedure FindBnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure GridDblClick(Sender: TObject);
    procedure GridEnter(Sender: TObject);
    procedure GridExit(Sender: TObject);
    procedure GridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure KindCbxSelect(Sender: TObject);
    procedure ModuleCbxSelect(Sender: TObject);
    procedure ValueEdKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure ShowResult;
    function GetSelectedModule: TScriptData;
    function GetSelectedAction: TExprAction;
    procedure FindInActions(AForm: TdxForm; AButton: TdxButton; const Xml: String);
    procedure FillModules(KeepSelected: Boolean);
    procedure FillActions(KeepSelected: Boolean);
  public
    procedure ShowForm;
    procedure DeleteForm(Fm: TdxForm);
    procedure DeleteButton(Bn: TObject);
    procedure DeleteModule(SD: TObject);
    procedure Reset;
    procedure UpdateForm;
  end;

var
  FindActionsFm: TFindActionsFm;

procedure ShowFindActionsForm;

implementation

uses
  formmanager, designerframe, actionseditform;

procedure ShowFindActionsForm;
begin
  if FindActionsFm = nil then
    FindActionsFm := TFindActionsFm.Create(Application);
  FindActionsFm.ShowForm;
end;

function ExprActionToActionType(EA: TExprAction): TdxActionType;
begin
  if PtrInt(EA) <= Ord(High(TdxActionType)) then
    Result := TdxActionType(PtrInt(EA))
  else
    Result := actCustom;
end;

{$R *.lfm}

{ TFindActionsFm }

procedure TFindActionsFm.ModuleCbxSelect(Sender: TObject);
begin
  FillActions(False);
end;

procedure TFindActionsFm.ValueEdKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then FindBn.Click;
end;

procedure TFindActionsFm.ShowResult;
var
  c, r, mr, EAPos, i: Integer;
  Fm: TdxForm;
  Bn: TdxButton;
  ActType: TdxActionType;
  ActionId: String;
begin
  if Grid.RowCount = 1 then Exit;
  r := Grid.Row;
  c := Grid.Col;
  ActionId := Grid.Cells[5, r];
  if Grid.Objects[5, r] <> nil then
    ActType := TdxActionType(PtrInt(Grid.Objects[5, r]))
  else
    ActType := actCustom;
  if Grid.Cells[3, r] = '?' then
    ActionId := '';

  Fm := TdxForm(Grid.Objects[1, r]);
  if Fm <> nil then
    DesignFr.FormsTreeView.SelectForm(Fm);
  if c > 1 then
  begin
    Bn := TdxButton(Grid.Objects[2, r]);
    if Bn <> nil then
    begin
      DesignFr.CompTree.SelectComponents([Bn]);
      DesignFr.SelectControl(Bn, False);
    end;
  end;
  if c > 2 then
  begin
    EAPos := 0;
    for i := Grid.Row - 1 downto 1 do
      if (Grid.Objects[1, i] = Fm) and (Grid.Objects[2, i] = Bn) and
        (Grid.Cells[5, i] = ActionId) then Inc(EAPos);
    if Bn <> nil then
      Bn.ActionOnClick := ShowActionsEditForm(rsButtonActions, Bn.ActionOnClick, Fm, [atAll, atButton], mr, ActType, ActionId, EAPos)
    else if Fm <> nil then
      Fm.ActionOnCreate := ShowActionsEditForm(rsFormActions, Fm.ActionOnCreate, Fm, [atAll, atForm], mr, ActType, ActionId, EAPos)
    else
      DXMain.Actions := ShowActionsEditForm(rsStartupActions, DXMain.Actions, nil, [atAll, atMain], mr, ActType, ActionId, EAPos);
  end;
end;

procedure TFindActionsFm.FindBnClick(Sender: TObject);
var
  i, j: Integer;
  Fm: TdxForm;
  C: TComponent;
begin
  NothingPan.Visible := False;
  Grid.RowCount := 1;
  FindInActions(nil, nil, DXMain.Actions);
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    FindInActions(Fm, nil, Fm.ActionOnCreate);
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if C is TdxButton then
        FindInActions(Fm, TdxButton(C), TdxButton(C).ActionOnClick);
    end;
  end;
  Grid.SortColRow(True, 1);
  if Grid.RowCount > 1 then Grid.Row := 1
  else NothingPan.Visible := True;
end;

procedure TFindActionsFm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  // При закрытии формы окно программы может быть перекрыто другим приложением.
  Application.MainForm.SetFocus;
end;

procedure TFindActionsFm.FormCreate(Sender: TObject);
begin
  Caption := rsFindActions;
  PopupParent := Application.MainForm;
  Label1.Caption := rsExtensionsModule;
  Label5.Caption := rsActionType;
  KindCbx.Items.AddStrings([rsAllTypesList, rsForAllAction, rsFormAction,
    rsButtonAction, rsStartupAction]);
  KindCbx.ItemIndex := 0;
  IfChk.Caption := rsSearchInIF;
  Label2.Caption := rsAction;
  Label3.Caption := rsValue;
  ValueEd.TextHint := rsAllValues;
  FindBn.Caption := rsFind;
  FindBn.LoadGlyphFromLazarusResource('find16');
  Label4.Caption := rsSearchResult;
  Grid.Columns[0].Title.Caption := rsForm;
  Grid.Columns[1].Title.Caption := rsButton;
  Grid.Columns[2].Title.Caption := rsModule;
  Grid.Columns[3].Title.Caption := rsAction;
  Grid.Cells[0, 0] := '#';
  Grid.FocusRectVisible := False;
  Grid.AllowOutboundEvents := False;
  NothingPan.Caption := rsNothingFound;
end;

procedure TFindActionsFm.FormDestroy(Sender: TObject);
begin
  FindActionsFm := nil;
end;

procedure TFindActionsFm.FormShow(Sender: TObject);
begin
  ValueEd.SetFocus;
end;

procedure TFindActionsFm.GridCompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
begin
  Result := Utf8CompareText(Grid.Cells[1, ARow], Grid.Cells[1, BRow]);
  if Result = 0 then
    Result := Utf8CompareText(Grid.Cells[2, ARow], Grid.Cells[2, BRow]);
  if Result = 0 then
    Result := Utf8CompareText(Grid.Cells[3, ARow], Grid.Cells[3, BRow]);
  if Result = 0 then
    Result := Utf8CompareText(Grid.Cells[4, ARow], Grid.Cells[4, BRow]);
end;

procedure TFindActionsFm.GridDblClick(Sender: TObject);
var
  P: TPoint;
  c, r: Integer;
begin
  P := Grid.ScreenToClient(Mouse.CursorPos);
  Grid.MouseToCell(P.x, P.y, c, r);
  if (c > 0) and (r > 0) then ShowResult;
end;

procedure TFindActionsFm.GridEnter(Sender: TObject);
begin
  TStringGrid(Sender).SelectedColor := clHighlight;
end;

procedure TFindActionsFm.GridExit(Sender: TObject);
begin
  TStringGrid(Sender).SelectedColor := clSilver;
end;

procedure TFindActionsFm.GridGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  HintText:=StringReplace(HintText, '|', '/', [rfReplaceAll]);
end;

procedure TFindActionsFm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then ShowResult;
end;

procedure TFindActionsFm.GridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if (Grid.Cells[3, aRow] = '?') and (aState * [gdSelected, gdFocused, gdFixed] = []) then
    Grid.Canvas.Brush.Color := $008080FF;
end;

procedure TFindActionsFm.KindCbxSelect(Sender: TObject);
begin
  FillActions(False);
end;

function TFindActionsFm.GetSelectedModule: TScriptData;
begin
  with ModuleCbx do
    Result := TScriptData(Items.Objects[ItemIndex]);
end;

function TFindActionsFm.GetSelectedAction: TExprAction;
begin
  with ActionCbx do
    Result := TExprAction(Items.Objects[ItemIndex]);
end;

procedure TFindActionsFm.FindInActions(AForm: TdxForm; AButton: TdxButton;
  const Xml: String);
var
  AR: TActionRunner;
  Act: TExprAction;
  ActType: TdxActionType;
  Value: String;
  Module: TScriptData;

  procedure AddResult(const ActionId: String);
  var
    EA: TExprAction;
    SD: TScriptData;
    r: Integer;
    S: TCaption;
  begin
    if (Act <> nil) and (Act.Id = ActionId) then
      EA := Act
    else
      EA := ScriptMan.Actions.FindAction(ActionId);
    if EA <> nil then
      SD := ScriptMan.Scripts[EA.SDi]
    else
      SD := nil;
    r := Grid.RowCount;
    Grid.RowCount := Grid.RowCount + 1;
    if AForm <> nil then
      Grid.Cells[1, r] := AForm.FormCaption;
    Grid.Objects[1, r] := AForm;
    if AButton <> nil then
    begin
      S := AButton.Caption;
      if S <> '' then S := S + ' (' + AButton.Name + ')'
      else S := AButton.Name;
      Grid.Cells[2, r] := S;
    end;
    Grid.Objects[2, r] := AButton;
    if EA <> nil then
    begin
      Grid.Cells[3, r] := SD.Name;
      Grid.Cells[4, r] := EA.Name;
    end
    else
    begin
      Grid.Cells[3, r] := '?';
      Grid.Cells[4, r] := ActionId;;
    end;
    Grid.Cells[5, r] := ActionId;
    Grid.Objects[3, r] := SD;
    {Grid.Objects[4, r] := EA;}
  end;

  procedure AddResultStd(AType: TdxActionType);
  var
    r: Integer;
    S: TCaption;
  begin
    r := Grid.RowCount;
    Grid.RowCount := Grid.RowCount + 1;
    if AForm <> nil then
      Grid.Cells[1, r] := AForm.FormCaption;
    Grid.Objects[1, r] := AForm;
    if AButton <> nil then
    begin
      S := AButton.Caption;
      if S <> '' then S := S + ' (' + AButton.Name + ')'
      else S := AButton.Name;
      Grid.Cells[2, r] := S;
    end;
    Grid.Objects[2, r] := AButton;
    Grid.Cells[3, r] := rsBuiltInActions;
    Grid.Cells[4, r] := ActionTypeToStr(AType);
    Grid.Cells[5, r] := IntToStr(Ord(AType));
    Grid.Objects[5, r] := TObject(PtrInt(AType));
  end;

  procedure AddResultIf(ALine: TActionLine);
  var
    r: Integer;
    S: TCaption;
  begin
    r := Grid.RowCount;
    Grid.RowCount := Grid.RowCount + 1;
    if AForm <> nil then
      Grid.Cells[1, r] := AForm.FormCaption;
    Grid.Objects[1, r] := AForm;
    if AButton <> nil then
    begin
      S := AButton.Caption;
      if S <> '' then S := S + ' (' + AButton.Name + ')'
      else S := AButton.Name;
      Grid.Cells[2, r] := S;
    end;
    Grid.Objects[2, r] := AButton;
    if ALine.Kind = alkIf then S := '[' + rsIf + ']'
    else S := '[' + rsElseIf + ']';
    Grid.Cells[3, r] := S;
    Grid.Cells[4, r] := ALine.Cond;
  end;

  procedure Find(Lines: TActionLines);
  var
    i: Integer;
    L: TActionLine;
    EA: TExprAction;
    ActionId: String;
  begin
    for i := 0 to Lines.Count - 1 do
    begin
      L := Lines[i];
      if L.Kind = alkAction then
      begin
        if L.Action is TActionCustom then
        begin
          if (ActType in [actNone, actCustom]) and (ModuleCbx.ItemIndex <> 1) then
          begin
            ActionId := TActionCustom(L.Action).ActionId;
            EA := ScriptMan.Actions.FindAction(ActionId);
            if (Act = nil) or (ActionId = Act.Id) then
            begin
              if (Act = nil) and (Module <> nil) then
              begin
                if (EA <> nil) and (ScriptMan.Scripts[EA.SDi] <> Module) then Continue;
              end;

              if (KindCbx.ItemIndex = 0) or (EA = nil) or
                (Ord(EA.Target) = KindCbx.ItemIndex - 1) then
              begin
                if (Value = '') or L.Action.ValueExists(Value) then
                  AddResult(ActionId);
              end;
            end;
          end;
        end
        else
        begin
          if ModuleCbx.ItemIndex <= 1 then
          begin
            if (ActionCbx.ItemIndex = 0) or (L.Action.ActionType = ActType) then
            begin
              if (KindCbx.ItemIndex = 0) or ((KindCbx.ItemIndex = 1) and
                (L.Action.ActionType in [actShowMessage, actCallFunc])) or
                ((KindCbx.ItemIndex = 3) and not (L.Action.ActionType in [actShowMessage, actCallFunc])) then
              begin
                if (Value = '') or L.Action.ValueExists(Value) then
                  AddResultStd(L.Action.ActionType);
              end;
            end;
          end;
        end;
      end
      else if L.Kind in [alkIf, alkElseIf, alkElse] then
      begin
        if (L.Kind <> alkElse) and IfChk.Checked then
        begin
          if (Value = '') or (Utf8Pos(Value, Utf8LowerCase(L.Cond), 1) > 0) then AddResultIf(L);
        end;
        Find(L.Lines);
      end;
    end;
  end;

begin
  Module := GetSelectedModule;
  Act := GetSelectedAction;
  ActType := ExprActionToActionType(Act);
  Value := Utf8LowerCase(ValueEd.Text);
  AR := TActionRunner.Create;
  AR.Load(Xml);
  Find(AR.Lines);
  AR.Free;
end;

procedure TFindActionsFm.FillModules(KeepSelected: Boolean);
var
  Tmp: TCaption;
  i: Integer;
begin
  with ModuleCbx do
  begin
    if KeepSelected then Tmp := Text;
    ScriptMan.ModulesToList(Items, skExpr);
    Sorted := True;
    Sorted := False;
    Items.Insert(0, '[' + rsBuiltInActions + ']');
    Items.Insert(0, rsAllModulesList);

    ItemIndex := 0;
    if KeepSelected then
    begin
      i := Items.IndexOf(Tmp);
      if i > 0 then ItemIndex := i;
    end;
  end;
end;

procedure TFindActionsFm.FillActions(KeepSelected: Boolean);
var
  Module, SD: TScriptData;
  i: Integer;
  EA: TExprAction;
  Tmp: TCaption;
begin
  if KeepSelected then Tmp := ActionCbx.Text;
  ActionCbx.Clear;
  if ModuleCbx.ItemIndex <> 1 then
  begin
    Module := GetSelectedModule;
    for i := 0 to ScriptMan.Actions.Count - 1 do
    begin
      EA := ScriptMan.Actions[i];
      SD := ScriptMan.Scripts[EA.SDi];
      if ((Module = nil) or (SD = Module)) and
        ((KindCbx.ItemIndex = 0) or (Ord(EA.Target) = KindCbx.ItemIndex - 1)) then
        ActionCbx.Items.AddObject(EA.Name, EA);
    end;
  end;

  if ModuleCbx.ItemIndex <= 1 then
  begin
    if KindCbx.ItemIndex in [0, 3] then
    begin
      ActionCbx.Items.AddObject(rsGoToForm, TObject(actGotoForm));
      ActionCbx.Items.AddObject(rsPrint, TObject(actPrint));
      ActionCbx.Items.AddObject(rsMassCalc, TObject(actMassCalc));
      ActionCbx.Items.AddObject(rsOpenReport, TObject(actOpenReport));
      ActionCbx.Items.AddObject(rsSaveChanges, TObject(actSaveChanges));
      ActionCbx.Items.AddObject(rsUserMonitor, TObject(actUserMonitor));
      ActionCbx.Items.AddObject(rsClearFields, TObject(actClearFields));
    end;
    if KindCbx.ItemIndex <= 1 then
    begin
      ActionCbx.Items.AddObject(rsCallFunction, TObject(actCallFunc));
      ActionCbx.Items.AddObject(rsShowMessage, TObject(actShowMessage));
    end;
  end;

  ActionCbx.Sorted := True;
  ActionCbx.Sorted := False;
  ActionCbx.Items.Insert(0, rsAllActionsList);
  ActionCbx.ItemIndex := 0;

  if KeepSelected then
  begin
    with ActionCbx do
    begin
      i := Items.IndexOf(Tmp);
      if i > 0 then ItemIndex := i;
    end;
  end;
end;

procedure TFindActionsFm.ShowForm;
begin
  NothingPan.Visible := False;
  FillModules(True);
  FillActions(True);
  Grid.SelectedColor := clSilver;
  if WindowState = wsMinimized then WindowState := wsNormal;
  Show;
end;

procedure TFindActionsFm.DeleteForm(Fm: TdxForm);
var
  i: Integer;
begin
  for i := Grid.RowCount - 1 downto 1 do
    if (Fm = Grid.Objects[1, i]) or (Fm.Id = TdxForm(Grid.Objects[1, i]).PId) then Grid.DeleteRow(i);
end;

procedure TFindActionsFm.DeleteButton(Bn: TObject);
var
  i: Integer;
begin
  for i := Grid.RowCount - 1 downto 1 do
    if Bn = Grid.Objects[2, i] then Grid.DeleteRow(i);
end;

procedure TFindActionsFm.DeleteModule(SD: TObject);
var
  i: Integer;
begin
  for i := Grid.RowCount - 1 downto 1 do
    if SD = Grid.Objects[3, i] then Grid.DeleteRow(i);
end;

procedure TFindActionsFm.Reset;
begin
  FillModules(False);
  KindCbx.ItemIndex := 0;
  FillActions(False);
  ValueEd.Text := '';
  Grid.RowCount := 1;
  NothingPan.Visible := False;
end;

procedure TFindActionsFm.UpdateForm;
begin
  FillModules(True);
  FillActions(True);
end;

end.

