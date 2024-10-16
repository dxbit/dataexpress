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

unit ActionPans;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, DxActions, DxCtrls,
  strconsts, MyTypes, editbtn, Graphics, Grids, Menus, LclType,
  Dialogs, scriptmanager, actioncontrols, FPCanvas, Variants, Forms, StrUtils;

type

  { TProblemIcon }

  TProblemIcon = class(TWinControl)
  private
    FImage: TImage;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TBasicActionPanel }

  TBasicActionPanel = class(TCustomPanel)
  private
    FForm: TdxForm;
    FCreateAction: Boolean;
    FProblemControl: TWinControl;
    procedure TimerTimer(Sender: TObject);
  protected
    FAction: TBaseAction;
    FProblemIcon: TProblemIcon;
    FTimer: TTimer;
    function InnerCreateControls: TControl; virtual;
  public
    constructor CreatePanel(AOwner: TComponent; AAction: TBaseAction; AForm: TdxForm); virtual;
    procedure CreateControls;
    procedure DeleteControls;
    destructor Destroy; override;
    procedure Init; virtual;
    procedure SaveAction; virtual;
    function Validate: Boolean; virtual;
    procedure FocusProblem;
    property Form: TdxForm read FForm;
  end;

  { TGotoFormPanel }

  TGotoFormPanel = class(TBasicActionPanel)
  private
    FForms: TFormCbx;
  protected
    function InnerCreateControls: TControl; override;
  public
    procedure Init; override;
    procedure SaveAction; override;
    function Validate: Boolean; override;
  end;

  { TPrintActionPanel }

  TPrintActionPanel = class(TBasicActionPanel)
  private
    FTempls: TTemplateCbx;
    FExpr, FOutFile: TActionExpr;
    FSaveRecord: TCheckBox;
    FFileAction: TActionCbx;
  protected
    function InnerCreateControls: TControl; override;
  public
    procedure Init; override;
    procedure SaveAction; override;
    function Validate: Boolean; override;
  end;

  { TMassCalcActionPanel }

  TMassCalcActionPanel = class(TBasicActionPanel)
    procedure FormsChange(Sender: TObject);
  private
    FForms: TFormCbx;
    FTbl: TChildFormCbx;
    FFilter: TActionFilterExpr;
    FFields: TFieldCbx;
    FExpr: TActionExpr;
    procedure TblChange(Sender: TObject);
  protected
    function InnerCreateControls: TControl; override;
  public
    procedure Init; override;
    procedure SaveAction; override;
    function Validate: Boolean; override;
  end;

  { TOpenReportActionPanel }

  TOpenReportActionPanel = class(TBasicActionPanel)
  private
    FReps: TReportCbx;
  protected
    function InnerCreateControls: TControl; override;
  public
    procedure Init; override;
    procedure SaveAction; override;
    function Validate: Boolean; override;
  end;

  TSaveChangesActionPanel = class(TBasicActionPanel);

  TUserMonitorActionPanel = class(TBasicActionPanel);

  { TCallFuncActionPanel }

  TCallFuncActionPanel = class(TBasicActionPanel)
  private
    FExpr: TActionExpr;
  protected
    function InnerCreateControls: TControl; override;
  public
    procedure Init; override;
    procedure SaveAction; override;
    function Validate: Boolean; override;
  end;

  { TClearFieldsActionPanel }

  TClearFieldsActionPanel = class(TBasicActionPanel)
  private
    FGrid: TActionGrid;
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure MenuHandler(Sender: TObject);
  protected
    function InnerCreateControls: TControl; override;
  public
    procedure Init; override;
    procedure SaveAction; override;
    function Validate: Boolean; override;
  end;

  { TShowMessageActionPanel }

  TShowMessageActionPanel = class(TBasicActionPanel)
  private
    FTitle, FMessage: TEdit;
    FMsgType: TComboBox;
    FButtons: TActionOptions;
    FExprMsg: TActionExpr;
	protected
    function InnerCreateControls: TControl; override;
  public
    procedure Init; override;
    procedure SaveAction; override;
  end;

  { TCustomActionPanel }

  TCustomActionPanel = class(TBasicActionPanel)
  private
    FEAction: TExprAction;
    procedure ComboBoxChange(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridGetDefaultCellText(Sender: TObject; Col: Integer;
      var aText: String);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    function CreateControl(EAC: TEAControl): TControl;
    function GetComponentSourceForm(AGrid: TEAControl; ARow: Integer; const ASource: String): TdxForm;
  protected
    function InnerCreateControls: TControl; override;
  public
    procedure Init; override;
    procedure SaveAction; override;
    function Validate: Boolean; override;
    procedure SetDefaultValues;
    property EAction: TExprAction read FEAction write FEAction;
  end;

implementation

uses
  apputils, formmanager, actionexpressions;

procedure CheckMsg(const S: String; Args: array of const);
begin
  MessageDlg(rsActionCheck, Format(S, Args), mtWarning, [mbOk], 0);
end;

{ TProblemIcon }

constructor TProblemIcon.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FImage := TImage.Create(Self);
  FImage.Parent := Self;
  SetupPicture(FImage.Picture, 'exprcheck16');
  FImage.AutoSize := True;
  AutoSize := True;
end;

{ TShowMessageActionPanel }

function TShowMessageActionPanel.InnerCreateControls: TControl;
var
  L: TLabel;
begin
  L := CreateLabel(Self, rsTitle);
  FTitle := TEdit.Create(Self);
  FTitle.Parent := Self;
  AnchorCtrl(FTitle, L, 1);

  L := CreateLabel(Self, rsMessage);
  AnchorCtrl(L, FTitle, 4);
  FMessage := TEdit.Create(Self);
  FMessage.Parent := Self;
  AnchorCtrl(FMessage, L, 1);

  L := CreateLabel(Self, rsOrExpression);
  AnchorCtrl(L, FMessage, 4);
  FExprMsg := TActionExpr.Create(Self);
  FExprMsg.Parent := Self;
  FExprMsg.CurForm := FForm;
  AnchorCtrl(FExprMsg, L, 1);

  L := CreateLabel(Self, rsMsgType);
  AnchorCtrl(L, FExprMsg, 4);
  FMsgType := TComboBox.Create(Self);
  FMsgType.Style := csDropDownList;
  FMsgType.Parent := Self;
  FMsgType.Items.AddStrings([rsWarning, rsError, rsInformation, rsConfirmation]);
  AnchorCtrl(FMsgType, L, 1);

  L := CreateLabel(Self, rsShowButtons);
  AnchorCtrl(L, FMsgType, 4);
  FButtons := TActionOptions.Create(Self);
  FButtons.Columns:=3;
  FButtons.Parent := Self;
  FButtons.Height := 100;
  FButtons.Items.AddStrings([rsOk, rsCancel, rsAbort, rsRetry, rsIgnore, rsYes, rsNo,
    rsAll, rsNoToAll, rsYesToAll, rsClose]);
  AnchorCtrl(FButtons, L, 1);

  Result := FButtons;
end;

procedure TShowMessageActionPanel.Init;
var
  A: TShowMessageAction;
begin
  inherited Init;
  A := TShowMessageAction(FAction);
  FTitle.Text := A.Title;
  FMessage.Text := A.Message;
  FExprMsg.Text := A.ExprMsg;
  FMsgType.ItemIndex := Ord(A.MsgType);
  FButtons.Checked[0] := mbOk in A.Buttons;
  FButtons.Checked[1] := mbCancel in A.Buttons;
  FButtons.Checked[2] := mbAbort in A.Buttons;
  FButtons.Checked[3] := mbRetry in A.Buttons;
  FButtons.Checked[4] := mbIgnore in A.Buttons;
  FButtons.Checked[5] := mbYes in A.Buttons;
  FButtons.Checked[6] := mbNo in A.Buttons;
  FButtons.Checked[7] := mbAll in A.Buttons;
  FButtons.Checked[8] := mbNoToAll in A.Buttons;
  FButtons.Checked[9] := mbYesToAll in A.Buttons;
  FButtons.Checked[10] := mbClose in A.Buttons;
end;

procedure TShowMessageActionPanel.SaveAction;
var
  A: TShowMessageAction;
  B: TMsgDlgButtons;
begin
  A := TShowMessageAction(FAction);
  A.Title := FTitle.Text;
  A.Message := FMessage.Text;
  A.ExprMsg := FExprMsg.Text;
  A.MsgType := TMsgDlgType(FMsgType.ItemIndex);
  B := [];
  if FButtons.Checked[0] then Include(B, mbOk);
  if FButtons.Checked[1] then Include(B, mbCancel);
  if FButtons.Checked[2] then Include(B, mbAbort);
  if FButtons.Checked[3] then Include(B, mbRetry);
  if FButtons.Checked[4] then Include(B, mbIgnore);
  if FButtons.Checked[5] then Include(B, mbYes);
  if FButtons.Checked[6] then Include(B, mbNo);
  if FButtons.Checked[7] then Include(B, mbAll);
  if FButtons.Checked[8] then Include(B, mbNoToAll);
  if FButtons.Checked[9] then Include(B, mbYesToAll);
  if FButtons.Checked[10] then Include(B, mbClose);
  A.Buttons := B;
end;

{ TCustomActionPanel }

procedure TCustomActionPanel.ComboBoxChange(Sender: TObject);
var
  i: Integer;
  Nm: String;

  procedure _Change(EAC: TEAControl);
  var
    C: TComponent;
    j: Integer;
  begin
    if CompareText(EAC.Source, Nm) = 0 then
    begin
      C := FindComponent(EAC.Name);
      if C is TActionCbx then
      begin
      	with TActionCbx(C) do
        begin
          Fill;
          OnChange(C);
        end;
      end;
    end;
  	if EAC.ControlType = eacGrid then
	    for j := 0 to EAC.Controls.Count - 1 do
  	  	_Change(EAC.Controls[j]);
  end;

begin
  Nm := TComponent(Sender).Name;
  for i := 0 to FEAction.Controls.Count - 1 do
  begin
    _Change(FEAction.Controls[i]);
  end;
end;

procedure TCustomActionPanel.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  EAC: TEAControl;
  Grid: TActionGrid;
  C: TCanvas;
  S: String;
  Clr: TColor;
begin
  Grid := TActionGrid(Sender);
  EAC := FEAction.Controls.FindByName(Grid.Name);
  if (aRow >= Grid.FixedRows) and (EAC.Controls[aCol].ControlType = eacColor) then
  begin
    C := Grid.Canvas;
    S := Grid.Cells[aCol, aRow];
    if not TryStrToColor(S, Clr) then Clr := clNone;
    if Clr <> clNone then
    begin
      C.Brush.Color := Clr;
      C.Brush.Style := bsSolid;
    end
    else
    begin
      C.Brush.Color := clBlack;
      C.Brush.Style := bsBDiagonal;
    end;
    if (gdSelected in aState) and (Clr <> clNone) then
      aRect.Inflate(-ScaleToScreen(4), -ScaleToScreen(4), -ScaleToScreen(5),
        -ScaleToScreen(5));
    C.FillRect(aRect);
  end;
end;

procedure TCustomActionPanel.GridGetDefaultCellText(Sender: TObject;
  Col: Integer; var aText: String);
var
  EAC: TEAControl;
  S: String;
begin
  EAC := FEAction.Controls.FindByName(TComponent(Sender).Name);
  TestNil(EAC, 'GridGetDefaultCellText: EAC=nil');
  S := EAC.Controls[Col].DefaultValue;
  if S <> '' then aText := S;
end;

procedure TCustomActionPanel.GridSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
var
  EAC, GEAC: TEAControl;
  Grid: TActionGrid;
  C: TComponent;
  i: Integer;
begin
  Grid := TActionGrid(Sender);
  EAC := FEAction.Controls.FindByName(Grid.Name);

  GEAC := EAC.Controls[aCol];
  if GEAC.ControlType = eacCheckBox then Exit;

	Editor := TWinControl(FindComponent(GEAC.Name));
  Editor.Tag := Grid.ComponentIndex;

  // Актуализируем списки
  if Editor is TActionCbx then
    for i := 0 to aCol do
    begin
      GEAC := EAC.Controls[i];
      C := FindComponent(GEAC.Name);
      if C is TActionCbx then
    	  with TActionCbx(C) do
        begin
      	  if (SourceCbx <> nil) and (EAC.Controls.FindByName(GEAC.Source) <> nil) then Fill;
          Text := Grid.Cells[i, aRow];
        end;
    end;
end;

function TCustomActionPanel.CreateControl(EAC: TEAControl): TControl;
var
  C, CC: TControl;
  i: Integer;
  GEAC: TEAControl;
  Column: TGridColumn;
  SrcC: TComponent;
  DefColor: TColor;
begin
  case EAC.ControlType of
    eacGrid: C := TActionGrid.Create(Self);
    eacText: C := TActionText.Create(Self);
    eacNumber: C := TActionNumber.Create(Self);
    eacCheckBox: C := TActionCheckBox.Create(Self);
    eacFile: C := TActionFile.Create(Self);
    eacFolder: C := TActionFolder.Create(Self);
    eacExpr: C := TActionExpr.Create(Self);
    eacForm: C := TFormCbx.Create(Self);
    eacChildForm: C := TChildFormCbx.Create(Self);
    eacQuery: C := TQueryCbx.Create(Self);
    eacObject: C := TObjectCbx.Create(Self);
    eacField: C := TFieldCbx.Create(Self);
    eacComponent: C := TComponentCbx.Create(Self);
    eacReport: C := TReportCbx.Create(Self);
    eacTemplate: C := TTemplateCbx.Create(Self);
    eacList: C := TListCbx.Create(Self);
    eacFilter: C := TActionFilterExpr.Create(Self);
    eacDivider: C := TActionDivider.Create(Self, EAC.Caption);
    eacColor: C := TActionColor.Create(Self);
    eacImage: C := TActionImage.Create(Self);
  end;

  if C is TActionCbx then
  begin
    TActionCbx(C).OnChange:=@ComboBoxChange;
    SrcC := FindComponent(EAC.Source);
    if (SrcC <> nil) and (SrcC is TActionCbx) then
	    TActionCbx(C).SourceCbx := TActionCbx(SrcC);
    TActionCbx(C).CurForm := FForm;
    TActionCbx(C).Filter := EAC.Filter;
  end
  else if C is TListCbx then
  	with TListCbx(C) do
    begin
      LoadItems(EAC.Items);
      if EAC.Editing then Style := csDropDown;
    end
  else if C is TActionFile then
  	with TActionFile(C) do
    begin
      Filter:=EAC.Filter;
    end
  else if C is TActionExpr then
  begin
    SrcC := FindComponent(EAC.Source);
    if (SrcC <> nil) and (SrcC is TActionCbx) then
	    TActionExpr(C).SourceCbx := TActionCbx(SrcC);
    TActionExpr(C).CurForm := FForm;
    TActionExpr(C).NoForm := EAC.NoForm;
  end
  else if C is TActionFilterExpr then
  begin
    SrcC := FindComponent(EAC.Source);
    if (SrcC <> nil) and (SrcC is TActionCbx) then
	    TActionFilterExpr(C).SourceCbx := TActionCbx(SrcC);
    SrcC := FindComponent(EAC.Form);
    if (SrcC <> nil) and (SrcC is TActionCbx) then
      TActionFilterExpr(C).FormCbx := TActionCbx(SrcC);
    TActionFilterExpr(C).CurForm := FForm;
    TActionFilterExpr(C).NoForm := EAC.NoForm;
  end
  else if C is TActionColor then
  begin
    if (EAC.DefaultValue <> '') and TryStrToColor(EAC.DefaultValue, DefColor) then
      TActionColor(C).DefaultColor := DefColor;
  end
  else if C is TActionGrid then
    with TActionGrid(C) do
    begin
      for i := 0 to EAC.Controls.Count - 1 do
      begin
        GEAC := EAC.Controls[i];
        Column := Columns.Add;
        if GEAC.ControlType = eacCheckBox then
        	Column.ButtonStyle := cbsCheckboxColumn;
        //Column.Name := GEAC.Name;
        Column.Title.Caption := GEAC.Caption;
        if GEAC.Width > 0 then
        begin
          Column.SizePriority:=0;
          Column.Width:=GEAC.Width;
        end;
        CC := CreateControl(GEAC);
      end;
      if Columns.Count = 1 then
      	FixedRows:=0
      else if Columns.Count > 1 then
      begin
        RowCount := 1;
        FixedRows := 1;
      end;
      OnSelectEditor:=@GridSelectEditor;
      OnGetDefaultCellText:=@GridGetDefaultCellText;
      OnDrawCell:=@GridDrawCell;
      if EAC.Height > 0 then
      	Height := EAC.Height;
      AutoFillColumns:=True;
    end;

  if C is TCustomEdit then
    TCustomEdit(C).TextHint:=EAC.TextHint
  else if C is TCustomEditButton then
    TCustomEditButton(C).TextHint:=EAC.TextHint;

  C.Name := EAC.Name;
  Result := C;
end;

function TCustomActionPanel.GetComponentSourceForm(AGrid: TEAControl;
  ARow: Integer; const ASource: String): TdxForm;
var
  i, idx: Integer;
  EAC: TEAControl;
  C: TWinControl;
  Gr: TActionGrid;
begin
  Result := nil;

  if ASource = '' then Exit(FForm);

  idx := AGrid.Controls.FindIndexByName(ASource);
  if idx >= 0 then
  begin
    EAC := AGrid.Controls[idx];
    if EAC.ControlType = eacObject then
    begin
      // Чтобы сработал метод GetSourceForm надо всем компонентам, привязанным
      // к ячейкам задать текст из ячеек.
      Gr := TActionGrid(FindComponent(AGrid.Name));
      for i := 0 to idx do
      begin
	      C := TWinControl(FindComponent(AGrid.Controls[i].Name));
  	    C.Caption := Gr.Cells[i, ARow];
      end;
      Result := TObjectCbx(C).GetSourceForm;
    end
    else
    begin
      C := TWinControl(FindComponent(AGrid.Name));
     	Result := FormMan.FindFormByName( TActionGrid(C).Cells[idx, ARow] );
    end;
  end
  else
  begin
    C := TWinControl(FindComponent(ASource));
    if C is TObjectCbx then
	    Result := TObjectCbx(C).GetSourceForm
    else
    	Result := FormMan.FindFormByName(C.Caption);
  end;
end;

function TCustomActionPanel.InnerCreateControls: TControl;
var
  i: Integer;
  EAC: TEAControl;
  C: TControl;
  L: TLabel;
begin
  Result := nil;
	for i := 0 to FEAction.Controls.Count - 1 do
  begin
		EAC := FEAction.Controls[i];
    if EAC.ControlType = eacNone then Continue;

    C := CreateControl(EAC);
    C.Parent := Self;

    if (C is TActionCheckBox) or (C is TActionDivider) then
    begin
      C.Caption := EAC.Caption;
      //if Result <> nil then
    		AnchorCtrl(C, Result, 4);
    end
    else
    begin
      L := CreateLabel(Self, EAC.Caption);
      if Result <> nil then
    	  AnchorCtrl(L, Result, 4);

      AnchorCtrl(C, L, 1);
    end;
    Result := C;

    if C is TActionGrid then
    	with TActionGrid(C) do
      	Options := Options + [goEditing];
  end;
end;

procedure TCustomActionPanel.Init;
var
  i, j, z, idx, r: Integer;
  EAC, GEAC: TEAControl;
  Props: TActionProps;
  P: TActionProp;
  C, CC, FmC: TComponent;
  V: String;
  Rows, Cols, Titles: TStringList;
  Grid: TActionGrid;
  Fm: TdxForm;
begin
  inherited Init;
  if FEAction = nil then Exit;
  Props := TActionCustom(FAction).Props;
  for i := 0 to FEAction.Controls.Count - 1 do
  begin
    EAC := FEAction.Controls[i];
    P := Props.Find(EAC.Name);
    if P <> nil then V := P.Value
    else V := '';
    C := FindComponent(EAC.Name);
    if C is TActionCbx then
    	TActionCbx(C).Fill;
    if C is TActionCheckBox then
    	TActionCheckBox(C).Checked := Str2Bool(V)
    else if C is TComponentCbx then
    	TComponentCbx(C).SelectComponent(V)
    else if C is TWinControl then
    	TWinControl(C).Caption := V;
    if C is TActionGrid then
    begin
      Grid := TActionGrid(C);
      for j := 0 to EAC.Controls.Count - 1 do
      begin
        GEAC := EAC.Controls[j];
        CC := FindComponent(GEAC.Name);
        if CC is TActionCbx then
        	TActionCbx(CC).Fill;
      end;

      Grid.RowCount:=Grid.FixedRows;
    	if V <> '' then
      begin
        Rows := TStringList.Create;
        Cols := TStringList.Create;
        Titles := TStringList.Create;
        SplitStr(V, '|', Rows);
        // Учитываем, что при одном столбце нет заголовка
        Grid.RowCount := Rows.Count - 1 + Grid.FixedRows;
        //
        SplitStr(Rows[0], ';', Titles);
        for j := 1 to Rows.Count - 1 do
        begin
          SplitStr(Rows[j], ';', Cols);
          for z := 0 to Cols.Count - 1 do
          begin
            // Порядок столбцов и значений может не совпадать. Такое может быть,
            // если разработчик изменил состав и порядок столбцов в новой
            // версии расширения.
            idx := EAC.Controls.FindIndexByName(Titles[z]);
            if idx >= 0 then
            begin
              GEAC := EAC.Controls[idx];
              r := j - 1 + Grid.FixedRows;
              if GEAC.ControlType <> eacComponent then
	            	Grid.Cells[idx, r] := DecodeCellText(Cols[z])
              // Для ui-компонентов особая обработка. Нужно определить форму,
              // которой принадлежит компонент.
              else
              begin
                Fm := GetComponentSourceForm(EAC, r, GEAC.Source);
                if Fm <> nil then
                begin
                  FmC := Fm.FindComponent(Cols[z]);
                  if FmC <> nil then
                  begin
	                  Grid.Cells[idx, r] := GetComponentName(FmC) + ' - ' + GetComponentType(FmC);
                    Grid.Objects[idx, r] := FmC;
                  end;
                end;
              end;
            end;
          end;
        end;
        Titles.Free;
        Cols.Free;
        Rows.Free;
      end;
    end;
  end;
end;

procedure TCustomActionPanel.SaveAction;
var
  Props: TActionProps;
  i, j, z: Integer;
  C, FmC: TComponent;
  EAC, EAC2: TEAControl;
  S: String;
  Grid: TActionGrid;
  Prop: TActionProp;
begin
  Props := TActionCustom(FAction).Props;
  Props.Clear;

  for i := 0 to FEAction.Controls.Count - 1 do
  begin
    EAC := FEAction.Controls[i];
    if EAC.ControlType = eacDivider then Continue;
    C := FindComponent(EAC.Name);
    if C = nil then Continue;
    S := '';
    if C is TActionGrid then
    begin
      Grid := TActionGrid(C);
      for j := 0 to EAC.Controls.Count - 1 do
      begin
        S := S + EAC.Controls[j].Name;
        if j < EAC.Controls.Count - 1 then
          S := S + ';';
      end;
      if Grid.RowCount > Grid.FixedRows then
      begin
        S := S + '|';
        for j := Grid.FixedRows to Grid.RowCount - 1 do
        begin
          for z := 0 to Grid.Columns.Count - 1 do
          begin
            EAC2 := EAC.Controls[z];
            if EAC2.ControlType <> eacComponent then
	            S := S + EncodeCellText(Grid.Cells[z, j])
            else
            begin
              FmC := TComponent(Grid.Objects[z, j]);
              if FmC <> nil then S := S + FmC.Name;
            end;
            if z < Grid.Columns.Count - 1 then
              S := S + ';';
          end;
          if j < Grid.RowCount - 1 then
  					S := S + '|';
        end;
      end;
    end
    else if C is TActionCheckBox then
      S := Bool2Str(TActionCheckBox(C).Checked)
    else if C is TComponentCbx then
    	with TComponentCbx(C) do
      begin
        if ItemIndex > 0 then
        	S := TComponent(Items.Objects[ItemIndex]).Name;
      end
    else if C is TWinControl then
      S := TWinControl(C).Caption;

    Prop := Props.AddProp;
    Prop.Name := C.Name;
    Prop.Value := S;
  end;
end;

function TCustomActionPanel.Validate: Boolean;
var
  AEB: TAExpressionBuilder;
  ChkIdx, ChkGridIdx: Integer;

  function EvalMsg(const Msg: String): String;
  var
    p0, p: SizeInt;
    Expr, S: String;
    E: TAExpression;
  begin
    Result := '';
    p0 := 1;
    repeat
      p := PosEx('`', Msg, p0);
      if p > 0 then
      begin
        Result := Result + Copy(Msg, p0, p - p0);
        p0 := p + 1;
        p := PosEx('`', Msg, p0);
        if p > 0 then
        begin
          Expr := Copy(Msg, p0, p - p0);
          E := nil;
          try
            E := AEB.Build(Expr);
            if E <> nil then S := VarToStr(E.Calc);
          finally
            FreeAndNil(E);
          end;
          Result := Result + S;
          p0 := p + 1;
        end;
      end;
    until p = 0;
    Result := Result + Copy(Msg, p0, MAXINT);
  end;

  function EvalCheckExpr(Chk: TEACheck; DoFocus: Boolean): Boolean;
  var
    E: TAExpression;
    V: Variant;
  begin
    Result := False;
    E := nil;
    try
      E := AEB.Build(Chk.Expr);
      if E <> nil then V := E.Calc;
    finally
      FreeAndNil(E);
    end;
    if VarIsBool(V) then
    begin
      if V = True then
      begin
        CheckMsg(EvalMsg(Chk.Msg), []);
        Result := True;
        if DoFocus and (Chk.FocusControl <> '') then
          FProblemControl := TWinControl(FindComponent(Chk.FocusControl));
      end;
    end
    else
      raise Exception.Create(rsExprNotLogical);
  end;

  function CheckGrid(ACheck: TEACheck): Boolean;
  var
    G: TActionGrid;
    EAC, GEAC: TEAcontrol;
    i, j, n: Integer;
    C: TWinControl;
    Chk: TEACheck;
  begin
    Result := False;
    G := TActionGrid(FindComponent(ACheck.GridControl));
    EAC := FEAction.Controls.FindByName(G.Name);
    for i := G.FixedRows to G.RowCount - 1 do
    begin
      for j := 0 to EAC.Controls.Count - 1 do
      begin
        GEAC := EAC.Controls[j];
        C := TWinControl(FindComponent(GEAC.Name));
        if C is TCheckBox then
          TCheckBox(C).Checked := Str2Bool(G.Cells[j, i])
        else
          C.Caption := G.Cells[j, i];
      end;
      for j := 0 to ACheck.Checks.Count - 1 do
      begin
        Chk := ACheck.Checks[j];
        ChkGridIdx := j+1;
        Result := EvalCheckExpr(Chk, False);
        if Result then
        begin
          if Chk.FocusControl <> '' then
          begin
            n := EAC.Controls.FindIndexByName(Chk.FocusControl);
            if n >= 0 then G.Col := n
            else G.Col := 0;
          end;
          G.Row := i;
          FProblemControl := G;
          Exit;
        end;
      end;
    end;
  end;

var
  i, j, r: Integer;
  EAC, GEAC: TEAControl;
  C: TComponent;
  Grid: TActionGrid;
  Chk: TEACheck;
  S: String;
begin
  FProblemControl := nil;
  if FAction.Disabled then Exit(True);
  Result := False;
  if FEAction = nil then Exit;
  for i := 0 to FEAction.Controls.Count - 1 do
  begin
    EAC := FEAction.Controls[i];
    if EAC.ControlType <> eacGrid then
    begin
	    if EAC.Required then
      begin
        C := FindComponent(EAC.Name);
        if (C is TWinControl) and (TWinControl(C).Caption = '') then
        begin
          //TWinControl(C).SetFocus;
          CheckMsg(rsParamRequired, [EAC.Caption]);
          FProblemControl := TWinControl(C);
          Exit;
        end;
      end;
    end
    else
    begin
      Grid := TActionGrid(FindComponent(EAC.Name));
      for j := 0 to EAC.Controls.Count - 1 do
      begin
        GEAC := EAC.Controls[j];
        if GEAC.Required then
          for r := Grid.FixedRows to Grid.RowCount - 1 do
            if Grid.Cells[j, r] = '' then
            begin
              Grid.Row := r; Grid.Col := j;
              //Grid.SetFocus;
              CheckMsg(rsCellRequired, [GEAC.Caption]);
              FProblemControl := Grid;
              Exit;
            end;
      end;
    end;
  end;

  AEB := TAExpressionBuilder.Create;
  try
    AEB.Panel := Self;
    for i := 0 to FEAction.Checks.Count - 1 do
    begin
      Chk := FEAction.Checks[i];
      ChkIdx := i+1;
      ChkGridIdx := 0;
      if Chk.GridControl = '' then
      begin
        if EvalCheckExpr(Chk, True) then Exit;
      end
      else if CheckGrid(Chk) then Exit;
    end;
  except
    on E: Exception do
    begin
      S := rsErrOccurredInTag;
      if ChkGridIdx > 0 then S := S + Format(rsIfGridAtIndex, [ChkIdx, ChkGridIdx])
      else S := S + Format(rsIfAtIndex, [ChkIdx]);
      ErrMsgFmt(rsValidateActionError, [Spaces + E.Message + Spaces + S]);
    end;
  end;
  AEB.Free;
  Result := True;
end;

procedure TCustomActionPanel.SetDefaultValues;
var
  i: Integer;
  EAC: TEAControl;
  C: TComponent;
  V: String;
begin
  for i := 0 to FEAction.Controls.Count - 1 do
  begin
    EAC := FEAction.Controls[i];
    V := EAC.DefaultValue;
    if V <> '' then
    begin
      C := FindComponent(EAC.Name);
      if C = nil then Continue;
      if C is TActionCheckBox then TActionCheckBox(C).Checked := Str2Bool(V)
      else if C is TWinControl then TWinControl(C).Caption := V;
    end;
  end;
end;

{ TClearFieldsActionPanel }

procedure TClearFieldsActionPanel.MenuHandler(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
  	0:
      begin
        FGrid.RowCount := FGrid.RowCount + 1;
        FGrid.Row := FGrid.RowCount - 1;
      end;
    1:
      begin
        if Confirm(rsDeleteField, rsDeleteFieldMsg) = mrYes then
	        FGrid.DeleteRow(FGrid.Row);
      end;
  end;
end;

procedure TClearFieldsActionPanel.GridSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if Editor is TPickListCellEditor then
  	with TPickListCellEditor(Editor) do
    	Style := csDropDownList;
end;

function TClearFieldsActionPanel.InnerCreateControls: TControl;
var
  L: TLabel;
begin
  L := CreateLabel(Self, rsSelectFields);
  FGrid := TActionGrid.Create(Self);
  with FGrid do
  begin
    Parent := Self;
    AutoFillColumns:=True;
    RowCount := 1;
    Flat := True;
    AlternateColor := $00EEEEEE;
    Columns.Add;
    FixedCols := 0; FixedRows := 0;
    Height := 300;
    Options := Options + [goEditing, goThumbTracking, goDrawFocusSelected] - [goRangeSelect];
		OnSelectEditor:=@GridSelectEditor;
  end;
  AnchorCtrl(FGrid, L, 1);
  Result := FGrid;
end;

procedure TClearFieldsActionPanel.Init;
var
  i, r: Integer;
  C: TComponent;
  SL: TStringListUtf8;
  Fields: TStringList;
begin
  inherited Init;
  r := 0;
  Fields := TClearFieldsAction(FAction).Fields;
  for i := 0 to Fields.Count - 1 do
  begin
    FGrid.RowCount := r + 1;
    FGrid.Cells[0, r] := Fields[i];
    Inc(r);
  end;

  SL := TStringListUtf8.Create;
  for i := 0 to FForm.ComponentCount - 1 do
  begin
    C := FForm.Components[i];
    if IsField(C) then SL.Add(GetFieldName(C));
  end;
  SL.Sort;
  FGrid.Columns[0].PickList := SL;
  SL.Free;
end;

procedure TClearFieldsActionPanel.SaveAction;
var
  i: Integer;
begin
  with TClearFieldsAction(FAction) do
  begin
    Fields.Clear;
    for i := 0 to FGrid.RowCount - 1 do
  	begin
    	Fields.Add(FGrid.Cells[0, i]);
  	end;
  end;
end;

function TClearFieldsActionPanel.Validate: Boolean;
var
  i: Integer;
begin
  if FAction.Disabled then Exit(True);
  Result:=inherited Validate;
  for i := 0 to FGrid.RowCount - 1 do
  begin
    if FGrid.Cells[0, i] = '' then
    begin
      ErrMsg(rsFieldNotSel);
      FGrid.Col := 0; FGrid.Row := i;
      Result := False;
      FProblemControl := FGrid;
      //FGrid.SetFocus;
      Break;
    end;
  end;
end;

{ TCallFuncActionPanel }

function TCallFuncActionPanel.InnerCreateControls: TControl;
var
  L: TLabel;
begin
  L := CreateLabel(Self, rsExpression);
  FExpr := TActionExpr.Create(Self);
  FExpr.Parent := Self;
  FExpr.CurForm := FForm;
  AnchorCtrl(FExpr, L, 1);
  Result := FExpr;
end;

procedure TCallFuncActionPanel.Init;
begin
  inherited Init;
  FExpr.Text := TCallFuncAction(FAction).Expression;
end;

procedure TCallFuncActionPanel.SaveAction;
begin
  with TCallFuncAction(FAction) do
  begin
    Expression:=FExpr.Text;
  end;
end;

function TCallFuncActionPanel.Validate: Boolean;
begin
  Result:=inherited Validate;
end;

{ TOpenReportActionPanel }

function TOpenReportActionPanel.InnerCreateControls: TControl;
var
  L: TLabel;
begin
  L := CreateLabel(Self, rsReport);
  FReps := TReportCbx.Create(Self);
  FReps.Parent := Self;
  AnchorCtrl(FReps, L, 1);
  Result := FReps;
end;

procedure TOpenReportActionPanel.Init;
begin
  inherited Init;
  FReps.Fill;
  FReps.Text := TOpenReportAction(FAction).RpName;
end;

procedure TOpenReportActionPanel.SaveAction;
begin
  with TOpenReportAction(FAction) do
  begin
    RpName:=FReps.Text;
  end;
end;

function TOpenReportActionPanel.Validate: Boolean;
begin
  if FAction.Disabled then Exit(True);
  Result:=False;
  if FReps.Text = '' then
  begin
    ErrMsg(rsReportNotSel);
    FProblemControl := FReps;
    //FReps.SetFocus;
  end
  else
    Result := True;
end;

{ TMassCalcActionPanel }

procedure TMassCalcActionPanel.FormsChange(Sender: TObject);
begin
  FTbl.Fill;
  FFields.SourceCbx := FForms;
  FExpr.SourceCbx := FForms;
  FFields.Fill;
end;

procedure TMassCalcActionPanel.TblChange(Sender: TObject);
begin
  if FTbl.Text <> '' then
  begin
    FFields.SourceCbx := FTbl;
    FExpr.SourceCbx := FTbl;
  end
  else
  begin
    FFields.SourceCbx := FForms;
    FExpr.SourceCbx := FForms;
  end;
  FFields.Fill;
end;

function TMassCalcActionPanel.InnerCreateControls: TControl;
var
  L: TLabel;
begin
  L := CreateLabel(Self, rsForm);
  FForms := TFormCbx.Create(Self);
  FForms.Parent := Self;
  AnchorCtrl(FForms, L, 1);
  FForms.OnChange:=@FormsChange;

  L := CreateLabel(Self, rsSelCond);
  AnchorCtrl(L, FForms, 4);
  FFilter := TActionFilterExpr.Create(Self);
  FFilter.SourceCbx := FForms;
  FFilter.CurForm := FForm;
  FFilter.Parent := Self;
  AnchorCtrl(FFilter, L, 1);

  L := CreateLabel(Self, rsTable);
  AnchorCtrl(L, FFilter, 4);
  FTbl := TChildFormCbx.Create(Self);
  FTbl.Parent := Self;
  FTbl.SourceCbx := FForms;
  AnchorCtrl(FTbl, L, 1);
  FTbl.OnChange:=@TblChange;

  L := CreateLabel(Self, rsField);
  AnchorCtrl(L, FTbl, 4);
  FFields := TFieldCbx.Create(Self);
  FFields.Parent := Self;
  FFields.SourceCbx := FForms;
  FFields.Filter:='text;number;date;time;counter;object;checkbox';
  AnchorCtrl(FFields, L, 1);

  L := CreateLabel(Self, rsExpression);
  AnchorCtrl(L, FFields, 4);
  FExpr := TActionExpr.Create(Self);
  FExpr.CurForm := FForm;
  FExpr.SourceCbx := FForms;
  FExpr.Parent := Self;
  AnchorCtrl(FExpr, L, 1);

  Result := FExpr;
end;

procedure TMassCalcActionPanel.Init;
var
  A: TMassCalcAction;
begin
  inherited Init;
  A := TMassCalcAction(FAction);
  FForms.Fill;
  FForms.Text := A.FormName;
  FTbl.Fill;
  FTbl.Text := A.TableName;
  FTbl.OnChange(FTbl);
  //FFields.Fill;
  FFields.Text := A.FieldName;
  FFilter.Text := A.Filter;
  FExpr.Text := A.Expression;
end;

procedure TMassCalcActionPanel.SaveAction;
begin
  with TMassCalcAction(FAction) do
  begin
    FormName:=FForms.Text;
    Filter:=FFilter.Text;
    TableName:=FTbl.Text;
    FieldName:=FFields.Text;
    Expression:=FExpr.Text;
  end;
end;

function TMassCalcActionPanel.Validate: Boolean;
begin
  if FAction.Disabled then Exit(True);
  Result:=False;
  if FForms.Text = '' then
  begin
    ErrMsg(rsFormNotSel);
    FProblemControl := FForms;
    //FForms.SetFocus;
  end
  else if FFields.Text = '' then
  begin
    ErrMsg(rsFieldNotSel);
    FProblemControl := FFields;
    //FFields.SetFocus;
  end
  else
    Result := True;
end;

{ TPrintActionPanel }

function TPrintActionPanel.InnerCreateControls: TControl;
var
  L: TLabel;
begin
  L := CreateLabel(Self, rsTemplateFile);
  FTempls := TTemplateCbx.Create(Self);
  FTempls.Parent := Self;
  AnchorCtrl(FTempls, L, 1);

  L := CreateLabel(Self, rsOrExpression);
  AnchorCtrl(L, FTempls, 4);
  FExpr := TActionExpr.Create(Self);
  FExpr.Parent := Self;
  FExpr.CurForm := FForm;
  AnchorCtrl(FExpr, L, 1);

  L := CreateLabel(Self, rsSaveToFile);
  AnchorCtrl(L, FExpr, 4);
  FOutFile := TActionExpr.Create(Self);
  FOutFile.Parent := Self;
  FOutFile.CurForm := FForm;
  FOutFile.TextHint:=rsSaveToOutputFolder;
  AnchorCtrl(FOutFile, L, 1);

  L := CreateLabel(Self, rsFileAction);
  AnchorCtrl(L, FOutFile, 4);
  FFileAction := TActionCbx.Create(Self);
  FFileAction.Parent := Self;
  FFileAction.Items.AddStrings([rsNoAction, rsOpenWithApp, rsSentToPrinter]);
  AnchorCtrl(FFileAction, L, 1);

  FSaveRecord := TCheckBox.Create(Self);
  FSaveRecord.Parent := Self;
  FSaveRecord.Caption := rsSaveRecordBeforePrint;
  AnchorCtrl(FSaveRecord, FFileAction, 4);

  Result := FSaveRecord;
end;

procedure TPrintActionPanel.Init;
begin
  inherited Init;
  with TPrintAction(FAction) do
  begin
    FTempls.Fill;
    FTempls.Text := TemplateFile;
    FExpr.Text := Expression;
    FOutFile.Text := OutFile;
    FFileAction.ItemIndex := Ord(FileAction);
    FSaveRecord.Checked := SaveRecord;
  end;
end;

procedure TPrintActionPanel.SaveAction;
begin
  with TPrintAction(FAction) do
  begin
    TemplateFile:=FTempls.Text;
    Expression:=FExpr.Text;
    OutFile:=FOutFile.Text;
    FileAction:=TPrintFileAction(FFileAction.ItemIndex);
    SaveRecord:=FSaveRecord.Checked;
  end;
end;

function TPrintActionPanel.Validate: Boolean;
begin
  if FAction.Disabled then Exit(True);
  Result:=False;
  if (Trim(FTempls.Text) = '') and (Trim(FExpr.Text) = '') then
  begin
    ErrMsg(rsTemplateUnknown);
    FProblemControl := FTempls;
    //FTempls.SetFocus;
  end
  else
    Result := True;
end;

{ TGotoFormPanel }

function TGotoFormPanel.InnerCreateControls: TControl;
var
  L: TLabel;
begin
  L := CreateLabel(Self, rsForm);
  FForms := TFormCbx.Create(Self);
  FForms.Parent := Self;
  AnchorCtrl(FForms, L, 1);

  Result := FForms;
end;

procedure TGotoFormPanel.Init;
begin
  inherited Init;
  FForms.Fill;
  FForms.Text := TGotoFormAction(FAction).FormName;
end;

procedure TGotoFormPanel.SaveAction;
begin
  with TGotoFormAction(FAction) do
  begin
    FormName := FForms.Text;
  end;
end;

function TGotoFormPanel.Validate: Boolean;
begin
  if FAction.Disabled then Exit(True);
  Result := False;
  if FForms.Text = '' then
  begin
    ErrMsg(rsFormNotSel);
    FProblemControl := FForms;
    //FForms.SetFocus;
  end
  else
    Result := True;
end;

{ TBasicActionPanel }

{procedure TBasicActionPanel.ExecCondClick(Sender: TObject);
var
  S: TCaption;
begin
  S := FExecCond.Text;
  if ShowExprForm(rsExecCond, nil, S, FForm, nil, nil, nil, 'expression') then
    FExecCond.Text := S;
end;   }

procedure TBasicActionPanel.TimerTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  FProblemIcon.Hide;
end;

function TBasicActionPanel.InnerCreateControls: TControl;
begin
  Result := nil;
end;

procedure TBasicActionPanel.DeleteControls;
var
  i: Integer;
begin
  for i := ControlCount - 1 downto 0 do
  	Controls[i].Free;
end;

constructor TBasicActionPanel.CreatePanel(AOwner: TComponent;
  AAction: TBaseAction; AForm: TdxForm);
begin
  inherited Create(AOwner);
  FForm := AForm;
  FAction := AAction;

  BevelInner := bvNone;
  BevelOuter := bvNone;
  Caption := '';
  FProblemIcon := TProblemIcon.Create(Self);
  FProblemIcon.Parent := Self;
  FProblemIcon.Visible := False;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 2000;
  FTimer.OnTimer := @TimerTimer;
end;

procedure TBasicActionPanel.CreateControls;
begin
  //try
	  InnerCreateControls;
  {except
    on E: Exception do
    begin
      DeleteControls;
    	ErrMsgFmt(rsFailedToInitActionCmp, [E.Message]);
    end;
  end; }
end;

destructor TBasicActionPanel.Destroy;
begin
  if FCreateAction then
	  FAction.Free;
  inherited Destroy;
end;

{procedure TBasicActionPanel.Load(const Xml: String);
begin
  FAction.Load(Xml);
end; }

procedure TBasicActionPanel.Init;
begin
end;

procedure TBasicActionPanel.SaveAction;
begin

end;

function TBasicActionPanel.Validate: Boolean;
begin
  Result := True;
end;

procedure TBasicActionPanel.FocusProblem;
var
  Box: TScrollBox;
begin
  if FProblemControl <> nil then
  begin
    Box := TScrollBox(Parent);
    Box.ScrollInView(FProblemControl);

    FProblemIcon.Show;
    FProblemIcon.Left := ClientWidth - FProblemIcon.Width;
    if FProblemControl is TCheckBox then
      FProblemIcon.Top := FProblemControl.Top
    else
      FProblemIcon.Top := FProblemControl.Top - FProblemIcon.Height;
    if FProblemIcon.Top < Box.VertScrollBar.Position then
      Box.VertScrollBar.Position := Box.VertScrollBar.Position - FProblemIcon.Height;
    FTimer.Enabled := True;

    if FProblemControl.CanFocus then FProblemControl.SetFocus;
  end;
end;

end.

