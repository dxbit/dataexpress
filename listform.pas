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

unit ListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Forms, dxctrls, strconsts, sqldb, Db, ExtCtrls,
  ButtonPanel, ComCtrls, Controls, LclType, LclIntf;

type

  { TListWindow }

  TListWindow = class(TWindow)
    procedure GridDblClick(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridKeyDown2(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StateChange(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
  private
    FButtons: TButtonPanel;
    //FCallerObject: TdxLookupComboBox;
    FDestMemo: TdxMemo;
    FDSProc: TObject;
    FDSRi: Integer;
    FFm: TdxForm;
    FKey: Variant;
    FDestDS: TDataSet;
    FDestForm: TdxForm;
    FToolbar: TToolbar;
    FUpdateTree: Boolean;
    FValue: Variant;
    FValueField: Integer;
    FVw: TCustomPanel;
    FAppendBn, FEditBn, FDeleteBn: TToolButton;
    //FTreeDblClicked: Boolean;
    procedure DoShopping;
    procedure DoPicking;
    procedure AcceptSelection;
    //procedure TreeDblClick(Sender: TObject);
    procedure PositionPickForm;
  protected
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    procedure Load(TId: Integer);
    procedure Load2(TId: Integer; aViewType: TViewType);
    procedure Done;
    function ShowForm: Integer;
    procedure RefreshLookups(TId: Integer);
    property Key: Variant read FKey write FKey;
    property ValueField: Integer read FValueField write FValueField;
    property Value: Variant read FValue;
    //property CallerObject: TdxLookupComboBox read FCallerObject write FCallerObject;
    property DestForm: TdxForm read FDestForm write FDestForm;
    property DestDS: TDataSet read FDestDS write FDestDS;
    property DestMemo: TdxMemo read FDestMemo write FDestMemo;
    property DSProc: TObject read FDSProc write FDSProc;
    property DSRi: Integer read FDSRi write FDSRi;
    property UpdateTreeWhenShow: Boolean read FUpdateTree write FUpdateTree;
  public
    class function CreateListWindow(const aFormName: String; aViewType: TViewType): TListWindow;
    property Buttons: TButtonPanel read FButtons;
    property Toolbar: TToolbar read FToolbar;
    property FormView: TCustomPanel read FVw;
  end;

implementation

uses
  formview, datasetprocessor, lists, apputils, sqlgen, inputform, formmanager,
  dbengine, LazUtf8;

function Vw(aVw: TCustomPanel): TFormView;
begin
  Result := TFormView(aVw);
end;

function CreateToolButton(Toolbar: TToolbar; ImageIndex: Integer; const Hint: String;
  Tag: Integer; Handler: TNotifyEvent): TToolButton;
begin
  Result := TToolButton.Create(Toolbar);
  Result.Parent := Toolbar;
  Result.ImageIndex := ImageIndex;
  Result.Hint := Hint;
  Result.Tag := Tag;
  Result.OnClick:=Handler;
end;

{ TListWindow }

procedure TListWindow.GridDblClick(Sender: TObject);
begin
  with Vw(FVw).Grid do
  	if MouseToCell(ScreenToClient(Mouse.CursorPos)).Y = 0 then Exit;

  AcceptSelection;
end;

procedure TListWindow.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    AcceptSelection;
    Key := 0;
  end
  else if Key = VK_ESCAPE then
  begin
    if FButtons.CancelButton.Visible then
	  	ModalResult := mrCancel
    else ModalResult := mrClose;
  end;
end;

procedure TListWindow.GridKeyDown2(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    if FButtons.CancelButton.Visible then
	  	ModalResult := mrCancel
    else ModalResult := mrClose;
  end;
end;

procedure TListWindow.StateChange(Sender: TObject);
var
  DSP: TDataSetProcessor;
  DS: TSQLQuery;
  bEdit, bNotEdit, bHasRec: Boolean;
  DSR: TDataSetRec;
begin
  DSP := TDataSetProcessor(Sender);
  DSR := DSP.DataSets[0]^;
  DS := DSR.DataSet;

  bEdit := DS.State in [dsInsert, dsEdit];
  bNotEdit := not bEdit;
  bHasRec := DS.RecordCount > 0;

  if DSR.CanEdit then
  begin
    FEditBn.ImageIndex := 1;
    FEditBn.Hint := rsEdit;
  end
  else
  begin
    FEditBn.ImageIndex := 2;
    FEditBn.Hint := rsLook;
  end;

  FAppendBn.Enabled := DSR.Adding and bNotEdit;
  FEditBn.Enabled := bNotEdit and bHasRec;
  FDeleteBn.Enabled := DSR.Deleting and DSR.Editing and bNotEdit and bHasRec;


  // !!! Доступ
  {DSP := TDataSetProcessor(Sender);
  FAppendBn.Enabled:=DSP.CanAdd;
  FEditBn.Enabled:=(not (DSP.MasterSet.State in [dsInsert, dsEdit])) and
  	(DSP.MasterSet.RecordCount > 0);
  if DSP.CanEdit then
  begin
    FEditBn.Hint:=rsEdit;
    FEditBn.ImageIndex := 1;
  end
  else
  begin
    FEditBn.Hint := rsLook;
    FEditBn.ImageIndex := 2;
  end;
  FDeleteBn.Enabled := DSP.MasterSet.Active and
    (not DSP.MasterSet.Fields[0].IsNull) and DSP.CanDelete and DSP.CanEdit; }
end;

procedure TListWindow.ToolButtonClick(Sender: TObject);
var
  V: TFormView;
begin
  V := Vw(FVw);
  case TComponent(Sender).Tag of
    0: V.DataSetProc.Append;
    1: V.DataSetProc.Edit;
    2: V.DataSetProc.Delete;
  end;
end;

procedure TListWindow.DoShopping;
var
  SD: TShopData;
  Fm, ObjFm: TdxForm;
  DS, ObjDS: TDataSet;
  QC, PC, ObjPC: TdxCalcEdit;
  S: String;
  QFl, PFl, ObjFl, ObjFlT, ObjPFl: TField;
  ObjC: TdxLookupComboBox;
  Qtty, Price: Extended;
  ObjKey: Integer;
begin
  Fm := FDestForm;
  ObjFm := FFm;
  DS := FDestDS;
  ObjDS := Vw(FVw).DataSetProc.MasterSet;
  SD := Fm.ShopData;
  QC := TdxCalcEdit(FindById(Fm, SD.QttyFId));
  PC := TdxCalcEdit(FindById(Fm, SD.PriceFId));
  ObjC := TdxLookupComboBox(FindById(Fm, SD.ObjId));
  TestNil(ObjC, 'DoShopping: ObjC = nil');
  ObjFl := DS.FieldByName(FieldStr(ObjC));
  ObjFlT := DS.FieldByName(FieldStr(ObjC) + 'l');

  if QC <> nil then
  begin
    QFl := DS.FieldByName(FieldStr(QC));
    S := '1';
    if SD.QttyInput then
    begin
      if QC.Precission = 0 then
      begin
        if InputInt(QC.FieldName, rsEnterValue, '', S, Trunc(QC.MinValue), Trunc(QC.MaxValue)) then
          Qtty := StrToFloat(S)
        else Exit;
      end
      else
      begin
        if InputFloat(QC.FieldName, rsEnterValue, '', S, QC.MinValue, QC.MaxValue) then
          Qtty := StrToFloat(S)
        else Exit;
      end;
    end
    else Qtty := 1;
  end;

  if PC <> nil then
  begin
    PFl := DS.FieldByName(FieldStr(PC));
    ObjPC := TdxCalcEdit(FindById(ObjFm, SD.PriceObjFId));
    S := '0';
    if ObjPC <> nil then
    begin
      ObjPFl := ObjDS.FieldByName(FieldStr(ObjPC));
      S := ObjPFl.AsString;
    end;
    if SD.PriceInput then
    begin
      if PC.Precission = 0 then
      begin
        if InputInt(PC.FieldName, rsEnterValue, '', S, Trunc(PC.MinValue), Trunc(PC.MaxValue)) then
          Price := StrToFloat(S)
        else Exit;
      end
      else
      begin
        if InputFloat(PC.FieldName, rsEnterValue, '', S, PC.MinValue, PC.MaxValue) then
          Price := StrToFloat(S)
        else Exit;
      end;
    end
    else Price := StrToFloat(S);
  end;

  ObjKey := ObjDS.Fields[0].AsInteger;
  if SD.AddToExisting then
  begin
    if DS.Locate(ObjFl.FieldName, ObjKey, []) then
      DS.Edit
    else DS.Append;
  end
  else DS.Append;
  if DS.State = dsInsert then
  begin
    ObjFlT.Value := ObjDS.FieldByName(FieldStr(FValueField)).AsString;
    ObjFl.Value := ObjKey;
  end;
  if QC <> nil then
  begin
    if DS.State = dsEdit then
      QFl.AsFloat := QFl.AsFloat + Qtty
    else
      QFl.Value := Qtty;
  end;
  if PC <> nil then PFl.Value := Price;
  if TDataSetProcessor(FDSProc).Validate(FDSRi) then DS.Post
  else DS.Cancel;
end;

procedure TListWindow.DoPicking;
var
  SrcFl: TField;
  S: String;
begin
  SrcFl :=  Vw(FVw).DataSetProc.MasterSet.FieldByName(FieldStr(FDestMemo.SourceFId));
  S := FDestMemo.Text;
  if Trim(S) <> '' then S := S + FDestMemo.Delimiter;
  S := S + SrcFl.AsString;
  if (FDestMemo.FieldSize > 0) and (Utf8Length(S) > FDestMemo.FieldSize) then
    Info(rsTextLengthExceedMemo)
  else
  begin
    FDestMemo.Text := S;
    FDestMemo.SelStart := Utf8Length(S);
  end;
  {$ifdef linux}
  // Если добавить только одну строчку, то событие change почему-то не возникает.
  // Из-за этого текст в поле не сохраняется. Вызываем событие принудительно.
  FDestMemo.Change;
  {$endif}
end;

procedure TListWindow.AcceptSelection;
begin
  if FDestForm <> nil then DoShopping
  else if FDestMemo <> nil then DoPicking
  else
  begin
    with Vw(FVw).DataSetProc.MasterSet do
      if Fields[0].IsNull = False then
        ModalResult := mrOk;
  end;
end;

procedure TListWindow.PositionPickForm;
var
  P: TPoint;
  {SH, SW, }N: Integer;
  M: TMonitor;
  R, MemoR, WR: TRect;
begin
  AutoPosition := False;
  Position := poDesigned;
  //with FDestMemo do
  //  P := Parent.ClientToScreen(Point(Left, Top));
  M := Screen.MonitorFromWindow(FDestMemo.Handle);
  if M = nil then Exit;
  GetWindowRect(FDestMemo.Handle, MemoR);
  R := M.WorkareaRect;
  GetWindowBounds(Self, WR);

  // Поместиться ли окно снизу заметки?
  if MemoR.Bottom + WR.Height <= R.Bottom then
    Top := MemoR.Bottom
  // Поместиться ли окно сверху заметки?
  else if MemoR.Top - WR.Height >= R.Top then
    Top := MemoR.Top - WR.Height
  // Если снизу заметки больше места, чем сверху
  else if R.Bottom - MemoR.Bottom > MemoR.Top - R.Top then
  begin
    Top := MemoR.Bottom;
    Height := R.Bottom - MemoR.Bottom - GetSystemMetrics(SM_CYCAPTION) -
      GetSystemMetrics(SM_CYFRAME) * 2;
  end
  // Если сверху все-таки больше места
  else
  begin
    Top := R.Top;
    Height := (MemoR.Top - R.Top) - GetSystemMetrics(SM_CYCAPTION) -
      GetSystemMetrics(SM_CYFRAME) * 2;
  end;

  if MemoR.Left + WR.Width > R.Right then
    Left := R.Right - WR.Width
  else
    Left := MemoR.Left;
  if Left < R.Left then
    Left := R.Left;
  if WR.Width > R.Width then
    Width := R.Width - GetSystemMetrics(SM_CXFRAME) * 2;

  //SH := Screen.Height - 50;
  {if P.y + FDestMemo.Height + Height < SH then
    Top := P.y + FDestMemo.Height
  else if Height + 50 < P.y then
    Top := P.y - Height - 50
  else if SH - (P.y + FDestMemo.Height) > P.y then
  begin
    Top := P.y + FDestMemo.Height;
    Height := SH - Top;
  end
  else
  begin
    Top := 0; Height := P.y - 50;
  end;

  SW := Screen.Width;
  N := P.x - 7;
  if N + Width > SW then N := SW - Width - 8
  else if N < 0 then N := -7;
  Left := N;  }
end;

{procedure TListWindow.TreeDblClick(Sender: TObject);
var
  N: TTreeNode;
  Tree: TTreeView;
  S: String;
begin
  if FDestForm <> nil then Exit
  else if FFm.ParentField <> FFm.GroupField then Exit;

  Tree := Vw(FVw).Tree;
  N := Tree.Selected;
  if (N <> nil) and (N.Data <> nil) and (N.Count = 0) then
  begin
  	FKey := PtrInt(N.Data);
    S := '';
    while N <> nil do
    begin
    	S := N.Text + '\' + S;
      N := N.Parent;
    end;
    FValue := Copy(S, 1, Length(S) - 1);
    FTreeDblClicked := True;
    ModalResult := mrOk;
  end;
end;    }

procedure TListWindow.DoShow;
begin
  if FFm.Grid.CanFocus then FFm.Grid.SetFocus;
  inherited DoShow;
end;

constructor TListWindow.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  //Position := poDesigned;//poMainFormCenter;
  Width := 700; Height := 420;
  BorderIcons := [biSystemMenu, biMaximize];
  FToolbar := TToolbar.Create(Self);
  with FToolbar do
  begin
    Parent := Self;
    Align := alTop;
    EdgeBorders:=[];
    Images := TImageList.Create(Self);
    ButtonHeight:=32;
    ButtonWidth:=32;
    ShowHint:=True;
    List:=True;
    AutoSize := True;
  end;
  with FToolbar.Images do
  begin
    Width := 24;
    Height:=24;
    AddLazarusResource('add24');
    AddLazarusResource('edit_24');
    AddLazarusResource('eyes24');
    AddLazarusResource('delete24');
  end;
  FAppendBn := CreateToolButton(FToolbar, 0, rsAppend, 0, @ToolButtonClick);
  FEditBn := CreateToolButton(FToolbar, 1, rsEdit, 1, @ToolButtonClick);
  FDeleteBn := CreateToolButton(FToolbar, 3, rsDelete, 2, @ToolButtonClick);
  FVw := TFormView.Create(Self);
  with Vw(FVw) do
  begin
    Parent := Self;
    Align := alClient;
    BorderSpacing.Left := 4;
    BorderSpacing.Top := 4;
    BorderSpacing.Right := 4;
  end;
  FButtons := TButtonPanel.Create(Self);
  FButtons.ShowButtons:=[pbClose];
  with FButtons do
  begin
    Parent := Self;
    ShowHint := True;
    OKButton.Caption:=rsOk;
    OKButton.Hint := rsAcceptSelectionHint;
    CancelButton.Caption := rsCancel;
    CancelButton.Hint := rsCancelSelectionHint;
    CloseButton.Caption:=rsClose;
    CloseButton.Hint := rsCloseListWindowHint;
  end;
end;

class function TListWindow.CreateListWindow(const aFormName: String;
  aViewType: TViewType): TListWindow;
var
  Fm: TdxForm;
begin
  Fm := FormMan.FindFormByName(aFormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [aFormName]);
  if Fm.PId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [aFormName]);
  Result := CreateNew(nil);
  Result.Load2(Fm.Id, aViewType);
end;

procedure TListWindow.Load(TId: Integer);
begin
  Vw(FVw).BindForm(TId, True, vtDefault);
  FFm := Vw(FVw).Form;
  FFm.Grid.ReadOnly := True;
  FFm.Grid.OnDblClick:=@GridDblClick;
  FFm.Grid.OnKeyDown:=@GridKeyDown;
  Vw(FVw).DataSetProc.OnStateChange:=@StateChange;
  if FFm.Tree.Visible then
    Width := Width + FFm.Tree.Width;
end;

procedure TListWindow.Load2(TId: Integer; aViewType: TViewType);
begin
  Vw(FVw).BindForm(TId, False, aViewType);
  FFm := Vw(FVw).Form;
  FFm.Grid.OnKeyDown:=@GridKeyDown2;
  Vw(FVw).DataSetProc.OnStateChange:=@StateChange;
  if FFm.Tree.Visible then
    Width := Width + FFm.Tree.Width;
end;

procedure TListWindow.Done;
begin

end;

function GetGroupId(FmId, GroupFieldId, RecId: Integer): Integer;
var
  SQL: String;
begin
  SQL := 'select ' + FieldStr(GroupFieldId) + ' from ' + TableStr(FmId) +
  	' where id=' + IntToStr(RecId);
  with DBase.OpenDataSet(SQL) do
  try
    Result := Fields[0].AsInteger;
  finally
    Free;
  end;
end;

function TListWindow.ShowForm: Integer;
var
  S: String;
  DSP: TDataSetProcessor;
begin
  if FFm = nil then Exit(mrNone);
  if (FDestForm = nil) and (FDestMemo = nil) then
    FButtons.ShowButtons := [pbOk, pbCancel]
  else
    FButtons.ShowButtons := [pbClose];
  DSP := Vw(FVw).DataSetProc;

  if FUpdateTree then FFm.Tree.UpdateTree;
  DSP.DataSets[0]^.Filter.Clear;
  if (FDestForm <> nil) or (FKey = Null) or (FDestMemo <> nil) then
  begin
    if FDestMemo <> nil then PositionPickForm;
    if FFm.Tree.Visible then
    	FFm.Tree.UpdateSelection;
    if not DSP.Opened then
      try
        DSP.Open;
      except
        on E: ESQLSelectStatementError do
          raise;
      end;
    DSP.MasterSet.First;
  end
  else
  begin
    if FFm.Tree.Visible then
    	FFm.Tree.SelectByRecord(FKey);
    if not DSP.Opened then
      DSP.Open;
    DSP.MasterSet.Locate('id', FKey, []);
  end;
  if Self.Caption = '' then Self.Caption := FFm.FormCaption;
  Result := ShowModal;
  if Result = mrOk then
    if (FDestForm = nil) {and (not FTreeDblClicked)} then
      with DSP.MasterSet do
      begin
        FKey := Fields[0].Value;
        FValue := FieldByName(FieldStr(FValueField)).Value;
        if GetFormParentFieldFieldId(FFm) = FValueField then
        begin
          S := FieldByName(FieldStr(FFm.ParentField) + 'l').AsString;
          if S <> '' then FValue := S + '\' + FValue;
        end;
      end;
  DSP.Close;
end;

procedure TListWindow.RefreshLookups(TId: Integer);
begin
  Vw(FVw).DataSetProc.RefreshLookups(TId);
end;

end.

