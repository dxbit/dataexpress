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
unit ListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, dxctrls, strconsts, Db, ExtCtrls, ButtonPanel,
  ComCtrls, Controls, LclType;

type

  { TListWindow }

  TListWindow = class(TWindow)
    procedure GridDblClick(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StateChange(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
  private
    FButtons: TButtonPanel;
    FDestMemo: TdxMemo;
    FDSProc: TObject;
    FDSRi: Integer;
    FFilter: String;
    FFm: TdxForm;
    FKey: Variant;
    FDestDS: TDataSet;
    FDestForm: TdxForm;
    FToolbar: TToolbar;
    FValue: Variant;
    FValueField: String;
    FVw: TCustomPanel;
    FAppendBn, FEditBn, FDeleteBn: TToolButton;
    FTreeDblClicked: Boolean;
    procedure DoShopping;
    procedure DoPicking;
    procedure TreeDblClick(Sender: TObject);
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
    property ValueField: String read FValueField write FValueField;
    property Value: Variant read FValue;
    property Filter: String read FFilter write FFilter;
    property DestForm: TdxForm read FDestForm write FDestForm;
    property DestDS: TDataSet read FDestDS write FDestDS;
    property DestMemo: TdxMemo read FDestMemo write FDestMemo;
    property DSProc: TObject read FDSProc write FDSProc;
    property DSRi: Integer read FDSRi write FDSRi;
  public
    constructor CreateWindow(const aFormName: String; aViewType: TViewType);
    property Buttons: TButtonPanel read FButtons;
    property Toolbar: TToolbar read FToolbar;
    property FormView: TCustomPanel read FVw;
  end;

implementation

uses
  formview, datasetprocessor, lists, apputils, sqlgen, inputform, formmanager,
  dbengine;

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

  if FDestForm <> nil then DoShopping
  else if FDestMemo <> nil then DoPicking
  else
  begin
    with Vw(FVw).DataSetProc.MasterSet do
      if Fields[0].IsNull = False then
        ModalResult := mrOk;
  end;
end;

procedure TListWindow.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    GridDblClick(nil);
    Key := 0;
  end;
end;

procedure TListWindow.StateChange(Sender: TObject);
var
  DSP: TDataSetProcessor;
begin
  // !!! Доступ
  DSP := TDataSetProcessor(Sender);
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
    (not DSP.MasterSet.Fields[0].IsNull) and DSP.CanDelete and DSP.CanEdit;
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
      if InputFloat(QC.FieldName, rsEnterValue, '', S, QC.MinValue, QC.MaxValue) then
        Qtty := StrToFloat(S)
      else Exit;
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
      if InputFloat(PC.FieldName, rsEnterValue, '', S, PC.MinValue, PC.MaxValue) then
        Price := StrToFloat(S)
      else Exit;
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
    ObjFl.Value := ObjKey;
    ObjFlT.Value := ObjDS.FieldByName(FValueField).AsString;
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
  Fl, SrcFl: TField;
  DS: TDataSet;
  S: String;
begin
  Fl := FDestMemo.Field;
  DS := Fl.DataSet;
  SrcFl :=  Vw(FVw).DataSetProc.MasterSet.FieldByName(FieldStr(FDestMemo.SourceFId));
  if not (DS.State in [dsInsert, dsEdit]) then
    DS.Edit;
  S := Fl.AsString;
  if Trim(S) <> '' then S := S + FDestMemo.Delimiter;
  Fl.AsString:=S + SrcFl.AsString;
end;

procedure TListWindow.TreeDblClick(Sender: TObject);
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
end;

procedure TListWindow.DoShow;
begin
  FFm.Grid.SetFocus;
  inherited DoShow;
end;

constructor TListWindow.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  Width := 700; Height := 420;
  Position := poOwnerFormCenter;
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
    OKButton.Caption:=rsOk;
    CancelButton.Caption := rsCancel;
    CloseButton.Caption:=rsClose;
  end;
end;

constructor TListWindow.CreateWindow(const aFormName: String;
  aViewType: TViewType);
var
  Fm: TdxForm;
begin
  Fm := FormMan.FindFormByName(aFormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [aFormName]);
  if Fm.PId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [aFormName]);
  CreateNew(nil);
  Load2(Fm.Id, aViewType);
end;

procedure TListWindow.Load(TId: Integer);
begin
  Vw(FVw).BindForm(TId, True, vtDefault);
  FFm := Vw(FVw).Form;
  FFm.Grid.ReadOnly := True;
  FFm.Grid.OnDblClick:=@GridDblClick;
  FFm.Grid.OnKeyDown:=@GridKeyDown;
  Vw(FVw).DataSetProc.OnStateChange:=@StateChange;
  Vw(FVw).OnTreeDblClick:=@TreeDblClick;
  if FFm.GroupField > 0 then
    Width := Width + FFm.TreeWidth;
end;

procedure TListWindow.Load2(TId: Integer; aViewType: TViewType);
begin
  Vw(FVw).BindForm(TId, False, aViewType);
  FFm := Vw(FVw).Form;
  Vw(FVw).DataSetProc.OnStateChange:=@StateChange;
  if FFm.GroupField > 0 then
    Width := Width + FFm.TreeWidth;
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
  gid: Integer;
  S: String;
  DSP: TDataSetProcessor;
begin
  if FFm = nil then Exit(mrNone);
  FTreeDblClicked := False;
  if (FDestForm = nil) and (FDestMemo = nil) then
    FButtons.ShowButtons := [pbOk, pbCancel]
  else
    FButtons.ShowButtons := [pbClose];
  DSP := Vw(FVw).DataSetProc;

  Vw(FVw).ClearSelection(False);
  DSP.DataSets[0]^.Filter.Clear;
  if (FDestForm <> nil) or (FKey = Null) then
  begin
	  DSP.OpenList(FFilter);
    DSP.MasterSet.First;
  end
  else
  begin
    gid := 0;
    if FFm.GroupField > 0 then
    begin
      gid := GetGroupId(FFm.Id, FFm.GroupField, FKey);
      //gid := DataSetProc.MasterSet.FieldByName(FieldStr(Form.GroupField)).AsInteger;
      if gid > 0 then
      begin
        Vw(FVw).SelectGroup(gid);
        //DSP.MasterSet.Locate('id', FKey, []);
      end;
    end;
    if gid = 0 then DSP.OpenList(FFilter);
    DSP.MasterSet.Locate('id', FKey, []);
  end;
  if Self.Caption = '' then Self.Caption := FFm.FormCaption;
  Result := ShowModal;
  if Result = mrOk then
    if (FDestForm = nil) and (not FTreeDblClicked) then
      with DSP.MasterSet do
      begin
        FKey := Fields[0].Value;
        FValue := FieldByName(FValueField).Value;
        if FFm.ParentField > 0 then
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

