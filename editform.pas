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

unit EditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ButtonPanel, strconsts, dxctrls, DbCtrls,
  StdCtrls, Controls, Db, SqlDb, Dialogs, LclType, Buttons, LCLIntf;

type

  { TEditWindow }

  TEditWindow = class(TWindow)
    procedure HelpButtonClick(Sender: TObject);
  private
    FButtons: TButtonPanel;
    FDataSet: TSQLQuery;
    FDSP: TObject;
    FDSRi: Integer;
    FForm: TdxForm;
    FScrollBox: TScrollBox;
    procedure CheckCbx;
    //procedure CorrectSize;
    procedure SetForm(AValue: TdxForm);
    //procedure SetFocusControl(GoForward: Boolean);
  protected
    procedure DoShow; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function CloseQuery: boolean; override;
    function ShowModal: Integer; override;
    procedure Show;
    function ShowForm: Integer;
    property Form: TdxForm read FForm write SetForm;
    property DataSet: TSQLQuery read FDataSet write FDataSet;
    property DSP: TObject read FDSP write FDSP;
    property DSRi: Integer read FDSRi write FDSRi;
  public
    property Buttons: TButtonPanel read FButtons;
    property ScrollBox: TScrollBox read FScrollBox;
  end;

implementation

uses
  helpviewform, apputils, datasetprocessor;

{ TEditWindow }

procedure TEditWindow.HelpButtonClick(Sender: TObject);
begin
  ShowHelpForm(FForm.HelpText.Text);
end;

procedure TEditWindow.CheckCbx;
var
  i: Integer;
  C: TComponent;
begin
  for i := 0 to FForm.ComponentCount - 1 do
  begin
    C := FForm.Components[i];
    if C is TCustomDBComboBox then
      with TCustomDBComboBox(C) do
        if Style = csDropDownList then Text := Field.AsString;
  end;
end;

{procedure TEditWindow.CorrectSize;
var
  WR, R: TRect;
  M: TMonitor;
begin
  M := Screen.MonitorFromRect(BoundsRect);
  if M = nil then Exit;
  R := M.WorkareaRect;

  if Height > R.Height then
  begin
    Top := R.Top; Height := R.Height;
    GetWindowRect(Handle, WR);
    Height := Height - (WR.Height - Height);
  end;
  if Width > R.Width then
  begin
    Left := R.Left; Width := R.Width;
    GetWindowRect(Handle, WR);
    Width := Width - (WR.Width - Width);
  end;
end; }

procedure TEditWindow.SetForm(AValue: TdxForm);
var
  FormW, FormH: Integer;
begin
  FForm:=AValue;
  Caption := FForm.GetRecordCaption;
  FormW := FForm.Width;
  FormH := FForm.Height;
  if not FForm.ShowScrollBars then
  begin
    FScrollBox.AutoScroll := False;
    FForm.Align := alClient;
  end;
  ClientWidth := FormW;
  ClientHeight := FormH + ScaleToScreen(FButtons.Height);
  FForm.Left := 0;
  FForm.Top := 0;
  FScrollBox.Color := FForm.Color;
  Color := FForm.Color;
  FForm.Parent := FScrollBox;

  if FForm.AllowResizeWindow then
    BorderStyle := bsSizeable
  else
    BorderStyle := bsSingle;

  //CorrectSize;
end;

{procedure TEditWindow.SetFocusControl(GoForward: Boolean);
begin
  if not (ActiveControl is TdxGrid) then
  	SelectNext(ActiveControl, GoForward, True);
end;          }

procedure TEditWindow.DoShow;
var
  C: TWinControl;
begin
  FButtons.HelpButton.Visible:=FForm.HelpText.Text > '';
  C := GetTopControl(FForm);
  if (C <> nil) and (C.CanFocus) then
    C.SetFocus;
  //ShowImages(FForm);
  with TDataSetProcessor(FDSP) do
    ShowImages(FDSRi);
  CheckCbx;

  //TDataSetProcessor(FDSP).RunAction(FForm.ActionOnShowEditWindow, FDSRi);
  inherited DoShow;
end;

procedure TEditWindow.DoClose(var CloseAction: TCloseAction);
begin
  //TDataSetProcessor(FDSP).RunAction(FForm.ActionOnCloseEditWindow, FDSRi);
  TDataSetProcessor(FDSP).HideNotif;
  inherited DoClose(CloseAction);
end;

procedure TEditWindow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    Key := 0;
    if FDataSet.State in [dsInsert, dsEdit] then
    	ModalResult := mrOk
    else
      ModalResult := mrClose;
  end
  else if (Key = VK_ESCAPE) and (Shift = [ssShift]) then
  begin
    Key := 0;
    if FDataSet.State in [dsInsert, dsEdit] then
    	ModalResult := mrCancel
    else
      ModalResult := mrClose;
  end
  else if Key = VK_F1 then
  begin
    if FButtons.HelpButton.Visible then
    	FButtons.HelpButton.Click;
  end;
end;

constructor TEditWindow.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  KeyPreview := True;
  FScrollBox := TScrollBox.Create(Self);
  with FScrollBox do
  begin
	  BorderStyle:=bsNone;
  	HorzScrollBar.Smooth:=True;
    HorzScrollBar.Tracking:=True;
  	VertScrollBar.Smooth:=True;
    VertScrollBar.Tracking:=True;
  end;
  with FScrollBox do
  begin
    Parent := Self;
    Align := alClient;
  end;
  FButtons := TButtonPanel.Create(Self);
  with FButtons do
  begin
    Parent := Self;
    ShowHint := True;
    ShowButtons := [pbOk, pbCancel];
    OKButton.Caption := rsOK;
    OkButton.Hint:=rsSaveChangesHint;
    OkButton.Default:=False;
    CancelButton.Caption:=rsCancel;
    CancelButton.Hint := rsCancelChangesHint;
    //CancelButton.Cancel := False;
    CloseButton.Caption:=rsClose;
    CloseButton.Hint := rsCloseEditWindowHint;
    HelpButton.Caption:=rsHelp;
    HelpButton.OnClick:=@HelpButtonClick;
    HelpButton.Hint := rsShowHelpHint;
  end;
  //Position := poDesigned;//poMainFormCenter;
  BorderIcons := [biSystemMenu];
end;

function TEditWindow.CloseQuery: boolean;
begin
  Result:=inherited CloseQuery;
  if ModalResult = mrOk then
  begin
    TDataSetProcessor(FDSP).ForceChangeFields(FDSRi);
    if FForm.ConfirmSaveRecord and TDataSetProcessor(FDSP).AnyDataSetModified(FDSRi) {FDataSet.Modified} then
    begin
      if MessageDlg(rsWarning, rsConfirmSaveMsg, mtConfirmation,
      	[mbYes, mbNo], 0) <> mrYes then Exit(False);
    end;
    //FButtons.SetFocus;
    Result := TDataSetProcessor(FDSP).Validate(FDSRi, False);
  end
  else if ModalResult in [mrClose, mrCancel] then
  begin
    TDataSetProcessor(FDSP).ForceChangeFields(FDSRi);
    if FForm.ConfirmCancelEditing and TDataSetProcessor(FDSP).AnyDataSetModified(FDSRi) {FDataSet.Modified} then
    begin
      if MessageDlg(rsWarning, rsConfirmCancelEditMsg, mtConfirmation,
        [mbYes, mbNo], 0) <> mrYes then Exit(False);
    end;
  end;
end;

function TEditWindow.ShowModal: Integer;
begin
  if Visible and (Screen.GetCurrentModalForm <> Self) then Close;

  with TDataSetProcessor(FDSP) do
  begin
    PrepareBeforeShowEditForm(FDSRi);
    RefreshAllData(FDSRi);
  end;
  Result:=inherited ShowModal;
end;

procedure TEditWindow.Show;
begin
  with TDataSetProcessor(FDSP) do
  begin
    PrepareBeforeShowEditForm(FDSRi);
    RefreshAllData(FDSRi);
  end;
  inherited Show;
end;

function TEditWindow.ShowForm: Integer;
begin
  Result := ShowModal;
  if Result = mrOk then
  begin
    if FDataSet.State in [dsInsert, dsEdit] then FDataSet.Post;
  end
  else
    FDataSet.Cancel;
end;

end.

