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
unit EditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ButtonPanel, strconsts, dxctrls, DbCtrls,
  StdCtrls, Controls, Db, SqlDb, Dialogs;

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
    procedure CorrectSize;
    procedure SetForm(AValue: TdxForm);
  protected
    procedure DoShow; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function CloseQuery: boolean; override;
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
  HelpViewFm.ShowForm(FForm.HelpText.Text);
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

procedure TEditWindow.CorrectSize;
begin
  if Screen.DesktopHeight < Height - 120 then
  begin
    Top := 0;
    Height := Screen.DesktopHeight - 120;
  end;
  if Screen.DesktopWidth < Width then
  begin
    Left := 0;
    Width := Screen.DesktopWidth - 80;
  end;
end;

procedure TEditWindow.SetForm(AValue: TdxForm);
var
  FormW, FormH: Integer;
begin
  FForm:=AValue;
  Caption := FForm.FormCaption;
  //FForm.Align := alNone;
  FormW := FForm.Width;
  FormH := FForm.Height;
  if not FForm.ShowScrollBars then
  begin
    FScrollBox.AutoScroll := False;
    FForm.Align := alClient;
  end;
  ClientWidth := FormW;
  ClientHeight := FormH + FButtons.Height{ + 32};
  FForm.Left := 0;
  FForm.Top := 0;
  FScrollBox.Color := FForm.Color;
  Color := FForm.Color;
  //FForm.Align:=alClient;
  FForm.Parent := FScrollBox;
  CorrectSize;
end;

procedure TEditWindow.DoShow;
var
  C: TWinControl;
begin
  //CorrectSize;
  FButtons.HelpButton.Visible:=FForm.HelpText.Text > '';
  C := GetTopControl(FForm);
  if (C <> nil) and (C.CanFocus) then
    C.SetFocus;
  ShowImages(FForm);
  CheckCbx;
  inherited DoShow;
end;

constructor TEditWindow.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  FScrollBox := TScrollBox.Create(Self);
  FScrollBox.BorderStyle:=bsNone;
  with FScrollBox do
  begin
    Parent := Self;
    Align := alClient;
  end;
  FButtons := TButtonPanel.Create(Self);
  with FButtons do
  begin
    Parent := Self;
    ShowButtons := [pbOk, pbCancel];
    OKButton.Caption := rsOK;
    CancelButton.Caption:=rsCancel;
    CloseButton.Caption:=rsClose;
    HelpButton.Caption:=rsHelp;
    HelpButton.OnClick:=@HelpButtonClick;
  end;
  Position := poOwnerFormCenter;
  BorderIcons := [biSystemMenu];
end;

function TEditWindow.CloseQuery: boolean;
begin
  Result:=inherited CloseQuery;
  if ModalResult = mrOk then
  begin
    TDataSetProcessor(FDSP).ForceChangeFields(FDSRi);
    if FForm.ConfirmSaveRecord and FDataSet.Modified then
    begin
      if MessageDlg(rsWarning, rsConfirmSaveMsg, mtConfirmation,
      	[mbYes, mbNo], 0) <> mrYes then Exit(False);
    end;
    FButtons.SetFocus;
    Result := TDataSetProcessor(FDSP).Validate(FDSRi, False);
  end
  else if ModalResult in [mrClose, mrCancel] then
  begin
    TDataSetProcessor(FDSP).ForceChangeFields(FDSRi);
    if FForm.ConfirmCancelEditing and FDataSet.Modified then
    begin
      if MessageDlg(rsWarning, rsConfirmCancelEditMsg, mtConfirmation,
        [mbYes, mbNo], 0) <> mrYes then Exit(False);
    end;
  end;
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

