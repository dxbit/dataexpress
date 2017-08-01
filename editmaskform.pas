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
unit EditMaskForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MaskEdit, ButtonPanel, ComCtrls, dxctrls, myctrls, strconsts;

{ TEditMaskFm }

type
  TEditMaskFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Samples: TListView;
    SaveChars: TCheckBox;
    Mask: TEdit;
    Blank: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure BlankChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MaskChange(Sender: TObject);
    procedure SamplesDblClick(Sender: TObject);
    procedure SaveCharsChange(Sender: TObject);
  private
    { private declarations }
    FTestMask: TMaskEditEx;
    procedure UpdateMask;
    procedure AddSample(const aCaption, aMask: String);
  public
    { public declarations }
    procedure ParseMask(const aMask: String);
    property TestMask: TMaskEditEx read FTestMask;
  end;

var
  EditMaskFm: TEditMaskFm;

function ShowInputMaskForm(C: TdxEdit): Integer;

implementation

uses
  apputils, helpform;

function ShowInputMaskForm(C: TdxEdit): Integer;
begin
  with TEditMaskFm.Create(nil) do
	begin
    ParseMask(C.EditMask);
    Result := ShowModal;
    if Result = mrOk then
    	C.EditMask := TestMask.EditMask;
    Free;
  end;
end;

{$R *.lfm}

{ TEditMaskFm }

procedure TEditMaskFm.FormShow(Sender: TObject);
begin
  Mask.SetFocus;
end;

procedure TEditMaskFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('editmask');
end;

procedure TEditMaskFm.MaskChange(Sender: TObject);
begin
  UpdateMask;
end;

procedure TEditMaskFm.SamplesDblClick(Sender: TObject);
begin
  if Samples.Selected <> nil then
  	Mask.Text := Samples.Selected.SubItems[0];
end;

procedure TEditMaskFm.SaveCharsChange(Sender: TObject);
begin
  UpdateMask;
end;

procedure TEditMaskFm.BlankChange(Sender: TObject);
begin
  UpdateMask;
end;

procedure TEditMaskFm.FormCreate(Sender: TObject);
begin
  Caption := rsEditMask;
  Label1.Caption := rsEditMask;
  Label2.Caption := rsCharForBlanks;
  SaveChars.Caption := rsSaveLiteralChars;
  Label3.Caption := rsTestInput;
  Label4.Caption := rsSampleMasks;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;

	AddSample(rsPhoneNumber, '8 (000) 000-00-00');
  AddSample(rsPassportSeries, '00 00');
  AddSample(rsPassportID, '000000');
  AddSample(rsSNILS, '000-000-000-00');
  AddSample(rsIPAddress, '999.999.999.999');
  AddSample(rsMACAddress, '00:00:00:00:00:00');
  AddSample(rsName, '>C<ccccccccccccccccccc');

  FTestMask := TMaskEditEx.Create(Self);
  with FTestMask do
  begin
    Parent := Self;
    Left := 8;
    Width := 264;
    AnchorSideTop.Control := Label3;
    AnchorSideTop.Side := asrBottom;
    BorderSpacing.Top := 1;
    TabOrder := 3;
  end;
  with Samples do
  begin
    AnchorSideBottom.Control := FTestMask;
    AnchorSideBottom.Side := asrBottom;
  end;
end;

procedure TEditMaskFm.ParseMask(const aMask: String);
var
  SL: TStringList;
begin
  if aMask = '' then Exit;

  SL := TStringList.Create;
  SplitStr(aMask, ';', SL);
  Mask.Text := SL[0];
  SaveChars.Checked := (SL.Count = 1) or (SL[1] = '1');
  if SL.Count > 2 then
	  Blank.Text := SL[2];
  SL.Free;
end;

procedure TEditMaskFm.UpdateMask;
begin
  FTestMask.EditMask:=Mask.Text + ';' + Bool2Str(SaveChars.Checked) +
  	';' + Blank.Text;
end;

procedure TEditMaskFm.AddSample(const aCaption, aMask: String);
begin
  with Samples.Items.Add do
  begin
    Caption := aCaption;
    SubItems.Add(aMask);
  end;
end;

end.

