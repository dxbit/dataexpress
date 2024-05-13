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
    function ShowForm(C: TdxEdit): Integer;
    property TestMask: TMaskEditEx read FTestMask;
  end;

var
  EditMaskFm: TEditMaskFm;

function ShowEditMaskForm(C: TdxEdit): Integer;

implementation

uses
  apputils, helpmanager;

function ShowEditMaskForm(C: TdxEdit): Integer;
begin
  if EditMaskFm = nil then
  	EditMaskFm := TEditMaskFm.Create(Application);
  Result := EditMaskFm.ShowForm(C);
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
  if aMask = '' then
  begin
    Mask.Text := '';
    Blank.Text := '_';
    SaveChars.Checked := True;
  	Exit;
  end;

  SL := TStringList.Create;
  SplitStr(aMask, ';', SL);
  Mask.Text := SL[0];
  SaveChars.Checked := (SL.Count = 1) or (SL[1] = '1');
  if SL.Count > 2 then
	  Blank.Text := SL[2];
  SL.Free;
end;

function TEditMaskFm.ShowForm(C: TdxEdit): Integer;
begin
  ParseMask(C.EditMask);
  Result := EditMaskFm.ShowModal;
  if Result = mrOk then
    C.EditMask := TestMask.EditMask;
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

