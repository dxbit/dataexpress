{-------------------------------------------------------------------------------

    Copyright 2015-2025 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit LabelTextForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ButtonPanel, strconsts, dxctrls;

type

  { TLabelTextFm }

  TLabelTextFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
    Memo: TMemo;
    ToolBar1: TToolBar;
    LeftBn: TToolButton;
    CenterBn: TToolButton;
    RightBn: TToolButton;
    ToolButton1: TToolButton;
    WordWrapBn: TToolButton;
    TopBn: TToolButton;
    MiddleBn: TToolButton;
    BottomBn: TToolButton;
    ToolButton5: TToolButton;
    AutoSizeBn: TToolButton;
    procedure LeftBnClick(Sender: TObject);
    procedure CenterBnClick(Sender: TObject);
    procedure RightBnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FLabel: TdxLabel;
  public
    { public declarations }
    function ShowForm(C: TdxLabel): Integer;
  end;

var
  LabelTextFm: TLabelTextFm;

function ShowLabelTextForm(C: TdxLabel): Integer;

implementation

uses
  apputils;

function ShowLabelTextForm(C: TdxLabel): Integer;
begin
  if LabelTextFm = nil then
  	LabelTextFm := TLabelTextFm.Create(Application);
  Result := LabelTextFm.ShowForm(C);
end;

{$R *.lfm}

{ TLabelTextFm }

procedure TLabelTextFm.FormCreate(Sender: TObject);
begin
  SetupImageList(ImageList1, ['leftjustify16', 'centertext16', 'rightjustify16',
    'toplayout16', 'centerlayout16', 'bottomlayout16', 'lock16', 'wordwrap16']);
  Caption := rsLblText;
  LeftBn.Hint := rsLeftJustify;
  CenterBn.Hint := rsCenter;
  RightBn.Hint := rsRightJustify;
  TopBn.Hint := rsLayoutTop;
  MiddleBn.Hint := rsCenter;
  BottomBn.Hint := rsLayoutBottom;
  AutoSizeBn.Hint := rsAutoSize;
  WordWrapBn.Hint := rsWordWrap;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
end;

procedure TLabelTextFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  S: String;
begin
  if ModalResult = mrOk then
  begin
    S := Trim(Memo.Text);
    if S = '' then
    begin
      ErrMsg(rsLabelEmpty);
      CanClose := False;
    end
    else if Trim(FLabel.Expression) <> '' then
      CanClose := CheckDuplicateFieldName(S, FLabel) and CheckFieldName(S);
  end;
end;

procedure TLabelTextFm.LeftBnClick(Sender: TObject);
begin
  Memo.Alignment:=taLeftJustify;
end;

procedure TLabelTextFm.CenterBnClick(Sender: TObject);
begin
  Memo.Alignment:=taCenter;
end;

procedure TLabelTextFm.RightBnClick(Sender: TObject);
begin
  Memo.Alignment:=taRightJustify;
end;

procedure TLabelTextFm.FormShow(Sender: TObject);
begin
  Memo.SetFocus;
end;

function TLabelTextFm.ShowForm(C: TdxLabel): Integer;
begin
  {if Trim(C.Expression) <> '' then
  begin
    Info(rsTextPropNotAvail);
    Exit;
  end;  }
  FLabel := C;
  LeftBn.Down := C.Alignment = taLeftJustify;
  CenterBn.Down := C.Alignment = taCenter;
  RightBn.Down := C.Alignment = taRightJustify;
  TopBn.Down := C.Layout = tlTop;
  MiddleBn.Down := C.Layout = tlCenter;
  BottomBn.Down := C.Layout = tlBottom;
  AutoSizeBn.Down := C.AutoSize;
  WordWrapBn.Down := C.WordWrap;

  Memo.Alignment:=C.Alignment;
  Memo.Text := C.Caption;
  Result := ShowModal;
  if Result = mrOk then
  begin
    if LeftBn.Down then C.Alignment := taLeftJustify
    else if CenterBn.Down then C.Alignment := taCenter
    else if RightBn.Down then C.Alignment := taRightJustify;
    if TopBn.Down then C.Layout := tlTop
    else if MiddleBn.Down then C.Layout := tlCenter
    else if BottomBn.Down then C.Layout := tlBottom;
    C.AutoSize := AutoSizeBn.Down;
    C.WordWrap := WordWrapBn.Down;
    C.Caption := Memo.Text;
    C.FieldName := Memo.Text;
  end;
end;

end.

