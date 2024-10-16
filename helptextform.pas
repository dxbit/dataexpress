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

unit HelpTextForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterHTML, Forms, Controls,
  Graphics, Dialogs, ComCtrls, ButtonPanel, Menus, strconsts, LclType, SynEditTypes;

type

  { THelpTextFm }

  THelpTextFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ColorDlg: TColorDialog;
    MenuImages: TImageList;
    ToolImages: TImageList;
    CutMnu: TMenuItem;
    CopyMnu: TMenuItem;
    PasteMnu: TMenuItem;
    MenuItem4: TMenuItem;
    SelectAllMnu: TMenuItem;
    UndoMnu: TMenuItem;
    RedoMnu: TMenuItem;
    MenuItem8: TMenuItem;
    PopupMenu1: TPopupMenu;
    SynHTMLSyn1: TSynHTMLSyn;
    Editor: TSynMemo;
    ToolBar1: TToolBar;
    BoldBn: TToolButton;
    MarkListBn: TToolButton;
    BrBn: TToolButton;
    ViewBn: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    CaretPosBn: TToolButton;
    ImageBn: TToolButton;
    ItalicBn: TToolButton;
    UnderlineBn: TToolButton;
    FontBn: TToolButton;
    ColorBn: TToolButton;
    LeftJustifyBn: TToolButton;
    CenterTextBn: TToolButton;
    RightJustifyBn: TToolButton;
    NumListBn: TToolButton;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure CutMnuClick(Sender: TObject);
    procedure CopyMnuClick(Sender: TObject);
    procedure PasteMnuClick(Sender: TObject);
    procedure SelectAllMnuClick(Sender: TObject);
    procedure UndoMnuClick(Sender: TObject);
    procedure RedoMnuClick(Sender: TObject);
    procedure MarkListBnClick(Sender: TObject);
    procedure BrBnClick(Sender: TObject);
    procedure ViewBnClick(Sender: TObject);
    procedure ImageBnClick(Sender: TObject);
    procedure BoldBnClick(Sender: TObject);
    procedure ItalicBnClick(Sender: TObject);
    procedure UnderlineBnClick(Sender: TObject);
    procedure FontBnClick(Sender: TObject);
    procedure ColorBnClick(Sender: TObject);
    procedure LeftJustifyBnClick(Sender: TObject);
    procedure CenterTextBnClick(Sender: TObject);
    procedure RightJustifyBnClick(Sender: TObject);
    procedure NumListBnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(const S: String): String;
  end;

var
  HelpTextFm: THelpTextFm;

function ShowHelpTextForm(const S: String): String;

implementation

uses
  helpviewform, apputils, helpmanager, imagesform;

function ShowHelpTextForm(const S: String): String;
begin
  if HelpTextFm = nil then
  	HelpTextFm := THelpTextFm.Create(Application);
  Result := HelpTextFm.ShowForm(S);
end;

{$R *.lfm}

{ THelpTextFm }

procedure THelpTextFm.BoldBnClick(Sender: TObject);
var
  S: String;
begin
  S := '<b>' + Editor.SelText + '</b>';
  Editor.SelText:=S;
  Editor.SelStart:=Editor.SelStart - Length(S) + 3;
end;

procedure THelpTextFm.ViewBnClick(Sender: TObject);
begin
  ShowHelpForm(Editor.Text);
end;

procedure THelpTextFm.ImageBnClick(Sender: TObject);
begin
  if ShowImagesForm(True, '', False) = mrOk then
  begin
    Editor.SelText:='<img src="images:' + ImagesFm.SelectedImageName + '">';
  end;
end;

procedure THelpTextFm.MarkListBnClick(Sender: TObject);
begin
  Editor.SelText:='<ul>' + LineEnding + '<li></li>' + LineEnding +
    '<li></li>' + LineEnding +  '<li></li>' + LineEnding + '<li></li>' + LineEnding +
    '<li></li>' + LineEnding + '</ul>';
end;

procedure THelpTextFm.BrBnClick(Sender: TObject);
begin
  Editor.SelText:='<br>';
end;

procedure THelpTextFm.FormCreate(Sender: TObject);
begin
  Caption := rsHelpText;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  ColorDlg.Title := rsColor;
  BoldBn.Hint := rsBold;
  ItalicBn.Hint := rsItalic;
  UnderlineBn.Hint := rsUnderline;
  FontBn.Hint := rsTextColor;
  ColorBn.Hint := rsColor;
  LeftJustifyBn.Hint := rsLeftJustify;
  CenterTextBn.Hint := rsCenterText;
  RightJustifyBn.Hint := rsRightJustify;
  NumListBn.Hint := rsOrderedList;
  MarkListBn.Hint := rsUnorderedList;
  BrBn.Hint := rsNewLine;
  ViewBn.Hint := rsPreview;
  ImageBn.Hint := rsImage;
  CutMnu.Caption := rsCut;
  CopyMnu.Caption := rsCopy;
  PasteMnu.Caption := rsPaste;
  UndoMnu.Caption := rsUndo;
  RedoMnu.Caption := rsRedo;
  SelectAllMnu.Caption := rsSelectAll;
  SetupImageList(ToolImages, ['bold16', 'italic16', 'underline16', 'font16',
    'color16', 'image16', 'leftjustify16', 'centertext16', 'rightjustify16',
    'num_list16', 'mark_list16', 'br16', 'eyes16']);
  SetupImageList(MenuImages, ['cut16', 'copy16', 'paste16', 'undo16', 'goto16']);
end;

procedure THelpTextFm.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then
    case Key of
      VK_B: BoldBn.Click;
      VK_I: ItalicBn.Click;
      VK_U: UnderlineBn.Click;
      VK_SPACE: begin
        BrBn.Click;
        Key := 0;
      end;
    end
  else if Key = VK_F9 then ViewBn.Click
  else if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure THelpTextFm.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  CaretPosBn.Caption := Format('%d: %d', [Editor.CaretY, Editor.CaretX]);
  CutMnu.Enabled := Editor.SelText <> '';
  CopyMnu.Enabled := Editor.SelText <> '';
  PasteMnu.Enabled := Editor.CanPaste;
  UndoMnu.Enabled := Editor.CanUndo;
  RedoMnu.Enabled := Editor.CanRedo;
end;

procedure THelpTextFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
  begin
    if Editor.Modified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure THelpTextFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('helptext');
end;

procedure THelpTextFm.CutMnuClick(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure THelpTextFm.CopyMnuClick(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure THelpTextFm.PasteMnuClick(Sender: TObject);
begin
  Editor.PasteFromClipboard;
end;

procedure THelpTextFm.SelectAllMnuClick(Sender: TObject);
begin
  Editor.SelectAll;
end;

procedure THelpTextFm.UndoMnuClick(Sender: TObject);
begin
  Editor.Undo;
end;

procedure THelpTextFm.RedoMnuClick(Sender: TObject);
begin
  Editor.Redo;
end;

procedure THelpTextFm.ItalicBnClick(Sender: TObject);
var
  S: String;
begin
  S := '<i>' + Editor.SelText + '</i>';
  Editor.SelText:=S;
  Editor.SelStart:=Editor.SelStart - Length(S) + 3;
end;

procedure THelpTextFm.UnderlineBnClick(Sender: TObject);
var
  S: String;
begin
  S := '<u>' + Editor.SelText + '</u>';
  Editor.SelText:=S;
  Editor.SelStart:=Editor.SelStart - Length(S) + 3;
end;

procedure THelpTextFm.FontBnClick(Sender: TObject);
var
  S: String;
begin
  S := '<font color="">' + Editor.SelText + '</font>';
  Editor.SelText:=S;
  Editor.SelStart:=Editor.SelStart - Length(S) + 15;
end;

procedure THelpTextFm.ColorBnClick(Sender: TObject);
var
  RGB: LongInt;
  R, G, B: Byte;
  S: String;
begin
  if ColorDlg.Execute then
  begin
    RGB := ColorToRGB(ColorDlg.Color);
    RedGreenBlue(RGB, R, G, B);
    S := '#' + IntToHex(R, 2) + IntToHex(G, 2) + IntToHex(B, 2);
    Editor.SelText:=S;
  end;
end;

procedure THelpTextFm.LeftJustifyBnClick(Sender: TObject);
begin
  Editor.SelText:='<p align="left">' + Editor.SelText + '</p>';
end;

procedure THelpTextFm.CenterTextBnClick(Sender: TObject);
begin
  Editor.SelText:='<p align="center">' + Editor.SelText + '</p>';
end;

procedure THelpTextFm.RightJustifyBnClick(Sender: TObject);
begin
  Editor.SelText:='<p align="right">' + Editor.SelText + '</p>';
end;

procedure THelpTextFm.NumListBnClick(Sender: TObject);
begin
  Editor.SelText:='<ol>' + LineEnding + '<li></li>' + LineEnding +
    '<li></li>' + LineEnding +  '<li></li>' + LineEnding + '<li></li>' + LineEnding +
    '<li></li>' + LineEnding + '</ol>';
end;

function THelpTextFm.ShowForm(const S: String): String;
begin
  Result := S;
  Editor.Text:=S;
  if Editor.Text = '' then
    Editor.Text := '<body bgcolor=#fff8dc>' + LineEnding + LineEnding + '</body>';
  // Сбрасываем флаг изменения.
  Editor.Modified:=False;
  if ShowModal = mrOk then Result := Trim(Editor.Text);
end;

end.

