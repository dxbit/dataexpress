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
unit HelpTextForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterHTML, Forms, Controls,
  Graphics, Dialogs, ComCtrls, ButtonPanel, Menus, strconsts, LclType;

type

  { THelpTextFm }

  THelpTextFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ColorDlg: TColorDialog;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    PopupMenu1: TPopupMenu;
    SynHTMLSyn1: TSynHTMLSyn;
    Editor: TSynMemo;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ToolButton12Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(const S: String): String;
  end;

var
  HelpTextFm: THelpTextFm;

implementation

uses
  helpviewform, apputils, helpform;

{$R *.lfm}

{ THelpTextFm }

procedure THelpTextFm.ToolButton1Click(Sender: TObject);
var
  S: String;
begin
  S := '<b>' + Editor.SelText + '</b>';
  Editor.SelText:=S;
  Editor.SelStart:=Editor.SelStart - Length(S) + 3;
end;

procedure THelpTextFm.ToolButton12Click(Sender: TObject);
begin
  HelpViewFm.ShowForm(Editor.Text);
end;

procedure THelpTextFm.ToolButton10Click(Sender: TObject);
begin
  Editor.SelText:='<ul>' + LineEnding + '<li></li>' + LineEnding +
    '<li></li>' + LineEnding +  '<li></li>' + LineEnding + '<li></li>' + LineEnding +
    '<li></li>' + LineEnding + '</ul>';
end;

procedure THelpTextFm.ToolButton11Click(Sender: TObject);
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
  ToolButton1.Hint := rsBold;
  ToolButton2.Hint := rsItalic;
  ToolButton3.Hint := rsUnderline;
  ToolButton4.Hint := rsTextColor;
  ToolButton5.Hint := rsColor;
  ToolButton6.Hint := rsLeftJustify;
  ToolButton7.Hint := rsCenterText;
  ToolButton8.Hint := rsRightJustify;
  ToolButton9.Hint := rsOrderedList;
  ToolButton10.Hint := rsUnorderedList;
  ToolButton11.Hint := rsNewLine;
  ToolButton12.Hint := rsPreview;
  MenuItem1.Caption := rsCut;
  SetMenuItemImage(MenuItem1, 'cut16');
  MenuItem2.Caption := rsCopy;
  SetMenuItemImage(MenuItem2, 'copy16');
  MenuItem3.Caption := rsPaste;
  SetMenuItemImage(MenuItem3, 'paste16');
  MenuItem6.Caption := rsUndo;
  SetMenuItemImage(MenuItem6, 'undo16');
  MenuItem7.Caption := rsRedo;
  SetMenuItemImage(MenuItem7, 'goto16');
  MenuItem5.Caption := rsSelectAll;
end;

procedure THelpTextFm.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then
    case Key of
      VK_B: ToolButton1.Click;
      VK_I: ToolButton2.Click;
      VK_U: ToolButton3.Click;
      VK_SPACE: begin
        ToolButton11.Click;
        Key := 0;
      end;
    end
  else if Key = VK_F5 then ToolButton12.Click;
end;

procedure THelpTextFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('helptext');
end;

procedure THelpTextFm.MenuItem1Click(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure THelpTextFm.MenuItem2Click(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure THelpTextFm.MenuItem3Click(Sender: TObject);
begin
  Editor.PasteFromClipboard;
end;

procedure THelpTextFm.MenuItem5Click(Sender: TObject);
begin
  Editor.SelectAll;
end;

procedure THelpTextFm.MenuItem6Click(Sender: TObject);
begin
  Editor.Undo;
end;

procedure THelpTextFm.MenuItem7Click(Sender: TObject);
begin
  Editor.Redo;
end;

procedure THelpTextFm.ToolButton2Click(Sender: TObject);
var
  S: String;
begin
  S := '<i>' + Editor.SelText + '</i>';
  Editor.SelText:=S;
  Editor.SelStart:=Editor.SelStart - Length(S) + 3;
end;

procedure THelpTextFm.ToolButton3Click(Sender: TObject);
var
  S: String;
begin
  S := '<u>' + Editor.SelText + '</u>';
  Editor.SelText:=S;
  Editor.SelStart:=Editor.SelStart - Length(S) + 3;
end;

procedure THelpTextFm.ToolButton4Click(Sender: TObject);
var
  S: String;
begin
  S := '<font color="">' + Editor.SelText + '</font>';
  Editor.SelText:=S;
  Editor.SelStart:=Editor.SelStart - Length(S) + 15;
end;

procedure THelpTextFm.ToolButton5Click(Sender: TObject);
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

procedure THelpTextFm.ToolButton6Click(Sender: TObject);
begin
  Editor.SelText:='<p align="left">' + Editor.SelText + '</p>';
end;

procedure THelpTextFm.ToolButton7Click(Sender: TObject);
begin
  Editor.SelText:='<p align="center">' + Editor.SelText + '</p>';
end;

procedure THelpTextFm.ToolButton8Click(Sender: TObject);
begin
  Editor.SelText:='<p align="right">' + Editor.SelText + '</p>';
end;

procedure THelpTextFm.ToolButton9Click(Sender: TObject);
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

  if ShowModal = mrOk then Result := Trim(Editor.Text);
end;

end.

