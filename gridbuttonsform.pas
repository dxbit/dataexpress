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
unit GridButtonsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, ButtonPanel, Buttons, myctrls, CtrlUtils, dxreports,
  strconsts;

type

  { TGridButtonsFm }

  TGridButtonsFm = class(TForm)
    HideBns: TCheckBox;
    FontBns: TBitBtn;
    BkColor: TColorSampler;
    ButtonPanel1: TButtonPanel;
    Label2: TLabel;
    Label3: TLabel;
    AlignBns: TRadioGroup;
    ShowBns: TCheckBox;
    FlatBns: TCheckBox;
    VisBns: TCheckGroup;
    Label1: TLabel;
    SizeBns: TSpinEdit;
    VisCaps: TCheckGroup;
    procedure FontBnsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(G: TMyDBGrid);
  end;

var
  GridButtonsFm: TGridButtonsFm;

implementation

uses
  fontform, helpform;

{$R *.lfm}

{ TGridButtonsFm }

procedure TGridButtonsFm.FontBnsClick(Sender: TObject);
begin
  FontFm.ShowForm(FontBns.Font);
end;

procedure TGridButtonsFm.FormCreate(Sender: TObject);
begin
  Caption := rsButtons;
  ShowBns.Caption := rsShowButtons;
  VisBns.Caption := rsVisibleButtons;
  VisCaps.Caption := rsVisibleCaptions;
  Label1.Caption := rsSize;
  Label2.Caption := rsBackground;
  Label3.Caption:= rsFont;
  FontBns.Caption := rsFont;
  AlignBns.Caption := rsAlignment;
  AlignBns.Items[0] := rsLeft;
  AlignBns.Items[1] := rsRight;
  HideBns.Caption := rsHideWhenLostFocus;
  FlatBns.Caption := rsFlatButtons;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TGridButtonsFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('gridbuttons');
end;

procedure TGridButtonsFm.ShowForm(G: TMyDBGrid);
var
  i: TGridButtonType;
  j: Integer;
  Bns: TGridButtonSet;
begin
  ShowBns.Checked := G.ShowButtons;
  VisBns.Items.Clear;
  VisCaps.Items.Clear;
  j := 0;
  for i := gbnAppend to gbnGoto do
  begin
    if G is TdxQueryGrid then
    begin
      if i in [gbnDuplicate, gbnShopping, gbnMoveUp, gbnMoveDown] then Continue;
    end
    else
    begin
      if i in [gbnRefresh, gbnGoto] then Continue;
    end;
    VisBns.Items.AddObject(GetBnCaption(i), TObject(PtrInt(i)));
    VisBns.Checked[j] := i in G.VisibleButtons;
    VisCaps.Items.AddObject(GetBnCaption(i), TObject(PtrInt(i)));
    VisCaps.Checked[j] := i in G.VisibleCaptions;
    Inc(j);
  end;
  FlatBns.Checked := G.FlatButtons;
  SizeBns.Value:=G.ButtonSize;
  BkColor.DefaultColor:=clBtnFace;
  BkColor.SampleColor:=G.ButtonsColor;
  FontBns.Font := G.ButtonFont;
  AlignBns.ItemIndex:=Ord(G.AlignmentButtons);
  HideBns.Checked:=G.HideButtonsWhenLostFocus;
  if ShowModal = mrOk then
  begin
    Bns := [];
    for j := 0 to VisBns.Items.Count - 1 do
      if VisBns.Checked[j] then Include(Bns, TGridButtonType(VisBns.Items.Objects[j]));
    G.VisibleButtons:=Bns;
    Bns := [];
    for j := 0 to VisCaps.Items.Count - 1 do
      if VisCaps.Checked[j] then Include(Bns, TGridButtonType(VisCaps.Items.Objects[j]));
    G.VisibleCaptions := Bns;
    G.FlatButtons:=FlatBns.Checked;
    G.ButtonSize:=SizeBns.Value;
    G.ShowButtons:=ShowBns.Checked;
    G.ButtonsColor := BkColor.SampleColor;
    G.ButtonFont := FontBns.Font;
    G.AlignmentButtons:=TAlignment(AlignBns.ItemIndex);
    G.HideButtonsWhenLostFocus:=HideBns.Checked;
  end;
end;

end.

