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
unit TreeForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, ExtCtrls, StdCtrls, Buttons, Spin, CtrlUtils, dxctrls, strconsts;

type

  { TTreeFm }

  TTreeFm = class(TForm)
    Fnt: TBitBtn;
    ButtonPanel1: TButtonPanel;
    BkColor: TColorSampler;
    LineColor: TColorSampler;
    SelColor: TColorSampler;
    Grp: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    TrWidth: TSpinEdit;
    Tree: TTreeView;
    procedure BkColorChange(Sender: TObject);
    procedure FntClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure LineColorChange(Sender: TObject);
    procedure SelColorChange(Sender: TObject);
  private
    { private declarations }
    FFm: TdxForm;
    procedure FillFields;
  public
    { public declarations }
    procedure ShowForm(aFm: TdxForm);
  end;

var
  TreeFm: TTreeFm;

implementation

uses
  FontForm, helpform, mytypes;

{$R *.lfm}

{ TTreeFm }

procedure TTreeFm.FntClick(Sender: TObject);
begin
  if FontFm.ShowForm(Tree.Font) = mrOk then
  begin
    if FontFm.IsDefault then
      Tree.Font.Assign(FFm.Font);
  end;

end;

procedure TTreeFm.FormCreate(Sender: TObject);
begin
  Caption := rsTree;
  Label1.Caption:=rsGroupField;
  Label2.Caption := rsBackColor;
  Label3.Caption := rsLineColor;
  Label4.Caption:=rsSelectColor;
  Fnt.Caption := rsFont;
  Label5.Caption := rsWidth;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  Tree.Selected := Tree.Items[0];
end;

procedure TTreeFm.FormShow(Sender: TObject);
begin
  TrWidth.SetFocus;
end;

procedure TTreeFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('tree');
end;

procedure TTreeFm.LineColorChange(Sender: TObject);
begin
  Tree.TreeLineColor:=LineColor.SampleColor;
end;

procedure TTreeFm.SelColorChange(Sender: TObject);
begin
  Tree.SelectionColor:=SelColor.SampleColor;
end;

procedure TTreeFm.BkColorChange(Sender: TObject);
begin
  Tree.BackgroundColor:=BkColor.SampleColor;
end;

procedure TTreeFm.FillFields;
var
  i: Integer;
  C: TComponent;
  SL: TStringListUtf8;
begin
  SL := TStringListUtf8.Create;
  SL.Add('');
  for i := 0 to FFm.ComponentCount - 1 do
  begin
    C := FFm.Components[i];
    if C is TdxLookupComboBox then
      if (GetSourceTId(C) > 0) and (GetSourceFId(C) > 0) then
        SL.AddObject(GetFieldName(C), TObject(PtrInt(GetId(C))));
  end;
  SL.Sort;
  Grp.Items := SL;
  SL.Free;
end;

procedure TTreeFm.ShowForm(aFm: TdxForm);
begin
  FFm := aFm;
  BkColor.DefaultColor:=clWindow;
  BkColor.SampleColor:=aFm.TreeBackColor;
  LineColor.DefaultColor:=clWindowFrame;
  LineColor.SampleColor:=aFm.TreeLineColor;
  SelColor.DefaultColor:=clHighlight;
  SelColor.SampleColor:=aFm.TreeSelectColor;
  TrWidth.Value := aFm.TreeWidth;
  Tree.BackgroundColor:=aFm.TreeBackColor;
  Tree.TreeLineColor:=aFm.TreeLineColor;
  Tree.SelectionColor:=aFm.TreeSelectColor;
  Tree.Font := aFm.TreeFont;
  FillFields;
  with Grp do
    ItemIndex := Items.IndexOfObject(TObject(PtrInt(aFm.GroupField)));
  if ShowModal = mrOk then
  begin
    aFm.TreeBackColor:=BkColor.SampleColor;
    aFm.TreeLineColor:=LineColor.SampleColor;
    aFm.TreeSelectColor:=SelColor.SampleColor;
    aFm.TreeFont := Tree.Font;
    aFm.TreeWidth:=TrWidth.Value;
    with Grp do
      if ItemIndex >= 0 then aFm.GroupField:=PtrInt(Items.Objects[ItemIndex])
    else
      aFm.GroupField := 0;
  end;
end;

end.

