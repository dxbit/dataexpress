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

unit PivotFieldForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, StdCtrls, ColorBox, EditBtn, Spin, pivotgrid, dxreports,
  strconsts, KGraphics;

type

  { TPivotFieldFm }

  TPivotFieldFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DataType: TComboBox;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    ShowTotal: TCheckBox;
    FixClr: TColorBox;
    Clr: TColorBox;
    CellWidth: TSpinEdit;
    CellHeight: TSpinEdit;
    TotalWidth: TSpinEdit;
    TabSheet5: TTabSheet;
    TotalFixClr: TColorBox;
    TotalClr: TColorBox;
    Func: TComboBox;
    TotalCaption: TEdit;
    FixFnt: TEditButton;
    Fnt: TEditButton;
    TotalFixFnt: TEditButton;
    TotalFnt: TEditButton;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure FixFntButtonClick(Sender: TObject);
    procedure FixFntKeyPress(Sender: TObject; var Key: char);
    procedure FntButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure TotalFixFntButtonClick(Sender: TObject);
    procedure TotalFntButtonClick(Sender: TObject);
  private
    { private declarations }
    FGrid: TdxPivotGrid;
  public
    { public declarations }
    function ShowForm(const aCaption: String; FI: TFieldItem; Grid: TdxPivotGrid): Integer;
  end;

var
  PivotFieldFm: TPivotFieldFm;

function ShowPivotFieldForm(const aCaption: String; FI: TFieldItem; Grid: TdxPivotGrid): Integer;

implementation

uses
  fontform, helpmanager;

function ShowPivotFieldForm(const aCaption: String; FI: TFieldItem;
  Grid: TdxPivotGrid): Integer;
begin
  if PivotFieldFm = nil then
  	PivotFieldFm := TPivotFieldFm.Create(Application);
  Result := PivotFieldFm.ShowForm(aCaption, FI, Grid);
end;

{$R *.lfm}

{ TPivotFieldFm }

procedure TPivotFieldFm.FormCreate(Sender: TObject);
begin
  TabSheet1.Caption := rsColor;
  Label1.Caption := rsHeader;
  Label2.Caption := rsDataArea;
  Label3.Caption := rsSubtotalColumn;
  Label4.Caption := rsHeader;
  Label5.Caption := rsDataArea;

  TabSheet2.Caption := rsFont;
  Label6.Caption := rsHeader;
  Label7.Caption := rsDataArea;
  Label8.Caption := rsSubtotalColumn;
  Label9.Caption := rsHeader;
  Label10.Caption := rsDataArea;

  TabSheet3.Caption := rsTextAlignment;
  Label11.Caption := rsTextAlignInDataCells;

  TabSheet5.Caption := rsCellSize;
  Label14.Caption := rsWidth;
  Label15.Caption := rsHeight;

  TabSheet4.Caption := rsTotal;
  ShowTotal.Caption := rsShowSubtotal;
  Label12.Caption := rsCaptionSubtotal;
  Label13.Caption := rsFunction;
  Label16.Caption := rsDataType;
  Label17.Caption := rsWidth;

  with Func.Items do
  begin
    Add('');
    AddObject(rsSum, TObject(tfSum));
    AddObject(rsAverage, TObject(tfAvg));
    AddObject(rsMaximum, TObject(tfMax));
    AddObject(rsMinimum, TObject(tfMin));
    AddObject(rsCount, TObject(tfCount));
  end;

  with DataType.Items do
  begin
    AddObject(rsText, TObject(flText));
    AddObject(rsNumber, TObject(flNumber));
    AddObject(rsDate, TObject(flDate));
    AddObject(rsTime, TObject(flTime));
  end;

  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TPivotFieldFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('pivotfield');
end;

procedure TPivotFieldFm.TotalFixFntButtonClick(Sender: TObject);
begin
  if ShowFontForm(TotalFixFnt.Font, FGrid.Font) = mrOk then
    TotalFixFnt.Text := TotalFixFnt.Font.Name;
end;

procedure TPivotFieldFm.TotalFntButtonClick(Sender: TObject);
begin
  if ShowFontForm(TotalFnt.Font, FGrid.Font) = mrOk then
    TotalFnt.Text := TotalFnt.Font.Name;
end;

procedure TPivotFieldFm.FixFntKeyPress(Sender: TObject; var Key: char);
begin
  Key := #0;
end;

procedure TPivotFieldFm.FntButtonClick(Sender: TObject);
begin
  if ShowFontForm(Fnt.Font, FGrid.Font) = mrOk then
    Fnt.Text := Fnt.Font.Name;
end;

procedure TPivotFieldFm.FixFntButtonClick(Sender: TObject);
begin
  if ShowFontForm(FixFnt.Font, FGrid.Font) = mrOk then
    FixFnt.Text := FixFnt.Font.Name;
end;

function TPivotFieldFm.ShowForm(const aCaption: String; FI: TFieldItem;
  Grid: TdxPivotGrid): Integer;
var
  b, bb: Boolean;
begin
  FGrid := Grid;

  Caption := Format(rsFieldProps, [aCaption]);

  TabSheet3.Enabled:=FI.Collection = Grid.DataFields;
  b := (FI.Collection = Grid.ColFields) and (FI.Index < Grid.ColFields.Count - 1);
  bb := (FI.Collection = Grid.RowFields) and (FI.Index < Grid.RowFields.Count - 1);
  TotalFixClr.Enabled := b;
  TotalClr.Enabled := b;
  TotalFixFnt.Enabled := b;
  TotalFnt.Enabled := b;
  ShowTotal.Enabled := b or bb;
  TotalCaption.Enabled := b;
  TotalWidth.Enabled := b;
  Func.Enabled := FI.Collection = Grid.DataFields;

  FixClr.Selected:=FI.FixedColor;
  Clr.Selected := FI.Color;
  TotalFixClr.Selected := FI.TotalFixedColor;
  TotalClr.Selected := FI.TotalColor;
  FixFnt.Font := FI.FixedFont;
  FixFnt.Text := FI.FixedFont.Name;
  Fnt.Font := FI.Font;
  Fnt.Text := FI.Font.Name;
  TotalFixFnt.Font := Fi.TotalFixedFont;
  TotalFixFnt.Text := FI.TotalFixedFont.Name;
  TotalFnt.Font := FI.TotalFont;
  TotalFnt.Text := FI.TotalFont.Name;

  RadioButton1.Checked:=(FI.VAlign = valTop) and (FI.HAlign = halLeft);
  RadioButton2.Checked:=(FI.VAlign = valTop) and (FI.HAlign = halCenter);
  RadioButton3.Checked:=(FI.VAlign = valTop) and (FI.HAlign = halRight);
  RadioButton4.Checked:=(FI.VAlign = valCenter) and (FI.HAlign = halLeft);
  RadioButton5.Checked:=(FI.VAlign = valCenter) and (FI.HAlign = halCenter);
  RadioButton6.Checked:=(FI.VAlign = valCenter) and (FI.HAlign = halRight);
  RadioButton7.Checked:=(FI.VAlign = valBottom) and (FI.HAlign = halLeft);
  RadioButton8.Checked:=(FI.VAlign = valBottom) and (FI.HAlign = halCenter);
  RadioButton9.Checked:=(FI.VAlign = valBottom) and (FI.HAlign = halRight);
  CellWidth.Value := FI.Width;
  CellHeight.Value := FI.Height;
  ShowTotal.Checked := FI.ShowTotal;
  TotalCaption.Text:=FI.TotalCaption;
  TotalWidth.Value := FI.TotalWidth;
  Func.ItemIndex := Ord(FI.Func);
  with DataType do
    case FI.DataType of
      flText: ItemIndex := 0;
      flNumber: ItemIndex := 1;
      flDate: ItemIndex := 2;
      flTime: ItemIndex := 3;
    end;

  Result := ShowModal;
  if Result = mrOk then
  begin
    FI.FixedColor := FixClr.Selected;
    FI.Color := Clr.Selected;
    FI.TotalFixedColor := TotalFixClr.Selected;
    FI.TotalColor := TotalClr.Selected;
    FI.FixedFont := FixFnt.Font;
    FI.Font := Fnt.Font;
    FI.TotalFixedFont := TotalFixFnt.Font;
    FI.TotalFont := TotalFnt.Font;
    if RadioButton1.Checked then
    begin
      FI.VAlign := valTop; FI.HAlign := halLeft;
    end;
    if RadioButton2.Checked then
    begin
      FI.VAlign := valTop; FI.HAlign := halCenter;
    end;
    if RadioButton3.Checked then
    begin
      FI.VAlign := valTop; FI.HAlign := halRight;
    end;
    if RadioButton4.Checked then
    begin
      FI.VAlign := valCenter; FI.HAlign := halLeft;
    end;
    if RadioButton5.Checked then
    begin
      FI.VAlign := valCenter; FI.HAlign := halCenter;
    end;
    if RadioButton6.Checked then
    begin
      FI.VAlign := valCenter; FI.HAlign := halRight;
    end;
    if RadioButton7.Checked then
    begin
      FI.VAlign := valBottom; FI.HAlign := halLeft;
    end;
    if RadioButton8.Checked then
    begin
      FI.VAlign := valBottom; FI.HAlign := halCenter;
    end;
    if RadioButton9.Checked then
    begin
      FI.VAlign := valBottom; FI.HAlign := halRight;
    end;
    FI.Width := CellWidth.Value;
    FI.Height := CellHeight.Value;
    FI.ShowTotal := ShowTotal.Checked;
    FI.TotalCaption := TotalCaption.Text;
    FI.TotalWidth := TotalWidth.Value;
    FI.Func := TRpTotalFunc(Func.ItemIndex);
    case DataType.ItemIndex of
      0: FI.DataType:=flText;
      1: FI.DataType:=flNumber;
      2: FI.DataType:=flDate;
      3: FI.DataType:=flTime;
    end;
  end;
end;

end.

