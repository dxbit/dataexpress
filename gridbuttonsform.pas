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
    FGrid: TMyDBGrid;
  public
    { public declarations }
    procedure ShowForm(G: TMyDBGrid);
  end;

var
  GridButtonsFm: TGridButtonsFm;

procedure ShowGridButtonsForm(G: TMyDBGrid);

implementation

uses
  fontform, helpmanager, dxctrls;

procedure ShowGridButtonsForm(G: TMyDBGrid);
begin
  if GridButtonsFm = nil then
  	GridButtonsFm := TGridButtonsFm.Create(Application);
  GridButtonsFm.ShowForm(G);
end;

{$R *.lfm}

{ TGridButtonsFm }

procedure TGridButtonsFm.FontBnsClick(Sender: TObject);
begin
  ShowFontForm(FontBns.Font, TdxForm(FGrid.Owner).Font);
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
  FGrid := G;
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
      if VisBns.Checked[j] then Include(Bns, TGridButtonType(PtrInt(VisBns.Items.Objects[j])));
    G.VisibleButtons:=Bns;
    Bns := [];
    for j := 0 to VisCaps.Items.Count - 1 do
      if VisCaps.Checked[j] then Include(Bns, TGridButtonType(PtrInt(VisCaps.Items.Objects[j])));
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

