unit PanelForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ButtonPanel, Buttons, CtrlUtils, dxctrls, strconsts;

type

  { TPanelFm }

  TPanelFm = class(TForm)
    BevelClr: TColorSampler;
    BackClr: TColorSampler;
    ResetBn: TBitBtn;
    ButtonPanel1: TButtonPanel;
    StyleCbx: TComboBox;
    SampleGrp: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    BevelWidthSpn: TSpinEdit;
    RadiusSpn: TSpinEdit;
    procedure BackClrChange(Sender: TObject);
    procedure BevelClrChange(Sender: TObject);
    procedure BevelWidthSpnChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure RadiusSpnChange(Sender: TObject);
    procedure ResetBnClick(Sender: TObject);
    procedure StyleCbxChange(Sender: TObject);
  private
    FSamplePan: TdxPanel;
    FStopUpdate: Boolean;
    procedure UpdateSample;
    procedure SetControlState;
  public
    function ShowForm(APan: TdxPanel): Integer;
  end;

var
  PanelFm: TPanelFm;

function ShowPanelForm(APan: TdxPanel): Integer;

implementation

uses
  HelpManager;

function ShowPanelForm(APan: TdxPanel): Integer;
begin
  if PanelFm = nil then
    PanelFm := TPanelFm.Create(Application);
  Result := PanelFm.ShowForm(APan);
end;

{$R *.lfm}

{ TPanelFm }

procedure TPanelFm.FormCreate(Sender: TObject);
begin
  Caption := rsPanel;
  Label1.Caption := rsBevelStyle;
  Label2.Caption := rsWidth;
  Label3.Caption := rsRadius;
  Label4.Caption := rsBevelColor;
  Label5.Caption := rsBackColor;
  ResetBn.Caption := rsReset;
  SampleGrp.Caption := rsExample;
  ButtonPanel1.OkButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;

  FSamplePan := TdxPanel.Create(Self);
  with FSamplePan do
  begin
    Parent := SampleGrp;
    Width := 200;
    Height := 200;
    AnchorSideLeft.Control := SampleGrp;
    AnchorSideLeft.Side := asrCenter;
    AnchorSideTop.Control := SampleGrp;
    AnchorSideTop.Side := asrCenter;
  end;
  BevelClr.DefaultColor := clSilver;
  StyleCbx.Items.AddStrings([rsNone, rsDefault, rsSolid, rsDash, rsDot]);
end;

procedure TPanelFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('panel');
end;

procedure TPanelFm.RadiusSpnChange(Sender: TObject);
begin
  UpdateSample;
end;

procedure TPanelFm.ResetBnClick(Sender: TObject);
begin
  FStopUpdate := True;
  StyleCbx.ItemIndex := 1;
  BevelWidthSpn.Value := 1;
  RadiusSpn.Value := 0;
  BevelClr.SampleColor := BevelClr.DefaultColor;
  BackClr.SampleColor := BackClr.DefaultColor;
  FStopUpdate := False;
  UpdateSample;
  SetControlState;
end;

procedure TPanelFm.BevelWidthSpnChange(Sender: TObject);
begin
  UpdateSample;
end;

procedure TPanelFm.BevelClrChange(Sender: TObject);
begin
  UpdateSample;
end;

procedure TPanelFm.BackClrChange(Sender: TObject);
begin
  UpdateSample;
end;

procedure TPanelFm.StyleCbxChange(Sender: TObject);
begin
  UpdateSample;
  SetControlState;
end;

procedure TPanelFm.UpdateSample;
begin
  if FStopUpdate then Exit;
  with FSamplePan do
  begin
    BevelStyle := TdxPanelBevelStyle(StyleCbx.ItemIndex);
    BevelWidth := BevelWidthSpn.Value;
    BevelRadius := RadiusSpn.Value;
    BevelColor := BevelClr.SampleColor;
    Color := BackClr.SampleColor;
  end;
end;

procedure TPanelFm.SetControlState;
var
  b: Boolean;
begin
  b := StyleCbx.ItemIndex > 1;
  BevelWidthSpn.Enabled := b;
  RadiusSpn.Enabled := b;
  BevelClr.Enabled := b;
end;

function TPanelFm.ShowForm(APan: TdxPanel): Integer;
begin
  FStopUpdate := True;
  StyleCbx.ItemIndex := Ord(APan.BevelStyle);
  BevelWidthSpn.Value := APan.BevelWidth;
  RadiusSpn.Value := APan.BevelRadius;
  BevelClr.SampleColor := APan.BevelColor;
  BackClr.SampleColor := APan.Color;
  BackClr.DefaultColor := APan.Parent.Color;
  FStopUpdate := False;

  UpdateSample;
  SetControlState;

  Result := ShowModal;

  if Result = mrOk then
  begin
    APan.BevelStyle := TdxPanelBevelStyle(StyleCbx.ItemIndex);
    APan.BevelWidth := BevelWidthSpn.Value;
    APan.BevelRadius := RadiusSpn.Value;
    APan.BevelColor := BevelClr.SampleColor;
    APan.Color := BackClr.SampleColor;
  end;
end;

end.

