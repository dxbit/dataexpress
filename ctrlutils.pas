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

unit CtrlUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, StdCtrls, Graphics, ExtCtrls, Buttons, LResources,
  Controls, LMessages, dialoggrid, checktreeview, treeviewex, Dialogs, Menus;

type

  { TFontSampler }

  {TFontSampler = class(TLabel)
  private
    FSampleFont: TFont;
    procedure SetSampleFont(AValue: TFont);
  protected
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property SampleFont: TFont read FSampleFont write SetSampleFont;
  end;   }

  { TColorSampler }

  TColorSampler = class(TShape)
  private
    FButton: TSpeedButton;
    FDefaultColor: TColor;
    FOnChange: TNotifyEvent;
    FSampleColor: TColor;
    procedure PositionButton;
    procedure SetSampleColor(AValue: TColor);
    procedure DoChange;
    procedure DoButtonClick(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property DefaultColor: TColor read FDefaultColor write FDefaultColor;
    property SampleColor: TColor read FSampleColor write SetSampleColor;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TColorButtonEx }

  TColorButtonEx = class(TColorButton)
  protected
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint;
      AState: TButtonState; ATransparent: Boolean; BiDiFlags: Longint): TRect;
      override;
    procedure DoOnShowHint(HintInfo: PHintInfo); override;
  end;

  { TComboBoxAdv }

  TComboBoxAdv = class(TComboBox)
  private
    FOldItemIndex: Integer;
  protected
    procedure DoEnter; override;
    procedure Select; override;
  public
    procedure EditingDone; override;
  end;

procedure Register;

implementation

var
  CustColors: TStringList;

procedure Register;
begin
  RegisterComponents('dxComponents', [{TFontSampler, }TColorSampler, TStringGridEx,
    TDialogGrid, TDialogGridButtons, TCheckTreeView, TColorButtonEx, TComboBoxAdv]);
end;

{ TComboBoxAdv }

procedure TComboBoxAdv.DoEnter;
begin
  inherited DoEnter;
  FOldItemIndex := ItemIndex;
end;

procedure TComboBoxAdv.Select;
begin
  inherited Select;
  if ItemIndex >= 0 then
    FOldItemIndex := ItemIndex;
end;

procedure TComboBoxAdv.EditingDone;
begin
  inherited EditingDone;
  if ItemIndex < 0 then ItemIndex := FOldItemIndex;
end;

{ TColorButtonEx }

function TColorButtonEx.DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
  const AOffset: TPoint; AState: TButtonState; ATransparent: Boolean;
  BiDiFlags: Longint): TRect;
var
  Size: TSize;
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Style := bsSolid;
  if AState = bsDisabled then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Bitmap := GetDisabledPattern;
  end
  else if ButtonColor = clNone then
  begin
    Canvas.Brush.Bitmap := nil;
    Canvas.Brush.Style := bsBDiagonal;
    Canvas.Brush.Color := clBlack;
  end
  else if ButtonColor = clDefault then
  begin
    Canvas.Brush.Bitmap := nil;
    Canvas.Brush.Style := bsClear;
  end
  else
  begin
    Canvas.Brush.Bitmap := nil;
    Canvas.Brush.Color := ButtonColor;
  end;
  Size := GetGlyphSize(true,AClient);

  Result := Bounds(AClient.Left + AOffset.X, AClient.Top + AOffset.Y,
                   Size.CX - 1, Size.CY - 1);
  Canvas.Rectangle(Result);
end;

procedure TColorButtonEx.DoOnShowHint(HintInfo: PHintInfo);
var
  R, G, B: Byte;
  S: String;
begin
  inherited DoOnShowHint(HintInfo);
  RedGreenBlue(ButtonColor, R, G, B);
  if ColorToIdent(ButtonColor, S) then
    S := Format('%s (rgb: %d, %d, %d)', [Copy(S, 3, 255), R, G, B])
  else
    S := Format('rgb: %d, %d, %d', [R, G, B]);
  HintInfo^.HintStr := S;
end;

{ TColorSampler }

procedure TColorSampler.DoButtonClick(Sender: TObject);
begin
  SampleColor := DefaultColor;
  DoChange;
end;

procedure TColorSampler.PositionButton;
begin
  if FButton = nil then exit;
  FButton.Parent := Parent;
  FButton.Visible := Visible;
  FButton.AnchorToCompanion(akLeft,0,Self);
end;

procedure TColorSampler.SetSampleColor(AValue: TColor);
var
  S: String;
begin
  //if FSampleColor=AValue then Exit;
  FSampleColor:=AValue;
  Brush.Color := AValue;
  S := ColorToString(AValue);
  if Copy(S, 1, 2) = 'cl' then Delete(S, 1, 2)
  else S := Format('r: %d g: %d b: %d', [Red(AValue), Green(AValue), Blue(AValue)]);
  Hint := S;
end;

procedure TColorSampler.DoChange;
begin
  if FOnChange <> nil then FOnChange(Self);
end;

procedure TColorSampler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FButton) and (Operation = opRemove) then
    FButton := nil;
end;

procedure TColorSampler.Loaded;
begin
  inherited Loaded;
  PositionButton;
end;

procedure TColorSampler.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if FButton <> nil then
    PositionButton;
end;

procedure TColorSampler.CMVisibleChanged(var Msg: TLMessage);
begin
  if FButton <> nil then
    FButton.Visible := Visible;
end;

procedure TColorSampler.CMEnabledChanged(var Msg: TLMessage);
begin
  if FButton<>nil then
    FButton.Enabled:=Enabled;
end;

procedure TColorSampler.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    with TColorDialog.Create(Self.Parent) do
    try
      Color:=SampleColor;
      if CustColors.Count > 0 then CustomColors.Assign(CustColors);
      if Execute then
      begin
        SampleColor := Color;
        CustColors.Assign(CustomColors);
        DoChange;
      end;
    finally
      Free;
    end;
end;

constructor TColorSampler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 88; Height := 23;
  Brush.Color := clWhite; Pen.Color:=clBlack;
  FButton := TSpeedButton.Create(Self);
  FButton.Width := Self.Height;
  FButton.Height := Self.Height;
  FButton.FreeNotification(Self);
  FButton.OnClick:=@DoButtonClick;
  FButton.Cursor := crArrow;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable];
  SetupSpeedButton(FButton, '_delete16');
  FButton.Flat := True;
  FDefaultColor := clDefault;
  ShowHint := True;
end;

{ TFontSampler }

{procedure TFontSampler.SetSampleFont(AValue: TFont);
begin
  FSampleFont.Assign(AValue);
  Font.Assign(AValue);
  Caption := Font.Name;
end;

procedure TFontSampler.Click;
begin
  inherited Click;
  //FontFm.ShowForm(FSampleFont);
  SampleFont := FSampleFont;
end;

constructor TFontSampler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSampleFont := TFont.Create;
  Color := clWhite;
  Transparent:=False;
  AutoSize := False;
  Alignment:=taCenter;
  Layout:=tlCenter;
  Width := 150; Height := 50;
  Caption := Font.Name;
end;

destructor TFontSampler.Destroy;
begin
  FSampleFont.Free;
  inherited Destroy;
end;

procedure TFontSampler.Paint;
begin
  inherited Paint;
  Canvas.Pen.Color:=clBlack;
  Canvas.MoveTo(0, 0);
  Canvas.LineTo(Width - 1, 0);
  Canvas.LineTo(Width - 1, Height - 1);
  Canvas.LineTo(0, Height - 1);
  Canvas.LineTo(0, 0);
end;     }

initialization
  {$i dxcomponents.lrs}
  CustColors := TStringList.Create;


finalization
  FreeAndNil(CustColors);

end.

