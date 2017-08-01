unit ListBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Buttons, LResources;

type

  TListBarButton = (bnAdd, bnEdit, bnDelete, bnUp, bnDown);
  TListBarButtons = set of TListBarButton;

  TLBButtonClickEvent = procedure (Sender: TObject; Bn: TListBarButton) of object;

  { TListBar }

  TListBar = class(TCustomPanel)
  private
    FButtons: TList;
    FOnButtonClick: TLBButtonClickEvent;
    FShowButtons: TListBarButtons;
    procedure CreateButtons;
    procedure UpdateButtons;
    procedure SetShowButtons(AValue: TListBarButtons);
    procedure ButtonClick(Sender: TObject);
  protected
    procedure Loading; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ShowButtons: TListBarButtons read FShowButtons write SetShowButtons;
    property Align;
    property BorderSpacing;
    property OnButtonClick: TLBButtonClickEvent read FOnButtonClick
      write FOnButtonClick;
  end;

implementation

const
  ButtonWidth = 23;
  ButtonHeight = 22;
  ButtonCount = 5;

{ TListBar }

procedure TListBar.CreateButtons;
const
  BnRes: array [TListBarButton] of String = ('add16', 'edit16', 'delete16',
    'up16', 'down16');
var
  i: Integer;
  Bn: TSpeedButton;
begin
  for i := 1 to ButtonCount do
  begin
    Bn := TSpeedButton.Create(Self);
    Bn.Width := ButtonWidth;
    Bn.Height := ButtonHeight;
    Bn.Flat := True;
    Bn.LoadGlyphFromLazarusResource(BnRes[TListBarButton(i - 1)]);
    Bn.Visible:=False;
    Bn.Parent := Self;
    Bn.Tag := i - 1;
    Bn.OnClick:=@ButtonClick;
    FButtons.Add(Bn);
  end;
end;

procedure TListBar.UpdateButtons;
var
  i, x: Integer;
  Bn: TSpeedButton;
begin
  x := 0;
  for i := 0 to ButtonCount - 1 do
  begin
    Bn := TSpeedButton(FButtons[i]);
    if TListBarButton(i) in FShowButtons then
    begin
      Bn.Left := x;
      Bn.Visible:=True;
      x := x + ButtonWidth;
      if csDesigning in ComponentState then
        Bn.ControlStyle := Bn.ControlStyle - [csNoDesignVisible];
    end
    else
    begin
      Bn.Visible := False;
      if csDesigning in ComponentState then
        Bn.ControlStyle := Bn.ControlStyle + [csNoDesignVisible];
    end;
  end;
  ClientWidth := x;
end;

procedure TListBar.SetShowButtons(AValue: TListBarButtons);
begin
  if FShowButtons=AValue then Exit;
  FShowButtons:=AValue;
  UpdateButtons;
end;

procedure TListBar.ButtonClick(Sender: TObject);
begin
  if FOnButtonClick <> nil then
    FOnButtonClick(Self, TListBarButton( TSpeedButton(Sender).Tag ));
end;

procedure TListBar.Loading;
begin
  inherited Loading;
  CreateButtons;
  UpdateButtons;
end;

constructor TListBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ClientHeight := ButtonHeight;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  ControlStyle := ControlStyle - [csSetCaption];
  Align := alTop;
  FShowButtons := [bnAdd, bnEdit, bnDelete, bnUp, bnDown];
  FButtons := TList.Create;
  CreateButtons;
  UpdateButtons;
end;

destructor TListBar.Destroy;
begin
  FButtons.Free;
  inherited Destroy;
end;

initialization
{$i listbar.lrs}

end.

