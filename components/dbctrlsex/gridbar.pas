unit GridBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Buttons, LResources;

type

  TGridBarButton = (gbnAdd, gbnEdit, gbnDelete, gbnRefresh, gbnFilter,
    gbnPrint, gbnImport, gbnExport);
  TGridBarButtons = set of TGridBarButton;

  TLBButtonClickEvent = procedure (Sender: TObject; Bn: TGridBarButton) of object;

  { TGridBar }

  TGridBar = class(TCustomPanel)
  private
    FButtons: TList;
    FOnButtonClick: TLBButtonClickEvent;
    FShowButtons: TGridBarButtons;
    procedure CreateButtons;
    procedure UpdateButtons;
    procedure SetShowButtons(AValue: TGridBarButtons);
    procedure ButtonClick(Sender: TObject);
  protected
    procedure Loading; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ShowButtons: TGridBarButtons read FShowButtons write SetShowButtons;
    property Align;
    property OnButtonClick: TLBButtonClickEvent read FOnButtonClick
      write FOnButtonClick;
  end;

implementation

const
  ButtonWidth = 33;
  ButtonHeight = 32;
  ButtonCount = 8;

{ TGridBar }

procedure TGridBar.CreateButtons;
const
  BnRes: array [TGridBarButton] of String = ('add24', 'edit24', 'delete24',
    'refresh24', 'filter24', 'print24', 'import24', 'export24');
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
    Bn.LoadGlyphFromLazarusResource(BnRes[TGridBarButton(i - 1)]);
    Bn.Visible:=False;
    Bn.Parent := Self;
    Bn.Tag := i - 1;
    Bn.OnClick:=@ButtonClick;
    FButtons.Add(Bn);
  end;
end;

procedure TGridBar.UpdateButtons;
var
  i, x: Integer;
  Bn: TSpeedButton;
begin
  x := 0;
  for i := 0 to ButtonCount - 1 do
  begin
    Bn := TSpeedButton(FButtons[i]);
    if TGridBarButton(i) in FShowButtons then
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

procedure TGridBar.SetShowButtons(AValue: TGridBarButtons);
begin
  if FShowButtons=AValue then Exit;
  FShowButtons:=AValue;
  UpdateButtons;
end;

procedure TGridBar.ButtonClick(Sender: TObject);
begin
  if FOnButtonClick <> nil then
    FOnButtonClick(Self, TGridBarButton( TSpeedButton(Sender).Tag ));
end;

procedure TGridBar.Loading;
begin
  inherited Loading;
  CreateButtons;
  UpdateButtons;
end;

constructor TGridBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ClientHeight := ButtonHeight;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  ControlStyle := ControlStyle - [csSetCaption];
  Align := alTop;
  FShowButtons := [gbnAdd, gbnEdit, gbnDelete, gbnRefresh, gbnFilter,
    gbnImport, gbnExport];
  FButtons := TList.Create;
  CreateButtons;
  UpdateButtons;
end;

destructor TGridBar.Destroy;
begin
  FButtons.Free;
  inherited Destroy;
end;

initialization
{$i gridbar.lrs}

end.

