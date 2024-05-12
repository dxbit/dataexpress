unit CalcErrsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, Menus, strconsts, DialogGrid, Buttons, erroricon,
  Grids, ExtCtrls, ComCtrls, SynEditTypes;

type

  { TCalcErrsFm }

  TCalcErrsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TDialogGrid;
    Memo: TSynEdit;
    Panel1: TPanel;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure HelpButtonClick(Sender: TObject);
    procedure MemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    { private declarations }
    FErrs: TStrings;
    FCE: TCalcError;
    procedure FillGrid;
  public
    { public declarations }
    procedure ShowForm(CE: TCalcError);
  end;

var
  CalcErrsFm: TCalcErrsFm;

procedure ShowCalcErrsForm(CE: TCalcError);

implementation

procedure ShowCalcErrsForm(CE: TCalcError);
begin
  if CalcErrsFm = nil then
  	CalcErrsFm := TCalcErrsFm.Create(Application);
  CalcErrsFm.ShowForm(CE);
end;

{$R *.lfm}

{ TCalcErrsFm }

procedure TCalcErrsFm.FormCreate(Sender: TObject);
begin
  Caption := rsErrorsInExpr;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Kind:=bkCustom;
  ButtonPanel1.HelpButton.Caption := rsClear;
  ButtonPanel1.HelpButton.LoadGlyphFromLazarusResource('delete16');
end;

procedure TCalcErrsFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
end;

procedure TCalcErrsFm.GridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  EM: TErrMsg;
  p, L: Integer;
begin
  EM := TErrMsg(Grid.Objects[0, aRow]);
  if EM <> nil then
  begin
    L := Length(EM.Expr);
    Memo.Text := EM.Expr;
    p := EM.Position;
    if p = 0 then Inc(p);
    Memo.SelStart := p;
    Inc(p);
    while (p <= L) and (EM.Expr[p] in [#129..#191]) do Inc(p);
    Memo.SelEnd := p;
  end;
end;

procedure TCalcErrsFm.HelpButtonClick(Sender: TObject);
begin
  Grid.RowCount := 1;
  Memo.Clear;
  FCE.ErrList.Clear;
end;

procedure TCalcErrsFm.MemoStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  StatusBar.SimpleText := Format('%d: %d', [Memo.CaretY, Memo.CaretX]);
end;

procedure TCalcErrsFm.FillGrid;
var
  EM: TErrMsg;
  r, i: Integer;
begin
  r := 1;
  Grid.RowCount := r;
  for i := FCE.ErrList.Count - 1 downto 0 do
  begin
    EM := FCE.ErrList[i];
    Grid.RowCount := r + 1;
    Grid.Objects[0, r] := EM;
    Grid.Cells[0, r] := EM.Tp;
    Grid.Cells[1, r] := EM.Name;
    Grid.Cells[2, r] := EM.Prop;
    Grid.Cells[3, r] := EM.Msg;
    Memo.Text := EM.Expr;
    Memo.SelStart := EM.Position;
    Grid.Cells[4, r] := Format('%d: %d', [Memo.CaretY, Memo.CaretX]);
    Inc(r);
  end;
end;

procedure TCalcErrsFm.ShowForm(CE: TCalcError);
begin
  FCE := CE;
  Memo.Clear;
  FillGrid;
  ShowModal;
end;

end.

