unit FilterForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, strconsts, dxctrls, filtercontrol,
  myclasses, LclType;

type
  { TFilterFm }

  TFilterFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    FForm: TdxForm;
    FGrid: TFormFilterControl;
    { private declarations }
  public
    { public declarations }
    function ShowForm(var Filter: TFilterObject): Integer;
    property Form: TdxForm read FForm write FForm;
  end;

//var
//  FilterFm: TFilterFm;

implementation

uses
  helpmanager;

{$R *.lfm}

{ TFilterFm }

procedure TFilterFm.FormCreate(Sender: TObject);
begin
  Caption := rsFilter;
  FGrid := TFormFilterControl.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align := alClient;
  FGrid.BorderSpacing.Left:=4;
  FGrid.BorderSpacing.Right:=4;
  FGrid.Buttons := ToolBar1;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
  ButtonPanel1.OKButton.Hint:=rsApplyFilter;
  ButtonPanel1.OKButton.Default := False;
  ImageList1.AddLazarusResource('add16');
  ImageList1.AddLazarusResource('delete16');
  ToolButton1.Caption:=rsAddField;
  ToolButton2.Hint:=rsDeleteField;
  ToolButton4.Caption:=rsAddValue;
  ToolButton5.Hint := rsDeleteValue;
end;

procedure TFilterFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    Key := 0;
    FGrid.EditorMode := False;
    ModalResult := mrOk;
  end;
end;

procedure TFilterFm.FormShow(Sender: TObject);
begin
  FGrid.SetFocus;
  FGrid.Col := 3;
  if FGrid.RowCount > 1 then FGrid.Row := 1;
end;

procedure TFilterFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('filter');
end;

function TFilterFm.ShowForm(var Filter: TFilterObject): Integer;
begin
  FGrid.Form := FForm;
  FGrid.Filter := Filter;
  FGrid.Init;
  FGrid.Load;
  Result := ShowModal;
  if Result = mrOk then
  begin
    FGrid.Save;
    Filter := FGrid.Filter;
  end;
end;

end.

