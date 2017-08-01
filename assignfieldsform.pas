unit AssignFieldsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, Menus, strconsts, dxctrls;

type

  { TAssignFieldsFm }

  TAssignFieldsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TStringGrid;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(C: TdxLookupComboBox);
  end;

var
  AssignFieldsFm: TAssignFieldsFm;

implementation

{$R *.lfm}

{ TAssignFieldsFm }

procedure TAssignFieldsFm.FormCreate(Sender: TObject);
begin
  Caption := rsAssignFields;
  Grid.Columns[0].Title.Caption:=rsObjField;
  Grid.Columns[1].Title.Caption:=rsFormField;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  MenuItem1.Caption:=rsAppend;
  MenuItem2.Caption := rsDelete;
  ToolButton1.Caption := rsAppend;
  ToolButton2.Caption := rsDelete;
  ImageList1.AddLazarusResource('check16');
  ImageList1.AddLazarusResource('delete16');
end;

procedure TAssignFieldsFm.ShowForm(C: TdxLookupComboBox);
begin

end;

end.

