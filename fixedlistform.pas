unit FixedListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel, Spin, strconsts, dxctrls;

type

  { TFixedListFm }

  TFixedListFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    Label2: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    RowCnt: TSpinEdit;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowForm(C: TdxComboBox);
  end;

var
  FixedListFm: TFixedListFm;

procedure ShowFixedListForm(C: TdxComboBox);

implementation

uses
  apputils;

procedure ShowFixedListForm(C: TdxComboBox);
begin
  if FixedListFm = nil then
  	FixedListFm := TFixedListFm.Create(Application);
  FixedListFm.ShowForm(C);
end;

{$R *.lfm}

{ TFixedListFm }

procedure TFixedListFm.FormCreate(Sender: TObject);
begin
  Caption := rsList;
  CheckBox1.Caption:=rsOnlyList;
  Label2.Caption := rsRowCountInList;
  ButtonPanel1.OKButton.Caption := rsOK;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TFixedListFm.ShowForm(C: TdxComboBox);
begin
  Memo1.Lines := C.Items;
  CheckBox1.Checked := C.Style = csDropDownList;
  CheckBox1.Enabled := C.SourceTId = 0;
  RowCnt.Value := C.DropDownCount;
  if ShowModal = mrOk then
  begin
    if (CheckBox1.Checked) and (C.SourceTId = 0) then
      C.Style := csDropDownList
    else C.Style := csDropDown;
    C.Items := Memo1.Lines;
    C.DropDownCount:=RowCnt.Value;
  end;
end;

end.

