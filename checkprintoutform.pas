unit CheckPrintOutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, strconsts, dxctrls;

type

  { TCheckPrintOutFm }

  TCheckPrintOutFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(Check: TdxCheckBox): Integer;
  end;

var
  CheckPrintOutFm: TCheckPrintOutFm;

function ShowCheckPrintOutForm(Check: TdxCheckBox): Integer;

implementation

uses
  helpmanager;

function ShowCheckPrintOutForm(Check: TdxCheckBox): Integer;
begin
  if CheckPrintOutFm = nil then
  	CheckPrintOutFm := TCheckPrintOutFm.Create(Application);
  Result := CheckPrintOutFm.ShowForm(Check);
end;

{$R *.lfm}

{ TCheckPrintOutFm }

procedure TCheckPrintOutFm.FormCreate(Sender: TObject);
begin
  Caption := rsPrintOut;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TCheckPrintOutFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('checkout');
end;

function TCheckPrintOutFm.ShowForm(Check: TdxCheckBox): Integer;
begin
  Edit1.Text := Check.CheckedText;
  Edit2.Text := Check.UnCheckedText;
  Result := ShowModal;
  if Result <> mrOk then Exit;
  Check.CheckedText:=Edit1.Text;
  Check.UnCheckedText:=Edit2.Text;
end;

end.

