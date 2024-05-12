unit ImageProcessPrintForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ButtonPanel, dximages, strconsts;

type

  { TImgProcessPrintFm }

  TImgProcessPrintFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    RB1: TRadioButton;
    RB2: TRadioButton;
    PrintSize: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure RB1Change(Sender: TObject);
  private

  public
    function ShowForm(C: TdxDBImage): Integer;
  end;

var
  ImgProcessPrintFm: TImgProcessPrintFm;

function ShowImgProcessPrintForm(C: TdxDBImage): Integer;

implementation

uses
  HelpManager;

function ShowImgProcessPrintForm(C: TdxDBImage): Integer;
begin
  if ImgProcessPrintFm = nil then
    ImgProcessPrintFm := TImgProcessPrintFm.Create(Application);
  Result := ImgProcessPrintFm.ShowForm(C);
end;

{$R *.lfm}

{ TImgProcessPrintFm }

procedure TImgProcessPrintFm.RB1Change(Sender: TObject);
begin
  PrintSize.Enabled := RB2.Checked;
end;

procedure TImgProcessPrintFm.FormCreate(Sender: TObject);
begin
  Caption := rsImgProcessPrint;
  RB1.Caption := rsSaveSize;
  RB2.Caption := rsReduceTo;
  ButtonPanel1.OKButton.Caption:=rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TImgProcessPrintFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('imageprocessprint');
end;

function TImgProcessPrintFm.ShowForm(C: TdxDBImage): Integer;
begin
  PrintSize.Value := C.PrintSize;
  if PrintSize.Value = 0 then RB1.Checked := True
  else RB2.Checked := True;
  Result := ShowModal;
  if Result = mrOk then
    if RB1.Checked then
      C.PrintSize := 0
    else
      C.PrintSize := PrintSize.Value;
end;

end.

