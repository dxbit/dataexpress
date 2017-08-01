unit DateNowForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  dxctrls, strconsts;

type

  { TDateNowFm }

  TDateNowFm = class(TForm)
    Check: TCheckBox;
    procedure CheckChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(C: TdxDateEdit): Integer;
  end;

var
  DateNowFm: TDateNowFm;

implementation

{$R *.lfm}

{ TDateNowFm }

procedure TDateNowFm.CheckChange(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TDateNowFm.FormCreate(Sender: TObject);
begin
  Check.Caption := rsNow;
end;

function TDateNowFm.ShowForm(C: TdxDateEdit): Integer;
begin
  Check.Checked := C.DateNow;
  Result := ShowModal;
  if Result = mrOk then
    C.DateNow := Check.Checked;
end;

end.

