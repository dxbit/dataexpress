unit ProcessMiniForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TProcessMiniFm }

  TProcessMiniFm = class(TForm)
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ProcessMiniFm: TProcessMiniFm;

procedure ShowProcessMiniForm(const Msg: String);
procedure FreeProcessMiniForm;

implementation

procedure ShowProcessMiniForm(const Msg: String);
begin
  if ProcessMiniFm = nil then
    ProcessMiniFm := TProcessMiniFm.Create(Application);

  with ProcessMiniFm do
	begin
    Label1.Caption := Msg;
    Show;
  end;
end;

procedure FreeProcessMiniForm;
begin
  FreeAndNil(ProcessMiniFm);
end;

{$R *.lfm}

end.

