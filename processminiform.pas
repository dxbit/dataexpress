{-------------------------------------------------------------------------------

    Copyright 2015-2024 Pavel Duborkin ( mydataexpress@mail.ru )

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

-------------------------------------------------------------------------------}

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

