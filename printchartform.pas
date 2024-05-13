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

unit PrintChartForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ButtonPanel, strconsts, dxcharts;

type

  { TPrintChartFm }

  TPrintChartFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ChartSize: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    OrigSize: TCheckBox;
    ChartWidth: TSpinEdit;
    ChartHeight: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OrigSizeChange(Sender: TObject);
  private

  public
    function ShowForm(C: TdxChart): Integer;
  end;

var
  PrintChartFm: TPrintChartFm;

function ShowPrintChartForm(C: TdxChart): Integer;

implementation

uses
  helpmanager;

function ShowPrintChartForm(C: TdxChart): Integer;
begin
  if PrintChartFm = nil then
    PrintChartFm := TPrintChartFm.Create(Application);
  Result := PrintChartFm.ShowForm(C);
end;

{$R *.lfm}

{ TPrintChartFm }

procedure TPrintChartFm.FormCreate(Sender: TObject);
begin
  Caption := rsPrintChart;
  OrigSize.Caption := rsOriginalSize;
  ChartSize.Caption := rsSpecifiedSize;
  Label1.Caption := rsWidth;
  Label2.Caption := rsHeight;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TPrintChartFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('printchart');
end;

procedure TPrintChartFm.OrigSizeChange(Sender: TObject);
begin
  ChartSize.Enabled := not OrigSize.Checked;
end;

function TPrintChartFm.ShowForm(C: TdxChart): Integer;
begin
  OrigSize.Checked := C.SaveOriginalSize;
  ChartWidth.Value := C.SaveImageWidth;
  ChartHeight.Value := C.SaveImageHeight;
  Result := ShowModal;
  if Result = mrOk then
  begin
    C.SaveOriginalSize := OrigSize.Checked;
    C.SaveImageWidth := ChartWidth.Value;
    C.SaveImageHeight := ChartHeight.Value;
  end;
end;

end.

