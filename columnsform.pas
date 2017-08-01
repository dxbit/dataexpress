unit ColumnsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel, Grids, Buttons, DBGrids, strconsts, dxctrls, CtrlUtils;

type

  { TColumnsFm }

  TColumnsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ColorSampler1: TColorSampler;
    ColorSampler2: TColorSampler;
    FontSampler1: TFontSampler;
    FontSampler2: TFontSampler;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Cols: TListBox;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure ColsSelectionChange(Sender: TObject; User: boolean);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FGrid: TdxGrid;
    FFm: TdxForm;
    FIndex: Integer;
    procedure FillColumns;
    procedure LoadItem;
    procedure SaveItem;
    procedure ClearControls;
    function GetCol: TColumn;
  public
    { public declarations }
    function ShowForm(aForm: TdxForm): Integer;
  end;

var
  ColumnsFm: TColumnsFm;

implementation

{$R *.lfm}

{ TColumnsFm }

procedure TColumnsFm.FillColumns;
var
  i: Integer;
  Col: TColumn;
  C: TComponent;
begin
  Cols.Clear;
  for i := 0 to FGrid.Columns.Count - 1 do
  begin
    Col := FGrid.Columns[i];
    C := FindById(FFm, Col.Tag);
    Cols.Items.AddObject(GetFieldName(C), Col);
  end;
end;

procedure TColumnsFm.LoadItem;
var
  C: TColumn;
begin
  C := GetCol;
  if C = nil then Exit;
  FontSampler1.SampleFont := C.Font;
  FontSampler2.SampleFont := C.Title.Font;
  ColorSampler1.SampleColor := C.Color;
  ColorSampler2.SampleColor := C.Title.Color;
end;

procedure TColumnsFm.SaveItem;
var
  C: TColumn;
begin
  C := GetCol;
  if C = nil then Exit;
  C.Font := FontSampler1.SampleFont;
  C.Title.Font := FontSampler2.SampleFont;
  C.Color := ColorSampler1.SampleColor;
  C.Title.Color := ColorSampler2.SampleColor;
end;

procedure TColumnsFm.ClearControls;
begin
  FontSampler1.SampleFont := FGrid.Font;
  FontSampler2.SampleFont := FGrid.TitleFont;
  ColorSampler1.SampleColor := FGrid.Color;
  ColorSampler2.SampleColor := FGrid.FixedColor;
end;

function TColumnsFm.GetCol: TColumn;
begin
  Result := nil;
  if FIndex >= 0 then
    Result := TColumn(Cols.Items.Objects[FIndex]);
end;

function TColumnsFm.ShowForm(aForm: TdxForm): Integer;
begin
  FFm := aForm;
  FGrid := aForm.Grid;
  FillColumns;
  ClearControls;
  FIndex := -1;
  Result := ShowModal;
  if Result = mrOk then SaveItem;
end;

procedure TColumnsFm.ColsSelectionChange(Sender: TObject; User: boolean);
begin
  SaveItem;
  FIndex := Cols.ItemIndex;
  LoadItem;
end;

procedure TColumnsFm.FormCreate(Sender: TObject);
begin
  Caption := rsColumns;
  Label1.Caption := rsFont;
  Label2.Caption := rsFont;
  Label3.Caption := rsColor;
  Label4.Caption := rsColor;
  Label5.Caption := rsDataArea;
  Label6.Caption := rsTitle;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
end;

end.

