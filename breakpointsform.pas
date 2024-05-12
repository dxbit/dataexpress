unit BreakpointsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ButtonPanel, Menus, strconsts;

{ TBreakpointsFm }

type
  TBreakpointsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TStringGrid;
    Images: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
  private
    { private declarations }
    procedure FillGrid;
    procedure SetControlState;
  public
    { public declarations }
    function ShowForm: Integer;
  end;

var
  BreakpointsFm: TBreakpointsFm;

function ShowBreakpointsForm: Integer;

implementation

uses
  apputils, scriptmanager, LazUtf8, scriptform;

function ShowBreakpointsForm: Integer;
begin
  if BreakpointsFm = nil then
  	BreakpointsFm := TBreakpointsFm.Create(Application);
  Result := BreakpointsFm.ShowForm;
end;

{$R *.lfm}

{ TBreakpointsFm }

procedure TBreakpointsFm.FormCreate(Sender: TObject);
begin
  Caption := rsBreakpoints;
  Grid.Columns[0].Title.Caption := rsModuleName;
  Grid.Columns[1].Title.Caption := rsRowNum;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption := rsCancel;
  ToolButton1.Caption := rsDelete;
  ToolButton2.Caption := rsDeleteAll;
  ToolButton4.Caption := rsGoTo;
  with Images do
	begin
    AddLazarusResource('delete16');
    AddLazarusResource('goto16');
  end;
  MenuItem1.Caption:=rsDelete;
  MenuItem2.Caption:=rsDeleteAll;
  MenuItem4.Caption:=rsGoTo;
  SetMenuItemImage(MenuItem1, 'delete16');
  SetMenuItemImage(MenuItem2, 'delete16');
  SetMenuItemImage(MenuItem4, 'goto16');
end;

procedure TBreakpointsFm.FormShow(Sender: TObject);
begin
  Grid.SetFocus;
  SetControlState;
end;

procedure TBreakpointsFm.GridDblClick(Sender: TObject);
begin
  MenuItem4.Click;
end;

procedure TBreakpointsFm.MenuItem1Click(Sender: TObject);
var
  SD: TScriptData;
  Mark: TSourceMarkData;
begin
  if Confirm(rsDeletingBreakpoint, rsDeleteBreakpointMsg) = mrNo then Exit;
  SD := TScriptData(Grid.Objects[0, Grid.Row]);
  Mark := TSourceMarkData(Grid.Objects[1, Grid.Row]);
  SD.SourceData.Marks.DeleteBreakpoint(Mark);
  Grid.DeleteRow(Grid.Row);
  SetControlState;
end;

procedure TBreakpointsFm.MenuItem2Click(Sender: TObject);
var
  i: Integer;
begin
  if Confirm(rsDeletingAllBreakpoints, rsDeleteAllBreakpointsMsg) = mrNo
    then Exit;
  for i := 0 to ScriptMan.ScriptCount - 1 do
    ScriptMan.Scripts[i].SourceData.Marks.ClearBreakpoints;
  Grid.RowCount:=1;
  SetControlState;
end;

procedure TBreakpointsFm.MenuItem4Click(Sender: TObject);
var
  SD: TScriptData;
  Mark: TSourceMarkData;
begin
  SD := TScriptData(Grid.Objects[0, Grid.Row]);
  Mark := TSourceMarkData(Grid.Objects[1, Grid.Row]);
  ScriptFm.GoToBreakpoint(SD, Mark);
end;

procedure TBreakpointsFm.ToolButton1Click(Sender: TObject);
begin
  MenuItem1.Click;
end;

procedure TBreakpointsFm.ToolButton2Click(Sender: TObject);
begin
  MenuItem2.Click;
end;

procedure TBreakpointsFm.ToolButton4Click(Sender: TObject);
begin
  MenuItem4.Click;
end;

procedure TBreakpointsFm.FillGrid;
var
  i, j: Integer;
  SD: TScriptData;
  M: TSourceMarkData;
  ModName: String;

  procedure AddRow(ASD: TScriptData; const ModuleName: String; Mark: TSourceMarkData);
  var
    i, r, n: Integer;
    S: String;
  begin
    r := 0;
    for i := 1 to Grid.RowCount - 1 do
    begin
      S := Grid.Cells[0, i];
      n := MyUtf8CompareText(ModuleName, S);
      if n < 0 then
      begin
        r := i; Break;
      end
      else if n = 0 then
      begin
        S := Grid.Cells[1, i];
        if Mark.Row < StrToInt(S) then
        begin
          r := i; Break;
        end;
      end;
    end;
    if r = 0 then
    begin
      Grid.RowCount:=Grid.RowCount + 1;
      r := Grid.RowCount - 1;
    end
    else
    	Grid.InsertColRow(False, r);
    Grid.Cells[0, r] := ModuleName;
    Grid.Cells[1, r] := IntToStr(Mark.Row);
    Grid.Objects[0, r] := ASD;
    Grid.Objects[1, r] := Mark;
  end;

begin
  Grid.RowCount := 1;
  for i := 0 to ScriptMan.ScriptCount - 1 do
  begin
    SD := ScriptMan.Scripts[i];
    ModName := SD.GetModuleName;
    for j := 0 to SD.SourceData.Marks.Count - 1 do
    begin
      M := SD.SourceData.Marks[j];
      if not M.IsBookmark then
				AddRow(SD, ModName, M);
    end;
  end;
end;

procedure TBreakpointsFm.SetControlState;
var
  b: Boolean;
begin
  b := Grid.Row > 0;
  ToolButton1.Enabled := b;
  ToolButton2.Enabled := b;
  ToolButton4.Enabled := b;
  MenuItem1.Enabled := b;
  MenuItem2.Enabled := b;
  MenuItem4.Enabled := b;
end;

function TBreakpointsFm.ShowForm: Integer;
begin
  FillGrid;
  Result := ShowModal;
end;

end.

