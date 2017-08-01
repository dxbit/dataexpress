{
Copyright © 2015-2017 Pavel Duborkin
Author: Pavel Duborkin
E-Mail: 7bit@list.ru, mydataexpress@mail.ru

This file is part of DataExpress.

DataExpress is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

DataExpress is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with DataExpress.  If not, see <http://www.gnu.org/licenses/>.
}
unit ModulesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, StdCtrls, ButtonPanel, scriptmanager, strconsts;

{ TModulesFm }

type
  TModulesFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TStringGrid;
    ImageList1: TImageList;
    Memo1: TMemo;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure HelpButtonClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
  private
    { private declarations }
    FModified: Boolean;
    procedure AddModule(SD: TScriptData);
    procedure FillModules;
    procedure UpdateMemo;
    procedure SetControlState;
  public
    { public declarations }
    procedure ShowForm;
  end;

var
  ModulesFm: TModulesFm;

implementation

uses
  apputils, designerframe, mainframe, mydialogs, appsettings, scriptform,
  helpform;

{$R *.lfm}

function ImportModuleDlg: String;
var
  Flt: String;
begin
  Result := '';
  Flt := rsExprModulesFlt +	rsAllFilesFlt;
  with TOpenDialog.Create(nil) do
  begin
    Title := rsImportModule;
    Options := Options + [ofFileMustExist];
    Filter := Flt;
  	if Execute then
    	Result := FileName;
    Free;
  end;
end;

function ExportModuleDlg: String;
var
  Flt: String;
begin
  Flt := rsExprModulesFlt + rsAllFilesFlt;
  with TSaveDialog.Create(nil) do
  begin
    Title := rsExportModule;
    Options:=Options + [ofOverwritePrompt, ofPathMustExist];
    Filter := Flt;
    if Execute then
    	Result := FileName;
    Free;
  end;
end;

{ TModulesFm }

procedure TModulesFm.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  UpdateMemo;
end;

procedure TModulesFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('exprmodules');
end;

procedure TModulesFm.FormCreate(Sender: TObject);
begin
  Caption := rsExpressionModules;
  Grid.Columns[0].Title.Caption := rsModuleName;
  Grid.Columns[1].Title.Caption := rsAuthor;
  Grid.Columns[2].Title.Caption := rsVersion;
  ToolButton1.Caption := rsImportModule;
  ToolButton2.Caption := rsDelete;
  ToolButton3.Caption := rsExportModule;
  ToolButton4.Caption := rsDownload;
  with ImageList1 do
  begin
    AddLazarusResource('add16');
    AddLazarusResource('delete16');
    AddLazarusResource('save16');
    AddLazarusResource('down16');
  end;
  ButtonPanel1.CloseButton.Caption:=rsClose;
  ButtonPanel1.HelpButton.Caption := rsHelp;
end;

procedure TModulesFm.ToolButton1Click(Sender: TObject);
var
  FileName, S, ModuleName: String;
  SD: TScriptData;
begin
  FileName := ImportModuleDlg;
  if FileName = '' then Exit;
  ModuleName := ChangeFileExt(ExtractFileName(FileName), '');
  if ScriptMan.FindScriptByName(ModuleName) <> nil then
  begin
    ErrMsgFmt(rsModuleAlreadyExists, [ModuleName]);
    Exit;
  end;
  with TFileStream.Create(FileName, fmOpenRead) do
  try
		SetLength(S, Size);
    Read(Pointer(S)^, Size);
  finally
    Free;
  end;
  SD := ScriptMan.AddScript(0, ModuleName, S);
  SD.Kind := skExpr;
  AddModule(SD);
  FModified := True;
  SetControlState;
  ScriptMan.CompileExpr;
  if ScriptMan.HasErrors then
    ShowCompilerErrors;
end;

procedure TModulesFm.ToolButton2Click(Sender: TObject);
var
  SD: TScriptData;
begin
  if MessageDlg(rsWarning, rsDeleteExprModuleConfirm, mtWarning,
  	[mbYes, mbNo], 0) = mrNo then Exit;
  SD := TScriptData(Grid.Objects[0, Grid.Row]);
  ScriptMan.DeleteScript(SD);
  Grid.DeleteRow(Grid.Row);
  SetControlState;
  FModified := True;
  ScriptMan.CompileExpr;
  if ScriptMan.HasErrors then
    ShowCompilerErrors;
end;

procedure TModulesFm.ToolButton3Click(Sender: TObject);
var
  FileName, S: String;
  SD: TScriptData;
begin
  FileName := ExportModuleDlg;
  if FileName = '' then Exit;
  SD := TScriptData(Grid.Objects[0, Grid.Row]);
  with TFileStream.Create(FileName, fmCreate) do
  try
    S := SD.Source;
    Write(Pointer(S)^, Length(S));
  finally
    Free;
  end;
end;

procedure TModulesFm.ToolButton4Click(Sender: TObject);
begin
  OpenUrl('http://forum.mydataexpress.ru/viewforum.php?f=16');
end;

procedure TModulesFm.AddModule(SD: TScriptData);
var
  r: Integer;
  //Author, Version, Description: String;
  P: TModuleParser;
begin
  P := TModuleParser.Create;
  r := Grid.RowCount;
  Grid.RowCount := r + 1;
  //P.ParseHeadModule(SD.Source, Author, Version, Description);
  try
  	P.Parse(SD.Source, True);
  except
    on E: EModuleParserError do ;
  end;
  Grid.Cells[0, r] := SD.Name;
  Grid.Cells[1, r] := P.Author;
  Grid.Cells[2, r] := P.Version;
  Grid.Cells[3, r] := P.Description;
  Grid.Objects[0, r] := SD;
  P.Free;
end;

procedure TModulesFm.FillModules;
var
  SL: TStringList;
  i: Integer;
  SD: TScriptData;
begin
  SL := TStringList.Create;
  ScriptMan.ModulesToList(SL, skExpr);
  SL.Sort;

  Grid.RowCount := 1;
  for i := 0 to SL.Count - 1 do
  begin
    SD := TScriptData(SL.Objects[i]);
    AddModule(SD);
  end;
  SL.Free;
end;

procedure TModulesFm.UpdateMemo;
begin
  if Grid.Row > 0 then
	  Memo1.Text := Grid.Cells[3, Grid.Row]
  else
    Memo1.Text := '';
end;

procedure TModulesFm.SetControlState;
begin
  ToolButton2.Enabled := Grid.Row > 0;
  ToolButton3.Enabled := Grid.Row > 0;
  UpdateMemo;
end;

procedure TModulesFm.ShowForm;
begin
  FModified := False;
  FillModules;
  if Grid.RowCount > 1 then Grid.Row := 1;
  SetControlState;
	if ScriptFm <> nil then ScriptFm.SaveAll;
  ShowModal;
  if FModified then
  begin
    if DesignFr = nil then
    begin
  		ScriptMan.SaveToDB;
      if not ScriptMan.HasErrors then
      begin
      	ExprModule.LoadBin;
  			ExprModule.BindVars;
    	end;
    end
    else if ScriptFm <> nil then
      DesignFr.ReCreateScriptForm;
  end;
end;

end.

