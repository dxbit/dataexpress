unit ExtensionsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, StdCtrls, ButtonPanel, scriptmanager, strconsts;

{ TExtensionsFm }

type
  TExtensionsFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TStringGrid;
    ImageList1: TImageList;
    Memo1: TMemo;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
    { private declarations }
    FModified: Boolean;
    procedure AddModule(SD: TScriptData);
    procedure FillModules;
    procedure UpdateMemo;
    procedure SetControlState;
    procedure UpdateExprModule;
  public
    { public declarations }
    procedure ShowForm;
  end;

var
  ExtensionsFm: TExtensionsFm;

implementation

uses
  apputils, designerframe, mainframe, mydialogs, appsettings, scriptform;

{$R *.lfm}

function AddModuleDlg: String;
var
  Flt: String;
begin
  Result := '';
  Flt := rsExprModulesFlt +	rsAllFilesFlt;
  with TOpenDialog.Create(nil) do
  begin
    Title := rsAddModule;
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

{ TExtensionsFm }

procedure TExtensionsFm.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  UpdateMemo;
end;

procedure TExtensionsFm.FormCreate(Sender: TObject);
begin
  Caption := rsExtensions;
  ToolButton1.Caption := rsAddModule;
  ToolButton2.Caption := rsDelete;
  ToolButton3.Caption := rsExportModule;
  with ImageList1 do
  begin
    AddLazarusResource('add16');
    AddLazarusResource('delete16');
    AddLazarusResource('save16');
  end;
end;

procedure TExtensionsFm.ToolButton1Click(Sender: TObject);
var
  FileName, S, ModuleName: String;
  SD: TScriptData;
begin
  FileName := AddModuleDlg;
  if FileName = '' then Exit;
  ModuleName := ExtractFileNameOnly(FileName);
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
  ScriptMan.CompileExpr;
  if not ScriptMan.HasErrors then
  	UpdateExprModule
  else
    ShowCompilerErrors;
  SetControlState;
end;

procedure TExtensionsFm.ToolButton2Click(Sender: TObject);
var
  SD: TScriptData;
begin
  if MessageDlg(rsWarning, rsDeleteExprModuleConfirm, mtWarning,
  	[mbYes, mbNo], 0) = mrNo then Exit;
  SD := TScriptData(Grid.Objects[0, Grid.Row]);
  ScriptMan.DeleteScript(SD);
  Grid.DeleteRow(Grid.Row);
  SetControlState;
  UpdateExprModule;
  FModified := True;
end;

procedure TExtensionsFm.ToolButton3Click(Sender: TObject);
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

procedure TExtensionsFm.AddModule(SD: TScriptData);
var
  r: Integer;
  Author, Version, Description: String;
  P: TModuleParser;
begin
  P := TModuleParser.Create;
  r := Grid.RowCount;
  Grid.RowCount := r + 1;
  P.ParseHeadModule(SD.Source, Author, Version, Description);
  Grid.Cells[0, r] := SD.Name;
  Grid.Cells[1, r] := Author;
  Grid.Cells[2, r] := Version;
  Grid.Cells[3, r] := Description;
  Grid.Objects[0, r] := SD;
  P.Free;
end;

procedure TExtensionsFm.FillModules;
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

procedure TExtensionsFm.UpdateMemo;
begin
  if Grid.Row > 1 then
	  Memo1.Text := Grid.Cells[3, Grid.Row]
  else
    Memo1.Text := '';
end;

procedure TExtensionsFm.SetControlState;
begin
  ToolButton2.Enabled := Grid.Row > 1;
  ToolButton3.Enabled := Grid.Row > 1;
end;

procedure TExtensionsFm.UpdateExprModule;
begin
  if MainFr <> nil then
  begin
	  ExprModule.LoadBin(ScriptMan.ExprModule.Bin);
  	ExprModule.BindVars;
  end;
  if (DesignFr <> nil) and AppConfig.ExpertMode then
  begin
    ScriptFm.SaveAll;
    DesignFr.ReCreateScriptForm;
  end;
end;

procedure TExtensionsFm.ShowForm;
begin
  FModified := False;
  FillModules;
  if Grid.RowCount > 1 then Grid.Row := 1;
  UpdateMemo;
  ShowModal;
  if FModified and (DesignFr = nil) then
  	ScriptMan.SaveToDB;
end;

end.

