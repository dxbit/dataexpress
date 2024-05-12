unit ModulesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Forms, Controls, Graphics,
  Dialogs, ComCtrls, Grids, StdCtrls, ButtonPanel, scriptmanager, strconsts,
  LclType, ExtCtrls, IpMsg, Types;

{ TModulesFm }

type
  TModulesFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Grid: TStringGrid;
    ImageList1: TImageList;
    HtmlPan: TIpHtmlPanel;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure HelpButtonClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
  private
    { private declarations }
    FModified: Boolean;
    FExtBmp, FWebExtBmp: TBitmap;
    procedure FillModules(FocusModule: TScriptData);
    procedure UpdateHelp;
    procedure SetControlState;
  public
    { public declarations }
    procedure ShowForm;
  end;

var
  ModulesFm: TModulesFm;

procedure ShowModulesForm;

implementation

uses
  apputils, designerframe, helpmanager, mydialogs, scriptform, dbengine,
  myctrls, dxmains, findactionsform;

{$R *.lfm}

function ImportModuleDlg: String;
var
  Flt: String;
begin
  Result := '';
  Flt := rsAllExtModulesFlt + rsExtModulesFlt + rsWebExtModulesFlt + rsAllFilesFlt;
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

function ExportModuleDlg(const ModName: String): String;
var
  Flt: String;
begin
  Flt := rsAllExtModulesFlt + rsExtModulesFlt + rsWebExtModulesFlt + rsAllFilesFlt;
  with TSaveDialog.Create(nil) do
  begin
    Title := rsExportModule;
    Options := Options + [ofOverwritePrompt, ofPathMustExist];
    FileName := ModName;
    Filter := Flt;
    if Execute then
    	Result := FileName;
    Free;
  end;
end;

procedure ShowModulesForm;
begin
  if ModulesFm = nil then
    ModulesFm := TModulesFm.Create(Application);
  ModulesFm.ShowForm;
end;

{ TModulesFm }

procedure TModulesFm.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  UpdateHelp;
  SetControlState;
end;

procedure TModulesFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('exprmodules');
end;

procedure TModulesFm.FormCreate(Sender: TObject);
begin
  Caption := rsExtensions;
  Grid.Columns[0].Title.Caption := rsModuleName;
  Grid.Columns[1].Title.Caption := rsAuthor;
  Grid.Columns[2].Title.Caption := rsVersion;
  Grid.FocusRectVisible:=False;
  ToolButton1.Caption := rsImportModule;
  ToolButton2.Caption := rsDelete;
  ToolButton3.Caption := rsExportModule;
  ToolButton4.Caption := rsDownload;
  ToolButton6.Caption := rsHomepage;
  with ImageList1 do
  begin
    AddLazarusResource('add16');
    AddLazarusResource('delete16');
    AddLazarusResource('save16');
    AddLazarusResource('down16');
    AddLazarusResource('home16');
  end;
  ButtonPanel1.CloseButton.Caption := rsClose;
  ButtonPanel1.HelpButton.Caption := rsHelp;
  HtmlPan.DataProvider := THtmlProvider.Create(Self);
  HtmlPan.BorderSpacing.Left := 4;
  HtmlPan.BorderSpacing.Right := 4;
  FExtBmp := LoadBitmapFromLazarusResource('ext16');
  FWebExtBmp := LoadBitmapFromLazarusResource('webext16');
end;

procedure TModulesFm.FormDestroy(Sender: TObject);
begin
  FExtBmp.Free;
  FWebExtBmp.Free;
end;

procedure TModulesFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrClose;
end;

procedure TModulesFm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  SD: TScriptData;
  Bmp: TBitmap;
begin
  if (aCol = 3) and (aRow > 0) then
  begin
    SD := TScriptData(Grid.Objects[0, aRow]);
    if SD.Kind = skExpr then Bmp := FExtBmp
    else Bmp := FWebExtBmp;
    Grid.Canvas.Draw(aRect.Left + aRect.Width div 2 - Bmp.Width div 2,
      aRect.Top + aRect.Height div 2 - Bmp.Height div 2, Bmp);
  end;
end;

procedure TModulesFm.ToolButton1Click(Sender: TObject);
var
  FileName: String;
  SD: TScriptData;
begin
  FileName := ImportModuleDlg;
  if FileName = '' then Exit;
  try
    if not ScriptMan.AddExtModule(FileName, SD) then Exit;
    FModified := True;
    FillModules(SD);
    SetControlState;
    if ScriptMan.HasErrors then
      ShowCompilerErrors;
  except
    on E: Exception do
      ErrMsg(rsErrorWhenImportModule + LineEnding + LineEnding + E.Message);
  end;
end;

procedure TModulesFm.ToolButton2Click(Sender: TObject);
var
  SD: TScriptData;
  Msg: String;
begin
  Msg := rsDeleteExtModuleConfirm;
  if DesignFr = nil then Msg := Msg + Spaces + rsDeleteExtModuleNote;
  if MessageDlg(rsWarning, Msg, mtWarning,
  	[mbYes, mbNo], 0) <> mrYes then Exit;
  SD := TScriptData(Grid.Objects[0, Grid.Row]);
  ScriptMan.DeleteScript(SD);
  if FindActionsFm <> nil then FindActionsFm.DeleteModule(SD);
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
  SD := TScriptData(Grid.Objects[0, Grid.Row]);
  FileName := Grid.Cells[0, Grid.Row];
  if SD.Kind = skExpr then FileName := FileName + '.epas'
  else FileName := FileName + '.wepas';
  FileName := ExportModuleDlg(FileName);
  if FileName = '' then Exit;
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

procedure TModulesFm.ToolButton6Click(Sender: TObject);
begin
  with Grid do
    OpenUrl(TScriptData(Objects[0, Row]).HomePage);
end;

procedure TModulesFm.FillModules(FocusModule: TScriptData);
var
  SL: TStringList;
  i, r, FocusRow: Integer;
  SD: TScriptData;
begin
  SL := TStringList.Create;
  ScriptMan.ModulesToList(SL, skExpr);
  ScriptMan.ModulesToList(SL, skWebExpr, False);
  SL.Sort;

  FocusRow := 0;
  Grid.RowCount := 1;
  Grid.RowCount := SL.Count + 1;
  for i := 0 to SL.Count - 1 do
  begin
    r := i + 1;
    SD := TScriptData(SL.Objects[i]);
    Grid.Cells[0, r] := SD.Name;
    Grid.Cells[1, r] := SD.Author;
    Grid.Cells[2, r] := SD.Version;
    Grid.Objects[0, r] := SD;
    if SD = FocusModule then FocusRow := r;
  end;
  if FocusRow > 0 then
    Grid.Row := FocusRow;
  SL.Free;
end;

procedure TModulesFm.UpdateHelp;
begin
  with Grid do
    if Row > 0 then
      HtmlPan.SetHtmlFromStr(ToHtml(TScriptData(Objects[0, Row]).Description))
    else
      HtmlPan.SetHtmlFromStr(ToHtml(''));
  {if Grid.Row > 0 then
	  Memo1.Text := Grid.Cells[3, Grid.Row]
  else
    Memo1.Text := ''; }
end;

procedure TModulesFm.SetControlState;
begin
  ToolButton2.Enabled := Grid.Row > 0;
  ToolButton3.Enabled := Grid.Row > 0;
  ToolButton6.Enabled := (Grid.Row > 0) and (TScriptData(Grid.Objects[0, Grid.Row]).HomePage <> '');
  UpdateHelp;
end;

procedure TModulesFm.ShowForm;
begin
  if not DXMain.CanProjectChange then Exit;
  FModified := False;
  FillModules(nil);
  if Grid.RowCount > 1 then Grid.Row := 1;
  SetControlState;
	if ScriptFm <> nil then ScriptFm.SaveAll;
  ShowModal;
  if FModified then
  begin
    if (DesignFr = nil) and DXMain.CanProjectSave then
      try
  		  ScriptMan.SaveToDB;
        DXMain.SetLastModified;
        DBase.Commit;
      except
        on E: Exception do
          ErrMsg(rsSaveModulesError + ExceptionToString(E, True, False));
      end
    else if ScriptFm <> nil then
      ScriptFm.Reset(True);
//      ReCreateScriptForm;
  end;
end;

end.

