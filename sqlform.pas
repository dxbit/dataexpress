unit SqlForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterSQL, Forms, Controls,
  Graphics, Dialogs, DBGrids, ExtCtrls, ComCtrls, strconsts, db, SqlDb,
  dxsqlquery, SynEditTypes, LclType, StdCtrls, ButtonPanel, dxreports, LazUtf8,
  propgrids, dxctrls;

{ TSqlFm }

type
  TSqlFm = class(TForm)
    Buttons: TButtonPanel;
    DataSource1: TDataSource;
    Errors: TMemo;
    Grid: TDBGrid;
    ImageList1: TImageList;
    FieldImages: TImageList;
    SidePages: TPageControl;
    Panel2: TPanel;
    Panel3: TPanel;
    PropsPan: TPanel;
    Splitter1: TSplitter;
    SideSplitter: TSplitter;
    Splitter3: TSplitter;
    StatusBar1: TStatusBar;
    Edit: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    TabSheet1: TTabSheet;
    ToolBar1: TToolBar;
    ExecuteBn: TToolButton;
    CopyBn: TToolButton;
    PasteBn: TToolButton;
    FieldsTree: TTreeView;
    BuilderBn: TToolButton;
    procedure EditChange(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FieldsTreeSelectionChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridGetCellHint(Sender: TObject; Column: TColumn;
      var AText: String);
    procedure ExecuteBnClick(Sender: TObject);
    procedure CopyBnClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure PasteBnClick(Sender: TObject);
    procedure BuilderBnClick(Sender: TObject);
  private
    { private declarations }
    FModified: Boolean;
    FSqlFields: TSQLFieldList;
    FQry: TdxSQLQuery;
    FRD: TReportData;
    FQGrid: TdxQueryGrid;
    FCurrentForm: TdxForm;
    FProps: TVirtualPropGrid;
    procedure ShowGrid;
    procedure ShowErrors;
    function ExecuteSQLQuery: Boolean;
    procedure FieldGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure PropsChange(Sender: TObject; const PropName: String);
    procedure PropsEditing(Sender: TObject; const PropName: String;
      var Allowed: Boolean);
    procedure PropsNeedValues(Sender: TObject; const PropName: String;
      Values: TStrings);
    procedure SetColWidths;
    procedure FreeQry;
    procedure FillSqlFields;
    procedure FillFields;
    procedure FillProps;
    function IsSqlMode: Boolean;
    function GetSelectedSqlField: TSQLField;
    procedure UpdateSqlFieldNode(N: TTreeNode);
    procedure SetDisplayFormat;
    function CanOldFieldsDeleted: Boolean;
    function DetectRenameFields: Boolean;
    procedure ProcessRenameFields;
    function HasDuplicatesFields: Boolean;
    function IsValidFieldNames: Boolean;
    function Validate: Boolean;
    procedure SelectFieldInTree(SqlF: TSqlField);
    procedure SaveConfig;
  public
    { public declarations }
    function ShowForm: Integer;
    function ShowSqlModeForm(ARD: TReportData; AQGrid: TdxQueryGrid): Integer;
  end;

var
  SqlFm: TSqlFm;
  SqlModeFm: TSqlFm;

function ShowSqlForm: Integer;
//function ShowSqlQueryForm(RD: TReportData; AQGrid: TdxQueryGrid): Integer;

implementation

uses
  Clipbrd, expressions, apputils, helpmanager, appsettings;

function ShowSqlForm: Integer;
begin
  if SqlFm = nil then
  	SqlFm := TSqlFm.Create(Application);
  Result := SqlFm.ShowForm;
end;

{function ShowSqlQueryForm(RD: TReportData; AQGrid: TdxQueryGrid): Integer;
begin
  if SqlModeFm = nil then
  	SqlModeFm := TSqlFm.Create(Application);
  Result := SqlModeFm.ShowSqlModeForm(RD, AQGrid);
end; }

{$R *.lfm}

{ TSqlFm }

function MakeSqlFieldName(F: TSQLField): String;
begin
  if Utf8CompareText(F.Name, F.FieldNameDS) <> 0 then
    Result := F.Name + ' (' + F.FieldNameDS + ')'
  else
    Result := F.Name;
end;

function RpFieldTypeToImageIndex(Tp: TRpFieldType): Integer;
begin
  case Tp of
    flText: Result := 0;
    flNumber: Result := 1;
    flDate: Result := 2;
    flTime: Result := 3;
    flBool: Result := 4;
    else Result := -1;
  end;
end;

procedure TSqlFm.FormCreate(Sender: TObject);
begin
  FSqlFields := TSQLFieldList.Create;
  Caption := rsSQLEditor;
  ExecuteBn.Hint := rsExecuteSQL;
  CopyBn.Hint := rsCopyToPasteScript;
  PasteBn.Hint := rsPasteFromScript;
  BuilderBn.Hint := rsSwitchToBuilderMode;
  with ImageList1 do
	begin
    AddLazarusResource('play16');
    AddLazarusResource('copy16');
    AddLazarusResource('paste16');
    AddLazarusResource('query16');
  end;
  Buttons.OKButton.Caption := rsOk;
  Buttons.CancelButton.Caption := rsCancel;
  Buttons.HelpButton.Caption := rsHelp;
end;

procedure TSqlFm.FormDestroy(Sender: TObject);
begin
  FSqlFields.Free;
end;

procedure TSqlFm.FormShow(Sender: TObject);
begin
  ShowGrid;
  Edit.SetFocus;
end;

procedure TSqlFm.GridGetCellHint(Sender: TObject; Column: TColumn;
  var AText: String);
begin
  AText := StringReplace(AText, '|', '/', [rfReplaceAll]);
end;

procedure TSqlFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeQry;
  SaveConfig;
end;

procedure TSqlFm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if IsSqlMode then
  begin
    if ModalResult = mrOk then
      CanClose := Validate
    else if FModified then
      CanClose := Confirm(rsWarning, rsCancelChangesMsg) = mrYes;
  end;
end;

procedure TSqlFm.EditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  with StatusBar1 do
  begin
    Panels[0].Text := IntToStr(Edit.CaretY) + ': ' + IntToStr(Edit.CaretX);
    if Edit.InsertMode then
      Panels[1].Text := rsInserting
    else
      Panels[1].Text := rsReplacing;
  end;
end;

procedure TSqlFm.FieldsTreeSelectionChanged(Sender: TObject);
begin
  FillProps;
end;

procedure TSqlFm.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key = VK_F9 then
  begin
    Key := 0;
    ExecuteBn.Click;
  end
  else if Key = VK_ESCAPE then ModalResult := mrClose;
end;

procedure TSqlFm.EditChange(Sender: TObject);
begin
  FModified := True;
end;

procedure TSqlFm.ExecuteBnClick(Sender: TObject);
begin
  ExecuteSQLQuery;
end;

procedure TSqlFm.CopyBnClick(Sender: TObject);
var
  i: Integer;
  S: String;
begin
  S := 'SQL := ';
  for i := 0 to Edit.Lines.Count - 1 do
  begin
  	S := S + '''' + StringReplace(Edit.Lines[i], #39, #39#39, [rfReplaceAll]) + ' ''';
    if i < Edit.Lines.Count - 1 then
    	S := S + ' + ' + LineEnding + '  ';
  end;
  Clipboard.AsText := S + ';';
  MessageDlg(rsSQLEditor, rsSQLTextCopyToClip,	mtInformation, [mbOk], 0);
end;

procedure TSqlFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('sqleditor');
end;

function DeleteQuotes(const S: String): String;
var
  i, Len: Integer;
  Ch: String;
begin
  Result := '';
  i := 1; Len := Length(S);
  if Len = 0 then Exit;
  while i <= Len do
	begin
    if S[i] = #39 then
    begin
      if Copy(S, i + 1, 1) = #39 then
      begin
      	Result := Result + S[i];
        Inc(i);
      end;
    end
    else
    	Result := Result + S[i];
    Inc(i);
  end;
  Ch := Copy(Result, Length(Result), 1);
  if (Ch = '+') or (Ch = ';') then
  	Result := Copy(Result, 1, Length(Result) - 1);
end;

procedure TSqlFm.PasteBnClick(Sender: TObject);
var
  SL: TStringList;
  i: Integer;
  S: String;
begin
  if not Clipboard.HasFormat(CF_Text) then Exit;
  SL := TStringList.Create;
  SL.Text := Trim(Clipboard.AsText);
  for i := 0 to SL.Count - 1 do
  begin
    S := Trim(SL[i]);
    SL[i] := DeleteQuotes(S);
  end;
  Edit.SelText := SL.Text;
  SL.Free;
end;

procedure TSqlFm.BuilderBnClick(Sender: TObject);
begin
   if Confirm(rsWarning, rsSwitchBuilderModeMsg) <> mrYes then Exit;
  FRD.SqlMode := False;
  ModalResult := mrCancel;
end;

procedure TSqlFm.ShowGrid;
begin
  Grid.Visible := True;
  Errors.Visible := False;
end;

procedure TSqlFm.ShowErrors;
begin
  Grid.Visible := False;
  Errors.Visible := True;
end;

function TSqlFm.ExecuteSQLQuery: Boolean;
var
  i: Integer;
  F: TField;
begin
  Result := True;
  FreeQry;
  if Trim(Edit.Text) = '' then Exit;
  try
	  FQry := TdxSQLQuery.Create(Edit.Text, FCurrentForm, True);
  	DataSource1.DataSet := FQry.DataSet;
    FQry.Open;

    for i := 0 to FQry.DataSet.Fields.Count - 1 do
    begin
      F := FQry.DataSet.Fields[i];
      if F.IsBlob then F.OnGetText:=@FieldGetText;
    end;

    SetColWidths;
    ShowGrid;

    if IsSqlMode then
    begin
      FillSqlFields;
      FillFields;
      SetDisplayFormat;
      Edit.Modified := False;
    end;
  except
    on E: Exception do
    begin
      ShowErrors;
    	Errors.Text := ExceptionToString(E, False, False);
      if FQry <> nil then
      	Errors.Lines.AddStrings(['', '', FQry.DataSet.SQL.Text]);
      Result := False;
    end;
  end;
end;

procedure TSqlFm.SetColWidths;
var
  i: Integer;
begin
  for i := 0 to Grid.Columns.Count - 1 do
    Grid.Columns[i].Width := 100;
end;

procedure TSqlFm.FieldGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  aText := Sender.AsString;
end;

procedure TSqlFm.PropsChange(Sender: TObject; const PropName: String);
begin
  if (PropName = 'Name') or (PropName = 'Tp') then
    UpdateSqlFieldNode(FieldsTree.Selected);
  FModified := True;
end;

procedure TSqlFm.PropsEditing(Sender: TObject; const PropName: String;
  var Allowed: Boolean);
begin
  if PropName = 'FieldNameDS' then Allowed := False;
end;

procedure TSqlFm.PropsNeedValues(Sender: TObject; const PropName: String;
  Values: TStrings);
begin
  if PropName = 'Tp' then
    Values.AddStrings([
      IntToStr(Ord(flText)) + '=' + rsText,
      IntToStr(Ord(flNumber)) + '=' + rsNumber,
      IntToStr(Ord(flDate)) + '=' + rsDate,
      IntToStr(Ord(flTime)) + '=' + rsTime,
      IntToStr(Ord(flBool)) + '=' + rsCheckBox]);
end;

procedure TSqlFm.FreeQry;
begin
  if FQry <> nil then
  begin
    FQry.Close;
    FreeAndNil(FQry);
  end;
end;

function FieldTypeToRpFieldType(F: TField): TRpFieldType;
begin
  if F is TNumericField then
    Result := flNumber
  else if F is TDateField then
    Result := flDate
  else if F is TTimeField then
    Result := flTime
  else
    Result := flText;
end;

procedure TSqlFm.FillSqlFields;
var
  i: Integer;
  F: TField;
  TmpFields: TSQLFieldList;
  SqlF, TmpF: TSQLField;
begin
  TmpFields := TSqlFieldList.Create;
  TmpFields.CopyFrom(FSqlFields);
  FSqlFields.Clear;
  for i := 0 to FQry.FieldCount - 1 do
  begin
    F := FQry.Field[i];
    SqlF := FSqlFields.AddField;
    TmpF := TmpFields.FindFieldDS(F.FieldName);
    if TmpF <> nil then
      SqlF.CopyFrom(TmpF)
    else
    begin
      SqlF.Name := F.FieldName;
      SqlF.FieldNameDS := SqlF.Name;
      SqlF.Tp := FieldTypeToRpFieldType(F);
    end;
  end;
  TmpFields.Free;
end;

procedure TSqlFm.FillFields;
var
  i: Integer;
  SqlF: TSQLField;
begin
  FieldsTree.Items.Clear;
  for i := 0 to FSqlFields.Count - 1 do
  begin
    SqlF := FSqlFields[i];
    UpdateSqlFieldNode( FieldsTree.Items.AddChildObject(nil, '', SqlF) );
  end;
end;

procedure TSqlFm.FillProps;
var
  SqlF: TSQLField;
begin
  FProps.Clear;
  SqlF := GetSelectedSqlField;
  if SqlF = nil then Exit;
  FProps.TIObject := SqlF;
  FProps.AddProp(rsFieldName, 'Name', nil);
  FProps.AddProp(rsSQLName, 'FieldNameDS', nil);
  FProps.AddPropList(rsFieldType, 'Tp', nil, nil);
  FProps.AddProp(rsFormat, 'DisplayFormat', nil);
end;

function TSqlFm.IsSqlMode: Boolean;
begin
  Result := FRD <> nil;
end;

function TSqlFm.GetSelectedSqlField: TSQLField;
begin
  if FieldsTree.Selected <> nil then
    Result := TSQLField(FieldsTree.Selected.Data)
  else
    Result := nil;
end;

procedure TSqlFm.UpdateSqlFieldNode(N: TTreeNode);
var
  SqlF: TSQLField;
begin
  SqlF := TSQLField(N.Data);
  N.Text := MakeSqlFieldName(SqlF);
  N.ImageIndex := RpFieldTypeToImageIndex(SqlF.Tp);
  N.SelectedIndex := N.ImageIndex;
end;

procedure TSqlFm.SetDisplayFormat;
var
  i: Integer;
  SqlF: TSQLField;
begin
  for i := 0 to FSqlFields.Count - 1 do
  begin
    SqlF := FSqlFields[i];
    Grid.Columns[i].DisplayFormat := SqlF.DisplayFormat;
  end;
end;

function TSqlFm.CanOldFieldsDeleted: Boolean;
var
  i: Integer;
  OldSqlF, SqlF: TSQLField;
  pF: PRpField;
begin
  Result := True;

  if FRD.Sources.Count > 0 then
    for i := 0 to FRD.Sources[0]^.Fields.Count - 1 do
    begin
      pF := FRD.Sources[0]^.Fields[i];
      SqlF := FSqlFields.FindByName(pF^.Name);
      if (SqlF = nil) and CheckExistsInActions(FRD, renRpField, pF^.Name) then Exit(False);
    end;
  for i := 0 to FRD.SqlFields.Count - 1 do
  begin
    OldSqlF := FRD.SqlFields[i];
    SqlF := FSqlFields.FindFieldDS(OldSqlF.FieldNameDS);
    if (SqlF = nil) and CheckExistsInActions(FRD, renRpField, OldSqlF.Name) then Exit(False);
  end;
end;

function TSqlFm.DetectRenameFields: Boolean;
var
  i: Integer;
  OldSqlF, SqlF: TSQLField;
begin
  Result := False;
  for i := 0 to FRD.SqlFields.Count - 1 do
  begin
    OldSqlF := FRD.SqlFields[i];
    SqlF := FSqlFields.FindFieldDS(OldSqlF.FieldNameDS);
    if (SqlF <> nil) and (SqlF.Name <> OldSqlF.Name) and CheckExistsInActions(FRD,
      renRpField, OldSqlF.Name, LineEnding + rsCantRenameRpFieldMsg) then Exit(True);
  end;
end;

{function TSqlFm.CanOldFieldsDeleted: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FRD.GetRpSQLFieldCount - 1 do
  begin
    if FSqlFields.FindFieldDS(FRD.GetFieldNameDS(i)) = nil then
      if CheckExistsInActions(FRD, renRpField, FRD.GetFieldName(i)) then Exit(False);
  end;
end;  }

procedure TSqlFm.ProcessRenameFields;
var
  i: Integer;
  OldSqlF, SqlF: TSQLField;
begin
  for i := 0 to FRD.SqlFields.Count - 1 do
  begin
    OldSqlF := FRD.SqlFields[i];
    SqlF := FSqlFields.FindFieldDS(OldSqlF.FieldNameDS);
    if (SqlF <> nil) and (SqlF.Name <> OldSqlF.Name) then
      RenameInActions(FRD, renRpField, OldSqlF.Name, SqlF.Name);
  end;
end;

{procedure TSqlFm.ProcessRenameFields;
var
  i: Integer;
  SqlF: TSQLField;
begin
  for i := 0 to FRD.GetRpSQLFieldCount - 1 do
  begin
    SqlF := FSqlFields.FindFieldDS(FRD.GetFieldNameDS(i));
    if (SqlF <> nil) and (SqlF.Name <> FRD.GetFieldName(i)) then
      RenameInActions(FRD, renRpField, FRD.GetFieldName(i), SqlF.Name);
  end;
end; }

function TSqlFm.HasDuplicatesFields: Boolean;
var
  i, j: Integer;
  SqlF, SqlF2: TSQLField;
begin
  Result := False;
  for i := 0 to FSqlFields.Count - 1 do
    for j := 0 to FSqlFields.Count - 1 do
    begin
      if i = j then Continue;
      SqlF := FSqlFields[i];
      SqlF2 := FSqlFields[j];
      if Utf8CompareText(SqlF.Name, SqlF2.Name) = 0 then
      begin
        SelectFieldInTree(SqlF);
        ErrMsg(rsDuplicateFieldName);
        Exit(True);
      end
      else if CompareText(SqlF.FieldNameDS, SqlF2.FieldNameDS) = 0 then
      begin
        SelectFieldInTree(SqlF);
        ErrMsg(rsDuplicateSqlFieldName);
        Exit(True);
      end;
    end;

  for i := 0 to FSqlFields.Count - 1 do
  begin
    SqlF := FSqlFields[i];
    if FRD.CalcFields.FindFieldByName(SqlF.Name) <> nil then
    begin
      SelectFieldInTree(SqlF);
      ErrMsg(rsCalcFieldNameExists);
      Exit(True);
    end
    else if FRD.CalcFields.FindFieldByNameDS(SqlF.FieldNameDS) <> nil then
    begin
      SelectFieldInTree(SqlF);
      ErrMsg(rsCalcFieldSqlNameExists);
      Exit(True);
    end;
  end;
end;

function TSqlFm.IsValidFieldNames: Boolean;
var
  i: Integer;
  SqlF: TSQLField;
begin
  Result := True;
  for i := 0 to FSqlFields.Count - 1 do
  begin
    SqlF := FSqlFields[i];
    if not CheckFieldName(SqlF.Name) then
    begin
      SelectFieldInTree(SqlF);
      Exit(False);
    end;
  end;
end;

function TSqlFm.Validate: Boolean;
begin
  Result := False;
  if Edit.Modified and not ExecuteSQLQuery then Exit;
  if not IsValidFieldNames then Exit;
  if not CanOldFieldsDeleted then Exit;
  if HasDuplicatesFields then Exit;
  try
    if FQGrid <> nil then LoopDetect(Edit.Text, FQGrid, FCurrentForm, FRD, True);
  except
    on E: ELoopException do
    begin
      ErrMsg(E.Message);
      Exit;
    end;
  end;
  if not IsDesignerMode and DetectRenameFields then Exit;

  Result := True;
end;

procedure TSqlFm.SelectFieldInTree(SqlF: TSqlField);
var
  N: TTreeNode;
begin
  N := FieldsTree.Items.FindNodeWithData(SqlF);
  if N <> nil then N.Selected := True;
end;

procedure TSqlFm.SaveConfig;
var
  R: TRect;
begin
  R := ScaleRectTo96(GetFormRealBounds(Self));
  if IsSqlMode then
  begin
    AppConfig.SQLModeEditorWidth := R.Width;
    AppConfig.SQLModeEditorHeight := R.Height;
    AppConfig.SQLModeEditorRightPanelWidth := ScaleTo96(SidePages.Width);
  end
  else
  begin
    AppConfig.SQLEditorWidth := R.Width;
    AppConfig.SQLEditorHeight := R.Height;
  end;
end;

function TSqlFm.ShowForm: Integer;
begin
  SideSplitter.Visible := False;
  SidePages.Visible := False;
  BuilderBn.Visible := False;
  Buttons.Visible := False;

  Width := ScaleToScreen(AppConfig.SQLEditorWidth);
  Height := ScaleToScreen(AppConfig.SQLEditorHeight);

  Result := ShowModal;
end;

function TSqlFm.ShowSqlModeForm(ARD: TReportData; AQGrid: TdxQueryGrid): Integer;
begin
  FRD := ARD;
  FQGrid := AQGrid;
  if FQGrid <> nil then
    FCurrentForm := TdxForm(FQGrid.Owner)
  else
    FCurrentForm := nil;
  if FProps = nil then
  begin
    FProps := TVirtualPropGrid.Create(Self);
    FProps.Parent := PropsPan;
    FProps.Align := alClient;
    FProps.Header.AutoSizeIndex:=-1;
    FProps.Header.Columns[0].Width := 100;
    FProps.OnNeedValues := @PropsNeedValues;
    FProps.OnPropChange := @PropsChange;
    FProps.OnPropEditing := @PropsEditing;

    FieldImages.AddLazarusResource('text16');
    FieldImages.AddLazarusResource('calc16');
    FieldImages.AddLazarusResource('date16');
    FieldImages.AddLazarusResource('clock16');
    FieldImages.AddLazarusResource('checkbox16');
  end
  else
  begin
    FProps.Clear;
    FProps.TIObject := nil;
  end;
  FSqlFields.CopyFrom(FRD.SQLFields);
  Edit.Text := FRD.SQL;
  FillFields;

  FModified := False;
  Width := ScaleToScreen(AppConfig.SQLModeEditorWidth);
  Height := ScaleToScreen(AppConfig.SQLModeEditorHeight);
  SidePages.Width := ScaleToScreen(AppConfig.SQLModeEditorRightPanelWidth);

  Result := ShowModal;

  if Result = mrOk then
  begin
    FRD.Sources.Clear;

    ProcessRenameFields;
    FRD.SQLFields.CopyFrom(FSqlFields);
    FRD.SQL := Edit.Text;
    RemoveLostFieldsFromReportData(FRD);
    CreateOrUpdateReportGridColumns(FRD);
    // Обновляем поля сводок
    if FCurrentForm <> nil then
	    UpdatePivotFieldCaptions(FCurrentForm, FRD);
  end;
end;

end.

