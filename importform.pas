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

unit ImportForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ButtonPanel, ExtCtrls, strconsts, dxctrls, Db, SqlDb;

type

  EImportError = class(Exception);

  { TImportFm }

  TImportFm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    FileNameEdit1: TFileNameEdit;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ShowForm(Fm: TdxForm): Integer;
  end;

var
  ImportFm: TImportFm;

function ShowImportForm(Fm: TdxForm): Integer;

implementation

uses
  apputils, sqlgen, dbengine, importerrorsform, processform, dximages, dxfiles,
  helpmanager, mainform, formmanager, DateUtils, LazUtf8, Math, processminiform,
  csvfiles;

function ShowImportForm(Fm: TdxForm): Integer;
begin
  if ImportFm = nil then
  	ImportFm := TImportFm.Create(Application);
  Result := ImportFm.ShowForm(Fm);
end;

{$R *.lfm}

{ TImportFm }

procedure TImportFm.FormCreate(Sender: TObject);
begin
  Caption := rsImportData;
  Label2.Caption := rsFilename;
  FileNameEdit1.DialogTitle:=rsImportData;
  FileNameEdit1.Filter := rsImportExportDataFilter;
  FileNameEdit1.DefaultExt:='csv';
  FileNameEdit1.DialogOptions:=FileNameEdit1.DialogOptions + [ofFileMustExist];
  RadioGroup1.Caption := rsEncoding;
  CheckBox1.Caption := rsCheckUnique;
  ButtonPanel1.OKButton.Caption := rsOk;
  ButtonPanel1.CancelButton.Caption:=rsCancel;
  ButtonPanel1.HelpButton.Caption:=rsHelp;
end;

procedure TImportFm.FormShow(Sender: TObject);
begin
  FileNameEdit1.SetFocus;
end;

procedure TImportFm.HelpButtonClick(Sender: TObject);
begin
  OpenHelp('import');
end;

procedure TImportFm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult <> mrOk then Exit;
  CanClose := False;
  if FileNameEdit1.Text = '' then
    ErrMsg(rsFileNameEmpty)
  else
    CanClose := True;
end;

function GetLookupId(L: TdxLookupComboBox; Value: String): Integer;
var
  TNm, FNm: String;
  Fm: TdxForm;
  C: TComponent;
begin
  Result := 0;
  if Value = '' then Exit;
  TNm := TableStr(L.SourceTId);
  FNm := FieldStr(L.SourceFId);
  Fm := FormMan.FindForm(L.SourceTId);
  C := FindById(Fm, L.SourceFId);
  if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) then
  	Value := '''' + Value + '''';
  with DBase.OpenDataSet('select id from ' + TNm + ' where ' + FNm +
    '=' + Value) do
  try
    if RecordCount > 0 then
      Result := Fields[0].AsInteger;
  finally
    Free;
  end;
end;

function GetTreeLookupId(SrcFm: TdxForm; L: TdxLookupComboBox;
  const Value: String; var SubPath: String): Integer;
var
  SL: TStringList;
  i, j, Id: Integer;
  TNm, FNm, PFNm, S, SQL: String;
begin
  Result := 0; Id := 0; SubPath := '';
  PFNm := FieldStr(SrcFm.ParentField);
  SL := TStringList.Create;
  try

  SplitStr(Value, '\', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    TNm := TableStr(L.SourceTId);
    FNm := FieldStr(L.SourceFId);
    SQL := 'select id from ' + TNm + ' where ' + FNm +
      '=''' + S + '''';
    if Id > 0 then
      SQL := SQL + ' and ' + PFNm + '=' + IntToStr(Id);
    with DBase.OpenDataSet(SQL) do
    try
      if RecordCount > 0 then
        Id := Fields[0].AsInteger
      else Id := 0;
    finally
      Free;
    end;
    if Id = 0 then
    begin
      SubPath := '';
      for j := i to SL.Count - 1 do
      begin
        SubPath := SubPath + SL[j];
        if j < SL.Count - 1 then SubPath := SubPath + '\';
      end;
      Break;
    end
    else Result := Id;
  end;

  finally
    SL.Free;
  end;
end;

function GetLookupCbxId(L: TdxLookupComboBox; const Value: String): Integer;
var
  Fm: TdxForm;
  Tmp: String;
begin
  Result := 0;
  Fm := FormMan.FindForm(L.SourceTId);
  if Fm = nil then Exit;

  if (Fm.ParentField = 0) or (GetFormParentFieldFieldId(Fm) <> L.SourceFId) then
    Result := GetLookupId(L, Value)
  else
  begin
    Result := GetTreeLookupId(Fm, L, Value, Tmp);
    if Tmp <> '' then Result := 0;
  end;
end;

procedure ImportLookup(L: TdxLookupComboBox; F: TField; const Value: String);
var
  Id: Integer;
  TNm: String;
begin
  Id := GetLookupId(L, Value);
  if Id = 0 then
  begin
    TNm := TableStr(L.SourceTId);
    Id := DBase.GenId('gen_' + TNm);
    DBase.Execute('insert into ' + TNm + ' (id,' + FieldStr(L.SourceFId) +
      ') values (' + IntToStr(Id) + ',''' + Value + ''');');
  end;
  F.DataSet.FieldByName(F.FieldName + 'l').AsString := Value;
  F.AsInteger := Id;
end;

procedure ImportTreeLookup(SrcFm: TdxForm; L: TdxLookupComboBox; F: TField; const Value: String);
var
  PFNm, SubPath, TNm, FNm, SQL, S: String;
  SL: TStringList;
  i, Id, PId: Integer;
begin
  PId := GetTreeLookupId(SrcFm, L, Value, SubPath);

  if SubPath > '' then
  begin
    PFNm := FieldStr(SrcFm.ParentField);
    SL := TStringList.Create;
    SplitStr(SubPath, '\', SL);
    for i := 0 to SL.Count - 1 do
    begin
      S := SL[i];
      TNm := TableStr(L.SourceTId);
      FNm := FieldStr(L.SourceFId);
      Id := DBase.GenId('gen_' + TNm);
      SQL := 'insert into ' + TNm + ' (id,' + FNm + ',' + PFNm +
        ') values (' + IntToStr(Id) + ',''' + S + ''', ';
      if PId = 0 then SQL := SQL + 'null);'
      else SQL := SQL + IntToStr(PId) + ');';
      DBase.Execute(SQL);
      PId := Id;
    end;
  end;

  F.DataSet.FieldByName(F.FieldName + 'l').AsString := Value;
  F.AsInteger := PId;
end;

procedure ImportLookupCbx(L: TdxLookupComboBox; F: TField; const Value: String);
var
  Fm: TdxForm;
begin
  Fm := FormMan.FindForm(L.SourceTId);
  if Fm = nil then Exit;

  if (Fm.ParentField = 0) or (GetFormParentFieldFieldId(Fm) <> L.SourceFId) then ImportLookup(L, F, Value)
  else ImportTreeLookup(Fm, L, F, Value);
end;

function CheckUnique(Fm: TdxForm; Cmps, Fields: TList; Csv: TCsvData; Row: Integer): Boolean;
var
  S, V: String;
  i, id: Integer;
  F: TField;
  C: TComponent;
  T, DT: TDateTime;
begin
  Result := True;
  S := '';
  for i := 0 to Csv.ColCount - 1 do
  begin
    if Fields[i] = nil then Continue;
    F := TField(Fields[i]);
    C := TComponent(Cmps[i]);
    V := Csv[i, Row];
    if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) then
    begin
      if V = '' then S := S + F.fieldName + ' is null and '
      else
      begin
        // экранируем апострофы
        V := StringReplace(V, '''', '''' + '''', [rfReplaceAll]);
        S := S + 'lower(' + F.FieldName + ')=lower(''' + V + ''') and ';
      end;
      //S := S + 'lower(coalesce(' + F.FieldName + ',''''))=lower(''' + V + ''') and '
    end
    else if C is TdxDateEdit then
    begin
      if V = '' then S := S + F.FieldName + ' is null and '
      else
      begin
        DT := StrToDate(V);
        V := Date2Str(DT);
        S := S + F.FieldName + '=''' + V + ''' and ';
      end;
    end
    else if C is TdxTimeEdit then
    begin
      if V = '' then S := S + F.FieldName + ' is null and '
      else
      begin
        T := StrToTime(V);
        T := IncSecond(T);
        S := S + F.FieldName + '>=''' + V + ''' and ' +
        F.FieldName + '<''' + TimeToStr(T) + ''' and ';
      end;
    end
    else if (C is TdxCheckBox) or (C is TdxCounter) then
    begin
      if V = '' then S := S + F.FieldName + ' is null and '
      else S := S + F.FieldName + '=' + V + ' and ';
    end
    else if C is TdxCalcEdit then
    begin
      if V = '' then S := S + F.FieldName + ' is null and '
      else
      begin
        V := StringReplace(V, ',', '.', []);
        S := S + F.FieldName + '>=' + V + '-' + V + '*2e-12 and ' +
        	F.FieldName + '<=' + V + '+' + V + '*2e-12 and ';
      end;
    end
    else if C is TdxLookupComboBox then
    begin
      id := GetLookupCbxId(TdxLookupComboBox(C), V);
      S := S + F.FieldName;
      if id > 0 then
        S := S + '=' + IntToStr(id)
      else S := S + ' is null ';
      S := S + ' and ';
    end
  end;
  if S = '' then Exit;
  S := Copy(S, 1, Length(S) - 5);
  S := 'select id from ' + TableStr(Fm.Id) + ' where ' + S;
  //DebugStr(S);
  //Debug(S);
  with DBase.OpenDataSet(S) do
  try
    Result := Fields[0].IsNull;
    //if not Result then ShowMessage(S);
  finally
    Free;
  end;
end;

function CheckData(Csv: TCsvData; Errors: TStringList; Cmps: TList; Row: Integer): Boolean;
var
  i, N, Prec, FieldSize, TextLen, ErrCount: Integer;
  E: Extended;
  D: TDatetime;
  C: TComponent;
  RowStr, FlNm, S: String;
  Fm: TdxForm;
begin
  Result := False;
  ErrCount := Errors.Count;
  for i := 0 to Csv.ColCount - 1 do
  begin
    S := Csv[i, Row];
    C := TComponent(Cmps[i]);
    if (C = nil) or (S = '') then Continue;

    if C is TdxLookupComboBox then
    begin
      FlNm := GetFieldName(C);
      Fm := FormMan.FindForm(GetSourceTId(C));
      C := FindById(Fm, GetSourceFId(C));
      FlNm := FlNm + '|' + GetFieldName(C);
    end
    else
    	FlNm := GetFieldName(C);

    RowStr := Format(rsRowN, [Row + 1, FlNm]);
    if C is TdxCalcEdit then
    begin
      Prec := TdxCalcEdit(C).Precission;
      if TryStrToFloat(S, E) then
      begin
        if ((E >= 0) and (E > Power(10, 15 - Prec))) or
  				((E < 0) and (E < -Power(10, 15 - Prec))) then
        begin
          Errors.Add(RowStr + Format(rsNumberOutOfRange, [S]));
        	Continue;
        end;
        Csv[i, Row] := FloatToStr(AppUtils.MathRound(E, Prec));
      end
      else
      begin
        Errors.Add(RowStr + Format(rsImpIncorrectNumber, [S]));
        Continue;
      end;
    end
    else if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) then
    begin
      TextLen := Utf8Length(S);
      FieldSize := GetFieldSize(C);
      if (FieldSize > 0) and (TextLen > FieldSize) then
      begin
      	Errors.Add(RowStr + Format(rsTextTooLong, [TextLen, FieldSize]));
        Continue;
      end;
    end
    else if C is TdxCounter then
    begin
      if not TryStrToInt(S, N) then
      begin
        Errors.Add(RowStr + Format(rsImpIncorrectCounter, [S]));
        Continue;
      end;
    end
    else if C is TdxCheckBox then
    begin
      if not TryStrToInt(S, N) then
      begin
        Errors.Add(RowStr + Format(rsImpIncorrectCheck, [S]));
        Continue;
      end;
    end
    else if C is TdxDateEdit then
    begin
      if TryStrToDate(S, D) then
        Csv[i, Row] := DateToStr(D)
      else
      begin
        Errors.Add(RowStr + Format(rsImpIncorrectDate, [S]));
        Continue;
      end;
    end
    else if C is TdxTimeEdit then
    begin
      if TryStrToTime(S, D) then
      begin
        Csv[i, Row] := TimeToStr(D)
      end
      else
      begin
        Errors.Add(RowStr + Format(rsImpIncorrectTime, [S]));
        Continue;
      end;
    end
    else if (C is TdxDBImage) or (C is TdxFile) then
    begin
      if Utf8Length(S) > 255 then
      begin
        Errors.Add(RowStr + rsFilePathTooLong);
        Continue;
      end
      else if Utf8Length(ExtractFileName(S)) > 150 then
      begin
        Errors.Add(RowStr + rsFileNameTooLong);
        Exit;
      end
      else if not FileExists(S) then
      begin
        Errors.Add(RowStr + Format(rsFileNotExists, [S]));
        Continue;
      end;
    end;
  end;
  Result := Errors.Count = ErrCount;
end;

{procedure CsvSplitStr(const S: String; SL: TStrings);
var
  L, b, i, qn: Integer;
begin
  SL.Clear;
  L := Length(S);
  b := 1; qn := 0;
  for i := 1 to L do
  begin
    if S[i] = '"' then Inc(qn);
    if (S[i] = ';') and (not Odd(qn)) then
    begin
      SL.Add(CsvToStr(Copy(S, b, i - b)));
      b := i + 1;
    end;
  end;
  SL.Add(CsvToStr(Copy(S, b, i - b + 1)));
end;

procedure LoadCsv(const FileName: String; SL: TStrings);
var
  FS: TFileStream;
  r, b: Byte;
  Ch, PrevCh: Char;
  Qn: Integer;
  S: String;
begin
  Qn := 0;
  S := '';
  PrevCh := #0;
  FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
  try
   	SkipBOM(FS);

    r := FS.Read(b, 1);
    while r > 0 do
    begin
      Ch := Chr(b);
      if Ch = '"' then Inc(Qn);
      if (Ch in [#13, #10]) and (not Odd(Qn)) then
      begin
        if PrevCh <> #13 then
        begin
          SL.Add(S);
          S := '';
          Qn := 0;
        end;
      end
      else
        S := S + Ch;
      PrevCh := Ch;
      r := FS.Read(b, 1);
    end;
    if S <> '' then
      SL.Add(S);
  finally
    FS.Free;
  end;
end;    }

function ImportData(Fm: TdxForm; const FName: String; ANSI, Unique: Boolean): Boolean;
var
  Fields, Cmps: TList;
  Errors: TStringList;
  S: String;
  i, j, RowN: Integer;
  F: TField;
  DS: TSQLQuery;
  C: TComponent;
  ParentObjExists, AllFieldsLost: Boolean;
  FieldsLost, UnsupportedFields, Msg: String;
  E: Extended;
  DT: TDateTime;
  Csv: TCsvData;
  Skips: array of Boolean;
begin
  Result := False;
  ParentObjExists := False;
  Csv := TCsvData.Create;
  Fields := TList.Create;
  Cmps := TList.Create;
  Errors := TStringList.Create;
  DS := TdxDataSet.Create(nil);
  DS.SQL.Text := SqlSelectStatement(Fm, nil, False, False, nil, '', 0, False);
  DS.InsertSQL.Text := SqlInsertStatement(Fm);
  DS.UpdateSQL.Text := SqlUpdateStatement(Fm);
  DBase.AttachDataSet(DS);
  DS.Open;
  try
	  MainFm.Lock(True);

  	ShowProcessMiniForm(rsLoadingData);
    Application.ProcessMessages;

    Csv.LoadFromFile(FName, ANSI);
    FreeProcessMiniForm;

    if Csv.RowCount = 0 then
    begin
      Info(rsNoDataToImport);
      Exit;
    end;

    // fields
    FieldsLost := '';
    UnsupportedFields := '';
    Msg := '';
    AllFieldsLost := True;
    for i := 0 to Csv.ColCount - 1 do
    begin
      C := FindComponentByFieldName(Fm, Csv[i, 0]);
      F := nil;
      if C = nil then
        FieldsLost := FieldsLost + Csv[i, 0] + '; '
      else if (C is TdxRecordId) or (C is TdxObjectField) or ((C is TdxLookupComboBox) and (GetSourceFId(C) = 0)) then
      begin
      	C := nil;
        UnsupportedFields := UnsupportedFields + Csv[i, 0] + '; '
      end
      else
      begin
        F := DS.FindField(FieldStr(C));
        AllFieldsLost := False;
      end;
      Fields.Add(F);
      Cmps.Add(C);
    end;
    SetLength(FieldsLost, Length(FieldsLost) - 2);
    if AllFieldsLost then
    begin
      ErrMsg(rsImportAllFieldsLostMsg);
      Exit;
    end
    else// if FieldsLost <> '' then
    begin
      SetLength(FieldsLost, Length(FieldsLost) - 2);
      SetLength(UnsupportedFields, Length(UnsupportedFields) - 2);
      if FieldsLost <> '' then
        Msg := Msg + Format(rsImportFieldsLostMsg, [FieldsLost]) + LineEnding + LineEnding;
      if UnsupportedFields <> '' then
        Msg := Msg + Format(rsImportUnsupportedFieldsMsg, [UnsupportedFields]) + LineEnding + LineEnding;
      if Msg <> '' then
      begin
        Msg := Msg + rsImportAnyway;
        if Confirm(rsWarning, Msg) = mrNo then Exit;
      end;
    end;

  // check data

  SetLength(Skips, Csv.RowCount);
  for i := 0 to High(Skips) do
    Skips[i] := False;

  ShowProcessMiniForm(rsCheckingData);
  Application.ProcessMessages;
  for i := 1 to Csv.RowCount - 1 do
    if not CheckData(Csv, Errors, Cmps, i) then
      // помечаются строки, не попадающие в базу
      Skips[i] := True;
  FreeProcessMiniForm;

  if Errors.Count > 0 then
  begin
    if ShowImportErrorsForm(Errors) <> mrOk then Exit;
    Errors.Clear;
  end;

  //data
  ShowProcessForm;
  Application.ProcessMessages;
  try
  	RowN := 0;
    for i := 1 to Csv.RowCount - 1 do
    begin
      if Skips[i] then Continue;
      try

      if Unique and not (CheckUnique(Fm, Cmps, Fields, Csv, i)) then
      begin
        Skips[i] := True;
        Application.ProcessMessages;
      	Continue;
      end;

      DS.Append;
      DS['id'] := DBase.GenId('gen_' + TableStr(Fm.Id));

      for j := 0 to Csv.ColCount - 1 do
      begin
        S := Csv[j, i];
        F := TField(Fields[j]);
        C := TComponent(Cmps[j]);
        if (F = nil) or (S = '') then Continue;

        if C is TdxCalcEdit then
        begin
          E := StrToFloat(S);
          F.AsFloat := E;
        end
        else if C is TdxDateEdit then
        begin
          DT := StrToDate(S);
          F.AsDateTime := DT;
        end
        else if C is TdxTimeEdit then
        begin
          DT := StrToTime(S);
          F.AsDateTime := DT;
        end
        else if C is TdxDBImage then
        begin
          //if FileExists(SL[j]) then
            LoadImageFromFile(S, TdxDBImage(C), DS);
        end
        else if C is TdxFile then
        begin
          //if FileExists(Sl[j]) then
            LoadFileFromFile(S, TdxFile(C), DS);
        end
        else if C is TdxLookupComboBox then
        begin
          if GetSourceTId(C) = Fm.Id then ParentObjExists := True // пропускаем родительские объекты
          else ImportLookupCbx(TdxLookupComboBox(C), F, S)
        end
        else
        begin
          F.Value:=S;
        end;
      end;
      DS.Post;
      Inc(RowN);

      except
        on E: Exception do
        begin
          if DS.State = dsInsert then DS.Cancel;
          Skips[i] := True;
          Errors.Add(Format(rsImportErrorRow, [i, E.Message]));
        end;
      end;

      ProcessFm.Msg.Caption := IntToStr(i) + ' / ' + IntToStr(Csv.RowCount-1);
      ProcessFm.Progress.Position := Round(i /  (Csv.RowCount-1) * 100);
      DBase.ApplyDataSet(DS);
 			DBase.Commit;

	    Application.ProcessMessages;
      if ProcessFm.Canceled then raise EImportError.Create(rsCanceledByUser);
    end;

    // Импорт родительских объектов
    if ParentObjExists then
    begin
      DS.First;
      for i := 1 to Csv.RowCount - 1 do
      begin
        if Skips[i] then Continue;
        DS.Edit;

        try

        for j := 0 to Csv.ColCount - 1 do
        begin
          S := Csv[j, i];
          F := TField(Fields[j]);
          C := TComponent(Cmps[j]);
          if (F <> nil) and (S <> '') then
          begin
            if C is TdxLookupComboBox then
            begin
              if GetSourceTId(C) = Fm.Id  then
                ImportLookupCbx(TdxLookupComboBox(C), F, S);
            end
          end;
        end;
        DS.Post;

        except
          on E: Exception do
          begin
            DS.Cancel;
            Errors.Add(Format(rsImportErrorRow, [i, E.Message]));
          end;
        end;

        ProcessFm.Msg.Caption := IntToStr(i) + ' / ' + IntToStr(Csv.RowCount-1);
        ProcessFm.Progress.Position := Round(i /  (Csv.RowCount-1) * 100);
        DBase.ApplyDataSet(DS);
 			  DBase.Commit;

        DS.Next;
        if DS.Eof then Break;

        Application.ProcessMessages;
        if ProcessFm.Canceled then raise EImportError.Create(rsCanceledByUser);
      end;
    end;

    ProcessFm.Close;
    if Errors.Count = 0 then
      MessageDlg(rsImportData, Format(rsImportSucess, [RowN]), mtInformation, [mbOk], 0)
    else
      ShowAfterImportErrorsForm(Errors);
    Result := True;
  except
    on E: EImportError do
    begin
      ProcessFm.Close;
      Info(E.Message);
    end;
    on E: Exception do
    begin
      ProcessFm.Close;
      ErrMsg(rsDataImportError + ExceptionToString(E, True, False), True, 'ImportData');
    end;
  end;

  finally
    MainFm.Lock(False);
    FreeProcessForm;
    FreeProcessMiniForm;    // На случай какой-нибудь непредвиденной ошибки
    Errors.Free;
    Fields.Free;
    Cmps.Free;
    DS.Close;
    DS.Free;
    Csv.Free;
  end;
end;

function TImportFm.ShowForm(Fm: TdxForm): Integer;
var
  S: TCaption;
begin
  Caption := rsImportData + ': ' + Fm.GetRecordsCaption;
  FileNameEdit1.Text := '';
  RadioGroup1.ItemIndex := 0;
  CheckBox1.Checked := False;
  Result := ShowModal;
  if Result <> mrOk then Exit;

  S := FileNameEdit1.Text;
  if not FileExists(S) then
    ErrMsg(Format(rsFileNotExists, [S]))
  else
  begin
    if ImportData(Fm, S, RadioGroup1.ItemIndex = 1, CheckBox1.Checked) then
    begin
    	Fm.Refresh;
      //Fm.MoveLast;
    end;
  end;
end;

end.

