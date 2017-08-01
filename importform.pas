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

implementation

uses
  apputils, sqlgen, dbengine, importerrorsform, processform, dximages, dxfiles,
  helpform, mainform, formmanager, DateUtils, Math, LazUtf8;

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

  if Fm.ParentField = 0 then Result := GetLookupId(L, Value)
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
  F.AsInteger := Id;
  F.DataSet.FieldByName(F.FieldName + 'l').AsString := Value;
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

  F.AsInteger := PId;
  F.DataSet.FieldByName(F.FieldName + 'l').AsString := Value;
end;

procedure ImportLookupCbx(L: TdxLookupComboBox; F: TField; const Value: String);
var
  Fm: TdxForm;
begin
  Fm := FormMan.FindForm(L.SourceTId);
  if Fm = nil then Exit;

  if Fm.ParentField = 0 then ImportLookup(L, F, Value)
  else ImportTreeLookup(Fm, L, F, Value);
end;

function CheckUnique(Fm: TdxForm; Vals: TStringList; Cmps, Fields: TList): Boolean;
var
  S, V: String;
  i, id, n: Integer;
  F: TField;
  C: TComponent;
  T: TDateTime;
  FS: TFormatSettings;
begin
  Result := True;
  S := '';
  for i := 0 to Vals.Count - 1 do
  begin
    if i >= Fields.Count then Break;
    if Fields[i] = nil then Continue;
    F := TField(Fields[i]);
    C := TComponent(Cmps[i]);
    // экранируем апострофы
    V := StringReplace(Vals[i], '''', '''' + '''', [rfReplaceAll]);
    if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) then
    begin
      if V = '' then S := S + F.fieldName + ' is null and '
      else S := S + 'lower(' + F.FieldName + ')=lower(''' + V + ''') and '
      //S := S + 'lower(coalesce(' + F.FieldName + ',''''))=lower(''' + V + ''') and '
    end
    else if C is TdxDateEdit then
    begin
      if V = '' then S := S + F.FieldName + ' is null and '
      else S := S + F.FieldName + '=''' + V + ''' and ';
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
      n := TdxCalcEdit(C).Precission;
      if V = '' then S := S + F.FieldName + ' is null and '
      else
      begin
        FS := DefaultFormatSettings;
        FS.DecimalSeparator:='.';
        S := S + F.FieldName + '>=' + V + '-' + V + '*2e-12 and ' +
        	F.FieldName + '<=' + V + '+' + V + '*2e-12 and ';
        //V := FloatToStr(RoundTo(StrToFloat(V, FS), -n), FS);
        //S := S + Format('ABS(%s-(%s))<0.01 and ', [F.FieldName, V]);
        //S := S + Format('ROUND(%s,%d)=%s and ', [F.FieldName, n, V]);
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
  with DBase.OpenDataSet(S) do
  try
    Result := Fields[0].IsNull;
  finally
    Free;
  end;
end;

function CheckData(Vals, Errors: TStringList; Cmps: TList; CurLine: Integer): Boolean;
var
  i, N: Integer;
  E: Extended;
  D: TDatetime;
  C: TComponent;
begin
  Result := False;
  for i := 0 to Vals.Count - 1 do
  begin
    if i >= Cmps.Count then Break;
    if (Cmps[i] = nil) or (Vals[i] = '') then Continue;
    C := TComponent(Cmps[i]);
    if C is TdxCalcEdit then
    begin
      if TryStrToFloat(Vals[i], E) then
        Vals[i] := StringReplace(Vals[i], ',', '.', [rfReplaceAll])
      else
      begin
        Errors.Add(Format(rsRowN, [CurLine]) + Format(rsImpIncorrectNumber, [Vals[i]]));
        Exit;
      end;
    end
    else if C is TdxCounter then
    begin
      if not TryStrToInt(Vals[i], N) then
      begin
        Errors.Add(Format(rsRowN, [CurLine]) + Format(rsImpIncorrectCounter, [Vals[i]]));
        Exit;
      end;
    end
    else if C is TdxCheckBox then
    begin
      if not TryStrToInt(Vals[i], N) then
      begin
        Errors.Add(Format(rsRowN, [CurLine]) + Format(rsImpIncorrectCheck, [Vals[i]]));
        Exit;
      end;
    end
    else if C is TdxDateEdit then
    begin
      if TryStrToDate(Vals[i], D) then
        Vals[i] := DateToStr(D)
      else
      begin
        Errors.Add(Format(rsRowN, [CurLine]) + Format(rsImpIncorrectDate, [Vals[i]]));
        Exit;
      end;
    end
    else if C is TdxTimeEdit then
    begin
      if TryStrToTime(Vals[i], D) then
      begin
        Vals[i] := TimeToStr(D)
      end
      else
      begin
        Errors.Add(Format(rsRowN, [CurLine]) + Format(rsImpIncorrectTime, [Vals[i]]));
        Exit;
      end;
    end;
  end;
  Result := True;
end;

procedure CsvSplitStr(const S: String; SL: TStrings);
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
  FS := TFileStream.Create(FileName, fmOpenRead);
  try
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
end;

procedure ImportData(Fm: TdxForm; const FName: String; ANSI, Unique: Boolean);
var
  Fields, Cmps: TList;
  Data, SL, Errors: TStringList;
  S: String;
  i, j, RowN: Integer;
  F: TField;
  InsertEvent, ScrollEvent, PostEvent: TDataSetNotifyEvent;
  aDS: TSQLQuery;
  C: TComponent;
  ParentObjExists: Boolean;
  ChangeEvents: array of TFieldNotifyEvent;
begin
  ParentObjExists := False;
  Fields := TList.Create;
  Cmps := TList.Create;
  Data := TStringList.Create;
  SL := TStringList.Create;
  Errors := TStringList.Create;
  aDS := TSQLQuery(Fm.Grid.DataSource.DataSet);
  aDS.DisableControls;
  InsertEvent := aDS.AfterInsert;
  ScrollEvent := aDS.AfterScroll;
  PostEvent := aDS.AfterPost;
  aDS.AfterInsert := nil;
  aDS.AfterScroll := nil;
  aDS.AfterPost := nil;
  try
    LoadCsv(FName, Data);
    if Data.Count = 0 then Exit;
    // fields
    S := Data[0];
    if ANSI then S := WinCPToUtf8(S);
    CsvSplitStr(S, SL);
    for i := 0 to SL.Count - 1 do
    begin
      C := FindComponentByFieldName(Fm, CsvToStr(SL[i]));
      F := nil;
      if (C <> nil) and (not (C is TdxObjectField)) then
        F := aDS.FindField(FieldStr(C));
      Fields.Add(F);
      Cmps.Add(C);
    end;
    // Отключаем событие изменения поля
    SetLength(ChangeEvents, Fields.Count);
    for i := 0 to Fields.Count - 1 do
    begin
      F := TField(Fields[i]);
      if F <> nil then
      begin
 		    ChangeEvents[i] := F.OnChange;
   		  F.OnChange := nil;
      end;
    end;

  //data
  MainFm.Lock(True);
  ProcessFm.ShowForm;
  Application.ProcessMessages;
  try
  	RowN := 0;
    for i := 1 to Data.Count - 1 do
    begin
      S := Data[i];
      if S = '' then Continue;
      if ANSI then S := WinCPToUtf8(S);
      CsvSplitStr(S, SL);
      if not CheckData(SL, Errors, Cmps, i) then Continue;
      if Unique and not (CheckUnique(Fm, SL, Cmps, Fields)) then
      begin
        Application.ProcessMessages;
      	Continue;
      end;
      aDS.Append;
      aDS['id'] := DBase.GenId('gen_' + TableStr(Fm.Id));

      try
      for j := 0 to SL.Count - 1 do
      begin
        if (j < Fields.Count) and (Fields[j] <> nil) and (SL[j] <> '') then
        begin
          F := TField(Fields[j]);
          C := TComponent(Cmps[j]);
          if C is TdxDBImage then
          begin
            if FileExists(SL[j]) then
              LoadImageFromFile(SL[j], TdxDBImage(C), aDS);
          end
          else if C is TdxFile then
          begin
            if FileExists(Sl[j]) then
              LoadFileFromFile(SL[j], TdxFile(C), aDS);
          end
          else if C is TdxLookupComboBox then
          begin
            if GetSourceTId(C) = Fm.Id then ParentObjExists := True // пропускаем родительские объекты
            else ImportLookupCbx(TdxLookupComboBox(C), F, SL[j])
          end
          else
            F.Value:=SL[j];
        end;
      end;
      aDS.Post;
      Inc(RowN);

      except
        on E: Exception do
        begin
          aDS.Cancel;
          Errors.Add(Format(rsImportErrorRow, [i, E.Message]));
        end;
      end;

      if ProcessFm.Canceled then raise EImportError.Create(rsCanceledByUser);
      ProcessFm.Msg.Caption := IntToStr(i) + ' / ' + IntToStr(Data.Count-1);
      ProcessFm.Progress.Position := Round(i /  (Data.Count-1) * 100);
      DBase.ApplyDataSet(aDS);
 			DBase.Commit;
	    Application.ProcessMessages;
    end;

    // Импорт родительских объектов
    if ParentObjExists then
    begin
      aDS.First;
      for i := 1 to Data.Count - 1 do
      begin
        S := Data[i];
        if S = '' then Continue;
        if ANSI then S := WinCPToUtf8(S);
        CsvSplitStr(S, SL);
        aDS.Edit;

        try

        for j := 0 to SL.Count - 1 do
        begin
          if (j < Fields.Count) and (Fields[j] <> nil) and (SL[j] <> '') then
          begin
            F := TField(Fields[j]);
            C := TComponent(Cmps[j]);
            if C is TdxLookupComboBox then
            begin
              if GetSourceTId(C) = Fm.Id  then
                ImportLookupCbx(TdxLookupComboBox(C), F, SL[j])
            end
          end;
        end;
        aDS.Post;

        except
          on E: Exception do
          begin
            aDS.Cancel;
            Errors.Add(Format(rsImportErrorRow, [i, E.Message]));
          end;
        end;
        aDS.Next;
        if aDS.Eof then Break;

        if ProcessFm.Canceled then raise EImportError.Create(rsCanceledByUser);
        ProcessFm.Msg.Caption := IntToStr(i) + ' / ' + IntToStr(Data.Count-1);
        ProcessFm.Progress.Position := Round(i /  (Data.Count-1) * 100);
        DBase.ApplyDataSet(aDS);
 			  DBase.Commit;

        Application.ProcessMessages;
      end;
    end;

    if Errors.Count = 0 then
      MessageDlg(rsImportData, Format(rsImportSucess, [RowN]), mtInformation, [mbOk], 0)
    else
      ImportErrorsFm.ShowForm(Errors);
  except
    on E: EImportError do
    begin
      aDS.CancelUpdates;
      DBase.Rollback;
      ErrMsg(E.Message);
    end;
  end;

  finally
    for i := 0 to Fields.Count - 1 do
    	if Fields[i] <> nil then
	    	TField(Fields[i]).OnChange:=ChangeEvents[i];
    SetLength(ChangeEvents, 0);

    MainFm.Lock(False);
    ProcessFm.Close;
    Data.Free;
    SL.Free;
    Errors.Free;
    Fields.Free;
    Cmps.Free;
    aDS.AfterInsert := InsertEvent;
    aDS.AfterScroll := ScrollEvent;
    aDS.AfterPost := PostEvent;
    aDS.EnableControls;
  end;
end;

function TImportFm.ShowForm(Fm: TdxForm): Integer;
var
  S: TCaption;
begin
  Caption := rsImportData + ': ' + Fm.FormCaption;
  FileNameEdit1.Text := '';
  RadioGroup1.ItemIndex := 0;
  CheckBox1.Checked := False;
  Result := ShowModal;
  if Result <> mrOk then Exit;
  S := FileNameEdit1.Text;
  if not FileExists(S) then
    ErrMsg(Format(rsFileNotExists, [S]))
  else
    ImportData(Fm, S, RadioGroup1.ItemIndex = 1, CheckBox1.Checked);
end;

end.

