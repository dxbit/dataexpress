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

unit FormManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dxctrls, strconsts, sqldb, Forms, Controls, LclType, Db;

type

  { TFormManager }

  TFormManager = class
  private
    FForms: TList;
    FDataSet: TDataSet;
    FReadErrors: String;
    function GetForm(Index: Integer): TdxForm;
    function GetFormCount: Integer;
    procedure ChangeIndexes(DeletedFm: TdxForm);
    function GetParentFormCount: Integer;
    function ReadForm(St: TStream): TdxForm;
    procedure ReaderError(Reader: TReader; const Message: string;
      var Handled: Boolean);
    procedure WriteComponent(St: TStream; C: TComponent);
    procedure ShowReadErrorsMsg;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromDb;
    procedure SaveToDb;
    procedure SaveToDir(const aDir: String);
    //procedure SaveToDirFromDataSet(const aDir: String);
    procedure LoadFromDir(const aDir: String);
    function LoadFromFile(const FileName: String): TdxForm;
    function CreateNewForm: TdxForm;
    function CreateNewChildForm(ParentId: Integer): TdxForm;
    procedure DeleteForm(Id: Integer);
    function FindForm(Id: Integer): TdxForm;
    function FindFormByName(const FormName: String): TdxForm;
    function FindFormByComponentName(const aName: String): TdxForm;
    procedure SourceFormsToList(SL: TStrings);
    procedure FormsToList(SL: TStrings);
    procedure AllFormsToList(SL: TStrings);
    procedure SortFormsByIndex(SL: TStrings);
    function LoadForm(TId: Integer): TdxForm;
    procedure CorrectMaxTId;
    procedure CorrectMaxFId;
    function AddCopyForm(aForm: TdxForm): TdxForm;
    function CloneManager: TFormManager;
    procedure SetDefaultIndexes;
    procedure Clear;
    function MakeUniqueFormName(const AnyFormName: String): String;
    function MakeUniqueFormComponentName(const AnyName: String): String;
    property Forms[Index: Integer]: TdxForm read GetForm;
    property FormCount: Integer read GetFormCount;
  end;

var
  FormMan: TFormManager;

implementation

uses
  dbengine, apputils, LazUtf8, FileUtil, mytypes, dxmains, dximages;

{ TFormManager }

function TFormManager.GetForm(Index: Integer): TdxForm;
begin
  Result := TdxForm(FForms[Index]);
end;

function TFormManager.FindForm(Id: Integer): TdxForm;
var
  i: Integer;
  Fm: TdxForm;
begin
  Result := nil;
  for i := 0 to FForms.Count - 1 do
  begin
    Fm := GetForm(i);
    if Id = Fm.Id then
      Exit(Fm);
  end;
end;

function TFormManager.FindFormByName(const FormName: String): TdxForm;
var
  i: Integer;
  Fm: TdxForm;
begin
  Result := nil;
  for i := 0 to FForms.Count - 1 do
  begin
    Fm := GetForm(i);
    if MyUtf8CompareText(Fm.FormCaption, FormName) = 0 then
      Exit(Fm);
  end;
end;

function TFormManager.FindFormByComponentName(const aName: String): TdxForm;
var
  i: Integer;
  Fm: TdxForm;
begin
  Result := nil;
  for i := 0 to FForms.Count - 1 do
  begin
    Fm := GetForm(i);
    if CompareText(Fm.Name, aName) = 0 then
      Exit(Fm);
  end;
end;

procedure TFormManager.SourceFormsToList(SL: TStrings);
var
  i: Integer;

  procedure AddToList(Fm: TdxForm);
  var
    j: Integer;
  begin
    for j := 0 to SL.Count - 1 do
      if MyUtf8CompareText(SL[j], Fm.FormCaption) > 0 then
      begin
        SL.InsertObject(j, Fm.FormCaption, Fm);
        Exit;
      end;
    SL.AddObject(Fm.FormCaption, Fm);
  end;

begin
  SL.Clear;
  for i := 0 to FormCount - 1 do
    if (Forms[i].PId = 0) and (Forms[i].ViewType <> vtSimpleForm) then
      AddToList(Forms[i]);
end;

function TFormManager.GetFormCount: Integer;
begin
  Result := FForms.Count;
end;

procedure TFormManager.ChangeIndexes(DeletedFm: TdxForm);
var
  Idx, i: Integer;
  Fm: TdxForm;
begin
  Idx := DeletedFm.Index;
  for i := 0 to FormCount - 1 do
  begin
    Fm := Forms[i];
    if (Fm.PId = 0) and (Fm.Index > Idx) then Fm.Index := Fm.Index - 1;
  end;
end;

function TFormManager.GetParentFormCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FormCount - 1 do
    if Forms[i].PId = 0 then Inc(Result);
end;

constructor TFormManager.Create;
begin
  FForms := TList.Create;
end;

destructor TFormManager.Destroy;
begin
  ClearList(FForms);
  FForms.Free;
  inherited Destroy;
end;

function TFormManager.ReadForm(St: TStream): TdxForm;
var
  MS: TMemoryStream;
begin
  St.Position:=0;
  MS := TMemoryStream.Create;
  try
    ObjectTextToBinary(St, MS);
    MS.Position:=0;
    with TReader.Create(MS, 4096) do
    try
      OnError:=@ReaderError;
      Result := TdxForm(ReadRootComponent(nil));
      Result.RemoveGridTree;
      Result.SetTabStops;
    finally
      Free;
    end;
  finally
    MS.Free;
  end;
end;

procedure TFormManager.ReaderError(Reader: TReader; const Message: string;
  var Handled: Boolean);
begin
  FReadErrors := FReadErrors + '- ' + Message + LineEnding;
  Handled := True;
end;

procedure TFormManager.WriteComponent(St: TStream; C: TComponent);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.WriteComponent(C);
    MS.Position := 0;
    St.Size := 0;
    ObjectBinaryToText(MS, St, oteLfm);
    St.Position := 0;
  finally
    MS.Free;
  end;
end;

procedure TFormManager.ShowReadErrorsMsg;
begin
  Info(Format(rsReadMetadataError, [Spaces + FReadErrors]));
end;

function ProcessSimpleForm(St: TStream): Boolean;
var
  P: TParser;
  Buf: String;
begin
  Result := False;
  St.Position := 0;
  P := TParser.Create(St);
  try

  P.NextToken; // пропускаем слово object
  P.NextToken;
  while P.Token <> toEof do
  begin
    if P.TokenSymbolIs('ViewType') then
    begin
      P.NextToken;      // =
      P.NextToken;      // значение
      if P.TokenSymbolIs('vtSimpleForm') then
        Result := True
      else
        Exit;
    end
    else if P.TokenSymbolIs('object') then
    begin
      if Result then
      begin
        St.Size := P.SourcePos - Length(P.TokenString);
        St.Position := St.Size - 1;
        Buf := 'end';
        St.Write(Pointer(Buf)^, Length(Buf));
      end;
      Exit;
    end;
    P.NextToken;
  end;

  finally
    P.Free;
  end;
end;

procedure StoreOnlyFields(Fm: TdxForm);
var
  i: Integer;
  C: TComponent;
begin
  for i := Fm.ComponentCount - 1 downto 0 do
  begin
    C := Fm.Components[i];
    if not HasFId(C) then C.Free;
  end;
end;

procedure TFormManager.LoadFromDb;
var
  DS: TDataSet;
  MS: TStream;
  Fm: TdxForm;
begin
  FreeAndNil(FDataSet);
  FReadErrors := '';
  ClearList(FForms);
  DS := DBase.OpenDataSet('select id, form from dx_forms order by id');
  FDataSet := DS;
  try
    while DS.EOF = False do
    begin
      MS := DS.CreateBlobStream(DS.Fields[1], bmRead);
      try
        if DXMain.AllowDynamicForms then ProcessSimpleForm(MS);
        Fm := ReadForm(MS);
      finally
        MS.Free;
      end;
      if DXMain.AllowDynamicForms then StoreOnlyFields(Fm);

      FForms.Add(Fm);

      if DBase.IsRemote then
        Application.ProcessMessages;
      DS.Next;
    end;
    SetDefaultIndexes;
    if FReadErrors <> '' then
      ShowReadErrorsMsg;
  finally
    if not DXMain.AllowDynamicForms then FreeAndNil(FDataSet);
  end;
end;

procedure TFormManager.SaveToDb;
var
  i: Integer;
  Fm: TdxForm;
  MS: TMemoryStream;
  DS: TSQLQuery;
begin
  //DBase.Execute('delete from dx_forms;');

  DS := DBase.OpenDataSet('select id, form from dx_forms');
  MS := TMemoryStream.Create;
  try
    // Удаляем из кэша вместо выполнения прямого запроса на удаление. Был
    // случай когда оборвалось соединение с базой в тот момент когда были
    // удалены записи в dx_forms (Shand от 30.03.2017).
  	while not DS.Eof do
    	DS.Delete;

    for i := 0 to FForms.Count - 1 do
    begin
      Fm := GetForm(i);
      WriteComponent(MS, Fm);

      DS.Append;
      DS.Fields[0].Value:=Fm.Id;
      TBlobField(DS.Fields[1]).LoadFromStream(MS);
      DS.Post;
    end;
    DBase.ApplyDataset(DS);
    //DBase.Commit;
  finally
    MS.Free;
    DS.Free;
  end;
end;

procedure TFormManager.SaveToDir(const aDir: String);
var
  i: Integer;
  FS: TFileStream;
  Fm: TdxForm;
begin
  for i := 0 to FormCount - 1 do
  begin
    Fm := Forms[i];
    FS := TFileStream.Create(aDir + IntToStr(Fm.Id) + '.frm', fmCreate + fmOpenWrite);
    try
      WriteComponent(FS, Fm);
    finally
      FS.Free;
    end;
  end;
end;

{procedure TFormManager.SaveToDirFromDataSet(const aDir: String);
var
  BS: TStream;
begin
  FDataSet.First;
  while not FDataSet.EOF do
  begin
    BS := FDataSet.CreateBlobStream(FDataSet.Fields[1], bmRead);
    with TFileStream.Create(aDir + FDataSet.Fields[0].AsString + '.frm', fmCreate) do
    try
      CopyFrom(BS, BS.Size);
    finally
      Free;
      BS.Free;
    end;
    FDataSet.Next;
  end;
end;        }

function SortFN(List: TStringList; Index1, Index2: Integer): Integer;
var
  n1, n2: integer;
begin
  Result := 0;
  if not TryStrToInt(ExtractFileName(ChangeFileExt(List[Index1], '')), n1) then Exit;
  if not TryStrToInt(ExtractFileName(ChangeFileExt(List[Index2], '')), n2) then Exit;
  Result := n1 - n2;
end;

procedure TFormManager.LoadFromDir(const aDir: String);
var
  SL: TStringList;
  i, id: Integer;
  FS: TFileStream;
  Fm: TdxForm;
begin
  FreeAndNil(FDataSet);
  FReadErrors := '';
  ClearList(FForms);
  SL := TStringList.Create;
  try

  FindAllFiles(SL, aDir, '*.frm', False);
  SL.CustomSort(@SortFN);
  for i := 0 to SL.Count - 1 do
  begin
    if not TryStrToInt(ChangeFileExt(ExtractFileName(SL[i]), ''), id) then
      Continue;
    FS := TFileStream.Create(SL[i], fmOpenRead + fmShareDenyNone);
    try
      Fm := ReadForm(FS);
      // Убираем FGrid из Components
      //Fm.RemoveGridTree;
      FForms.Add(Fm);
    finally
      FS.Free;
    end;
  end;
  SetDefaultIndexes;
  if FReadErrors <> '' then
    ShowReadErrorsMsg;


  finally
    SL.Free;
  end;
end;

function TFormManager.LoadFromFile(const FileName: String): TdxForm;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
  try
    Result := ReadForm(FS);
    // Убираем FGrid из Components
    //Result.RemoveGridTree;
  finally
    FS.Free;
  end;
end;

function TFormManager.CreateNewForm: TdxForm;
begin
  Result := TdxForm.Create(nil);
  Result.Id:=DBase.GenId('gen_tid');
  Result.Name := MakeUniqueFormComponentName('Form');  //'Form' + IntToStr(Result.Id);
  Result.FormCaption := MakeUniqueFormName(rsForm); //Format(rsDefaultFormCaption, [IntToStr(Result.Id)]);
  FForms.Add(Result);
  Result.Index := GetParentFormCount;
end;

function TFormManager.CreateNewChildForm(ParentId: Integer): TdxForm;
begin
  Result := CreateNewForm;
  Result.PId := ParentId;
  Result.Index := 0;
end;

procedure TFormManager.DeleteForm(Id: Integer);
var
  Fm: TdxForm;
  i: Integer;
begin
  for i := FormCount - 1 downto 0 do
  begin
    Fm := Forms[i];
    if (Fm.Id = Id) or (Fm.PId = Id) then
    begin
      if Fm.PId = 0 then ChangeIndexes(Fm);
      Fm.Free;
      FForms.Remove(Fm)
    end;
  end;
end;

procedure TFormManager.FormsToList(SL: TStrings);
var
  i: Integer;

  procedure AddToList(Fm: TdxForm);
  var
    j: Integer;
  begin
    for j := 0 to SL.Count - 1 do
      if MyUtf8CompareText(SL[j], Fm.FormCaption) > 0 then
      begin
        SL.InsertObject(j, Fm.FormCaption, Fm);
        Exit;
      end;
    SL.AddObject(Fm.FormCaption, Fm);
  end;

begin
  SL.Clear;
  for i := 0 to FormCount - 1 do
    if Forms[i].PId = 0 then
      AddToList(Forms[i]);
end;

procedure TFormManager.AllFormsToList(SL: TStrings);
var
  i: Integer;

  procedure AddToList(Fm: TdxForm);
  var
    j: Integer;
  begin
    for j := 0 to SL.Count - 1 do
      if (TdxForm(SL.Objects[j]).PId = 0) and (MyUtf8CompareText(SL[j], Fm.FormCaption) > 0) then
      begin
        SL.InsertObject(j, Fm.FormCaption, Fm);
        Exit;
      end;
    SL.AddObject(Fm.FormCaption, Fm);
  end;

  function FindFm(Id: Integer): Integer;
  var
    j: Integer;
    Fm: TdxForm;
  begin
    Result := -1;
    for j := 0 to SL.Count - 1 do
    begin
      Fm := TdxForm(SL.Objects[j]);
      if Fm.Id = Id then Exit(j);
    end;
  end;

  procedure AddChildToList(Fm: TdxForm);
  var
    j, n: Integer;
  begin
    n := FindFm(Fm.PId);
    for j := n + 1 to SL.Count - 1 do
      if (TdxForm(SL.Objects[j]).PId <> Fm.PId) or (MyUtf8CompareText(SL[j], '    ' + Fm.FormCaption) > 0) then
      begin
        SL.InsertObject(j, '    ' + Fm.FormCaption, Fm);
        Exit;
      end;
    SL.AddObject('    ' + Fm.FormCaption, Fm);
  end;

begin
  SL.Clear;
  for i := 0 to FormCount - 1 do
  begin
    if Forms[i].PId = 0 then
      AddToList(Forms[i])
    else
      AddChildToList(Forms[i]);
  end;
end;

procedure TFormManager.SortFormsByIndex(SL: TStrings);

  procedure AddToList(Fm: TdxForm);
  var
    i: Integer;
  begin
    for i := 0 to SL.Count - 1 do
    begin
      if Fm.Index < TdxForm(SL.Objects[i]).Index then
      begin
        SL.InsertObject(i, Fm.FormCaption, Fm);
        Exit;
      end;
    end;
    SL.AddObject(Fm.FormCaption, Fm);
  end;

var
  i: Integer;
  Fm: TdxForm;
begin
  SL.Clear;
  for i := 0 to FormCount - 1 do
  begin
    Fm := Forms[i];
    if Fm.PId = 0 then AddToList(Fm);
  end;
end;

{function TFormManager.LoadForm(TId: Integer): TdxForm;
var
  DS: TSQLQuery;
  MS: TMemoryStream;
  Fm: TdxForm;
begin
  Fm := nil;
  DS := DBase.OpenDataSet('select id, form from dx_forms where id=' + IntToStr(TId));
  MS := TMemoryStream.Create;
  try
    if DS.RecordCount > 0 then
    begin
      TBlobField(DS.Fields[1]).SaveToStream(MS);
      Fm := TdxForm(ReadComponent(MS));
      // Убираем FGrid из Components
      Fm.RemoveComponent(Fm.Grid);
      Fm.RemoveControl(Fm.Grid);
      Fm.Grid.Parent := nil;
    end;
    Result := Fm;
  finally
    MS.Free;
    DS.Free;
  end;
end;    }

procedure ScaleAllFonts(Fm: TdxForm);

  procedure _DoScale(WC: TWinControl);
  var
    i: Integer;
    C: TControl;
  begin
    for i := 0 to WC.ControlCount - 1 do
    begin
      C := WC.Controls[i];
      C.Font.Height := C.Scale96ToFont(C.Font.Height);
      if C is TWinControl then
        _DoScale(TWinControl(C));
    end;
  end;

begin
  _DoScale(Fm);
end;

function TFormManager.LoadForm(TId: Integer): TdxForm;
var
  MS: TMemoryStream;
  Fm: TdxForm;
  BS: TStream;
begin
  BS := nil;
  MS := TMemoryStream.Create;
  try
    Fm := FindForm(TId);
    if DXMain.AllowDynamicForms and FDataSet.Locate('id', TId, []) then
    begin
      BS := FDataSet.CreateBlobStream(FDataSet.Fields[1], bmRead);
      Fm := ReadForm(BS);
      ScaleForm(Fm, DXMain.DesignTimePPI);
    end
    else
    begin
      MS.WriteComponent(Fm);
      MS.Position := 0;
      Fm := TdxForm(MS.ReadComponent(nil));
      Fm.RemoveGridTree;
      Fm.SetTabStops;
    end;
    Result := Fm;
  finally
    MS.Free;
    FreeAndNil(BS);
  end;
end;

procedure TFormManager.CorrectMaxTId;
var
  i, MaxId: Integer;
  Fm: TdxForm;
begin
  MaxId := DBase.GetCurrentId('gen_tid');
  for i := 0 to FormCount - 1 do
  begin
    Fm := Forms[i];
    if Fm.Id > MaxId then MaxId := Fm.Id;
  end;
  DBase.ChangeId('gen_tid', MaxId + 1);
end;

procedure TFormManager.CorrectMaxFId;
var
  i, j, id, MaxId: Integer;
  Fm: TdxForm;
  C: TComponent;
begin
  MaxId := DBase.GetCurrentId('gen_fid');
  for i := 0 to FormCount - 1 do
  begin
    Fm := Forms[i];
    for j := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[j];
      if not HasFId(C) then Continue;
      id := GetId(C);
      if id > MaxId then MaxId := id;
    end;
  end;
  DBase.ChangeId('gen_fid', MaxId + 1);
end;

function TFormManager.AddCopyForm(aForm: TdxForm): TdxForm;
var
  MS: TMemoryStream;
  Fm: TdxForm;
begin
  MS := TMemoryStream.Create;
  MS.WriteComponent(aForm);
  MS.Position := 0;
  Fm := TdxForm(MS.ReadComponent(nil));
  Fm.RemoveGridTree;
  Fm.SetTabStops;
  FForms.Add(Fm);
  MS.Free;
  Result := Fm;
end;

function TFormManager.CloneManager: TFormManager;
var
  i: Integer;
begin
  Result := TFormManager.Create;
  for i := 0 to FormCount - 1 do
    Result.AddCopyForm(GetForm(i));
end;

procedure TFormManager.SetDefaultIndexes;
var
  SL: TStrings;
  i: Integer;
  Fm1, Fm2: TdxForm;
  NeedSetIndex: Boolean;
begin
  NeedSetIndex := False;
  SL := TStringList.Create;
  FormsToList(SL);

  if SL.Count > 1 then
  begin
    Fm1 := TdxForm(SL.Objects[0]);
    Fm2 := TdxForm(SL.Objects[1]);
    NeedSetIndex := (Fm1.Index = 0) and (Fm2.Index = 0);
  end;

  if NeedSetIndex then
    for i := 0 to SL.Count - 1 do
      TdxForm(SL.Objects[i]).Index := i;

  SL.Free;
end;

procedure TFormManager.Clear;
begin
  ClearList(FForms);
end;

function TFormManager.MakeUniqueFormName(const AnyFormName: String): String;
var
  S: String;
  n: Integer;
begin
  SplitComponentName(AnyFormName, S, n);
  Inc(n);
  while FindFormByName(S + IntToStr(n)) <> nil do
    Inc(n);
  Result := S + IntToStr(n);
end;

function TFormManager.MakeUniqueFormComponentName(const AnyName: String): String;
var
  S: String;
  n: Integer;
begin
  SplitComponentName(AnyName, S, n);
  Inc(n);
  while FindFormByComponentName(S + IntToStr(n)) <> nil do
    Inc(n);
  Result := S + IntToStr(n);
end;

end.

