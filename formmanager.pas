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
unit FormManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dxctrls, strconsts, sqldb;

type

  { TFormManager }

  TFormManager = class
  private
    FForms: TList;
    function GetForm(Index: Integer): TdxForm;
    function GetFormCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromDb;
    procedure SaveToDb;
    procedure SaveToDir(const aDir: String);
    procedure LoadFromDir(const aDir: String);
    function CreateNewForm: TdxForm;
    procedure DeleteForm(Id: Integer);
    function FindForm(Id: Integer): TdxForm;
    function FindFormByName(const FormName: String): TdxForm;
    function FindFormByComponentName(const aName: String): TdxForm;
    procedure FormsToList(SL: TStrings);
    procedure AllFormsToList(SL: TStrings);
    function LoadForm(TId: Integer): TdxForm;
    procedure CorrectMaxTId;
    procedure CorrectMaxFId;
    procedure AddCopyForm(aForm: TdxForm);
    property Forms[Index: Integer]: TdxForm read GetForm;
    property FormCount: Integer read GetFormCount;
  end;

var
  FormMan: TFormManager;

implementation

uses
  dbengine, apputils, Db, LazUtf8, FileUtil;

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
    if Utf8CompareText(Fm.FormCaption, FormName) = 0 then
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

function TFormManager.GetFormCount: Integer;
begin
  Result := FForms.Count;
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

function ReadComponent(St: TStream): TComponent;
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    St.Position:=0;
    ObjectTextToResource(St, MS);
    MS.Position:=0;
    Result := MS.ReadComponentRes(nil);
  finally
    MS.Free;
  end;
end;

procedure WriteComponent(St: TStream; C: TComponent);
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

procedure TFormManager.LoadFromDb;
var
  DS: TDataSet;
  MS: TMemoryStream;
  Fm: TdxForm;
begin
  ClearList(FForms);
  DS := DBase.OpenDataSet('select id, form from dx_forms order by id');
  MS := TMemoryStream.Create;
  try
    while DS.EOF = False do
    begin
      MS.Size:=0;
      TBlobField(DS.Fields[1]).SaveToStream(MS);
      Fm := TdxForm(ReadComponent(MS));
      // Убираем FGrid из Components
      Fm.RemoveComponent(Fm.Grid);
      Fm.RemoveControl(Fm.Grid);
      Fm.Grid.Parent := nil;
      //
      FForms.Add(Fm);

      DS.Next;
    end;
  finally
    MS.Free;
    DS.Free;
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

  DS := DBase.OpenSysTable('select id, form from dx_forms', 'dx_forms');
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
    DBase.Commit;
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
  ClearList(FForms);
  SL := TStringList.Create;
  try

  FindAllFiles(SL, aDir, '*.frm', False);
  SL.CustomSort(@SortFN);
  for i := 0 to SL.Count - 1 do
  begin
    if not TryStrToInt(ChangeFileExt(ExtractFileName(SL[i]), ''), id) then
      Continue;
    FS := TFileStream.Create(SL[i], fmOpenRead);
    try
      Fm := TdxForm(ReadComponent(FS));
      // Убираем FGrid из Components
      Fm.RemoveComponent(Fm.Grid);
      Fm.RemoveControl(Fm.Grid);
      Fm.Grid.Parent := nil;
      //
      FForms.Add(Fm);
    finally
      FS.Free;
    end;
  end;

  finally
    SL.Free;
  end;
end;

function TFormManager.CreateNewForm: TdxForm;
begin
  Result := TdxForm.Create(nil);
  Result.Id:=DBase.GenId('gen_tid');
  Result.Name := 'Form' + IntToStr(Result.Id);
  Result.FormCaption:=Format(rsDefaultFormCaption, [IntToStr(Result.Id)]);
  FForms.Add(Result);
end;

procedure TFormManager.DeleteForm(Id: Integer);
var
  Fm: TdxForm;
begin
  Fm := FindForm(Id);
  Fm.Free;
  FForms.Remove(Fm);
end;

procedure TFormManager.FormsToList(SL: TStrings);
var
  i: Integer;

  procedure AddToList(Fm: TdxForm);
  var
    j: Integer;
  begin
    for j := 0 to SL.Count - 1 do
      if Utf8CompareText(SL[j], Fm.FormCaption) > 0 then
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
      if Utf8CompareText(SL[j], Fm.FormCaption) > 0 then
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
      if (TdxForm(SL.Objects[j]).PId <> Fm.PId) or (Utf8CompareText(SL[j], '    ' + Fm.FormCaption) > 0) then
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

function TFormManager.LoadForm(TId: Integer): TdxForm;
var
  MS: TMemoryStream;
  Fm: TdxForm;
begin
  MS := TMemoryStream.Create;
  try
    Fm := FindForm(TId);
    WriteComponent(MS, Fm);
    Fm := TdxForm(ReadComponent(MS));
    // Убираем FGrid из Components
    Fm.RemoveComponent(Fm.Grid);
    Fm.RemoveControl(Fm.Grid);
    Fm.Grid.Parent := nil;
    Result := Fm;
  finally
    MS.Free;
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
  DBase.ChangeId('gen_tid', MaxId);
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
  DBase.ChangeId('gen_fid', MaxId);
end;

procedure TFormManager.AddCopyForm(aForm: TdxForm);
var
  MS: TMemoryStream;
  Fm: TdxForm;
begin
  MS := TMemoryStream.Create;
  WriteComponent(MS, aForm);
  Fm := TdxForm(ReadComponent(MS));
  // Убираем FGrid из Components
  Fm.RemoveComponent(Fm.Grid);
  Fm.RemoveControl(Fm.Grid);
  Fm.Grid.Parent := nil;
  FForms.Add(Fm);
end;

end.

