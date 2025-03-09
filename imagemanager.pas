{-------------------------------------------------------------------------------

    Copyright 2015-2025 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit ImageManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LazUtf8, Forms, Controls, Db, SqlDb, dbengine,
  BGRABitmap, BGRABitmapTypes, FPimage;

type
  PImageCacheData = ^TImageCacheData;
  TImageCacheData = record
    ImageName: String;
    ImageIndex: Integer;
    ImageList: TImageList;
  end;

  { TImageCache }

  TImageCache = class(TList)
  private
    function GetImages(Index: Integer): PImageCacheData;
  public
    function AddImage(const ImageName: String; Bitmap: TBitmap): PImageCacheData;
    function FindImage(const ImageName: String): PImageCacheData;
    procedure Clear; override;
    procedure DeleteImage(const ImageName: String);
    procedure RenameImage(const OldName, NewName: String);
    property Images[Index: Integer]: PImageCacheData read GetImages; default;
  end;

  { TImageManager }

  TImageManager = class
  private
    FDataSet: TSQLQuery;
    FMaxId: Integer;
    function GetMaxId: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateTable;
    procedure LoadFromDb;
    procedure SaveToDb;
    procedure LoadFromDir(const Dir: String);
    procedure SaveToDir(const Dir: String);
    procedure LoadFromCache(const Dir: String);
    procedure GetNames(SL: TStrings);
    procedure AddImage(const AName: String);
    procedure RenameImage(const OldName, NewName: String);
    procedure DeleteImage(const AName: String);
    function ImageExists(const AName: String): Boolean;
    function GetImageExt(const AName: String; AIndex: Integer): String;
    procedure GetImageStreamPPI(const AName: String; out BS: TStream);
    function GetImageStream(const AName: String; AIndex: Integer; out BS: TStream): Boolean;
    function SetImage(const AName: String; AIndex: Integer; const AFileName: String): Boolean;
    function SetImageStream(const AName: String; AIndex: Integer; St: TStream): Boolean;
    procedure ClearImage(const AName: String; AIndex: Integer);
    function MakeUniqueImageName(const AnyName: String): String;
    property DataSet: TSQLQuery read FDataSet;
  end;

var
  ImageMan: TImageManager;
  ImageCache: TImageCache;

implementation

uses
  FileUtil, LazFileUtils, appimagelists, apputils;

function ImgFormatToExt(F: TBGRAImageFormat): String;
const
  Exts: array [TBGRAImageFormat] of String = ('', 'jpg', 'png', 'gif', 'bmp',
    'bmp', 'ico', '', 'pcx', '', '', '', '', 'psd', 'tga', 'tif', 'xwd', '', '',
    '', '', '');
begin
  Result := Exts[F];
end;

{ TImageCache }

function TImageCache.GetImages(Index: Integer): PImageCacheData;
begin
  Result := PImageCacheData(Items[Index]);
end;

function TImageCache.AddImage(const ImageName: String; Bitmap: TBitmap
  ): PImageCacheData;
var
  pImg: PImageCacheData;
  IL: TImageList;
begin
  Result := nil;
  if (Bitmap.Width = 16) and (Bitmap.Height = 16) then
    IL := Images_16
  else if (Bitmap.Width = 24) and (Bitmap.Height = 24) then
    IL := Images_24
  else if (Bitmap.Width = 32) and (Bitmap.Height = 32) then
    IL := Images_32
  else if (Bitmap.Width = 36) and (Bitmap.Height = 36) then
    IL := Images_36
  else if (Bitmap.Width = 48) and (Bitmap.Height = 48) then
    IL := Images_48
  else
    Exit;

  New(pImg);
  pImg^.ImageName := ImageName;
  pImg^.ImageList := IL;
  pImg^.ImageIndex := IL.Add(Bitmap, nil);
  Add(pImg);
  Result := pImg;
end;

function TImageCache.FindImage(const ImageName: String): PImageCacheData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Utf8CompareText(ImageName, Images[i]^.ImageName) = 0 then
      Exit(Images[i]);
  end;
end;

procedure TImageCache.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(PImageCacheData(Items[i]));
  inherited Clear;
end;

procedure TImageCache.DeleteImage(const ImageName: String);
var
  pImg: PImageCacheData;
begin
  pImg := FindImage(ImageName);
  if pImg <> nil then
  begin
    pImg^.ImageList.Replace(pImg^.ImageIndex, nil, nil);
    Dispose(pImg);
    Remove(pImg);
  end;
end;

procedure TImageCache.RenameImage(const OldName, NewName: String);
var
  pImg: PImageCacheData;
begin
  pImg := FindImage(OldName);
  if pImg <> nil then pImg^.ImageName := NewName;
end;

{ TImageManager }

function TImageManager.GetMaxId: Integer;
begin
  FDataSet.First;
  Result := 0;
  while not FDataSet.Eof do
  begin
    if FDataSet.Fields[0].AsInteger > Result then
      Result := FDataSet.Fields[0].AsInteger;
    FDataSet.Next;
  end;
end;

constructor TImageManager.Create;
begin
  FDataSet := TSQLQuery.Create(nil);
  DBase.AttachDataSet(FDataSet);
  FDataSet.Options := [sqoKeepOpenOnCommit];
end;

destructor TImageManager.Destroy;
begin
  FreeAndNil(FDataSet);
  inherited Destroy;
end;

procedure TImageManager.CreateTable;
var
  S: String;
begin
  S := 'CREATE TABLE DX_IMAGES (ID INTEGER, NAME VARCHAR(50), ' +
    'IMG_100 BLOB SUB_TYPE 0 SEGMENT SIZE 512, ' +
    'IMG_150 BLOB SUB_TYPE 0 SEGMENT SIZE 512, ' +
    'IMG_200 BLOB SUB_TYPE 0 SEGMENT SIZE 512);';
  DBase.Execute(S);
end;

procedure TImageManager.LoadFromDb;
begin
  FDataSet.Close;
  FDataSet.SQL.Text := 'select id, name, img_100, img_150, img_200, lastmodified from dx_images';
  FDataSet.Open;
  FDataSet.Fields[0].ProviderFlags:=[pfInWhere, pfInKey, pfInUpdate];
  FMaxId := GetMaxId;
end;

procedure TImageManager.SaveToDb;
begin
  DBase.ApplyDataSet(FDataSet);
end;

// Используется только при объединении проектов (часть функционала уже не нужна,
// но пока оставил)
procedure TImageManager.LoadFromDir(const Dir: String);

  procedure _Load(const ImgDir: String; idx: Integer; OnlyAdd: Boolean);
  var
    SL: TStringList;
    i: Integer;
    S, ImgNm: String;
    FS: TFileStream;
    St: TStream;
  begin
    SL := TStringList.Create;
    FindAllFiles(SL, ImgDir, '*');
    for i := 0 to SL.Count - 1 do
    begin
      S := SL[i];
      ImgNm := ExtractFileNameOnly(S);
      FS := TFileStream.Create(S, fmOpenRead + fmShareDenyNone);

      if OnlyAdd then
        FDataSet.Append
      else
      begin
        if FDataSet.Locate('name', ImgNm, [loCaseInsensitive]) then
          FDataSet.Edit
        else
          FDataSet.Append;
      end;

      if FDataSet.State = dsInsert then
      begin
        Inc(FMaxId);
        FDataSet.Fields[0].AsInteger := FMaxId;
        FDataSet.Fields[1].AsString := ImgNm;
        FDataSet.Fields[5].AsDateTime := GetFileDateTime(SL[i]);
      end;

      St := FDataSet.CreateBlobStream(FDataSet.Fields[idx + 2], bmWrite);
      St.CopyFrom(FS, FS.Size);
      FDataSet.Post;
      St.Free;
      FS.Free;
    end;
    SL.Free;
  end;

var
  ImgDir: String;
begin
  if not FDataSet.Active then
  begin
    FDataSet.SQL.Text := 'select id, name, img_100, img_150, img_200, lastmodified from dx_images where id=0';
    FDataSet.Open;
    FDataSet.Fields[0].ProviderFlags:=[pfInWhere, pfInKey, pfInUpdate];
  end;
  FDataSet.First;
  while not FDataSet.Eof do
    FDataSet.Delete;
  FMaxId := 0;
  ImgDir := Dir + 'images' + PathDelim;
  _Load(ImgDir + '100', 0, True);
  _Load(ImgDir + '150', 1, False);
  _Load(ImgDir + '200', 2, False);

  // FDataSet.MergeChangeLog; Вот это я забыл добавить, когда метод использовался
  // для загрузки картинок из кэша.
end;

procedure TImageManager.SaveToDir(const Dir: String);
var
  Dir100, Dir150, Dir200, Nm, FlNm: String;
  St: TStream;
  FS: TFileStream;
  LastModified: TDateTime;
begin
  Dir100 := Dir + 'images' + PathDelim + '100' + PathDelim;
  Dir150 := Dir + 'images' + PathDelim + '150' + PathDelim;
  Dir200 := Dir + 'images' + PathDelim + '200' + PathDelim;
  ForceDirectories(Dir100);
  ForceDirectories(Dir150);
  ForceDirectories(Dir200);
  FDataSet.First;
  while not FDataSet.Eof do
  begin
    Nm := FDataSet.Fields[1].AsString;
    LastModified := FDataSet.Fields[5].AsDateTime;

    // 100
    St := FDataSet.CreateBlobStream(FDataSet.Fields[2], bmRead);
    if St <> nil then
    begin
      // To do: Есть функция SuggestImageExtension
      FlNm := Dir100 + Nm + '.' + ImgFormatToExt(DetectFileFormat(St));
      FS := TFileStream.Create(FlNm, fmCreate);
      FS.CopyFrom(St, St.Size);
      //SetFileDateTime(FS.Handle, LastModified);
      FS.Free;
      St.Free;
      SetFileDateTime(FlNm, LastModified);
    end;
    // 150
    St := FDataSet.CreateBlobStream(FDataSet.Fields[3], bmRead);
    if St <> nil then
    begin
      FlNm := Dir150 + Nm + '.' + ImgFormatToExt(DetectFileFormat(St));
      FS := TFileStream.Create(FlNm, fmCreate);
      FS.CopyFrom(St, St.Size);
      //SetFileDateTime(FS.Handle, LastModified);
      FS.Free;
      St.Free;
      SetFileDateTime(FlNm, LastModified);
    end;
    // 200
    St := FDataSet.CreateBlobStream(FDataSet.Fields[4], bmRead);
    if St <> nil then
    begin
      FlNm := Dir200 + Nm + '.' + ImgFormatToExt(DetectFileFormat(St));
      FS := TFileStream.Create(FlNm, fmCreate);
      FS.CopyFrom(St, St.Size);
      //SetFileDateTime(FS.Handle, LastModified);
      FS.Free;
      St.Free;
      SetFileDateTime(FlNm, LastModified);
    end;
    FDataSet.Next;
  end;
end;

procedure ExtractImageFiles(SL: TStringList; out Img100, Img150, Img200, Img, ImgName: String);
var
  i: Integer;
  Tmp, S: String;
begin
  Img := SL[0];
  ImgName := ExtractFileNameOnly(Img);

  for i := SL.Count - 1 downto 0 do
  begin
    S := SL[i];
    if ExtractFileNameOnly(S) = ImgName then
    begin
      Tmp := RightStr(ExcludeTrailingPathDelimiter(ExtractFilePath(S)), 3);
      if Tmp = '100' then Img100 := S
      else if Tmp = '150' then Img150 := S
      else Img200 := S;
      SL.Delete(i);
    end;
  end;
end;

type
  TImgInfo = record
    Img100, Img150, Img200, Img, ImgName: String;
    Id: Integer;
  end;

procedure TImageManager.LoadFromCache(const Dir: String);
var
  SL: TStringList;
  Img100, Img150, Img200, Img, ImgNm, Ids: String;
  DS: TSQLQuery;
  ImgInfo: array of TImgInfo;
  n, i: Integer;
  Inf: TImgInfo;
begin
  SL := TStringList.Create;
  DS := DBase.OpenDataSet('select id, name, lastmodified from dx_images');

  try
    FindAllFiles(SL, Dir + 'images', GetAllFilesMask, True);

    SetLength(ImgInfo, SL.Count);
    n := 0;
    while SL.Count > 0 do
    begin
      ExtractImageFiles(SL, Img100, Img150, Img200, Img, ImgNm);
      if not DS.Locate('name', ImgNm, []) then
      begin
        DeleteFile(Img100);
        DeleteFile(Img150);
        DeleteFile(Img200);
      end
      else
      begin
        if SameFileDateTime(Img, DS.Fields[2].AsDateTime) then
        begin
          ImgInfo[n].Img100 := Img100;
          ImgInfo[n].Img150 := Img150;
          ImgInfo[n].Img200 := Img200;
          ImgInfo[n].Img := Img;
          ImgInfo[n].ImgName := ImgNm;
          ImgInfo[n].Id := DS.Fields[0].AsInteger;
          Inc(n);

          // Эти не грузим из базы
          DS.Edit;
          DS.Fields[0].SetData(nil);
          DS.Post;
        end;
      end;
    end;
    SetLength(ImgInfo, n);

    // Грузим из базы только обновленные и новые картинки
    Ids := '';
    DS.First;
    while not DS.Eof do
    begin
      if not DS.Fields[0].IsNull then Ids := Ids + DS.Fields[0].AsString + ',';
      DS.Next;
    end;
    SetLength(Ids, Length(Ids) - 1);
    if Ids = '' then Ids := '0';

  finally
    DS.Free;
    SL.Free;
  end;

  FDataSet.Close;

  FDataSet.SQL.Text := 'select id, name, img_100, img_150, img_200, ' +
    'lastmodified from dx_images where id in (' + Ids + ')'; ;
  FDataSet.Open;
  FDataSet.Fields[0].ProviderFlags:=[pfInWhere, pfInKey, pfInUpdate];

  SaveToDir(Dir);

  for i := 0 to High(ImgInfo) do
  begin
    Inf := ImgInfo[i];
    FDataSet.Append;
    FDataSet.Fields[0].AsInteger := Inf.Id;
    FDataSet.Fields[1].AsString := Inf.ImgName;
    if Inf.Img100 <> '' then
      TBlobField(FDataSet.Fields[2]).LoadFromFile(Inf.Img100);
    if Inf.Img150 <> '' then
      TBlobField(FDataSet.Fields[3]).LoadFromFile(Inf.Img150);
    if Inf.Img200 <> '' then
      TBlobField(FDataSet.Fields[4]).LoadFromFile(Inf.Img200);
    FDataSet.Fields[5].AsDateTime := GetFileDateTime(Inf.Img);
    FDataSet.Post;
  end;
  FDataSet.MergeChangeLog;

  FMaxId := GetMaxId;
  SetLength(ImgInfo, 0);
end;

procedure TImageManager.GetNames(SL: TStrings);
begin
  SL.Clear;
  FDataSet.First;
  while not FDataSet.Eof do
  begin
    SL.Add(FDataSet.Fields[1].AsString);
    FDataSet.Next;
  end;
end;

procedure TImageManager.AddImage(const AName: String);
begin
  FDataSet.Insert;
  Inc(FMaxId);
  FDataSet.Fields[0].AsInteger := FMaxId;
  FDataSet.Fields[1].AsString := AName;
  FDataSet.Fields[5].AsDateTime := Now;
  FDataSet.Post;
end;

procedure TImageManager.RenameImage(const OldName, NewName: String);
begin
  if FDataSet.Locate('name', OldName, [loCaseInsensitive]) then
  begin
    FDataSet.Edit;
    FDataSet.Fields[1].AsString := NewName;
    FDataSet.Fields[5].AsDateTime := Now;
    FDataSet.Post;
  end;
end;

procedure TImageManager.DeleteImage(const AName: String);
begin
  if FDataSet.Locate('name', AName, [loCaseInsensitive]) then
    FDataSet.Delete;
end;

function TImageManager.ImageExists(const AName: String): Boolean;
begin
  Result := FDataSet.Locate('name', AName, [loCaseInsensitive]);
end;

function TImageManager.GetImageExt(const AName: String; AIndex: Integer
  ): String;
var
  St: TStream;
begin
  GetImageStream(AName, AIndex, St);
  if St <> nil then
  begin
    Result := ImgFormatToExt(DetectFileFormat(St));
    St.Free;
  end
  else
    Result := '';
end;

procedure TImageManager.GetImageStreamPPI(const AName: String; out BS: TStream);
var
  i: Integer;
begin
  BS := nil;
  i := GetPPIndex;
  while i >= 0 do
  begin
    GetImageStream(AName, i, BS);
    if BS <> nil then Exit;
    Dec(i);
  end;
end;

function TImageManager.GetImageStream(const AName: String; AIndex: Integer; out
  BS: TStream): Boolean;
begin
  BS := nil;
  Result := FDataSet.Locate('name', AName, [loCaseInsensitive]);
  if not Result then Exit;
  BS := FDataSet.CreateBlobStream(FDataSet.Fields[AIndex + 2], bmRead);
end;

function TImageManager.SetImage(const AName: String; AIndex: Integer;
  const AFileName: String): Boolean;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  try
    Result := SetImageStream(AName, AIndex, FS);
  finally
    FS.Free;
  end;
end;

function TImageManager.SetImageStream(const AName: String; AIndex: Integer;
  St: TStream): Boolean;
var
  BS: TStream;
begin
  Result := FDataSet.Locate('name', AName, [loCaseInsensitive]);
  if not Result then Exit;

  FDataSet.Edit;
  BS := FDataSet.CreateBlobStream(FDataSet.Fields[AIndex + 2], bmWrite);
  try
    BS.CopyFrom(St, St.Size);
    FDataSet.Fields[5].AsDateTime := Now;
    FDataSet.Post;
  finally
    BS.Free;
  end;
end;

procedure TImageManager.ClearImage(const AName: String; AIndex: Integer);
begin
  if FDataSet.Locate('name', AName, [loCaseInsensitive]) then
  begin
    FDataSet.Edit;
    FDataSet.Fields[AIndex + 2].SetData(nil);
    FDataSet.Fields[5].AsDateTime := Now;
    FDataSet.Post;
  end;
end;

function TImageManager.MakeUniqueImageName(const AnyName: String): String;
var
  S: String;
  n: Integer;
begin
  SplitComponentName(AnyName, S, n);
  Inc(n);
  while ImageExists(S + IntToStr(n)) do
    Inc(n);
  Result := S + IntToStr(n);
end;

end.

