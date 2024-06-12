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

unit XmlReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Db, dxctrls, ExtCtrls, dialogs, expressions, strconsts,
  DXReports, datasetprocessor, mytypes;

const
  ttEof = #0;
  ttTag = #1;
  ttField = #2;
  ttBand = #3;
  ttComment = #4;

type

  { TXmlParser }

  TXmlParser = class
  private
    FStream: TStream;
    FToken: String;
    Ch: Char;
    procedure ReadChar;
    function Eof: Boolean;
    function TryReadStr(Count: Integer): String;
    //procedure SkipWhites;
  public
    function GetToken: Char;
    function GetTagName: String;
    function GetBandType: String;
    procedure ParseField(out FieldName, Tags: String);
    procedure ParseBand(out BandKind, BandName, Tags: String);
    property InputStream: TStream read FStream write FStream;
    property Token: String read FToken;
  end;

  PGroupRec = ^TGroupRec;
  TGroupRec = record
    FieldName, Value: String;
    GroupPos, Pos: Integer;
  end;

  PDataRec = ^TDataRec;
  TDataRec = record
    Form: TdxForm;
    RD: TReportData;
    QGrid: TdxQueryGrid;      // для поиска поля
    DataSet: TDataSet;
    Id: Integer;
    BandKind: String;
    BandName: String;
    Pos: Integer;
    //AfterScroll: TDataSetNotifyEvent;
    ExprList: TStringListUtf8;
    Parent: PDataRec;
    Groups: TList;
    Script: Boolean;					// Данные формируются в скрипте.
  end;

  { TXmlReport }

  TXmlReport = class
  private
    FAltImageSizeTag: String;
    FBrTag: String;
    FExtraTag: Boolean;
    FImageFiles: TStringList;
    FImageHeightStr: String;
    FImagesFolder: String;
    FImageSizeFloat: Boolean;
    FImageSizeTag: String;
    FImageTag: String;
    FImageTagSrc: String;
    FImageTagSrcPrefix: String;
    FImageWidthStr: String;
    FInputStream: TStream;
    FOldImageTagSrc: String;
    FOutputStream: TStream;
    FParaTag: String;
    FPars: TXmlParser;
    FDestFolder: String;
    FRowTag: String;
    FData: TList;
    FImageId: Integer;
    FTemplateName: String;
    //FTranslitFileNames: Boolean;
    FWriteImageId, FIsImageFound, FIsImageSize, FIsAltImageSize: Boolean;
    FImageFileName: String;
    FImgSize: TPoint;
    FErrors: String;
    procedure ClearData;
    procedure BuildExprs(pD: PDataRec);
    procedure AddDataRec(Fm: TdxForm; DS: TDataSet);
    procedure AddDataRecsQ(pD: PDataRec);
    procedure AddDataRecQ(QG: TdxQueryGrid; aParent: PDataRec);
    function GetData(i: Integer): PDataRec;
    function FindDataByFormCaption(const S: String): PDataRec;
    function LookupFieldValue(Form: TdxForm; const FieldName: String; DataSet: TDataSet): String;
    procedure AddError(const S: String);
    function DoCalcField(pD: PDataRec; const FldNm: String; var V: Variant): Boolean;
    function LookupQueryFieldValue(D: TDataRec; const FieldName: String): String;
    function FindGroup(D: TDataRec; const S: String): PGroupRec;
    procedure CheckGroupPos(D: TDataRec; P: Integer);
    procedure CheckBandPos(pCurD: PDataRec; P: Integer; const BnNm, BnKd: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure BindForm(DSP: TDataSetProcessor);
    procedure UnBind;
    procedure Execute;
    function GetImageId: Integer;
    property TemplateName: String read FTemplateName write FTemplateName;
    property InputStream: TStream read FInputStream write FInputStream;
    property OutputStream: TStream read FOutputStream write FOutputStream;
    property RowTag: String read FRowTag write FRowTag;
    property ParaTag: String read FParaTag write FParaTag;
    property BrTag: String read FBrTag write FBrTag;
    property ImageTag: String read FImageTag write FImageTag;
    property ImageTagSrc: String read FImageTagSrc write FImageTagSrc;
    property ImageTagSrcPrefix: String read FImageTagSrcPrefix write FImageTagSrcPrefix;
    property OldImageTagSrc: String read FOldImageTagSrc write FOldImageTagSrc;
    property ImageSizeTag: String read FImageSizeTag write FImageSizeTag;
    property AltImageSizeTag: String read FAltImageSizeTag write FAltImageSizeTag;
    property ImageWidthStr: String read FImageWidthStr write FImageWidthStr;
    property ImageHeightStr: String read FImageHeightStr write FImageHeightStr;
    property ImageSizeFloat: Boolean read FImageSizeFloat write FImageSizeFloat;
    property WriteImageId: Boolean read FWriteImageId write FWriteImageId;
    //property TranslitFileNames: Boolean read FTranslitFileNames write FTranslitFileNames;
    property DestFolder: String read FDestFolder write FDestFolder;
    property ImagesFolder: String read FImagesFolder write FImagesFolder;
    property ImageFiles: TStringList read FImageFiles;
    property Errors: String read FErrors;
    property ExtraTag: Boolean read FExtraTag write FExtraTag;
  end;

procedure ReportToWordXML(DSP: TDataSetProcessor; const AFileName, OutName: String; out Errs: String);
procedure ReportToDocX(DSP: TDataSetProcessor; const AFileName, OutName: String; out Errs: String);
procedure ReportToOdt(DSP: TDataSetProcessor; const AFileName, OutName: String; out Errs: String);
procedure ReportToHTML(DSP: TDataSetProcessor; const AFileName, OutName: String; out Errs: String);
procedure ReportToOds(DSP: TDataSetProcessor; const AFileName, OutName: String; out Errs: String);
procedure ReportToXXX(DSP: TDataSetProcessor; const AFileName: String; var OutName: String; out Errs: String; AOpenFile, ChangeOutName: Boolean);

implementation

uses
  LazUtf8, formmanager, apputils, sqlgen, dbengine, appsettings, zipper,
  fileutil, StrUtils, dximages, dxfiles, Forms,
  Variants, reportmanager, DateUtils, DOM, XmlRead, XmlWrite, pivotgrid, base64,
  Math, dxcharts;

function ImageFileToBase64(const FileName: String): String;
var
  i, n: Integer;
  S: String;
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
  SetLength(S, FS.Size);
  FS.Read(Pointer(S)^, FS.Size);
  FS.Free;
  S := EncodeStringBase64(S);
  Result := '';
  n := 1;
  for i := 1 to Length(S) do
  begin
    if n = 77 then
    begin
      Result := Result + #13#10;
      n := 1;
    end;
    Result := Result + S[i];
    Inc(n);
  end;
end;

function GetMimeType(const FileName: String): String;
var
  S: String;
begin
  S := ExtractFileExt(LowerCase(FileName));
  if (S = '.jpg') or (S = '.jpeg') then Result := 'image/jpeg'
  else if S = '.png' then Result := 'image/png'
  else if (S = '.tif') or (S = '.tiff') then Result := 'image/tiff'
  else S := 'image/' + Copy(S, 2, 255);
end;

function GetExtraTag(const dt: String; pr: Integer): String;
begin
  Result := '@@@' + dt + IntToStr(pr) + ';'
end;

function MyCopyFile(const SrcFilename, DestFilename: String): Boolean;
var
  SrcHandle: THandle;
  DestHandle: THandle;
  Buffer: array[1..4096] of byte;
  ReadCount, WriteCount, TryCount: LongInt;
begin
  Result := False;
  TryCount := 0;
  While TryCount <> 3 Do Begin
    SrcHandle := FileOpen(SrcFilename, fmOpenRead + fmShareDenyNone);
    if (THandle(SrcHandle)=feInvalidHandle) then Begin
      Inc(TryCount);
      Sleep(10);
    End
    Else Begin
      TryCount := 0;
      Break;
    End;
  End;
  If TryCount > 0 Then
    raise EFOpenError.Createfmt({SFOpenError}'Unable to open file "%s"', [SrcFilename]);
  try
    DestHandle := FileCreate(DestFileName);
    if (THandle(DestHandle)=feInvalidHandle) then
      raise EFCreateError.createfmt({SFCreateError}'Unable to create file "%s"',[DestFileName]);
    try
      repeat
        ReadCount:=FileRead(SrcHandle,Buffer[1],High(Buffer));
        if ReadCount<=0 then break;
        WriteCount:=FileWrite(DestHandle,Buffer[1],ReadCount);
        if WriteCount<ReadCount then
          raise EWriteError.createfmt({SFCreateError}'Unable to write to file "%s"',[DestFileName])
      until false;
    finally
      FileClose(DestHandle);
    end;
    Result := True;
  finally
    FileClose(SrcHandle);
  end;
end;

procedure OpenReportFile(FileName: String);
var
  Ext, App: String;
begin
  App := '';
  Ext := AnsiLowerCase(ExtractFileExt(FileName));
  if Ext = '.xml' then
    App := AppConfig.AppXmlFile
  else if (Ext = '.docx') or (Ext = '.docm') then
    App := AppConfig.AppDocXFile
  else if Ext = '.odt' then
    App := AppConfig.AppOdtFile
  else if Ext = '.ods' then
    App := AppConfig.AppOdsFile
  else if Ext = '.html' then
    App := AppConfig.AppHtmlFile;
  if App <> '' then
  begin
    ShellExec('open', App, QuoteStr(FileName), '', 1)
  end
  else
    ShellExec('open', FileName, '', '',1);
end;

//<pkg:part pkg:name="/word/_rels/document.xml.rels"

procedure ChangeWordXml(const FileName: String; ImageFiles: TStrings);
var
  SL: TStringList;
  S: String;
  i, p: SizeInt;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(FileName, True);
  S := SL.Text;
  p := Pos('document.xml.rels', S);
  if p > 0 then
  begin
    p := PosEx('</Relationships>', S, p);
    if p > 0 then
    begin
      for i := 0 to ImageFiles.Count - 1 do
      begin
        Insert('<Relationship Id="rId' + IntToStr(1000 + i) +
          '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" ' +
          'Target="images/' + ExtractFileName(ImageFiles[i]) + '"/>', S, p);
      end;

      p := Pos('<pkg:part', S);
      if p > 0 then
        for i := 0 to ImageFiles.Count - 1 do
        begin
          Insert('<pkg:part pkg:contentType="' + GetMimeType(ImageFiles[i]) +
            '" pkg:name="/word/images/' + ExtractFileName(ImageFiles[i]) +
            '" pkg:compression="store"><pkg:binaryData>' +
            ImageFileToBase64(ImageFiles[i]) + '</pkg:binaryData></pkg:part>', S, p);
        end;
    end;
  end;
  SL.Text := S;
  SL.SaveToFile(FileName, True);
  SL.Free;
end;

//<w:binData xml:space="preserve" w:name="wordml://03000001.png">
procedure ChangeWordXml2003(const FileName: String; ImageFiles: TStrings);
var
  SL: TStringList;
  S: String;
  i, p: SizeInt;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(FileName, True);
  S := SL.Text;
  p := Pos('<w:body', S);
  if p > 0 then
  begin
    for i := 0 to ImageFiles.Count - 1 do
    begin
      Insert('<w:binData xml:space="preserve" w:name="wordml://images/' +
        ExtractFileName(ImageFiles[i]) + '">' +
        ImageFileToBase64(ImageFiles[i]) + '</w:binData>', S, p);
    end;
  end;
  SL.Text := S;
  SL.SaveToFile(FileName, True);
  SL.Free;
end;

function IsWordXml2003(const FileName:String): Boolean;
var
  FS: TFileStream;
  Tk: Char;
begin
  FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
  with TXmlParser.Create do
  try
    InputStream := FS;
    Tk := GetToken;
    while Tk <> ttEof do
    begin
      if Tk = ttTag then
      begin
        if CompareText(GetTagName, 'w:worddocument') = 0 then Exit(True)
        else if CompareText(GetTagName, 'pkg:package') = 0 then Exit(False);
      end;
      Tk := GetToken;
    end;
  finally
    Free;
    FS.Free;
  end;
end;

procedure ReportToWordXML(DSP: TDataSetProcessor; const AFileName,
  OutName: String; out Errs: String);
var
  Inp, Outp: TFileStream;
  MS: TMemoryStream;
  xml2003: Boolean;
begin
  xml2003 := IsWordXml2003(AFileName);
  Inp := nil; Outp := nil;
  MS := nil;
  with TXmlReport.Create do
  try
    RowTag:='w:tr';
    ParaTag := 'w:p';
    BrTag := '</w:t></w:r><w:r><w:br/><w:t>';
    if xml2003 then
    begin
      ImageTag := 'v:imagedata';
      ImageTagSrc := 'src';
      ImageTagSrcPrefix := 'wordml://images/';
      ImageSizeTag := 'v:shape';
      ImageWidthStr := 'width:';
      ImageHeightStr := 'height:';
    end
    else begin
      ImageTag := 'a:blip';
      ImageTagSrc := 'r:embed';
      OldImageTagSrc := 'r:link';
      ImageSizeTag := 'wp:extent';
      AltImageSizeTag := 'a:ext';
      ImageWidthStr := 'cx="';
      ImageHeightStr := 'cy="';
      WriteImageId := True;
      //TranslitFileNames := True;
    end;
    DestFolder := GetOutputDir;
    ImagesFolder := DestFolder + 'images' + DirectorySeparator;
    Inp := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
    MS := TMemoryStream.Create;
    MS.CopyFrom(Inp, Inp.Size);
    MS.Position := 0;
    Outp := TFileStream.Create(OutName, fmCreate);
    InputStream := MS;
    OutputStream := Outp;
    BindForm(DSP);
    Execute;
    Errs := Errors;
    FreeAndNil(Outp);
    if ImageFiles.Count > 0 then
    begin
      if not xml2003 then
        ChangeWordXml(OutName, ImageFiles)
      else
        ChangeWordXml2003(OutName, ImageFiles);
      DeleteDirectory(ImagesFolder, False);
    end;
  finally
    Unbind;
    FreeAndNil(MS);
    FreeAndNil(Outp);
    FreeAndNil(Inp);
    Free;
  end;
end;

procedure ChangeDocXRels(const FileName: String; ImageFiles: TStrings);
var
  SL: TStringList;
  S: String;
  i, p: SizeInt;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(FileName, True);
  S := SL.Text;

  p := Pos('</Relationships>', S);
  if p > 0 then
    for i := 0 to ImageFiles.Count - 1 do
    begin
      Insert('<Relationship Id="rId' + IntToStr(1000 + i) +
        '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" ' +
        'Target="images/' + ExtractFileName(ImageFiles[i]) + '"/>', S, p);
    end;

  SL.Text := S;
  SL.SaveToFile(FileName, True);
  SL.Free;
end;

procedure ChangeContentTypes(const FileName: String; ImageFiles: TStrings);
var
  SL, Exts: TStringList;
  S, Ext: String;
  p, p0, i: Integer;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(FileName, True);
  S := SL.Text;
  Exts := TStringList.Create;
  Exts.Duplicates:=dupIgnore;
  for i := 0 to ImageFiles.Count - 1 do
  begin
    Ext := ExtractFileExt(ImageFiles[i]);
    Delete(Ext, 1, 1);
    Exts.Add(LowerCase(Ext));
  end;

  p0 := Pos('<Default Extension', S);
  for i := 0 to Exts.Count - 1 do
  begin
    Ext := Exts[i];
    p := Pos('<Default Extension="' + Ext + '"', S);
    if p = 0 then
      Insert(Format('<Default Extension="%0:s" ContentType="image/%0:s"/>', [Ext]), S, p0);
  end;

  SL.Text := S;
  SL.SaveToFile(FileName, True);
  SL.Free;
  Exts.Free;
end;

procedure ReportToDocX(DSP: TDataSetProcessor; const AFileName,
  OutName: String; out Errs: String);
var
  OutDir, TempDir, WordFile, TempFile, InpFile, S: String;
  Inp, Outp: TFileStream;
  SL: TStringList;
  i: Integer;
  MS: TMemoryStream;
begin
  OutDir := GetOutputDir;
  TempDir := OutDir + 'temp' + IntToStr(Random(100000000)) + DirectorySeparator;
  WordFile := TempDir + 'word' + DirectorySeparator + 'document.xml';
  TempFile := TempDir + 'word' + DirectorySeparator + 'temp.xml';
  InpFile := OutDir + ExtractFileName(aFileName) + '.tmp';

  MyCopyFile(aFileName, InpFile);

  with TUnZipper.Create do
  try
    FileName:=InpFile;
    OutputPath:=TempDir;
    UnZipAllFiles;
  finally
    Free;
  end;

  Inp := nil; Outp := nil; MS := nil;

  with TXmlReport.Create do
  try
    Inp := TFileStream.Create(WordFile, fmOpenRead + fmShareDenyNone);
    MS := TMemoryStream.Create;
    MS.CopyFrom(Inp, Inp.Size);
    MS.Position := 0;
    Outp := TFileStream.Create(TempFile, fmCreate);
    InputStream := MS;
    OutputStream := Outp;
    RowTag:='w:tr';
    ParaTag := 'w:p';
    BrTag := '</w:t></w:r><w:r><w:br/><w:t>';
    ImageTag := 'a:blip';
    ImageTagSrc := 'r:embed';
    OldImageTagSrc := 'r:link';
    ImageSizeTag := 'wp:extent';
    AltImageSizeTag := 'a:ext';
    ImageWidthStr := 'cx="';
    ImageHeightStr := 'cy="';
    WriteImageId := True;
    //TranslitFileNames := True;
    DestFolder := OutDir;
    ImagesFolder := TempDir + 'word' + DirectorySeparator + 'images' + DirectorySeparator;
    BindForm(DSP);
    Execute;
    Errs := Errors;
    if ImageFiles.Count > 0 then
    begin
      ChangeDocXRels(TempDir + 'word' + DirectorySeparator + '_rels' + DirectorySeparator + 'document.xml.rels',
        ImageFiles);
      ChangeContentTypes(TempDir + '[Content_Types].xml', ImageFiles);
    end;
  finally
    UnBind;
    FreeAndNil(MS);
    FreeAndNil(Outp);
    FreeAndNil(Inp);
    Free;
  end;

  DeleteFile(WordFile);
  RenameFile(TempFile, WordFile);

  SL := FindAllFiles(TempDir, '*', True);
  with TZipper.Create do
  try
    for i := 0 to SL.Count - 1 do
    begin
      S := Copy(SL[i], Length(TempDir) + 1, 1024);
      S := StringReplace(S, '\', '/', [rfReplaceAll]);
      Entries.AddFileEntry(SL[i], S);
    end;
    FileName:=OutName;
    ZipAllFiles;
  finally
    SL.Free;
    Free;
  end;

  DeleteDirectory(TempDir, False);
  DeleteFile(InpFile);
end;

procedure ChangeManifest(const FileName: String; ImageFiles: TStrings);
var
  SL: TStringList;
  Text, FlNm: String;
  p: SizeInt;
  i: Integer;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(FileName, True);
  Text := SL.Text;

  p := Pos('</manifest:manifest>', Text);
  for i := 0 to ImageFiles.Count - 1 do
  begin
    FlNm := ImageFiles[i];
    Insert('<manifest:file-entry manifest:full-path="images/' +
      ExtractFileName(FlNm) + '" manifest:media-type="' +
      GetMimeType(FlNm) + '"/>', Text, p);
  end;

  SL.Text := Text;
  SL.SaveToFile(FileName, True);
  SL.Free;
end;

procedure ReportToOdt(DSP: TDataSetProcessor; const AFileName, OutName: String;
  out Errs: String);
var
  OutDir, TempDir, WordFile, TempFile, InpFile, S: String;
  Inp, Outp: TFileStream;
  SL: TStringList;
  i: Integer;
  MS: TMemoryStream;
begin
  OutDir := GetOutputDir;
  TempDir := OutDir + 'temp' + IntToStr(Random(100000000)) + DirectorySeparator;
  WordFile := TempDir + 'content.xml';
  TempFile := TempDir + 'temp.xml';
  InpFile := OutDir + ExtractFileName(aFileName) + '.tmp';

  MyCopyFile(aFileName, InpFile);

  with TUnZipper.Create do
  try
    FileName:=InpFile;
    OutputPath:=TempDir;
    UnZipAllFiles;
  finally
    Free;
  end;

  Inp := nil; Outp := nil; Ms := nil;
  with TXmlReport.Create do
  try
    Inp := TFileStream.Create(WordFile, fmOpenRead + fmShareDenyNone);
    MS := TMemoryStream.Create;
    MS.CopyFrom(Inp, Inp.Size);
    MS.Position := 0;
    Outp := TFileStream.Create(TempFile, fmCreate);
    InputStream := MS;
    OutputStream := Outp;
    RowTag:='table:table-row';
    ParaTag := 'text:p';
    BrTag := '<text:line-break />';
    ImageTag := 'draw:image';
    ImageTagSrc := 'xlink:href';
    ImageSizeTag := 'draw:frame';
    ImageWidthStr := 'svg:width="';
    ImageHeightStr := 'svg:height="';
    ImageSizeFloat := True;
    ImageTagSrcPrefix := 'images/';
    ImagesFolder := TempDir + 'images' + DirectorySeparator;
    DestFolder := OutDir;
    BindForm(DSP);
    Execute;
    Errs := Errors;
    if ImageFiles.Count > 0 then
      ChangeManifest(TempDir + 'META-INF' + DirectorySeparator + 'manifest.xml',
        ImageFiles);
  finally
    UnBind;
    FreeAndNil(MS);
    FreeAndNil(Outp);
    FreeAndNil(Inp);
    Free;
  end;

  DeleteFile(WordFile);
  RenameFile(TempFile, WordFile);

  SL := FindAllFiles(TempDir, '*', True);
  with TZipper.Create do
  try
    for i := 0 to SL.Count - 1 do
    begin
      S := Copy(SL[i], Length(TempDir) + 1, 1024);
      S := StringReplace(S, '\', '/', [rfReplaceAll]);
      Entries.AddFileEntry(SL[i], S);
    end;
    FileName:=OutName;
    ZipAllFiles;
  finally
    SL.Free;
    Free;
  end;

  DeleteDirectory(TempDir, False);
  DeleteFile(InpFile);
end;

procedure ReportToHTML(DSP: TDataSetProcessor; const AFileName,
  OutName: String; out Errs: String);
var
  Inp, Outp: TFileStream;
  MS: TMemoryStream;
begin
  Inp := nil; Outp := nil; MS := nil;
  with TXmlReport.Create do
  try
    RowTag:='tr';
    ParaTag := 'p';
    BrTag := '<br>';
    ImageTag := 'img';
    ImageTagSrc := 'src';
    ImageSizeTag := 'img';
    ImageWidthStr := 'width';
    ImageHeightStr := 'height';
    ImageTagSrcPrefix := 'images/';
    DestFolder := GetOutputDir;
    ImagesFolder := DestFolder + 'images' + DirectorySeparator;
    Inp := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
    MS := TMemoryStream.Create;
    MS.CopyFrom(Inp, Inp.Size);
    MS.Position := 0;
    Outp := TFileStream.Create(OutName, fmCreate);
    InputStream := MS;
    OutputStream := Outp;
    BindForm(DSP);
    Execute;
    Errs := Errors;
  finally
    Unbind;
    FreeAndNil(MS);
    FreeAndNil(Outp);
    FreeAndNil(Inp);
    Free;
  end;
end;

// Ищет экстра-теги и меняет тип данных в ячейке
procedure ProcessOds(aFileName: String);
var
  Xml: TXMLDocument;
  Styles: TDOMNode;
  SL: TStringList;
  Nodes: TDOMNodeList;

  // Если ODS создается в Excel, то он в последнюю строку записывает кол-во
  // повторений строки большое число раз. Из-за этого файлы не открываются в Экселе.
  // Устанавливает значение в 1.
  procedure ProcessLastRows;
  var
    L, LL: TDOMNodeList;
    i: Integer;
  begin
    L := Xml.GetElementsByTagName('table:table');
    for i := 0 to L.Length - 1 do
    begin
      LL := TDOMElement(L[i]).GetElementsByTagName('table:table-row');
      if LL.Count > 0 then
        TDOMElement(LL[LL.Count - 1]).SetAttribute('table:number-rows-repeated', '1');
      LL.Free;
    end;
    L.Free;
  end;

  function ExtractExtraTag(var S, dt: String; var Pr: Integer): Integer;
  var
    p, i, Len: Integer;
  begin
    Result := 0;
    i := 1; Len := Length(S);
    while i <= Len do
    begin
      if (S[i] = '@') and (Copy(S, i + 1, 2) = '@@') then
      begin
        Delete(S, i, 3);
        dt := Copy(S, i, 1);
        Delete(S, i, 1);
        p := PosEx(';', S, i);
        Len := Len - 4;
        if p > 0 then
        begin
          Pr := StrToInt(Copy(S, i, p - i));
          Delete(S, i, p - i + 1);
          Inc(Result);
          Len := Len - (p - i + 1);
        end;
      end
      else
        Inc(i);
    end;
  end;

  function CloneStyle(aName: String): TDOMElement;
  var
    i: Integer;
    N: TDOMNode;
  begin
    Result := nil;
    for i := 0 to Styles.ChildNodes.Length - 1 do
    begin
      N := Styles.ChildNodes[i];
      if (N.NodeName = 'style:style') and
        (TDOMElement(N).GetAttribute('style:name') = aName) then
        Exit(TDOMElement(N.CloneNode(True)));
    end;
  end;

  procedure Process(L: TDOMNodeList);
  var
    i, j, m, Pr: Integer;
    N, Txt: TDOMElement;
    S, StyleName, dt: String;
    St, Ch: TDOMElement;
    Dat: TDateTime;
    E: Extended;
    FS: TFormatSettings;
    PL, ChildNodes: TDOMNodeList;
    NumGrouping: Boolean;
    ts: Char;
  begin
    ts := DefaultFormatSettings.ThousandSeparator;
    for i := 0 to L.Count - 1 do
    begin
      if not (L[i] is TDOMElement) then Continue;
      N := TDOMElement(L[i]);
      if N.NodeName = 'office:automatic-styles' then
        Styles := N
      else if N.NodeName = 'table:table-cell' then
      begin
        dt := 's'; Pr := 0;
        PL := N.GetElementsByTagName('text:p');
        for j := 0 to PL.Count - 1 do
        begin
          Txt := TDOMElement(PL[j]);
          S := Txt.TextContent;
          m := ExtractExtraTag(S, dt, Pr);

          // Проверяем корректность определения типа данных. В ячейке кроме
          // данных может быть произвольный текст или несколько полей.
          if (m > 1) or ((dt = 'n') and
            not TryStrToFloat(StringReplace(S, ts, '', [rfReplaceAll]), E)) or
            ((dt = 'd') and not TryStrToDate(S, Dat)) or
            ((dt = 't') and not TryStrToTime(S, Dat)) then dt := 's';

          NumGrouping := (dt = 'n') and (Pos(ts, S) > 0);
          if NumGrouping then S := StringReplace(S, ts, '', [rfReplaceAll]);
          Txt.TextContent := S;
        end;

        if (m = 1) and (PL.Count = 1) then
        begin
          StyleName := N.GetAttribute('table:style-name');
          if StyleName = '' then StyleName := 'Default';
          S := StyleName + dt;
          if (dt = 'n') and NumGrouping then S := S + 'g';
          S := S + IntToStr(Pr);
          if SL.IndexOf(S) < 0 then
          begin
            SL.Add(S);

            // Стиль числа в ячейке
            if dt = 'n' then
            begin
              St := Xml.CreateElement('number:number-style');
              St.SetAttribute('style:name', 'N' + S);
              Ch := Xml.CreateElement('number:number');
              // Закомментировал, потому что в целых числах не группируются цифры
              //if Pr > 0 then
                Ch.SetAttribute('number:decimal-places', IntToStr(Pr));
              Ch.SetAttribute('number:min-integer-digits', '1');
              if NumGrouping then
                Ch.SetAttribute('number:grouping', 'true');
              St.AppendChild(Ch);
              Styles.AppendChild(St);
            end
            else if dt = 'd' then
            begin
              St := Xml.CreateElement('number:date-style');
              St.SetAttribute('style:name', 'N' + S);
              Ch := Xml.CreateElement('number:day');
              Ch.SetAttribute('number:style', 'long');
              St.AppendChild(Ch);
              Ch := Xml.CreateElement('number:text');
              Ch.TextContent:='.';
              St.AppendChild(Ch);
              Ch := Xml.CreateElement('number:month');
              Ch.SetAttribute('number:style', 'long');
              St.AppendChild(Ch);
              Ch := Xml.CreateElement('number:text');
              Ch.TextContent:='.';
              St.AppendChild(Ch);
              Ch := Xml.CreateElement('number:year');
              Ch.SetAttribute('number:style', 'long');
              St.AppendChild(Ch);
              Styles.AppendChild(St);
            end
            else if dt = 't' then
            begin
              St := Xml.CreateElement('number:time-style');
              St.SetAttribute('style:name', 'N' + S);
              Ch := Xml.CreateElement('number:hours');
              Ch.SetAttribute('number:style', 'long');
              St.AppendChild(Ch);
              if Pr > 0 then
              begin
                Ch := Xml.CreateElement('number:text');
                Ch.TextContent:=':';
                St.AppendChild(Ch);
                Ch := Xml.CreateElement('number:minutes');
                Ch.SetAttribute('number:style', 'long');
                St.AppendChild(Ch);
              end;
              if Pr > 1 then
              begin
                Ch := Xml.CreateElement('number:text');
                Ch.TextContent:=':';
                St.AppendChild(Ch);
                Ch := Xml.CreateElement('number:seconds');
                Ch.SetAttribute('number:style', 'long');
                St.AppendChild(Ch);
              end;
              Styles.AppendChild(St);
            end;

            // Стиль ячейки
            St := nil;
            if StyleName <> 'Default' then
              St := CloneStyle(StyleName);
            if St = nil then
              St := Xml.CreateElement('style:style');

            St.SetAttribute('style:name', S);
            if dt <> 's' then
              St.SetAttribute('style:data-style-name', 'N' + S);
            St.SetAttribute('style:family', 'table-cell');
            Styles.AppendChild(St);

          end;
          N.SetAttribute('table:style-name', S);
          if dt = 'n' then
          begin
            if TryStrToFloat(N.TextContent, E) then
            begin
              FS := DefaultFormatSettings;
              FS.DecimalSeparator:='.';
              S := FloatToStr(E, FS);
              N.SetAttribute('office:value', S);
              N.SetAttribute('office:value-type', 'float');
              N.RemoveAttribute('calcext:value-type');
            end;
          end
          else if dt = 'd' then
          begin
            if TryStrToDate(N.TextContent, Dat) then
            begin
              N.SetAttribute('office:date-value', Format('%s-%s-%sT00:00:00',
                [SetZeros(YearOf(Dat), 2), SetZeros(MonthOf(Dat), 2), SetZeros(DayOf(Dat), 2)]));
              N.SetAttribute('office:value-type', 'date');
              N.RemoveAttribute('calcext:value-type');
            end;
          end
          else if dt = 't' then
          begin
            if TryStrToTime(N.TextContent, Dat) then
            begin
              N.SetAttribute('office:time-value', Format('PT%dH%dM%dS',
                [HourOf(Dat), MinuteOf(Dat), SecondOf(Dat)]));
              N.SetAttribute('office:value-type', 'time');
              N.RemoveAttribute('calcext:value-type');
            end;
          end;
        end;
        PL.Free;
      end
      // При печати изображения привязаны к одной ячейке, что вызывает
      // искажение изображений. Убираем атрибут привязки, чтобы офис
      // мог сам вычислить реальное положение изображений.
      else if N.NodeName = 'draw:frame' then
        N.RemoveAttribute('table:end-cell-address');
      ChildNodes := N.ChildNodes;
      Process(ChildNodes);
      ChildNodes.Free;
    end;
  end;

begin
  SL := TStringList.Create;
  ReadXmlFile(Xml, aFileName);
  try
    Nodes := Xml.ChildNodes;
    Process(Nodes);
    Nodes.Free;
    ProcessLastRows;
    WriteXmlFile(Xml, aFileName);
  finally
    Xml.Free;
    SL.Free;
  end;
end;

procedure ReportToOds(DSP: TDataSetProcessor; const AFileName, OutName: String;
  out Errs: String);
var
  OutDir, TempDir, WordFile, TempFile, InpFile, S: String;
  Inp, Outp: TFileStream;
  SL: TStringList;
  i: Integer;
  MS: TMemoryStream;
begin
  OutDir := GetOutputDir;
  TempDir := OutDir + 'temp' + DirectorySeparator;
  WordFile := TempDir + 'content.xml';
  TempFile := TempDir + 'temp.xml';
  InpFile := OutDir + ExtractFileName(aFileName) + '.tmp';

  MyCopyFile(aFileName, InpFile);

  with TUnZipper.Create do
  try
    FileName:=InpFile;
    OutputPath:=TempDir;
    UnZipAllFiles;
  finally
    Free;
  end;

  Inp := nil; Outp := nil; MS := nil;
  with TXmlReport.Create do
  try
    ExtraTag:=True;
    Inp := TFileStream.Create(WordFile, fmOpenRead + fmShareDenyNone);
    MS := TMemoryStream.Create;
    MS.CopyFrom(Inp, Inp.Size);
    MS.Position := 0;
    Outp := TFileStream.Create(TempFile, fmCreate);
    InputStream := MS;
    OutputStream := Outp;
    RowTag:='table:table-row';
    ParaTag := 'text:p';
    BrTag := '</text:p><text:p>';
    ImageTag := 'draw:image';
    ImageTagSrc := 'xlink:href';
    ImageSizeTag := 'draw:frame';
    ImageWidthStr := 'svg:width="';
    ImageHeightStr := 'svg:height="';
    ImageTagSrcPrefix := 'images/';
    ImagesFolder := TempDir + 'images' + DirectorySeparator;
    DestFolder := OutDir;
    BindForm(DSP);
    Execute;
    Errs := Errors;
    if ImageFiles.Count > 0 then
      ChangeManifest(TempDir + 'META-INF' + DirectorySeparator + 'manifest.xml',
        ImageFiles);
  finally
    UnBind;
    FreeAndNil(MS);
    FreeAndNil(Outp);
    FreeAndNil(Inp);
    Free;
  end;

  ProcessOds(TempFile);
  DeleteFile(WordFile);
  RenameFile(TempFile, WordFile);

  SL := FindAllFiles(TempDir, '*', True);
  with TZipper.Create do
  try
    FileName:=OutName;
    for i := 0 to SL.Count - 1 do
    begin
      S := Copy(SL[i], Length(TempDir) + 1, 1024);
      S := StringReplace(S, '\', '/', [rfReplaceAll]);
      Entries.AddFileEntry(SL[i], S);
    end;
    ZipAllFiles;
  finally
    SL.Free;
    Free;
  end;

  DeleteDirectory(TempDir, False);
  DeleteFile(InpFile);
end;

procedure ReportToXXX(DSP: TDataSetProcessor; const AFileName: String;
  var OutName: String; out Errs: String; AOpenFile, ChangeOutName: Boolean);
var
  Ext, OutFile, TmpFile, Value: String;
  Accept: Boolean;
begin
  Value := ''; Accept := True;
  if OutName = '' then
    OutFile := GetOutputDir + ExtractFileName(AFileName)
  else
    OutFile := OutName;
  ForceDirectories(ExtractFilePath(OutFile));

  Ext := AnsiLowerCase(ExtractFileExt(AFileName));
  TmpFile := ExtractFilePath(OutFile) + 'temp' + IntToStr(Random(100000000)) + Ext;
  try
    if Ext = '.xml' then ReportToWordXml(DSP, AFileName, TmpFile, Errs)
    else if (Ext = '.docx') or (Ext = '.docm') then ReportToDocX(DSP, AFileName, TmpFile, Errs)
    else if Ext = '.odt' then ReportToOdt(DSP, AFileName, TmpFile, Errs)
    else if (Ext = '.html') or (Ext = '.htm') then ReportToHtml(DSP, AFileName, TmpFile, Errs)
    else if Ext = '.ods' then ReportToOds(DSP, AFileName, TmpFile, Errs);

    if ChangeOutName then OutFile := GetOutputFileName(OutFile);
    if FileExists(OutFile) then DeleteFile(OutFile);
    // Если по каким-то причинам переименовать не удается, то открываем хотя бы
    // временный файл.
    if not RenameFile(TmpFile, OutFile) then OutFile := TmpFile;

    if Errs <> '' then
      DSP.Form.DoPrintEvent(paPrintError, AFileName, '', Errs, Accept);

    OutName := OutFile;

    if AOpenFile then
    begin
      DSP.Form.DoPrintEvent(paBeforeOpenFile, OutFile, '', Value, Accept);
      OpenReportFile(OutFile);
      DSP.Form.DoPrintEvent(paAfterOpenFile, OutFile, '', Value, Accept);
    end;
  except
    on E: Exception do
    begin
      Errs := Errs + E.Message + LineEnding + rsPrintAborted;
      if FileExists(TmpFile) then DeleteFile(TmpFile);
      DSP.Form.DoPrintEvent(paPrintError, AFileName, '', Errs, Accept);
    end;
  end;
end;

{ TXmlReport }

procedure TXmlReport.ClearData;
var
  pD: PDataRec;
  i: Integer;
begin
  while FData.Count > 0 do
  begin
    pD := GetData(0);

    if pD^.ExprList <> nil then
      for i := 0 to pD^.ExprList.Count - 1 do
        pD^.ExprList.Objects[i].Free;
    FreeAndNil(pD^.ExprList);
    for i := 0 to pD^.Groups.Count - 1 do
      Dispose(PGroupRec(pD^.Groups[i]));
    pD^.Groups.Free;

    Dispose(pD);
    FData.Delete(0);
  end;
end;

procedure TXmlReport.BuildExprs(pD: PDataRec);
var
  Fm: TdxForm;
  i: Integer;
  EB: TExpressionBuilder;
  Ex: TExpression;
  S: String;
begin
  Fm := pD^.Form;
  pD^.ExprList := TStringListUtf8.Create;
  EB := TExpressionBuilder.Create;
  EB.Form := pD^.Form;
  EB.ParentForm := GetData(0)^.Form;
  EB.DataSet := pD^.DataSet;
  for i := 0 to Fm.CalcFields.Count - 1 do
  begin
    S := Trim(Fm.CalcFields.ValueFromIndex[i]);
    if S = '' then Continue;
    try
      Ex := EB.Build(S);
      if Ex <> nil then
        pD^.ExprList.AddObject(Fm.CalcFields.Names[i], Ex);
    except
      on E: Exception do
        AddError(Format(rsErrorCalcField, [Fm.CalcFields.Names[i]]) +
          E.Message);
    end;
  end;
  EB.Free;
end;

procedure TXmlReport.AddDataRec(Fm: TdxForm; DS: TDataSet);
var
  pD: PDataRec;
begin
  New(pD);
  pD^.Form := Fm;
  pD^.RD := nil;
  pD^.QGrid := nil;
  pD^.DataSet := DS;
  if (Fm.DSRi > 0) and PDataSetRec(Fm.DSR)^.NeedRefresh then
    pD^.Id := 0
  else
    pD^.Id := DS.FieldByName('id').AsInteger;
  DS.DisableControls;
  pD^.BandKind:='';
  pD^.BandName := Fm.FormCaption;
  pD^.Pos := 0;
  if Fm.PId = 0 then
    pD^.Parent:=nil
  else
    pD^.Parent := FData[0];
  pD^.Groups := TList.Create;
  pD^.Script := False;
  FData.Add(pD);
  BuildExprs(pD);
  AddDataRecsQ(pD);
end;

procedure TXmlReport.AddDataRecsQ(pD: PDataRec);
var
  i: Integer;
begin
  for i := 0 to pD^.Form.QueryCount - 1 do
    AddDataRecQ(TdxQueryGrid(pD^.Form.QueryByIndex[i]), pD);
end;

procedure TXmlReport.AddDataRecQ(QG: TdxQueryGrid; aParent: PDataRec);
var
  pD: PDataRec;
  RD: TReportData;
begin
  RD := ReportMan.FindReport(QG.Id);
  if RD.IsEmpty then Exit;
  New(pD);
  pD^.Form := nil;
  pD^.RD := RD;
  pD^.QGrid := QG;
  pD^.DataSet := QG.DataSource.DataSet;
  if TDataSetProcessor(QG.DSP).Queries[QG.QRi]^.NeedRefresh then
    pD^.Id := 0
  else
    pD^.Id := pD^.DataSet.RecNo;
  pD^.DataSet.DisableControls;
  pD^.BandKind := '';
  pD^.BandName := RD.Name;
  pD^.Pos := 0;
  pD^.ExprList := nil;
  pD^.Parent := aParent;
  pD^.Groups := TList.Create;
  pD^.Script := False;
  FData.Add(pD);
end;

function TXmlReport.GetData(i: Integer): PDataRec;
begin
  Result := PDataRec(FData[i]);
end;

function TXmlReport.FindDataByFormCaption(const S: String): PDataRec;
var
  i: Integer;
  pD: PDataRec;
begin
  Result := nil;
  for i := 0 to FData.Count - 1 do
  begin
    pD := getData(i);
    if MyUtf8CompareText(pD^.BandName, S) = 0 then Exit(pD);
  end;
end;

constructor TXmlReport.Create;
begin
  FPars := TXmlParser.Create;
  FData := TList.Create;
  FImageFiles := TStringList.Create;
end;

destructor TXmlReport.Destroy;
begin
  ClearData;
  FData.Free;
  FPars.Free;
  FImageFiles.Free;
  inherited Destroy;
end;

procedure TXmlReport.BindForm(DSP: TDataSetProcessor);
var
  i: Integer;
  DSR: PDataSetRec;
begin
  FErrors := '';
  for i := 0 to DSP.DataSetCount - 1 do
  begin
    DSR := DSP.DataSets[i];
    AddDataRec(DSR^.Form, DSR^.DataSet);
  end;
end;

procedure TXmlReport.UnBind;
var
  i: Integer;
begin
  for i := 0 to FData.Count - 1 do
  begin
    with PDataRec(FData[i])^ do
    begin
      if Form <> nil then
      begin
        // Датасет сначала был закрыт, а потом открылся в процессе печати
        if (Id = 0) and not PDataSetRec(Form.DSR)^.NeedRefresh then
          DataSet.First
        // Возвращаемся к записи до печати
        else if (Id <> 0) and (DataSet.FieldByName('id').AsInteger <> Id) then
          DataSet.Locate('id', Id, []);
      end
      else if RD <> nil then
      begin
        if not TDataSetProcessor(QGrid.DSP).Queries[QGrid.QRi]^.NeedRefresh then
        begin
          if (Id = 0) and (DataSet.RecNo > 1)then
            DataSet.First
          else if (Id > 0) and (DataSet.RecNo <> Id) and (Id <= DataSet.RecordCount) then
            DataSet.RecNo := Id;
        end;
      end;
      if DataSet <> nil then
	      DataSet.EnableControls;
    end;
  end;
  ClearData;
end;

function TXmlReport.LookupFieldValue(Form: TdxForm; const FieldName: String; DataSet: TDataSet): String;
var
  i, pr: Integer;
  S, FldNm, Dir, Tmp: String;
  SL: TStringList;
  DS: TDataSet;
  Fm: TdxForm;
  C: TComponent;
  BlankImage, NeedConvert: Boolean;
  Lbl: TdxLabel;
  dt: String;
  Cbx: TdxLookupComboBox;
begin
  Result := '';
  SL := TStringList.Create;
  SplitStr(FieldName, '|', SL);

  if SL[0][1] = '!' then
  begin
    SL[0] := Copy(SL[0], 2, 1024);
    Form := GetData(0)^.Form;
    DataSet := GetData(0)^.DataSet;
  end
  else if SL[0][1] = ':' then
    SL[0] := Copy(SL[0], 2, 1024);

  DS := DataSet;
  Fm := Form;
  S := '';
  dt := ''; pr := 0;

  try

  C := FindComponentByFieldName(Fm, SL[0]);

  if C <> nil then
    for i := 0 to SL.Count - 1 do
    begin
      if (not (C is TdxLookupComboBox)) and (i < SL.Count - 1) then
      begin
        AddError(rsNotObjField + ': ' + FieldName);
        Exit;
      end;
      FldNm := FieldStr(C);
      // Получаем компонент, на который ссылается поле объекта, но при этом
      // обращаемся к текущему набору данных.
      if C is TdxObjectField then
      begin
        C := GetObjectFieldField(TdxObjectField(C));
        if C = nil then Break;
      end;
      // ------------------ IMAGE --------------------
      if C is TdxDBImage then
      begin
        if i > 0 then
        begin
          S := DS.FieldByName(FldNm + 'src').AsString;
          Break;
        end;

        S := GetImageFileName(TdxDBImage(C), DS);
        BlankImage := S = '';
        if BlankImage then
        begin
          Dir := FImagesFolder;
          S := S + Dir + '__blank__.png';
        end
        else
        begin
          // Преобразуем в png, если формат отличается от поддерживаемых
          NeedConvert := Pos(';' + ExtractFileExt(S) + ';', ';.jpg;.jpeg;.png;.tif;.tiff;') = 0;
          if NeedConvert then S := ChangeFileExt(S, '.png');
          Dir := FImagesFolder;
          S := 'img' + IntToStr(FImageId) + ExtractFileExt(S);
          S := Dir + S;
        end;
        FImageFileName := S;
        FImageFiles.Add(S);
        FIsImageFound := True;
        FIsImageSize := True;
        FIsAltImageSize := FAltImageSizeTag <> '';
        Inc(FImageId);
        if ForceDirectories(Dir) then
        begin
          if not BlankImage then
          begin
            if TdxDBImage(C).PrintSize > 0 then
              SaveImageToFile(FImageFileName, TdxDBImage(C).PrintSize, TdxDBImage(C), DS)
            else if NeedConvert then
              SaveImageToFileConvert(FImageFileName, TdxDBImage(C), DS)
            else
              SaveImageToFile(FImageFileName, TdxDBImage(C), DS);
            GetImageSize(S, FImgSize);
          end
          else
          begin
            CreateBlankImage(FImageFileName);
            FImgSize := FImgSize.Zero;
          end;
        end;
        S := '';
        Break;
      end
      // ---------------------------------------------
      else if C is TdxFile then
      begin
        S := DS.FieldByName(FldNm + 'd').AsString;
        Break;
      end
      else if (C is TdxLookupComboBox) and (i < SL.Count - 1) then
      begin
        S := DS.FieldByName(FldNm).AsString;
        if S = '' then Break;
        Fm := FormMan.FindForm(GetSourceTId(C));
        if Fm = nil then Exit;
        if DS <> DataSet then FreeAndNil(DS);
        Cbx := TdxLookupComboBox(C);

        C := FindComponentByFieldName(Fm, SL[i+1]);
        if C is TdxObjectField then C := nil;
		    if C = nil then Break;
        // Если поле объекта отображается в компоненте, то значение берется из
        // компонента без запроса к базе.
        if (GetId(C) = Cbx.SourceFId) and (SL.Count = 2) then
        begin
          S := DataSet.FieldByName(FieldStr(Cbx.Id) + 'l').AsString;
          Break;
        end
        else if C is TdxRecordId then Break;

        Tmp := SqlSelectGroups(Fm.Id, True);
        if Tmp <> '' then Tmp := '(' + Tmp + ')'
        else Tmp := TableStr(Fm.Id);

        DS := DBase.OpenDataSet('select ' + GetComponentDataSetFieldName(C)
          {FieldStr(C)} + ' from ' + Tmp + ' where id=' + S);
      end
      else if C is TdxCheckBox then
        with TdxCheckBox(C) do
        begin
          S := DS.FieldByName(FldNm).AsString;
          if S = '1' then S := CheckedText
          else S := UnCheckedText;
          Break;
        end
      else if C is TdxCalcEdit then
      begin
        dt := 'n';
        pr := TdxCalcEdit(C).Precission;
        with DS.FieldByName(FldNm) do
          if not IsNull then
            S := FormatFloat(GetPrecStr(C), AsFloat)
          else S := '';
        Break;
      end
      else if C is TdxCounter then
      begin
        dt := 'n';
        S := DS.FieldByName(FldNm).AsString;
        Break;
      end
      else if C is TdxRecordId then
      begin
        dt := 'n';
        S := DS.FieldByName(FldNm).AsString;
        Break;
      end
      else if C is TdxTimeEdit then
        with DS.FieldByName(FldNm) do
        begin
          dt := 't';
          Pr := Ord(TdxTimeEdit(C).TimeFormat);
          if not IsNull then
            S := FormatDateTime(TdxTimeEdit(C).TimeFormatStr, AsDateTime)
          else
            S := '';
          Break;
        end
      else if C is TdxDateEdit then
      begin
        dt := 'd';
        S := DS.FieldByName(FldNm).AsString;
        Break;
      end
      else
      begin
        S := DS.FieldByName(FldNm).AsString;
        Break;
      end;
    end;

  if C = nil then
  begin
    Lbl := nil;
    if SL.Count = 1 then
    begin
      Lbl := FindLabelByFieldName(Fm, SL[0], False);
      if Lbl <> nil then
      begin
        if Lbl.Value = unassigned then
          try
            CalcLabelExpr(Lbl, DS, GetData(0)^.Form);
          except
            on E: Exception do
            begin
              AddError(Format(rsErrorInField, [Lbl.FieldName, E.Message]));
              Result := '';
              Exit;
            end;
          end;
        if VarIsNumeric(Lbl.Value) then
          dt := 'n'
        else if VarType(Lbl.Value) = vardate then
        begin
          if TimeOf(Lbl.Value) = 0 then dt := 'd'
          else
          begin
            dt := 't';
            pr := 2;
          end;
        end;
        Result := VarToStr(Lbl.Value);
      end;
    end;
    if Lbl = nil then
    begin
      AddError(Format(rsFieldNotFound, [FieldName]));
      Result := '';
    end;
  end
  else
    Result := S;

  finally
    if DS <> DataSet then FreeAndNil(DS);
    SL.Free;
  end;

  // Используется для определения типа значения в ячейке электронной таблицы
  if (FExtraTag) and (Result <> '') and (dt <> '') then
  begin
    Result := GetExtraTag(dt, pr) + Result;
  end;

  //ShowMessage(FieldName + ' - ' + Result);
end;

procedure TXmlReport.AddError(const S: String);
begin
  if Pos(LineEnding + S + LineEnding, LineEnding + FErrors) > 0 then Exit;
  FErrors := FErrors + S + LineEnding;
end;

function TXmlReport.DoCalcField(pD: PDataRec; const FldNm: String;
  var V: Variant): Boolean;
var
  i: Integer;
  S: String;
  E: TExpression;
begin
  Result := False;
  S := FldNm;
  if S[1] = '!' then
  begin
    Delete(S, 1, 1);
    pD := GetData(0);
  end
  else if S[1] = ':' then
    Delete(S, 1, 1);
  i := pD^.ExprList.IndexOf(S);
  if i < 0 then Exit(False);
  try
    E := TExpression(pD^.ExprList.Objects[i]);
    V := E.Calc;
    if FExtraTag then
    begin
      S := '';
      if VarIsNumeric(V) then
        S := GetExtraTag('n', 0)
      else if VarType(V) = varDate then
      begin
        if TimeOf(V) = 0 then
          S := GetExtraTag('d', 0)
        else
          S := GetExtraTag('t', 2);
      end;
      if S <> '' then V := S + VarToStr(V);
    end;
  except
    on E: Exception do
    begin
      V := '';
      AddError(Format(rsErrorCalcField, [FldNm]) + E.Message);
    end;
  end;
  Result := True;
end;

function TXmlReport.LookupQueryFieldValue(D: TDataRec; const FieldName: String
  ): String;
var
  Col: TRpGridColumn;
  F: TField;
  dt: String;
  pr: Integer;
begin
  Result := ''; dt := ''; pr := 0;
  Col := D.RD.Grid.FindColumnByTitle(FieldName);
  if Col <> nil then
  begin
    F := D.DataSet.FieldByName(Col.FieldNameDS);
    if F.IsNull then
      Exit
    else if F is TNumericField then
    begin
      with TNumericField(F) do
      begin
        dt := 'n';
        if DisplayFormat <> '' then
        begin
          Result := FormatFloat(DisplayFormat, AsFloat);
          pr := Length(DisplayFormat) - 2;
          // Учитываем также символ группировки
          if DisplayFormat[1] = ',' then Dec(pr);
        end
        else Result := F.AsString;
      end;

    end
    else if F is TTimeField then
    begin
      Result := FormatDateTime(TTimeField(F).DisplayFormat, F.AsDateTime);
      dt := 't';
      pr := Length(TTimeField(F).DisplayFormat);
      case pr of
        8: pr := 2;
        5: pr := 1;
        2: pr := 0;
      end;
    end
    else if F is TDateField then
    begin
      dt := 'd';
      Result := F.AsString;
    end
    else
      Result := F.AsString;
  end
  else AddError(Format(rsFieldNotFound, [FieldName]));
  if FExtraTag and (Result <> '') and (dt <> '') then
    Result := GetExtraTag(dt, pr) + Result;
end;

function TXmlReport.FindGroup(D: TDataRec; const S: String): PGroupRec;
var
  i: Integer;
  pG: PGroupRec;
begin
  Result := nil;
  for i := 0 to D.Groups.Count - 1 do
  begin
    pG := PGroupRec(D.Groups[i]);
    if MyUtf8CompareText(pG^.FieldName, S) = 0 then Exit(pG);
  end;
end;

procedure TXmlReport.CheckGroupPos(D: TDataRec; P: Integer);
var
  i: Integer;
  G: TGroupRec;
begin
  for i := 0 to D.Groups.Count - 1 do
  begin
    G := PGroupRec(D.Groups[i])^;
    if G.GroupPos = P then
      raise Exception.Create(rsInvalidUsingGroupTag);
  end;
end;

procedure TXmlReport.CheckBandPos(pCurD: PDataRec; P: Integer; const BnNm,
  BnKd: String);
begin
  if (pCurD <> nil) and (pCurD^.Pos = P) then
  begin
    if BnKd = 'form' then
      raise Exception.CreateFmt(rsInvalidUseTagForm, [pCurD^.BandName, BnNm])
    else if BnKd = 'grid' then
      raise Exception.CreateFmt(rsInvalidUseTagGrid, [pCurD^.BandName, BnNm]);
  end;
end;

function CheckAmps(const S: String): String;
var
  i: Integer;
  Skip: Boolean;
begin
  Result := '';
  Skip := False;
  for i := 1 to Length(S) do
  begin
    case S[i] of
      '&': Result := Result + '&amp;';
      '<':
        begin
          if Copy(S, i + 1, 4) = 'dx:v' then
          begin
            Skip := True;
            Result := Result + '<';
          end
          else
            Result := Result + '&lt;';
        end;
      '>':
        begin
          if not Skip then
            Result := Result + '&gt;'
          else
          begin
            Skip := False;
            Result := Result + '>';
          end;
        end
      else Result := Result + S[i];
    end;
  end;
end;

function ReplaceBr(const S, Br: String): String;
var
  i, L: Integer;
  Ch: Char;
begin
  if Br = '' then Exit(S);

  Result := '';
  L := Length(S);
  Ch := #0;
  for i := 1 to L do
  begin
    if S[i] in [#13, #10] then
    begin
      if not (Ch in [#13, #10]) then
        Result := Result + Br
    end
    else
      Result := Result + S[i];
    Ch := S[i];
  end;
end;

// Извлекает из атрибута тега число
procedure ExtractNumber(const S: String; StartPos: Integer; var Number: String;
  var StartNumPos: Integer);
var
  i: Integer;
  Ch: Char;
begin
  Number := ''; StartNumPos := 0;
  for i := StartPos to Length(S) do
  begin
    Ch := S[i];
    if Ch in ['0'..'9', '.'] then
    begin
      Number := Number + Ch;
      if StartNumPos = 0 then StartNumPos := i;
    end
    else if StartNumPos > 0 then Break;
  end;
end;

function TryStr2Float(const S: String; var D: Double): Boolean;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator:='.';
  Result := TryStrToFloat(S, D, FS);
end;

function Float2Str(D: Double): String;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator:='.';
  Result := FloatToStr(D, FS);
end;

function ProcessImageSize(S, ImageWidthStr, ImageHeightStr: String; ImgSize: TPoint;
  ImgSizeFloat: Boolean): String;
var
  ImgWStr, ImgHStr: String;
  i, ImgHStrPos, ImgWStrPos: Integer;
  ImgH, ImgW: Double;
  SizeFactor: Extended;
begin
  Result := S;
  if ImgSize.IsZero then Exit;
  ImgWStr := ''; ImgHStr := '';
  i := Pos(ImageHeightStr, S);
  if i > 0 then
    ExtractNumber(S, i, ImgHStr, ImgHStrPos);
  i := Pos(ImageWidthStr, S);
  if i > 0 then
    ExtractNumber(S, i, ImgWStr, ImgWStrPos);
  if TryStr2Float(ImgHStr, ImgH) and TryStr2Float(ImgWStr, ImgW) then
  begin
    SizeFactor := Max(ImgSize.x, ImgSize.y) / Min(ImgSize.x, ImgSize.y);
    if ImgSize.y > ImgSize.x then
      ImgW := ImgH / SizeFactor
    else
      ImgH := ImgW / SizeFactor;
    if not ImgSizeFloat then
    begin
      ImgW := Round(ImgW);
      ImgH := Round(ImgH);
    end;
    Delete(S, ImgHStrPos, Length(ImgHStr));
    Insert(Float2Str(ImgH), S, ImgHStrPos);
    // Снова определяем позицию, т. к. строка уже изменилась
    i := Pos(ImageWidthStr, S);
    ExtractNumber(S, i, ImgWStr, ImgWStrPos);
    Delete(S, ImgWStrPos, Length(ImgWStr));
    Insert(Float2Str(ImgW), S, ImgWStrPos);
    Result := S;
  end;
end;

procedure TXmlReport.Execute;
var
  Tk: Char;
  BnKd, BnNm, Tags, FldNm, FldVl, TagNm, S, Tmp: String;
  pOldD, pD: PDataRec;
  FormEndTagFound, GridEndTagFound, IsGroup, NewParaTagFind, NewRowTagFind: Boolean;
  ParaTagPos, RowTagPos, i, n: Integer;
  V: Variant;
  pG: PGroupRec;
  Bands: TList;
  Form: TdxForm;
  Accept: Boolean;
  PvGrid: TdxPivotGrid;
  Chart: TdxChart;

  function GetParentData(aData: PDataRec): PDataRec;
  begin
    if aData^.Parent <> nil then
      Result := aData^.Parent
    else
      Result := FData[0]
  end;

  function ProcessField(pD: PDataRec; const FldNm: String): String;
  var
    pTmpD: PDataRec;
    Accept: Boolean;
  begin
    Result := '';
    Accept := False;
    Form.DoPrintEvent(paPrintField, pD^.BandName, FldNm, Result, Accept);
    if Accept then Exit;

    pTmpD := pD;
    // Внутри данных запроса могут быть поля форм.
    if pD^.RD <> nil then
    begin
      if FldNm[1] = ':' then pTmpD := GetParentData(pD)
      else if FldNm[1] = '!' then pTmpD := GetData(0);
    end;
    if pTmpD^.Form <> nil then
    begin
      if DoCalcField(pTmpD, FldNm, V) then
        Result := VarToStr(V)
      else
        Result := LookupFieldValue(pTmpD^.Form, FldNm, pTmpD^.DataSet);
    end
    else if pTmpD^.RD <> nil then
    begin
    	Result := LookupQueryFieldValue(pTmpD^, FldNm);
    end;
  end;

  function GetPos(pD: PDataRec): Integer;
  var
    i: Integer;
    pG: PGroupRec;
    V: String;
  begin
    Result := -1;
    if pD^.Groups.Count > 0 then
    begin
      for i := 0 to pD^.Groups.Count - 1 do
      begin
        pG := PGroupRec(pD^.Groups[i]);
        V := ProcessField(pD, pG^.FieldName);
        if V <> pG^.Value then
        begin
          pG^.Value := V;
          // Возвращаем позицию из первой измененной группы
          if Result < 0 then
          begin
            if i = 0 then Result := pD^.Pos
            else Result := PGroupRec(pD^.Groups[i - 1])^.Pos;
          end;
        end;
      end;
      if Result < 0 then Result := pG^.Pos;
    end
    else Result := pD^.Pos;
  end;

  function AddVirtualData(const BnNm: String): PDataRec;
  begin
    New(Result);
    FillChar(Result^, SizeOf(TDataRec), 0);
    Result^.BandName:=BnNm;
    Result^.Groups := TList.Create;
    Result^.Script := True;
    FData.Add(Result);
  end;

  function NextData(pD: PDataRec): Boolean;
  var
    Tmp: String;
  begin
    Result := False;
    Tmp := '';
    Form.DoPrintEvent(paNextData, pD^.BandName, '', Tmp, Result);
    if (not Result) and (pD^.DataSet <> nil) then
    begin
      pD^.DataSet.Next;
      Result := not pD^.DataSet.EOF;
    end;
  end;

begin
  if FData.Count = 0 then Exit;
  Bands := TList.Create;
  pD := nil; Form := GetData(0)^.Form;
  FInputStream.Position:=0;
  RowTagPos := 0;
  ParaTagPos := 0;
  FImageId := 1000;
  //EndPrevToken := 0; BeginNextToken := 0;
  FIsImageFound := False;
  GridEndTagFound := False;
  FormEndTagFound := False;
  IsGroup := False;
  NewParaTagFind := False;
  NewRowTagFind := False;
  FPars.InputStream := FInputStream;

  try

  Tk := FPars.GetToken;
  while Tk <> ttEof do
  begin
    if Tk = ttTag then
    begin
      TagNm := FPars.GetTagName;
      if CompareText(TagNm, FRowTag) = 0 then
      begin
        RowTagPos := FInputStream.Position - Length(FPars.Token) - 2; {<>}
        if NewRowTagFind then
        begin
          pG^.Pos:=RowTagPos;
          NewRowTagFind := False;
        end;
      end
      else if (CompareText(TagNm, '/' + FRowTag) = 0) and (pD <> nil) and GridEndTagFound then
      begin
        FInputStream.Position := GetPos(pD);
        GridEndTagFound := False;
      end
      else if CompareText(TagNm, FParaTag) = 0 then
      begin
        ParaTagPos := FInputStream.Position - Length(FPars.Token) - 2; {<>}
        if NewParaTagFind then
        begin
          pG^.Pos := ParaTagPos;
          NewParaTagFind:=False;
        end;
      end
      else if (CompareText(TagNm, '/' + FParaTag) = 0) and (pD <> nil) and FormEndTagFound then
      begin
        FInputStream.Position := GetPos(pD);
        FormEndTagFound := False;
      end
      // --------------------- IMAGE --------------------------
      else if (CompareText(TagNm, FImageTag) = 0) and FIsImageFound then
      begin
        S := FPars.FToken;
        // Обеспечиваем совместимость с шаблонами, где связь с изображением
        // внешняя.
        if FOldImageTagSrc <> '' then
          S := StringReplace(S, FOldImageTagSrc, FImageTagSrc, []);
        i := Pos(' ' + FImageTagSrc + '="', S);
        if i > 0 then
        begin
          i := i + Length(FImageTagSrc) + 3;
          Delete(S, i, PosEx('"', S, i) - i);
          if FWriteImageId then
            System.Insert('rId' + IntToStr(FImageId - 1), S, i)
          else
            System.Insert(FImageTagSrcPrefix + ExtractFileName(FImageFileName), S, i);
        end;
        FIsImageFound := False;
        FPars.FToken := S;
      end;

      // Размер изображения
      if (CompareText(TagNm, FImageSizeTag) = 0) and FIsImageSize then
      begin
        FIsImageSize := False;
        FPars.FToken := ProcessImageSize(FPars.Token, FImageWidthStr, FImageHeightStr,
          FImgSize, FImageSizeFloat);
      end
      else if (FAltImageSizeTag <> '') and (CompareText(TagNm, FAltImageSizeTag) = 0) and
        FIsAltImageSize then
      begin
        // сделал специально для docx и word xml, т. к. a:ext встречается дважды
        // с разными атрибутами
        if Pos(FImageWidthStr, FPars.Token) > 0 then
        begin
          FIsAltImageSize := False;
          FPars.FToken := ProcessImageSize(FPars.Token, FImageWidthStr, FImageHeightStr,
            FImgSize, FImageSizeFloat);
        end;
      end;
      // -------------------------------------------------------
      Tags := '<' + FPars.Token + '>';
      FOutputStream.Write(Tags[1], Length(Tags));
    end
    else if Tk = ttBand then
    begin
      FPars.ParseBand(BnKd, BnNm, Tags);

      if FPars.Eof then
      begin
        if BnNm <> '' then Tmp := BnKd + '|' + Utf8Copy(BnNm, 1, 30)
        else Tmp := BnKd;
        raise Exception.CreateFmt(rsTagWithoutClosingBracket, [Tmp + '...']);
      end;

      BnKd := AnsiLowerCase(BnKd);
      if Pos(';' + BnKd + ';', ';form;grid;end;group;') > 0 then
      begin
        if BnKd = 'end' then
        begin
          if (pD <> nil) and (Bands.Count > 0) then
          begin

            if  (FormEndTagFound = False) and (GridEndTagFound = False) then
            begin
              if not IsGroup then
              begin
                FormEndTagFound := False;
                GridEndTagFound := False;

                if not NextData(pD) then
                begin
                  if Bands.Count > 0 then
                  begin
                    pD := PDataRec(Bands[Bands.Count - 1]);
                    Bands.Delete(Bands.Count - 1);
                  end
                  else
                    pD := FData[0];
                end
                else if pD^.BandKind = 'form' then
                  FormEndTagFound := True
                else if pD^.BandKind = 'grid' then
                  GridEndTagFound := True;
                {else
                  AddError(rsEndWthBegin); }
              end
              else
              begin
                if pD^.BandKind = 'form' then NewParaTagFind := True
                else if pD^.BandKind = 'grid' then NewRowTagFind := True;
                IsGroup := False;
              end;
            end;

          end
          else
            AddError(rsEndWthBegin);
        end
        else if BnKd = 'group' then
        begin
          if pD <> nil then
          begin
            pG := FindGroup(pD^, BnNm);
            if pG = nil then
            begin
              if pD^.BandKind = 'form' then
                n := ParaTagPos
              else if pD^.BandKind = 'grid' then
                n := RowTagPos;

              CheckGroupPos(pD^, n);

              New(pG);
              pG^.FieldName := BnNm;
              pG^.Value := ProcessField(pD, BnNm);
              pG^.GroupPos := n;
              pD^.Groups.Add(pG);
            end;
            IsGroup := True;
          end;
        end
        else if (pD = nil) or (pD^.BandKind = '') or
          ((pD^.Form <> nil) and (MyUtf8CompareText(pD^.Form.FormCaption, BnNm) <> 0)) or
          ((pD^.RD <> nil) and (MyUtf8CompareText(pD^.RD.Name, BnNm) <> 0)) then
        begin
          if (BnKd = 'form') then
            n := ParaTagPos
          else if (BnKd = 'grid') then
            n := RowTagPos;
          CheckBandPos(pD, n, BnNm, BnKd);

          Accept := False; Tmp := '';
          Form.DoPrintEvent(paBeginData, BnNm, '', Tmp, Accept);

          pOldD := pD;
          pD := FindDataByFormCaption(BnNm);
          if Accept and (pD = nil) then pD := AddVirtualData(BnNm);

          if pD <> nil then
          begin
            Bands.Add(pOldD);

            pD^.Pos := n;
            pD^.BandKind:=BnKd;
            pD^.BandName:=BnNm;     // Сохраняем оригинальный регистр в шаблоне.

            if (not Accept) and (pD^.DataSet <> nil) then
            begin
              if (pD^.Form <> nil) and pD^.Form.IsHide then pD^.Form.RequeryIfNeed
              else if pD^.RD <> nil then pD^.QGrid.RequeryIfNeed(True);
	            pD^.DataSet.First;
            end;
          end
          else
            AddError(Format(rsFormNotFound, [BnNm]));
        end;
      end
      else if BnKd = 'pivotgrid' then
      begin
        if pD = nil then pD := GetData(0);
        PvGrid := TdxPivotGrid(pD^.Form.FindComponent(BnNm));
        if PvGrid <> nil then
        begin
          S := PivotGridToHtml(PvGrid);
          FOutputStream.Write(Pointer(S)^, Length(S));
        end
        else
        	AddError(Format(rsPivotGridNotFound, [BnNm]));
      end
      else if BnKd = 'chart' then
      begin
        if pD = nil then pD := GetData(0);
        if pD^.QGrid <> nil then
          Chart := TdxChart(TdxForm(pD^.QGrid.Owner).FindComponent(BnNm))
        else
          Chart := TdxChart(pD^.Form.FindComponent(BnNm));
        if Chart <> nil then
        begin
          FImageFileName := 'img' + IntToStr(FImageId) + '.png';
          FImageFiles.Add(FImageFileName);
          FIsImageFound := True;
          FIsImageSize := True;
          FIsAltImageSize := FAltImageSizeTag <> '';
          Inc(FImageId);
          if ForceDirectories(FImagesFolder) then
          begin
            Chart.SaveImageToFile(FImagesFolder + FImageFileName);
            //Chart.SaveImageToFile('d:\test.png');
            if Chart.SaveOriginalSize then
              FImgSize := Point(Chart.Width, Chart.Height)
            else
              FImgSize := Point(Chart.SaveImageWidth, Chart.SaveImageHeight);
          end;
        end
        else
          AddError(Format(rsChartNotFound, [BnNm]));
      end
      else
        AddError(Format(rsUnknownTag, [BnKd]));

      FOutputStream.Write(Pointer(Tags)^, Length(Tags));
    end
    else if Tk = ttField then
    begin
      if pD = nil then pD := GetData(0);
      FPars.ParseField(FldNm, Tags);

      if FPars.Eof then
        raise Exception.CreateFmt(rsFieldWithoutClosingBracket, [Utf8Copy(FldNm, 1, 30) + '...']);

      FldVl := '';
      if FldNm <> '' then
        FldVl := ProcessField(pD, FldNm);
      FOutputStream.Write(Pointer(Tags)^, Length(Tags));
      FldVl := ReplaceBr(CheckAmps(FldVl), FBrTag);
      FOutputStream.Write(Pointer(FldVl)^, Length(FldVl));
    end
    else if Tk = ttComment then
    begin
      Tags := '<' + FPars.Token + '>';
      FOutputStream.Write(Tags[1], Length(Tags));
    end
    else
    begin
      FOutputStream.Write(FPars.Token[1], Length(FPars.Token));
    end;
    Tk := FPars.GetToken;
  end;

  finally
    Bands.Free;
  end;
end;

function TXmlReport.GetImageId: Integer;
begin
  Result := FImageId;
end;

{ TXmlParser }

procedure TXmlParser.ReadChar;
begin
  Ch := Char(FStream.ReadByte)
end;

function TXmlParser.Eof: Boolean;
begin
  Result := FStream.Position >= FStream.Size;
end;

function TXmlParser.TryReadStr(Count: Integer): String;
var
  OldPos: Int64;
begin
  SetLength(Result, Count);
  OldPos := FStream.Position;
  FStream.Read(Pointer(Result)^, Count);
  FStream.Position:=OldPos;
end;

{procedure TXmlParser.SkipWhites;
begin
  while (not Eof) and (Ch in [#9, #10, #13, #32]) do ReadChar;
end;}

function TXmlParser.GetToken: Char;
var
  n: Integer;
  S: String;
begin
  if Eof then Exit(ttEof);
  ReadChar;
  //SkipWhites;
  case Ch of
    '<':
      if TryReadStr(3) = '!--' then
      begin
        S := '';
        while not Eof do
        begin
          if (Ch = '-') and (TryReadStr(2) = '->') then
          begin
            S := S + Ch;
            ReadChar;
            S := S + Ch;
            ReadChar;
            Break;
          end;
          ReadChar;
          S := S + Ch;
        end;
        FToken := S;
        Result := ttComment;
      end
      else
      begin
        n := 0; S := '';
        ReadChar;
        while not Eof do
        begin
          if Ch = '<' then
            Inc(n)
          else if Ch = '>' then
          begin
            if n = 0 then Break
            else Dec(n);
          end
          else S := S + Ch;
          ReadChar;
        end;
        FToken := S;
        Result := ttTag;
      end;
    '[':
      begin
        n := 0; S := '';
        ReadChar;
        while not Eof do
        begin
          if Ch = '[' then
            Inc(n)
          else if Ch = ']' then
          begin
            if n = 0 then Break
            else Dec(n);
          end;
          //else S := S + Ch;
          S := S + Ch;
          ReadChar;
        end;
        FToken := S;
        Result := ttField;
      end;
    '{':
      begin
        n := 0; S := '';
        ReadChar;
        while not Eof do
        begin
          if Ch = '{' then
            Inc(n)
          else if Ch = '}' then
          begin
            if n = 0 then Break
            else Dec(n);
          end;
          //else S := S + Ch;
          S := S + Ch;
          ReadChar;
        end;
        FToken := S;
        Result := ttBand;
      end;
    else
    begin
      FToken := Ch;
      Result := Ch;
    end;
  end;
end;

function TXmlParser.GetTagName: String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(FToken) do
    if FToken[i] in [#9, #10, #13, #32] then Break
    else Result := Result + FToken[i];
end;

function TXmlParser.GetBandType: String;
begin
  Result := GetTagName;
end;

function ExtractTags(var S: String): String;
var
  i, Len: Integer;
  Tags: String;
  IsTag: Boolean;
begin
  Tags := '';
  Len := Length(S);
  IsTag := False;
  i := 1;
  while i <= Len do
  begin
    if S[i] = '<' then
    begin
      Tags := Tags + S[i];
      Delete(S, i, 1);
      Dec(Len);
      IsTag := True;
    end
    else if S[i] = '>' then
    begin
      Tags := Tags + S[i];
      Delete(S, i, 1);
      Dec(Len);
      IsTag := False;
    end
    else if IsTag then
    begin
      Tags := Tags + S[i];
      Delete(S, i, 1);
      Dec(Len);
    end
    else
      Inc(i);
  end;
  Result := Tags;
end;

procedure TXmlParser.ParseField(out FieldName, Tags: String);
begin
  FieldName := FToken;
  Tags := Trim(ExtractTags(FieldName));
end;

procedure TXmlParser.ParseBand(out BandKind, BandName, Tags: String);
var
  S: String;
  i: SizeInt;
begin
  BandKind := ''; BandName := '';
  S := Trim(FToken);
  Tags := ExtractTags(S);
  i := Pos('|', S);
  if i > 0 then
  begin
    BandKind := Copy(S, 1, i - 1);
    BandName := Trim(Copy(S, i + 1, 255));
  end
  else
    BandKind := S;
end;

end.

