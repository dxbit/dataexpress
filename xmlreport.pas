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
    procedure SkipWhites;
  public
    function GetToken: Char;
    function GetTagName: String;
    function GetBandType: String;
    procedure ParseField(var FieldName: String; var Tags: String);
    procedure ParseBand(var BandKind, BandName, Tags: String);
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
    Pos: Integer;
    AfterScroll: TDataSetNotifyEvent;
    ExprList: TStringListUtf8;
    Parent: PDataRec;
    Groups: TList;
  end;

  { TXmlReport }

  TXmlReport = class
  private
    FBrTag: String;
    FExtraTag: Boolean;
    FImageFiles: TStringList;
    //FImageFolder: String;
    FImageTag: String;
    FImageTagSrc: String;
    FInputStream: TStream;
    FOutputStream: TStream;
    FParaTag: String;
    FPars: TXmlParser;
    FDestFolder: String;
    FRowTag: String;
    FData: TList;
    FImageId: Integer;
    FSaveRelativePath: Boolean;
    FWriteImageId: Boolean;
    FIsImageFound: Boolean;
    FImageFileName: String;
    FErrors: String;
    procedure ClearData;
    procedure BuildExprs(pD: PDataRec);
    procedure AddDataRec(Fm: TdxForm; DS: TDataSet);
    procedure AddDataRecsQ(pD: PDataRec);
    procedure AddDataRecQ(QG: TdxQueryGrid; aParent: PDataRec);
    //procedure SetQueryDataParents(aParent: PDataRec);
    function GetData(i: Integer): PDataRec;
    function FindDataByFormCaption(const S: String): PDataRec;
    function LookupFieldValue(Form: TdxForm; const FieldName: String; DataSet: TDataSet): String;
    procedure AddError(const S: String);
    function DoCalcField(pD: PDataRec; const FldNm: String; var V: Variant): Boolean;
    function LookupQueryFieldValue(D: TDataRec; const FieldName: String): String;
    function FindGroup(D: TDataRec; const S: String): PGroupRec;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BindForm(DSP: TDataSetProcessor);
    procedure UnBind;
    procedure Execute;
    function GetImageId: Integer;
    property InputStream: TStream read FInputStream write FInputStream;
    property OutputStream: TStream read FOutputStream write FOutputStream;
    property RowTag: String read FRowTag write FRowTag;
    property ParaTag: String read FParaTag write FParaTag;
    property BrTag: String read FBrTag write FBrTag;
    property ImageTag: String read FImageTag write FImageTag;
    property ImageTagSrc: String read FImageTagSrc write FImageTagSrc;
    property WriteImageId: Boolean read FWriteImageId write FWriteImageId;
    //property ImageFolder: String read FImageFolder write FImageFolder;
    property DestFolder: String read FDestFolder write FDestFolder;
    property SaveRelativePath: Boolean read FSaveRelativePath write FSaveRelativePath;
    property ImageFiles: TStringList read FImageFiles;
    property Errors: String read FErrors;
    property ExtraTag: Boolean read FExtraTag write FExtraTag;
  end;

procedure ReportToWordXML(DSP: TDataSetProcessor; const FileName: String; var OutFileName: String; var Errs: String);
procedure ReportToDocX(DSP: TDataSetProcessor; const aFileName: String; var OutFileName: String; var Errs: String);
procedure ReportToOdt(DSP: TDataSetProcessor; const aFileName: String; var OutFileName: String; var Errs: String);
procedure ReportToHTML(DSP: TDataSetProcessor; const FileName: String; var OutFileName: String; var Errs: String);
procedure ReportToOds(DSP: TDataSetProcessor; const aFileName: String; var OutFileName: String; var Errs: String);
procedure ReportToXXX(DSP: TDataSetProcessor; const FileName, OutFileName: String; var Errs: String; aOpenFile: Boolean);

implementation

uses
  LazUtf8, formmanager, apputils, sqlgen, dbengine, appsettings, zipper,
  fileutil, StrUtils, dximages, dxfiles, Forms,
  Variants, reportmanager, DateUtils, DOM, XmlRead, XmlWrite;

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

function GetOutputFileName(const FileName: String): String;
var
  SrcHandle: THandle;
  i: Integer;
  Ext: String;
begin
  Result := FileName;
  Ext := ExtractFileExt(FileName);
  for i := 1 to 100 do
  begin
    if FileExists(Result) then
    begin
      SrcHandle := FileOpen(Result, fmOpenRead + fmShareExclusive);
      if (THandle(SrcHandle)=feInvalidHandle) then
        Result := ChangeFileExt(FileName, '') + SetZeros(i, 3) + Ext
      else
      begin
        FileClose(SrcHandle);
        Break;
      end;
    end
    else Break;
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

procedure ChangeWordXmlRels(const FileName: String; ImageFiles: TStrings);
var
  SL: TStringList;
  S: String;
  i, p: SizeInt;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(FileName);
  S := SL.Text;
  p := Pos('document.xml.rels', S);
  if p > 0 then
  begin
    p := PosEx('<Relationship Id="', S, p);
    if p > 0 then
      for i := 0 to ImageFiles.Count - 1 do
      begin
        Insert('<Relationship Id="rId' + IntToStr(1000 + i) +
          '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" ' +
          'Target="file:///' + StringReplace(ImageFiles[i], '\', '/', [rfReplaceAll]) + '" ' +
          'TargetMode="External"/>', S, p);
      end;
  end;
  SL.Text := S;
  SL.SaveToFile(FileName);
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

procedure ReportToWordXML(DSP: TDataSetProcessor; const FileName: String;
  var OutFileName: String; var Errs: String);
var
  Inp, Outp: TFileStream;
  OutName: String;
  xml2003: Boolean;
begin
  xml2003 := IsWordXml2003(FileName);
  if OutFileName = '' then
  begin
    OutName := GetOutputFileName(GetOutputDir + ExtractFileName(FileName));
    OutFileName := OutName;
  end
  else
    OutName := OutFileName;
  Inp := nil; Outp := nil;
  with TXmlReport.Create do
  try
    RowTag:='w:tr';
    ParaTag := 'w:p';
    BrTag := '</w:t></w:r><w:r><w:br/><w:t>';
    if xml2003 then
    begin
      ImageTag := 'v:imagedata';
      ImageTagSrc := 'src';
      SaveRelativePath:=True;
    end
    else begin
      ImageTag := 'a:blip';
      ImageTagSrc := 'r:link';//'r:embed';
      WriteImageId := True;
    end;
    DestFolder := GetOutputDir;
    Inp := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
    Outp := TFileStream.Create(OutName, fmCreate);
    InputStream := Inp;
    OutputStream := Outp;
    BindForm(DSP);
    Execute;
    Errs := Errors;
    FreeAndNil(Outp);
    ChangeWordXmlRels(OutName, ImageFiles);
    OpenReportFile(OutName);
  finally
    Unbind;
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
  SL.LoadFromFile(FileName);
  S := SL.Text;
  p := Pos('<Relationship Id="', S);
  if p > 0 then
    for i := 0 to ImageFiles.Count - 1 do
    begin
      Insert('<Relationship Id="rId' + IntToStr(1000 + i) +
        '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" ' +
        'Target="file:///' + StringReplace(ImageFiles[i], '\', '/', [rfReplaceAll]) + '" ' +
        'TargetMode="External"/>', S, p);
    end;
  SL.Text := S;
  SL.SaveToFile(FileName);
  SL.Free;
end;

procedure ReportToDocX(DSP: TDataSetProcessor; const aFileName: String;
  var OutFileName: String; var Errs: String);
var
  OutName, OutDir, TempDir, WordFile, TempFile, InpFile, S: String;
  Inp, Outp: TFileStream;
  SL: TStringList;
  i: Integer;
begin
  OutDir := GetOutputDir;
  TempDir := OutDir + 'temp' + DirectorySeparator;
  WordFile := TempDir + 'word' + DirectorySeparator + 'document.xml';
  TempFile := TempDir + 'word' + DirectorySeparator + 'temp.xml';
  if OutFileName = '' then
  begin
    OutName := GetOutputFileName(OutDir + ExtractFileName(aFileName));
    OutFileName := OutName;
  end
  else
    OutName := OutFileName;
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

  Inp := nil; Outp := nil;

  with TXmlReport.Create do
  try
    Inp := TFileStream.Create(WordFile, fmOpenRead);
    Outp := TFileStream.Create(TempFile, fmCreate);
    InputStream := Inp;
    OutputStream := Outp;
    RowTag:='w:tr';
    ParaTag := 'w:p';
    BrTag := '</w:t></w:r><w:r><w:br/><w:t>';
    ImageTag := 'a:blip';
    ImageTagSrc := 'r:link';//'r:embed';
    WriteImageId := True;
    DestFolder := OutDir;
    //ImageFolder := OutDir + 'images' + DirectorySeparator; // + 'media' + DirectorySeparator;
    //DestFolder := '';//TempDir + 'word' + DirectorySeparator;
    //ForceDirectoriesUTF8(DestFolder + ImageFolder);
    BindForm(DSP);
    Execute;
    Errs := Errors;
    ChangeDocXRels(TempDir + 'word' + DirectorySeparator + '_rels' + DirectorySeparator + 'document.xml.rels',
      ImageFiles);
  finally
    UnBind;
    FreeAndNil(Outp);
    FreeAndNil(Inp);
    Free;
  end;

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
  //OpenReportFile(OutName);
end;

procedure ReportToOdt(DSP: TDataSetProcessor; const aFileName: String;
  var OutFileName: String; var Errs: String);
var
  OutName, OutDir, TempDir, WordFile, TempFile, InpFile, S: String;
  Inp, Outp: TFileStream;
  SL: TStringList;
  i: Integer;
begin
  OutDir := GetOutputDir;
  TempDir := OutDir + 'temp' + DirectorySeparator;
  WordFile := TempDir + 'content.xml';
  TempFile := TempDir + 'temp.xml';
  if OutFileName = '' then
  begin
    OutName := GetOutputFileName(OutDir + ExtractFileName(aFileName));
    OutFileName := OutName;
  end
  else
    OutName := OutFileName;
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

  Inp := nil; Outp := nil;
  with TXmlReport.Create do
  try
    Inp := TFileStream.Create(WordFile, fmOpenRead);
    Outp := TFileStream.Create(TempFile, fmCreate);
    InputStream := Inp;
    OutputStream := Outp;
    RowTag:='table:table-row';
    ParaTag := 'text:p';
    BrTag := '<text:line-break />';
    ImageTag := 'draw:image';
    ImageTagSrc := 'xlink:href';
    //ImageFolder := OutDir + 'images' + DirectorySeparator;
    //DestFolder := '';//OutDir;
    DestFolder := OutDir;
    //ForceDirectoriesUTF8(DestFolder + ImageFolder);
    BindForm(DSP);
    Execute;
    Errs := Errors;
  finally
    UnBind;
    FreeAndNil(Outp);
    FreeAndNil(Inp);
    Free;
  end;

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
  //OpenReportFile(OutName);
end;

procedure ReportToHTML(DSP: TDataSetProcessor; const FileName: String;
  var OutFileName: String; var Errs: String);
var
  Inp, Outp: TFileStream;
  OutName: String;
begin
  if OutFileName = '' then
  begin
    OutName := GetOutputFileName(GetOutputDir + ExtractFileName(FileName));
    OutFileName := OutName;
  end
  else
    OutName := OutFileName;
  Inp := nil; Outp := nil;
  with TXmlReport.Create do
  try
    RowTag:='tr';
    ParaTag := 'p';
    BrTag := '<br>';
    ImageTag := 'img';
    ImageTagSrc := 'src';
    //ImageFolder := 'images' + DirectorySeparator;
    //DestFolder := GetOutputDir + DirectorySeparator;
    DestFolder := GetOutputDir;
    SaveRelativePath:=True;
    //ForceDirectoriesUTF8(DestFolder + ImageFolder);
    Inp := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
    Outp := TFileStream.Create(OutName, fmCreate);
    InputStream := Inp;
    OutputStream := Outp;
    BindForm(DSP);
    Execute;
    Errs := Errors;
    //OpenReportFile(OutName);
  finally
    Unbind;
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
  begin
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
          Txt.TextContent := S;
        end;

        {S := N.TextContent;
        dt := 's'; Pr := 0;
        m := ExtractExtraTag(S, dt, Pr);
        Txt := TDOMElement(N.FindNode('text:p'));
        if Txt <> nil then
          Txt.TextContent:=S;  }
        if (m = 1) and (PL.Count = 1) then
        begin
          StyleName := N.GetAttribute('table:style-name');
          if StyleName = '' then StyleName := 'Default';
          S := StyleName + dt + IntToStr(Pr);
          if SL.IndexOf(S) < 0 then
          begin
            SL.Add(S);

            // Стиль числа в ячейке
            if dt = 'n' then
            begin
              St := Xml.CreateElement('number:number-style');
              St.SetAttribute('style:name', 'N' + S);
              Ch := Xml.CreateElement('number:number');
              if Pr > 0 then
                Ch.SetAttribute('number:decimal-places', IntToStr(Pr));
              Ch.SetAttribute('number:min-integer-digits', '1');
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
            end;
          end
          else if dt = 'd' then
          begin
            if TryStrToDate(N.TextContent, Dat) then
            begin
              N.SetAttribute('office:date-value', Format('%s-%s-%sT00:00:00',
                [SetZeros(YearOf(Dat), 2), SetZeros(MonthOf(Dat), 2), SetZeros(DayOf(Dat), 2)]));
              N.SetAttribute('office:value-type', 'date');
            end;
          end
          else if dt = 't' then
          begin
            if TryStrToTime(N.TextContent, Dat) then
            begin
              N.SetAttribute('office:time-value', Format('PT%dH%dM%dS',
                [HourOf(Dat), MinuteOf(Dat), SecondOf(Dat)]));
              N.SetAttribute('office:value-type', 'time');
            end;
          end;
        end;
        PL.Free;
      end;
      ChildNodes := N.ChildNodes;
      Process(ChildNodes);
      ChildNodes.Free;
    end;
  end;

begin
  SL := TStringList.Create;
  ReadXmlFile(Xml, aFileName);
  Nodes := Xml.ChildNodes;
  Process(Nodes);
  Nodes.Free;
  ProcessLastRows;
  //CalcAllRows;
  WriteXmlFile(Xml, aFileName);
  SL.Free;
end;

procedure ReportToOds(DSP: TDataSetProcessor; const aFileName: String;
  var OutFileName: String; var Errs: String);
var
  OutName, OutDir, TempDir, WordFile, TempFile, InpFile, S: String;
  Inp, Outp: TFileStream;
  SL: TStringList;
  i: Integer;
begin
  OutDir := GetOutputDir;
  TempDir := OutDir + 'temp' + DirectorySeparator;
  WordFile := TempDir + 'content.xml';
  TempFile := TempDir + 'temp.xml';
  if OutFileName = '' then
  begin
    OutName := GetOutputFileName(OutDir + ExtractFileName(aFileName));
    OutFileName := OutName;
  end
  else
    OutName := OutFileName;
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

  Inp := nil; Outp := nil;
  with TXmlReport.Create do
  try
    ExtraTag:=True;
    Inp := TFileStream.Create(WordFile, fmOpenRead);
    Outp := TFileStream.Create(TempFile, fmCreate);
    InputStream := Inp;
    OutputStream := Outp;
    RowTag:='table:table-row';
    ParaTag := 'text:p';
    BrTag := '</text:p><text:p>';
    ImageTag := 'draw:image';
    ImageTagSrc := 'xlink:href';
    //ImageFolder := OutDir + 'images' + DirectorySeparator;
    //DestFolder := '';//OutDir;
    DestFolder := OutDir;
    //ForceDirectoriesUTF8(DestFolder + ImageFolder);
    BindForm(DSP);
    Execute;
    Errs := Errors;
  finally
    UnBind;
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
  //OpenReportFile(OutName);
end;

procedure ReportToXXX(DSP: TDataSetProcessor; const FileName,
  OutFileName: String; var Errs: String; aOpenFile: Boolean);
var
  Ext, OutFile: String;
begin
  OutFile := OutFileName;
  Ext := AnsiLowerCase(ExtractFileExt(FileName));
  if Ext = '.xml' then ReportToWordXml(DSP, FileName, OutFile, Errs)
  else if (Ext = '.docx') or (Ext = '.docm') then ReportToDocX(DSP, FileName, OutFile, Errs)
  else if Ext = '.odt' then ReportToOdt(DSP, FileName, OutFile, Errs)
  else if Ext = '.html' then ReportToHtml(DSP, FileName, OutFile, Errs)
  else if Ext = '.ods' then ReportToOds(DSP, FileName, OutFile, Errs);

  if aOpenFile then
    OpenReportFile(OutFile);
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
  pD^.Id := DS.FieldByName('id').AsInteger;
  DS.DisableControls;
  pD^.BandKind:='';
  pD^.Pos := 0;
  if Fm.PId = 0 then
    pD^.Parent:=nil
  else
    pD^.Parent := FData[0];
  pD^.Groups := TList.Create;
  FData.Add(pD);
  BuildExprs(pD);
  AddDataRecsQ(pD);
  //SetQueryDataParents(pD);
end;

procedure TXmlReport.AddDataRecsQ(pD: PDataRec);
var
  i: Integer;
  C: TComponent;
begin
  for i := 0 to pD^.Form.ComponentCount - 1 do
  begin
    C := pD^.Form.Components[i];
    if C is TdxQueryGrid then
      AddDataRecQ(TdxQueryGrid(C), pD);
  end;
end;

procedure TXmlReport.AddDataRecQ(QG: TdxQueryGrid; aParent: PDataRec);
var
  pD: PDataRec;
  RD: TReportData;
begin
  RD := ReportMan.FindReport(QG.Id);
  if RD.Sources.Count = 0 then Exit;
  New(pD);
  pD^.Form := nil;
  pD^.RD := RD;
  pD^.QGrid := QG;
  pD^.DataSet := QG.DataSource.DataSet;
  pD^.Id := 0;
  pD^.DataSet.DisableControls;
  pD^.BandKind := '';
  pD^.Pos := 0;
  pD^.ExprList := nil;
  pD^.Parent := aParent;
  pD^.Groups := TList.Create;
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
    if (pD^.Form <> nil) and (Utf8CompareText(pD^.Form.FormCaption, S) = 0) then
      Exit(pD)
    else if (pD^.RD <> nil) and (Utf8CompareText(pD^.RD.Name, S) = 0) then
      Exit(pD);
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
      if (Form <> nil) and (DataSet.FieldByName('id').AsInteger <> Id) then
        DataSet.Locate('id', Id, []);
      //DataSet.AfterScroll:=AfterScroll;
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
  BlankImage: Boolean;
  Lbl: TdxLabel;
  dt: String;
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
      // ------------------ IMAGE --------------------
      if C is TdxDBImage then
      begin
        S := GetImageFileName(TdxDBImage(C), DS);
        if S = '' then
        begin
          Dir := DestFolder + 'images' + DirectorySeparator;
          S := S + Dir + '__blank__.png';
          BlankImage := True;
        end
        else
        begin
          Dir := DestFolder + 'images' + DirectorySeparator + SL[i] + DirectorySeparator;
          S := Dir + ExtractFileName(S);
          BlankImage := False;
        end;
        FImageFileName := S;
        FImageFiles.Add(S);
        FIsImageFound := True;
        Inc(FImageId);
        if ForceDirectories(Dir) and not (FileExists(S)) then
        begin
          if not BlankImage then
            SaveImageToFile(FImageFileName, 640, TdxDBImage(C), DS)
          else
            CreateBlankImage(FImageFileName);
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
        if DS <> DataSet then DS.Close;
        Tmp := SqlSelectGroups(Fm.Id, GetSourceFId(C), True);
        if Tmp <> '' then Tmp := '(' + Tmp + ')'
        else Tmp := TableStr(Fm.Id);

        C := FindComponentByFieldName(Fm, SL[i+1]);
		    if C = nil then Break;

        DS := DBase.OpenDataSet('select ' + FieldStr(C) + ' from ' + Tmp + ' where id=' + S);
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
      Lbl := FindLabelByFieldName(Form, SL[0], False);
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
    if DS <> DataSet then DS.Close;
    SL.Free;
  end;

  // Используется для определения типа значения в ячейке электронной таблицы
  if (FExtraTag) and (Result <> '') and (dt <> '') then
  begin
    Result := GetExtraTag(dt, pr) + Result;
  end;
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
    F := D.DataSet.FieldByName(Col.FieldName);
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
    if Utf8CompareText(pG^.FieldName, S) = 0 then Exit(pG);
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

procedure TXmlReport.Execute;
var
  Tk: Char;
  BnKd, BnNm, Tags, FldNm, FldVl, TagNm, S: String;
  pOldD, pD: PDataRec;
  FormEndTagFound, GridEndTagFound, IsGroup, NewParaTagFind, NewRowTagFind: Boolean;
  ParaTagPos, RowTagPos, i: Integer;
  V: Variant;
  pG: PGroupRec;
  Bands: TList;
  Ok: Boolean;

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
  begin
    pTmpD := pD;
    // Внутри данных запроса могут быть поля форм.
    if pD^.RD <> nil then
    begin
      if FldNm[1] = ':' then pTmpD := GetParentData(pD)
      else if FldNm[1] = '!' then pTmpD := GetData(0);
    end;
    if pTmpD^.Form <> nil then
    begin
      Ok := False;
      if pTmpD^.Form.OnPrintField <> nil then pTmpD^.Form.OnPrintField(pTmpD^.Form, FldNm, Result, Ok);
      if not Ok then
      begin
        if DoCalcField(pTmpD, FldNm, V) then
          Result := VarToStr(V)
        else
          Result := LookupFieldValue(pTmpD^.Form, FldNm, pTmpD^.DataSet);
      end;
    end
    else
    begin
      Ok := False;
      with pTmpD^.QGrid do
        if OnPrintField <> nil then OnPrintField(pTmpD^.QGrid, FldNm, Result, Ok);
      if not Ok then
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

begin
  if FData.Count = 0 then Exit;
  Bands := TList.Create;
  pD := nil; //GetData(0);
  //pDold := nil;
  FInputStream.Position:=0;
  RowTagPos := 0;
  ParaTagPos := 0;
  FImageId := 1000;
  FIsImageFound := False;
  GridEndTagFound := False;
  FormEndTagFound := False;
  IsGroup := False;
  NewParaTagFind := False;
  NewRowTagFind := False;
  FPars.InputStream := FInputStream;
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
        i := Pos(' ' + FImageTagSrc + '="', S);
        if i > 0 then
        begin
          i := i + Length(FImageTagSrc) + 3;
          Delete(S, i, PosEx('"', S, i) - i);
          if FWriteImageId then
            System.Insert('rId' + IntToStr(FImageId - 1), S, i)
          else if FSaveRelativePath then
            System.Insert(StringReplace(Copy(FImageFileName, Length(FDestFolder) + 1, 1024)
              , '\', '/', [rfReplaceAll]), S, i)
          else
            System.Insert('file:///' + StringReplace(FImageFileName, '\', '/', [rfReplaceAll]), S, i);
        end;
        FIsImageFound := False;
        FPars.FToken := S;
      end;
      // -------------------------------------------------------
      Tags := '<' + FPars.Token + '>';
      FOutputStream.Write(Tags[1], Length(Tags));
    end
    else if Tk = ttBand then
    begin
      FPars.ParseBand(BnKd, BnNm, Tags);
      BnKd := AnsiLowerCase(BnKd);
      if Pos(';' + BnKd + ';', ';form;grid;end;group;') > 0 then
      begin
        if (pD <> nil) and (BnKd = 'end') then
        begin
          if  (FormEndTagFound = False) and (GridEndTagFound = False) then
          begin
            if not IsGroup then
            begin
              FormEndTagFound := False;
              GridEndTagFound := False;
              pD^.DataSet.Next;
              if pD^.DataSet.Eof then
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
                GridEndTagFound := True
              else
                AddError(rsEndWthBegin);
            end
            else
            begin
              if pD^.BandKind = 'form' then NewParaTagFind := True
              else if pD^.BandKind = 'grid' then NewRowTagFind := True;
              IsGroup := False;
            end;
          end;
        end
        else if BnKd = 'group' then
        begin
          if pD <> nil then
          begin
            pG := FindGroup(pD^, BnNm);
            if pG = nil then
            begin
              New(pG);
              pG^.FieldName := BnNm;
              pG^.Value := ProcessField(pD, BnNm);
              if pD^.BandKind = 'form' then
                pG^.GroupPos := ParaTagPos
              else if pD^.BandKind = 'grid' then
                pG^.GroupPos:=RowTagPos;
              pD^.Groups.Add(pG);
            end;
            IsGroup := True;
          end;
        end
        else if (pD = nil) or (pD^.BandKind = '') or
          ((pD^.Form <> nil) and (Utf8CompareText(pD^.Form.FormCaption, BnNm) <> 0)) or
          ((pD^.RD <> nil) and (Utf8CompareText(pD^.RD.Name, BnNm) <> 0)) then
        begin
          pOldD := pD;
          pD := FindDataByFormCaption(BnNm);
          if pD <> nil then
          begin
            Bands.Add(pOldD);
            if (BnKd = 'form') then
              pD^.Pos := ParaTagPos
            else if (BnKd = 'grid') then
              pD^.Pos := RowTagPos;
            pD^.BandKind:=BnKd;
            pD^.DataSet.First;
          end
          else
            AddError(Format(rsFormNotFound, [BnNm]));
        end;
      end
      else
        AddError(Format(rsUnknownTag, [BnKd]));
      FOutputStream.Write(Pointer(Tags)^, Length(Tags));
    end
    else if Tk = ttField then
    begin
      if pD = nil then pD := GetData(0);
      FPars.ParseField(FldNm, Tags);
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
    //Application.ProcessMessages;
  end;
  Bands.Free;
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
  Result := FStream.Position = FStream.Size;
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

procedure TXmlParser.SkipWhites;
begin
  while (not Eof) and (Ch in [#9, #10, #13, #32]) do ReadChar;
end;

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
      begin
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

procedure TXmlParser.ParseField(var FieldName: String; var Tags: String);
begin
  FieldName := FToken;
  Tags := Trim(ExtractTags(FieldName));
end;

procedure TXmlParser.ParseBand(var BandKind, BandName, Tags: String);
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

