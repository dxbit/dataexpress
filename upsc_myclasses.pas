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

    Based on the original uPSC_Classes.pas file of the RemObject Pascal Script
    component.
-------------------------------------------------------------------------------}

{ Compiletime Classes support }
unit uPSC_MyClasses;

interface
uses
  uPSCompiler, uPSUtils;

{
  Will register files from:
    Classes (exception TPersistent and TComponent)

  Register STD first

}

procedure SIRegister_Classes_TypesAndConsts(Cl: TPSPascalCompiler);

procedure SIRegisterTStrings(cl: TPSPascalCompiler);
procedure SIRegisterTStringList(cl: TPSPascalCompiler);
procedure SIRegisterTSTREAM(Cl: TPSPascalCompiler);
procedure SIRegisterTMEMORYSTREAM(Cl: TPSPascalCompiler);
procedure SIRegisterTFILESTREAM(Cl: TPSPascalCompiler);
procedure SIRegisterTSTRINGSTREAM(Cl: TPSPascalCompiler);
procedure SIRegisterTCOLLECTIONITEM(CL: TPSPascalCompiler);
procedure SIRegisterTCOLLECTION(CL: TPSPascalCompiler; Web: Boolean);

procedure SIRegister_Classes(Cl: TPSPascalCompiler; Web: Boolean);

implementation

procedure SIRegisterTStrings(cl: TPSPascalCompiler); // requires TPersistent
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TStrings') do
  begin
    IsAbstract := True;
    RegisterMethod('constructor Create');
    RegisterMethod('function Add(S: string): Integer;');
    RegisterMethod('procedure AddStrings(Strings: TStrings);');
    RegisterMethod('procedure Clear;');
    RegisterMethod('procedure Delete(Index: Integer);');
    RegisterMethod('function IndexOf(const S: string): Integer; ');
    RegisterMethod('procedure Insert(Index: Integer; S: string); ');
    RegisterMethod('procedure LoadFromFile(FileName: string);');
    RegisterMethod('procedure SaveToFile(FileName: string);');
    RegisterMethod('procedure BeginUpdate;');
    RegisterMethod('procedure EndUpdate;');
    RegisterMethod('function Equals(Strings: TStrings): Boolean;');
    RegisterMethod('procedure Exchange(Index1, Index2: Integer);');
    RegisterMethod('function IndexOfName(Name: string): Integer;');
    RegisterMethod('procedure LoadFromStream(Stream: TStream); ');
    RegisterMethod('procedure Move(CurIndex, NewIndex: Integer); ');
    RegisterMethod('procedure SaveToStream(Stream: TStream); ');
    RegisterMethod('function AddObject(S: string; AObject: TObject): Integer');
    RegisterMethod('function IndexOfObject(AObject: TObject): Integer');
    RegisterMethod('procedure InsertObject(Index: Integer; S: string; AObject: TObject)');

    RegisterProperty('Capacity', 'Integer', iptRW);
    RegisterProperty('Delimiter', 'Char', iptRW);
    RegisterProperty('DelimitedText', 'string', iptrw);
    RegisterProperty('NameValueSeparator', 'Char', iptRW);
    RegisterProperty('QuoteChar', 'Char', iptRW);
    RegisterProperty('Count', 'Integer', iptR);
    RegisterProperty('Text', 'string', iptrw);
    RegisterProperty('CommaText', 'string', iptrw);
    RegisterProperty('Strings', 'string Integer', iptRW);
    SetDefaultPropery('Strings');
    RegisterProperty('StrictDelimiter', 'Boolean', iptRW);
    RegisterProperty('Objects', 'TObject Integer', iptRW);
    RegisterProperty('Names', 'string Integer', iptr);
    RegisterProperty('Values', 'string string', iptRW);
    RegisterProperty('ValueFromIndex', 'string Integer', iptRW);

  end;
end;

procedure SIRegisterTStringList(cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStrings'), 'TStringList') do
  begin
    RegisterMethod('function Find(S: string; var Index: Integer): Boolean');
    RegisterMethod('procedure Sort');
    RegisterProperty('CaseSensitive', 'Boolean', iptrw);
    RegisterProperty('Duplicates', 'TDuplicates', iptrw);
    RegisterProperty('Sorted', 'Boolean', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    RegisterProperty('OnChanging', 'TNotifyEvent', iptrw);
  end;

  Cl.AddTypeS('TStringListCompareEvent', 'function(Sender: TStringList; const s1, s2: String): Integer');
  with Cl.AddClassN(cl.FindClass('TStringList'), 'TStringListUtf8') do
  begin
    RegisterProperty('OnCompare', 'TStringListCompareEvent', iptrw);
  end;
end;

{procedure SIRegisterTBITS(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TObject'), 'TBits') do
  begin
    RegisterMethod('function OpenBit: Integer');
    RegisterProperty('Bits', 'Boolean Integer', iptrw);
    RegisterProperty('Size', 'Integer', iptrw);
  end;
end;  }

procedure SIRegisterTSTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TObject'), 'TStream') do
  begin
    //IsAbstract := True;
    RegisterMethod('function Read(Buffer: string; Count: LongInt): LongInt; virtual;');
    RegisterMethod('function Write(Buffer: string; Count: LongInt): LongInt; virtual;');
    RegisterMethod('function Seek(Offset: LongInt; Origin: Word): LongInt; virtual;');
    RegisterMethod('procedure ReadBuffer(Buffer: string; Count: LongInt)');
    RegisterMethod('procedure WriteBuffer(Buffer: string; Count: LongInt)');
    RegisterMethod('function CopyFrom(Source: TStream; Count: Int64): Int64');
    RegisterProperty('Position', 'Int64', iptrw);
    RegisterProperty('Size', 'Int64', iptrw);
  end;
end;

procedure SIRegisterTHANDLESTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStream'), 'THandleStream') do
  begin
    RegisterMethod('constructor Create(AHandle: Integer)');
    RegisterProperty('Handle', 'Integer', iptr);
  end;
end;

procedure SIRegisterTMEMORYSTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStream'), 'TMemoryStream') do
  begin
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure LoadFromStream(Stream: TStream)');
    RegisterMethod('procedure LoadFromFile(FileName: string)');
    RegisterMethod('procedure SaveToStream(Stream: TStream)');
    RegisterMethod('procedure SaveToFile(FileName: string)');
    RegisterMethod('procedure SetSize(NewSize: LongInt)');
  end;
end;

procedure SIRegisterTFILESTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStream'), 'TFileStream') do
  begin
    RegisterMethod('constructor Create(FileName: string; Mode: Word)');
    RegisterProperty('Handle', 'Integer', iptr);
  end;
end;

procedure SIRegisterTSTRINGSTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TMemoryStream'), 'TStringStream') do
  begin
    RegisterMethod('constructor Create(AString: string)');
    RegisterProperty('DataString', 'String', iptR);
  end;
end;

procedure SIRegisterTCOLLECTIONITEM(CL: TPSPascalCompiler);
Begin
  if cl.FindClass('TCollection') = nil then cl.AddClassN(cl.FindClass('TPersistent'), 'TCollection');
  With cl.AddClassN(cl.FindClass('TPersistent'),'TCollectionItem') do
  begin
  //RegisterMethod('constructor Create(Collection: TCollection)');
  RegisterProperty('Collection', 'TCollection', iptrw);
  RegisterProperty('Index', 'Integer', iptrw);
  end;
end;

procedure SIRegisterTCOLLECTION(CL: TPSPascalCompiler; Web: Boolean);
var
  cr: TPSCompileTimeClass;
Begin
  cr := CL.FindClass('TCollection');
  if cr = nil then cr := cl.AddClassN(cl.FindClass('TPersistent'), 'TCollection');
  With cr do
  begin
//  RegisterMethod('constructor Create(ItemClass: TCollectionItemClass)');
  if not Web then
  begin
    RegisterMethod('function Add: TCollectionItem');
    RegisterMethod('procedure BeginUpdate');
    RegisterMethod('procedure EndUpdate');
  end;
  RegisterMethod('procedure Clear');
  RegisterProperty('Count', 'Integer', iptr);
  RegisterProperty('Items', 'TCollectionItem Integer', iptrw);
  end;
end;

procedure SIRegister_Classes_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  cl.AddConstantN('soFromBeginning', 'LongInt').Value^.ts32 := 0;
  cl.AddConstantN('soFromCurrent', 'LongInt').Value^.ts32 := 1;
  cl.AddConstantN('soFromEnd', 'LongInt').Value^.ts32 := 2;
  cl.AddConstantN('toEOF', 'Char').SetString(#0);
  cl.AddConstantN('toSymbol', 'Char').SetString(#1);
  cl.AddConstantN('toString', 'Char').SetString(#2);
  cl.AddConstantN('ToInteger', 'Char').SetString(#3);
  cl.AddConstantN('toFloat', 'Char').SetString(#4);
  cl.AddConstantN('fmCreate', 'LongInt').Value^.ts32 := $FF00;//$FFFF;
  cl.AddConstantN('fmOpenRead', 'LongInt').Value^.ts32 := 0;
  cl.AddConstantN('fmOpenWrite', 'LongInt').Value^.ts32 := 1;
  cl.AddConstantN('fmOpenReadWrite', 'LongInt').Value^.ts32 := 2;
  cl.AddConstantN('fmShareCompat', 'LongInt').Value^.ts32 := 0;
  cl.AddConstantN('fmShareExclusive', 'LongInt').Value^.ts32 := $10;
  cl.AddConstantN('fmShareDenyWrite', 'LongInt').Value^.ts32 := $20;
  cl.AddConstantN('fmShareDenyRead', 'LongInt').Value^.ts32 := $30;
  cl.AddConstantN('fmShareDenyNone', 'LongInt').Value^.ts32 := $40;
  cl.AddConstantN('SecsPerDay', 'LongInt').Value^.ts32 := 86400;
  cl.AddConstantN('MSecPerDay', 'LongInt').Value^.ts32 := 86400000;
  cl.AddConstantN('DateDelta', 'LongInt').Value^.ts32 := 693594;
  cl.AddTypeS('TAlignment', '(taLeftJustify, taRightJustify, taCenter)');
  //cl.AddTypeS('THelpEvent', 'function (Command: Word; Data: LongInt; var CallHelp: Boolean): Boolean');
  //cl.AddTypeS('TGetStrProc', 'procedure(const S: string)');
  cl.AddTypeS('TDuplicates', '(dupIgnore, dupAccept, dupError)');
  //cl.AddTypeS('TOperation', '(opInsert, opRemove)');
  cl.AddTypeS('THandle', 'LongInt');

  cl.AddTypeS('TNotifyEvent', 'procedure (Sender: TObject)');
end;

procedure SIRegister_Classes(Cl: TPSPascalCompiler; Web: Boolean);
begin
  SIRegister_Classes_TypesAndConsts(Cl);
  SIRegisterTSTREAM(Cl);
  SIRegisterTStrings(cl);
  SIRegisterTStringList(cl);
  SIRegisterTFILESTREAM(Cl);
  SIRegisterTMEMORYSTREAM(Cl);
  SIRegisterTSTRINGSTREAM(Cl);
  SIRegisterTCOLLECTIONITEM(Cl);
  SIRegisterTCOLLECTION(Cl, Web);
end;

// PS_MINIVCL changes by Martijn Laan (mlaan at wintax _dot_ nl)


end.
