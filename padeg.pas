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

unit padeg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, LazUtf8;

function GetFIOPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
function GetIFPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
function GetNominativePadeg(aName: String): String;
function GetAppointmentPadeg(aName: String; aPadeg: LongInt): String;
function GetFullAppointmentPadeg(aName, aOffice: String; aPadeg: LongInt): String;
function GetOfficePadeg(aName: String; aPadeg: LongInt): String;
function GetSex(aMidName: String): Byte;
function GetPadegID(aName: String): Byte;
function NumberToString(Value: Extended; iSex, Decimal: Integer; RemoveZero,
  CnvtFrac: Boolean): String;
function DoubleToVerbal(Value: Extended): String;
function DeclNumeral(Value: String; aPadeg, aSex: Integer; Order, Soul:
  Boolean): String;
function DeclCurrency(Value: Currency; CurrName: String; aPadeg: Integer;
  aForms: Byte): String;

function GetFIOBriefPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
function GetIOFBriefPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
function GetF(aName: String): String;
function GetI(aName: String): String;
function GetO(aName: String): String;

implementation

{type

  PPartsFIO = ^TPartsFIO;

  TPartsFIO = record
    pLastName, pFirstName, pMiddleName: String;
    nLastName, nFirstName, nMiddleName: LongInt;
  end;       }

var
  _GetFIOPadegFS: function(pFIO: PWideChar; bSex: Boolean; nPadeg: LongInt; pResult: PWideChar; var nLen: LongInt): Integer; stdcall;
  _GetFIOPadegFSAS: function(pFIO: PWideChar; nPadeg: LongInt; pResult: PWideChar; var nLen: LongInt): Integer; stdcall;
  _GetIFPadegFS: function(pIF: PWideChar; bSex: Boolean; nPadeg: LongInt; pResult: PWideChar; var nLen: LongInt): Integer; stdcall;
  _GetNominativePadeg: function(pFIO, pResult: PWideChar; var nLen: LongInt): Integer; stdcall;
  _GetAppointmentPadeg: function(pAppointment: PWideChar; nPadeg: LongInt; pResult: PWideChar; var nLen: LongInt): Integer; stdcall;
  _GetFullAppointmentPadeg: function(pAppointment, pOffice: PWideChar; nPadeg: LongInt; pResult: PWideChar;
    var nLen: LongInt): Integer; stdcall;
  _GetOfficePadeg: function(pOffice: PWideChar; nPadeg: LongInt; pResult: PWideChar; var nLen: LongInt): Integer; stdcall;
  _GetSex: function(pMiddleName: PWideChar): Integer;  stdcall;
  _GetPadegId: function (pFIO: PWideChar): Integer; stdcall;
  _SetDictionary: function(DicName: PWideChar): Boolean; stdcall;
  _NumberToString: function(Value: Extended; iSex: Integer; Decimal: Integer; RemoveZero, CnvtFrac: Boolean;
    pResult: PWideChar; var nLen: LongInt): Integer; stdcall;
  _DoubleToVerbal: function(Value: Extended; pResult: PWideChar; var nLen: LongInt): Integer; stdcall;
  _DeclNumeral: function(Value: PWideChar; nPadeg: Integer; iSex: Integer; Order, Soul: Boolean; pResult: PWideChar;
    var nLen: Integer): Integer; stdcall;
  _DeclCurrency: function(Value: Currency; CurrName: PWideChar; nPadeg: Integer; aForms: Byte; pResult: PWideChar;
    var nLen: Integer): Integer; stdcall;

  {_GetFIOParts: function(pFIO: PChar; Parts: PPartsFIO): Integer; stdcall;
  _GetIFPadeg: function(pFirstName, pLastName: PChar; bSex: Boolean; nPadeg: LongInt; pResult: PChar;
    var nLen: LongInt): Integer; stdcall;
  _GetFIOPadeg: function(pLastName, pFirstName, pMiddleName: PChar; bSex: Boolean; nPadeg: LongInt; pResult: PChar;
    var nLen: LongInt): Integer; stdcall;
  _GetFIOPadegAS: function(pLastName, pFirstName, pMiddleName: PChar; nPadeg: LongInt; pResult: PChar;
    var nLen: LongInt): Integer; stdcall;
  _UpdateExceptions: function: Boolean; stdcall;

  _GetNameFileExceptions: function(pResult: PChar; var nLen: LongInt): Integer; stdcall;
  _FormingFIO: function(pFIO: PChar; pResult: PChar; var nLen: Integer): Integer; stdcall;
  _SumInWords: function(Value: PChar; iSex: Integer; Decimal: Integer; RemoveZero, CnvtFrac: Boolean; pResult: PChar;
    var nLen: LongInt): Integer; stdcall;
  _GetCurrencyFile: function(pResult: PChar; var nLen: LongInt): Integer; stdcall;
  _CatalogISO: function(pResult: PChar; var nLen: LongInt): Integer; stdcall;
  _NameCurrency: function(pISO: PChar; pResult: PChar; var nLen: Integer): Integer; stdcall;    }

var
  LibHandle: TLibHandle;

{$ifdef windows}
{function _GetFIOPadegFS(pName: PChar; bSex: Boolean; nPadeg: LongInt; pResult: PChar;
  var nLen: LongInt): Integer; stdcall; external 'padeg.dll' name 'GetFIOPadegFS';
function _GetFIOPadegFSAS(pName: PChar; nPadeg: LongInt; pResult: PChar;
  var nLen: LongInt): Integer; stdcall; external 'padeg.dll' name 'GetFIOPadegFSAS';
function _GetIFPadegFS(pIF: PChar; bSex: Boolean; nPadeg: LongInt;
  pResult: PChar; var nLen: LongInt): Integer; stdcall; external 'padeg.dll' name 'GetIFPadegFS';
function _GetNominativePadeg(pFIO, pResult: PChar; var nLen: LongInt): Integer; stdcall;
  external 'padeg.dll' name 'GetNominativePadeg';
function _GetAppointmentPadeg(pAppointment: PChar; nPadeg: LongInt;
  pResult: PChar; var nLen: LongInt): Integer; stdcall; external 'padeg.dll' name 'GetAppointmentPadeg';
function _GetFullAppointmentPadeg(pAppointment, pOffice: PChar; nPadeg: LongInt; pResult: PChar;
  var nLen: LongInt): Integer; stdcall; external 'padeg.dll' name 'GetFullAppointmentPadeg';
function _GetOfficePadeg(pOffice: PChar; nPadeg: LongInt; pResult: PChar;
  var nLen: LongInt): Integer; stdcall; external 'padeg.dll' name 'GetOfficePadeg';
function _GetSex(pMiddleName: PChar): Integer; stdcall; external 'padeg.dll' name 'GetSex';
function _GetPadegID(pFIO: PChar): Integer; stdcall; external 'padeg.dll' name 'GetPadegID';}
{$endif}

procedure DoError(const Msg: String);
begin
  raise Exception.Create(Msg);
end;

function WideSetDictionary(const FileName: UnicodeString): Boolean;
begin
  Result := _SetDictionary( PWideChar(FileName) );
end;

function SetDictionary(const FileName: String): Boolean;
begin
  Result := WideSetDictionary( Utf8ToUtf16(FileName) );
end;

procedure InitPadeg;
var
  LibPath: String;
begin
  if LibHandle <> 0 then Exit;
  LibPath := ExtractFilePath(ParamStr(0)) + 'PadegUC.dll';

  LibHandle := SafeLoadLibrary(LibPath);
  if LibHandle = 0 then DoError('Padeg library has been not initialized.');

  Pointer(_GetSex) := GetProcAddress(LibHandle, 'GetSex');
  Pointer(_GetIFPadegFS) := GetProcAddress(LibHandle, 'GetIFPadegFS');
  Pointer(_GetFIOPadegFSAS) := GetProcAddress(LibHandle, 'GetFIOPadegFSAS');
  Pointer(_GetFIOPadegFS) := GetProcAddress(LibHandle, 'GetFIOPadegFS');
  Pointer(_GetNominativePadeg) := GetProcAddress(LibHandle, 'GetNominativePadeg');
  Pointer(_GetAppointmentPadeg) := GetProcAddress(LibHandle, 'GetAppointmentPadeg');
  Pointer(_GetFullAppointmentPadeg) := GetProcAddress(LibHandle, 'GetFullAppointmentPadeg');
  Pointer(_GetOfficePadeg) := GetProcAddress(LibHandle, 'GetOfficePadeg');
  Pointer(_SetDictionary) := GetProcAddress(LibHandle, 'SetDictionary');
  Pointer(_DoubleToVerbal) := GetProcAddress(LibHandle, 'DoubleToVerbal');
  Pointer(_NumberToString) := GetProcAddress(LibHandle, 'NumberToString');
  Pointer(_DeclCurrency) := GetProcAddress(LibHandle, 'DeclCurrency');
  Pointer(_DeclNumeral) := GetProcAddress(LibHandle, 'DeclNumeral');
  Pointer(_GetPadegID) := GetProcAddress(LibHandle, 'GetPadegID');

{  Pointer(_GetNameFileExceptions) := GetProcAddress(LibHandle, 'GetExceptionsFileName');
  Pointer(_UpdateExceptions) := GetProcAddress(LibHandle, 'UpdateExceptions');
  Pointer(_GetFIOParts) := GetProcAddress(LibHandle, 'GetFIOParts');
  Pointer(_GetIFPadeg) := GetProcAddress(LibHandle, 'GetIFPadeg');
  Pointer(_FormingFIO) := GetProcAddress(LibHandle, 'FormingFIO');
  Pointer(_GetFIOPadeg) := GetProcAddress(LibHandle, 'GetFIOPadeg');
  Pointer(_GetFIOPadegAS) := GetProcAddress(LibHandle, 'GetFIOPadegAS');
  Pointer(_SumInWords) := GetProcAddress(LibHandle, 'SumInWords');
  Pointer(_GetCurrencyFile) := GetProcAddress(LibHandle, 'GetCurrencyFile');
  Pointer(_CatalogISO) := GetProcAddress(LibHandle, 'CatalogISO');
  Pointer(_NameCurrency) := GetProcAddress(LibHandle, 'NameCurrency');}
end;

procedure CutStr(var S: String; out CutS: String);
var
  L, i, p0: Integer;
begin
  L := Length(S);
  i := 1;
  while (i <= L) and (S[i] <= #32) do Inc(i);
  p0 := i;
  while (i <= L) and (S[i] > #32) do Inc(i);

  CutS := Copy(S, p0, i - p0);
  Delete(S, 1, i);
end;

procedure SplitFIO(const FIO: String; out F, I, O: String);
var
  S: String;
begin
  S := FIO;
  CutStr(S, F);
  CutStr(S, I);
  CutStr(S, O);
end;

procedure CheckSex(i: Byte);
begin
  if not (i in [0..2]) then
    raise Exception.Create('Параметр "Род" может принимать значения от 0 до 2');
end;

procedure CheckPadeg(i: Byte);
begin
  if not (i in [1..6]) then
    raise Exception.Create('Параметр "Падеж" может принимать значения от 1 до 6');
end;

procedure CheckForms(i: Byte);
begin
  if not (i in [0..3]) then
    raise Exception.Create('Параметр "Форма" может принимать значения от 0 до 3');
end;

function WideGetFIOPadeg(aName: UnicodeString; aSex: Byte; aPadeg: LongInt
  ): UnicodeString;
var
  n, Len: Integer;
  Rslt: PWideChar;
begin
  Result := '';
  InitPadeg;

  CheckSex(aSex);
  CheckPadeg(aPadeg);

  Len := Length(aName) + 10;
  Rslt := WideStrAlloc(Len);
  try
    if aSex in [1..2] then
      n := _GetFIOPadegFS(PWideChar(aName), aSex = 1, aPadeg, Rslt, Len)
    else
      n := _GetFIOPadegFSAS(PWideChar(aName), aPadeg, Rslt, Len);
    if n = 0 then
    begin
      SetString(Result, Rslt, Len);
    end;
  finally
    StrDispose(Rslt);
  end;
end;

function GetFIOPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
begin
  Result := Utf16ToUtf8( WideGetFIOPadeg(Utf16ToUtf8(aName), aSex, aPadeg) );
end;

function WideGetIFPadeg(aName: UnicodeString; aSex: Byte; aPadeg: LongInt): UnicodeString;
var
  n, Len: Integer;
  Rslt: PWideChar;
begin
  Result := '';
  InitPadeg;

  CheckSex(aSex);
  CheckPadeg(aPadeg);

  Len := Length(aName) + 10;
  Rslt := WideStrAlloc(Len);
  try
    n := _GetIFPadegFS(PWideChar(aName), aSex = 1, aPadeg, Rslt, Len);
    if n = 0 then
      SetString(Result, Rslt, Len);
  finally
    StrDispose(Rslt);
  end;
end;

function GetIFPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
begin
  Result := Utf16ToUtf8( WideGetIFPadeg(Utf8ToUtf16(aName), aSex, aPadeg) );
end;

function WideGetNominativePadeg(aName: UnicodeString): UnicodeString;
var
  Len, n: Integer;
  Rslt: PWideChar;
begin
  Result := '';
  InitPadeg;

  Len := Length(aName) + 10;
  Rslt := WideStrAlloc(Len);
  try
    n := _GetNominativePadeg(PWideChar(aName), Rslt, Len);
    if n = 0 then
      SetString(Result, Rslt, Len);
  finally
    StrDispose(Rslt);
  end;
end;

function GetNominativePadeg(aName: String): String;
begin
  Result := Utf16ToUtf8( WideGetNominativePadeg(Utf8ToUtf16(aName)) );
end;

function WideGetAppointmentPadeg(aName: UnicodeString; aPadeg: LongInt): UnicodeString;
var
  Len, n: Integer;
  Rslt: PWideChar;
begin
  Result := '';
  InitPadeg;

  CheckPadeg(aPadeg);

  Len := Length(aName) + 10;
  Rslt := WideStrAlloc(Len);
  try
    n := _GetAppointmentPadeg(PWideChar(aName), aPadeg, Rslt, Len);
    if n = 0 then
      SetString(Result, Rslt, Len);
  finally
    StrDispose(Rslt);
  end;
end;

function GetAppointmentPadeg(aName: String; aPadeg: LongInt): String;
begin
  Result := Utf16ToUtf8( WideGetAppointmentPadeg(Utf8ToUtf16(aName), aPadeg) );
end;

function WideGetFullAppointmentPadeg(aName, aOffice: UnicodeString; aPadeg: LongInt
  ): UnicodeString;
var
  Len, n: Integer;
  Rslt: PWideChar;
begin
  Result := '';
  InitPadeg;

  CheckPadeg(aPadeg);

  Len := Length(aName) + Length(aOffice) + 20;
  Rslt := WideStrAlloc(Len);
  try
    n := _GetFullAppointmentPadeg(PWideChar(aName), PWideChar(aOffice), aPadeg, Rslt, Len);
    if n = 0 then
      SetString(Result, Rslt, Len);
  finally
    StrDispose(Rslt);
  end;
end;

function GetFullAppointmentPadeg(aName, aOffice: String; aPadeg: LongInt
  ): String;
begin
  Result := Utf16ToUtf8( WideGetFullAppointmentPadeg(Utf8ToUtf16(aName),
    Utf8ToUtf16(aOffice), aPadeg) );
end;

function WideGetOfficePadeg(aName: UnicodeString; aPadeg: LongInt): UnicodeString;
var
  Len, n: Integer;
  Rslt: PWideChar;
begin
  Result := '';
  InitPadeg;

  CheckPadeg(aPadeg);

  Len := Length(aName) + 10;
  Rslt := WideStrAlloc(Len);
  try
    n := _GetOfficePadeg(PWideChar(aName), aPadeg, Rslt, Len);
    if n = 0 then
      SetString(Result, Rslt, Len);
  finally
    StrDispose(Rslt);
  end;
end;

function GetOfficePadeg(aName: String; aPadeg: LongInt): String;
begin
  Result := Utf16ToUtf8( WideGetOfficePadeg(Utf8ToUtf16(aName), aPadeg) );
end;

function WideGetSex(aMidName: UnicodeString): Byte;
var
  n: Integer;
begin
  InitPadeg;

  n := _GetSex(PWideChar(aMidName));
  case n of
    0: Result := 2;
    1: Result := 1;
    -1: Result := 0;
  end;
end;

function GetSex(aMidName: String): Byte;
begin
  Result := WideGetSex( Utf8ToUtf16(aMidName) );
end;

function WideGetPadegID(aName: UnicodeString): Byte;
begin
  Result := _GetPadegID(PWideChar(aName));
end;

function GetPadegID(aName: String): Byte;
begin
  Result := WideGetPadegID( Utf8ToUtf16(aName) );
end;

function WideNumberToString(Value: Extended; iSex, Decimal: Integer; RemoveZero,
  CnvtFrac: Boolean): UnicodeString;
var
  Len, n: Integer;
  Rslt: PWideChar;
begin
  Result := '';
  InitPadeg;

  CheckSex(iSex);

  Len := 1000;
  Rslt := WideStrAlloc(Len);

  case iSex of
    0: iSex := -1;
    1: iSex := 1;
    2: iSex := 0;
  end;

  try
    n := _NumberToString(Value, iSex, Decimal, RemoveZero, CnvtFrac, Rslt, Len);
    if n = 0 then
      SetString(Result, Rslt, Len);
  finally
    StrDispose(Rslt);
  end;
end;

function NumberToString(Value: Extended; iSex, Decimal: Integer; RemoveZero,
  CnvtFrac: Boolean): String;
begin
  Result := Utf16ToUtf8( WideNumberToString(Value, iSex, Decimal, RemoveZero, CnvtFrac) );
end;

function WideDoubleToVerbal(Value: Extended): UnicodeString;
var
  Len, n: Integer;
  Rslt: PWideChar;
begin
  Result := '';
  InitPadeg;

  Len := 1000;
  Rslt := WideStrAlloc(Len);

  try
    n := _DoubleToVerbal(Value, Rslt, Len);
    if n = 0 then
      SetString(Result, Rslt, Len);
  finally
    StrDispose(Rslt);
  end;
end;

function DoubleToVerbal(Value: Extended): String;
begin
  Result := Utf16ToUtf8( WideDoubleToVerbal(Value) );
end;

function WideDeclNumeral(Value: UnicodeString; aPadeg, aSex: Integer; Order,
  Soul: Boolean): UnicodeString;
var
  Len, n: Integer;
  Rslt: PWideChar;
begin
  Result := '';
  InitPadeg;

  CheckPadeg(aPadeg);
  CheckSex(aSex);

  Len := 1000;
  Rslt := WideStrAlloc(Len);

  case aSex of
    0: aSex := -1;
    1: aSex := 1;
    2: aSex := 0;
  end;

  try
    n := _DeclNumeral(PWideChar(Value), aPadeg, aSex, Order, Soul, Rslt, Len);
    if n = 0 then
      SetString(Result, Rslt, Len);
  finally
    StrDispose(Rslt);
  end;
end;

function DeclNumeral(Value: String; aPadeg, aSex: Integer; Order, Soul: Boolean
  ): String;
begin
  // Переводим в нижний регистр, т. к. функция не работает корректно, если есть
  // символы в верхнем регистре.
  Value := Utf8LowerCase(Value);

  Result := Utf16ToUtf8( WideDeclNumeral(Utf8ToUtf16(Value), aPadeg, aSex,
    Order, Soul) );
end;

function WideDeclCurrency(Value: Currency; CurrName: UnicodeString; aPadeg: Integer;
  aForms: Byte): UnicodeString;
var
  Len, n: Integer;
  Rslt: PWideChar;
begin
  Result := '';
  InitPadeg;

  CheckPadeg(aPadeg);
  CheckForms(aForms);

  Len := 1000;
  Rslt := WideStrAlloc(Len);

  try
    n := _DeclCurrency(Value, PWideChar(CurrName), aPadeg, aForms, Rslt, Len);
    if n = 0 then
      SetString(Result, Rslt, Len);
  finally
    StrDispose(Rslt);
  end;
end;

function DeclCurrency(Value: Currency; CurrName: String; aPadeg: Integer;
  aForms: Byte): String;
begin
  Result := Utf16ToUtf8( WideDeclCurrency(Value, Utf8ToUtf16(CurrName), aPadeg,
    aForms) );
end;

function GetFIOBriefPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
var
  F, I, O: String;
begin
  Result := GetFIOPadeg(aName, aSex, aPadeg);
  SplitFIO(Result, F, I, O);
  Result := F + ' ' + Utf8Copy(I, 1, 1) + '.';
  if O > '' then Result := Result + Utf8Copy(O, 1, 1) + '.';
end;

function GetIOFBriefPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
var
  F, I, O: String;
begin
  Result := GetFIOPadeg(aName, aSex, aPadeg);
  SplitFIO(Result, F, I, O);
  Result := Utf8Copy(I, 1, 1) + '.';
  if O > '' then Result := Result + Utf8Copy(O, 1, 1) + '.';
  Result := Result + ' ' + F;
end;

function GetF(aName: String): String;
begin
  CutStr(aName, Result);
end;

function GetI(aName: String): String;
var
  Tmp: String;
begin
  CutStr(aName, Tmp);
  CutStr(aName, Result);
end;

function GetO(aName: String): String;
var
  Tmp: String;
begin
  CutStr(aName, Tmp);
  CutStr(aName, Tmp);
  CutStr(aName, Result);
end;

end.

