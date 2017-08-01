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
unit padeg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

function GetFIOPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
function GetIFPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
function GetNominativePadeg(aName: String): String;
function GetAppointmentPadeg(aName: String; aPadeg: LongInt): String;
function GetFullAppointmentPadeg(aName, aOffice: String; aPadeg: LongInt): String;
function GetOfficePadeg(aName: String; aPadeg: LongInt): String;
function GetSex(aMidName: String): Byte;
function GetPadegID(aName: String): Byte;

function GetFIOBriefPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
function GetIOFBriefPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
function GetF(aName: String): String;
function GetI(aName: String): String;
function GetO(aName: String): String;

implementation

uses
  LazUtf8;

{$ifdef windows}
function _GetFIOPadegFS(pName: PChar; bSex: Boolean; nPadeg: LongInt; pResult: PChar;
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
function _GetPadegID(pFIO: PChar): Integer; stdcall; external 'padeg.dll' name 'GetPadegID';
{$endif}

procedure CutStr(var S, CutS: String);
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

function GetFIOPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
var
  n, Len: Integer;
  Rslt: PChar;
begin
  Result := '';
  CheckSex(aSex);
  CheckPadeg(aPadeg);

  {$ifdef windows}
  aName := Utf8ToWinCP(aName);
  Len := Length(aName) + 10;
  Rslt := StrAlloc(Len);
  try
    if aSex in [1..2] then
      n := _GetFIOPadegFS(PChar(aName), aSex = 1, aPadeg, Rslt, Len)
    else
      n := _GetFIOPadegFSAS(PChar(aName), aPadeg, Rslt, Len);
    if n = 0 then
    begin
      SetString(Result, Rslt, Len);
      Result := WinCPToUtf8(Result);
    end;
  finally
    StrDispose(Rslt);
  end;
  {$else}
  Result := aName;
  {$endif}
end;

function GetIFPadeg(aName: String; aSex: Byte; aPadeg: LongInt): String;
var
  n, Len: Integer;
  Rslt: PChar;
begin
  Result := '';
  CheckSex(aSex);
  CheckPadeg(aPadeg);

  {$ifdef windows}
  aName := Utf8ToWinCP(aName);
  Len := Length(aName) + 10;
  Rslt := StrAlloc(Len);
  try
    n := _GetIFPadegFS(PChar(aName), aSex = 1, aPadeg, Rslt, Len);
    if n = 0 then
    begin
      SetString(Result, Rslt, Len);
      Result := WinCPToUtf8(Result);
    end;
  finally
    StrDispose(Rslt);
  end;
  {$else}
  Result := aName;
  {$endif}
end;

function GetNominativePadeg(aName: String): String;
var

  Len, n: Integer;
  Rslt: PChar;
begin
  Result := '';
  {$ifdef windows}
  aName := Utf8ToWinCP(aName);
  Len := Length(aName) + 10;
  Rslt := StrAlloc(Len);
  try
    n := _GetNominativePadeg(PChar(aName), Rslt, Len);
    if n = 0 then
    begin
      SetString(Result, Rslt, Len);
      Result := WinCPToUtf8(Result);
    end;
  finally
    StrDispose(Rslt);
  end;
  {$else}
  Result := aName;
  {$endif}
end;

function GetAppointmentPadeg(aName: String; aPadeg: LongInt): String;
var
  Len, n: Integer;
  Rslt: PChar;
begin
  Result := '';
  CheckPadeg(aPadeg);

  {$ifdef windows}
  aName := Utf8ToWinCP(aName);
  Len := Length(aName) + 10;
  Rslt := StrAlloc(Len);
  try
    n := _GetAppointmentPadeg(PChar(aName), aPadeg, Rslt, Len);
    if n = 0 then
    begin
      SetString(Result, Rslt, Len);
      Result := WinCPToUtf8(Result);
    end;
  finally
    StrDispose(Rslt);
  end;
  {$else}
  Result := aName;
  {$endif}
end;

function GetFullAppointmentPadeg(aName, aOffice: String; aPadeg: LongInt
  ): String;
var
  Len, n: Integer;
  Rslt: PChar;
begin
  Result := '';
  CheckPadeg(aPadeg);

  {$ifdef windows}
  aName := Utf8ToWinCP(aName);
  aOffice := Utf8ToWinCP(aOffice);
  Len := Length(aName) + Length(aOffice) + 20;
  Rslt := StrAlloc(Len);
  try
    n := _GetFullAppointmentPadeg(PChar(aName), PChar(aOffice), aPadeg, Rslt, Len);
    if n = 0 then
    begin
      SetString(Result, Rslt, Len);
      Result := WinCPToUtf8(Result);
    end;
  finally
    StrDispose(Rslt);
  end;
  {$else}
  Result := aName;
  {$endif}
end;

function GetOfficePadeg(aName: String; aPadeg: LongInt): String;
var
  Len, n: Integer;
  Rslt: PChar;
begin
  Result := '';
  CheckPadeg(aPadeg);

  {$ifdef windows}
  aName := Utf8ToWinCP(aName);
  Len := Length(aName) + 10;
  Rslt := StrAlloc(Len);
  try
    n := _GetOfficePadeg(PChar(aName), aPadeg, Rslt, Len);
    if n = 0 then
    begin
      SetString(Result, Rslt, Len);
      Result := WinCPToUtf8(Result);
    end;
  finally
    StrDispose(Rslt);
  end;
  {$else}
  Result := aName;
  {$endif}
end;

function GetSex(aMidName: String): Byte;
var
  n: Integer;
begin
  {$ifdef windows}
  aMidName := Utf8ToWinCP(aMidName);
  n := _GetSex(PChar(aMidName));
  case n of
    0: Result := 2;
    1: Result := 1;
    -1: Result := 0;
  end;
  {$else}
  Result := 0;
  {$endif}
end;

function GetPadegID(aName: String): Byte;
begin
  {$ifdef windows}
  aName := Utf8ToWinCP(aName);
  Result := _GetPadegID(PChar(aName));
  {$else}
  Result := 0;
  {$endif}
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

