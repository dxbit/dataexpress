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
unit MyTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type

  { TIntList }

  TIntList = class(TList)
  private
    function GetValues(Index: Integer): Integer;
  public
    function AddValue(Value: Integer): Integer;
    property Values[Index: Integer]: Integer read GetValues; default;
  end;

  { TStringListUtf8 }

  TStringListUtf8 = class(TStringList)
  protected
    function DoCompareText(const s1, s2: string): PtrInt; override;
  end;

  { TIniFileEx }

  TIniFileEx = class(TIniFile)
  private
    FS: TFileStream;
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds: Boolean=False); override;
    destructor Destroy; override;
  end;

implementation

uses
  LazUtf8;

{ TIniFileEx }

constructor TIniFileEx.Create(const AFileName: string; AEscapeLineFeeds: Boolean
  );
var
  MarkHolder: Cardinal;
begin
  FS := TFileStream.Create(AFileName, fmOpenReadWrite);
  MarkHolder := FS.ReadDWord;
  if (MarkHolder and $00FFFFFF) = $00BFBBEF then
    FS.Position := 3
  else
    FS.Position := 0;
  inherited Create(FS, AEscapeLineFeeds);
end;

destructor TIniFileEx.Destroy;
begin
  FS.Position := 0;   // Устанавливаю в начало, т. к. в Ubuntu записывается мусор.
  inherited Destroy;
  FS.Free;
end;

{ TStringListUtf8 }

function TStringListUtf8.DoCompareText(const s1, s2: string): PtrInt;
begin
  if not CaseSensitive then
    Result := Utf8CompareText(s1, s2)
  else
    Result := Utf8CompareStr(s1, s2);
end;

{ TIntList }

function TIntList.GetValues(Index: Integer): Integer;
begin
  Result := Integer(Items[Index]);
end;

function TIntList.AddValue(Value: Integer): Integer;
begin
  Add(Pointer(Value));
end;

end.

