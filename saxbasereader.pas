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
unit SAXBaseReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SAX, SAX_XML;

type

  { TSAXBaseReader }

	TSAXBaseReader = class(TSaxXmlReader)
  public
    function GetBool(Atts: TSAXAttributes; const aName: String): Boolean;
    function GetInt(Atts: TSAXAttributes; const aName: String): Integer;
    function GetStr(Atts: TSAXAttributes; const aName: String): String;
  end;

implementation

{ TSAXBaseReader }

function TSAXBaseReader.GetBool(Atts: TSAXAttributes; const aName: String
  ): Boolean;
var
  S: SAXString;
begin
  Result := False;
  S := GetStr(Atts, aName);
  if S = '1' then Result := True;
end;

function TSAXBaseReader.GetInt(Atts: TSAXAttributes; const aName: String
  ): Integer;
var
  S: SAXString;
begin
  Result := 0;
  S := GetStr(Atts, aName);
  if S <> '' then TryStrToInt(S, Result);
end;

function TSAXBaseReader.GetStr(Atts: TSAXAttributes; const aName: String
  ): String;
begin
  Result := Atts.GetValue('', aName);
end;

end.

