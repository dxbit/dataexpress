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

unit SAXBaseReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SAX, SAX_XML;

type

  { TSAXBaseReader }

	TSAXBaseReader = class(TSaxXmlReader)
  public
    function GetBool(Atts: TSAXAttributes; const aName: String; DefaultValue: Boolean = False): Boolean;
    function GetInt(Atts: TSAXAttributes; const aName: String): Integer;
    function GetStr(Atts: TSAXAttributes; const aName: String): String;
    function AttrExists(Atts: TSAXAttributes; const aName: String): Boolean;
  end;

implementation

uses AppUtils;

{ TSAXBaseReader }

function TSAXBaseReader.GetBool(Atts: TSAXAttributes; const aName: String;
  DefaultValue: Boolean): Boolean;
var
  S: String;
begin
  Result := DefaultValue;
  S := GetStr(Atts, aName);
  if S = '1' then Result := True
  else if S = '0' then Result := False;
end;

function TSAXBaseReader.GetInt(Atts: TSAXAttributes; const aName: String
  ): Integer;
var
  S: String;
begin
  Result := 0;
  S := GetStr(Atts, aName);
  if S <> '' then TryStrToInt(S, Result);
end;

function TSAXBaseReader.GetStr(Atts: TSAXAttributes; const aName: String
  ): String;
var
  i: Integer;
  S: String;
begin
  Result := '';
  for i := 0 to Atts.Length - 1 do
  begin
    S := Atts.GetLocalName(i);
		if CompareText(Trim(S), aName) = 0 then
    	Exit( Atts.GetValue(i) );
  end;
end;

function TSAXBaseReader.AttrExists(Atts: TSAXAttributes; const aName: String
  ): Boolean;
var
  i: Integer;
  S: String;
begin
  Result := False;
  for i := 0 to Atts.Length - 1 do
  begin
    S := Atts.GetLocalName(i);
		if CompareText(S, aName) = 0 then
    	Exit( True );
  end;
end;

end.

