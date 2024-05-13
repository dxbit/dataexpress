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

    Based on the original uPSC_DateUtils.pas file of the RemObject Pascal Script
    component.
-------------------------------------------------------------------------------}

{ Compile time Date Time library }
unit uPSC_mydateutils;

interface
uses
  SysUtils, uPSCompiler, uPSUtils;


procedure RegisterDateTimeLibrary_C(S: TPSPascalCompiler);

implementation

procedure RegisterDatetimeLibrary_C(S: TPSPascalCompiler);
begin
  s.AddType('TDateTime', btDouble).ExportName := True;
  s.AddDelphiFunction('function EncodeDate(Year, Month, Day: Word): TDateTime;');
  s.AddDelphiFunction('function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;');
  s.AddDelphiFunction('function TryEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;');
  s.AddDelphiFunction('function TryEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;');
  s.AddDelphiFunction('procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word);');
  s.AddDelphiFunction('procedure DecodeTime(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);');
  s.AddDelphiFunction('function DayOfWeek(const DateTime: TDateTime): Word;');
  s.AddDelphiFunction('function Date: TDateTime;');
  s.AddDelphiFunction('function Time: TDateTime;');
  s.AddDelphiFunction('function Now: TDateTime;');
  //s.AddDelphiFunction('function DateTimeToUnix(D: TDateTime): Int64;');
  //s.AddDelphiFunction('function UnixToDateTime(U: Int64): TDateTime;');

  s.AddDelphiFunction('function DateToStr(D: TDateTime): string;');
  s.AddDelphiFunction('function StrToDate(const S: string): TDateTime;');
  s.AddDelphiFunction('function FormatDateTime(const fmt: string; D: TDateTime): string;');
end;

end.
