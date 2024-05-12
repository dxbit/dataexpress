unit GtkThemeManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

procedure GetThemes(List: TStrings);
procedure ApplyTheme(const ThemeName: String);
function GetCurrentTheme: String;
procedure CreateThemeFile;

implementation

uses
  FileUtil;

function GetThemeFile: String;
begin
  Result := ExtractFilePath(Application.ExeName) + 'dataexpress.gtkrc';
end;

procedure GetThemes(List: TStrings);
var
  SL, Strings: TStringList;
  S: String;
  i, Len: Integer;
begin
  List.Clear;
  SL := TStringList.Create;
  Strings := TStringList.Create;
  FindAllFiles(SL, '/usr/share/themes/', 'gtkrc', True, faDirectory);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    Len := Length(S);
    if Copy(S, Len - 13, 14) = '/gtk-2.0/gtkrc' then
    begin
      Delete(S, 1, 18);
      Strings.Add(Copy(S, 1, Pos('/', S) - 1));
    end;
  end;
  Strings.Sort;
  List.AddStrings(Strings);
  SL.Free;
  Strings.Free;
end;

procedure ApplyTheme(const ThemeName: String);
var
  SL: TStringList;
  S, FileName: String;
begin
  FileName := GetThemeFile;
  CreateThemeFile;
  SL := TStringList.Create;
  SL.LoadFromFile(FileName, True);
  if SL.Count = 0 then
    SL.Add('');
  SL[0] := 'include "/usr/share/themes/' + ThemeName + '/gtk-2.0/gtkrc"';
  SL.SaveToFile(FileName, True);
  SL.Free;
end;

function GetCurrentTheme: String;
const
  Pfx = 'include "/usr/share/themes/';
var
  SL: TStringList;
  S: String;
  p: SizeInt;
begin
  Result := '';
  SL := TStringList.Create;
  SL.LoadFromFile(GetThemeFile, True);
  if SL.Count > 0 then
  begin
    S := SL[0];
    p := Pos(Pfx, S);
    if p > 0 then
    begin
      Delete(S, 1, p + Length(Pfx) - 1);
      Result := Copy(S, 1, Pos('/', S) - 1);
    end;
  end;
  SL.Free;
end;

procedure CreateThemeFile;
var
  S: String;
begin
  if not FileExists(GetThemeFile) then
    with TFileStream.Create(GetThemeFile, fmCreate) do
    try
      S := 'include "/usr/share/themes/Default/gtk-2.0/gtkrc"';
      Write(Pointer(S)^, Length(S));
    finally
      Free;
    end;
end;

end.

