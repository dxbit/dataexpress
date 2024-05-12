unit AppImageLists;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

procedure CreateImageLists;
procedure ClearImageLists;
procedure FreeImageLists;

const
  IMG16_ADD = 0;
  IMG16_EDIT = 1;
  IMG16_DELETE = 2;
  IMG16_COPY = 3;
  IMG16_SHOPPING = 4;
  IMG16_UP = 5;
  IMG16_DOWN = 6;
  IMG16_REFRESH = 7;
  IMG16_GOTO = 8;
  IMG16_CUT = 9;
  IMG16_PASTE = 10;
  IMG16_CALC = 11;
  IMG16_DATE = 12;
  IMG16_DROPDOWN = 13;
  IMG16_FORM = 14;
  IMG16_EYES = 15;
  IMG16_DB = 16;
  IMG16_SAVE = 17;
  IMG16_CLOCK = 18;
  IMG16_FILTER = 19;
  IMG16_FOLDER_ADD = 20;
  IMG16_FOLDER_DELETE = 21;
  IMG16_IMAGE = 22;
  IMG16_UP8 = 23;
  IMG16_DOWN8 = 24;

  IMG24_DATE = 0;
  IMG24_CLOCK = 1;
  IMG24_FORM = 2;
  IMG24_CALC = 3;

  IMG32_DATE = 0;
  IMG32_CLOCK = 1;
  IMG32_FORM = 2;
  IMG32_CALC = 3;

  IMG48_DATE = 0;
  IMG48_CLOCK = 1;
  IMG48_FORM = 2;
  IMG48_CALC = 3;

var
  Images16: TImageList;
  Images24: TImageList;
  Images32: TImageList;
  Images48: TImageList;

implementation

const
  Img16: array [0..24] of String = ('add16', 'edit16', 'delete16', 'copy16',
    'shopping16', 'up16', 'down16', 'refresh16', 'goto16', 'cut16', 'paste16',
    'calc16', 'date16', 'cbx_dropdown16', 'form16', 'eyes16', 'db16', 'save16',
    'clock16', 'filter16', 'folder_add16', 'folder_delete16', 'image16', 'up8',
    'down8');
  Img24: array [0..3] of String = ('date24', 'clock24', 'form24', 'calc24');
  Img32: array [0..3] of String = ('date32', 'clock32', 'form32', 'calc32');
  Img48: array [0..3] of String = ('date48', 'clock48', 'form48', 'calc48');

procedure CreateImageLists;
var
  i: Integer;
begin
  Images16 := TImageList.Create(nil);
  Images16.Width := 16;
  Images16.Height := 16;
  for i := 0 to High(Img16) do
    Images16.AddLazarusResource(Img16[i]);

  Images24 := TImageList.Create(nil);
  Images24.Width := 24;
  Images24.Height := 24;
  for i := 0 to High(Img24) do
    Images24.AddLazarusResource(Img24[i]);

  Images32 := TImageList.Create(nil);
  Images32.Width := 32;
  Images32.Height := 32;
  for i := 0 to High(Img32) do
    Images32.AddLazarusResource(Img32[i]);

  Images48 := TImageList.Create(nil);
  Images48.Width := 48;
  Images48.Height := 48;
  for i := 0 to High(Img48) do
    Images48.AddLazarusResource(Img48[i]);
end;

procedure ClearImageLists;
var
  i: Integer;
begin
  for i := Images16.Count - 1 downto High(Img16) + 1 do
    Images16.Delete(i);
  for i := Images24.Count - 1 downto High(Img24) + 1 do
    Images24.Delete(i);
  for i := Images32.Count - 1 downto High(Img32) + 1 do
    Images32.Delete(i);
  for i := Images48.Count - 1 downto High(Img48) + 1 do
    Images48.Delete(i);
end;

procedure FreeImageLists;
begin
  FreeAndNil(Images16);
  FreeAndNil(Images24);
  FreeAndNil(Images32);
  FreeAndNil(Images48);
end;

end.

