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
  IMG16_FILE = 23;

  IMG24_DATE = 0;
  IMG24_CLOCK = 1;
  IMG24_FORM = 2;
  IMG24_CALC = 3;
  IMG24_FILE = 4;

  IMG32_DATE = 0;
  IMG32_CLOCK = 1;
  IMG32_FORM = 2;
  IMG32_CALC = 3;
  IMG32_FILE = 4;

  IMG48_DATE = 0;
  IMG48_CLOCK = 1;
  IMG48_FORM = 2;
  IMG48_CALC = 3;
  IMG48_FILE = 4;

var
  Images16: TImageList;
  Images24: TImageList;
  Images32: TImageList;
  Images48: TImageList;

implementation

uses
  apputils;

const
  Img16: array [0..23] of String = ('add16', 'edit16', 'delete16', 'copy16',
    'shopping16', 'up16', 'down16', 'refresh16', 'goto16', 'cut16', 'paste16',
    'calc16', 'date16', 'cbx_dropdown16', 'form16', 'eyes16', 'db16', 'save16',
    'clock16', 'filter16', 'folder_add16', 'folder_delete16', 'image16', 'file16');
  Img24: array [0..4] of String = ('date24', 'clock24', 'form24', 'calc24', 'file24');
  Img32: array [0..4] of String = ('date16_200', 'clock16_200', 'form16_200', 'calc16_200', 'file16_200');
  Img48: array [0..4] of String = ('date24_200', 'clock24_200', 'form24_200', 'calc24_200', 'file24_200');

procedure CreateImageLists;
var
  i: Integer;
begin
  Images16 := TImageList.Create(nil);
  Images16.Width := 16;
  Images16.Height := 16;
  SetupImageList(Images16, Img16);

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

