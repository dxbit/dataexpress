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

unit ScriptEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, SynEdit, SynHighlighterPas, SynEditMarks,
  SynEditMiscClasses, scriptmanager, SynEditMouseCmds, SynEditTypes, LazUtf8;

type
  TBreakpointChangedEvent = procedure (Sender: TObject; Line: Integer) of object;

  { TScriptEdit }

  TScriptEdit = class(TSynEdit)
  private
    FBreakPointsChanges: Boolean;
    FCurLine: Integer;
    FOnBreakpointChanged: TBreakpointChangedEvent;
    FSD: TScriptData;
    FImageList: TImageList;
    procedure MemoGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure MemoSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; AMarkup: TSynSelectedColor);
    function FindBreakPointMark(Line: Integer): TSynEditMark;
    function FindRunLineMark: TSynEditMark;
		function AddMark(ImageIndex, Line: Integer): TSynEditMark;
    procedure DeleteMark(Mark: TSynEditMark);
    procedure SetCurLine(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadData(ASD: TScriptData);
    procedure ReloadMarks;
    procedure RestoreState;
    procedure SaveState;
    procedure ClearMarks;
    function FindAndDeleteBookmark(Num: Integer): Boolean;
    property SD: TScriptData read FSD;
    property CurLine: Integer read FCurLine write SetCurLine;
    property BreakPointsChanged: Boolean read FBreakPointsChanges;
    property OnBreakpointChanged: TBreakpointChangedEvent read FOnBreakpointChanged
			write FOnBreakpointChanged;
  end;

implementation

const
  IDX_BREAKPOINT = 10;
  IDX_RUNLINE = 11;

{ TScriptEdit }

function TScriptEdit.FindBreakPointMark(Line: Integer): TSynEditMark;
var
  Mrk: TSynEditMark;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Marks.Count - 1 do
  begin
    Mrk := Marks[i];
    if (Mrk.Line = Line) and (Mrk.ImageIndex = IDX_BREAKPOINT) then
    	Exit(Mrk);
  end;
end;

function TScriptEdit.FindRunLineMark: TSynEditMark;
var
  Mrk: TSynEditMark;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Marks.Count - 1 do
  begin
    Mrk := Marks[i];
    if Mrk.ImageIndex = IDX_RUNLINE then
    	Exit(Mrk);
  end;
end;

function TScriptEdit.AddMark(ImageIndex, Line: Integer): TSynEditMark;
var
  Mrk: TSynEditMark;
begin
  Mrk := TSynEditMark.Create(Self);
  Mrk.ImageList := FImageList;
  Mrk.ImageIndex := ImageIndex;
  Mrk.Line := Line;
  Mrk.Visible := True;
  Marks.Add(Mrk);
  Result := Mrk;
end;

procedure TScriptEdit.DeleteMark(Mark: TSynEditMark);
begin
  Marks.Remove(Mark);
  Mark.Free;
end;

procedure TScriptEdit.SetCurLine(AValue: Integer);
var
  M: TSynEditMark;
begin
  if FCurLine=AValue then Exit;
  FCurLine:=AValue;
  if FCurLine > 0 then
	  AddMark(IDX_RUNLINE, FCurLine).Priority:=1
  else
  begin
    M := FindRunLineMark;
    if M <> nil then DeleteMark(M);
  end;
end;

procedure TScriptEdit.MemoGutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
var
  Mrk: TSynEditMark;
begin
  FBreakPointsChanges:=True;
  Mrk := FindBreakPointMark(Line);
  if Mrk <> nil then
    while Mrk <> nil do
    begin
      DeleteMark(Mrk);
      Mrk := FindBreakPointMark(Line);
    end
  else
  	AddMark(IDX_BREAKPOINT, Line);
  if FOnBreakpointChanged <> nil then FOnBreakpointChanged(Self, Line);
end;

procedure TScriptEdit.MemoSpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; AMarkup: TSynSelectedColor);
begin
  if Line = FCurLine then
  begin
    Special := True;
    AMarkup.Background:=clBlue;
  end
  else if FindBreakPointMark(Line) <> nil then
  begin
    Special := True;
  	AMarkup.Background:=clRed;
  end;
end;

constructor TScriptEdit.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  // Удаляем "фишку" редактора - вставку выделенного по нажатию средней кнопки мыши.
  MouseOptions:=MouseOptions + [emUseMouseActions, emDragDropEditing];
  ResetMouseActions;
  for i := MouseTextActions.Count - 1 downto 0 do
    if MouseTextActions[i].Command = emcPasteSelection then
    begin
      MouseTextActions[i].Command := emcNone;
      Break;
    end;
  //

  Font.Size:=10;
  FImageList := TImageList.Create(Self);
  with FImageList do
  begin
    AddLazarusResource('0');
    AddLazarusResource('1');
    AddLazarusResource('2');
    AddLazarusResource('3');
    AddLazarusResource('4');
    AddLazarusResource('5');
    AddLazarusResource('6');
    AddLazarusResource('7');
    AddLazarusResource('8');
    AddLazarusResource('9');
    AddLazarusResource('breakpoint16');
    AddLazarusResource('runline16');
  end;
  Options:=Options - [eoSmartTabs] + [eoTabIndent];
  TabWidth:=2;
  Highlighter := TSynPasSyn.Create(Self);
  with TSynPasSyn(Highlighter) do
  begin
	  CommentAttri.Foreground := 7368816;
    CommentAttri.Style := [];
    StringAttri.Foreground := clBlue;
    SymbolAttri.Foreground := clRed;
    NestedComments := False;
  end;

  BookMarkOptions.BookmarkImages := FImageList;
  //Gutter.LineNumberPart.ShowOnlyLineNumbersMultiplesOf:=5;
  Gutter.MarksPart.AutoSize := False;
	Gutter.MarksPart.Width := 32;
  OnGutterClick:=@MemoGutterClick;
  OnSpecialLineMarkup:=@MemoSpecialLineMarkup;

  Font.Quality := fqDefault;
end;

procedure LoadMarks(Memo: TScriptEdit; Marks: TSourceMarks);
var
  i: Integer;
  M: TSourceMarkData;
  Mark: TSynEditMark;
begin
  Memo.ClearMarks;

  for i := 0 to Marks.Count - 1 do
  begin
    M := Marks[i];
    //if not M.IsBookmark then Continue;
    Mark := TSynEditMark.Create(Memo);
    Mark.BookmarkNumber := M.BookmarkNumber;
    Mark.Column := M.Column;
    Mark.Line := M.Row;
    if M.IsBookmark then
	    Mark.ImageIndex := M.BookmarkNumber
    else
    	Mark.ImageIndex := IDX_BREAKPOINT;		// Брейкпоинт
    Mark.Visible:=True;
    Memo.Marks.Add(Mark);
  end;
end;

procedure TScriptEdit.LoadData(ASD: TScriptData);
begin
  FSD := ASD;
  Text:=FSD.Source;
  LoadMarks(Self, FSD.SourceData.Marks);
end;

procedure TScriptEdit.ReloadMarks;
begin
  LoadMarks(Self, FSD.SourceData.Marks);
  Refresh;
end;

procedure TScriptEdit.RestoreState;
begin
  if Highlighter.NeedScan then
  	Highlighter.ScanAllRanges;
  FoldState := FSD.SourceData.FoldState;
  CaretX := FSD.SourceData.CaretX;
  CaretY := FSD.SourceData.CaretY;
  TopLine := FSD.SourceData.TopLine;
  LeftChar := FSD.SourceData.LeftChar;
end;

procedure SaveMarks(Memo: TSynEdit; AMarks: TSourceMarks);
var
  i: Integer;
  Mark: TSynEditMark;
  M: TSourceMarkData;
begin
  AMarks.Clear;
  for i := 0 to Memo.Marks.Count - 1 do
  begin
  	Mark := Memo.Marks[i];
    if Mark.ImageIndex = IDX_RUNLINE then Continue;
	  M := AMarks.AddMark;
  	M.BookmarkNumber := Mark.BookmarkNumber;
    M.Column := Mark.Column;
    M.Row := Mark.Line;
  end;
end;

procedure TScriptEdit.SaveState;
begin
  FSD.SourceData.CaretX := CaretX;
  FSD.SourceData.CaretY := CaretY;
  FSD.SourceData.TopLine := TopLine;
  FSD.SourceData.LeftChar := LeftChar;
  FSD.SourceData.FoldState := FoldState;
  SaveMarks(Self, FSD.SourceData.Marks);
end;

procedure TScriptEdit.ClearMarks;
var
  Mark: TSynEditMark;
begin
  while Marks.Count > 0 do
	begin
    Mark := Marks[0];
  	Marks.Delete(0);
    Mark.Free;
  end;
end;

function TScriptEdit.FindAndDeleteBookmark(Num: Integer): Boolean;
var
  Mrk: TSynEditMark;
  i: Integer;
begin
  Result := False;
  for i := 0 to Marks.Count - 1 do
  begin
    Mrk := Marks[i];
    if Mrk.BookmarkNumber = Num then
    begin
      DeleteMark(Mrk);
    	Exit(True);
    end;
  end;
end;

end.

