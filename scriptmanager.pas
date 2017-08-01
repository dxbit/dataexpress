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
unit ScriptManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSCompiler, uPSRuntime, uPSUtils, sqldb, dxctrls, Dialogs,
  Controls, strconsts, uPSPreProcessor, SAX, SAXBaseReader, uPSDebugger;

type
  TScriptData = class;

  TCompilerMsg = class
  public
    Col, Row, Pos: Integer;
    Msg, ErrorType, ModuleName: String;
    SD: TScriptData;
  end;

  TScriptKind = (skNone, skMain, skForm, skUser, skExpr);

  { TSourceMarkData }

  TSourceMarkData = class
  private
    FBookmarkNumber: Integer;
    FColumn: Integer;
    FRow: Integer;
  public
    constructor Create;
    function IsBookmark: Boolean;
    property BookmarkNumber: Integer read FBookmarkNumber write FBookmarkNumber;
    property Row: Integer read FRow write FRow;
    property Column: Integer read FColumn write FColumn;
  end;

  { TSourceMarks }

  TSourceMarks = class(TList)
  private
    function GetMarks(Index: Integer): TSourceMarkData;
  public
    function AddMark: TSourceMarkData;
    procedure Clear; override;
    procedure ClearBreakpoints;
    function FindBreakpoint(aRow: Integer): TSourceMarkData;
    procedure DeleteBreakpoint(Mark: TSourceMarkData);
    property Marks[Index: Integer]: TSourceMarkData read GetMarks; default;
  end;

  { TScriptSourceData }

  TScriptSourceData = class
  private
    FCaretX: Integer;
    FCaretY: Integer;
    FFoldState: String;
    FLeftChar: Integer;
    FMarks: TSourceMarks;
    FTopLine: Integer;
    FXml: TSAXBaseReader;
    procedure XmlStartElement(Sender: TObject; const NamespaceURI, LocalName,
      QName: SAXString; Atts: TSAXAttributes);
  public
    constructor Create;
    destructor Destroy; override;
    function SaveToString: String;
    procedure LoadFromString(const Buf: String);
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    property CaretX: Integer read FCaretX write FCaretX;
    property CaretY: Integer read FCaretY write FCaretY;
    property TopLine: Integer read FTopLine write FTopLine;
    property LeftChar: Integer read FLeftChar write FLeftChar;
    property FoldState: String read FFoldState write FFoldState;
    property Marks: TSourceMarks read FMarks;
  end;

  TIncludeData = class
  public
    SD: TScriptData;
    StartRow, RowCount: Integer;
  end;

  { TScriptData }
  TScriptData = class
  private
    FMsgs, FIncludes: TList;
    FSourceData: TScriptSourceData;
    function GetIncludes(Index: Integer): TIncludeData;
    function GetMsgs(Index: Integer): TCompilerMsg;
  public
    Name, Source, Bin, DebugData: String;
    FmId: Integer;
    Kind: TScriptKind;
    constructor Create;
    destructor Destroy; override;
    function GetModuleName: String;
    function AddMsg: TCompilerMsg;
    function MsgCount: Integer;
    procedure Clear;
    function IncludeCount: Integer;
    function AddInclude: TIncludeData;
    property Msgs[Index: Integer]: TCompilerMsg read GetMsgs;
    property SourceData: TScriptSourceData read FSourceData;
    property Includes[Index: Integer]: TIncludeData read GetIncludes;
  end;

  { TScriptCompiler }

  TScriptCompiler = class(TPSPascalCompiler)
  private
    FSD: TScriptData;
    FPre: TPSPreProcessor;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(const Source: String): Boolean;
    property SD: TScriptData read FSD write FSD;
    property Pre: TPSPreProcessor read FPre;
  end;

  { TEvalCompiler }

  TEvalCompiler = class(TPSPascalCompiler)
  private
    FObj: TObject;
    FSD: TScriptData;
  public
    constructor Create;
    function CompileSource: Boolean;
    property SD: TScriptData read FSD write FSD;
    property Obj: TObject read FObj write FObj;
  end;

  TExprFunc = class
  public
    OrigName: String;    		// Имя функции в модуле выражений
    Name: String;         	// Имя функции в выражении
    Args: String;
    ResultType: Char;
    Description: String;
    Group: String;
  end;

  { TExprFuncs }

  TExprFuncs = class(TList)
  private
    function GetFuncs(Index: Integer): TExprFunc;
  public
    function AddFunc: TExprFunc;
    function FindFunc(const Name: String): TExprFunc;
    function FindFuncIndex(const Name: String): Integer;
    procedure Clear; override;
    property Funcs[Index: Integer]: TExprFunc read GetFuncs; default;
  end;

  { TScriptManager }

  TScriptManager = class
  private
    FDebugMode: TDebugMode;
    FExprModule: TScriptData;
    FNeedUpdateFuncs: Boolean;
    FScripts: TList;
    FCompiler: TScriptCompiler;
    FFuncs: TExprFuncs;
    FUseDebugInfo: Boolean;
    function GetScripts(Index: Integer): TScriptData;
    function CheckScriptsToDB: Boolean;
    function CheckKindToDB: Boolean;
    procedure AddKindToDB;
    procedure CreateScriptsToDB;
    procedure ParseExprModules;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ModulesToList(SL: TStrings; Kind: TScriptKind);
    procedure CheckScripts;
    procedure LoadFromDB;
    procedure SaveToDB;
    procedure LoadFromDir(const aDir: String);
    procedure SaveToDir(const aDir: String);
    function AddScript(FmId: Integer; const aName, Source: String): TScriptData;
    function FindScript(FmId: Integer): TScriptData;
    function FindScriptByName(const aName: String): TScriptData;
    procedure DeleteScript(SD: TScriptData);
    function ScriptCount: Integer;
    procedure CompileModule(SD: TScriptData);
    procedure CompileAll;
    procedure CompileExpr;
    procedure MessagesToList(L: TStrings);
    function HasErrors: Boolean;
    property Scripts[Index: Integer]: TScriptData read GetScripts;
    property ExprModule: TScriptData read FExprModule;
    property Funcs: TExprFuncs read FFuncs;
    property NeedUpdateFuncs: Boolean read FNeedUpdateFuncs write
			FNeedUpdateFuncs; // Говорит окну выражений обновить список ключевых слов
    //property UseDebugInfo: Boolean read FUseDebugInfo write FUseDebugInfo;
    //property DebugMode: TDebugMode read FDebugMode write FDebugMode;
  end;

  { TBreakpointList }

  TBreakpointList = class(TList)
  private
    function GetBreakpoints(Index: Integer): Integer;
  public
    procedure AddBreakpoint(Line: Integer);
    function FindBreakpoint(Line: Integer): Integer;
    property Breakpoints[Index: Integer]: Integer read GetBreakpoints; default;
  end;

  { TRunScript }

  TRunScript = class
  private
    FExec: TPSDebugExec;
    FImporter: TPSRuntimeClassImporter;
    FSD: TScriptData;
    FUseDebugInfo: Boolean;
    FBreakpoints: TBreakpointList;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadBin: Boolean;
    function TryRunProc(const ProcName: String; Args: array of Variant): Boolean;
    function TryRunFunc(const FuncName: String; Args: array of Variant; var aResult: Variant): Boolean;
    //function TryRunProcS(const ProcName: String; Stack: TPSStack): Boolean;
    procedure BindForm(Fm: TdxForm);
    procedure BindVars;
    function AddBreakpoints: Boolean;
    property SD: TScriptData read FSD write FSD;
    property Exec: TPSDebugExec read FExec;
    property Breakpoints: TBreakpointList read FBreakpoints;
    property UseDebugInfo: Boolean read FUseDebugInfo write FUseDebugInfo;
  end;

  { EModuleParserError }

  EModuleParserError = class(Exception)
  private
    FPos: Integer;
  public
    constructor Create(const Msg: String; aPos: Integer);
    property Position: Integer read FPos;
  end;

  { TModuleParser }

  TModuleParser = class
  private
    FAuthor: String;
    FDescription: String;
    FFuncs: TExprFuncs;
    FPos, FLen: Integer;
    FBuf: String;
    FVersion: String;
    procedure SkipWhites;
    procedure Consume(const S: String);
    //procedure FindComment(const Str: String);
    function GetComment: String;
    function GetStr: String;
    function GetMultiStr: String;
    procedure DoError(const Msg: String; Params: array of const);
    procedure ParseFunction;
    procedure ParseHead;
  public
    procedure Parse(const Buf: String; OnlyHead: Boolean);
    {procedure Parse(const Buf: String; Funcs: TExprFuncs);
    procedure ParseHeadModule(const Buf: String;
    	var Author, Version, Description: String);  }
    property Funcs: TExprFuncs read FFuncs write FFuncs;
    property Author: String read FAuthor;
    property Version: String read FVersion;
    property Description: String read FDescription;
  end;

var
  ScriptMan: TScriptManager;
  MainModule: TRunScript;
  ExprModule: TRunScript;

implementation

uses
  Forms, dbengine, apputils, formmanager, outputform, LazUtf8, FileUtil,
  compilerdecls, rundecls, datasetprocessor, mainform, debugscriptform,
  uPSC_MyStd, uPSC_MyClasses, uPSC_MyGraphics, uPSC_MyControls, uPSC_MyStdCtrls,
  uPSC_MyExtCtrls, uPSC_MyButtons, uPSC_MyForms, uPSC_MyMenus,
  uPSC_Dll, uPSC_MyDateUtils,
  uPSR_MyStd, uPSR_MyClasses, uPSR_MyGraphics, uPSR_MyControls, uPSR_MyStdCtrls,
  uPSR_MyExtCtrls, uPSR_MyButtons, uPSR_MyForms, uPSR_MyMenus,
  uPSR_Dll, uPSR_MyDateUtils, StrUtils
  {$ifdef windows}
  ,uPSC_ComObj, uPSR_ComObj
  {$endif};

var
  IncludeRow, IncludeRowCount: Cardinal;

procedure CompilerBindVars(Cl: TPSPascalCompiler; SD: TScriptData);
var
  Fm, ChildFm: TdxForm;
  i: Integer;
  C: TComponent;
begin
  Cl.AddUsedVariableN('Application', 'TApplication');
  Cl.AddUsedVariableN('MainWindow', 'TMainFm');
  if SD.Kind in [skUser, skMain] then Exit;
//  if FmId = 0 then Exit;
	// SD.Kind in [skForm, skExpr]
  Cl.AddUsedVariableN('Self', 'TdxForm');
  //
  if SD.Kind <> skForm then Exit;
  Fm := FormMan.FindForm(SD.FmId);
  Cl.AddUsedVariableN(Fm.Name, Fm.ClassName);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C.Name = '') or (not (C is TControl)) then Continue;
    Cl.AddUsedVariableN(C.Name, C.ClassName);
    if C is TdxGrid then
    begin
      ChildFm := FormMan.FindForm(TdxGrid(C).Id);
      Cl.AddUsedVariableN(ChildFm.Name, ChildFm.ClassName);
    end;
  end;
end;

function CompilerUses(Sender: TPSPascalCompiler; const aName: tbtString
  ): Boolean;
begin
  Result := True;
  if aName = 'SYSTEM' then
  begin
    SIRegister_Std(Sender);
    SIRegister_Classes(Sender, True);
    SIRegister_Graphics(Sender, True);
    SIRegister_ImageList(Sender);
    SIRegister_Menus(Sender);
    SIRegister_Controls(Sender);
    SIRegister_Forms(Sender);
    SIRegister_StdCtrls(Sender);
    SIRegister_ExtCtrls(Sender);
    SIRegister_Buttons(Sender);
    RegisterDateTimeLibrary_C(Sender);
    SIRegister_dxCtrls(Sender);
    SIRegister_Consts(Sender);
    SIRegister_Functions(Sender);
    {$ifdef windows}
    SIRegister_ComObj(Sender);
    {$endif}
    RegisterDll_CompileTime(Sender);
	  CompilerBindVars(Sender, TScriptCompiler(Sender).SD)
  end;
end;

{ TModuleList }

{procedure ExecException(Sender: TPSExec; ExError: TPSError;
  const ExParam: tbtstring; ExObject: TObject; ProcNo, Position: Cardinal);
begin
  OutputFm.AddMsg(PSErrorToString(ExError, ExParam));
end;  }

{ TScriptCompiler }

function GetSourceRowCount(const S: String): Integer;
var
  OldCh, Ch: Char;
  i: Integer;
begin
  Result := 0;
  if S = '' then Exit;
  OldCh := S[1];
  for i := 1 to Length(S) do
  begin
    Ch := S[i];
    if Ch = #13 then Inc(Result)
    else if (Ch = #10) and (OldCh <> #13) then Inc(Result);
    OldCh := Ch;
  end;
end;

function NeedFile(Sender: TPSPreProcessor; const callingfilename: tbtstring;
  var FileName, Output: tbtstring): Boolean;
var
  SD: TScriptData;
  ID: TIncludeData;
begin
  SD := ScriptMan.FindScriptByName(FileName);
  Result := SD <> nil;
  if Result then
  begin
  	Output := SD.Source;
    ID := TScriptCompiler(Sender.Id).SD.AddInclude;
    ID.SD := SD;
    ID.StartRow:=IncludeRow;
    ID.RowCount:=GetSourceRowCount(SD.Source);
    IncludeRowCount := IncludeRowCount + ID.RowCount;
    //ShowMessage(SD.Name + ' StartRow: ' + IntToStr(ID.StartRow) + ' RowCount: ' + IntToStr(ID.RowCount));
  end;
end;

procedure UnknownDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: Boolean;
  const DirectiveName, DirectiveParam: tbtString; var Continue: Boolean);
begin
  Continue := True;
end;

procedure ProcessDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: Boolean;
  const DirectiveName, DirectiveParam: tbtString; var Continue: Boolean);
begin
  if UpperCase(DirectiveName) = 'I' then
  begin
    IncludeRow := Parser.Row + IncludeRowCount;
  end;
end;

procedure DetermineModule(aSD: TScriptData; Row: Cardinal; var Module: TScriptData; out AdjRow: Cardinal);
var
  i: Integer;
  ID: TIncludeData;
begin
  // Точка где-то в подключаемом модуле?
  for i := 0 to aSD.IncludeCount - 1 do
  begin
    ID := aSD.Includes[i];
    if (Row >= ID.StartRow) and (Row <= ID.StartRow + ID.RowCount) then
    begin
      Module := ID.SD;
      AdjRow := Row - ID.StartRow + 1;
    	Exit;
    end;
  end;

  // Нет. Точка где-то в основном модуле...
  Module := aSD;
  AdjRow := Row;
  for i := 0 to aSD.IncludeCount - 1 do
  begin
    ID := aSD.Includes[i];
    if Row <= ID.StartRow then Exit
    else AdjRow := AdjRow - ID.RowCount;
  end;
end;

procedure ExecSourceLine(Sender: TPSDebugExec; const Name: tbtstring; Position,
  Row, Col: Cardinal);
var
  SD: TScriptData;
  RS: TRunScript;
begin
  RS := TRunScript(Sender.Id);
  if ((RS.Exec.DebugMode <> dmRun) and (RS.Exec.DebugMode <> dmStepOver))
    or (RS.Breakpoints.FindBreakpoint(Row) >= 0) then
  begin
    DetermineModule(RS.SD, Row, SD, Row);
    ShowDebugForm(Sender, SD, Row);
  end;
end;

{ TEvalCompiler }

function EvalOnUses(Sender: TPSPascalCompiler; const aName: tbtString): Boolean;
begin
  Result := True;
  if aName = 'SYSTEM' then
  begin
    SIRegister_Std(Sender);
    SIRegister_Classes(Sender, True);
    SIRegister_Graphics(Sender, True);
    SIRegister_ImageList(Sender);
    SIRegister_Menus(Sender);
    SIRegister_Controls(Sender);
    SIRegister_Forms(Sender);
    SIRegister_StdCtrls(Sender);
    SIRegister_ExtCtrls(Sender);
    SIRegister_Buttons(Sender);
    RegisterDateTimeLibrary_C(Sender);
    SIRegister_dxCtrls(Sender);
    SIRegister_Consts(Sender);
    SIRegister_Functions(Sender);
    {$ifdef windows}
    SIRegister_ComObj(Sender);
    {$endif}
    RegisterDll_CompileTime(Sender);
	  Sender.AddUsedVariableN('OBJ', TEvalCompiler(Sender).Obj.ClassName);
    Sender.AddUsedVariableN('V', 'Variant');
  end;
end;

constructor TEvalCompiler.Create;
begin
  inherited Create;
  OnUses:=@EvalOnUses;
end;

function TEvalCompiler.CompileSource: Boolean;
begin
  Result := Compile(FSD.Source);
  if Result then GetOutput(FSD.Bin);
end;

{ TBreakpointList }

function TBreakpointList.GetBreakpoints(Index: Integer): Integer;
begin
  Result := Integer(Items[Index]);
end;

procedure TBreakpointList.AddBreakpoint(Line: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	if Line < Breakpoints[i] then
    begin
      Insert(i, Pointer(Line));
      Exit;
    end;
  Add(Pointer(Line));
end;

function TBreakpointList.FindBreakpoint(Line: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  	if Breakpoints[i] = Line then Exit(Breakpoints[i]);
end;

{ TSourceMarkData }

constructor TSourceMarkData.Create;
begin
  FBookmarkNumber:=-1;
end;

function TSourceMarkData.IsBookmark: Boolean;
begin
  Result := FBookmarkNumber >= 0;
end;

{ TSourceMarks }

function TSourceMarks.GetMarks(Index: Integer): TSourceMarkData;
begin
  Result := TSourceMarkData(Items[Index]);
end;

function TSourceMarks.AddMark: TSourceMarkData;
begin
  Result := TSourceMarkData.Create;
  Add(Result);
end;

procedure TSourceMarks.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Marks[i].Free;
  inherited Clear;
end;

procedure TSourceMarks.ClearBreakpoints;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if not Marks[i].IsBookmark then
    begin
      Marks[i].Free;
      Delete(i);
    end;
end;

function TSourceMarks.FindBreakpoint(aRow: Integer): TSourceMarkData;
var
  i: Integer;
  M: TSourceMarkData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    M := Marks[i];
    if (M.Row = aRow) and (M.IsBookmark = False) then Exit(M);
  end;
end;

procedure TSourceMarks.DeleteBreakpoint(Mark: TSourceMarkData);
begin
  Remove(Mark);
  Mark.Free;
end;

{ TScriptSourceData }

procedure TScriptSourceData.XmlStartElement(Sender: TObject;
  const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);
var
  M: TSourceMarkData;
begin
  if Atts = nil then Exit;
	if LocalName = 'sourcedata' then
  begin
		FCaretX := FXml.GetInt(Atts, 'caretx');
    FCaretY := FXml.GetInt(Atts, 'carety');
    FTopLine := FXml.GetInt(Atts, 'topline');
    FLeftChar := FXml.GetInt(Atts, 'leftchar');
  	FFoldState := FXml.GetStr(Atts, 'foldstate');
  end
  else if LocalName = 'mark' then
  begin
    M := FMarks.AddMark;
    M.BookmarkNumber := FXml.GetInt(Atts, 'bm');
    M.Column := FXml.GetInt(Atts, 'column');
    M.Row := FXml.GetInt(Atts, 'line');
  end;
end;

constructor TScriptSourceData.Create;
begin
  FMarks := TSourceMarks.Create;
end;

destructor TScriptSourceData.Destroy;
begin
  FMarks.Free;
  inherited Destroy;
end;

function TScriptSourceData.SaveToString: String;
var
  i: Integer;
  M: TSourceMarkData;
begin
  Result := '<sourcedata caretx="' + IntToStr(FCaretX) +
  	'" carety="' + IntToStr(FCaretY) +
    '" topline="' + IntToStr(FTopLine) +
    '" leftchar=" ' + IntToStr(FLeftChar) +
    '" foldstate="' + FFoldState + '"><marks>';
  for i := 0 to FMarks.Count - 1 do
  begin
    M := FMarks[i];
    Result := Result + '<mark bm="' + IntToStr(M.BookmarkNumber) +
    	'" column="' + IntToStr(M.Column) +
      '" line="' + IntToStr(M.Row) + '" />';
  end;
  Result := Result + '</marks></sourcedata>'
end;

procedure TScriptSourceData.LoadFromString(const Buf: String);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(Buf);
  FXml := TSAXBaseReader.Create;
  FXml.OnStartElement:=@XmlStartElement;
  try
  	FXml.ParseStream(SS);
  finally
	  FXml.Free;
    SS.Free;
  end;
end;

procedure TScriptSourceData.SaveToFile(const FileName: String);
var
  Buf: String;
begin
  Buf := SaveToString;
  with TFileStream.Create(FileName, fmCreate) do
  try
    Write(Pointer(Buf)^, Length(Buf));
  finally
    Free;
  end;
end;

procedure TScriptSourceData.LoadFromFile(const FileName: String);
var
  Buf: String;
begin
	with TFileStream.Create(FileName, fmOpenRead) do
  try
  	SetLength(Buf, Size);
    Read(Pointer(Buf)^, Size);
  finally
    Free;
  end;
  LoadFromString(Buf);
end;

{ EModuleParserError }

constructor EModuleParserError.Create(const Msg: String; aPos: Integer);
begin
  inherited Create(Msg);
  FPos := aPos;
end;

{ TModuleParser }

procedure TModuleParser.SkipWhites;
begin
  while (FPos <= FLen) and (FBuf[FPos] <= #32) do
  	Inc(FPos);
end;

procedure TModuleParser.Consume(const S: String);
var
  Len: Integer;
begin
	SkipWhites;
  Len := Length(S);
  if LowerCase(Copy(FBuf, FPos, Len)) <> S then
  	DoError('%s expected', [S]);
  FPos := FPos + Len;
end;

(*procedure TModuleParser.FindComment(const Str: String);
var
  P: Integer;
  S: String;
begin
  while FPos <= FLen do
  begin
    P := PosEx('{@', FBuf, FPos);
    if P = 0 then
    begin
    	FPos := FLen + 1;
      Break;
    end
    else FPos := P + 2;
    P := FPos;
    while (FPos <= FLen) and (FBuf[FPos] in ['A'..'Z', 'a'..'z']) do
      Inc(FPos);
    S := Copy(FBuf, P, FPos - P);
    if LowerCase(S) = Str then
      Break;
  end;
end;     *)

function TModuleParser.GetComment: String;
var
  P: Integer;
begin
  Result := '';
  P := PosEx('{@', FBuf, FPos);
  if P = 0 then
  begin
    FPos := FLen + 1;
    Exit;
  end
  else FPos := P + 2;
  P := FPos;
  while (FPos <= FLen) and (FBuf[FPos] in ['A'..'Z', 'a'..'z']) do
    Inc(FPos);
  Result := Copy(FBuf, P, FPos - P);
end;

function TModuleParser.GetStr: String;
var
  P: Integer;
begin
  P := FPos;
  while (FPos <= FLen) and (not (FBuf[FPos] in [#10, #13])) do
    Inc(FPos);
  Result := Trim(Copy(FBuf, P, FPos - P));
end;

function TModuleParser.GetMultiStr: String;
var
  P: Integer;
begin
  P := FPos;
  while (FPos <= FLen) and (Copy(FBuf, FPos, 2) <> '@}') do
    Inc(FPos);
  Result := Copy(FBuf, P, FPos - P);
  Consume('@}');
end;

procedure TModuleParser.DoError(const Msg: String; Params: array of const);
begin
  raise EModuleParserError.Create('[Error] ' + Format(Msg, Params), FPos);
end;

procedure TModuleParser.ParseFunction;
var
  F: TExprFunc;
  S, OrigName, Nm: String;
begin
  Consume('origname');
  Consume('=');
  OrigName := UpperCase(GetStr);
  Consume('name');
  Consume('=');
  Nm := UpperCase(GetStr);

  if FFuncs.FindFunc(Nm) <> nil then
  	DoError('Duplicate function name: %s', [Nm]);

  F := FFuncs.AddFunc;
  F.OrigName := OrigName;
  F.Name := Nm;

  Consume('args');
  Consume('=');
  F.Args:=LowerCase(GetStr);
  Consume('result');
  Consume('=');
  S := LowerCase(GetStr);
  if S <> '' then
	  F.ResultType:=S[1]
  else
    F.ResultType := #0;
  Consume('group');
  Consume('=');
  F.Group:=GetStr;
  Consume('description');
  Consume('=');
  F.Description:=GetMultiStr;
end;

procedure TModuleParser.ParseHead;
begin
  Consume('author');
  Consume('=');
  FAuthor := GetStr;
  Consume('version');
  Consume('=');
  FVersion := GetStr;
  Consume('description');
  Consume('=');
  FDescription := GetMultiStr;
end;

procedure TModuleParser.Parse(const Buf: String; OnlyHead: Boolean);
var
  S: String;
begin
  FAuthor := ''; FVersion := ''; FDescription := '';
  FBuf := Buf; FPos := 1; FLen := Length(FBuf);
  while FPos <= FLen do
  begin
    S := LowerCase(GetComment);
    if (not OnlyHead) and (S = 'function') then ParseFunction
    else if S = 'module' then
    begin
      ParseHead;
      if OnlyHead then Break;
    end;
  end;
end;

(*procedure TModuleParser.Parse(const Buf: String; Funcs: TExprFuncs);
var
  F: TExprFunc;
  S: String;
begin
  FBuf := Buf; FPos := 1; FLen := Length(FBuf);
  while FPos <= FLen do
  begin
    FindComment('function');
    if FPos > FLen then Break;
    F := Funcs.AddFunc;
    Consume('origname');
    Consume('=');
    F.OrigName := UpperCase(GetStr);
    Consume('name');
    Consume('=');
    F.Name:=UpperCase(GetStr);
    Consume('args');
    Consume('=');
    F.Args:=LowerCase(GetStr);
    Consume('result');
    Consume('=');
    S := LowerCase(GetStr);
    if S <> '' then
	    F.ResultType:=S[1]
    else
    	F.ResultType := #0;
    Consume('group');
    Consume('=');
    F.Group:=GetStr;
    Consume('description');
    Consume('=');
    F.Description:=GetMultiStr;
  end;
end;

procedure TModuleParser.ParseHeadModule(const Buf: String; var Author, Version,
  Description: String);
begin
  FPos := 1;
  FLen := Length(Buf);
  FBuf := Buf;
  Author := '';
  Version := '';
  Description:='';
  FindComment('module');
  if FPos < FLen then
  begin
    try
  	  Consume('author');
      Consume('=');
      Author := GetStr;
      Consume('version');
      Consume('=');
      Version := GetStr;
      Consume('description');
      Consume('=');
      Description := GetMultiStr;
    except
      on E: EModuleParserError do ;
    end;
  end;
end;   *)

{ TExprFuncs }

function TExprFuncs.GetFuncs(Index: Integer): TExprFunc;
begin
  Result := TExprFunc(Items[Index]);
end;

function TExprFuncs.AddFunc: TExprFunc;
begin
	Result := TExprFunc.Create;
  Add(Result);
end;

function TExprFuncs.FindFunc(const Name: String): TExprFunc;
var
  i: Integer;
  F: TExprFunc;
begin
  Result := nil;
	for i := 0 to Count - 1 do
  begin
    F := Funcs[i];
    if F.Name = Name then Exit(F);
  end;
end;

function TExprFuncs.FindFuncIndex(const Name: String): Integer;
var
  i: Integer;
  F: TExprFunc;
begin
  Result := -1;
	for i := 0 to Count - 1 do
  begin
    F := Funcs[i];
    if F.Name = Name then Exit(i);
  end;
end;

procedure TExprFuncs.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
		Funcs[i].Free;
  inherited Clear;
end;

constructor TScriptCompiler.Create;
begin
  inherited Create;
  BooleanShortCircuit:=True;
  AllowNoBegin:=True;
  AllowNoEnd:=True;
  OnUses:=@CompilerUses;
  FPre := TPSPreprocessor.Create;
  FPre.Id := Self;
  FPre.OnProcessUnknowDirective:=@UnknownDirective;
  FPre.OnProcessDirective:=@ProcessDirective;
  FPre.OnNeedFile:=@NeedFile;
end;

destructor TScriptCompiler.Destroy;
begin
  FPre.Free;
  inherited Destroy;
end;

function TScriptCompiler.Compile(const Source: String): Boolean;
var
  S: String;
begin
  FPre.Clear;
  {$ifdef windows}
  FPre.Defines.Add('WINDOWS');
  {$else}
  FPre.Defines.Add('LINUX');
  {$endif}
  FPre.MainFile:=Source;
  S := '';
  IncludeRowCount := 0;
  FPre.PreProcess('', S);
  Result := inherited Compile(S);
  FPre.AdjustMessages(Self);
end;

{ TRunScript }

function TRunScript.AddBreakpoints: Boolean;
var
  i, j: Integer;
  M: TSourceMarkData;
  ID: TIncludeData;

  function AdjustRow(R: Integer): Integer;
  var
    j: Integer;
    ID: TIncludeData;
  begin
    Result := R;
    for j := 0 to FSD.IncludeCount - 1 do
    begin
      ID := FSD.Includes[j];
      if Result <= ID.StartRow then Exit;
      Result := Result + ID.RowCount;
    end;
  end;

begin
  Result := False;
  FBreakpoints.Clear;

  for i := 0 to FSD.SourceData.Marks.Count - 1 do
  begin
    M := FSD.SourceData.Marks[i];
    if not M.IsBookmark then
    begin
      FBreakpoints.AddBreakpoint( AdjustRow(M.Row) );
      Result := True;
    end;
  end;

  for i := 0 to FSD.IncludeCount - 1 do
  begin
    ID := FSD.Includes[i];
    for j := 0 to ID.SD.SourceData.Marks.Count - 1 do
    begin
      M := ID.SD.SourceData.Marks[j];
      if not M.IsBookmark then
      begin
      	FBreakpoints.AddBreakpoint(M.Row + ID.StartRow - 1);
        Result := True;
      end;
    end;
  end;

  {for i := 0 to FBreakpoints.Count - 1 do
  	ShowMessage(IntToStr(FBreakpoints[i]));}
end;

constructor TRunScript.Create;
begin
  FExec := TPSDebugExec.Create;
  FImporter := TPSRuntimeClassImporter.Create;
  RegisterClassLibraryRuntime(FExec, FImporter);
  RIRegister_Std(FImporter);
  RIRegister_Classes(FImporter, True);
  RIRegister_Graphics(FImporter, True);
  RIRegister_ImageList(FImporter);
  RIRegister_Menus(FImporter);
  RIRegister_Controls(FImporter);
  RIRegister_Forms(FImporter);
  RIRegister_StdCtrls(FImporter);
  RIRegister_ExtCtrls(FImporter);
  RIRegister_Buttons(FImporter);
  RegisterDateTimeLibrary_R(FExec);
  RIRegister_dxCtrls(FImporter);
  RIRegister_Functions(FExec);
  {$ifdef windows}
  RIRegister_ComObj(FExec);
  {$endif}
  RegisterDllRuntime(FExec);

  FBreakpoints := TBreakpointList.Create;
//  FExec.OnException:=@ExecException;
end;

destructor TRunScript.Destroy;
begin
  FBreakpoints.Free;
  FImporter.Free;
  FExec.Free;
  inherited Destroy;
end;

function TRunScript.LoadBin: Boolean;
begin
  FExec.Id:=Self;
	Result := FExec.LoadData(SD.Bin);
  if not Result then Exit;
  if AddBreakpoints then
  begin
		FExec.DebugEnabled:=True;
		FExec.LoadDebugData(SD.DebugData);
	  FExec.OnSourceLine:=@ExecSourceLine;
  end;
end;

function TRunScript.TryRunProc(const ProcName: String; Args: array of Variant
  ): Boolean;
var
  ProcNo: Cardinal;
begin
  Result := True;
  ProcNo := FExec.GetProc(ProcName);
  if ProcNo <> InvalidVal then
  begin
    //try
      FExec.RunProcP(Args, ProcNo);
      FExec.RaiseCurrentException;
    {except
      on E: Exception do
      begin
        OutputFm.AddMsg(E.ClassName + ': ' + E.Message);
        Result := False;
      end;
    end  }
  end
  else Result := False;
end;

function TRunScript.TryRunFunc(const FuncName: String; Args: array of Variant;
  var aResult: Variant): Boolean;
var
  ProcNo: Cardinal;
begin
  Result := True;
  ProcNo := FExec.GetProc(FuncName);
  if ProcNo <> InvalidVal then
  begin
    aResult := FExec.RunProcP(Args, ProcNo);
    FExec.RaiseCurrentException;
  end
  else Result := False;
end;

(*function TRunScript.TryRunProcS(const ProcName: String; Stack: TPSStack
  ): Boolean;
var
  ProcNo: Cardinal;
begin
  ProcNo := FExec.GetProc(ProcName);
  Result := ProcNo <> InvalidVal;
  if Result then
  begin
    FExec.RunProc(Stack, ProcNo);
    if FExec.LastEx <> erNoError then
    	FExec.RaiseCurrentException;
  end;
  {if Result then
    try
      FExec.RunProc(Stack, ProcNo);
    except
      on E: Exception do
      begin
        OutputFm.AddMsg(E.ClassName + ': ' + E.Message);
        Result := False;
      end;
    end; }
end;  *)

procedure TRunScript.BindForm(Fm: TdxForm);
var
  i: Integer;
  C: TComponent;
begin
  SetVariantToClass(FExec.GetVar2('Self'), Fm);
  SetVariantToClass(FExec.GetVar2('Application'), Application);
  SetVariantToClass(FExec.GetVar2(Fm.Name), Fm);
  SetVariantToClass(FExec.GetVar2('MainWindow'), MainFm);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C.Name = '') or (not (C is TControl)) then Continue;
    SetVariantToClass(FExec.GetVar2(C.Name), C);
  end;
  if Fm.PId = 0 then
    with TDataSetProcessor(Fm.DSP) do
      for i := 1 to DataSetCount - 1 do
        SetVariantToClass(FExec.GetVar2(DataSets[i]^.Form.Name), DataSets[i]^.Form);
end;

procedure TRunScript.BindVars;
begin
  SetVariantToClass(FExec.GetVar2('Application'), Application);
  SetVariantToClass(FExec.GetVar2('MainWindow'), MainFm);
  if FSD.Kind = skExpr then
    SetVariantToClass(FExec.GetVar2('Self'), nil);
end;

{ TScriptData }

function TScriptData.GetMsgs(Index: Integer): TCompilerMsg;
begin
  Result := TCompilerMsg(FMsgs[Index]);
end;

function TScriptData.GetIncludes(Index: Integer): TIncludeData;
begin
  Result := TIncludeData(FIncludes[Index]);
end;

constructor TScriptData.Create;
begin
  FMsgs := TList.Create;
  FIncludes := TList.Create;
  FSourceData := TScriptSourceData.Create;
end;

destructor TScriptData.Destroy;
begin
  FSourceData.Free;
  ClearList(FIncludes);
  FIncludes.Free;
  ClearList(FMsgs);
  FMsgs.Free;
  inherited Destroy;
end;

function TScriptData.GetModuleName: String;
begin
  if Kind = skMain then Result := 'Main'
  else if Kind = skForm then Result := FormMan.FindForm(FmId).FormCaption
  else Result := Name;
end;

function TScriptData.AddMsg: TCompilerMsg;
begin
  Result := TCompilerMsg.Create;
  FMsgs.Add(Result);
end;

function TScriptData.MsgCount: Integer;
begin
  Result := FMsgs.Count;
end;

procedure TScriptData.Clear;
begin
  ClearList(FMsgs);
  ClearList(FIncludes);
end;

function TScriptData.IncludeCount: Integer;
begin
  Result := FIncludes.Count;
end;

function TScriptData.AddInclude: TIncludeData;
begin
  Result := TIncludeData.Create;
  FIncludes.Add(Result);
end;

{ TScriptManager }

function TScriptManager.GetScripts(Index: Integer): TScriptData;
begin
  Result := TScriptData(FScripts[Index]);
end;

function TScriptManager.AddScript(FmId: Integer; const aName, Source: String
  ): TScriptData;
begin
  Result := TScriptData.Create;
  Result.FmId := FmId;
  Result.Name := aName;
  Result.Source := Source;
  FScripts.Add(Result);
end;

constructor TScriptManager.Create;
begin
  FScripts := TList.Create;
  FExprModule := TScriptData.Create;
  FExprModule.Kind := skExpr;
  FCompiler := TScriptCompiler.Create;
  {FCompiler.BooleanShortCircuit:=True;
  FCompiler.AllowNoBegin:=True;
  FCompiler.AllowNoEnd:=True;
  FCompiler.ID:=Self;
  FCompiler.OnUses:=@CompilerUses;
  FCompiler.Pre.OnNeedFile:=@NeedFile;
  FCompiler.Pre.ID:=Self; }
  FFuncs := TExprFuncs.Create;
  FNeedUpdateFuncs:=True;
  FUseDebugInfo := True;
end;

destructor TScriptManager.Destroy;
begin
  FFuncs.Free;
  FCompiler.Free;
  FExprModule.Free;
  ClearList(FScripts);
  FScripts.Free;
  inherited Destroy;
end;

procedure TScriptManager.ModulesToList(SL: TStrings; Kind: TScriptKind);
var
  i: Integer;
  SD: TScriptData;
begin
  SL.Clear;
  for i := 0 to ScriptCount - 1 do
  begin
    SD := Scripts[i];
    if SD.Kind = Kind then SL.AddObject(SD.Name, SD);
  end;
end;

procedure TScriptManager.CheckScripts;
begin
  if not CheckScriptsToDB then CreateScriptsToDB
  else if not CheckKindToDB then AddKindToDB;
end;

function TScriptManager.CheckScriptsToDB: Boolean;
begin
  try
    with DBase.OpenDataSet('select id from dx_scripts where id=0') do
    begin
      Result := True;
      Free;
    end;
  except
    Result := False;
  end;
end;

function TScriptManager.CheckKindToDB: Boolean;
begin
  try
    with DBase.OpenDataSet('select kind from dx_scripts where id=0') do
    begin
      Result := True;
      Free;
    end;
  except
    Result := False;
  end;
end;

procedure TScriptManager.AddKindToDB;
begin
  DBase.Execute('ALTER TABLE DX_SCRIPTS ADD KIND SMALLINT;');
end;

procedure TScriptManager.CreateScriptsToDB;
begin
  DBase.Execute('CREATE TABLE DX_SCRIPTS (ID INTEGER, ' +
    'SCRIPT BLOB SUB_TYPE 1 SEGMENT SIZE 80, EXTRA BLOB SUB_TYPE 1 SEGMENT SIZE 80, ' +
    'FMID INTEGER, NAME VARCHAR(255), KIND SMALLINT);');
end;

procedure TScriptManager.ParseExprModules;
var
  P: TModuleParser;
  SL: TStringList;
  i: Integer;
  SD: TScriptData;
  Msg: TCompilerMsg;
begin
  FFuncs.Clear;
  P := TModuleParser.Create;
  P.Funcs := FFuncs;
  SL := TStringList.Create;
  try
    ModulesToList(SL, skExpr);
    SL.Sort;
    for i := 0 to SL.Count - 1 do
    begin
      SD := TScriptData(SL.Objects[i]);
      SD.Clear;
      try
	      P.Parse(SD.Source, False);
      except
        on E: EModuleParserError do
        begin
          Msg := SD.AddMsg;
          Msg.ErrorType:='Error';
          Msg.ModuleName:=SD.Name;
          Msg.SD := SD;
          Msg.Msg:=E.Message;
          Msg.Pos := E.Position;
        end;
      end;
    end;
  finally
    SL.Free;
    P.Free;
  end;
end;

procedure TScriptManager.LoadFromDB;
begin
  ClearList(FScripts);
  with DBase.OpenDataSet('select id, fmid, name, script, kind, extra from dx_scripts') do
  begin
    while not Eof do
    begin
      with AddScript(Fields[1].AsInteger,
      	Fields[2].AsString, Fields[3].AsString) do
        begin
        	Kind := TScriptKind(Fields[4].AsInteger);
          // Коррекция Kind. Временно.
          if Kind = skNone then
          begin
	          if FmId > 0 then Kind := skForm
  	        else if Name = 'Main' then Kind := skMain
    	      else if Name <> '' then Kind := skUser;
          end;
          //
          SourceData.LoadFromString(Fields[5].AsString);
        end;
      Next;
    end;
    Free;
  end;
  // Главный модуль
  if FindScriptByName('Main') = nil then
    AddScript(0, 'Main', '').Kind:=skMain;
end;

procedure TScriptManager.SaveToDB;
var
  DS: TSQLQuery;
  i: Integer;
begin
  //DBase.Execute('delete from dx_scripts;');

  DS := DBase.OpenSysTable('select id, script, fmid, name, kind, extra from dx_scripts',
  	'dx_scripts');
  try
    while not DS.Eof do
    	DS.Delete;
    for i := 0 to FScripts.Count - 1 do
    begin
      DS.Append;
      DS.Fields[0].AsInteger := i + 1;
      DS.Fields[1].AsString := GetScripts(i).Source;
      DS.Fields[2].AsInteger := GetScripts(i).FmId;
      DS.Fields[3].AsString := GetScripts(i).Name;
      DS.Fields[4].AsInteger := Ord(GetScripts(i).Kind);
      DS.Fields[5].AsString := GetScripts(i).SourceData.SaveToString;
      DS.Post;
    end;
    DBase.ApplyDataSet(DS);
    DBase.Commit;
  finally
    DS.Free;
  end;
end;

procedure TScriptManager.LoadFromDir(const aDir: String);
var
  SL, SL2: TStringList;
  i: Integer;
  S, Ext: String;
  SD: TScriptData;
begin
  ClearList(FScripts);
  SL := TStringList.Create;
  SL2 := TStringList.Create;
  FindAllFiles(SL, aDir, '*.pas;*.fpas;*.epas', False);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    SL2.LoadFromFile(S);
    S := ExtractFileName(S);
    Ext := LowerCase(ExtractFileExt(S));
    S := ChangeFileExt(S, '');
    SD := nil;
    if Ext = '.pas' then
    begin
      SD := AddScript(0, S, SL2.Text);
      if CompareText(S, 'main') = 0 then SD.Kind := skMain
      else SD.Kind := skUser;
    end
    else if Ext = '.fpas' then
    begin
      SD := AddScript(StrToInt(S), '', SL2.Text);
      SD.Kind := skForm;
    end
    else if Ext = '.epas' then
    begin
      SD := AddScript(0, S, SL2.Text);
      SD.Kind := skExpr;
    end;
    if (SD <> nil) and FileExists(SL[i] + '.cfg') then
    	SD.SourceData.LoadFromFile(SL[i] + '.cfg');
  end;
  SL2.Free;
  SL.Free;
  // Главный модуль
  if FindScriptByName('Main') = nil then
    AddScript(0, 'Main', '');
end;

procedure TScriptManager.SaveToDir(const aDir: String);
var
  i: Integer;
  SD: TScriptData;
  S: String;
begin
  for i := 0 to ScriptMan.ScriptCount - 1 do
  begin
    SD := ScriptMan.Scripts[i];
    case SD.Kind of
    	skMain, skUser: S := SD.Name + '.pas';
      skForm: S := IntToStr(SD.FmId) + '.fpas';
      skExpr: S := SD.Name + '.epas';
    end;
    SD.SourceData.SaveToFile(aDir + S + '.cfg');
    with TFileStream.Create(aDir + S, fmCreate) do
    try
      S := SD.Source;
      WriteBuffer(Pointer(S)^, Length(S));
    finally
      Free;
    end;
  end;
end;

function TScriptManager.FindScript(FmId: Integer): TScriptData;
var
  i: Integer;
  SD: TScriptData;
begin
  Result := nil;
  for i := 0 to FScripts.Count - 1 do
  begin
    SD := GetScripts(i);
    if SD.FmId = FmId then Exit(SD);
  end;
end;

function TScriptManager.FindScriptByName(const aName: String): TScriptData;
var
  i: Integer;
  SD: TScriptData;
begin
  Result := nil;
  for i := 0 to FScripts.Count - 1 do
  begin
    SD := GetScripts(i);
    if CompareText(aName, SD.Name) = 0 then Exit(SD);
  end;
end;

procedure TScriptManager.DeleteScript(SD: TScriptData);
begin
  FScripts.Remove(SD);
  SD.Free;
end;

function TScriptManager.ScriptCount: Integer;
begin
  Result := FScripts.Count;
end;

procedure ExtractColRow(const S: String; var R, C: Integer);
var
  p1, p2: SizeInt;
begin
  R := 1; C := 1;
  p1 := Pos(' at ', S);
  p2 := Pos(':', S);
  if (p1 <> 0) and (p2 > p1) then
  begin
    p1 := p1 + 4;
    TryStrToInt(Copy(S, p1, p2 - p1), R);
    TryStrToInt(Copy(S, p2 + 1, 255), C);
  end;
end;

procedure TScriptManager.CompileModule(SD: TScriptData);
var
  CMsg: TCompilerMsg;
  j: Integer;
  Msg: TPSPascalCompilerMessage;
begin
  SD.Clear;
  FCompiler.Clear;
  FCompiler.SD := SD;

  try

  if FCompiler.Compile(SD.Source) then
  begin
    if FCompiler.GetOutput(SD.Bin) = False then
    begin
      CMsg := SD.AddMsg;
      CMsg.Msg := 'Unsuccessfull compile';
      CMsg.SD := SD;
      CMsg.ErrorType := 'Error';
    end
    else
    	FCompiler.GetDebugOutput(SD.DebugData);
    //if SD.Source <> '' then ShowMessage(IntToStr(Length(SD.Bin)));
  end;

  for j := 0 to FCompiler.MsgCount - 1 do
  begin
    Msg := FCompiler.Msg[j];
    CMsg := SD.AddMsg;
    CMsg.Col:=Msg.Col;
    CMsg.Row := Msg.Row;
    CMsg.Msg:= Msg.MessageToString;
    CMsg.ErrorType:=Msg.ErrorType;
    CMsg.ModuleName:=Msg.ModuleName;
    CMsg.SD:=SD;
  end;

  except
    on E: EPSPreProcessor do
    begin
      CMsg := SD.AddMsg;
      CMsg.Msg := E.Message;
      CMsg.SD := SD;
      CMsg.ErrorType := 'Error';
      ExtractColRow(E.Message, CMsg.Row, CMsg.Col);
    end;
  end;
end;

// Компилирует только модули форм и модуль Main.
procedure TScriptManager.CompileAll;
var
  i: Integer;
  SD: TScriptData;
begin
  for i := 0 to FScripts.Count - 1 do
  begin
    SD := GetScripts(i);
    if SD.Kind in [skMain, skForm] then CompileModule(SD);
  end;
  CompileExpr;
end;

procedure TScriptManager.CompileExpr;
var
  S: String;
  SL: TStringList;
  i: Integer;
begin
  S := '';
  SL := TStringList.Create;
  ModulesToList(SL, skExpr);
  SL.Sort;
  for i := 0 to SL.Count - 1 do
    S := S + '{$I ' + SL[i] + '}';
  SL.Free;
  FExprModule.Source:=S;
  CompileModule(FExprModule);
  ParseExprModules;
  FNeedUpdateFuncs:=True;
end;

procedure TScriptManager.MessagesToList(L: TStrings);
var
  i: Integer;

  procedure _MessagesToList(SD: TScriptData);
  var
    S: String;
    j: Integer;
    Msg: TCompilerMsg;
  begin
    S := '';
    if SD.Name <> '' then
      S := SD.Name + ': '
    else if SD.Kind = skForm then
      S := FormMan.FindForm(SD.FmId).FormCaption + ': ';
    for j := 0 to SD.MsgCount - 1 do
    begin
      Msg := SD.Msgs[j];
      L.AddObject(S + Msg.Msg, Msg);
    end;
  end;

begin
  L.Clear;
  for i := 0 to ScriptMan.ScriptCount - 1 do
    _MessagesToList(ScriptMan.Scripts[i]);
  _MessagesToList(FExprModule);
end;

function TScriptManager.HasErrors: Boolean;
var
  i: Integer;

  function _HasErrors(SD: TScriptData): Boolean;
  var
    j: Integer;
    Msg: TCompilerMsg;
  begin
    Result := False;
    for j := 0 to SD.MsgCount - 1 do
    begin
      Msg := SD.Msgs[j];
      if Msg.ErrorType = 'Error' then Exit(True);
    end;
  end;

begin
  Result := False;
  for i := 0 to ScriptMan.ScriptCount - 1 do
    if _HasErrors(ScriptMan.Scripts[i]) then Exit(True);
  Result := _HasErrors(FExprModule);
end;

end.

