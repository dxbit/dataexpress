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

unit Expressions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Db, dxctrls, strconsts, Dialogs, dxreports, uPSRuntime,
  uPSDebugger, StrUtils;

type
  ELoopException = class(Exception);

  { ECalcError }

  ECalcError = class(Exception)
  private
    FPos: Integer;
  public
    constructor Create(const MsgFmt: string; const args: array of const; P: Integer);
    property Position: Integer read FPos;
  end;

  { TExpression }

  TExpression = class
  private
    FPos: Integer;
  public
    function Calc: Variant; virtual;
    property Position: Integer read FPos write FPos;
  end;

  { TENumber }

  TENumber = class(TExpression)
  private
    FValue: Extended;
  public
    constructor Create(aValue: Extended);
    function Calc: Variant; override;
  end;

  { TEString }

  TEString = class(TExpression)
  private
    FValue: String;
  public
    constructor Create(const aValue: String);
    function Calc: Variant; override;
    property Value: String read FValue;
  end;

  { TEField }

  TEField = class(TExpression)
  private
    FFieldName: String;
    FFieldType: Char;
    FForm, FParForm: TdxForm;
    FDS: TDataSet;
    FSkipLabels: Boolean;
  public
    constructor Create(const aFieldName: String; aForm, aParForm: TdxForm; aDS: TDataSet;
      aSkipLabels: Boolean);
    function Calc: Variant; override;
    property FieldName: String read FFieldName;
    property FieldType: Char read FFieldType write FFieldType;
  end;

  { TEQueryField }

  TEQueryField = class(TExpression)
  private
    FFieldName: String;
    FRD: TReportData;
    FDS: TDataSet;
  public
    constructor Create(const aFieldName: String; aRD: TReportData; aDS: TDataSet);
    function Calc: Variant; override;
  end;

  { TEUnary }

  TEUnary = class(TExpression)
  private
    FNot: Boolean;
    FExpr: TExpression;
  public
    constructor Create(E: TExpression; const Op: String);
    destructor Destroy; override;
    function Calc: Variant; override;
    property Expr: TExpression read FExpr;
  end;

  { TEBinary }

  TEBinary = class(TExpression)
  private
    FE1, FE2: TExpression;
    FOp: String;
    function Cast(O1, O2: Variant): Char;
    function CalcNum(O1, O2: Variant): Variant;
    function CalcStr(O1, O2: Variant): Variant;
    function CalcBool(O1, O2: Variant): Variant;
    function CalcDate(O1, O2: Variant): Variant;
    function CalcNull(O1, O2: Variant): Variant;
    procedure CheckZero(O2: Variant);
  public
    constructor Create(E1, E2: TExpression; const Op: String);
    destructor Destroy; override;
    function Calc: Variant; override;
    property E1: TExpression read FE1;
    property E2: TExpression read FE2;
  end;

  TExprArr = array of TExpression;

  { TEFunction }

  TEFunction = class(TExpression)
  private
    FFuncName: String;
    FArgs: TExprArr;
    FForm, FParForm: TdxForm;
    FDS: TDataSet;
    FIdx, FExprIdx: Integer;
    function CalcExprFunc(Vals: array of Variant): Variant;
    function CalcFunc(Vals: array of Variant): Variant;
    function CheckNull(Vals: array of Variant): Boolean;
  public
    constructor Create(aForm, aParForm: TdxForm; aDS: TDataSet;
    	const aFN: String; aIdx, aExprIdx: Integer; Args: TExprArr);
    destructor Destroy; override;
    function ResltType: Char;
    function Calc: Variant; override;
    property FuncName: String read FFuncName;
    property Args: TExprArr read FArgs;
  end;

  { TExpressionBuilder }

  TExpressionBuilder = class
  private
    FDataSet: TDataSet;
    FExpr: String;
    FForm: TdxForm;
    FParentForm: TdxForm;
    FPos, FLen: Integer;
    FRD: TReportData;
    FRDSet: TDataSet;
    FSkipLabels: Boolean;
    function GetChar: String;
    function InnerBuild(aState: Integer): TExpression;
    function LastState(aState: Integer): Boolean;
    function StartWith(const S: String): Boolean;
    procedure Skip(const S: String);
    procedure SkipLineComment;
    procedure SkipMultiLineComment;
    function ReadStateOperator(aState: Integer): String;
    function ReadSingle: TExpression;
    procedure CheckExprFunc(const Func: String; const Args: array of TExpression; FuncPos: Integer;
      const ArgsPos: array of Integer; var aIdx, aEIdx: Integer);
    procedure CheckFunc(const Func: String; const Args: array of TExpression; FuncPos: Integer;
      const ArgsPos: array of Integer; var aIdx, aEIdx: Integer);
    //function CheckTypes(O1, O2: TExpression): Boolean;
    procedure CheckQueryField(const FieldName: String; P: Integer);
    function CheckField(const FieldName: String; P: Integer): TComponent;
    function GetQueryFieldType(const FieldName: String): Char;
    function GetFieldType(C: TComponent): Char;
  public
    function Build(const aExpr: String): TExpression;
    function GetExprType(A: TExpression): Char;
    property Form: TdxForm read FForm write FForm;
    property DataSet: TDataSet read FDataSet write FDataSet;
    property ParentForm: TdxForm read FParentForm write FParentForm;
    property SkipLabels: Boolean read FSkipLabels write FSkipLabels;
    property RD: TReportData read FRD write FRD;
    property RDSet: TDataSet read FRDSet write FRDSet;
  end;

  { TExprList }

  TExprList = class(TList)
  private
    function GetExpressions(Index: Integer): TExpression;
  public
    procedure Clear; override;
    property Expressions[Index: Integer]: TExpression read GetExpressions; default;
  end;

implementation

uses
  Variants, formmanager, apputils, ExprFuncs,
  LazUtf8, towordsfuncs, DateUtils, dxfiles, dximages, scriptmanager, padeg;

const
  States: array [0..6, 0..5] of String =
    (('|', '', '', '', '', ''), ('&', '', '', '', '', ''),
    ('!', '', '', '', '', ''), ('=', '<>', '<=', '<', '>=', '>'),
    ('+', '-', '', '', '', ''), ('*', '/', '', '', '', ''),
    ('', '', '', '', '', ''));

  Funcs: array [0..143, 0..2] of String =
    (('COUNT', 's', 'n'),
    ('SUM', 'ss', 'n'),
    ('LENGTH', 's', 'n'),
    ('CUT', 'snn', 's'),
    ('FIND', 'ssn', 'n'),
    ('REPLACE', 'sss', 's'),
    ('REPLACEALL', 'sss', 's'),
    ('UPPER', 's', 's'),
    ('LOWER', 's', 's'),
    ('TOWORDS', 'n', 's'),
    ('RURTOWORDS', 'n', 's'),
    ('FUPPER', 's', 's'),
    ('ROUND', 'nn', 'n'),
    ('TRUNC', 'n', 'n'),
    ('FRAC', 'nn', 'n'),
    ('POWER', 'nn', 'n'),
    ('DATE', '', 'd'),
    ('TIME', '', 't'),
    ('YEARSBETWEEN', 'dd', 'n'),
    ('MONTHSBETWEEN', 'dd', 'n'),
    ('DAYSBETWEEN', 'dd', 'n'),
    ('DAYOF', 'd', 'n'),
    ('MONTHOF', 'd', 'n'),
    ('YEAROF', 'd', 'n'),
    ('WEEKOF', 'd', 'n'),
    ('WEEKDAY', 'd', 's'),
    ('WEEKDAYBRIEF', 'd', 's'),
    ('MONTH', 'd', 's'),
    ('MONTHBRIEF', 'd', 's'),
    ('NULL', '', 'v'),
    ('IIF', 'bvv', 'v'),
    ('DAYOFWEEK', 'd', 'n'),
    ('RECNO', 's', 'n'),
    ('SUMIF', 'sss', 'n'),
    ('COUNTIF', 'ss', 'n'),
    ('ROUNDTO', 'nn', 's'),
    ('MAX', 'ss', 'v'),
    ('MAXIF', 'sss', 'v'),
    ('MIN', 'ss', 'v'),
    ('MINIF', 'sss', 'v'),
    ('AVG', 'ss', 'n'),
    ('AVGIF', 'sss', 'n'),
    ('CSTR', 'v', 's'),
    ('CNUM', 's', 'n'),
    ('CDATE', 's', 'd'),
    ('ADDDAY', 'dn', 'd'),
    ('ADDWEEK', 'dn', 'd'),
    ('ADDMONTH', 'dn', 'd'),
    ('ADDYEAR', 'dn', 'd'),
    ('CTIME', 's', 't'),
    ('HOUROF', 't', 'n'),
    ('MINUTEOF', 't', 'n'),
    ('SECONDOF', 't', 'n'),
    ('HOURSBETWEEN', 'dtdt', 'n'),
    ('MINUTESBETWEEN', 'dtdt', 'n'),
    ('SECONDSBETWEEN', 'dtdt', 'n'),
    ('ADDHOUR', 'dtns', 't'),
    ('ADDMINUTE', 'dtns', 't'),
    ('ADDSECOND', 'dtns', 't'),
    ('PERIOD', 'ddn', 's'),
    ('INDEXOF', 'ns', 's'),
    ('NZ', 'vv', 'v'),
    ('FDATE', 'd', 's'),
    ('MERGE', 'sss', 's'),
    ('RECID', 's', 'n'),
    ('OBJID', 'sss', 'n'),
    ('USER', '', 's'),
    ('ROLE', '', 's'),
    ('DBGET', 'sss', 'v'),
    ('DBSUM', 'sss', 'n'),
    ('DBAVG', 'sss', 'n'),
    ('DBMAX', 'sss', 'v'),
    ('DBMIN', 'sss', 'v'),
    ('DBCOUNT', 'ss', 'n'),
    ('DBGETBYID', 'ssn', 'v'),
    ('MAXV', '...', 'v'),
    ('MINV', '...', 'v'),
    ('DBGETID', 'ss', 'n'),
    ('SETVAR', 'sv', 'v'),
    ('GETVAR', 's', 'v'),
    ('IFZ', 'nv', 'v'),
    ('IFE', 'sv', 'v'),
    ('TRIM', 's', 's'),
    ('ZEROS', 'nn', 's'),
    ('AGE', 'ddn', 's'),
    //('SUMPERIOD', 'sssn', 's'),
    ('BLOCK', '...', 'v'),
    ('VAREXISTS', 's', 'n'),
    ('PATHLEN', 's', 'n'),
    ('EXTRACTPATH', 'snn', 's'),
    ('NEWLINE', '', 's'),
    ('NEWREC', '', 'n'),
    ('GET', 'ss', 'v'),
    ('DBCOUNTD', 'sss', 'n'),
    ('OLDVALUE', 's', 'v'),
    ('DBMERGE', 'sss', 's'),
    ('DBMERGEALL', 'sss', 's'),
    ('BEGINYEAR', 'd', 'd'),
    ('BEGINMONTH', 'd', 'd'),
    ('BEGINWEEK', 'd', 'd'),
    ('ENDYEAR', 'd', 'd'),
    ('ENDMONTH', 'd', 'd'),
    ('ENDWEEK', 'd', 'd'),
    ('BEGINQUARTER', 'd', 'd'),
    ('ENDQUARTER', 'd', 'd'),
    ('QUARTEROF', 'd', 'n'),
    ('DBUNIQUE', 's', 'n'),
    ('UNIQUE', 'ss', 'n'),
    ('MSGBOX', 'ss', 'n'),
    ('YESNOBOX', 'ss', 'n'),
    ('YESNOCANCELBOX', 'ss', 'n'),
    ('SETFIELD', 'sv', 'v'),
    ('SETLABEL', 'sv', 'v'),
    ('EDITREC', '', 'n'),
    ('MODIFIEDREC', '', 'n'),

    ('PADEG_FIO', 'snn', 's'),
    ('PADEG_IF', 'snn', 's'),
    ('PADEG_NOMINATIVE', 's', 's'),
    ('PADEG_APPOINTMENT', 'sn', 's'),
    ('PADEG_FULLAPPOINTMENT', 'ssn', 's'),
    ('PADEG_OFFICE', 'sn', 's'),
    ('PADEG_SEX', 's', 'n'),
    ('PADEG_ID', 's', 'n'),
    ('PADEG_FIOBRIEF', 'snn', 's'),
    ('PADEG_IOFBRIEF', 'snn', 's'),
    ('PADEG_F', 's', 's'),
    ('PADEG_I', 's', 's'),
    ('PADEG_O', 's', 's'),

    ('RESULT', '', 'v'),
    ('CONCAT', '...', 's'),
    ('FNUMBER', 'nn', 's'),
    ('TEXT', 's', 's'),
    ('TIMESTAMP', 'dt', 'n'),
    ('MERGEX', 'sss', 's'),
    ('TAKE', 'sss', 'v'),

    ('CASEOF', 'vs', 's'),

    ('MONEYTOWORDS', 'nsnn', 's'),
    ('NUMTOWORDS', 'nnn', 's'),
    ('NUMPADEG', 'snnnn', 's'),
    ('FRACTOWORDS', 'n', 's'),

    ('TYPEDTEXT', 'n', 'v'),
    ('ISWEB', '', 'b'),
    ('ISSERVICE', '', 'b'),

    ('ENCODEDATE', 'nnn', 'd'),
    ('ENCODETIME', 'nnn', 't')

    );

    FUNC_COUNT = 0;
    FUNC_SUM = 1;
    FUNC_LENGTH = 2;
    FUNC_CUT = 3;
    FUNC_FIND = 4;
    FUNC_REPLACE = 5;
    FUNC_REPLACEALL = 6;
    FUNC_UPPER = 7;
    FUNC_LOWER = 8;
    FUNC_TOWORDS = 9;
    FUNC_RURTOWORDS = 10;
    FUNC_FUPPER = 11;
    FUNC_ROUND = 12;
    FUNC_TRUNC = 13;
    FUNC_FRAC = 14;
    FUNC_POWER = 15;
    FUNC_DATE = 16;
    FUNC_TIME = 17;
    FUNC_YEARSBETWEEN = 18;
    FUNC_MONTHSBETWEEN = 19;
    FUNC_DAYSBETWEEN = 20;
    FUNC_DAYOF = 21;
    FUNC_MONTHOF = 22;
    FUNC_YEAROF = 23;
    FUNC_WEEKOF = 24;
    FUNC_WEEKDAY = 25;
    FUNC_WEEKDAYBRIEF = 26;
    FUNC_MONTH = 27;
    FUNC_MONTHBRIEF = 28;
    FUNC_NULL = 29;
    FUNC_IIF = 30;
    FUNC_DAYOFWEEK = 31;
    FUNC_RECNO = 32;
    FUNC_SUMIF = 33;
    FUNC_COUNTIF = 34;
    FUNC_ROUNDTO = 35;
    FUNC_MAX = 36;
    FUNC_MAXIF = 37;
    FUNC_MIN = 38;
    FUNC_MINIF = 39;
    FUNC_AVG = 40;
    FUNC_AVGIF = 41;
    FUNC_CSTR = 42;
    FUNC_CNUM = 43;
    FUNC_CDATE = 44;
    FUNC_ADDDAY = 45;
    FUNC_ADDWEEK = 46;
    FUNC_ADDMONTH = 47;
    FUNC_ADDYEAR = 48;
    FUNC_CTIME = 49;
    FUNC_HOUROF = 50;
    FUNC_MINUTEOF = 51;
    FUNC_SECONDOF = 52;
    FUNC_HOURSBETWEEN = 53;
    FUNC_MINUTESBETWEEN = 54;
    FUNC_SECONDSBETWEEN = 55;
    FUNC_ADDHOUR = 56;
    FUNC_ADDMINUTE = 57;
    FUNC_ADDSECOND = 58;
    FUNC_PERIOD = 59;
    FUNC_INDEXOF = 60;
    FUNC_NZ = 61;
    FUNC_FDATE = 62;
    FUNC_MERGE = 63;
    FUNC_RECID = 64;
    FUNC_OBJID = 65;
    FUNC_USER = 66;
    FUNC_ROLE = 67;
    FUNC_DBGET = 68;
    FUNC_DBSUM = 69;
    FUNC_DBAVG = 70;
    FUNC_DBMAX = 71;
    FUNC_DBMIN = 72;
    FUNC_DBCOUNT = 73;
    FUNC_DBGETBYID = 74;
    FUNC_MAXV = 75;
    FUNC_MINV = 76;
    FUNC_DBGETID = 77;
    FUNC_SETVAR = 78;
    FUNC_GETVAR = 79;
    FUNC_IFZ = 80;
    FUNC_IFE = 81;
    FUNC_TRIM = 82;
    FUNC_ZEROS = 83;
    FUNC_AGE = 84;
    //FUNC_SUMPERIOD = ;
    FUNC_BLOCK = 85;
    FUNC_VAREXISTS = 86;
    FUNC_PATHLEN = 87;
    FUNC_EXTRACTPATH = 88;
    FUNC_NEWLINE = 89;
    FUNC_NEWREC = 90;
    FUNC_GET = 91;
    FUNC_DBCOUNTD = 92;
    FUNC_OLDVALUE = 93;
    FUNC_DBMERGE = 94;
    FUNC_DBMERGEALL = 95;
    FUNC_BEGINYEAR = 96;
    FUNC_BEGINMONTH = 97;
    FUNC_BEGINWEEK = 98;
    FUNC_ENDYEAR = 99;
    FUNC_ENDMONTH = 100;
    FUNC_ENDWEEK = 101;
    FUNC_BEGINQUARTER = 102;
    FUNC_ENDQUARTER = 103;
    FUNC_QUARTEROF = 104;
    FUNC_DBUNIQUE = 105;
    FUNC_UNIQUE = 106;
    FUNC_MSGBOX = 107;
    FUNC_YESNOBOX = 108;
    FUNC_YESNOCANCELBOX = 109;
    FUNC_SETFIELD = 110;
    FUNC_SETLABEL = 111;
    FUNC_EDITREC = 112;
    FUNC_MODIFIEDREC = 113;

    FUNC_PADEG_FIO = 114;
    FUNC_PADEG_IF = 115;
    FUNC_PADEG_NOMINATIVE = 116;
    FUNC_PADEG_APPOINTMENT = 117;
    FUNC_PADEG_FULLAPPOINTMENT = 118;
    FUNC_PADEG_OFFICE = 119;
    FUNC_PADEG_SEX = 120;
    FUNC_PADEG_ID = 121;
    FUNC_PADEG_FIOBRIEF = 122;
    FUNC_PADEG_IOFBRIEF = 123;
    FUNC_PADEG_F = 124;
    FUNC_PADEG_I = 125;
    FUNC_PADEG_O = 126;

    FUNC_RESULT = 127;
    FUNC_CONCAT = 128;
    FUNC_FNUMBER = 129;
    FUNC_TEXT = 130;
    FUNC_TIMESTAMP = 131;
    FUNC_MERGEX = 132;
    FUNC_TAKE = 133;

    FUNC_CASEOF = 134;

    FUNC_MONEYTOWORDS = 135;
    FUNC_NUMTOWORDS = 136;
    FUNC_NUMPADEG = 137;
    FUNC_FRACTOWORDS = 138;

    FUNC_TYPEDTEXT = 139;
    FUNC_ISWEB = 140;
    FUNC_ISSERVICE = 141;

    FUNC_ENCODEDATE = 142;
    FUNC_ENCODETIME = 143;


function FindFunc(const Func: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  //Func := AnsiUpperCase(Func);
  for i := 0 to High(Funcs) do
    if Funcs[i, 0] = Func then Exit(i);
end;

{ TEQueryField }

constructor TEQueryField.Create(const aFieldName: String; aRD: TReportData;
  aDS: TDataSet);
begin
  FFieldName := aFieldName;
  FRD := aRD;
  FDS := aDS;
end;

function TEQueryField.Calc: Variant;
var
  Col: TRpGridColumn;
  F: TField;
begin
  Result := Null;
  Col := FRD.Grid.FindColumnByTitle(FFieldName);
  if Col <> nil then
  begin
    F := FDS.FindField(Col.FieldNameDS);
    if F <> nil then Result := F.Value;
  end;
end;


{ TExprList }

function TExprList.GetExpressions(Index: Integer): TExpression;
begin
  Result := TExpression(Items[Index]);
end;

procedure TExprList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Expressions[i].Free;
  inherited Clear;
end;

{ TEFunction }

function TEFunction.CalcExprFunc(Vals: array of Variant): Variant;
var
  F: TExprFunc;
  pV: PIFVariant;
  OldSelf: TObject;
  ok: Boolean;
  SD: TScriptData;
  Exec: TPSDebugExec;
begin
  F := ScriptMan.Funcs[FExprIdx];
  SD := ScriptMan.Scripts[F.SDi];
  Exec := ExtRunMan.GetExec(SD);

  pV := Exec.GetVar2('Self');
  if pV = nil then raise ECalcError.Create(rsExtModuleFailedMsg, [F.Name, SD.Name], FPos);

	OldSelf := PSGetObject(@PPSVariantData(pV)^.Data, pV^.FType);
  SetVariantToClass(pV, FForm);

  try
	  ok := ExtRunMan.TryRunFunc(SD, F.OrigName, Vals, Result);
  finally
	  SetVariantToClass(pV, OldSelf);
  end;
  if not ok then
  	raise ECalcError.Create(rsFuncNotFoundInModule, [F.OrigName, SD.Name], FPos);
end;

function TEFunction.CalcFunc(Vals: array of Variant): Variant;
var
  //FN: String;
  V: Variant;
begin
  Result := Null;
  if FExprIdx >= 0 then
  begin
    Result := CalcExprFunc(Vals);
    Exit;
  end;

  //FN := Funcs[FIdx, 0];
  if CheckNull(Vals) and not (FIdx in [FUNC_IIF, FUNC_CSTR, FUNC_NZ, FUNC_SETVAR,
    FUNC_SETFIELD, FUNC_SETLABEL, FUNC_BLOCK, FUNC_CONCAT, FUNC_IFZ, FUNC_IFE,
    FUNC_TIMESTAMP, FUNC_CASEOF]) then Exit;

  {(FN <> 'IIF') and (FN <> 'CSTR') and (FN <> 'NZ')
    and (FN <> 'SETVAR') and (FN <> 'SETFIELD') and (FN <> 'SETLABEL')
    and (FN <> 'BLOCK') and (FN <> 'CONCAT') and (FN <> 'IFZ') and (FN <> 'IFE')
    and (FN <> 'TIMESTAMP') then Exit;  }

  case FIdx of
    FUNC_COUNT: V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], '', '', tfCount);
    FUNC_SUM: V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], '', tfSum);
    FUNC_LENGTH: V := Utf8Length(Vals[0]);
    FUNC_CUT: V := Utf8Copy(Vals[0], Vals[1], Vals[2]);
    FUNC_FIND: V := Utf8Pos(Vals[0], Vals[1], Vals[2]);
    FUNC_REPLACE: V := Utf8StringReplace(Vals[0], Vals[1], Vals[2], []);
    FUNC_REPLACEALL: V := Utf8StringReplace(Vals[0], Vals[1], Vals[2], [rfReplaceAll]);
    FUNC_UPPER: V := Utf8UpperCase(Vals[0]);
    FUNC_LOWER: V := Utf8LowerCase(Vals[0]);
    FUNC_TOWORDS: V := ToWords(Vals[0], False);
    FUNC_RURTOWORDS: V := ToWords(Vals[0], True);
    FUNC_FUPPER: V := FirstLetterUpper(Vals[0]);
    FUNC_ROUND: V := MathRound(Extended(Vals[0]), Vals[1]);
    FUNC_TRUNC: V := Trunc(Double(Vals[0]));
    FUNC_FRAC: V := MyFrac(Vals[0], Vals[1]);
    FUNC_POWER: V := MyPower(Vals[0], Vals[1]);
    FUNC_DATE: V := Date;
    FUNC_TIME: V := Time;
    FUNC_YEARSBETWEEN: V := YearsBetween(Vals[0], Vals[1]);
    FUNC_MONTHSBETWEEN: V := MonthsBetween(Vals[0], Vals[1]);
    FUNC_DAYSBETWEEN: V := DaysBetween(Vals[0], Vals[1]);
    FUNC_DAYOF: V := DayOf(Vals[0]);
    FUNC_MONTHOF: V := MonthOf(Vals[0]);
    FUNC_YEAROF: V := YearOf(Vals[0]);
    FUNC_WEEKOF: V := WeekOf(Vals[0]);
    FUNC_WEEKDAY: V := GetWeekName(Vals[0], False);
    FUNC_WEEKDAYBRIEF: V := GetWeekName(Vals[0], True);
    FUNC_MONTH: V := GetMonthName(Vals[0], False);
    FUNC_MONTHBRIEF: V := GetMonthName(Vals[0], True);
    FUNC_NULL: V := Null;
    FUNC_IIF: V := IIF(Vals[0], Vals[1], Vals[2]);
    FUNC_DAYOFWEEK: V := DayOfTheWeek(Vals[0]);
    FUNC_RECNO: V := GetRecNo(FParForm, Vals[0]);
    FUNC_SUMIF: V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfSum);
    FUNC_COUNTIF: V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], '', Vals[1], tfCount);
    FUNC_ROUNDTO: V := MyRoundToStr(Vals[0], Vals[1]);
    FUNC_MAX: V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], '', tfMax);
    FUNC_MAXIF: V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfMax);
    FUNC_MIN: V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], '', tfMin);
    FUNC_MINIF: V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfMin);
    FUNC_AVG: V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], '', tfAvg);
    FUNC_AVGIF: V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfAvg);
    FUNC_CSTR: V := VarToStr(Vals[0]);
    FUNC_CNUM: V := StrToFloat(Vals[0]);
    FUNC_CDATE: V := TextToDate(Vals[0]);
    FUNC_ADDDAY: V := IncDay(Vals[0], Vals[1]);
    FUNC_ADDWEEK: V := IncWeek(Vals[0], Vals[1]);
    FUNC_ADDMONTH: V := IncMonth(Vals[0], Vals[1]);
    FUNC_ADDYEAR: V := IncYear(Vals[0], Vals[1]);
    FUNC_CTIME: V := StrToTime(Vals[0]);
    FUNC_HOUROF: V := HourOf(Vals[0]);
    FUNC_MINUTEOF: V := MinuteOf(Vals[0]);
    FUNC_SECONDOF: V := SecondOf(Vals[0]);
    FUNC_HOURSBETWEEN: V := MyHoursBetween(Vals[0], Vals[1], Vals[2], Vals[3]);
    FUNC_MINUTESBETWEEN: V := MyMinutesBetween(Vals[0], Vals[1], Vals[2], Vals[3]);
    FUNC_SECONDSBETWEEN: V := MySecondsBetween(Vals[0], Vals[1], Vals[2], Vals[3]);
    FUNC_ADDHOUR: V := AddHour(FForm, FDS, Vals[0], Vals[1], Vals[2], Vals[3], 'h');
    FUNC_ADDMINUTE: V := AddHour(FForm, FDS, Vals[0], Vals[1], Vals[2], Vals[3], 'm');
    FUNC_ADDSECOND: V := AddHour(FForm, FDS, Vals[0], Vals[1], Vals[2], Vals[3], 's');
    FUNC_PERIOD: V := CalcPeriod(Vals[0], Vals[1], Vals[2], False);
    FUNC_INDEXOF: V := MyIndexOf(Vals[0], Vals[1]);
    FUNC_NZ: V := IIF(Vals[0] = Null, Vals[1], Vals[0]);
    FUNC_FDATE: V := FmtDate(Vals[0]);
    FUNC_MERGE: V := MergeRows(FForm, Vals[0], Vals[1], Vals[2]);
    FUNC_RECID: V := GetRecId(FForm, Vals[0]);
    FUNC_OBJID: V := GetObjId(Vals[0], Vals[1], Vals[2]);
    FUNC_USER: V := GetUser;
    FUNC_ROLE: V := GetRole;
    FUNC_DBGET: V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfNone);
    FUNC_DBSUM: V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfSum);
    FUNC_DBAVG: V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfAvg);
    FUNC_DBMAX: V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfMax);
    FUNC_DBMIN: V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfMin);
    FUNC_DBCOUNT: V := DBGet(FForm, FParForm, FDS, Vals[0], '', Vals[1], tfCount);
    FUNC_DBGETBYID: V := DBGetById(Vals[0], Vals[1], Vals[2]);
    FUNC_MAXV: V := GetMaxV(Vals);
    FUNC_MINV: V := GetMinV(Vals);
    FUNC_DBGETID: V := DBGet(FForm, FParForm, FDS, Vals[0], '', Vals[1], tfNone);
    FUNC_SETVAR: V := SetVar(Vals[0], Vals[1]);
    FUNC_GETVAR: V := GetVar(Vals[0]);
    FUNC_IFZ: V := IIF(Vals[0] = 0, Vals[1], Vals[0]);
    FUNC_IFE: V := IIF(Vals[0] = '', Vals[1], Vals[0]);
    FUNC_TRIM: V := Trim(Vals[0]);
    FUNC_ZEROS: V := SetZeros(Vals[0], Vals[1]);
    FUNC_AGE: V := CalcPeriod(Vals[0], Vals[1], Vals[2], True);
    //FUNC_SUMPERIOD: V := SumPeriod(FForm, Vals[0], Vals[1], Vals[2], Vals[3]);
    FUNC_BLOCK: V := Block(Vals);
    FUNC_VAREXISTS: V := VarExists(Vals[0]);
    FUNC_PATHLEN: V := PathLength(Vals[0]);
    FUNC_EXTRACTPATH: V := ExtractPath(Vals[0], Vals[1], Vals[2]);
    FUNC_NEWLINE: V := LineEnding;
    FUNC_NEWREC: V := IsNewRec(FDS);
    FUNC_GET: V := GetFieldValue(FForm, Vals[0], Vals[1]);
    FUNC_DBCOUNTD: V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfDistCount);
    FUNC_OLDVALUE: V := GetOldValue(FForm, FDS, Vals[0]);
    FUNC_DBMERGE: V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfMerge);
    FUNC_DBMERGEALL: V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfMergeAll);
    FUNC_BEGINYEAR: V := BeginYear(Vals[0]);
    FUNC_BEGINMONTH: V := BeginMonth(Vals[0]);
    FUNC_BEGINWEEK: V := BeginWeek(Vals[0]);
    FUNC_ENDYEAR: V := EndYear(Vals[0]);
    FUNC_ENDMONTH: V := EndMonth(Vals[0]);
    FUNC_ENDWEEK: V := EndWeek(Vals[0]);
    FUNC_BEGINQUARTER: V := BeginQuarter(Vals[0]);
    FUNC_ENDQUARTER: V := EndQuarter(Vals[0]);
    FUNC_QUARTEROF: V := QuarterOf(Vals[0]);
    FUNC_DBUNIQUE: V := DBUnique(FForm, FDS, Vals[0]);
    FUNC_UNIQUE: V := IsUniqueRecords(FForm, Vals[0], Vals[1]);
    FUNC_MSGBOX: V := MsgBox(Vals[0], Vals[1]);
    FUNC_YESNOBOX: V := YesNoBox(Vals[0], Vals[1]);
    FUNC_YESNOCANCELBOX: V := YesNoCancelBox(Vals[0], Vals[1]);
    FUNC_SETFIELD: V := SetField(FForm, FDS, Vals[0], Vals[1]);
    FUNC_SETLABEL: V := SetLabel(FForm, Vals[0], Vals[1]);
    FUNC_EDITREC: V := IsEditRec(FForm);
    FUNC_MODIFIEDREC: V := IsModifiedRec(FForm);

    // Падеж
    FUNC_PADEG_FIO: V := GetFIOPadeg(Vals[0], Vals[1], Vals[2]);
    FUNC_PADEG_IF: V := GetIFPadeg(Vals[0], Vals[1], Vals[2]);
    FUNC_PADEG_NOMINATIVE: V := GetNominativePadeg(Vals[0]);
    FUNC_PADEG_APPOINTMENT: V := GetAppointmentPadeg(Vals[0], Vals[1]);
    FUNC_PADEG_FULLAPPOINTMENT: V := GetFullAppointmentPadeg(Vals[0], Vals[1], Vals[2]);
    FUNC_PADEG_OFFICE: V := GetOfficePadeg(Vals[0], Vals[1]);
    FUNC_PADEG_SEX: V := GetSex(Vals[0]);
    FUNC_PADEG_ID: V := GetPadegID(Vals[0]);
    FUNC_PADEG_FIOBRIEF: V := GetFIOBriefPadeg(Vals[0], Vals[1], Vals[2]);
    FUNC_PADEG_IOFBRIEF: V := GetIOFBriefPadeg(Vals[0], Vals[1], Vals[2]);
    FUNC_PADEG_F: V := GetF(Vals[0]);
    FUNC_PADEG_I: V := GetI(Vals[0]);
    FUNC_PADEG_O: V := GetO(Vals[0]);
    //

    FUNC_RESULT: V := FForm.ActionResult;
    FUNC_CONCAT: V := Concat(Vals);
    FUNC_FNUMBER: V := FNumber(Vals[0], Vals[1]);
    FUNC_TEXT: V := TextFormat(Vals[0], FForm);
    FUNC_TIMESTAMP: V := GetTimeStamp(Vals[0], Vals[1]);
    FUNC_MERGEX: V := MergeRowsEx(FForm, Vals[0], Vals[1], Vals[2]);
    FUNC_TAKE: V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfNone);

    FUNC_CASEOF: V := CaseOf(VarToStr(Vals[0]), VarToStr(Vals[1]));

    // Падеж
    FUNC_MONEYTOWORDS: V := DeclCurrency(Vals[0], Vals[1], Vals[2], Vals[3]);
    FUNC_NUMTOWORDS: V := NumberToString(Vals[0], Vals[1], 15, True, Vals[2] = 1);
    FUNC_NUMPADEG: V := DeclNumeral(Vals[0], Vals[1], Vals[2], Vals[3], Vals[4]);
    FUNC_FRACTOWORDS: V := DoubleToVerbal(Vals[0]);
    //

    FUNC_TYPEDTEXT: V := GetTypedText(Vals[0]);
    FUNC_ISWEB: V := IsWebServer;
    FUNC_ISSERVICE: V := False;

    FUNC_ENCODEDATE: V := EncodeDate(Vals[0], Vals[1], Vals[2]);
    FUNC_ENCODETIME: V := EncodeTime(Vals[0], Vals[1], Vals[2], 0);
  end;
  Result := V;
end;

function TEFunction.CheckNull(Vals: array of Variant): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(Vals) - 1 do
    if Vals[i] = Null then Exit(True);
end;

constructor TEFunction.Create(aForm, aParForm: TdxForm; aDS: TDataSet;
  const aFN: String; aIdx, aExprIdx: Integer; Args: TExprArr);
var
  i: Integer;
begin
  FForm := aForm;
  FParForm := aParForm;
  FDS := aDS;
  FFuncName := aFN;
  FIdx := aIdx;
  FExprIdx := aExprIdx;
  SetLength(FArgs, Length(Args));
  for i := 0 to High(Args) do FArgs[i] := Args[i];
end;

destructor TEFunction.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FArgs) do FArgs[i].Free;
  SetLength(FArgs, 0);
  inherited Destroy;
end;

function TEFunction.ResltType: Char;
begin
  if FExprIdx >= 0 then
  	Result := ScriptMan.Funcs[FExprIdx].ResultType
  else if FIdx >= 0 then
	  Result := Funcs[FIdx, 2][1]
end;

function TEFunction.Calc: Variant;
var
  Vals: array of Variant;
  i, P: Integer;
  Msg: String;
begin
  SetLength(Vals, Length(FArgs));
  try try
    // Здесь особая обработка функции IIF. Если значение истина, то вычисляется
    // только 2 аргумент, иначе только третий. Т. о. можно использовать присваивание
    // переменным в разных ветках.
    if LowerCase(FFuncName) = 'iif' then
    begin
      Vals[0] := FArgs[0].Calc;
      Vals[1] := Null;
      Vals[2] := Null;
      if VarIsBool(Vals[0]) then
      begin
        if Vals[0] = True then
          Vals[1] := FArgs[1].Calc
        else
          Vals[2] := FArgs[2].Calc;
      end;
    end
    else
      for i := 0 to High(FArgs) do
        Vals[i] := FArgs[i].Calc;
    Result := CalcFunc(Vals);
  except
    // Гасим ошибку, если датасет еще не успел открыться
    //on E: EGetQueryDataError do
    //	;
    on E: Exception do
    begin
      if E = ScriptLastError.ExObj then Msg := ScriptLastErrorToString
      else if E is EPSException then Msg := EPSexceptionToString(EPSException(E))
      else if ExceptionInvalidValueForField(E) then Msg := rsIncompatibleValueForField
      else Msg := E.Message;
      P := FPos;
      if E is ECalcError then P := ECalcError(E).Position;
      raise ECalcError.Create(FFuncName + ' -> ' + Msg, [], P);
      //raise Exception.CreateFmt('%s -> %s', [FFuncName, Msg]);
    end;
  end;
  finally
    SetLength(Vals, 0);
  end;
end;

{ TExpressionBuilder }

function TExpressionBuilder.Build(const aExpr: String): TExpression;
begin
  FExpr := aExpr;
  FLen := Length(aExpr);
  FPos := 1;
  Skip(' ');
  Result := InnerBuild(0);
  if (Result <> nil) and (FPos <= FLen) then
  begin
    FreeAndNil(Result);
    raise ECalcError.Create(rsUnexpectedToken {rsExpectedOperationOrBracket}, [GetChar], FPos);
  end;
end;

function OpTypeToStr(T: Char): String;
begin
  case T of
    'n': Result := rsNumber;
    's': Result := rsText;
    'd': Result := rsDate;
    't': Result := rsTime;
    'b': Result := rsBoolean;
    'v': Result := rsNumberOrText;
    else Result := rsUnknown;
  end;
end;

// Для rsUnexpectedToken
function TExpressionBuilder.GetChar: String;
var
  p: Integer;
begin
  p := FPos;
  Result := '';
  repeat
    Result := Result + FExpr[p];
    Inc(p);
  until (p > FLen) or not (FExpr[p] in [#129..#191]);
end;

function TExpressionBuilder.InnerBuild(aState: Integer): TExpression;
var
  IsMinus, UnarNot: Boolean;
  a1, a2: TExpression;
  Op: String;
  Op1Pos, Op2Pos: Integer;
  t: Char;

  procedure CheckOperands;
  var
    t1, t2: Char;
    t1S, t2S: String;
  begin
    t1 := GetExprType(a1);
    t1S := OpTypeToStr(t1);
    t2 := GetExprType(a2);
    t2S := OpTypeToStr(t2);

    case Op of
      '+':
        if not (t1 in ['n', 's', 'v']) then
          raise ECalcError.Create(rsInvalidOpForType, [Op, t1S], Op1Pos)
        else if not (t2 in ['n', 's', 'v']) then
          raise ECalcError.Create(rsInvalidOpForTypeExpected, [Op, t2S, t1S], Op2Pos)
        else if ((t1 = 'n') and (t2 = 's')) or ((t1 = 's') and (t2 = 'n')) then
          raise ECalcError.Create(rsOpNotValidForDifOperands, [Op, t1S, t2S], Op2Pos);
      '-', '*', '/':
        if not (t1 in ['n', 'v']) then
          raise ECalcError.Create(rsInvalidOpForTypeExpected, [Op, t1S, rsNumber], Op1Pos)
        else if not (t2 in ['n', 'v']) then
          raise ECalcError.Create(rsInvalidOpForTypeExpected, [Op, t2S, rsNumber], Op2Pos);
      // логические
      '&', '|':
        if not (t1 in ['b', 'v']) then
          raise ECalcError.Create(rsInvalidOpForTypeExpected, [Op, t1S, rsBoolean], Op1Pos)
        else if not (t2 in ['b', 'v']) then
          raise ECalcError.Create(rsInvalidOpForTypeExpected, [Op, t2S, rsBoolean], Op2Pos);
      // Равенство
      else
        if (t1 <> t2) and (t1 <> 'v') and (t2 <> 'v') then
          raise ECalcError.Create(rsOpNotValidForDifOperands, [Op, t1S, t2S],  Op2Pos);
    end;
  end;

begin
  if LastState(aState) then
  begin
    Result := nil;
    Op1Pos := FPos;
    IsMinus := StartWith('-');
    if IsMinus then
    begin
      Skip('-');
      Op1Pos := FPos;
    end;
    if StartWith('(') then
    begin
      Skip('(');
      Result := InnerBuild(0);
      if not StartWith(')') then
      begin
        FreeAndNil(Result);
        raise ECalcError.Create(rsExpectedClosingBracketOrOp, [], FPos);
      end;
      Skip(')');
    end
    else
      Result := ReadSingle;
    if IsMinus then
    begin
      t := GetExprType(Result);
      if not (t in ['n', 'v']) then
      begin
        FreeAndNil(Result);
        raise ECalcError.Create(rsInvalidOpForTypeExpected, ['-', OpTypeToStr(t), rsNumber], Op1Pos);
      end;
      Result := TEUnary.Create(Result, '-');
      //Result.Position := Op1Pos;
    end;
    if Result <> nil then Result.Position := Op1Pos;
    Exit;
  end;

  UnarNot := (aState = 2) and StartWith('!');
  if UnarNot then
    Skip('!');

  // Первый операнд
  Op1Pos := FPos;
  a1 := InnerBuild(aState + 1);
  if UnarNot then
  begin
    t := GetExprType(a1);
    if not (t in ['b', 'v']) then
    begin
      FreeAndNil(a1);
      raise ECalcError.Create(rsInvalidOpForTypeExpected, ['!', OpTypeToStr(t), rsBoolean], Op1Pos);
    end;
    a1 := TEUnary.Create(a1, '!');
    a1.Position := Op1Pos;
  end;

  // последуюущие операнды
  while True do
  begin
    Op := ReadStateOperator(aState);
    if Op='' then Break;
    Op2Pos := FPos;
    try
      a2 := nil;
      a2 := InnerBuild(aState + 1);

      if (a1 = nil) or (a2 = nil) then
      	raise ECalcError.Create(rsOperandExpected, [], Op2Pos);
      CheckOperands;
    except
      on E: Exception do
      begin
        FreeAndNil(a1);
        FreeAndNil(a2);
        raise;
      end;
    end;
    a1 := TEBinary.Create(a1, a2, Op);
    a1.Position := Op1Pos;
  end;

  Result := a1;
end;

function TExpressionBuilder.LastState(aState: Integer): Boolean;
begin
  Result := (aState + 1) >= Length(States);
end;

function TExpressionBuilder.StartWith(const S: String): Boolean;
begin
  Result := Copy(FExpr, FPos, Length(S)) = S;
end;

procedure TExpressionBuilder.Skip(const S: String);
begin
  if StartWith(S) then
    FPos := FPos + Length(S);
  while (FPos <= FLen) and (FExpr[FPos] in [#9, #10, #13, #32]) do
    Inc(FPos);
  // Пропускаем комментарии
  if StartWith('//') then SkipLineComment
  else if StartWith('/*') then SkipMultiLineComment;
end;

procedure TExpressionBuilder.SkipLineComment;
begin
  Skip('//');
  // Есть баг с пропуском пустого комментария: следующая строка считается комментарием.
  // Делаем проверку был ли предыдущий символ переводом строки.
  if (FPos > 2) and (FExpr[FPos - 1] in [#10, #13]) then Exit;
  //
  while (FPos <= FLen) and (not (FExpr[FPos] in [#10, #13])) do
    Inc(FPos);
  Skip('');
end;

procedure TExpressionBuilder.SkipMultiLineComment;
begin
  Skip('/*');
  while (FPos < FLen) do
  begin
    if (FExpr[FPos] = '*') and (FExpr[FPos + 1] = '/') then Break
    else Inc(FPos);
  end;
  Skip('*/');
end;

function TExpressionBuilder.ReadStateOperator(aState: Integer): String;
var
  Ops: array [0..5] of String;
  i: Integer;
begin
  Result := '';
  Ops := States[aState];
  for i := 0 to High(Ops) do
    if Ops[i] = '' then Exit
    else if StartWith(Ops[i]) then
    begin
      Skip(Ops[i]);
      Exit(Ops[i]);
    end;
end;

function TExpressionBuilder.ReadSingle: TExpression;
var
  p0, idx, eidx, FuncPos, i: Integer;
  q: Char;
  E: Extended;
  Args: array of TExpression;
  ArgsPos: array of Integer;
  A: TExpression;
  Func, S: String;
  FS: TFormatSettings;
  C: TComponent;
begin
  p0 := FPos;

  // строка
  if StartWith('''') or StartWith('"') then
  begin
    q := '''';
    if StartWith('"') then q := '"';
    FPos := PosEx(q, FExpr, FPos + 1);
    if FPos = 0 then raise ECalcError.Create(rsEndLineExpected, [], p0);
    Result := TEString.Create(Copy(FExpr, p0 + 1, FPos - p0 - 1));
    //Result.Position := p0;
    Skip(q);
    Exit;
  end;

  // поле
  if StartWith('[') then
  begin
    FPos := PosEx(']', FExpr, FPos + 1);
    if FPos = 0 then raise ECalcError.Create(rsSqBrExpected, [], p0);
    S := Copy(FExpr, p0 + 1, FPos - p0 - 1);
    if S = '' then raise ECalcError.Create(rsFieldNameEmpty, [], p0);
    // Это может быть поле и формы, и запроса/отчета
    if (FRD <> nil) and (S[1] <> '!') and (S[1] <> ':') then
    begin
      CheckQueryField(S, p0);
      Result := TEQueryField.Create(S, FRD, FRDSet);
      //Result.Position := p0;
    end
    else if FForm <> nil then
    begin
      C := CheckField(S, p0);
      Result := TEField.Create(S, FForm, FParentForm, FDataSet, FSkipLabels);
      //Result.Position := p0;
      TEField(Result).FieldType := GetFieldType(C);
    end
    else
      raise ECalcError.Create(rsFieldNotFound, [S], p0);
    Skip(']');
    Exit;
  end;

  // число
  while (FPos <= FLen) and (FExpr[FPos] in ['0'..'9', '.']) do
    Inc(FPos);

  if FPos > p0 then
  begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator:='.';
    S := Copy(FExpr, p0, FPos - p0);
    if not TryStrToFloat(S, E, FS) then
      raise ECalcError.Create(rsInvalidNumber, [S], p0);
    Skip(' ');
    Result := TENumber.Create(E);
    //Result.Position := p0;
    Exit;
  end;

  // функция
  while (FPos <= FLen) and (FExpr[FPos] in ['A'..'Z', 'a'..'z', '_', '0'..'9']) do
    Inc(FPos);

  if FPos > p0 then
  begin
    FuncPos := p0;
    Func := Copy(FExpr, p0, FPos - p0);
    Skip(' ');
    Args := [];
    ArgsPos := [];
    A := nil;

    try
      if StartWith('(') then
      begin
        Skip('(');
        if not StartWith(')') then
          while FPos <= FLen do
          begin
            p0 := FPos;
            A := InnerBuild(0);
            if A = nil then raise ECalcError.Create(rsArgExpected, [], p0);

            SetLength(Args, Length(Args) + 1);
            Args[High(Args)] := A;
            SetLength(ArgsPos, Length(ArgsPos) + 1);
            ArgsPos[High(ArgsPos)] := p0;

            if StartWith(')') then Break
            else if StartWith(',') then Skip(',')
            else raise ECalcError.Create(rsCommaExpected, [], FPos);
          end;
        if not StartWith(')') then raise ECalcError.Create(rsArgExpected, [], FPos);
        Skip(')');
      end;
      CheckFunc(Func, Args, FuncPos, ArgsPos, idx, eidx);
    except
      on E: Exception do
      begin
        for i := 0 to High(Args) do
          FreeAndNil(Args[i]);
        raise;
      end;
    end;
    Result := TEFunction.Create(FForm, FParentForm, FDataSet, Func, idx, eidx, Args);
    Exit;
  end;

  if FPos <= FLen then
    raise ECalcError.Create(rsUnexpectedToken, [GetChar], FPos);
  Result := nil;
end;

procedure TExpressionBuilder.CheckExprFunc(const Func: String;
  const Args: array of TExpression; FuncPos: Integer;
  const ArgsPos: array of Integer; var aIdx, aEIdx: Integer);
var
  i: Integer;
  F: TExprFunc;
  S: String;
  W: Char;
begin
  aEIdx := ScriptMan.Funcs.FindFuncIndex(Func);
  if aEIdx >= 0 then
  begin
    F := ScriptMan.Funcs[aEIdx];
    S := F.Args;
    if Length(S) <> Length(Args) then
    	raise ECalcError.Create(rsInvalidNumberArgsFunc, [Func, Length(Args),
        Length(S)], FuncPos);
    for i := 0 to High(Args) do
	  begin
  	  W := GetExprType(Args[i]);
      if (W <> 'v') and (S[i+1] <> 'v') and (W <> S[i+1]) then
      	raise ECalcError.Create(rsIncompatibleArgOfFunc,
          [i+1, Func, OpTypeToStr(W), OpTypeToStr(S[i+1])], ArgsPos[i]);
    end;
  end
  else
  	raise ECalcError.Create(rsFuncNotFound2, [Func], FuncPos);
end;

procedure TExpressionBuilder.CheckFunc(const Func: String;
  const Args: array of TExpression; FuncPos: Integer;
  const ArgsPos: array of Integer; var aIdx, aEIdx: Integer);
var
  i: Integer;
  S: String;
  W: Char;
begin
  aIdx := -1;
  aEIdx := -1;
  S := AnsiUpperCase(Func);
  // Встроенные
  i := FindFunc(S);
  if i >= 0 then
  begin
    aIdx := i;
    if (S <> 'MAXV') and (S <> 'MINV') and (S <> 'BLOCK') and (S <> 'CONCAT') then
    begin
      S := Funcs[i, 1];
      if Length(S) <> Length(Args) then
        raise ECalcError.Create(rsInvalidNumberArgsFunc, [Func, Length(Args), Length(S)], FuncPos);
      for i := 0 to High(Args) do
      begin
        W := GetExprType(Args[i]);
        if (W <> 'v') and (S[i+1] <> 'v') and (W <> S[i+1]) then
          raise ECalcError.Create(rsIncompatibleArgOfFunc,
            [i+1, Func, OpTypeToStr(W), OpTypeToStr(S[i+1])], ArgsPos[i]);
      end;
    end;
  end
  else
    // Модули выражений
    CheckExprFunc(S, Args, FuncPos, ArgsPos, aIdx, aEIdx);
end;

{function TExpressionBuilder.CheckTypes(O1, O2: TExpression): Boolean;
var
  t1, t2: String;
begin
  t1 := GetExprType(O1);
  t2 := GetExprType(O2);
  if (t1 <> 'v') and (t2 <> 'v') and (t1 <> t2) then Result := False
  else Result := True;
//    raise ECalcError.Create(ecIncompatibleTypes, FPos, '');
end;     }

procedure TExpressionBuilder.CheckQueryField(const FieldName: String; P: Integer
  );
var
  i: Integer;
begin
  i := FRD.IndexOfName(FieldName);
  if (i < 0) or not FRD.GetFieldVisible(i) then
    raise ECalcError.Create(rsFieldNotFound, [FieldName], P);


  {pF := nil;
  if FRD.Sources.Count > 0 then
    pF := FRD.Sources[0]^.Fields.FindFieldByName(FieldName);
  if (pF = nil) or ((pF <> nil) and (pF^.Visible = False)) then
  begin
    if FRD.CalcFields.FindFieldByName(FieldName) = nil then
      raise ECalcError.Create(rsFieldNotFound, [FieldName], P);
  end;   }
end;

function TExpressionBuilder.CheckField(const FieldName: String; P: Integer
  ): TComponent;
var
  SL: TStringList;
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
begin
  C := nil;
  SL := TStringList.Create;

  try

  SplitStr(FieldName, '|', SL);
  Fm := FForm;
  if SL[0][1] = '!' then
  begin
    SL[0] := Copy(SL[0], 2, 1024);
    if (Fm.PId > 0) and (FParentForm <> nil) then
      Fm := FParentForm;
  end
  // Чтобы отличить поля формы от полей запроса/отчета
  else if SL[0][1] = ':' then
    SL[0] := Copy(SL[0], 2, 1024);
  for i := 0 to SL.Count - 1 do
  begin
    C := FindComponentByFieldName(Fm, SL[i]);
    if C = nil then Break
    else if C is TdxLookupComboBox then
      with TdxLookupComboBox(C) do
      begin
        if (SourceTId > 0) and (SourceFId > 0) then
          Fm := FormMan.FindForm(SourceTId)
        else
          raise ECalcError.Create(rsObjectNotHaveLinkForm, [SL[i]], P);
      end
    else if i < SL.Count - 1 then
      raise ECalcError.Create(rsFieldNotObject, [SL[i]], P);
  end;

  if C is TdxObjectField then
  begin
    C := LookupObjectField(TdxObjectField(C), True);
    if C = nil then raise ECalcError.Create(rsObjectFieldNotFound, [
      FieldName], P);
  end;

  if (C = nil) and (SL.Count = 1) and (not FSkipLabels) then
    C := FindLabelByFieldName(Fm, SL[0], False);
  if C = nil then raise ECalcError.Create(rsFieldNotFound, [FieldName], P)

  finally
    SL.Free;
  end;
  Result := C;
end;

function RpFieldTypeToChar(Tp: TRpFieldType): Char;
begin
  case Tp of
    flText: Result := 's';
    flNumber, flBool, flCounter, flObject, flRecId: Result := 'n';
    flDate: Result := 'd';
    flTime: Result := 't';
    else Result := '?';
	end;
end;

{function RpFieldFuncToChar(Fn: TRpTotalFunc): Char;
begin
  case Fn of
    tfCount, tfDistCount: Result := 'n';
    tfMerge, tfMergeAll: Result := 's';
    else Result := ' ';
  end;
end;  }

function TExpressionBuilder.GetQueryFieldType(const FieldName: String): Char;
{var
  pF: PRpField;
  pCF: PRpCalcField; }
var
  i: Integer;
begin
  i := FRD.IndexOfName(FieldName);
  Result := RpFieldTypeToChar( FRD.GetFieldType(i) );
  {pF := FRD.FindFieldByName(FieldName);
  if pF <> nil then
  begin
    Result := RpFieldTypeToChar(GetRealRpFieldType(FRD, pF));
  end
  else
  begin
    pCF := FRD.CalcFields.FindFieldByName(FieldName);
    if pCF <> nil then
			Result := RpFieldTypeToChar(pCF^.Tp);
  end;  }
end;

function TExpressionBuilder.GetFieldType(C: TComponent): Char;
begin
  if (C is TdxEdit) or (C is TdxMemo) or (C is TdxFile) or (C is TdxComboBox) or
    (C is TdxFile) or (C is TdxDBImage) then Result := 's'
  else if (C is TdxCalcEdit) or (C is TdxCheckBox) or (C is TdxCounter) or
    (C is TdxLookupComboBox) or (C is TdxRecordId) then Result := 'n'
  else if C is TdxDateEdit then Result := 'd'
  else if C is TdxLabel then Result := 'v'
  else if C is TdxTimeEdit then Result := 't'
  else Result := '?'
end;

function TExpressionBuilder.GetExprType(A: TExpression): Char;
var
  a1, a2: Char;
begin
  if A is TENumber then Result := 'n'
  else if A is TEString then Result := 's'
  else if A is TEUnary then
  begin
    if TEUnary(A).FNot then Result := 'b'
    else Result := 'n';
  end
  else if A is TEBinary then
    with TEBinary(A) do
    begin
      if FOp = '+' then
      begin
        // Здесь гарантировано оба операнада (см. CheckOperands): n, s или v
        a1 := GetExprType(FE1);
        a2 := GetExprType(FE2);
        if a1 <> 'v' then Result := a1
        else Result := a2;
      end
      else if FOp[1] in ['*', '/', '-'] then Result := 'n'
      else Result := 'b';
    end
  else if A is TEFunction then
  begin
    Result := TEFunction(A).ResltType;
  end
  else if A is TEField then
    Result := TEField(A).FieldType
  else if A is TEQueryField then
    Result := GetQueryFieldType(TEQueryField(A).FFieldName)
  else Result := '?';
end;

{ ECalcError }

constructor ECalcError.Create(const MsgFmt: string; const args: array of const;
  P: Integer);
begin
  inherited CreateFmt(MsgFmt, args);
  FPos := P;
end;

{ TEBinary }

function TEBinary.Cast(O1, O2: Variant): Char;
begin
  if (O1 = Null) or (O2 = Null) then Exit('0')
  else if VarIsBool(O1) and VarIsBool(O2) then Exit('b')
  else if VarIsNumeric(O1) and VarIsNumeric(O2) then Exit('n')
  else if VarIsStr(O1) and VarIsStr(O2) then Exit('s')
  else if VarIsType(O1, varDate) and VarIsType(O2, varDate) then Exit('d')
  else
  begin
    raise ECalcError.Create(rsOpNotValidForDifOperands, [FOp, VarTypeToStr(O1),
      VarTypeToStr(O2)], E2.Position);
  end;
end;

function TEBinary.CalcNum(O1, O2: Variant): Variant;
begin
  if FOp = '*' then Result := O1 * O2
  else if FOp = '/' then begin CheckZero(O2); Result := O1 / O2; end
  else if FOp = '+' then Result := O1 + O2
  else if FOp = '-' then Result := O1 - O2
  else if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  else if FOp = '>' then Result := O1 > O2
  else if FOp = '>=' then Result := O1 >= O2
  else if FOp = '<' then Result := O1 < O2
  else if FOp = '<=' then Result := O1 <= O2
  // Никогда не должно выполнятся
  else raise ECalcError.Create(rsIllegalNumOp, [], E2.Position);
end;

function TEBinary.CalcStr(O1, O2: Variant): Variant;
begin
  if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  else if FOp = '>' then Result := O1 > O2
  else if FOp = '>=' then Result := O1 >= O2
  else if FOp = '<' then Result := O1 < O2
  else if FOp = '<=' then Result := O1 <= O2
  else if FOp = '+' then Result := VarToStr(O1) + VarToStr(O2)
  // Никогда не должно выполнятся
  else raise ECalcError.Create(rsIllegalStrOp, [], E2.Position);
end;

function TEBinary.CalcBool(O1, O2: Variant): Variant;
begin
  if FOp = '&' then Result := O1 and O2
  else if FOp = '|' then Result := O1 or O2
  else if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  // Никогда не должно выполнятся
  else raise ECalcError.Create(rsIllegalBoolOp, [], E2.Position);
end;

function TEBinary.CalcDate(O1, O2: Variant): Variant;
begin
  if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  else if FOp = '>' then Result := O1 > O2
  else if FOp = '>=' then Result := O1 >= O2
  else if FOp = '<' then Result := O1 < O2
  else if FOp = '<=' then Result := O1 <= O2
  // Никогда не должно выполнятся
  else raise ECalcError.Create(rsIllegalDateOp, [], E2.Position);
end;

function TEBinary.CalcNull(O1, O2: Variant): Variant;
begin
  if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  else Result := Null;
end;

procedure TEBinary.CheckZero(O2: Variant);
begin
  if O2 = 0 then raise ECalcError.Create(rsDivZero, [], E2.Position);
end;

constructor TEBinary.Create(E1, E2: TExpression; const Op: String);
begin
  FE1 := E1;
  FE2 := E2;
  FOp := Op;
end;

destructor TEBinary.Destroy;
begin
  FreeAndNil(FE1);
  FreeAndNil(FE2);
  inherited Destroy;
end;

function TEBinary.Calc: Variant;
var
  O1, O2: Variant;
  t: Char;
begin
  O1 := FE1.Calc;
  O2 := Null;

  // Реализуем неполное вычисление логических выражений
  if VarIsBool(O1) then
  begin
    if (FOp = '&') and (O1 = False) then O2 := False
    else if (FOp = '|') and (O1 = True) then O2 := True;
  end;
  if O2 = Null then
	  O2 := FE2.Calc;
  //

  t := Cast(O1, O2);

  case t of
    'n': Result := CalcNum(O1, O2);
    's': Result := CalcStr(O1, O2);
    'b': Result := CalcBool(O1, O2);
    'd': Result := CalcDate(O1, O2);
    '0': Result := CalcNull(O1, O2);
  end;
end;

{ TEUnary }

constructor TEUnary.Create(E: TExpression; const Op: String);
begin
  FExpr := E;
  FNot := Op = '!';
end;

destructor TEUnary.Destroy;
begin
  FExpr.Free;
  inherited Destroy;
end;

function TEUnary.Calc: Variant;
begin
  Result:=FExpr.Calc;
  if Result = Null then Exit;
  if FNot then
  begin
    if VarIsBool(Result) then Result := not Result
    else raise ECalcError.Create(rsInvalidOpForTypeExpected, ['!', VarTypeToStr(Result),
      rsBoolean], FPos);
  end
  else
  begin
    if VarIsNumeric(Result) then Result := -Result
    else raise ECalcError.Create(rsInvalidOpForTypeExpected, ['-', VarTypeToStr(Result),
      rsNumber], FPos);
  end;
end;

{ TEField }

constructor TEField.Create(const aFieldName: String; aForm, aParForm: TdxForm;
  aDS: TDataSet; aSkipLabels: Boolean);
begin
  FFieldName := aFieldName;
  FForm := aForm;
  FParForm := aParForm;
  FDS := aDS;
  FSkipLabels := aSkipLabels;
end;

function TEField.Calc: Variant;
begin
  Result := LookupFieldValue(FForm, FParForm, FDS, FFieldName, FSkipLabels);
end;

{function LabelFieldExists(const FieldName, E: String): Boolean;
var
  S, SS: String;
  L, i, p: Integer;
  IsField: Boolean;
begin
  Result := False;
  S := Utf8LowerCase(FieldName);
  L := Length(E);
  IsField := False;
  for i := 1 to L do
  begin
    if E[i] = '[' then
    begin
      IsField := True;
      p := i + 1;
    end
    else if IsField and (p = i) and (E[i] in ['!', ':']) then
      p := i + 1
    else if IsField and ((E[i] = ']') or (i = L)) then
    begin
      SS := Utf8LowerCase(Copy(E, p, i - p));
      if S = SS then Exit(True);
      IsField := False;
    end;
  end;
end;

function TEField.Calc: Variant;
var
  SL: TStringList;
  Fm: TdxForm;
  i: Integer;
  C: TComponent;
  DS, MainDS: TDataSet;
  V: Variant;
  Lbl: TdxLabel;
  Tmp: String;
begin
  Result := Null;
  SL := TStringList.Create;
  SplitStr(FFieldName, '|', SL);
  Fm := FForm;
  DS := FDS;
  if SL[0][1] = '!' then
  begin
    SL[0] := Copy(SL[0], 2, 1024);
    if (Fm.PId > 0) and (FParForm <> nil) then
    begin
      Fm := FParForm;
      DS := Fm.Grid.DataSource.DataSet;
    end
  end
  // Префикс для указания, что это поле текущей формы. Введено, чтобы различать
  // поля форм и поля запроса/отчета.
  else if SL[0][1] = ':' then
    SL[0] := Copy(SL[0], 2, 1024);
  MainDS := DS;

  try

  for i := 0 to SL.Count - 1 do
  begin
    C := FindComponentByFieldName(Fm, SL[i]);
    if C = nil then Break;
    if C is TdxLookupComboBox then
    begin
      V := DS.FieldByName(FieldStr(C)).Value;
      Result := V;
      if V = Null then Break;
      Fm := FormMan.FindForm(GetSourceTId(C));
      if Fm = nil then Exit;
      if DS <> MainDS then DS.Free;
      Tmp := SqlSelectGroups(Fm.Id, GetSourceFId(C), True);
      if Tmp <> '' then Tmp := '(' + Tmp + ')'
      else Tmp := TableStr(Fm.Id);
      DS := DBase.OpenDataSet('select * from ' + Tmp + ' where id='
        + VarToStr(V));
    end
    else
    begin
      Result := DS.FieldByName(FieldStr(C)).Value;
      Break;
    end;
  end;

  Lbl := nil;
  if (C = nil) and (SL.Count = 1) and (not FSkipLabels) then
  begin
    Lbl := FindLabelByFieldName(FForm, SL[0], False);
    if (Lbl <> nil) and (not LabelFieldExists(FFieldName, Lbl.Expression)) then
    begin
      if Lbl.Value = unassigned then
        CalcLabelExpr(Lbl, FDS, FParForm);
      Result := Lbl.Value;
    end;
  end;

  finally
    if DS <> MainDS then DS.Free;
    SL.Free;
  end;
end;    }

{ TEString }

constructor TEString.Create(const aValue: String);
begin
  FValue := aValue;
end;

function TEString.Calc: Variant;
begin
  Result:=FValue;
end;

{ TENumber }

constructor TENumber.Create(aValue: Extended);
begin
  FValue := aValue;
end;

function TENumber.Calc: Variant;
begin
  Result:=FValue;
end;

{ TExpression }

function TExpression.Calc: Variant;
begin
  Result := unassigned;
end;

end.

