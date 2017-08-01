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
unit Expressions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Db, dxctrls, strconsts, Dialogs, dxreports, uPSRuntime;

type

  TErrCodes = (ecIncompatibleTypes, ecIllegalStrOp, ecIllegalNumOp,
    ecIllegalBoolOp, ecDivZero, ecInvalidNumber, ecArgExpected, ecEndLineExpected,
    ecSqBrExpected, ecBrExpected, ecOpExpected, ecOperandExpected,
    ecFuncNotFound, ecIncorrectArgCount, ecIncompatibleArg, ecCommaExpected,
    ecFieldNameEmpty, ecFieldNotFound, ecUnknownFieldSource, ecNotObjField,
    ecIllegalDateOp, ecExprExpect, ecUnknownToken);

  { ECalcError }

  ECalcError = class(Exception)
  private
    FErrCode: TErrCodes;
    FExtra: String;
    FPos: Integer;
  public
    constructor Create(aErrCode: TErrCodes; P: Integer; const aExtra: String);
    property ErrCode: TErrCodes read FErrCode;
    property Position: Integer read FPos;
    property Extra: String read FExtra;
  end;

  { TExpression }

  TExpression = class
  public
    function Calc: Variant; virtual;
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
    FForm, FParForm: TdxForm;
    FDS: TDataSet;
    FSkipLabels: Boolean;
  public
    constructor Create(const aFieldName: String; aForm, aParForm: TdxForm; aDS: TDataSet;
      aSkipLabels: Boolean);
    function Calc: Variant; override;
    property FieldName: String read FFieldName;
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
    constructor Create(aForm, aParForm: TdxForm; aDS: TDataSet; const aFN: String;
      aIdx, aExprIdx: Integer; Args: TExprArr);
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
    FStartPos, FPos, FLen: Integer;
    FRD: TReportData;
    FRDSet: TDataSet;
    FSkipLabels: Boolean;
    function InnerBuild(aState: Integer): TExpression;
    function LastState(aState: Integer): Boolean;
    function StartWith(const S: String): Boolean;
    procedure Skip(const S: String);
    procedure SkipLineComment;
    procedure SkipMultiLineComment;
    function ReadStateOperator(aState: Integer): String;
    function ReadSingle: TExpression;
    procedure CheckExprFunc(const Func: String; Args: array of TExpression; var aIdx, aEIdx: Integer);
    procedure CheckFunc(const Func: String; Args: array of TExpression; var aIdx, aEIdx: Integer);
    procedure CheckTypes(O1, O2: TExpression);
    procedure CheckQueryField(const FieldName: String);
    function CheckField(const FieldName: String): TComponent;
    function GetQueryFieldType(const FieldName: String): Char;
    function GetFieldType(const FieldName: String): Char;
    function GetExprType(A: TExpression): Char;
  public
    function Build(const aExpr: String): TExpression;
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
  Variants, StrUtils, formmanager, apputils, ExprFuncs,
  LazUtf8, Math, towordsfuncs, DateUtils, dxfiles, scriptmanager, padeg;

const
  States: array [0..6, 0..5] of String =
    (('|', '', '', '', '', ''), ('&', '', '', '', '', ''),
    ('!', '', '', '', '', ''), ('=', '<>', '<=', '<', '>=', '>'),
    ('+', '-', '', '', '', ''), ('*', '/', '', '', '', ''),
    ('', '', '', '', '', ''));

  Funcs: array [0..127, 0..2] of String =
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
    ('SUMPERIOD', 'sssn', 's'),
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
    ('PADEG_O', 's', 's')

    );

function ErrorCodeToString(Err: TErrCodes): String;
var
  S: String;
begin
  S := '';
  case Err of
    ecIncompatibleTypes: S := rsIncompatibleTypes;
    ecIllegalStrOp: S := rsIllegalStrOp;
    ecIllegalNumOp: S := rsIllegalNumOp;
    ecIllegalBoolOp: S := rsIllegalBoolOp;
    ecDivZero: S := rsDivZero;
    ecInvalidNumber: S := rsInvalidNumber;
    ecArgExpected: S := rsArgExpected;
    ecEndLineExpected: S := rsEndLineExpected;
    ecSqBrExpected: S := rsSqBrExpected;
    ecBrExpected: S := rsBrExpected;
    ecOpExpected: S := rsOpExpected;
    ecOperandExpected: S := rsOperandExpected;
    ecFuncNotFound: S := rsFuncNotFound;
    ecIncorrectArgCount: S := rsIncorrectArgCount;
    ecIncompatibleArg: S := rsIncompatibleArg;
    ecCommaExpected: S := rsCommaExpected;
    ecFieldNameEmpty: S := rsFieldNameEmpty;
    ecFieldNotFound: S := rsErrFieldNotFound;
    ecUnknownFieldSource: S := rsUnknownFieldSource;
    ecNotObjField: S := rsNotObjField;
    ecIllegalDateOp: S := rsIllegalDateOp;
    ecExprExpect: S := rsFPExprExpect;
    ecUnknownToken: S := rsUnknownToken;
  end;
  Result := S;
end;

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
    F := FDS.FindField(Col.FieldName);
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
begin
  pV := ExprModule.Exec.GetVar2('Self');
  if pV = nil then raise ECalcError.CreateFmt(rsExprModuleFailedMsg, []);

	OldSelf := PSGetObject(@PPSVariantData(pV)^.Data, pV^.FType);
  SetVariantToClass(pV, FForm);

  F := ScriptMan.Funcs[FExprIdx];
  try
	  ok := ExprModule.TryRunFunc(F.OrigName, Vals, Result);
  finally
	  SetVariantToClass(pV, OldSelf);
  end;
  if not ok then
  	raise ECalcError.CreateFmt(rsFuncNotFound2, [F.Name]);
end;

function TEFunction.CalcFunc(Vals: array of Variant): Variant;
var
  FN: String;
  V: Variant;
begin
  Result := Null;
  if FExprIdx >= 0 then
  begin
    Result := CalcExprFunc(Vals);
    Exit;
  end;

  FN := Funcs[FIdx, 0];
  if CheckNull(Vals) and (FN <> 'IIF') and (FN <> 'CSTR') and (FN <> 'NZ')
    and (FN <> 'SETVAR') and (FN <> 'SETFIELD') and (FN <> 'SETLABEL') then Exit;

  if FN = 'COUNT' then V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], '', '', tfCount)
  else if FN = 'SUM' then V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], '', tfSum)
  else if FN = 'LENGTH' then V := Utf8Length(Vals[0])
  else if FN = 'CUT' then V := Utf8Copy(Vals[0], Vals[1], Vals[2])
  else if FN = 'FIND' then V := Utf8Pos(Vals[0], Vals[1], Vals[2])
  else if FN = 'REPLACE' then V := Utf8StringReplace(Vals[0], Vals[1], Vals[2], [])
  else if FN = 'REPLACEALL' then V := Utf8StringReplace(Vals[0], Vals[1], Vals[2], [rfReplaceAll])
  else if FN = 'UPPER' then V := Utf8UpperCase(Vals[0])
  else if FN = 'LOWER' then V := Utf8LowerCase(Vals[0])
  else if FN = 'TOWORDS' then V := ToWords(Vals[0], False)
  else if FN = 'RURTOWORDS' then V := ToWords(Vals[0], True)
  else if FN = 'FUPPER' then V := FirstLetterUpper(Vals[0])
  else if FN = 'ROUND' then V := MathRound(Extended(Vals[0]), Vals[1])
  else if FN = 'TRUNC' then V := Trunc(Vals[0])
  else if FN = 'FRAC' then V := MyFrac(Vals[0], Vals[1])
  else if FN = 'POWER' then V := MyPower(Vals[0], Vals[1])
  else if FN = 'DATE' then V := Date
  else if FN = 'TIME' then V := Time
  else if FN = 'YEARSBETWEEN' then V := YearsBetween(Vals[0], Vals[1])
  else if FN = 'MONTHSBETWEEN' then V := MonthsBetween(Vals[0], Vals[1])
  else if FN = 'DAYSBETWEEN' then V := DaysBetween(Vals[0], Vals[1])
  else if FN = 'DAYOF' then V := DayOf(Vals[0])
  else if FN = 'MONTHOF' then V := MonthOf(Vals[0])
  else if FN = 'YEAROF' then V := YearOf(Vals[0])
  else if FN = 'WEEKOF' then V := WeekOf(Vals[0])
  else if FN = 'WEEKDAY' then V := GetWeekName(Vals[0], False)
  else if FN = 'WEEKDAYBRIEF' then V := GetWeekName(Vals[0], True)
  else if FN = 'MONTH' then V := GetMonthName(Vals[0], False)
  else if FN = 'MONTHBRIEF' then V := GetMonthName(Vals[0], True)
  else if FN = 'NULL' then V := Null
  else if FN = 'IIF' then V := IIF(Vals[0], Vals[1], Vals[2])
  else if FN = 'DAYOFWEEK' then V := DayOfTheWeek(Vals[0])
  else if FN = 'RECNO' then V := GetRecNo(FParForm, Vals[0])
  else if FN = 'SUMIF' then V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfSum)
  else if FN = 'COUNTIF' then V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], '', Vals[1], tfCount)
  else if FN = 'ROUNDTO' then V := MyRoundToStr(Vals[0], Vals[1])
  else if FN = 'MAX' then V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], '', tfMax)
  else if FN = 'MAXIF' then V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfMax)
  else if FN = 'MIN' then V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], '', tfMin)
  else if FN = 'MINIF' then V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfMin)
  else if FN = 'AVG' then V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], '', tfAvg)
  else if FN = 'AVGIF' then V := CalcAggFunc(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfAvg)
  else if FN = 'CSTR' then V := VarToStr(Vals[0])
  else if FN = 'CNUM' then V := StrToFloat(Vals[0])
  else if FN = 'CDATE' then V := StrToDate(Vals[0])
  else if FN = 'ADDDAY' then V := IncDay(Vals[0], Vals[1])
  else if FN = 'ADDWEEK' then V := IncWeek(Vals[0], Vals[1])
  else if FN = 'ADDMONTH' then V := IncMonth(Vals[0], Vals[1])
  else if FN = 'ADDYEAR' then V := IncYear(Vals[0], Vals[1])
  else if FN = 'CTIME' then V := StrToTime(Vals[0])
  else if FN = 'HOUROF' then V := HourOf(Vals[0])
  else if FN = 'MINUTEOF' then V := MinuteOf(Vals[0])
  else if FN = 'SECONDOF' then V := SecondOf(Vals[0])
  else if FN = 'HOURSBETWEEN' then V := MyHoursBetween(Vals[0], Vals[1], Vals[2], Vals[3])
  else if FN = 'MINUTESBETWEEN' then V := MyMinutesBetween(Vals[0], Vals[1], Vals[2], Vals[3])
  else if FN = 'SECONDSBETWEEN' then V := MySecondsBetween(Vals[0], Vals[1], Vals[2], Vals[3])
  else if FN = 'ADDHOUR' then V := AddHour(FForm, FDS, Vals[0], Vals[1], Vals[2], Vals[3], 'h')
  else if FN = 'ADDMINUTE' then V := AddHour(FForm, FDS, Vals[0], Vals[1], Vals[2], Vals[3], 'm')
  else if FN = 'ADDSECOND' then V := AddHour(FForm, FDS, Vals[0], Vals[1], Vals[2], Vals[3], 's')
  else if FN = 'PERIOD' then V := CalcPeriod(Vals[0], Vals[1], Vals[2], False)
  else if FN = 'INDEXOF' then V := MyIndexOf(Vals[0], Vals[1])
  else if FN = 'NZ' then
    V := IIF(Vals[0] = Null, Vals[1], Vals[0])
  else if FN = 'FDATE' then V := FmtDate(Vals[0])
  else if FN = 'MERGE' then V := MergeRows(FForm, Vals[0], Vals[1], Vals[2])
  else if FN = 'RECID' then V := GetRecId(FForm, FParForm, Vals[0])
  else if FN = 'OBJID' then V := GetObjId(Vals[0], Vals[1], Vals[2])
  else if FN = 'USER' then V := GetUser
  else if FN = 'ROLE' then V := GetRole
  else if FN = 'DBGET' then V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfNone)
  else if FN = 'DBSUM' then V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfSum)
  else if FN = 'DBAVG' then V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfAvg)
  else if FN = 'DBMAX' then V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfMax)
  else if FN = 'DBMIN' then V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfMin)
  else if FN = 'DBCOUNT' then V := DBGet(FForm, FParForm, FDS, Vals[0], '', Vals[1], tfCount)
  else if FN = 'DBGETBYID' then V := DBGetById(Vals[0], Vals[1], Vals[2])
  else if FN = 'MAXV' then V := GetMaxV(Vals)
  else if FN = 'MINV' then V := GetMinV(Vals)
  else if FN = 'DBGETID' then V := DBGet(FForm, FParForm, FDS, Vals[0], '', Vals[1], tfNone)
  else if FN = 'SETVAR' then V := SetVar(Vals[0], Vals[1])
  else if FN = 'GETVAR' then V := GetVar(Vals[0])
  else if FN = 'IFZ' then V := IIF(Vals[0] = 0, Vals[1], Vals[0])
  else if FN = 'IFE' then V := IIF(Vals[0] = '', Vals[1], Vals[0])
  else if FN = 'TRIM' then V := Trim(Vals[0])
  else if FN = 'ZEROS' then V := SetZeros(Vals[0], Vals[1])
  else if FN = 'AGE' then V := CalcPeriod(Vals[0], Vals[1], Vals[2], True)
  else if FN = 'SUMPERIOD' then V := SumPeriod(FForm, Vals[0], Vals[1], Vals[2], Vals[3])
  else if FN = 'BLOCK' then V := Block(Vals)
  else if FN = 'VAREXISTS' then V := VarExists(Vals[0])
  else if FN = 'PATHLEN' then V := PathLength(Vals[0])
  else if FN = 'EXTRACTPATH' then V := ExtractPath(Vals[0], Vals[1], Vals[2])
  else if FN = 'NEWLINE' then V := LineEnding
  else if FN = 'NEWREC' then V := IsNewRec(FDS)
  else if FN = 'GET' then V := GetFieldValue(FForm, Vals[0], Vals[1])
  else if FN = 'DBCOUNTD' then V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfDistCount)
  else if FN = 'OLDVALUE' then V := GetOldValue(FForm, FDS, Vals[0])
  else if FN = 'DBMERGE' then V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfMerge)
  else if FN = 'DBMERGEALL' then V := DBGet(FForm, FParForm, FDS, Vals[0], Vals[1], Vals[2], tfMergeAll)
  else if FN = 'BEGINYEAR' then V := BeginYear(Vals[0])
  else if FN = 'BEGINMONTH' then V := BeginMonth(Vals[0])
  else if FN = 'BEGINWEEK' then V := BeginWeek(Vals[0])
  else if FN = 'ENDYEAR' then V := EndYear(Vals[0])
  else if FN = 'ENDMONTH' then V := EndMonth(Vals[0])
  else if FN = 'ENDWEEK' then V := EndWeek(Vals[0])
  else if FN = 'BEGINQUARTER' then V := BeginQuarter(Vals[0])
  else if FN = 'ENDQUARTER' then V := EndQuarter(Vals[0])
  else if FN = 'QUARTEROF' then V := QuarterOf(Vals[0])
  else if FN = 'DBUNIQUE' then V := DBUnique(FForm, FDS, Vals[0])
  else if FN = 'UNIQUE' then V := IsUniqueRecords(FForm, Vals[0], Vals[1])
  else if FN = 'MSGBOX' then V := MsgBox(Vals[0], Vals[1])
  else if FN = 'YESNOBOX' then V := YesNoBox(Vals[0], Vals[1])
  else if FN = 'YESNOCANCELBOX' then V := YesNoCancelBox(Vals[0], Vals[1])
  else if FN = 'SETFIELD' then V := SetField(FForm, FDS, Vals[0], Vals[1])
  else if FN = 'SETLABEL' then V := SetLabel(FForm, Vals[0], Vals[1])
  else if FN = 'EDITREC' then V := IsEditRec(FDS)
  else if FN = 'MODIFIEDREC' then V := IsModifiedRec(FDS)

  // Падеж
  else if FN = 'PADEG_FIO' then V := GetFIOPadeg(Vals[0], Vals[1], Vals[2])
  else if FN = 'PADEG_IF' then V := GetIFPadeg(Vals[0], Vals[1], Vals[2])
  else if FN = 'PADEG_NOMINATIVE' then V := GetNominativePadeg(Vals[0])
  else if FN = 'PADEG_APPOINTMENT' then V := GetAppointmentPadeg(Vals[0], Vals[1])
  else if FN = 'PADEG_FULLAPPOINTMENT' then V := GetFullAppointmentPadeg(Vals[0], Vals[1], Vals[2])
  else if FN = 'PADEG_OFFICE' then V := GetOfficePadeg(Vals[0], Vals[1])
  else if FN = 'PADEG_SEX' then V := GetSex(Vals[0])
  else if FN = 'PADEG_ID' then V := GetPadegID(Vals[0])
  else if FN = 'PADEG_FIOBRIEF' then V := GetFIOBriefPadeg(Vals[0], Vals[1], Vals[2])
  else if FN = 'PADEG_IOFBRIEF' then V := GetIOFBriefPadeg(Vals[0], Vals[1], Vals[2])
  else if FN = 'PADEG_F' then V := GetF(Vals[0])
  else if FN = 'PADEG_I' then V := GetI(Vals[0])
  else if FN = 'PADEG_O' then V := GetO(Vals[0])
  ;
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
  i: Integer;
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
    on E: EGetQueryDataError do
    	;
    on E: Exception do
     raise Exception.CreateFmt('%s -> %s', [FFuncName, E.Message]);
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
  {if Result = nil then raise ECalcError.Create(ecExprExpect, 0, '')
  else }if (Result <> nil) and (FPos <= FLen) then raise ECalcError.Create(ecOpExpected, FPos, '');
end;

function TExpressionBuilder.InnerBuild(aState: Integer): TExpression;
var
  IsMinus, UnarNot: Boolean;
  a1, a2: TExpression;
  Op: String;
begin
  if LastState(aState) then
  begin
    Result := nil;
    IsMinus := StartWith('-');
    if IsMinus then Skip('-');
    if StartWith('(') then
    begin
      Skip('(');
      Result := InnerBuild(0);
      if not StartWith(')') then raise ECalcError.Create(ecBrExpected, FPos, '');
      Skip(')');
    end
    else Result := ReadSingle;
    if IsMinus then
    begin
      if not (GetExprType(Result) in ['n', 'v']) then
        raise ECalcError.Create(ecIncompatibleTypes, FPos, '');
      Result := TEUnary.Create(Result, '-');
    end;
    Exit;
  end;

  UnarNot := (aState = 2) and StartWith('!');
  if UnarNot then Skip('!');

  // Первый операнд
  a1 := InnerBuild(aState + 1);
  if UnarNot then
  begin
    if not (GetExprType(a1) in ['b', 'v']) then
      raise ECalcError.Create(ecIncompatibleTypes, FPos, '');
    a1 := TEUnary.Create(a1, '!');
  end;

  // последуюущие операнды
  while True do
  begin
    Op := ReadStateOperator(aState);
    if Op='' then Break;
    a2 := InnerBuild(aState + 1);
    if (a1 = nil) or (a2 = nil) then
    	raise ECalcError.Create(ecOperandExpected, FPos, '');
    CheckTypes(a1, a2);
    a1 := TEBinary.Create(a1, a2, Op);
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
  p0, idx, eidx: Integer;
  q: Char;
  E: Extended;
  Args: array of TExpression;
  A: TExpression;
  Func, S: String;
  FS: TFormatSettings;
begin
  p0 := FPos;

  // строка
  if StartWith('''') or StartWith('"') then
  begin
    q := '''';
    if StartWith('"') then q := '"';
    FPos := PosEx(q, FExpr, FPos + 1);
    if FPos = 0 then raise ECalcError.Create(ecEndLineExpected, p0, '');
    Result := TEString.Create(Copy(FExpr, p0 + 1, FPos - p0 - 1));
    Skip(q);
    Exit;
  end;

  // поле
  if StartWith('[') then
  begin
    FPos := PosEx(']', FExpr, FPos + 1);
    if FPos = 0 then raise ECalcError.Create(ecSqBrExpected, p0, '');
    S := Copy(FExpr, p0 + 1, FPos - p0 - 1);
    if S = '' then raise ECalcError.Create(ecFieldNameEmpty, FPos, '');
    // Это может быть поле и формы, и запроса/отчета
    if (FRD <> nil) and (S[1] <> '!') and (S[1] <> ':') then
    begin
      CheckQueryField(S);
      Result := TEQueryField.Create(S, FRD, FRDSet);
    end
    else if FForm <> nil then
    begin
      CheckField(S);
      Result := TEField.Create(S, FForm, FParentForm, FDataSet, FSkipLabels);
    end
    else
      raise ECalcError.Create(ecFieldNotFound, p0, S);
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
      raise ECalcError.Create(ecInvalidNumber, FPos, S);
    Skip(' ');
    Result := TENumber.Create(E);
    Exit;
  end;

  // функция
  while (FPos <= FLen) and (FExpr[FPos] in ['A'..'Z', 'a'..'z', '_', '0'..'9']) do
    Inc(FPos);

  if FPos > p0 then
  begin
    Func := Copy(FExpr, p0, FPos - p0);
    Skip(' ');
    SetLength(Args, 0);
    A := nil;
    if StartWith('(') then
    begin
      Skip('(');
      if not StartWith(')') then
        while FPos <= FLen do
        begin
          A := InnerBuild(0);
          if A = nil then raise ECalcError.Create(ecArgExpected, FPos, '');
          if StartWith(')') then Break
          else if StartWith(',') then
          begin
            SetLength(Args, Length(Args) + 1);
            Args[High(Args)] := A;
            Skip(',');
          end
          else raise ECalcError.Create(ecCommaExpected, FPos, '');
        end;
      if A <> nil then
      begin
        SetLength(Args, Length(Args) + 1);
        Args[High(Args)] := A;
      end;
      if not StartWith(')') then raise ECalcError.Create(ecBrExpected, FPos, '');
      Skip(')');
    end;
    CheckFunc(Func, Args, idx, eidx);
    Result := TEFunction.Create(FForm, FParentForm, FDataSet, Func, idx, eidx, Args);
    Exit;
  end;

  if FPos <= FLen then
    raise ECalcError.Create(ecUnknownToken, FPos, '');
  Result := nil;
end;

procedure TExpressionBuilder.CheckExprFunc(const Func: String;
  Args: array of TExpression; var aIdx, aEIdx: Integer);
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
    	raise ECalcError.Create(ecIncorrectArgCount, FPos, Func);
    for i := 0 to High(Args) do
	  begin
  	  W := GetExprType(Args[i]);
      if (W <> 'v') and (S[i+1] <> 'v') and (W <> S[i+1]) then
      	raise ECalcError.Create(ecIncompatibleArg, FPos, Func);
    end;
  end
  else
  	raise ECalcError.Create(ecFuncNotFound, FPos, Func);
end;

procedure TExpressionBuilder.CheckFunc(const Func: String;
  Args: array of TExpression; var aIdx, aEIdx: Integer);
var
  i: Integer;
  S, W: String;
begin
  aIdx := -1;
  aEIdx := -1;
  S := AnsiUpperCase(Func);
  // Встроенные
  i := FindFunc(S);
  if i >= 0 then
  begin
    aIdx := i;
    if (S <> 'MAXV') and (S <> 'MINV') and (S <> 'BLOCK') then
    begin
      S := Funcs[i, 1];
      if Length(S) <> Length(Args) then
        raise ECalcError.Create(ecIncorrectArgCount, FPos, Func);
      for i := 0 to High(Args) do
      begin
        W := GetExprType(Args[i]);
        if (W <> 'v') and (S[i+1] <> 'v') and (W <> S[i+1]) then
          raise ECalcError.Create(ecIncompatibleArg, FPos, Func);
      end;
    end;
  end
  else
    // Модули выражений
    CheckExprFunc(S, Args, aIdx, aEIdx);
end;

procedure TExpressionBuilder.CheckTypes(O1, O2: TExpression);
var
  t1, t2: String;
begin
  t1 := GetExprType(O1);
  t2 := GetExprType(O2);
  if (t1 <> 'v') and (t2 <> 'v') and (t1 <> t2) then
    raise ECalcError.Create(ecIncompatibleTypes, FPos, '');
end;

procedure TExpressionBuilder.CheckQueryField(const FieldName: String);
var
  pF: PRpField;
begin
  pF := nil;
  if FRD.Sources.Count > 0 then
    pF := FRD.Sources[0]^.Fields.FindFieldByName(FieldName);
  if (pF = nil) or ((pF <> nil) and (pF^.Visible = False)) then
  begin
    if FRD.CalcFields.FindFieldByName(FieldName) = nil then
      raise ECalcError.Create(ecFieldNotFound, FPos, FieldName);
  end;
end;

function TExpressionBuilder.CheckField(const FieldName: String): TComponent;
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
          raise ECalcError.Create(ecUnknownFieldSource, FPos, FieldName);
      end
    else if i < SL.Count - 1 then
      raise ECalcError.Create(ecNotObjField, FPos, FieldName);
  end;

  if (C = nil) and (SL.Count = 1) and (not FSkipLabels) then
    C := FindLabelByFieldName(FForm, SL[0], False);
  if C = nil then raise ECalcError.Create(ecFieldNotFound, FPos, FieldName)

  finally
    SL.Free;
  end;
  Result := C;
end;

function RpFieldTypeToChar(Tp: TRpFieldType): Char;
begin
  case Tp of
    flText: Result := 's';
    flNumber, flBool, flCounter, flObject: Result := 'n';
    flDate: Result := 'd';
    flTime: Result := 't';
    else Result := '?';
	end;
end;

function TExpressionBuilder.GetQueryFieldType(const FieldName: String): Char;
var
  pF: PRpField;
  pCF: PRpCalcField;
begin
  pF := FRD.FindFieldByName(FieldName);
  if pF <> nil then
  	Result := RpFieldTypeToChar(GetLowField(pF)^.Tp)
	else
  begin
    pCF := FRD.CalcFields.FindFieldByName(FieldName);
    if pCF <> nil then
			Result := RpFieldTypeToChar(pCF^.Tp);
  end;
end;

function TExpressionBuilder.GetFieldType(const FieldName: String): Char;
var
  C: TComponent;
begin
  C := CheckField(FieldName);
  if C is TdxObjectField then C := LookupObjectField(TdxObjectField(C), True);
  if C = nil then raise Exception.Create(Format(rsObjectFieldNotFound, [
    FieldName]));

  if (C is TdxEdit) or (C is TdxMemo) or (C is TdxFile) or (C is TdxComboBox) then Result := 's'
  else if (C is TdxCalcEdit) or (C is TdxCheckBox) or (C is TdxCounter) or
    (C is TdxLookupComboBox) then Result := 'n'
  else if C is TdxDateEdit then Result := 'd'
  else if C is TdxLabel then Result := 'v'
  else if C is TdxTimeEdit then Result := 't'
  else Result := '?'
end;

function TExpressionBuilder.GetExprType(A: TExpression): Char;
var
  a1, a2: String;
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
        a1 := GetExprType(FE1);
        a2 := GetExprType(FE2);
        if (a1 = 'n') or (a2 = 'n') then Result := 'n'
        else if (a1 = 's') or (a2 = 's') then Result := 's'
        else if (a1 = 'b') or (a2 = 'b') then Result := 'b'       // заведомо неверно
        else Result := 'v';
      end
      else if FOp[1] in ['*', '/', '-'] then Result := 'n'
      else Result := 'b';
    end
  else if A is TEFunction then
  begin
    Result := TEFunction(A).ResltType;
    {n := FindFunc(TEFunction(A).FFuncName);
    Result := Funcs[n, 2][1];}
  end
  else if A is TEField then
    Result := GetFieldType(TEField(A).FFieldName)
  else if A is TEQueryField then
    Result := GetQueryFieldType(TEQueryField(A).FFieldName)
  else Result := '?';
end;

{ ECalcError }

constructor ECalcError.Create(aErrCode: TErrCodes; P: Integer;
  const aExtra: String);
var
  S: String;
begin
  S := ErrorCodeToString(aErrCode);
  if aExtra <> '' then S := S + ': ' + aExtra;
  inherited Create(S);
  FErrCode := aErrCode;
  FPos := P;
  FExtra := aExtra;
end;

{ TEBinary }

function TEBinary.Cast(O1, O2: Variant): Char;
begin
  if (O1 = Null) or (O2 = Null) then Exit('0')
  else if VarIsBool(O1) and VarIsBool(O2) then Exit('b')
  else if VarIsNumeric(O1) and VarIsNumeric(O2) then Exit('n')
  else if VarIsStr(O1) and VarIsStr(O2) then Exit('s')
  else if VarIsType(O1, varDate) and VarIsType(O2, varDate) then Exit('d')
  else raise ECalcError.Create(ecIncompatibleTypes, 0, '');
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
  else raise ECalcError.Create(ecIllegalNumOp, 0, '');
end;

function TEBinary.CalcStr(O1, O2: Variant): Variant;
begin
  if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  else if FOp = '>' then Result := O1 > O2
  else if FOp = '>=' then Result := O1 >= O2
  else if FOp = '<' then Result := O1 < O2
  else if FOp = '<=' then Result := O1 <= O2
  else if FOp = '+' then Result := O1 + O2
  else raise ECalcError.Create(ecIllegalStrOp, 0, '');
end;

function TEBinary.CalcBool(O1, O2: Variant): Variant;
begin
  if FOp = '&' then Result := O1 and O2
  else if FOp = '|' then Result := O1 or O2
  else if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  else raise ECalcError.Create(ecIllegalBoolOp, 0, '');
end;

function TEBinary.CalcDate(O1, O2: Variant): Variant;
begin
  if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  else if FOp = '>' then Result := O1 > O2
  else if FOp = '>=' then Result := O1 >= O2
  else if FOp = '<' then Result := O1 < O2
  else if FOp = '<=' then Result := O1 <= O2
  else raise ECalcError.Create(ecIllegalDateOp, 0, '');
end;

function TEBinary.CalcNull(O1, O2: Variant): Variant;
begin
  if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  else Result := Null;
end;

procedure TEBinary.CheckZero(O2: Variant);
begin
  if O2 = 0 then raise ECalcError.Create(ecDivZero, 0, '');
end;

constructor TEBinary.Create(E1, E2: TExpression; const Op: String);
begin
  FE1 := E1;
  FE2 := E2;
  FOp := Op;
  //if (E1 = nil) or (E2 = nil) then raise ECalcError.Create(ecOperandExpected);
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
  O2 := FE2.Calc;
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
  if FNot then
    Result := not Result
  else
    Result := -Result;
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

