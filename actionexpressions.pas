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

unit ActionExpressions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Controls, StdCtrls, dxctrls, dxreports,
  actionpans, strconsts, ScriptManager;

type
  { EACalcError }

  EACalcError = class(Exception)
  private
    FPos: Integer;
  public
    constructor Create(const MsgFmt: string; const args: array of const; P: Integer);
    property Position: Integer read FPos;
  end;

  { TAExpression }

  TAExpression = class
  private
    FPos: Integer;
  public
    function Calc: Variant; virtual;
    property Position: Integer read FPos write FPos;
  end;

  { TEANumber }

  TEANumber = class(TAExpression)
  private
    FValue: Extended;
  public
    constructor Create(aValue: Extended);
    function Calc: Variant; override;
  end;

  { TEAString }

  TEAString = class(TAExpression)
  private
    FValue: String;
  public
    constructor Create(const aValue: String);
    function Calc: Variant; override;
    property Value: String read FValue;
  end;

  { TEAUnary }

  TEAUnary = class(TAExpression)
  private
    FNot: Boolean;
    FExpr: TAExpression;
  public
    constructor Create(E: TAExpression; const Op: String);
    destructor Destroy; override;
    function Calc: Variant; override;
    property Expr: TAExpression read FExpr;
  end;

  { TEABinary }

  TEABinary = class(TAExpression)
  private
    FE1, FE2: TAExpression;
    FOp: String;
    function Cast(O1, O2: Variant): Char;
    function CalcNum(O1, O2: Variant): Variant;
    function CalcStr(O1, O2: Variant): Variant;
    function CalcBool(O1, O2: Variant): Variant;
    function CalcDate(O1, O2: Variant): Variant;
    function CalcNull(O1, O2: Variant): Variant;
    procedure CheckZero(O2: Variant);
  public
    constructor Create(E1, E2: TAExpression; const Op: String);
    destructor Destroy; override;
    function Calc: Variant; override;
    property E1: TAExpression read FE1;
    property E2: TAExpression read FE2;
  end;

  TAExprArr = array of TAExpression;

  { TEAFunction }

  TEAFunction = class(TAExpression)
  private
    function GetText(CmpIdx: Integer): String;
    function GetFieldType(CmpIdx: Integer): String;
    function GetExprType(CmpIdx: Integer): String;
    function IsSameTypes(SrcIdx, DestIdx: Integer): Boolean;
    function ValueIn(Values: array of Variant): Boolean;
    function GetTypeText(CmpIdx: Integer): String;
    function GetNumber(CmpIdx: Integer): Double;
    function GetCmpType(CmpIdx: Integer; AsText: Boolean): String;
    function GetCmpFieldType(CmpIdx: Integer): String;
    function GetValuesCount(GridIdx, CmpIdx: Integer; Value: Variant): Integer;
    function GetRowCount(CmpIdx: Integer): Integer;
  private
    FPanel: TCustomActionPanel;
    FFuncName: String;
    FArgs: TAExprArr;
    FIdx: Integer;
    function CalcFunc(Vals: array of Variant): Variant;
  public
    constructor Create(aPanel: TCustomActionPanel; const aFN: String;
      aIdx: Integer; Args: TAExprArr);
    destructor Destroy; override;
    function ResltType: Char;
    function Calc: Variant; override;
    property FuncName: String read FFuncName;
    property Args: TAExprArr read FArgs;
  end;

  { TEAComponent }

  TEAComponent = class(TAExpression)
  private
    FPanel: TCustomActionPanel;
    FName: String;
  public
    constructor Create(aPanel: TCustomActionPanel; const aName: String);
    function Calc: Variant; override;
  end;

  { TAExpressionBuilder }

  TAExpressionBuilder = class
  private
    FExpr: String;
    FPanel: TCustomActionPanel;
    FPos, FLen: Integer;
    function GetChar: String;
    function InnerBuild(aState: Integer): TAExpression;
    function LastState(aState: Integer): Boolean;
    function StartWith(const S: String): Boolean;
    procedure Skip(const S: String);
    function ReadStateOperator(aState: Integer): String;
    function ReadSingle: TAExpression;
    procedure CheckFunc(const Func: String; const Args: array of TAExpression; FuncPos: Integer;
      const ArgsPos: array of Integer; out aIdx: Integer);
    function GetExprType(A: TAExpression): Char;
  public
    function Build(const aExpr: String): TAExpression;
    property Panel: TCustomActionPanel read FPanel write FPanel;
  end;

  { TAExprList }

  TAExprList = class(TList)
  private
    function GeTAExpressions(Index: Integer): TAExpression;
  public
    procedure Clear; override;
    property Expressions[Index: Integer]: TAExpression read GeTAExpressions; default;
  end;

implementation

uses
  Variants, apputils, LazUtf8, actioncontrols, expressions, formmanager;

const
  States: array [0..6, 0..5] of String =
    (('|', '', '', '', '', ''), ('&', '', '', '', '', ''),
    ('!', '', '', '', '', ''), ('=', '<>', '<=', '<', '>=', '>'),
    ('+', '-', '', '', '', ''), ('*', '/', '', '', '', ''),
    ('', '', '', '', '', ''));

  Funcs: array [0..9, 0..2] of String =
    (('GETTEXT', 'c', 's'),
    ('GETFIELDTYPE', 'c', 's'),
    ('GETEXPRTYPE', 'c', 's'),
    ('SAMETYPES', 'cc', 'b'),
    ('VALUEIN', '...', 'b'),
    ('GETTYPETEXT', 'c', 's'),
    ('GETNUMBER', 'c', 'n'),
    ('GETCOMPONENTTYPE', 'c', 's'),
    ('GETVALUESCOUNT', 'ccv', 'n'),
    ('GETROWCOUNT', 'c', 'n'));

  FUNC_GETTEXT = 0;
  FUNC_GETFIELDTYPE = 1;
  FUNC_GETEXPRTYPE = 2;
  FUNC_SAMETYPES = 3;
  FUNC_VALUEIN = 4;
  FUNC_GETTYPETEXT = 5;
  FUNC_GETNUMBER = 6;
  FUNC_GETCOMPONENTTYPE = 7;
  FUNC_GETVALUESCOUNT = 8;
  FUNC_GETROWCOUNT = 9;

function FindFunc(const Func: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(Funcs) do
    if Funcs[i, 0] = Func then Exit(i);
end;

{ TEAComponent }

constructor TEAComponent.Create(aPanel: TCustomActionPanel; const aName: String
  );
begin
  FPanel := aPanel;
  FName := aName;
end;

function TEAComponent.Calc: Variant;
var
  C: TComponent;
begin
  C := FPanel.FindComponent(FName);
  if C = nil then
    raise EACalcError.Create(rsComponentNotFound, [FName], FPos);
  Result := C.ComponentIndex;
end;

{ TAExprList }

function TAExprList.GeTAExpressions(Index: Integer): TAExpression;
begin
  Result := TAExpression(Items[Index]);
end;

procedure TAExprList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Expressions[i].Free;
  inherited Clear;
end;

{ TEAFunction }

function TEAFunction.GetText(CmpIdx: Integer): String;
var
  C: TControl;
begin
  C := TControl(FPanel.Components[CmpIdx]);
  if C is TCheckBox then
    Result := Bool2Str(TCheckBox(C).Checked)
  else
    Result := C.Caption;
end;

function TEAFunction.GetFieldType(CmpIdx: Integer): String;
var
  Ctrl: TControl;
  Cbx: TFieldCbx;
  Obj: TObject;
  C: TComponent;
  pF: PRpField;
  pCF: PRpCalcField;
  Tp: TRpFieldType;
begin
  Result := '';
  Ctrl := TControl(FPanel.Components[CmpIdx]);
  if Ctrl is TFieldCbx then
  begin
    if Ctrl.Caption = '' then Exit;
    Cbx := TFieldCbx(Ctrl);
    Obj := Cbx.GetSourceObject;
    if Obj is TdxForm then
    begin
      C := FindComponentByFieldName(TdxForm(Obj), Cbx.Text);
      if C <> nil then
        Result := ActionFieldTypeToStr(C)
      else
        Result := '?';
    end
    else if Obj is TReportData then
    begin
      pF := TReportData(Obj).FindFieldByName(Cbx.Text);
      if pF = nil then
      begin
        pCF := TReportData(Obj).CalcFields.FindFieldByName(Cbx.Text);
        if pCF <> nil then
          Tp := pCF^.Tp
        else
          Result := '?';
      end
      else
      begin
        pF := GetLowField(pF);
        Tp := pF^.Tp;
      end;
      Result := ActionRpFieldTypeToStr(Tp);
    end;
  end
  else raise EACalcError.Create(rsActionFuncRequires, ['field'], 0);
end;

function TEAFunction.GetExprType(CmpIdx: Integer): String;
var
  Ctrl: TControl;
  Fm: TdxForm;
  E: TExpression;
  EB: TExpressionBuilder;
  Tp: Char;
begin
  Result := '';
  Ctrl := TControl(FPanel.Components[CmpIdx]);
  if Ctrl is TActionExpr then
  begin
    Fm := TActionExpr(Ctrl).GetSourceForm;
    EB := TExpressionBuilder.Create;
    try
      E := nil;
      EB.Form := Fm;
      if (Fm <> nil) and (Fm.PId > 0) then
        EB.ParentForm := FormMan.FindForm(Fm.PId);
      EB.SkipLabels := True;
      try
        E := EB.Build(Ctrl.Caption);
      except
        Result := '?';
      end;
      if E <> nil then
      begin
        Tp := EB.GetExprType(E);
        case Tp of
          's': Result := 'text';
          'n': Result := 'number';
          'd': Result := 'date';
          't': Result := 'time';
          'b': Result := 'bool';
          //'v': Result := 'variant';
        end;
      end;
    finally
      FreeAndNil(E);
      EB.Free;
    end;
  end
  else
    raise EACalcError.Create(rsActionFuncRequires, ['expr'], 0);
end;

function TEAFunction.IsSameTypes(SrcIdx, DestIdx: Integer): Boolean;
var
  Src, Dest: TControl;
  SrcR, DestR: String;
begin
  Src := TControl(FPanel.Components[SrcIdx]);
  Dest := TControl(FPanel.Components[DestIdx]);

  if Src is TFieldCbx then SrcR := GetFieldType(SrcIdx)
  else if Src is TActionExpr then SrcR := GetExprType(SrcIdx)
  else if Src is TComponentCbx then SrcR := GetCmpFieldType(SrcIdx)
  else raise EACalcError.Create(rsActionFuncRequires, ['field, component, expr'], 0);

  if Dest is TFieldCbx then DestR := GetFieldType(DestIdx)
  else if Dest is TComponentCbx then DestR := GetCmpFieldType(DestIdx)
  else raise EACalcError.Create(rsActionFuncRequires, ['field'], 0);

  if (DestR = '?') or (SrcR = '?') then Exit(False);
  Result := (SrcR = DestR) or (SrcR = '') or (DestR = '') or (DestR = 'text')
    or (((DestR = 'object') or (DestR = 'counter') or (DestR = 'checkbox') or (DestR = 'recid'))
    and (Src is TActionExpr) and (SrcR = 'number'))
    or (((DestR = 'file') or (DestR = 'image')) and (Src is TActionExpr)
    and (SrcR = 'text'))
    or (((DestR = 'counter') or (DestR = 'number') or (DestR = 'object') or (DestR = 'recid'))
    and ((SrcR = 'counter') or (SrcR = 'number') or (SrcR = 'object') or (SrcR = 'recid')));
end;

function TEAFunction.ValueIn(Values: array of Variant): Boolean;
var
  L, i: Integer;
  Value: String;
begin
  Result := False;
  L := Length(Values);
  if L > 0 then Value := VarToStr(Values[0]);
  for i := 1 to L - 1 do
    if Value = VarToStr(Values[i]) then Exit(True);
end;

function TEAFunction.GetTypeText(CmpIdx: Integer): String;
var
  C: TControl;
  S: String;
begin
  C := TControl(FPanel.Components[CmpIdx]);
  if C is TFieldCbx then S := GetFieldType(CmpIdx)
  else if C is TActionExpr then S := GetExprType(CmpIdx)
  else if C is TComponentCbx then Exit(GetCmpType(CmpIdx, True))
  else raise EACalcError.Create(rsActionFuncRequires, ['field, component, expr'], 0);
  if S = 'text' then S := rsText
  else if S = 'number' then S := rsNumber
  else if S = 'date' then S := rsDate
  else if S = 'time' then S := rsTime
  else if S = 'bool' then S := rsBoolean
  else if S = 'object' then S := rsObject
  else if S = 'checkbox' then S := rsCheckBox
  else if S = 'image' then S := rsImage
  else if S = 'file' then S := rsFile
  else if S = 'counter' then S := rsCounter
  else if S = 'recid' then S := rsRecordId
  else if S = '?' then
  begin
    if C is TFieldCbx then S := rsFieldNotFoundError
    else if C is TActionExpr then S := rsExprContainsError
  end
  else S := '';
  Result := S;
end;

function TEAFunction.GetNumber(CmpIdx: Integer): Double;
var
  S: String;
begin
  S := GetText(CmpIdx);
  Result := StrToFloatDef(S, 0);
end;

function TEAFunction.GetCmpType(CmpIdx: Integer; AsText: Boolean): String;
var
  Ctrl: TControl;
  Fm: TdxForm;
  C: TComponent;
  Cbx: TComponentCbx;
begin
  Result := '';
  Ctrl := TControl(FPanel.Components[CmpIdx]);
  if Ctrl is TComponentCbx then
  begin
    if Ctrl.Caption = '' then Exit;
    Cbx := TComponentCbx(Ctrl);
    Fm := Cbx.GetSourceForm;
    if (Fm <> nil) and (Cbx.ItemIndex > 0) then
    begin
      C := TComponent(Cbx.Items.Objects[Cbx.ItemIndex]);
      C := Fm.FindComponent(C.Name);
      if AsText then
      begin
        if C <> nil then Result := GetComponentType(C)
        else Result := rsComponentNotFoundError;
      end
      else
      begin
        if C <> nil then Result := LowerCase(C.ClassName)
        else Result := '?';
      end;
    end;
  end
  else
    raise EACalcError.Create(rsActionFuncRequires, ['component'], 0);
end;

function TEAFunction.GetCmpFieldType(CmpIdx: Integer): String;
var
  Ctrl: TControl;
  Fm: TdxForm;
  C: TComponent;
  Cbx: TComponentCbx;
begin
  Result := '';
  Ctrl := TControl(FPanel.Components[CmpIdx]);
  if Ctrl is TComponentCbx then
  begin
    if Ctrl.Caption = '' then Exit;
    Cbx := TComponentCbx(Ctrl);
    Fm := Cbx.GetSourceForm;
    if (Fm <> nil) and (Cbx.ItemIndex > 0) then
    begin
      C := TComponent(Cbx.Items.Objects[Cbx.ItemIndex]);
      C := Fm.FindComponent(C.Name);
      if C <> nil then Result := ActionFieldTypeToStr(C)
      else Result := '?';
    end;
  end
end;

function TEAFunction.GetValuesCount(GridIdx, CmpIdx: Integer; Value: Variant
  ): Integer;
var
  GridCtrl, Ctrl: TComponent;
  GEAC: TEAControl;
  idx, i, j: Integer;
  Grid: TActionGrid;
  S: String;
begin
  Result := 0;
  GridCtrl := FPanel.Components[GridIdx];
  Ctrl := FPanel.Components[CmpIdx];
  if GridCtrl is TActionGrid then
  begin
    Grid := TActionGrid(GridCtrl);
    GEAC := FPanel.EAction.Controls.FindByName(GridCtrl.Name);
    idx := GEAC.Controls.FindIndexByName(Ctrl.Name);
    if idx < 0 then
      raise EACalcError.Create(rsUnableToFindGridColumn, [], 0);
    S := VarToStr(Value);
    for i := Grid.FixedRows to Grid.RowCount - 1 do
      if S = Grid.Cells[idx, i] then Inc(Result);
  end
  else
    raise EACalcError.Create(rsActionFuncRequires, ['grid'], 0);
end;

function TEAFunction.GetRowCount(CmpIdx: Integer): Integer;
var
  Ctrl: TComponent;
begin
  Ctrl := FPanel.Components[CmpIdx];
  if Ctrl is TActionGrid then
    with TActionGrid(Ctrl) do
      Result := RowCount - FixedRows
  else
    raise EACalcError.Create(rsActionFuncRequires, ['grid'], 0);
end;

function TEAFunction.CalcFunc(Vals: array of Variant): Variant;
var
  V: Variant;
begin
  V := Null;
  case FIdx of
    FUNC_GETTEXT: V := GetText(Vals[0]);
    FUNC_GETFIELDTYPE: V := GetFieldType(Vals[0]);
    FUNC_GETEXPRTYPE: V := GetExprType(Vals[0]);
    FUNC_SAMETYPES: V := IsSameTypes(Vals[0], Vals[1]);
    FUNC_VALUEIN: V := ValueIn(Vals);
    FUNC_GETTYPETEXT: V := GetTypeText(Vals[0]);
    FUNC_GETNUMBER: V := GetNumber(Vals[0]);
    FUNC_GETCOMPONENTTYPE: V := GetCmpType(Vals[0], False);
    FUNC_GETVALUESCOUNT: V := GetValuesCount(Vals[0], Vals[1], Vals[2]);
    FUNC_GETROWCOUNT: V := GetRowCount(Vals[0]);
  end;
  Result := V;
end;

constructor TEAFunction.Create(aPanel: TCustomActionPanel; const aFN: String;
  aIdx: Integer; Args: TAExprArr);
var
  i: Integer;
begin
  FPanel := aPanel;
  FFuncName := aFN;
  FIdx := aIdx;
  SetLength(FArgs, Length(Args));
  for i := 0 to High(Args) do FArgs[i] := Args[i];
end;

destructor TEAFunction.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FArgs) do FArgs[i].Free;
  SetLength(FArgs, 0);
  inherited Destroy;
end;

function TEAFunction.ResltType: Char;
begin
  if FIdx >= 0 then
	  Result := Funcs[FIdx, 2][1]
end;

function TEAFunction.Calc: Variant;
var
  Vals: array of Variant;
  i, P: Integer;
begin
  SetLength(Vals, Length(FArgs));
  try try
    for i := 0 to High(FArgs) do
      Vals[i] := FArgs[i].Calc;
    Result := CalcFunc(Vals);
  except
    on E: Exception do
    begin
      P := FPos;
      if E is EACalcError then P := EACalcError(E).Position;
      raise EACalcError.Create(FFuncName + ' -> ' + E.Message, [], P);
    end;
  end;
  finally
    SetLength(Vals, 0);
  end;
end;

{ TAExpressionBuilder }

function TAExpressionBuilder.Build(const aExpr: String): TAExpression;
begin
  FExpr := aExpr;
  FLen := Length(aExpr);
  FPos := 1;
  Skip(' ');
  Result := InnerBuild(0);
  if (Result <> nil) and (FPos <= FLen) then
    raise EACalcError.Create(rsUnexpectedToken, [GetChar], FPos);
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
    'c': Result := rsComponent;
    else Result := rsUnknown;
  end;
end;

// Для rsUnexpectedToken
function TAExpressionBuilder.GetChar: String;
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

function TAExpressionBuilder.InnerBuild(aState: Integer): TAExpression;
var
  IsMinus, UnarNot: Boolean;
  a1, a2: TAExpression;
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
          raise EACalcError.Create(rsInvalidOpForType, [Op, t1S], Op1Pos)
        else if not (t2 in ['n', 's', 'v']) then
          raise EACalcError.Create(rsInvalidOpForTypeExpected, [Op, t2S, t1S], Op2Pos)
        else if ((t1 = 'n') and (t2 = 's')) or ((t1 = 's') and (t2 = 'n')) then
          raise EACalcError.Create(rsOpNotValidForDifOperands, [Op, t1S, t2S], Op2Pos);
      '-', '*', '/':
        if not (t1 in ['n', 'v']) then
          raise EACalcError.Create(rsInvalidOpForTypeExpected, [Op, t1S, rsNumber], Op1Pos)
        else if not (t2 in ['n', 'v']) then
          raise EACalcError.Create(rsInvalidOpForTypeExpected, [Op, t2S, rsNumber], Op2Pos);
      // логические
      '&', '|':
        if not (t1 in ['b', 'v']) then
          raise EACalcError.Create(rsInvalidOpForTypeExpected, [Op, t1S, rsBoolean], Op1Pos)
        else if not (t2 in ['b', 'v']) then
          raise EACalcError.Create(rsInvalidOpForTypeExpected, [Op, t2S, rsBoolean], Op2Pos);
      // Равенство
      else
        if (t1 <> t2) and (t1 <> 'v') and (t2 <> 'v') then
          raise EACalcError.Create(rsOpNotValidForDifOperands, [Op, t1S, t2S],  Op2Pos);
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
        raise EACalcError.Create(rsExpectedClosingBracketOrOp, [], FPos);
      Skip(')');
    end
    else
      Result := ReadSingle;
    if IsMinus then
    begin
      t := GetExprType(Result);
      if not (t in ['n', 'v']) then
        raise EACalcError.Create(rsInvalidOpForTypeExpected, ['-', OpTypeToStr(t), rsNumber], Op1Pos);
      Result := TEAUnary.Create(Result, '-');
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
      raise EACalcError.Create(rsInvalidOpForTypeExpected, ['!', OpTypeToStr(t), rsBoolean], Op1Pos);
    a1 := TEAUnary.Create(a1, '!');
    a1.Position := Op1Pos;
  end;

  // последуюущие операнды
  while True do
  begin
    Op := ReadStateOperator(aState);
    if Op='' then Break;
    Op2Pos := FPos;
    a2 := InnerBuild(aState + 1);

    if (a1 = nil) or (a2 = nil) then
    	raise EACalcError.Create(rsOperandExpected, [], Op2Pos);
    {if not CheckTypes(a1, a2) then
      raise EACalcError.Create(ecIncompatibleTypes, Op2Pos, ''); }
    CheckOperands;
    a1 := TEABinary.Create(a1, a2, Op);
    a1.Position := Op1Pos;
  end;

  Result := a1;
end;

function TAExpressionBuilder.LastState(aState: Integer): Boolean;
begin
  Result := (aState + 1) >= Length(States);
end;

function TAExpressionBuilder.StartWith(const S: String): Boolean;
begin
  Result := Copy(FExpr, FPos, Length(S)) = S;
end;

procedure TAExpressionBuilder.Skip(const S: String);
begin
  if StartWith(S) then
    FPos := FPos + Length(S);
  while (FPos <= FLen) and (FExpr[FPos] in [#9, #10, #13, #32]) do
    Inc(FPos);
end;

function TAExpressionBuilder.ReadStateOperator(aState: Integer): String;
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

function TAExpressionBuilder.ReadSingle: TAExpression;
var
  p0, idx, FuncPos: Integer;
  q: Char;
  E: Extended;
  Args: array of TAExpression;
  ArgsPos: array of Integer;
  A: TAExpression;
  Func, S: String;
  FS: TFormatSettings;
begin
  p0 := FPos;

  // строка
  if StartWith('''') then
  begin
    q := '''';
    FPos := PosEx(q, FExpr, FPos + 1);
    if FPos = 0 then raise EACalcError.Create(rsEndLineExpected, [], p0);
    Result := TEAString.Create(Copy(FExpr, p0 + 1, FPos - p0 - 1));
    Skip(q);
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
      raise EACalcError.Create(rsInvalidNumber, [S], p0);
    Skip(' ');
    Result := TEANumber.Create(E);
    Exit;
  end;

  // функция или компонент
  while (FPos <= FLen) and (FExpr[FPos] in ['A'..'Z', 'a'..'z', '_', '0'..'9']) do
    Inc(FPos);

  if FPos > p0 then
  begin
    Skip(' ');
    // функция
    if StartWith('(') then
    begin
      FuncPos := p0;
      Func := Copy(FExpr, p0, FPos - p0);
      SetLength(Args, 0);
      SetLength(ArgsPos, 0);
      A := nil;

      Skip('(');
      if not StartWith(')') then
        while FPos <= FLen do
        begin
          p0 := FPos;
          A := InnerBuild(0);
          if A = nil then raise EACalcError.Create(rsArgExpected, [], p0);

          SetLength(Args, Length(Args) + 1);
          Args[High(Args)] := A;
          SetLength(ArgsPos, Length(ArgsPos) + 1);
          ArgsPos[High(ArgsPos)] := p0;

          if StartWith(')') then Break
          else if StartWith(',') then Skip(',')
          else raise EACalcError.Create(rsCommaExpected, [], FPos);
        end;
      if not StartWith(')') then raise EACalcError.Create(rsArgExpected, [], FPos);
      Skip(')');
      CheckFunc(Func, Args, FuncPos, ArgsPos, idx);
      Result := TEAFunction.Create(FPanel, Func, idx, Args);
      Exit;
    end
    // компонент
    else
    begin
      S := Copy(FExpr, p0, FPos - p0);
      Result := TEAComponent.Create(FPanel, S);
      Exit;
    end;
  end;

  if FPos <= FLen then
    raise EACalcError.Create(rsUnexpectedToken, [GetChar], FPos);
  Result := nil;
end;

procedure TAExpressionBuilder.CheckFunc(const Func: String;
  const Args: array of TAExpression; FuncPos: Integer;
  const ArgsPos: array of Integer; out aIdx: Integer);
var
  i: Integer;
  S: String;
  W: Char;
begin
  aIdx := -1;
  S := AnsiUpperCase(Func);
  // Встроенные
  i := FindFunc(S);
  if i >= 0 then
  begin
    aIdx := i;
    if S = 'VALUEIN' then Exit;
    S := Funcs[i, 1];
    if (Length(S) <> Length(Args)) and (Pos('.', S) = 0) then
      raise EACalcError.Create(rsInvalidNumberArgsFunc, [Func, Length(Args), Length(S)], FuncPos);
    for i := 0 to High(Args) do
    begin
      W := GetExprType(Args[i]);
      if (W <> 'v') and (S[i+1] <> 'v') and (W <> S[i+1]) then
        raise EACalcError.Create(rsIncompatibleArgOfFunc,
          [i+1, Func, OpTypeToStr(W), OpTypeToStr(S[i+1])], ArgsPos[i]);
    end;
  end
  else
    raise EACalcError.Create(rsFuncNotFound2, [Func], FuncPos);
end;

function TAExpressionBuilder.GetExprType(A: TAExpression): Char;
var
  a1, a2: Char;
begin
  if A is TEANumber then Result := 'n'
  else if A is TEAString then Result := 's'
  else if A is TEAUnary then
  begin
    if TEAUnary(A).FNot then Result := 'b'
    else Result := 'n';
  end
  else if A is TEABinary then
    with TEABinary(A) do
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
  else if A is TEAFunction then
  begin
    Result := TEAFunction(A).ResltType;
  end
  else if A is TEAComponent then
  begin
    Result := 'c';
  end
  else Result := '?';
end;

{ EACalcError }

constructor EACalcError.Create(const MsgFmt: string; const args: array of const;
  P: Integer);
begin
  inherited CreateFmt(MsgFmt, args);
  FPos := P;
end;

{ TEABinary }

function TEABinary.Cast(O1, O2: Variant): Char;
begin
  if (O1 = Null) or (O2 = Null) then Exit('0')
  else if VarIsBool(O1) and VarIsBool(O2) then Exit('b')
  else if VarIsNumeric(O1) and VarIsNumeric(O2) then Exit('n')
  else if VarIsStr(O1) and VarIsStr(O2) then Exit('s')
  else if VarIsType(O1, varDate) and VarIsType(O2, varDate) then Exit('d')
  else
  begin
    raise EACalcError.Create(rsOpNotValidForDifOperands, [FOp, VarTypeToStr(O1),
      VarTypeToStr(O2)], E2.Position);
  end;
end;

function TEABinary.CalcNum(O1, O2: Variant): Variant;
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
  else raise EACalcError.Create(rsIllegalNumOp, [], E2.Position);
end;

function TEABinary.CalcStr(O1, O2: Variant): Variant;
begin
  if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  else if FOp = '>' then Result := O1 > O2
  else if FOp = '>=' then Result := O1 >= O2
  else if FOp = '<' then Result := O1 < O2
  else if FOp = '<=' then Result := O1 <= O2
  else if FOp = '+' then Result := VarToStr(O1) + VarToStr(O2)
  // Никогда не должно выполнятся
  else raise EACalcError.Create(rsIllegalStrOp, [], E2.Position);
end;

function TEABinary.CalcBool(O1, O2: Variant): Variant;
begin
  if FOp = '&' then Result := O1 and O2
  else if FOp = '|' then Result := O1 or O2
  else if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  // Никогда не должно выполнятся
  else raise EACalcError.Create(rsIllegalBoolOp, [], E2.Position);
end;

function TEABinary.CalcDate(O1, O2: Variant): Variant;
begin
  if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  else if FOp = '>' then Result := O1 > O2
  else if FOp = '>=' then Result := O1 >= O2
  else if FOp = '<' then Result := O1 < O2
  else if FOp = '<=' then Result := O1 <= O2
  // Никогда не должно выполнятся
  else raise EACalcError.Create(rsIllegalDateOp, [], E2.Position);
end;

function TEABinary.CalcNull(O1, O2: Variant): Variant;
begin
  if FOp = '=' then Result := O1 = O2
  else if FOp = '<>' then Result := O1 <> O2
  else Result := Null;
end;

procedure TEABinary.CheckZero(O2: Variant);
begin
  if O2 = 0 then raise EACalcError.Create(rsDivZero, [], E2.Position);
end;

constructor TEABinary.Create(E1, E2: TAExpression; const Op: String);
begin
  FE1 := E1;
  FE2 := E2;
  FOp := Op;
end;

destructor TEABinary.Destroy;
begin
  FreeAndNil(FE1);
  FreeAndNil(FE2);
  inherited Destroy;
end;

function TEABinary.Calc: Variant;
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

{ TEAUnary }

constructor TEAUnary.Create(E: TAExpression; const Op: String);
begin
  FExpr := E;
  FNot := Op = '!';
end;

destructor TEAUnary.Destroy;
begin
  FExpr.Free;
  inherited Destroy;
end;

function TEAUnary.Calc: Variant;
begin
  Result:=FExpr.Calc;
  if Result = Null then Exit;
  if FNot then
  begin
    if VarIsBool(Result) then Result := not Result
    else raise EACalcError.Create(rsInvalidOpForTypeExpected, ['!', VarTypeToStr(Result),
      rsBoolean], FPos);
  end
  else
  begin
    if VarIsNumeric(Result) then Result := -Result
    else raise EACalcError.Create(rsInvalidOpForTypeExpected, ['-', VarTypeToStr(Result),
      rsNumber], FPos);
  end;
end;

{ TEAString }

constructor TEAString.Create(const aValue: String);
begin
  FValue := aValue;
end;

function TEAString.Calc: Variant;
begin
  Result:=FValue;
end;

{ TEANumber }

constructor TEANumber.Create(aValue: Extended);
begin
  FValue := aValue;
end;

function TEANumber.Calc: Variant;
begin
  Result:=FValue;
end;

{ TAExpression }

function TAExpression.Calc: Variant;
begin
  Result := unassigned;
end;

end.

