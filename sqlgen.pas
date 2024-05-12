unit SqlGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, dxctrls, Db, strconsts, expressions, myclasses;

type

  { ESQLSelectStatementError }

  ESQLSelectStatementError = class(Exception)
  private
    FPos: Integer;
    FProp: String;
  public
    constructor Create(const Msg, AProp: String; P: Integer);
    property Position: Integer read FPos;
    property Prop: String read FProp;
  end;

  { EFilterParserError }

  EFilterParserError = class(Exception)
  private
    FPos: Integer;
  public
    constructor Create(const msg: string; P: Integer);
    property Position: Integer read FPos;
  end;

  { TSQLFilterParser }

  TSQLFilterParser = class
  private
    FDisableCalcExpr: Boolean;
    FExprBuilder: TExpressionBuilder;
    FPos: Integer;
  protected
    FOldPos: Integer;
    function FieldNameParse(const FieldName: String): String; virtual;
    function CheckValue(var Value: String): Boolean; virtual;
    function CheckOp(const Op: String): Boolean; virtual;
    function GetAnotherStr(const AValue: String): String; virtual;
    function GetFieldType: String; virtual;
  public
    function Parse(const Flt: String): String;
    property ExprBuilder: TExpressionBuilder read FExprBuilder write FExprBuilder;
    property DisableCalcExpr: Boolean read FDisableCalcExpr write FDisableCalcExpr;
    property Position: Integer read FPos;
  end;

  { TSQLSourceFilterParser }

  TSQLSourceFilterParser = class(TSQLFilterParser)
  private
    FAliasSL: TStrings;
    FForm: TdxForm;
    FJoinStr, FFieldName, FOp{, FValue}: String;
    FParForm: TdxForm;
    FCmp: TComponent;
  protected
    function FieldNameParse(const aFieldName: String): String; override;
    function CheckValue(var Value: String): Boolean; override;
    function CheckOp(const Op: String): Boolean; override;
    function GetAnotherStr(const AValue: String): String; override;
    function GetFieldType: String; override;
  public
    property Form: TdxForm read FForm write FForm;
    property ParentForm: TdxForm read FParForm write FParForm;
    property JoinStr: String read FJoinStr write FJoinStr;
    property AliasSL: TStrings read FAliasSL write FAliasSL;
  end;

function SqlCreateTable(Id, PId: Integer): String;
function SqlCreateField(Id, FmId, FieldSize: Integer; ClsName: String): String;
function SqlDeleteTable(Id, PId: Integer): String;
function SqlDeleteField(Id, FmId: Integer; ClsName: String): String;
function SqlCompareTables(Fm, SFm: TdxForm): String;
function SqlSelectGroups(SourceTId: Integer; AllFields: Boolean): String;
function SqlSelectStatement(Fm: TdxForm; Fields: TList; UseSelCond, UseFilter: Boolean; CallerObj: TComponent; const UserWhere: String; RecId: Integer; IsTree: Boolean): String;
function SqlSimpleSelectStatement(Fm: TdxForm; RecId: Integer): String;
function SqlInsertStatement(Fm: TdxForm): String;
function SqlUpdateStatementOnlyChanged(Fm: TdxForm): String;
function SqlUpdateStatement(Fm: TdxForm): String;
function SqlDeleteStatement(Fm: TdxForm): String;
function SqlLookupSelect(C: TComponent; Fm, PFm: TdxForm; DS: TDataSet; UseFilter: Boolean; RecId: Integer): String;
function SqlLCbxSelect(LCbx: TdxLookupComboBox; Fm: TdxForm; const Fragments: String; LimitRecs: Boolean): String;
function SqlSelectIDs(SourceTId: Integer; SId: String): String;

function SQLSetFieldSize(Id, FmId, FieldSize: Integer; ClsName: String): String;
function CreateTempTable: String;
function DeleteTempTable: String;
function SQLSetCounter(Id, StartWith: Integer): String;
//function SqlUpdateField(Fm: TdxForm; C: TComponent): String;
function SqlInitField(Id, FmId: Integer; ClsName: String): String;

function TableStr(Id: Integer): String;
function FieldStr(aComponent: TComponent): String;
function FieldStr(Id: Integer): String; overload;
//function TableFieldStr(C: TComponent): String;
function GetAliasNameCbx(Cbx: TComponent): String;
function GetJoinType(ParentObj, Obj: TComponent): String;
function AliasStr(AliasSL: TStrings; const AliasName: String): String;

procedure DoFilterParserError(const Msg: String; P: Integer);

procedure CheckTime(fmt: TdxTimeFormat; var Bg, Ed: String);

implementation

uses
  dximages, dxfiles, apputils, formmanager, Dialogs, Variants, dxusers, dbengine,
  LazUtf8, dateutils, SQLDb, filtercontrol, mytypes;

  { TSQLLookupFilterParser }

  {TSQLLookupFilterParser = class(TSQLFilterParser)
  private
    FCmp: TComponent;
    FSrcForm: TdxForm;
    FFieldName, FValue, FOp: String;
  protected
    function FieldNameParse(const FieldName: String): String; override;
    function CheckValue(var Value: String): Boolean; override;
    function CheckOp(const Op: String): Boolean; override;
    function GetAnotherStr: String; override;
  public
    property SrcForm: TdxForm read FSrcForm write FSrcForm;
  end;     }

procedure DoFilterParserError(const Msg: String; P: Integer);
begin
  raise EFilterParserError.Create(Msg, P);
end;

function TableStr(Id: Integer): String;
begin
  Result := 't' + IntToStr(Id);
end;

function FieldStr(aComponent: TComponent): String;
begin
  Result := FieldStr(GetId(aComponent));
end;

function FieldStr(Id: Integer): String;
begin
  Result := 'f' + IntToStr(Id)
end;

{function TableFieldStr(C: TComponent): String;
begin
  Result := TableStr(TdxForm(C.Owner).Id) + '_' + FieldStr(C);
end;}

function GetAliasNameCbx(Cbx: TComponent): String;
begin
  with TdxLookupComboBox(Cbx) do
	  Result := TableStr(TdxForm(Owner).Id) + '_' + FieldStr(Id) + '_' +
    	TableStr(SourceTId);
end;

function GetJoinType(ParentObj, Obj: TComponent): String;
begin
  if ParentObj = nil then ShowMessage('Aga!!!')
  else
  begin
  if GetRequired(ParentObj) and GetRequired(Obj) and IsHierarchyObj(Obj) then Result := ' inner join '
  else Result := ' left join ';

  end;
end;

function AliasStr(AliasSL: TStrings; const AliasName: String): String;
var
  i: Integer;
begin
  i := AliasSL.IndexOf(AliasName);
  if i < 0 then raise Exception.Create('AliasStr: ' + AliasName + ' is -1');
  Result := 'a' + IntToStr(i);
end;

function SqlCreateTable(Id, PId: Integer): String;
var
  T: String;
begin
  T := TableStr(Id);
  Result := 'create table ' + T + ' (id integer primary key not null);' +
    'create sequence gen_' + T + ';';
  if PId > 0 then
    Result := Result + 'alter table ' + T + ' add pid integer;' +
      'create index i' + T + 'pid on ' + T + '(pid);';
end;

function SqlCreateField(Id, FmId, FieldSize: Integer; ClsName: String): String;
var
  ft, tn, fn: String;
begin
  Result := '';
  ClsName := LowerCase(ClsName);
  if ClsName = 'tdxedit' then
    ft := 'varchar(' + IntToStr(FieldSize) + ')'
  else if ClsName = 'tdxcalcedit' then
    ft := 'double precision'
  else if ClsName = 'tdxdateedit' then
    ft := 'date'
  else if ClsName = 'tdxmemo' then
  begin
    if FieldSize > 0 then
      ft := 'varchar(' + IntToStr(FieldSize) + ')'
    else
      ft := 'BLOB SUB_TYPE 1 SEGMENT SIZE 512'
  end
  else if ClsName = 'tdxcheckbox' then
    ft := 'smallint'
  else if ClsName = 'tdxcombobox' then
    ft := 'varchar(' + IntToStr(FieldSize) + ')'
  else if ClsName = 'tdxlookupcombobox' then
    ft := 'integer'
  else if ClsName = 'tdxdbimage' then
    ft := 'BLOB SUB_TYPE 0 SEGMENT SIZE 512'
  else if ClsName = 'tdxfile' then
    ft := 'BLOB SUB_TYPE 0 SEGMENT SIZE 512'
  else if ClsName = 'tdxtimeedit' then
    ft := 'time'
  else if ClsName = 'tdxcounter' then
    ft := 'integer'
  else
    Exit;
  tn := TableStr(FmId);
  fn := FieldStr(Id);
  if ClsName = 'tdxdbimage' then
    Result := Result + 'alter table ' + tn + ' add ' + fn + 'src varchar(255);' +
      'alter table ' + tn + ' add ' + fn + 'dest varchar(150);' +
      'alter table ' + tn + ' add ' + fn + 'thumb ' + ft + ';' +
      'alter table ' + tn + ' add ' + fn + ' ' + ft + ';'
  else if ClsName = 'tdxfile' then
    Result := Result + 'alter table ' + tn + ' add ' + fn + 'src varchar(255);' +
      'alter table ' + tn + ' add ' + fn + 'dest varchar(150);' +
      'alter table ' + tn + ' add ' + fn + 'd varchar(' + IntToStr(FieldSize) + ');' +
      'alter table ' + tn + ' add ' + fn + ' ' + ft + ';'
  else
    Result := Result + 'alter table ' + tn + ' add ' + fn + ' ' + ft + ';';
  if Pos(ClsName, 'tdxdateedit tdxlookupcombobox tdxcounter') > 0 then
    Result := Result + 'create index i' + fn + ' on ' + tn + '(' + fn + ');';
  if ClsName = 'tdxcounter' then
  begin
    Result := Result + 'create sequence gen_' + fn + ';';
  end;
end;

function SqlDeleteTable(Id, PId: Integer): String;
var
  T: String;
begin
  T := TableStr(Id);
  Result := '';
  if PId > 0 then
    Result := Result + 'drop index i' + T + 'pid;';
  Result := Result + 'drop table ' + T + ';drop sequence gen_' + T + ';';
end;

function SqlDeleteField(Id, FmId: Integer; ClsName: String): String;
var
  tn, fn: String;
begin
  Result := '';
  tn := TableStr(FmId);
  fn := FieldStr(Id);
  ClsName := LowerCase(ClsName);
  if Pos(ClsName, 'tdxdateedit tdxlookupcombobox tdxcounter') > 0 then
    Result := Result + 'drop index i' + fn + ';';
  if ClsName = 'tdxcounter' then
      Result := Result + 'drop sequence gen_' + fn + ';';
  if ClsName = 'tdxdbimage' then
    Result := Result + 'alter table ' + tn + ' drop ' + fn + 'src;' +
      'alter table ' + tn + ' drop ' + fn + 'dest;' +
      'alter table ' + tn + ' drop ' + fn + 'thumb;' +
      'alter table ' + tn + ' drop ' + fn + ';'
  else if ClsName = 'tdxfile' then
    Result := Result + 'alter table ' + tn + ' drop ' + fn + 'src;' +
      'alter table ' + tn + ' drop ' + fn + 'dest;' +
      'alter table ' + tn + ' drop ' + fn + 'd;' +
      'alter table ' + tn + ' drop ' + fn + ';'
  else
    Result := Result + 'alter table ' + tn + ' drop ' + fn + ';';
end;

function SqlCompareTables(Fm, SFm: TdxForm): String;
var
  i, id: Integer;
  C, CC: TComponent;
begin
  Result := '';
  {for i := 0 to SFm.ComponentCount - 1 do
  begin
    C := SFm.Components[i];
    if not IsField(C) then Continue;
    Id := GetId(C);
    CC := FindById(Fm, Id);
    if CC = nil then
      Result := Result + SQLDeleteField(C)
    else if IsTextField(CC) and IsTextField(C) then
      // пропускаем
    else if (CC.ClassName <> C.ClassName) then
      Result := Result + SQLDeleteField(C)
  end;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not IsField(C) then Continue;
    Id := GetId(C);
    CC := FindById(SFm, Id);
    if CC = nil then
      Result := Result + SQLCreateField(C) + SQLInitField(Fm, C)
    else if (IsTextField(CC) and IsTextField(C)) or ((CC is TdxCounter) and
      (C is TdxCounter)) then
      Result := Result + SQLUpdateField(Fm, C)
    else if (C.ClassName <> CC.ClassName) then
      Result := Result + SQLCreateField(C) + SQLInitField(Fm, C)
  end;     }
end;

function SqlSelectGroups(SourceTId: Integer; AllFields: Boolean): String;
var
  Fm: TdxForm;
  TNm, FNm, PFNm, S, sLevelChars: String;
  i, FId, LevelChars: Integer;
  C: TComponent;
begin
  Result := '';
  if SourceTId = 0 then Exit;
  Fm := FormMan.FindForm(SourceTId);
  FId := GetFormParentFieldFieldId(Fm);
  if FId = 0 then Exit;
  TNm := TableStr(SourceTId);
  FNm := FieldStr(FId);
  PFNm := FieldStr(Fm.ParentField);
	LevelChars := Fm.LevelCount * GetFieldSize(FindById(Fm, FId)) + Fm.LevelCount - 1;
  if LevelChars > 8191 then LevelChars := 8191;
  sLevelChars := IntToStr(LevelChars);
  Result := 'with recursive pathes as ' +
	  '(select id, CAST(LEFT(' + FNm + ',' +
    sLevelChars + ') as VARCHAR(' + sLevelChars + ')) as ' + FNm + ' from ' + TNm +
    ' where ' + PFNm + ' is null ' +
    'union all ' +
    'select t.id,LEFT(p.' + FNm + ' || ''\'' || t.' + FNm + ',' + sLevelChars + ') from ' + TNm +
    ' t join pathes p on t.' + PFNm + '=p.id) ';

  if AllFields then
  begin
    S := '';
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if IsField(C) and (GetId(C) <> FId) then
      	S := S + ',' + TableStr(Fm.Id) + '.' + FieldStr(C);
    end;
    Result := Result + 'select p.id,p.' + FNm +
    	S + ' from pathes p inner join ' +
    	TableStr(Fm.Id) + ' on p.id=' + TableStr(Fm.Id) + '.id';
  end
  else
    Result := Result + 'select id,' + FNm + ' from pathes';
end;

{function SqlSelectGroupsForTree(Cbx: TdxLookupComboBox): String;
var
  Fm: TdxForm;
  TNm, FNm, PFNm: String;
begin
  Result := '';
  if (Cbx.SourceTId = 0) or (Cbx.SourceFId = 0) then Exit;
  Fm := FormMan.FindForm(Cbx.SourceTId);
  if Fm.ParentField = 0 then Exit;
  TNm := TableStr(Cbx.SourceTId);
  FNm := FieldStr(Cbx.SourceFId);
  PFNm := FieldStr(Fm.ParentField);
  Result := 'with recursive pathes as ' +
    '(select id, ' + FNm + ' from ' + TNm +
    ' where ' + PFNm + ' is null ' +
    'union all ' +
    'select t.id,p.' + FNm + ' || ''\'' || t.' + FNm + ' from ' + TNm +
    ' t join pathes p on t.' + PFNm + '=p.id) ';

 Result := Result + 'select id,' + IntToStr(Cbx.Id) + ',' + FNm + ' from pathes';
end;        }

function SqlSelCondFilter(SrcFm, Fm: TdxForm; const Cond: String; var JStr: String; AliasSL: TStrings): String;
var
  EB: TExpressionBuilder;
  P: TSQLSourceFilterParser;
begin
  EB := TExpressionBuilder.Create;
  EB.SkipLabels:=True;
  if Fm <> nil then
  begin
    EB.Form := Fm;
    if Fm.ParentForm <> nil then
	    EB.ParentForm := Fm.ParentForm
    else
      EB.ParentForm := Fm;
    EB.DataSet := Fm.DataSet;
  end;
  P := TSQLSourceFilterParser.Create;
  P.ExprBuilder := EB;
  P.Form := SrcFm;
  P.ParentForm := SrcFm;
  P.AliasSL := AliasSL;
  P.JoinStr := JStr;
  try
    Result := P.Parse(Cond);
  finally
    JStr := P.JoinStr;
    P.Free;
    EB.Free;
  end;
end;

function SqlListFilter(Fm: TdxForm; Obj: TComponent; var JStr: String; AliasSL: TStrings): String;
var
  EB: TExpressionBuilder;
  P: TSQLSourceFilterParser;
begin
  Result := '';
  EB := TExpressionBuilder.Create;
  EB.DataSet := Fm.DataSet;
  EB.Form := TdxForm(Obj.Owner);
  EB.ParentForm := EB.Form.ParentForm;
  EB.DataSet := EB.Form.DataSet;
  EB.SkipLabels:=True;
  P := TSQLSourceFilterParser.Create;
  P.ExprBuilder := EB;
  P.Form := Fm;
  P.ParentForm := Fm;
  P.AliasSL := AliasSL;
  P.JoinStr := JStr;
  try
    Result := P.Parse(GetComboFilter(Obj));
    JStr := P.JoinStr;
  finally
    P.Free;
    EB.Free;
  end;
end;
        
function ProcessJoinObject(Obj: TdxLookupComboBox; var Jn: String;
  AliasSL: TStringList): Boolean;
var
  STa, ST, T, F, AliasNm: String;
begin
  Result := True;
  T := TableStr(TdxForm(Obj.Owner).Id);
  F := FieldStr(Obj);
  with Obj do
    if (SourceTId > 0) and (SourceFId > 0) then
    begin
      STa := GetAliasNameCbx(Obj);
      if AliasSL.IndexOf(STa) < 0 then
      begin
        AliasSL.Add(STa);
        AliasNm := AliasStr(AliasSL, STa);

        ST := SqlSelectGroups(SourceTId, True);
        if ST = '' then ST := TableStr(SourceTId)
        else ST := '(' + ST + ')';
        Jn := Jn + GetJoinType(Obj, Obj) + ST + ' ' + AliasNm + ' on ' + T + '.' + F + '=' +
          AliasNm + '.id';
      end;
    end
    else
      Result := False;
end;

function ProcessObjectField(Obj: TdxLookupComboBox; ObjF: TdxObjectField;
  var Fl, Jn: String; AliasSL: TStringList): Boolean;
var
  SrcFm: TdxForm;
  C: TComponent;
  F, Ta, ST, STa, AliasNm, CbxAliasNm, FlNm: String;
begin
  Result := False;
  if not ProcessJoinObject(Obj, Jn, AliasSL) then Exit;

  SrcFm := FormMan.FindForm(Obj.SourceTId);
  if SrcFm = nil then Exit;
  C := FindById(SrcFm, ObjF.FieldId);
  if C = nil then Exit;
  Ta := GetAliasNameCbx(Obj);
  CbxAliasNm := AliasStr(AliasSL, Ta);

  F := FieldStr(C);
  if C is TdxLookupComboBox then
    with TdxLookupComboBox(C) do
    begin
      if (SourceTId > 0) and (SourceFId > 0) then
      begin
        STa := Ta + '_' + FieldStr(C) + '_' + TableStr(SourceTId);
        if AliasSL.IndexOf(STa) < 0 then
        begin
          AliasSL.Add(STa);
          AliasNm := AliasStr(AliasSL, STa);

          ST := SqlSelectGroups(SourceTId, True);
          if ST = '' then ST := TableStr(SourceTId)
          else ST := '(' + ST + ')';
          Jn := Jn + GetJoinType(Obj, C) + ST + ' ' + AliasNm + ' on ' + CbxAliasNm + '.' + F + '=' +
            AliasNm + '.id';
        end
        else
          AliasNm := AliasStr(AliasSL, STa);

        if LookupObjectField(ObjF, True) is TdxRecordId then FlNm := 'id'
        else FlNm := FieldStr(SourceFId);
        Fl := Fl + AliasNm + '.' + FlNm + ' as ' + FieldStr(ObjF.Id) + ',';
        Fl := Fl + AliasNm + '.id as ' + FieldStr(ObjF.Id) + 'id,';
      end
      else Exit;
    end
  else if C is TdxRecordId then
    Fl := Fl + CbxAliasNm + '.id as ' + FieldStr(ObjF.Id) + ','
  else
  begin
    Fl := Fl + CbxAliasNm + '.' + F + ' as ' + FieldStr(ObjF.Id) + ',';
  end;
  Result := True;
end;

{procedure GetObjectFieldComponent(var C: TComponent; var FlNm: String);
var
  ObjF: TdxObjectField;
  Obj: TComponent;
  Fm, SrcFm: TdxForm;
begin
  ObjF := TdxObjectField(C);
  C := nil;
  Fm := TdxForm(ObjF.Owner);
  Obj := FindById(Fm, ObjF.ObjId);
  if Obj <> nil then
  begin
    SrcFm := FormMan.FindForm(GetSourceTId(Obj));
    if SrcFm <> nil then
    begin
      C := FindById(SrcFm, ObjF.FieldId);
      FlNm := TableStr(Fm.Id) + '_' + FieldStr(Obj) + '_' +
      	TableStr(SrcFm.Id) + '.';// + FieldStr(ObjF.FieldId);
      if C is TdxRecordId then
        FlNm := FlNm + 'id'
      else
        FlNm := FlNm + FieldStr(ObjF.FieldId);
    end;
  end;
end;}

procedure GetObjectFieldComponent(var C: TComponent; out AliasTblNm, FlNm: String);
var
  ObjF: TdxObjectField;
  Obj: TComponent;
  Fm, SrcFm: TdxForm;
begin
  ObjF := TdxObjectField(C);
  C := nil; AliasTblNm := '';
  Fm := TdxForm(ObjF.Owner);
  Obj := FindById(Fm, ObjF.ObjId);
  if Obj <> nil then
  begin
    SrcFm := FormMan.FindForm(GetSourceTId(Obj));
    if SrcFm <> nil then
    begin
      C := FindById(SrcFm, ObjF.FieldId);
      AliasTblNm := TableStr(Fm.Id) + '_' + FieldStr(Obj) + '_' +
      	TableStr(SrcFm.Id);
      if C is TdxRecordId then
        FlNm := 'id'
      else
        FlNm := FieldStr(ObjF.FieldId);
    end;
  end;
end;

function CheckNumber(const S: String): String;
begin
  Result := StringReplace(S, DefaultFormatSettings.DecimalSeparator, '.', []);
end;

procedure CheckTime(fmt: TdxTimeFormat; var Bg, Ed: String);
begin
  case fmt of
    ttHH:
      begin
        if Bg <> '' then
	        Bg := FormatDateTime('hh:00:00:0000', StrToTime(Bg), DefaultSQLFormatSettings);
        if Ed <> '' then
	        Ed := FormatDateTime('hh:59:59:9999', StrToTime(Ed), DefaultSQLFormatSettings);
      end;
    ttHHMM:
      begin
        if Bg <> '' then
	        Bg := FormatDateTime('hh:nn:00:0000', StrToTime(Bg), DefaultSQLFormatSettings);
        if Ed <> '' then
	        Ed := FormatDateTime('hh:nn:59:9999', StrToTime(Ed), DefaultSQLFormatSettings);
      end;
    ttHHMMSS:
      begin
        if Bg <> '' then
	        Bg := FormatDateTime('hh:nn:ss:0000', StrToTime(Bg), DefaultSQLFormatSettings);
        if Ed <> '' then
	        Ed := FormatDateTime('hh:nn:ss:9999', StrToTime(Ed), DefaultSQLFormatSettings);
      end;
  end;
end;

function CheckDate(const Value: String): String;
var
  DT: TDateTime;
begin
  if Value = '' then Exit('');
  DT := StrToDate(Value);
  Result := Date2Str(DT);
end;

function DateCodeToPeriod(const S: String; out Bg, Ed: String): Boolean;
begin
  Result := False;
  if Copy(S, 1, 1) = '$' then
  begin
    Ed := DateToStr(Date);
    case TPeriodType(StrToInt(Copy(S, 2, 10))) of
      ptToday: Bg := DateToStr(Date);
      ptThisWeek: Bg := DateToStr( IncDay(Date, -DayOfTheWeek(Date)+1) );
      ptThisMonth: Bg := DateToStr( IncDay(Date, -DayOf(Date)+1) );
      ptThisYear: Bg := DateToStr( EncodeDate(YearOf(Date), 1, 1) );
    end;
    Result := True;
  end;
end;

function SqlFormFilter(Fm: TdxForm; Flt: TFilterObject; var Jn: String;
  AliasSL: TStringList; IsTree: Boolean): String;
var
  S, W, V, FlNm, Bg, BgAbs, Ed, EdAbs, Tmp, AliasName: String;
  i, j: Integer;
  C: TComponent;
  F: TFilterField;
  ObjF: TdxObjectField;
begin
  Result := '';
  S := '';
  for i := 0 to Flt.Count - 1 do
  begin
    F := Flt.Fields[i];
    C := FindById(Fm, F.FId);
    if C = nil then Continue;
    if IsTree and ((Fm.Tree.Fields.FindField(F.FId, tfsForm) <> nil) or
      (Fm.Tree.Fields.FindField(F.FId, tfsObject) <> nil)) then Continue;
    FlNm := TableStr(Fm.Id) + '.' + FieldStr(C);
    // Поле объекта
    if C is TdxObjectField then
    begin
      ObjF := TdxObjectField(C);
      GetObjectFieldComponent(C, AliasName, FlNm);
      if C = nil then Continue;
      // ObjId > 0 и FieldId > 0, потому что C <> nil
      ProcessObjectField(TdxLookupcomboBox(FindById(Fm, ObjF.ObjId)), ObjF, Tmp, Jn, AliasSL);
      FlNm := AliasStr(AliasSL, AliasName) + '.' + FlNm;
    end
    else if C is TdxFile then
      FlNm := FlNm + 'd'
    else if C is TdxDBImage then
      FlNm := FlNm + 'src'
    else if C is TdxRecordId then
      FlNm := TableStr(Fm.Id) + '.id';
    //
    V := '';
    if F.IsNull then
      V := V + FlNm + ' is null or ';
    for j := 0 to F.Values.Count - 1 do
    begin
      W := F.Values[j];
      if W = '' then Continue;

      if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) or (C is TdxFile)
        or (C is TdxDBImage) then
        V := V + FlNm + ' containing ''' + EscapeSQuotes(W) + ''' or '
      else if C is TdxCalcEdit then
      begin
        V := V + '(';
        Bg := F.Value[j];
        if Bg <> '' then
        begin
          Bg := CheckNumber(F.Value[j]);
          BgAbs := StringReplace(Bg, '-', '', []);
          Bg := Bg + '-' + BgAbs + '*2e-12';
          V := V + FlNm + '>=' + Bg + ' and ';
        end;
        Ed := F.EndValue[j];
        if Ed <> '' then
        begin
          Ed := CheckNumber(Ed);
          EdAbs := StringReplace(Ed, '-', '', []);
          Ed := Ed + '+' + EdAbs + '*2e-12';
          V := V + FlNm + '<=' + Ed + ' and ';
        end;
        V := Copy(V, 1, Length(V) - 5) + ') or ';
      end
      else if (C is TdxCounter) or (C is TdxRecordId) then
      begin
        Bg := CheckNumber(F.Value[j]);
        Ed := CheckNumber(F.EndValue[j]);
        V := V + '(';
        if Bg <> '' then
          V := V + FlNm + '>=' + Bg + ' and ';
        if Ed <> '' then
          V := V + FlNm + '<=' + Ed + ' and ';
        V := Copy(V, 1, Length(V) - 5) + ') or ';
      end
      else if C is TdxDateEdit then
      begin
        if not DateCodeToPeriod(W, Bg, Ed) then
        begin
          Bg := CheckDate(F.Value[j]);
          Ed := CheckDate(F.EndValue[j]);
        end;
        V := V + '(';
        if Bg <> '' then
          V := V + FlNm + '>=''' + Bg + ''' and ';
        if Ed <> '' then
          V := V + FlNm + '<=''' + Ed + ''' and ';
        V := Copy(V, 1, Length(V) - 5) + ') or ';
      end
      else if C is TdxTimeEdit then
      begin
        Bg := F.Value[j];
        Ed := F.EndValue[j];
        CheckTime(TdxTimeEdit(C).TimeFormat, Bg, Ed);
        V := V + '(';
        if Bg <> '' then
          V := V + FlNm + '>=''' + Bg  + ''' and ';
        if Ed <> '' then
          V := V + FlNm + '<=''' + Ed + ''' and ';
        V := Copy(V, 1, Length(V) - 5) + ') or ';
      end
      else if C is TdxCheckBox then
        V := V + FlNm + '=' + W + ' or '
      else if C is TdxLookupComboBox then
      begin
        Tmp := SqlSelectIds(GetSourceTId(C), W);
        if Tmp <> '' then
          V := V + '''' + Tmp + ''' containing ''\'' || ' + FlNm + ' || ''\'' or '
        else
          V := V + FlNm + '=' + W + ' or ';
      end;
    end;
    V := Copy(V, 1, Length(V) - 4);
    if V <> '' then
    begin
      if F.IsNot then V := 'not (' + V + ')';
      S := S + '(' + V + ') and ';
    end;
  end;
  S := Copy(S, 1, Length(S) - 5);
  Result := S;
end;

function SqlSelectStatement(Fm: TdxForm; Fields: TList; UseSelCond,
  UseFilter: Boolean; CallerObj: TComponent; const UserWhere: String;
  RecId: Integer; IsTree: Boolean): String;
var
  i, j, Len: Integer;
  C, CC: TComponent;
  CF: TdxObjectField;
  Fl, Jn, S,
  T,        // главная таблица
  F,        // поле главной таблицы
  //ST,     // таблица-источник (для лукап полей)
  SF,       // поле таблицы-источника
  AliasNm   // алиас таблицы-источника
  , Wh: String;
  AliasSL: TStringList;
  SubFm: TdxForm;
  //Obj: TdxLookupComboBox;
begin
  Result := '';
  AliasSL := TStringList.Create;
  T := TableStr(Fm.Id);
  Fl := T + '.id,'; Jn := T;
  if Fm.PId > 0 then
    Fl := Fl + T + '.pid,' + T + '.id as oldid,';

  if Fields <> nil then
  	Len := Fields.Count
  else
    Len := Fm.ComponentCount;

  for i := 0 to Len - 1 do
  begin
    if Fields <> nil then
    	C := TComponent(Fields[i])
    else
	    C := Fm.Components[i];

    if not HasFId(C) then Continue;
    F := FieldStr(C);
    if not (C is TdxObjectField) and not (C is TdxRecordId) then
      Fl := Fl + T + '.' + F + ',';
    if C is TdxLookupComboBox then
    begin
      if ProcessJoinObject(TdxLookupComboBox(C), Jn, AliasSL) then
      begin
        AliasNm := AliasStr(AliasSL, GetAliasNameCbx(C));
        if GetListSourceField(C) is TdxRecordId then
          SF := 'id'
        else
          SF := FieldStr(GetSourceFId(C));
        Fl := Fl + AliasNm + '.' + SF + ' as ' + F + 'l,';
      end
      else
        Fl := Fl + 'null as ' + F + 'l,'; // заглушка
    end
    else if C is TdxObjectField then
    begin
      with TdxObjectField(C) do
        if (ObjId = 0) or (FieldId = 0) or not
          ProcessObjectField(TdxLookupComboBox(FindById(Fm, ObjId)), TdxObjectField(C), Fl, Jn, AliasSL) then
          Fl := Fl + 'null as ' + F + ',';
    end
    else if C is TdxDBImage then
    begin
      Fl := Fl + T + '.' + F + 'src,' + T + '.' + F + 'dest,' + T + '.' +
        F + 'thumb,' + '0 as ' + F + 'c,';
    end
    else if C is TdxFile then
    begin
      Fl := Fl + T + '.' + F + 'src,' + T + '.' + F + 'dest,' + T + '.' +
        F + 'd,' + '0 as ' + F + 'c,';
    end
    else if C is TdxRecordId then
    begin
      Fl := Fl + T + '.id as ' + F + ',';
    end;

  end;
  Fl := Copy(Fl, 1, Length(Fl) - 1);

  Wh := '';
  if RecId >= 0 then
  begin
    Wh := TableStr(Fm.Id) + '.id=' + IntToStr(RecId);
    // В иерархических справочниках, когда родитель является обязательным полем,
    // корневые элементы (без родителя) не попадают в выборку. Зато корневые
    // элементы можно увитедь в родительском поле. Однако при попытке редактировать
    // их (пункт "Изменить" в контекстном меню объекта) программа собщает, что
    // запись удалена другим пользователем. Просто из-за inner join элементы
    // с пустым родителем не выбираются. Поэтому при выборке одной записи
    // делаем left join, чтобы запись все-таки попала в выборку и ее можно
    // было редактировать.
    Jn := StringReplace(Jn, 'inner join', 'left join', [rfReplaceAll]);
  end
  else //if UserFilter = '' then
  begin
    // !!! Доступ
    if UseSelCond then
    begin
      S := UserMan.GetSelCond(Fm.Id);
      try
        S := SqlSelCondFilter(Fm, nil, S, Jn, AliasSL);
        if S <> '' then
	        Wh := Wh + '(' + S + ')';
      except
        on E: EFilterParserError do
          raise ESQLSelectStatementError.Create(E.Message, rsSelCond, E.Position);
      end;
    end;
    //
    if UseFilter then
    begin
      S := SqlFormFilter(Fm, Fm.Filter, Jn, AliasSL, IsTree);
      if S <> '' then
      begin
        if Wh <> '' then Wh := Wh + ' and ';
        Wh := Wh + '(' + S + ')';
      end;
      for i := 0 to Fm.FormCount - 1 do
      begin
        SubFm := Fm.FormByIndex[i];
        S := SqlFormFilter(SubFm, SubFm.Filter, Jn, AliasSL, False);
        if S <> '' then
        begin
          if Wh <> '' then Wh := Wh + ' and ';
          Wh := Wh + 'exists (select ' + TableStr(SubFm.Id) + '.id from ' +
            TableStr(SubFm.Id) + ' where ' + TableStr(SubFm.Id) + '.pid=' +
            TableStr(Fm.Id) + '.id and ' + S + ')';
        end;
      end;
    end;
    if CallerObj <> nil then
      try
  	    S := SqlListFilter(Fm, CallerObj, Jn, AliasSL);
        if S <> '' then
        begin
	        if Wh <> '' then Wh := Wh + ' and ';
  	      Wh := Wh + '(' + S + ')';
        end;
      except
        on E: EFilterParserError do
          raise ESQLSelectStatementError.Create(E.Message, rsListFilter, E.Position);
      end;
    if Fm.CustomFilter <> '' then
    begin
      S := SqlSelCondFilter(Fm, Fm.CustomFilterForm, Fm.CustomFilter, Jn, AliasSL);
      if S <> '' then
      begin
        if Wh <> '' then Wh := Wh + ' and ';
        Wh := Wh + '(' + S + ')';
      end;
    end;
	end;
  {else
  begin
    S := SqlSelCondFilter(Fm, CurForm, UserFilter, Jn, AliasSL);
    if S <> '' then
	    Wh := Wh + '(' + S + ')';
  end; }
  if UserWhere <> '' then
  begin
    if Wh <> '' then Wh := Wh + ' and ';
    Wh := Wh + '(' + UserWhere + ')';
  end;

  if IsTree then
  	Result := 'select distinct '
  else
    Result := 'select ';

  Result := Result + Fl + ' from ' + Jn;
  if Wh <> '' then Result := Result + ' where ' + Wh;
  AliasSL.Free;
end;

function SqlSimpleSelectStatement(Fm: TdxForm; RecId: Integer): String;
var
  i: Integer;
  C: TComponent;
  S, FNm: String;
begin
  S := 'select ';
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not IsField(C) then Continue;
    FNm := FieldStr(C);
    if C is TdxFile then
      S := S + FNm + 'src,' + FNm + 'dest,' + FNm + 'd,'
    else if C is TdxDBImage then
      S := S + FNm + 'src,' + FNm + 'dest,'
    else
      S := S + FNm + ',';
  end;

  if S = 'select ' then Exit('');

  S := Copy(S, 1, Length(S) - 1);
  Result := S + ' from ' + TableStr(Fm.Id) + ' where id=' + IntToStr(RecId);
end;

{function SqlSelectObjectFields(Cbx: TdxLookupComboBox; var SQL: String
  ): Boolean;
var
  Fm: TdxForm;
  RecId: LongInt;
  FL: TList;
  i: Integer;
  C: TComponent;
begin
  Result := False;
  Fm := TdxForm(Cbx.Owner);
  RecId := Cbx.Field.AsInteger;
  FL := TList.Create;
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C is TdxObjectField) and (TdxObjectField(C).ObjId = Cbx.Id) then
    	FL.Add(C);
  end;
  if FL.Count > 0 then
  begin
    FL.Insert(0, Cbx);			// Вставляем объект, чтобы получить соединение со справочником
    SQL := SqlSelectStatement(Fm, FL, False, nil, nil, '', RecId);
    Result := True;
  end;
  FL.Free;
end;   }

function SqlInsertStatement(Fm: TdxForm): String;
var
  i: Integer;
  C: TComponent;
  Fl, Vl, FNm: String;
begin
  Fl := 'id,'; Vl := ':id,';
  if Fm.PId > 0 then begin Fl := Fl + 'pid,'; Vl := Vl + ':pid,'; end;
  Result := 'insert into ' + TableStr(Fm.Id);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not IsField(C) then Continue;
    FNm := FieldStr(C);
    //if C is TdxDBImage then FNm := FNm + 'thumb';
    Fl := Fl + FNm + ',';
    Vl := Vl + ':' + FNm + ',';
    if C is TdxDBImage then
    begin
      Fl := Fl + FNm + 'src,' + FNm + 'dest,' + FNm + 'thumb,';
      Vl := Vl + ':' + FNm + 'src,:' + FNm + 'dest,:' + FNm + 'thumb,';
    end
    else if C is TdxFile then
    begin
      Fl := Fl + FNm + 'src,' + FNm + 'dest,' + FNm + 'd,';
      Vl := Vl + ':' + FNm + 'src,:' + FNm + 'dest,:' + FNm + 'd,';
    end
  end;
  Fl := Copy(Fl, 1, Length(Fl) - 1);
  Vl := Copy(Vl, 1, Length(Vl) - 1);
  Result := Result + ' (' + Fl + ') values (' + Vl + ')';
end;

function SqlUpdateStatementOnlyChanged(Fm: TdxForm): String;
var
  i: Integer;
  C: TComponent;
  FNm: String;
  F: TField;
  AllFields: Boolean;
begin
  // В таблицах строки могут быть поменяны местами. В этом случае обновлять
  // надо все поля.
  AllFields := (Fm.PId > 0) and (Fm.DataSet['id'] <> Fm.DataSet['oldid']);
  Result := 'update ' + TableStr(Fm.Id) + ' set id=:id,';
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    FNm := FieldStr(C);

    if not IsField(C) then Continue
    else if C is TdxDBImage then
    begin
      if AllFields or TdxDBImage(C).WasChanged then
        Result := Result + FNm + '=:' + FNm + ',' +
          FNm + 'src=:' + FNm + 'src,' + FNm + 'dest=:' +
          FNm + 'dest,' + FNm + 'thumb=:' + FNm + 'thumb,';
    end
    else if C is TdxFile then
    begin
      if AllFields or TdxFile(C).WasChanged then
        Result := Result + FNm + '=:' + FNm + ',' +
          FNm + 'src=:' + FNm + 'src,' + FNm + 'dest=:' +
          FNm + 'dest,' + FNm + 'd=:' + FNm + 'd,';
    end
    else
    begin
      F := Fm.DataSet.FieldByName(FNm);
      if AllFields or (F.Value <> F.OldValue) then
        Result := Result + FNm + '=:' + FNm + ',';
    end;
  end;
  Result := Copy(Result, 1, Length(Result) - 1) + ' where id=:id';
end;

function SqlUpdateStatement(Fm: TdxForm): String;
var
  i: Integer;
  C: TComponent;
  FNm: String;
begin
  Result := 'update ' + TableStr(Fm.Id) + ' set ';
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    FNm := FieldStr(C);

    if not IsField(C) then Continue
    else if C is TdxDBImage then
      Result := Result + FNm + '=:' + FNm + ',' +
        FNm + 'src=:' + FNm + 'src,' + FNm + 'dest=:' +
        FNm + 'dest,' + FNm + 'thumb=:' + FNm + 'thumb,'
    else if C is TdxFile then
      Result := Result + FNm + '=:' + FNm + ',' +
        FNm + 'src=:' + FNm + 'src,' + FNm + 'dest=:' +
        FNm + 'dest,' + FNm + 'd=:' + FNm + 'd,'
    else
      Result := Result + FNm + '=:' + FNm + ',';
  end;
  Result := Copy(Result, 1, Length(Result) - 1) + ' where id=:id';
end;

function SqlDeleteStatement(Fm: TdxForm): String;
begin
  Result := 'delete from ' + TableStr(Fm.Id) + ' where id=:id';
end;

function SqlLookupFilter(Fm, PFm: TdxForm; DS: TDataSet; Cbx: TComponent;
  AliasSL: TStrings; var JStr: String): String;
var
  EB: TExpressionBuilder;
  //P: TSQLLookupFilterParser;
  P: TSQLSourceFilterParser;
  SrcFm: TdxForm;
  Flt, S: String;
  //AliasSL: TStringList;
begin
  Result := '';
  SrcFm := FormMan.FindForm(GetSourceTId(Cbx));
  if SrcFm = nil then Exit;
  Flt := GetComboFilter(Cbx);
  //AliasSL := TStringList.Create;

  if Flt <> '' then
  begin
    EB := TEXpressionBuilder.Create;
    EB.DataSet := DS;
    EB.Form := Fm;
    EB.ParentForm := PFm;
    EB.SkipLabels:=True;
    P := TSQLSourceFilterParser.Create;
    P.ExprBuilder := EB;
    P.Form := SrcFm;
    P.ParentForm := SrcFm;
    P.AliasSL := AliasSL;
    P.JoinStr := JStr;
    try
      Result := P.Parse(Flt);
      JStr := P.JoinStr;
    finally
      P.Free;
      EB.Free;
    end;
  end;

  // !!! Доступ
  if UserMan.GetApplySelCondToObj(SrcFm.Id) then
  begin
    S := UserMan.GetSelCond(SrcFm.Id);
    if S <> '' then
    begin
      S := SqlSelCondFilter(SrcFm, nil, S, JStr, AliasSL);
      if Result <> '' then Result := Result + ' and ';
      Result := Result + '(' + S + ')';
    end;
  end;
  //

  //AliasSL.Free;
end;

function SqlLookupSelect(C: TComponent; Fm, PFm: TdxForm; DS: TDataSet;
  UseFilter: Boolean; RecId: Integer): String;
var
  TId, FId: Integer;
  Grps, Wh, JStr, FlNm: String;
  AliasSL: TStringList;
begin
  TId := GetSourceTId(C);
  FId := GetSourceFId(C);
  if (TId = 0) or (FId = 0) then Exit;

  if GetListSourceField(C) is TdxRecordId then
    FlNm := 'id'
  else
    FlNm := FieldStr(FId);
  Result := 'select ' + TableStr(TId) + '.id,' +
  	TableStr(TId) + '.' + FlNm + ' from ';

  Grps := SqlSelectGroups(TId, True);
  if Grps <> '' then
  	Result := Result + '(' + Grps + ') ' + TableStr(TId)
  	//Result := 'select * from (' + Grps + ') ' + TableStr(TId)
  else
    Result := Result + TableStr(TId);
	  {Result := 'select ' + TableStr(TId) + '.id,' +
  	 	TableStr(TId) + '.' + FieldStr(FId) + ' from ' + TableStr(TId);}


  AliasSL := TStringList.Create;
  JStr := '';
  Wh := '';
  if UseFilter then
	  Wh := SqlLookupFilter(Fm, PFm, DS, C, AliasSL, JStr)
  else if RecId > 0 then
		Wh :='id=' + IntToStr(RecId);

  if Wh <> '' then Result := Result + JStr + ' where ' + Wh;

  Result := Result + ' order by 2';// + TableStr(TId) + '.' + FieldStr(FId);
  AliasSL.Free;
  //DebugStr(Result);
end;

function DateFormatToSql(const FlNm: String): String;
const
  _lpad = 'lpad(extract(%s from %s), %d, ''0'') || ''%s'' || ';
var
  Fmt, PartDate: String;
  i, n: Integer;
  DateSep: Char;
begin
  Result := '';
  Fmt := AnsiLowerCase(DefaultFormatSettings.ShortDateFormat);
  DateSep := DefaultFormatSettings.DateSeparator;
  PartDate := '';
  for i := 1 to Length(Fmt) do
  begin
    case Fmt[i] of
      'd': begin PartDate := 'day'; n := 2; end;
      'm': begin PartDate := 'month'; n := 2; end;
      'y': begin PartDate := 'year'; n := 4; end;
      else
      begin
        if PartDate <> '' then
        	Result := Result + Format(_lpad, [PartDate, FlNm, n, DateSep]);
        PartDate := '';
        Continue;
      end;
    end;
  end;
  if PartDate <> '' then
  	Result := Result + Format(_lpad, [PartDate, FlNm, n, DateSep]);

  Result := Copy(Result, 1, Length(Result) - 11);
end;

function IsValidCharsSql(const S: String; Sep: Char): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(S) do
  begin
    if not (S[i] in ['0'..'9', Sep]) then
    	Exit(False);
  end;
end;

function GetCastByFieldId(Fm: TdxForm; FId: Integer; const S: String; LCbxFields: TStrings): String;
var
  C: TComponent;
  FlNm: String;
  i: Integer;
begin
  Result := '';
  FlNm := TableStr(Fm.Id) + '.' + FieldStr(FId);
  C := FindById(Fm, FId);
  if C is TdxCalcEdit then
  begin
    if IsValidCharsSql(S, DefaultFormatSettings.DecimalSeparator) then
    	Result := 'substring(' + FlNm + ' from 1 for position(''.'',' + FlNm + ')+' +
      	IntToStr(TdxCalcEdit(C).Precission) + ') containing ''' +
        StringReplace(S, DefaultFormatSettings.DecimalSeparator, '.', []) + ''''
  end
  else if C is TdxDateEdit then
  begin
    if IsValidCharsSql(S, DefaultFormatSettings.DateSeparator) then
	    Result := DateFormatToSql(FlNm) + ' containing ''' + S + ''''
  end
  else if C is TdxTimeEdit then
  begin
    if IsValidCharsSql(S, DefaultFormatSettings.TimeSeparator) then
    	Result := FlNm + ' containing ''' + S + ''''
  end
  else if (C is TdxCheckBox) or (C is TdxCounter) then
  begin
    if IsValidCharsSql(S, '0') then
    	Result := FlNm + ' containing ''' + S + ''''
  end
  else if C is TdxLookupComboBox then
  begin
    i := LCbxFields.IndexOfObject(C);
    if i >= 0 then
    	Result := LCbxFields[i] + ' containing ''' + S + ''''
  end
  else if C is TdxRecordId then
  begin
    if IsValidCharsSql(S, '0') then
    	Result := TableStr(Fm.Id) + '.id containing ''' + S + ''''
  end
  else
	  Result := FlNm + ' containing ''' + S + '''';

  if Result = '' then Result := '1=0';
end;

function SqlLCbxSelect(LCbx: TdxLookupComboBox; Fm: TdxForm;
  const Fragments: String; LimitRecs: Boolean): String;
var
  TId, FId, i, j: Integer;
  Grps, Wh, JStr, ST, STa, SF, F, T, S, Flt, FlNm, AliasNm: String;
  SrcFm: TdxForm;
  LF: TLCbxListField;
  C: TComponent;
  AliasSL, SL, LCbxFields: TStringList;
begin
  TId := LCbx.SourceTId;
  FId := LCbx.SourceFId;
  if (TId = 0) or (FId = 0) then Exit;
  T := TableStr(TId);

  SrcFm := FormMan.FindForm(TId);
  AliasSL := TStringList.Create;
  LCbxFields := TStringList.Create;

  JStr := '';
  Result := 'select ';
  if LimitRecs then Result := Result + ' first 100 ';
  if GetListSourceField(LCbx) is TdxRecordId then FlNm := 'id'
  else FlNm := FieldStr(FId);
  Result := Result + T + '.id,' + T + '.' + FlNm;

  for i := 0 to LCbx.ListFields.Count - 1 do
  begin
    LF := LCbx.ListFields[i];
    F := FieldStr(LF.FieldId);
    C := FindById(SrcFm, LF.FieldId);
    if C is TdxLookupComboBox then
    	with TdxLookupComboBox(C) do
	    begin
	      if (SourceTId > 0) and (SourceFId > 0) then
        begin
          STa := GetAliasNameCbx(C);
          AliasSL.Add(STa);
          AliasNm := AliasStr(AliasSL, STa);

          ST := SqlSelectGroups(SourceTId, True);
          if ST = '' then ST := TableStr(SourceTId)
          else ST := '(' + ST + ')';
          SF := FieldStr(SourceFId);
          Result := Result + ',' + AliasNm + '.' + SF + ' as ' + F;// + 'l,';
          JStr := JStr + GetJoinType(LCbx, C) + ST + ' ' + AliasNm + ' on ' + T + '.' + F + '=' +
            AliasNm + '.id';
          LCbxFields.AddObject(AliasNm + '.' + SF, C);
        end
        else
          Result := Result + ',null as ' + F; // заглушка
      end
    else
    begin
      if C is TdxRecordId then F := 'id';
	  	Result := Result + ',' + T + '.' + F;
    end;
  end;

	Wh := SqlLookupFilter(Fm, Fm.ParentForm, Fm.DataSet, LCbx, AliasSL, JStr);

  if Trim(Fragments) <> '' then
  begin

    Flt := '';
    SL := TStringList.Create;
    SplitStr(Fragments, ' ', SL);
    for i := 0 to SL.Count - 1 do
    begin
      S := EscapeSQuotes(SL[i]);
      if S = '' then Continue;

      Flt := Flt + '(' + GetCastByFieldId(SrcFm, FId, S, nil) + ' or ';

      for j := 0 to LCbx.ListFields.Count - 1 do
      begin
        LF := LCbx.ListFields[j];
        if not LF.Searchable then Continue;

        Flt := Flt + GetCastByFieldId(SrcFm, LF.FieldId, S, LCbxFields) + ' or ';
      end;

      Flt := Copy(Flt, 1, Length(Flt) - 4) + ') and ';
    end;
    Flt := Copy(Flt, 1, Length(Flt) - 5);
    SL.Free;

    if Wh <> '' then Wh := '(' + Wh + ') and ';
    Wh := Wh + '(' + Flt + ')';
  end;

  Grps := SqlSelectGroups(TId, True);
  if Grps <> '' then
  	Result := Result + ' from (' + Grps + ') ' + T
  else
    Result := Result + ' from ' + T;

  Result := Result + JStr;

  if Wh <> '' then Result := Result + ' where ' + Wh;

  Result := Result + ' order by 2';

  AliasSL.Free;
  LCbxFields.Free;
  //DebugStr(Result);
end;

function SqlSelectIDs(SourceTId: Integer; SId: String): String;
var
  Fm: TdxForm;
  S, PFNm, TNm, Tmp: String;
  p: Integer;
begin
  Result := '';
  Fm := FormMan.FindForm(SourceTId);
  if Fm.ParentField = 0 then Exit;
  TNm := TableStr(SourceTId);
  PFNm := FieldStr(Fm.ParentField);
  S := 'with recursive pathes as ' +
    '(select id, cast(id as varchar(100)) as pid from ' + TNm +
    ' where ' + PFNm + ' is null ' +
    'union all ' +
    'select t.id, t.id || ''\'' || p.pid from ' + TNm +
    ' t join pathes p on t.' + PFNm + '=p.id) ' +
    'select pid from pathes';
  with DBase.OpenDataSet(S) do
  try
    S := '\';
    SId := '\' + SId + '\';
    while not Eof do
    begin
      Tmp := Fields[0].AsString;
      if Pos(SId, '\' + Tmp + '\') > 0 then
      begin
        p := Pos('\', Tmp);
        if p > 0 then S := S + Copy(Tmp, 1, p - 1) + '\'
        else S := S + Tmp + '\';
      end;
      Next;
    end;
    Result := S;
  finally
    Free;
  end;
end;

{function SqlSelCondFilter2(SrcFm: TdxForm; const Cond: String; Fm,
  PFm: TdxForm; DS: TDataSet): String;
var
  EB: TExpressionBuilder;
  P: TSQLLookupFilterParser;
begin
  EB := TExpressionBuilder.Create;
  EB.SkipLabels:=True;
  EB.Form := Fm;
  EB.ParentForm := PFm;
  EB.DataSet := DS;
  P := TSQLLookupFilterParser.Create;
  P.ExprBuilder := EB;
  P.SrcForm := SrcFm;
  try
    Result := P.Parse(Cond);
  finally
    P.Free;
    EB.Free;
  end;
end;   }

{function SQLSetFieldSize(Id, FmId, FieldSize: Integer; ClsName: String): String;
var
  TNm, FNm: String;
begin
  ClsName := LowerCase(ClsName);
  TNm := TableStr(FmId);
  FNm := FieldStr(Id);
  if ClsName = 'tdxfile' then FNm := FNm + 'd';
  Result := Format('alter table %0:s alter column %1:s to %1:stmp;' +
    'alter table %0:s add %1:s varchar(%2:d);' +
    'commit;' +
    'update %0:s set %1:s=LEFT(%1:stmp,%2:d);' +
    'commit;' +
    'alter table %0:s drop %1:stmp;' +
    'commit;', [TNm, FNm, FieldSize]);
end;}

function SQLSetFieldSize(Id, FmId, FieldSize: Integer; ClsName: String): String;
var
  TNm, FNm: String;
begin
  ClsName := LowerCase(ClsName);
  TNm := TableStr(FmId);
  FNm := FieldStr(Id);
  if ClsName = 'tdxfile' then FNm := FNm + 'd';
  if FieldSize > 0 then
    Result := Format('delete from tmp;' +
      'commit;' +
      'insert into tmp (id, text) select id, LEFT(%1:s, 2000) from %0:s;' +
      'commit;' +
      'alter table %0:s drop %1:s;' +
      'commit;' +
      'alter table %0:s add %1:s varchar(%2:d);' +
      'commit;' +
      'update %0:s set %1:s=(select LEFT(text, %2:d) from tmp where id=%0:s.id);' +
      'commit;', [TNm, FNm, FieldSize])
  else
    Result := Format('delete from tmp;' +
      'commit;' +
      'insert into tmp (id, text) select id, %1:s from %0:s;' +
      'commit;' +
      'alter table %0:s drop %1:s;' +
      'commit;' +
      'alter table %0:s add %1:s BLOB SUB_TYPE 1 SEGMENT SIZE 512;' +
      'commit;' +
      'update %0:s set %1:s=(select text from tmp where id=%0:s.id);' +
      'commit;', [TNm, FNm])
end;

function CreateTempTable: String;
begin
  Result := 'create table tmp (id integer primary key not null, text varchar(2000));'
end;

function DeleteTempTable: String;
begin
  Result := 'drop table tmp;';
end;

function SQLSetCounter(Id, StartWith: Integer): String;
begin
  Result := 'alter sequence gen_' + FieldStr(Id) + ' restart with ' +
    IntToStr(StartWith) + ';';
end;

{function SqlUpdateField(Fm: TdxForm; C: TComponent): String;
var
  FS: Integer;
  S, TNm, FNm: String;
begin
  S := '';
  if (C is TdxEdit) or (C is TdxMemo) or (C is TdxComboBox) or (C is TdxFile) then
  begin
    FS := GetFieldSize(C);

    if FS <> GetOldSize(C) then
    begin
      TNm := TableStr(Fm.Id);
      FNm := FieldStr(C);
      if C is TdxFile then FNm := FNm + 'd';
      S := S + 'alter table ' + TNm + ' alter column ' + FNm + ' to tmp;' +
        'alter table ' + TNm + ' add ' + FNm + ' varchar(' + IntToStr(FS) + ');' +
        'commit;' +
        'update ' + TNm + ' set ' + FNm + '=LEFT(tmp,' + IntToStr(FS) + ');' +
        'commit;' +
        'alter table ' + TNm + ' drop tmp;' +
        'commit;';
      SetOldSize(C, FS);
    end
  end
  else if C is TdxCounter then
  begin
    with TdxCounter(C) do
      if Restart then
      begin
        S := S + 'alter sequence gen_' + FieldStr(C) + ' restart with ' +
          IntToStr(StartWith) + ';';
        Restart := False;
      end;
  end;
  Result := S;
end;  }

function SqlInitField(Id, FmId: Integer; ClsName: String): String;
begin
  Result := '';
  ClsName := LowerCase(ClsName);
  if (ClsName = 'tdxcalcedit') or (ClsName = 'tdxcheckbox') then
    Result := 'update ' + TableStr(FmId) + ' set ' + FieldStr(Id) + '=0;';
    {Result := 'commit;update ' + TableStr(FmId) + ' set ' + FieldStr(Id) +
      '=0;commit;' }
end;

{ ESQLSelectStatementError }

constructor ESQLSelectStatementError.Create(const Msg, AProp: String; P: Integer
  );
begin
  inherited Create(Msg);
  FPos := P;
  FProp := AProp;
end;

{ EFilterParserError }

constructor EFilterParserError.Create(const msg: string; P: Integer);
begin
  inherited Create(msg);
  FPos := P;
end;

{ TSQLLookupFilterParser }

(*function TSQLLookupFilterParser.FieldNameParse(const FieldName: String): String;
begin
  Result := '';
  FCmp := FindComponentByFieldName(FSrcForm, FieldName);
  if (FCmp = nil) or (FCmp is TdxFile) or (FCmp is TdxDBImage) then Exit;
  Result := TableStr(FSrcForm.Id) + '.' + FieldStr(FCmp);
  FFieldName := Result;
end;

function TSQLLookupFilterParser.CheckValue(var Value: String): Boolean;
var
  Tmp: String;
begin
  Result := CheckType(FCmp, Value);
  // экранируем апострофы
  if (FCmp is TdxEdit) or (FCmp is TdxMemo) or (FCmp is TdxComboBox) or
    (FCmp is TdxDateEdit) then
    Value := '''' + StringReplace(Value, #39, #39#39, [rfReplaceAll]) + ''''
	else if FCmp is TdxCalcEdit then
  begin
    if FOp = '>=' then
    	Value := Value + '-' + Value + '*2e-12'
    else if FOp = '<=' then
    	Value := Value + '+' + Value + '*2e-12';
  end
  else if FCmp is TdxTimeEdit then
  begin
    if FOp = '>=' then
    begin
    	CheckTime(TdxTimeEdit(FCmp).TimeFormat, Value, Tmp);
      Value := '''' + Value + '''';
    end
    else if FOp = '<=' then
    begin
    	CheckTime(TdxTimeEdit(FCmp).TimeFormat, Tmp, Value);
      Value := '''' + Value + '''';
    end
    else
    	Value := '''' + Value + '''';
  end;
  FValue := Value;
end;

function TSQLLookupFilterParser.CheckOp(const Op: String): Boolean;
begin
  FOp := Op;
  Result:=inherited CheckOp(Op);
  if (FCmp is TdxLookupComboBox) and (Op <> '=') and (Op <> '<>') then Result := False;
end;

function TSQLLookupFilterParser.GetAnotherStr: String;
var
  Fm: TdxForm;
  TId: Integer;
  Ed: String;
begin
  Result := '';
  if FCmp is TdxLookupComboBox then
  begin
    TId := GetSourceTId(FCmp);
    Fm := FormMan.FindForm(TId);
    if (Fm <> nil) and (Fm.ParentField > 0) then
    begin
      FValue := SqlSelectIDs(TId, FValue);
      Result := '''' + FValue + ''' containing ''\'' || ' + FFieldName + ' || ''\''';
      if FOp = '<>' then Result := 'not (' + Result + ')';
    end;
  end
  else if FCmp is TdxCalcEdit then
  begin
    if FOp = '=' then
    begin
      Result := FFieldName + '>=' + FValue + '-' + FValue + '*2e-12 and ' +
      	FFieldName + '<=' + FValue + '+' + FValue + '*2e-12';
    end;
  end
  else if FCmp is TdxTimeEdit then
  begin
    if FOp = '=' then
    begin
      Ed := FValue;
      CheckTime(TdxTimeEdit(FCmp).TimeFormat, FValue, Ed);
      Result := FFieldName + '>=''' + FValue + ''' and ' +
      	FFieldName + '<=''' + Ed + '''';
    end;
  end;
end; *)


////////////////////////////////////////////////////////////////////////////////

{ TSQLSourceFilterParser }

function TSQLSourceFilterParser.FieldNameParse(const aFieldName: String
  ): String;
var
  Fm: TdxForm;
  SL: TStringList;
  i: Integer;
  FieldName, S, ParentAliasNm, ParentField, AliasName, Tmp, FlNm, AliasNm: String;
  C, ParentC, RootC: TComponent;
begin
  Result := '';
  FieldName := aFieldName;
  if Length(FieldName) = 0 then Exit;
  Fm := FForm;
  if FieldName[1] = '!' then
  begin
    Fm := FParForm;
    Delete(FieldName, 1, 1);
  end;
  if (Length(FieldName) = 0) or (Fm = nil) then Exit;
  AliasName := '';
  SL := TStringList.Create;

  try

  SplitStr(FieldName, '|', SL);
  for i := 0 to SL.Count - 1 do
  begin
    if i > 0 then AliasName := AliasName + '_';
    AliasName := AliasName + TableStr(Fm.Id);
    S := SL[i];
    C := FindComponentByFieldName(Fm, S);
    if C = nil then Exit;
    if i > 0 then
    begin
      if AliasSL.IndexOf(AliasName) < 0 then
      begin
        AliasSL.Add(AliasName);
        AliasNm := AliasStr(AliasSL, AliasName);
        Tmp := SqlSelectGroups(Fm.Id, True);
        if Tmp <> '' then Tmp := '(' + Tmp + ')'
        else Tmp := TableStr(Fm.Id);
        JoinStr := JoinStr + GetJoinType(RootC, ParentC) + Tmp + ' ' + AliasNm +
          ' on ' + ParentAliasNm + '.' + ParentField + '=' + AliasNm + '.id';

      end
      else
        AliasNm := AliasStr(AliasSL, AliasName);
    end
    else
      AliasNm := AliasName;
    ParentAliasNm := AliasNm;
    ParentField := FieldStr(C);
    ParentC := C;
    if C is TdxLookupComboBox then
    begin
      if i = 0 then RootC := C;
      if i < SL.Count - 1 then
      begin
        AliasName := AliasName + '_' + ParentField;
        Fm := FormMan.FindForm(GetSourceTId(C));
        if Fm = nil then Exit;
      end;
    end
    else if i < SL.Count - 1 then Exit;
  end;
  FlNm := FieldStr(C);
  if C is TdxFile then FlNm := FlNm + 'd'
  else if C is TdxDBImage then FlNm := FlNm + 'src'
  else if C is TdxRecordId then FlNm := 'id';
  FCmp := C;
  Result := AliasNm + '.' + FlNm;

  finally
    SL.Free;
  end;
  FFieldName := Result;
end;

function TSQLSourceFilterParser.CheckValue(var Value: String): Boolean;
var
  Tmp, AbsValue: String;
begin
  Result := CheckType(FCmp, Value);
  // экранируем апострофы
  if (FCmp is TdxEdit) or (FCmp is TdxMemo) or (FCmp is TdxComboBox) or
    (FCmp is TdxDateEdit) or (FCmp is TdxFile) or (FCmp is TdxDBImage) then
    Value := '''' + EscapeSQuotes(Value) + ''''
  else if FCmp is TdxCalcEdit then
  begin
    AbsValue := StringReplace(Value, '-', '', []);
    if FOp = '>=' then
    	Value := Value + '-' + AbsValue + '*2e-12'
    else if FOp = '<=' then
    	Value := Value + '+' + AbsValue + '*2e-12';
  end
  else if FCmp is TdxTimeEdit then
  begin
    if FOp = '>=' then
    begin
    	CheckTime(TdxTimeEdit(FCmp).TimeFormat, Value, Tmp);
    end
    else if FOp = '<=' then
    begin
    	CheckTime(TdxTimeEdit(FCmp).TimeFormat, Tmp, Value);
    end;
    // = обрабатывается в GetAnotherStr
    {else}
    if (FOp <> '=') and (FOp <> '<>') and (FOp <> 'in') and (FOp <> 'notin') then
    	Value := '''' + Value + '''';
  end;
  //FValue := Value;
end;

function TSQLSourceFilterParser.CheckOp(const Op: String): Boolean;
begin
  FOp := Op;
  Result:=inherited CheckOp(Op);
  if (FCmp is TdxLookupComboBox) and (Op <> '=') and (Op <> '<>') and
    (Op <> 'in') and (Op <> 'notin') then Result := False;
end;

function TSQLSourceFilterParser.GetAnotherStr(const AValue: String): String;
var
  Fm: TdxForm;
  TId: Integer;
  Bg, Ed, AbsValue, S, Op: String;
begin
  Result := '';
  if FOp = 'in' then Op := '='
  else if FOp = 'notin' then Op := '<>'
  else Op := FOp;

  if FCmp is TdxLookupComboBox then
  begin
    TId := GetSourceTId(FCmp);
    Fm := FormMan.FindForm(TId);
    if (Fm <> nil) and (Fm.ParentField > 0) then
    begin
      S := SqlSelectIDs(TId, AValue);
      Result := '''' + S + ''' containing ''\'' || ' + FFieldName + ' || ''\''';
      if Op = '<>' then Result := 'not (' + Result + ')';
    end;
  end
  else if FCmp is TdxCalcEdit then
  begin
    if Op = '=' then
    begin
      AbsValue := StringReplace(AValue, '-', '', []);
      Result := FFieldName + '>=' + AValue + '-' + AbsValue + '*2e-12 and ' +
      	FFieldName + '<=' + AValue + '+' + AbsValue + '*2e-12';
    end
    else if Op = '<>' then
    begin
      AbsValue := StringReplace(AValue, '-', '', []);
      Result := '(' + FFieldName + '<' + AValue + '-' + AbsValue + '*2e-12 or ' +
      	FFieldName + '>' + AValue + '+' + AbsValue + '*2e-12)';
    end;
  end
  else if FCmp is TdxTimeEdit then
  begin
    if (Op = '=') or (Op = '<>') then
    begin
      Bg := AValue;
      Ed := AValue;
      CheckTime(TdxTimeEdit(FCmp).TimeFormat, Bg, Ed);
      if Op = '=' then
        Result := FFieldName + '>=''' + Bg + ''' and ' +
        	FFieldName + '<=''' + Ed + ''''
      else
        Result := '(' + FFieldName + '<''' + Bg + ''' or ' +
          FFieldName + '>''' + Ed + ''')';
    end;
  end;
end;

function TSQLSourceFilterParser.GetFieldType: String;
begin
  Result:=GetComponentDataTypeStr(FCmp);
end;

{ TSQLFilterParser }

function TSQLFilterParser.FieldNameParse(const FieldName: String): String;
begin
  Result := '';
end;

function TSQLFilterParser.CheckValue(var Value: String): Boolean;
begin
  Result := True;
end;

function TSQLFilterParser.CheckOp(const Op: String): Boolean;
begin
  Result := Pos(Op, '= <> <= >= == # in notin') > 0;
end;

// Эта функция введена для изменения строки условия, если полем является
// иерархическая группа
function TSQLFilterParser.GetAnotherStr(const AValue: String): String;
begin
  Result := '';
end;

function TSQLFilterParser.GetFieldType: String;
begin
  Result := '';
end;

// Если необязательные поля идут после, то возможна ситуация, когда в
// конце оказывается and или or. В этом случае добавляем фиктивное условие.
function TrailLastOp(const S: String): String;
var
  Tmp: String;
  L: Integer;
begin
  Result := S;
  L := Length(S);
  Tmp := Trim(Copy(S, L - 4, 4));
  if Tmp = 'and'  then Result := S + ' 1=1 '
  else if Tmp = 'or' then Result := S + ' 1=0 ';
end;

function ValueTypeToStr(const Value: String): String;
var
  N: Longint;
  E: Extended;
  DT: TDateTime;
begin
  if TryStrToInt(Value, N) then Result := rsNumber
  else if TryStrToFloat(Value, E) then Result := rsNumber
  else if TryStrToTime(Value, DT) then Result := rsTime
  else if TryStrToDate(Value, DT) then Result := rsDate
  else Result := rsText;
end;

function TSQLFilterParser.Parse(const Flt: String): String;
type
  TState = (stField, stOp, stExpr, stBoolOp, stBrace);
var
  S, Wh, WhIn, Expr, Op, BlOp, FlNm, Tmp: String;
  l, BrCnt, BrN, ExprPos, i: Integer;
  Tk: Char;
  St: TState;
  EB: TExpressionBuilder;
  E: TExpression;
  V: Variant;
  Optional, NeedBrace, EmptyInValueExists, PredicateInUsed: Boolean;
  SL: TStringList;
begin
  Result := '';
  Wh := '';
  FPos := 1;
  St := stField;
  EB := FExprBuilder;
  E := nil;
  BrN := 0;

  try

  while True do
  begin
    FOldPos := FPos;
    S := ReadToken(Flt, FPos, Tk);
    if (S = '{') and (St = stField) then
    begin
      Inc(BrN);
      Wh := Wh + '(';
      Continue;
    end
    else if (Tk in [' ', '/']) and (St <> stExpr) then Continue;
    case St of
      stField:
        begin
          if Tk = '[' then
          begin
            BlOp := '';
            Optional := False;
            Delete(S, 1, 1);          // Удаляем
            Delete(S, Length(S), 1);  // скобки
            if (Length(S) > 0) and (S[1] = '?') then
            begin
              Optional := True;
              Delete(S, 1, 1);
            end;
            FlNm := FieldNameParse(S);
            if FlNm = '' then DoFilterParserError(Format(rsFPSrcFldNotFound, [S]), FOldPos);
            St := stOp;
          end
          else if Tk = #0 then Break
          else DoFilterParserError(rsFPSrcFldExcept, FOldPos);
        end;
      stOp:
        begin
          if (Tk = '=') or ((Tk = 'a') and ((LowerCase(S) = 'in') or (LowerCase(S) = 'notin'))) then
          begin
            S := LowerCase(S);
            if not CheckOp(S) then DoFilterParserError(rsInvalidCmpOp, FOldPos);
            St := stExpr;
            BrCnt := 0;
            Expr := '';
            if S = '==' then S := ' containing '
            else if S = '#' then S := ' not containing ';
            Op := S;
            ExprPos := FPos;
          end
          else DoFilterParserError(rsFPCmpOpExpect, FOldPos);
        end;
      stExpr:
        begin
          NeedBrace := S = '}';
          if (Tk = #0) or (((S = '&') or (S = '|')) and (BrCnt = 0))
            or (NeedBrace) then
          begin
            try
              FreeAndNil(E);
              E := EB.Build(Expr);
              if E = nil then DoFilterParserError(rsFPExprNotParsed, ExprPos);
              if not FDisableCalcExpr then V := E.Calc;
            except
              on Err: ECalcError do
                DoFilterParserError(Err.Message, ExprPos + Err.Position - 1)
            end;
            if (V = Null) and Optional then
            else if V = Null then
            begin
              if (Op = '<>') or (Op = ' not containing ') or (Op = 'notin') then
                S := ' is not null '
              else
                S := ' is null ';
              Wh := Wh + FlNm + S;
            end
            else
            begin
              S := VarToStr(V);
              if (Op = 'in') or (Op = 'notin') then
              begin
                SL := TStringListUtf8.Create;
                SplitStr(Utf8LowerCase(S), ';', SL);
                WhIn := '';
                EmptyInValueExists := SL.Count = 0;
                for i := 0 to SL.Count - 1 do
                begin
                  S := SL[i];
                  if S = '' then
                  begin
                    EmptyInValueExists := True;
                    Continue;
                  end;
                  if not CheckValue(S) then
                    DoFilterParserError(Format(rsIncompatibleTypesSrcFieldAndExpr,
                      [ValueTypeToStr(S), GetFieldType]), ExprPos);
                  Tmp := GetAnotherStr(S);
                  if Tmp = '' then
                    WhIn := WhIn + S + ','
                  else
                  begin
                    WhIn := WhIn + Tmp;
                    if Op = 'in' then WhIn := WhIn + ' or '
                    else WhIn := WhIn + ' and ';
                  end;
                end;
                if WhIn <> '' then
                begin
                  if WhIn[Length(WhIn)] = ',' then
                  begin
                    SetLength(WhIn, Length(WhIn) - 1);
                    WhIn := 'in (' + WhIn + ')';
                    if Op = 'notin' then WhIn := 'not ' + WhIn;
                    if GetFieldType = rsText then
                      WhIn := 'lower(' + FlNm + ') ' + WhIn
                    else
                      WhIn := FlNm + ' ' + WhIn;
                  end
                  else
                    SetLength(WhIn, Length(WhIn) - 4);
                end;
                if EmptyInValueExists then
                begin
                  if WhIn <> '' then
                  begin
                    if Op = 'in' then WhIn := WhIn + ' or ' + FlNm + ' is null'
                    else WhIn := WhIn + ' and ' + FlNm + ' is not null';
                  end
                  else
                  begin
                    if Op = 'in' then WhIn := FlNm + ' is null'
                    else WhIn := FlNm + ' is not null';
                  end;
                end;
                if WhIn <> '' then
                  Wh := Wh + '(' + WhIn + ')';
                SL.Free;
              end
              else
              begin
                if not CheckValue(S) then
                  DoFilterParserError(Format(rsIncompatibleTypesSrcFieldAndExpr,
                    [VarTypeToStr(V), GetFieldType]), ExprPos);
                Tmp := GetAnotherStr(S);
                if Tmp = '' then
                  Wh := Wh + FlNm + Op + S
                else
                  Wh := Wh + Tmp;
              end;
            end;
            if NeedBrace then
            begin
              Dec(FPos);
              St := stBrace;
            end
            else if Tk <> #0 then
            begin
              St := stBoolOp;
              Dec(FPos);
            end
            else Break;
          end
          else
          begin
            if S = '(' then
            begin
              Inc(BrCnt);
              Expr := Expr + S;
            end
            else if S = ')' then
            begin
              Dec(BrCnt);
              Expr := Expr + S;
            end
            else Expr := Expr + S;
          end;
        end;
      stBoolOp:
        begin
          if Optional and (V = Null) then
          else
          begin
            if S = '&' then Wh := Wh + ' and '
            else if S = '|' then Wh := Wh + '  or '
            else DoFilterParserError(rsFPBoolOpExpect, FOldPos);
            BlOp := S;
          end;
          St := stField;
        end;
      stBrace:
        begin
          if S = '}' then
          begin
            Wh := TrailLastOp(Wh);
            l := Length(Wh);
            if Copy(Wh, l, 1) = '(' then
              Delete(Wh, l, 1)
            else
            begin
              Wh := Wh + ')';
              Optional := False;		// Сбрасываем флаг, иначе идущая после
              											// логическая операция проигнорируется (баг от 28.03.2017).
            end;
            Dec(BrN);
          end
          else if (S = '|') or (S = '&') then
          begin
	          BlOp := S;
	 	        Dec(FPos);
  	  	    St := stBoolOp;
          end
          else if Tk = #0 then Break
          else DoFilterParserError(rsFPBoolOpExpect, FOldPos);
        end;
    end;
  end;
  if (BlOp = '|') or (BlOp = '&') then DoFilterParserError(rsFPSrcFldExcept, FPos)
  else if BrN < 0 then DoFilterParserError(rsCharLBrExpext, 1)
  else if BrN > 0 then DoFilterParserError(rsCharRBrExpect, FPos);
  // Если необязательные поля идут после, то возможна ситуация, когда в
  // конце оказывается and или or
  //S := Trim(Copy(Wh, Length(Wh) - 4, 4));
  //if (S = 'and') or (S = 'or') then Wh := Copy(Wh, 1, Length(Wh) - 4);
  Result := TrailLastOp(Wh);

  finally
    FreeAndNil(E);
  end;
end;

end.

