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

unit ExprHighlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEditHighlighter, Graphics;

type
  TdxExprTokenKind = (etkUnknown, etkField, etkLabel, etkSourceField, etkQueryField,
    etkFunc, etkString, etkNumber, etkSymbol, etkComment, etkSpace, etkNull);

  TdxExprRangeState = (ersExpr, ersString, ersComment, ersSourceField);

  TdxExprHighlighterKind = (ekExpr, ekFilter, ekQuery);

  { TdxExprHighlighter }

  TdxExprHighlighter = class(TSynCustomHighlighter)
  private
    FKind: TdxExprHighlighterKind;
    FFieldAsParentField: Boolean;
    FSourceFieldAsParentSourceField: Boolean;
    FUnknownAttr, FFieldAttr, FFuncAttr, FStringAttr, FNumberAttr, FSymbolAttr,
      FCommentAttr, FSpaceAttr, FSourceFieldAttr, FQueryFieldAttr, FLabelAttr: TSynHighlighterAttributes;
    FPos, FTokenPos: Integer;
    FLine: PChar;
    FLineNumber: Integer;
    FRange, FOldRange: TdxExprRangeState;
    FTokenId: TdxExprTokenKind;
    FQuoteCh: Char;
    FBrArr: array of Integer;
    FFieldHashes, FParentFieldHashes, FSourceFieldHashes,
      FParentSourceFieldHashes, FQueryFieldHashes, FFuncHashes, FLabelHashes,
      FParentLabelHashes: TList;
    procedure UnknownProc;
    procedure InProc;
    procedure NotInProc;
    //procedure FragsProc;
    procedure FieldProc;
    procedure FuncProc;
    procedure StringProc;
    procedure RangeStringProc;
    procedure NumberProc;
    procedure SymbolProc;
    procedure SlashProc;
    procedure CommentProc;
    procedure SpaceProc;
    procedure NullProc;
    procedure ExprProc;
    procedure SourceFieldProc;
    procedure SetBracketsCount;
    function GetBracketsCount: Integer;
    function LabelExists(S: String): Boolean;
    function FieldExists(S: String): Boolean;
    function SourceFieldExists(S: String): Boolean;
    function QueryFieldExists(S: String): Boolean;
    function FuncExists(const S: String): Boolean;
    procedure ExcludeFieldPrefix(var S: String; out IsParent: Boolean);
    procedure ExcludeSourceFieldPrefix(var S: String; out IsParent: Boolean);
    function HasFieldPrefix(const S: String): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function GetEol: Boolean; override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    procedure AddField(S: String);
    procedure AddLabel(S: String);
    procedure AddSourceField(S: String);
    procedure AddQueryField(S: String);
    procedure AddFunc(const S: String);
    procedure ClearFuncs;
    procedure ClearFields;
    property FieldAsParentField: Boolean read FFieldAsParentField write
      FFieldAsParentField;
    property SourceFieldAsParentSourceField: Boolean read FSourceFieldAsParentSourceField
      write FSourceFieldAsParentSourceField;
    property Kind: TdxExprHighlighterKind read FKind write FKind;
  end;

implementation

uses
  LazUtf8;

function MakeHash(s: String): Longint;
{small hash maker}
var
  I: Integer;
begin
  Result := 0;
  S := Utf8UpperCase(s);
  for I := 1 to Length(s) do
    Result := ((Result shl 7) or (Result shr 25)) + Ord(s[I]);
end;

{ TdxExprHighlighter }

procedure TdxExprHighlighter.UnknownProc;
begin
  Inc(FPos);
  FTokenId := etkUnknown;
  // Субкоды UTF8
  while FLine[FPos] in [#128..#191] do inc(FPos);
end;

procedure TdxExprHighlighter.InProc;
begin
  if LowerCase(Copy(FLine, FPos+1, 2)) = 'in' then
  begin
    Inc(FPos, 2);
    FTokenId := etkSymbol;
  end
  else UnknownProc;
  FRange := ersExpr;
end;

procedure TdxExprHighlighter.NotInProc;
begin
  if LowerCase(Copy(FLine, FPos+1, 5)) = 'notin' then
  begin
    Inc(FPos, 5);
    FTokenId := etkSymbol;
  end
  else UnknownProc;
  FRange := ersExpr;
end;

{procedure TdxExprHighlighter.FragsProc;
begin
  if LowerCase(Copy(FLine, FPos+1, 5)) = 'frags' then
  begin
    Inc(FPos, 5);
    FTokenId := etkSymbol;
  end
  else UnknownProc;
  FRange := ersExpr;
end;    }

procedure TdxExprHighlighter.FieldProc;
var
  Tk: String;
begin
  Inc(FPos);
  while True do
    case FLine[FPos] of
      ']':
        begin
          Tk := GetToken;
          // Удаляем квадратную скобку
          Delete(Tk, 1, 1);
          if FRange = ersExpr then
          begin
            if FKind = ekQuery then
            begin
              if HasFieldPrefix(Tk) then
              begin
                if FieldExists(Tk) then FTokenId := etkField;
                // В выражениях запроса вычисляемые надписи запрещены
              end
              else if QueryFieldExists(Tk) then FTokenId := etkQueryField;
            end
            else if FieldExists(Tk) then FTokenId := etkField
            else if LabelExists(Tk) then FTokenId := etkLabel;
          end
          else if (FRange = ersSourceField) and SourceFieldExists(Tk) then
            FTokenId := etkSourceField;
          Inc(FPos);
          Break;
        end;
      #0, #10, #13: Break;
      else
        Inc(FPos);
    end;
end;

procedure TdxExprHighlighter.FuncProc;
begin
  Inc(FPos);
  while True do
    case FLine[FPos] of
      'A'..'Z', 'a'..'z', '_', '0'..'9': Inc(FPos);
      else
      begin
        if FuncExists(GetToken) then
          FTokenId := etkFunc;
        Break;
      end;
    end;
end;

procedure TdxExprHighlighter.StringProc;
begin
  FQuoteCh := FLine[FPos];
  Inc(FPos);
  FTokenId := etkString;
  while True do
    case FLine[FPos] of
      '''', '"':
        if FLine[FPos] = FQuoteCh then
        begin
          Inc(FPos);
          Break;
        end
        else Inc(FPos);
      #0, #10, #13:
        begin
          FRange := ersString;
          Break;
        end
      else Inc(FPos);
    end;
end;

procedure TdxExprHighlighter.RangeStringProc;
begin
  case FLine[FPos] of
    #0: NullProc;
    #10, #13: SpaceProc;
    else
    begin
      FTokenId := etkString;
      while True do
        case FLine[FPos] of
          '''', '"':
            if FLine[FPos] = FQuoteCh then
            begin
              FRange := ersExpr;
              Inc(FPos);
              Break;
            end
            else Inc(FPos);
          #0, #10, #13: Break;
          else Inc(FPos);
        end;
    end;
  end;
end;

procedure TdxExprHighlighter.NumberProc;
begin
  Inc(FPos);
  while True do
    case FLine[FPos] of
      '0'..'9', '.': Inc(FPos);
      else
      begin
        FTokenId := etkNumber;
        Break;
      end;
    end;
end;

procedure TdxExprHighlighter.SymbolProc;
begin
  Inc(FPos);
  FTokenId := etkSymbol;
end;

procedure TdxExprHighlighter.SlashProc;
begin
  Inc(FPos);
  case FLine[FPos] of
    '/':
      begin
        FTokenId := etkComment;
        while not (FLine[FPos] in [#0, #10, #13]) do
          Inc(FPos);
      end;
    '*':
      begin
        FTokenId := etkComment;
        Inc(FPos);
        while True do
          case FLine[FPos] of
            '*':
              begin
                Inc(FPos);
                if FLine[FPos] = '/' then
                begin
                  Inc(FPos);
                  Break;
                end;
              end;
            #0, #10, #13:
              begin
                FOldRange := FRange;
                FRange := ersComment;
                Break;
              end
            else Inc(FPos);
          end;
      end;
    else FTokenId := etkSymbol;
  end;
end;

procedure TdxExprHighlighter.CommentProc;
begin
  case FLine[FPos] of
    #0: NullProc;
    #10, #13: SpaceProc;
    else
    begin
      FTokenId := etkComment;
      while True do
        case FLine[FPos] of
          '*':
            begin
              Inc(FPos);
              if FLine[FPos] = '/' then
              begin
                if FKind = ekFilter then FRange := FOldRange
                else FRange := ersExpr;
                Inc(FPos);
                Break;
              end;
            end;
          #0, #10, #13: Break;
          else Inc(FPos);
        end;
      end;
  end;
end;

procedure TdxExprHighlighter.SpaceProc;
begin
  Inc(FPos);
  FTokenId := etkSpace;
end;

procedure TdxExprHighlighter.NullProc;
begin
  FTokenId := etkNull;
end;

procedure TdxExprHighlighter.ExprProc;
begin
  case FLine[FPos] of
    '[': FieldProc;
    '''', '"': StringProc;
    'A'..'Z', 'a'..'z', '_': FuncProc;
    '0'..'9': NumberProc;
    '&', '|':
      begin
        SymbolProc;
        if (FKind = ekFilter) and (GetBracketsCount = 0) then FRange := ersSourceField;
      end;
    '(', ')', ',', '>', '<', '=', '!', '+', '-', '*': SymbolProc;
    '}':
      if FKind = ekFilter then SymbolProc
      else UnknownProc;
    #1..#32: SpaceProc;
    '/': SlashProc;
    #0: NullProc;
    else UnknownProc;
  end;
end;

procedure TdxExprHighlighter.SourceFieldProc;
begin
  case FLine[FPos] of
    '[': FieldProc;
    '>', '<', '=', '#':
      begin
        SymbolProc;
        FRange := ersExpr;
      end;
    'i', 'I': InProc;
    'n', 'N': NotInProc;
    //'f', 'F': FragsProc;
    '{': SymbolProc;
    #1..#32: SpaceProc;
    '/': SlashProc;
    #0: NullProc;
    else UnknownProc;
  end;
end;

procedure TdxExprHighlighter.SetBracketsCount;
var
  h, i: Integer;
begin
  h := High(FBrArr);
  if h < FLineNumber then
  begin
    SetLength(FBrArr, FLineNumber + 1);
    for i := h + 1 to High(FBrArr) do FBrArr[i] := 0;
  end;
  FPos := 0;
  i := FLineNumber;
  FBrArr[i] := 0;
  while True do
  begin
    case FLine[FPos] of
      '(': Inc(FBrArr[i]);
      ')': Dec(FBrArr[i]);
      #0, #10, #13: Break;
    end;
    Inc(FPos);
  end;
end;

function TdxExprHighlighter.GetBracketsCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FLineNumber - 1 do
    Result := Result + FBrArr[i];
  for i := 0 to FPos do
    case FLine[i] of
      '(': Inc(Result);
      ')': Dec(Result);
    end;
end;

function TdxExprHighlighter.LabelExists(S: String): Boolean;
var
  IsParent: Boolean;
begin
  // Чтоб не вводить свойство SkipLabels (т. к. далеко не во всех выражениях
  // можно использовать вычисляемые надписи), просто проверяю списки.
  if (FLabelHashes.Count = 0) and (FParentLabelHashes.Count = 0) then Exit;

  ExcludeFieldPrefix(S, IsParent);
  if IsParent or FFieldAsParentField then
    Result := FParentLabelHashes.IndexOf(Pointer(PtrInt(MakeHash(S)))) >= 0
  else
    Result := FLabelHashes.IndexOf(Pointer(PtrInt(MakeHash(S)))) >= 0;
end;

function TdxExprHighlighter.FieldExists(S: String): Boolean;
var
  IsParent: Boolean;
begin
  ExcludeFieldPrefix(S, IsParent);
  if IsParent or FFieldAsParentField then
    Result := FParentFieldHashes.IndexOf(Pointer(PtrInt(MakeHash(S)))) >= 0
  else
    Result := FFieldHashes.IndexOf(Pointer(PtrInt(MakeHash(S)))) >= 0;
end;

function TdxExprHighlighter.SourceFieldExists(S: String): Boolean;
var
  IsParent: Boolean;
begin
  ExcludeSourceFieldPrefix(S, IsParent);
  if IsParent or FSourceFieldAsParentSourceField then
    Result := FParentSourceFieldHashes.IndexOf(Pointer(PtrInt(MakeHash(S)))) >= 0
  else
    Result := FSourceFieldHashes.IndexOf(Pointer(PtrInt(MakeHash(S)))) >= 0;
end;

function TdxExprHighlighter.QueryFieldExists(S: String): Boolean;
begin
  Result := FQueryFieldHashes.IndexOf(Pointer(PtrInt(MakeHash(S)))) >= 0;
end;

function TdxExprHighlighter.FuncExists(const S: String): Boolean;
begin
  Result := FFuncHashes.IndexOf(Pointer(PtrInt(MakeHash(S)))) >= 0;
end;

procedure TdxExprHighlighter.ExcludeFieldPrefix(var S: String; out
  IsParent: Boolean);
begin
  IsParent := Copy(S, 1, 1) = '!';
  if IsParent then
    Delete(S, 1, 1)
  else if Copy(S, 1, 1) = ':' then
    Delete(S, 1, 1);
end;

procedure TdxExprHighlighter.ExcludeSourceFieldPrefix(var S: String; out
  IsParent: Boolean);
begin
  if Copy(S, 1, 1) = '?' then Delete(S, 1, 1);
  IsParent := Copy(S, 1, 1) = '!';
  if IsParent then
    Delete(S, 1, 1);
end;

function TdxExprHighlighter.HasFieldPrefix(const S: String): Boolean;
begin
  Result := (Copy(S, 1, 1) = '!') or (Copy(S, 1, 1) = ':')
end;

constructor TdxExprHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUnknownAttr := TSynHighlighterAttributes.Create('ExprUnknown');
  FUnknownAttr.Foreground := clBlack;
  FUnknownAttr.Background := $AAAAFF;
  AddAttribute(FUnknownAttr);
  FFieldAttr := TSynHighlighterAttributes.Create('ExprField');
  FFieldAttr.Foreground := clOlive;
  AddAttribute(FFieldAttr);
  FLabelAttr := TSynHighlighterAttributes.Create('ExprLabel');
  FLabelAttr.Foreground := clOlive;
  FLabelAttr.Style := [fsItalic];
  AddAttribute(FLabelAttr);
  FSourceFieldAttr := TSynHighlighterAttributes.Create('ExprSourceField');
  FSourceFieldAttr.Foreground := clTeal;
  FSourceFieldAttr.Style := [fsBold];
  AddAttribute(FSourceFieldAttr);
  FQueryFieldAttr := TSynHighlighterAttributes.Create('ExprQueryField');
  FQueryFieldAttr.Foreground := clMaroon;
  AddAttribute(FQueryFieldAttr);
  FFuncAttr := TSynHighlighterAttributes.Create('ExprFunc');
  FFuncAttr.Foreground := clBlack;
  FFuncAttr.Style := [fsBold];
  AddAttribute(FFuncAttr);
  FStringAttr := TSynHighlighterAttributes.Create('ExprString');
  FStringAttr.Foreground := clGreen;
  AddAttribute(FStringAttr);
  FNumberAttr := TSynHighlighterAttributes.Create('ExprNumber');
  FNumberAttr.Foreground := clBlue;
  AddAttribute(FNumberAttr);
  FSymbolAttr := TSynHighlighterAttributes.Create('ExprSymbol');
  FSymbolAttr.Foreground := clRed;
  AddAttribute(FSymbolAttr);
  FCommentAttr := TSynHighlighterAttributes.Create('ExprComment');
  FCommentAttr.Foreground := clGray;
  FCommentAttr.Style := [fsItalic];
  AddAttribute(FCommentAttr);
  FSpaceAttr := TSynHighlighterAttributes.Create('ExprSpace');
  AddAttribute(FSpaceAttr);
  SetAttributesOnChange(@DefHighlightChange);
  FRange := ersSourceField;
  FFieldHashes := TList.Create;
  FParentFieldHashes := TList.Create;
  FSourceFieldHashes := TList.Create;
  FParentSourceFieldHashes := TList.Create;
  FQueryFieldHashes := TList.Create;
  FFuncHashes := TList.Create;
  FLabelHashes := TList.Create;
  FParentLabelHashes := TList.Create;
end;

destructor TdxExprHighlighter.Destroy;
begin
  FParentLabelHashes.Free;
  FLabelHashes.Free;
  FFuncHashes.Free;
  FQueryFieldHashes.Free;
  FParentSourceFieldHashes.Free;
  FSourceFieldHashes.Free;
  FParentFieldHashes.Free;
  FFieldHashes.Free;
  inherited Destroy;
end;

procedure TdxExprHighlighter.SetLine(const NewValue: String; LineNumber: Integer
  );
begin
  inherited SetLine(NewValue, LineNumber);
  FLine := PChar(NewValue);
  FLineNumber := LineNumber;
  if FKind = ekFilter then SetBracketsCount;
  FPos := 0;
  Next;
end;

procedure TdxExprHighlighter.Next;
begin
  FTokenPos := FPos;
  FTokenId := etkUnknown;
  case FRange of
    ersExpr: ExprProc;
    ersString: RangeStringProc;
    ersComment: CommentProc;
    ersSourceField: SourceFieldProc;
  end;
end;

function TdxExprHighlighter.GetEol: Boolean;
begin
  Result := FTokenId = etkNull;
end;

function TdxExprHighlighter.GetToken: String;
var
  Len: Integer;
begin
  Len := FPos - FTokenPos;
  Result := '';
  SetString(Result, FLine + FTokenPos, Len);
end;

procedure TdxExprHighlighter.GetTokenEx(out TokenStart: PChar; out
  TokenLength: integer);
begin
  TokenStart := FLine + FTokenPos;
  TokenLength := FPos - FTokenPos;
end;

function TdxExprHighlighter.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

function TdxExprHighlighter.GetTokenKind: integer;
begin
  Result := Ord(FTokenId);
end;

function TdxExprHighlighter.GetDefaultAttribute(Index: integer
  ): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttr;
    SYN_ATTR_IDENTIFIER: Result := FFuncAttr;
    SYN_ATTR_KEYWORD: Result := FFuncAttr;
    SYN_ATTR_STRING: Result := FStringAttr;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttr;
    SYN_ATTR_SYMBOL: Result := FSymbolAttr;
    SYN_ATTR_NUMBER: Result := FNumberAttr;
    else Result := nil;
  end;
end;

function TdxExprHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenId of
    etkUnknown: Result := FUnknownAttr;
    etkField: Result := FFieldAttr;
    etkSourceField: Result := FSourceFieldAttr;
    etkQueryField: Result := FQueryFieldAttr;
    etkFunc: Result := FFuncAttr;
    etkString: Result := FStringAttr;
    etkNumber: Result := FNumberAttr;
    etkSymbol: Result := FSymbolAttr;
    etkComment: Result := FCommentAttr;
    etkSpace: Result := FSpaceAttr;
    etkLabel: Result := FLabelAttr;
    else Result := nil;
  end;
end;

function TdxExprHighlighter.GetRange: Pointer;
begin
  Result:=Pointer(PtrUInt(FRange));
end;

procedure TdxExprHighlighter.SetRange(Value: Pointer);
begin
  FRange := TdxExprRangeState(PtrUInt(Value));
end;

procedure TdxExprHighlighter.ResetRange;
begin
  case FKind of
    ekFilter: FRange := ersSourceField;
    else FRange := ersExpr;
  end;
end;

procedure TdxExprHighlighter.AddField(S: String);
var
  IsParent: Boolean;
begin
  ExcludeFieldPrefix(S, IsParent);
  if IsParent or FFieldAsParentField then
    FParentFieldHashes.Add(Pointer(PtrInt(MakeHash(S))))
  else
    FFieldHashes.Add(Pointer(PtrInt(MakeHash(S))));
end;

procedure TdxExprHighlighter.AddLabel(S: String);
var
  IsParent: Boolean;
begin
  ExcludeFieldPrefix(S, IsParent);
  if IsParent or FFieldAsParentField then
    FParentLabelHashes.Add(Pointer(PtrInt(MakeHash(S))))
  else
    FLabelHashes.Add(Pointer(PtrInt(MakeHash(S))));
end;

procedure TdxExprHighlighter.AddSourceField(S: String);
var
  IsParent: Boolean;
begin
  ExcludeSourceFieldPrefix(S, IsParent);
  if IsParent or FSourceFieldAsParentSourceField then
    FParentSourceFieldHashes.Add(Pointer(PtrInt(MakeHash(S))))
  else
    FSourceFieldHashes.Add(Pointer(PtrInt(MakeHash(S))));
end;

procedure TdxExprHighlighter.AddQueryField(S: String);
begin
  FQueryFieldHashes.Add(Pointer(PtrInt(MakeHash(S))));
end;

procedure TdxExprHighlighter.AddFunc(const S: String);
begin
  FFuncHashes.Add(Pointer(PtrInt(MakeHash(S))));
end;

procedure TdxExprHighlighter.ClearFuncs;
begin
  FFuncHashes.Clear;
end;

procedure TdxExprHighlighter.ClearFields;
begin
  FLabelHashes.Clear;
  FParentLabelHashes.Clear;
  FQueryFieldHashes.Clear;
  FParentSourceFieldHashes.Clear;
  FSourceFieldHashes.Clear;
  FParentFieldHashes.Clear;
  FFieldHashes.Clear;
end;

end.

