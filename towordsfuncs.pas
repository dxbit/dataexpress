unit ToWordsFuncs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ToWords(Money: Currency; Full: Boolean): String;

implementation

function ToWords(Money: Currency; Full: Boolean): String;
const
  Hundreds: array [0..9] of String = ('', 'сто ', 'двести ', 'триста ', 'четыреста ',
    'пятьсот ', 'шестьсот ', 'семьсот ', 'восемьсот ', 'девятьсот ');
  Tens: array [0..19] of String = ('', '', 'двадцать ', 'тридцать ',
    'сорок ', 'пятьдесят ', 'шестьдесят ', 'семьдесят ', 'восемьдесят ',
    'девяносто ', 'десять ', 'одиннадцать ', 'двенадцать ', 'тринадцать ', 'четырнадцать ',
    'пятнадцать ', 'шестнадцать ', 'семнадцать ', 'восемнадцать ',
    'девятнадцать ');
  Units: array [0..9] of String = ('', 'один ', 'два ', 'три ', 'четыре ', 'пять ',
    'шесть ', 'семь ', 'восемь ', 'девять ');
  ThousandUnits: array [0..9] of String = ('', 'одна ', 'две ', 'три ', 'четыре ',
    'пять ', 'шесть ', 'семь ', 'восемь ', 'девять ');
  BillionWords: array [1..3] of String = ('миллиард ', 'миллиарда ', 'миллиардов ');
  MillionWords: array [1..3] of String = ('миллион ', 'миллиона ', 'миллионов ');
  ThousandWords: array [1..3] of String = ('тысяча ', 'тысячи ', 'тысяч ');
  RurWords: array [1..3] of String = ('рубль', 'рубля', 'рублей');
  PennyWords: array [1..3] of String = ('копейка', 'копейки', 'копеек');
var
  Idx, RIdx: Integer;     // Указатель на массивы склонений
  D: Integer;       // Текущий разряд
  P: Integer;       // Позиция в строке
  Rur, Penny, Str: String;
  Digit: Integer;

  procedure SkipZeros;
  begin
    while D > 0 do
      if Rur[P] = '0' then
      begin
        Inc(P); Dec(D);
      end
      else
        Break;
  end;

  procedure ParseUnits;
  begin
    if D in [10, 7, 4, 1] then
    begin
      Digit := StrToInt(Rur[P]);
      if Digit = 1 then Idx := 1
      else if Digit in [2..4] then Idx := 2
      else Idx := 3;
      if D = 4 then
        Str := Str + ThousandUnits[Digit]
      else
        Str := Str + Units[Digit];
      if D in [10, 7] then
        RIdx := 3
      else
        RIdx := Idx;
      Inc(P);
      Dec(D);
    end;
  end;

  procedure ParseTens;
  begin
    if D in [11, 8, 5, 2] then
    begin
      Idx := 3; RIdx := 3;
      if Rur[P] = '1' then
      begin
        Digit := StrToInt(Rur[P] + Rur[P + 1]);
        P := P + 2;
        D := D - 2;
      end
      else
      begin
        Digit := StrToInt(Rur[P]);
        Inc(P); Dec(D);
      end;
      Str := Str + Tens[Digit];
    end;
    ParseUnits;
  end;

  procedure ParseHundreds;
  begin
    if D in [12, 9, 6, 3] then
    begin
      Idx := 3; RIdx := 3;
      Digit := StrToInt(Rur[P]);
      Str := Str + Hundreds[Digit];
      Inc(P); Dec(D);
    end;
    ParseTens;
  end;

  procedure ParseThousands;
  begin
    SkipZeros;
    if not (D in [4..6]) then Exit;
    ParseHundreds;
    Str := Str + ThousandWords[Idx];
  end;

  procedure ParseMillions;
  begin
    SkipZeros;
    if not (D in [7..9]) then Exit;
    ParseHundreds;
    Str := Str + MillionWords[Idx];
  end;

  procedure ParseBillions;
  begin
    SkipZeros;
    if not (D in [10..12]) then Exit;
    ParseHundreds;
    Str := Str + BillionWords[Idx];
  end;

  procedure ParseRur;
  begin
    if (D = 1) and (Rur[P] = '0') then
    begin
      if Full then
        Str := 'Ноль рублей'
      else
        Str := 'Ноль';
      Exit;
    end;

    ParseBillions;
    ParseMillions;
    ParseThousands;
    ParseHundreds;
    if Full then
      Str := Str + RurWords[RIdx];
  end;

  procedure ParsePenny;
  begin
    if Length(Penny) = 1 then
      Penny := '0' + Penny;
    if Penny[1] = '1' then
      Idx := 3
    else if Penny[2] = '1' then
      Idx := 1
    else if Penny[2] in ['2', '3', '4'] then
      Idx := 2
    else
      Idx := 3;
    Str := Str + ' ' + Penny + ' ' + PennyWords[Idx];
  end;

begin
  Rur := IntToStr(Trunc(Abs(Money)));
  if Full then
    Penny := IntToStr(Round((Frac(Money) + 1) * 100) mod 100);
  Str := '';
  P := 1;
  D := Length(Rur);
  if (D = 0) or (D > 12) then Exit('');

  ParseRur;
  if Full then
    ParsePenny;
  Result := Str;
end;

{function ToWords(Money: Currency; Full: Boolean): String;
var
  L: String;
begin
  L := AppConfig.Language;
  if L = 'ru' then Result := ToWordsRu(Money, Full)
  else Result := 'Unsupported';
end;    }

end.

