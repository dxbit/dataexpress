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

unit crypt;

// Источник: http://www.delphisources.ru/pages/faq/base/encode_sim.html

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base64;

const
  StartKey = 174; // Start default key
  MultKey = 61224; // Mult default key
  AddKey = 17191; // Add default key
  // обязательно смените ключи до использования

function Encrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;

function Decrypt(InString: string; StartKey, MultKey, AddKey: Integer): string;

implementation

{ **** UBPFD *********** by delphibase.endimus.com ****
>> Шифрование строки

Предназначена для простого шифрование строк и паролей, ключ 96 бит, шифрование
симметричное.

Зависимости: UBPFD.decrypt
Автор:       Anatoly Podgoretsky, anatoly@podgoretsky.com, Johvi
Copyright:   (c) Anatoly Podgoretsky, 1996
Дата:        26 апреля 2002 г.
***************************************************** }

function Encrypt(const InString: string; StartKey, MultKey, AddKey: Integer):
  string;
var
  I: Byte;
  // Если поменять тип переменной I на Integer, то будет возможно
  // шифрование текста длиной более 255 символом - VID.
begin
  Result := '';
  for I := 1 to Length(InString) do
  begin
    Result := Result + CHAR(Byte(InString[I]) xor (StartKey shr 8));
    StartKey := (Byte(Result[I]) + StartKey) * MultKey + AddKey;
  end;
  Result := EncodeStringBase64(Result);
end;

{ **** UBPFD *********** by delphibase.endimus.com ****
>> Расшифровка строки

Предназначена для расшифровки строки, ранее зашифрованной фукцией UBPFD.Encrypt

Зависимости: UBPFD.Encrypt
Автор:       Anatoly Podgoretsky, anatoly@podgoretsky.com, Johvi
Copyright:   (c) Anatoly Podgoretsky, 1996
Дата:        26 апреля 2002 г.
***************************************************** }

{$R-}
{$Q-}

function Decrypt(InString: string; StartKey, MultKey, AddKey: Integer): string;
var
  I: Byte;
  // Если поменять тип переменной I на Integer, то будет возможно
  // шифрование текста длиной более 255 символом - VID.
begin
  Result := '';
  if InString = '' then Exit;

  try
	  InString := DecodeStringBase64(InString);
  except
    ;
  end;
  for I := 1 to Length(InString) do
  begin
    Result := Result + CHAR(Byte(InString[I]) xor (StartKey shr 8));
    StartKey := (Byte(InString[I]) + StartKey) * MultKey + AddKey;
  end;
end;
{$R+}
{$Q+}

end.

