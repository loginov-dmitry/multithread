{
Copyright (c) 2021, Loginov Dmitry Sergeevich
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

{
Основу модуля ParamsUtils составляет структура (record) TParamsRec, которая хранит
именованный (либо неименованный) список параметров. Данная структура необходима
для работы функции DoOperationInThread. Однако Вы можете использовать её как
универсальный способ передачи параметров в функцию, принимающую произвольное количество
параметров различного типа. Это намного лучше, чем передавать
параметры в виде вариантного массива (либо массива вариантов), поскольку обеспечивается
доступ к параметру по имени (а не только по индексу).
Конечно, возможности TParamsRec намного скромнее, чем SuperObject, которую также
удобно использовать для передачи произвольного списка параметров в функцию, однако
вся реализация TParamsRec занимает всего лишь несколько сотен строк кода.
Не используйте TParamsRec для передачи слишком большого количества параметров, т.к.
для доступа к значению параметра используется последовательный поиск строки в массиве
параметров, а это не самый быстрый способ доступа!
Примеры использования структуры TParamsRec смотрите в примере ExWaitWindow (проект WaitWindowExample).
}
  
unit ParamsUtils;

interface

uses
  SysUtils, Classes, Variants;

type
  TParamDesc = record
    ParamName: string;
    ParamValue: Variant;
  end;

  PParamsRec = ^TParamsRec;
  TParamsRec = record
    Params: array of TParamDesc;

    procedure AddParams(ParamNamesAndValues: array of Variant);
    procedure AddParamsNoNames(ParamValues: array of Variant);
    procedure SetParam(const ParamName: string; Value: Variant);

    procedure Clear;

    function HasParam(const ParamName: string): Boolean;
    function GetParamIndex(const ParamName: string): Integer;

    function GetValue(const ParamName: string): Variant; overload;

    // Короткие методы для извлечения значения параметра по его имени
    function I(const ParamName: string): Int64; overload;
    function U(const ParamName: string): Cardinal; overload;
    function D(const ParamName: string): Double; overload;
    function C(const ParamName: string): Currency; overload;
    function S(const ParamName: string): string; overload;
    function B(const ParamName: string): Boolean; overload;
    function DT(const ParamName: string): TDateTime; overload;

    function GetValue(Idx: Integer): Variant; overload;

    // Короткие методы для извлечения значения параметра по его интексу
    function I(Idx: Integer): Int64; overload;
    function U(Idx: Integer): Cardinal; overload;
    function D(Idx: Integer): Double; overload;
    function C(Idx: Integer): Currency; overload;
    function S(Idx: Integer): string; overload;
    function B(Idx: Integer): Boolean; overload;
    function DT(Idx: Integer): TDateTime; overload;

    // Методы для передачи параметров в функцию DoOperationInThread без объявления
    // переменной TParamsRec
    class function Build(ParamNamesAndValues: array of Variant): TParamsRec; static;
    class function BuildNoNames(ParamValues: array of Variant): TParamsRec; static;
  end;

var
  // Глобальная переменная, которую следует использовать, если передача переметров не требуется
  ParamsEmpty: TParamsRec;

implementation

{ TParamsRec }

procedure TParamsRec.AddParams(ParamNamesAndValues: array of Variant);
const
  ErrPrefix = 'TParamsRec.AddParams';
var  
  I, CurIdx: Integer;
begin
  if Odd(Length(ParamNamesAndValues)) then
    raise Exception.Create(ErrPrefix + ': Число элементов должно быть чётным');
  CurIdx := High(Params) + 1;
  SetLength(Params, Length(Params) + Length(ParamNamesAndValues) div 2);

  for I := 0 to High(ParamNamesAndValues) do
  begin
    if not Odd(I) then // Если Чётное (0, 2, 4, ...)
    begin
      if not VarIsStr(ParamNamesAndValues[I]) then
        raise Exception.CreateFmt('%s: Элемент %d массива ParamNamesAndValues должен быть строкой', [ErrPrefix, I]);
      Params[CurIdx].ParamName := ParamNamesAndValues[I];
    end else // Если нечётное (1, 3, 5, ...)
    begin
      Params[CurIdx].ParamValue := ParamNamesAndValues[I];
      Inc(CurIdx);
    end;
  end;
end;

procedure TParamsRec.AddParamsNoNames(ParamValues: array of Variant);
var
  I, CurIdx: Integer;
begin
  CurIdx := High(Params);
  SetLength(Params, Length(Params) + Length(ParamValues));
  for I := 0 to High(ParamValues) do
  begin
    Inc(CurIdx);
    Params[CurIdx].ParamValue := ParamValues[I];
  end;
end;

function TParamsRec.B(const ParamName: string): Boolean;
begin
  Result := GetValue(ParamName);
end;

function TParamsRec.B(Idx: Integer): Boolean;
begin
  Result := GetValue(Idx);
end;

class function TParamsRec.Build(
  ParamNamesAndValues: array of Variant): TParamsRec;
begin
  try
    Result.Clear;
    Result.AddParams(ParamNamesAndValues);
  except
    on E: Exception do
      raise Exception.Create('TParamsRec.Build: ' + E.Message);
  end;
end;

class function TParamsRec.BuildNoNames(
  ParamValues: array of Variant): TParamsRec;
begin
  Result.Clear;
  Result.AddParamsNoNames(ParamValues);
end;

function TParamsRec.C(const ParamName: string): Currency;
begin
  Result := GetValue(ParamName);
end;

function TParamsRec.C(Idx: Integer): Currency;
begin
  Result := GetValue(Idx);
end;

procedure TParamsRec.Clear;
begin
  Params := nil;
end;

function TParamsRec.D(const ParamName: string): Double;
begin
  Result := GetValue(ParamName);
end;

function TParamsRec.DT(const ParamName: string): TDateTime;
begin
  Result := GetValue(ParamName);
end;

function TParamsRec.GetParamIndex(const ParamName: string): Integer;
var
  I: Integer;
begin
  if ParamName = '' then
    raise Exception.Create('TParamsRec.GetParamIndex: не указано имя параметра');

  Result := -1;
  for I := 0 to High(Params) do
    if Params[I].ParamName = ParamName then
    begin
      Result := I;
      Exit;
    end;
end;

function TParamsRec.GetValue(Idx: Integer): Variant;
const
  ErrPrefix = 'TParamsRec.GetValue (by index)';
begin
  if (Idx < 0) or (Idx > High(Params)) then
    raise Exception.CreateFmt('%s: указан недопустимый индекс параметра (%d)', [ErrPrefix, Idx]);
  Result := Params[Idx].ParamValue;
end;

function TParamsRec.HasParam(const ParamName: string): Boolean;
begin
  Result := GetParamIndex(ParamName) >= 0;
end;

function TParamsRec.I(Idx: Integer): Int64;
begin
  Result := GetValue(Idx);
end;

function TParamsRec.I(const ParamName: string): Int64;
begin
  Result := GetValue(ParamName);
end;

function TParamsRec.S(const ParamName: string): string;
begin
  Result := GetValue(ParamName);
end;

function TParamsRec.U(const ParamName: string): Cardinal;
begin
  Result := GetValue(ParamName);
end;

function TParamsRec.GetValue(const ParamName: string): Variant;
const
  ErrPrefix = 'TParamsRec.GetValue';
var
  Idx: Integer;
begin
  if ParamName = '' then
    raise Exception.CreateFmt('%s: не указано имя параметра', [ErrPrefix]);

  Idx := GetParamIndex(ParamName);
  if Idx >= 0 then
    Result := Params[Idx].ParamValue
  else
    raise Exception.CreateFmt('%s: не удалось найти параметр "%s"', [ErrPrefix, ParamName]);
end;

function TParamsRec.D(Idx: Integer): Double;
begin
  Result := GetValue(Idx);
end;

function TParamsRec.DT(Idx: Integer): TDateTime;
begin
  Result := GetValue(Idx);
end;

function TParamsRec.S(Idx: Integer): string;
begin
  Result := GetValue(Idx);
end;

procedure TParamsRec.SetParam(const ParamName: string; Value: Variant);
var
  Idx: Integer;
begin
  Idx := GetParamIndex(ParamName);
  if Idx >= 0 then
    Params[Idx].ParamValue := Value
  else
    AddParams([ParamName, Value]);
end;

function TParamsRec.U(Idx: Integer): Cardinal;
begin
  Result := GetValue(Idx);
end;

end.
