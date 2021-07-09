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
Данный модуль был разработан при создании примера WaitWindowExample.
https://github.com/loginov-dmitry/multithread/tree/master/ExWaitWindow

Вы его можете применять в любых проектах!

Основу модуля ParamsUtils составляет структура (record) TParamsRec, которая хранит
именованный (либо неименованный) список параметров. Данная структура необходима
для работы функции DoOperationInThread. Однако Вы можете использовать её как
универсальный способ передачи параметров в функцию, принимающую произвольное количество
параметров различного типа. Это намного лучше, чем передавать
параметры в виде вариантного массива (либо массива вариантов), поскольку обеспечивается
доступ к параметру по имени (а не только по индексу).

  Что лучше? Классический способ передачи параметров в виде массива с произвольным количеством элементов:
    MyFunc(VarArrayOf([s, pr, cnt, v, crt, Now]))
  при этом доступ к элементам массива возможен только по индексу, например:
  sTovarName := Params[0];
  sSumma := Params[1] * Params[2]

  или с использованием структуры TParamsRec:
    MyFunc(TParamsRec.Build(['Tovar', s, 'Price', pr, 'Count', cnt, 'VidOpl', v, 'CardNum', crt, 'SaleTime', Now]))
  при этом доступ к элементам массива возможен по имени, например:
  sTovarName := par.S('Tovar');
  sSumma := par.C('Price') * par.C('Count');

Я считаю, что без сомнения, второй вариант намного более удобный, позволяет упростить
код программы, сделать его более читабельным и снизить вероятность ошибок.

Не используйте TParamsRec для передачи слишком большого количества параметров, т.к.
для доступа к значению параметра используется последовательный поиск строки в массиве
параметров, а это не самый быстрый способ доступа!
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

  TParamsStringArray = array of string;
  TParamsVariantArray = array of Variant;

  PParamsRec = ^TParamsRec;
  TParamsRec = record
    Params: array of TParamDesc;

    {Устанавливает значение заданного параметра. Если необходимо установить значения
     нескольких параметров (с именем), то используйте функцию SetParams}
    procedure SetParam(const ParamName: string; const Value: Variant);

    {Записывает указанные параметры (названия и значения) в массив Params. Каждый
     чётный элемент - имя параметра, нечётный - значение параметра. Пример:
     ParamsRec.SetParams(['Name1', Value1, 'Name2', Value2])}
    procedure SetParams(ParamNamesAndValues: array of Variant);

    {Загружает в Params параметры из вариантного массива. Может быть
     полезным при взаимодействии между модулями с использованием типа Variant, например:
        var V: Variant := VarArrayOf(['Name1', Value1, 'Name2', Value2]);
        ParamsRec.SetParamsFromVariant(V);
     К сожалению, не получается объявить функцию с тем же именем (SetParams), т.к.
     Delphi автоматически преобразует массив вариантов в вариантный массив, а затем
     вызывает версию функции, где параметр объявлен как Variant }
    procedure SetParamsFromVariant(ParamNamesAndValues: Variant);

    {Добавляет значения параметров (без имени) в массив Params. Для доступа к значениям
     параметров без имени необходимо обращаться по индексу с помощью соответствующего
     набора функций}
    procedure AddParamsNoNames(ParamValues: array of Variant);

    {Очищает массив Params}
    procedure Clear;

    {Проверяет, есть ли параметр с указанным именем}
    function HasParam(const ParamName: string): Boolean;
    
    function GetParamIndex(const ParamName: string): Integer;
    // Возвращает тип параметра. Если параметр отсутствует, то возвращает varEmpty
    // Описание типов см. в System.pas (varEmpty, varInteger, varDouble, varDate, varString и т.д.)
    function GetParamType(const ParamName: string): TVarType; overload;

    // Если индекс некорректный, то выбрасывает Exception
    function GetParamType(Idx: Integer): TVarType; overload;

    function ExtractParamNames: TParamsStringArray;
    function ExtractParamValues: TParamsVariantArray;

    { Возвращает список наименований и значений параметров в виде вариантного массива.
      По сути, выполняет сериализацию параметров в вариантный массив.
      Это может быть полезным при организации взаимодействия между модулями.
      Например, один модуль записывает наименования и значения параметров в TParamsRec,
      затем извлекает их в виде варианта с помощью метода ExtractAsVarArray,
      затем передаёт во второй модуль.
      Второй модуль принимает параметры в виде Variant и выполняет их десериализацию
      с помощью метода SetParamsFromVariant}
    function ExtractAsVarArray: Variant;

    function Count: Integer;

    {Возвращает значение параметра (в формате Variant) по его имени.
     Внимание! Не рекомендуется иметь дело в типом Variant. Вместо этого используйте
     типизированные методы: I, U, D, C, S, B, DT}
    function GetValue(const ParamName: string): Variant; overload;
    function GetValue(const ParamName: string; DefValue: Variant): Variant; overload;

    // Короткие методы для извлечения значения параметра по его имени
    // Группа методов без "DefValue". Если параметр отсутствует, то будет выдано исключение
    function I(const ParamName: string): Int64; overload;
    function U(const ParamName: string): Cardinal; overload;
    function D(const ParamName: string): Double; overload;
    function C(const ParamName: string): Currency; overload;
    function S(const ParamName: string): string; overload;
    function B(const ParamName: string): Boolean; overload;
    function DT(const ParamName: string): TDateTime; overload;

    // Группа методов с "DefValue". Если параметр отсутствует, то вернёт DefValue
    function I(const ParamName: string; DefValue: Int64): Int64; overload;
    function U(const ParamName: string; DefValue: Cardinal): Cardinal; overload;
    function D(const ParamName: string; DefValue: Double): Double; overload;
    function C(const ParamName: string; DefValue: Currency): Currency; overload;
    function S(const ParamName: string; DefValue: string): string; overload;
    function B(const ParamName: string; DefValue: Boolean): Boolean; overload;
    function DT(const ParamName: string; DefValue: TDateTime): TDateTime; overload;

    function GetValue(Idx: Integer): Variant; overload;

    // Короткие методы для извлечения значения параметра по его интексу
    // Внимание! Параметр должен существовать! Если парамет с указанным индексом
    // отсутствует, то будет сгенерировано исключение!
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
    class function BuildFromVariant(ParamNamesAndValues: Variant): TParamsRec; static;
    class function BuildNoNames(ParamValues: array of Variant): TParamsRec; static;
    class function ParamNamesToString(ParamNames: array of string; DelimChar: Char = ','): string; static;
  end;

var
  // Глобальная переменная, которую следует использовать, если передача переметров не требуется
  ParamsEmpty: TParamsRec;

implementation

{ TParamsRec }

procedure TParamsRec.SetParams(ParamNamesAndValues: array of Variant);
const
  ErrPrefix = 'TParamsRec.SetParams';
var
  I, CurIdx, Idx, MaxLen: Integer;
  NewParams: array of TParamDesc;
begin
  if Odd(Length(ParamNamesAndValues)) then
    raise Exception.Create(ErrPrefix + ': Число элементов должно быть чётным');

  SetLength(NewParams, Length(ParamNamesAndValues) div 2);
  CurIdx := 0;
  for I := 0 to High(ParamNamesAndValues) do
  begin
    if not Odd(I) then // Если Чётное (0, 2, 4, ...)
    begin
      if not VarIsStr(ParamNamesAndValues[I]) then
        raise Exception.CreateFmt('%s: Элемент %d массива ParamNamesAndValues должен быть строкой', [ErrPrefix, I]);
      if ParamNamesAndValues[I] = '' then
        raise Exception.CreateFmt('%s: Название элемена %d массива ParamNamesAndValues не указано', [ErrPrefix, I]);
      NewParams[CurIdx].ParamName := ParamNamesAndValues[I];
    end else // Если нечётное (1, 3, 5, ...)
    begin
      NewParams[CurIdx].ParamValue := ParamNamesAndValues[I];
      Inc(CurIdx);
    end;
  end;

  CurIdx := High(Params) + 1;
  MaxLen := Length(Params) + Length(NewParams);
  SetLength(Params, MaxLen); // Устанавливаем сначала максимальную длину
  for I := 0 to High(NewParams) do
  begin
    Idx := GetParamIndex(NewParams[I].ParamName);
    if Idx >= 0 then
      Params[Idx] := NewParams[I]
    else
    begin
      Params[CurIdx] := NewParams[I];
      Inc(CurIdx);
    end;
  end;
  if CurIdx <> MaxLen then
    SetLength(Params, CurIdx);
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

function TParamsRec.B(const ParamName: string; DefValue: Boolean): Boolean;
begin
  Result := GetValue(ParamName, DefValue);
end;

class function TParamsRec.Build(
  ParamNamesAndValues: array of Variant): TParamsRec;
begin
  try
    Result.Clear;
    Result.SetParams(ParamNamesAndValues);
  except
    on E: Exception do
      raise Exception.Create('TParamsRec.Build: ' + E.Message);
  end;
end;

class function TParamsRec.BuildFromVariant(ParamNamesAndValues: Variant): TParamsRec;
begin
  Result.Clear;
  Result.SetParamsFromVariant(ParamNamesAndValues);
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

function TParamsRec.C(const ParamName: string; DefValue: Currency): Currency;
begin
  Result := GetValue(ParamName, DefValue);
end;

procedure TParamsRec.Clear;
begin
  Params := nil;
end;

function TParamsRec.Count: Integer;
begin
  Result := Length(Params);
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

function TParamsRec.GetParamType(Idx: Integer): TVarType;
const
  ErrPrefix = 'TParamsRec.GetParamType (by index)';
begin
  if (Idx < 0) or (Idx > High(Params)) then
    raise Exception.CreateFmt('%s: указан недопустимый индекс параметра (%d)', [ErrPrefix, Idx]);
  Result := VarType(Params[Idx].ParamValue);
end;

function TParamsRec.GetParamType(const ParamName: string): TVarType;
var
  Idx: Integer;
begin
  Idx := GetParamIndex(ParamName);
  if Idx >= 0 then
    Result := VarType(Params[Idx].ParamValue)
  else
    Result := varEmpty;
end;

function TParamsRec.GetValue(Idx: Integer): Variant;
const
  ErrPrefix = 'TParamsRec.GetValue (by index)';
begin
  if (Idx < 0) or (Idx > High(Params)) then
    raise Exception.CreateFmt('%s: указан недопустимый индекс параметра (%d)', [ErrPrefix, Idx]);
  Result := Params[Idx].ParamValue;
end;

function TParamsRec.GetValue(const ParamName: string;
  DefValue: Variant): Variant;
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
    Result := DefValue;
end;

function TParamsRec.HasParam(const ParamName: string): Boolean;
begin
  Result := GetParamIndex(ParamName) >= 0;
end;

function TParamsRec.I(const ParamName: string; DefValue: Int64): Int64;
begin
  Result := GetValue(ParamName, DefValue);
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

function TParamsRec.D(const ParamName: string; DefValue: Double): Double;
begin
  Result := GetValue(ParamName, DefValue);
end;

function TParamsRec.DT(Idx: Integer): TDateTime;
begin
  Result := GetValue(Idx);
end;

function TParamsRec.DT(const ParamName: string; DefValue: TDateTime): TDateTime;
begin
  Result := GetValue(ParamName, DefValue);
end;

function TParamsRec.ExtractAsVarArray: Variant;
var
  Idx, I: Integer;
begin
  if Count = 0 then
    Result := VarArrayOf([])
  else
  begin
    Result := VarArrayCreate([0, Count * 2 - 1], varVariant);
    Idx := 0;
    for I := 0 to High(Params) do
    begin
      Result[Idx] := Params[I].ParamName;
      Inc(Idx);
      Result[Idx] := Params[I].ParamValue;
      Inc(Idx);
    end;
  end;
end;

function TParamsRec.ExtractParamNames: TParamsStringArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Params));
  for I := 0 to High(Params) do
    Result[I] := Params[I].ParamName;
end;

function TParamsRec.ExtractParamValues: TParamsVariantArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Params));
  for I := 0 to High(Params) do
    Result[I] := Params[I].ParamValue;
end;

function TParamsRec.S(Idx: Integer): string;
begin
  Result := GetValue(Idx);
end;

function TParamsRec.S(const ParamName: string; DefValue: string): string;
begin
  Result := GetValue(ParamName, DefValue);
end;

procedure TParamsRec.SetParam(const ParamName: string; const Value: Variant);
begin
  SetParams([ParamName, Value]);
end;

procedure TParamsRec.SetParamsFromVariant(ParamNamesAndValues: Variant);
var
  AParams: array of Variant;
  I: Integer;
begin
  if not VarIsArray(ParamNamesAndValues) then
    raise Exception.Create('TParamsRec.SetParams: входящий параметр должен быть вариантным массивом!');
  SetLength(AParams, VarArrayHighBound(ParamNamesAndValues, 1) + 1);
  for I := 0 to VarArrayHighBound(ParamNamesAndValues, 1) do
    AParams[I] := ParamNamesAndValues[I];
  SetParams(AParams);
end;

class function TParamsRec.ParamNamesToString(ParamNames: array of string;
  DelimChar: Char): string;
var
  s: string;  
begin
  Result := '';
  for s in ParamNames do
  begin
    if Result <> '' then
      Result := Result + DelimChar;
    Result := Result + s;
  end;
end;

function TParamsRec.U(const ParamName: string; DefValue: Cardinal): Cardinal;
begin
  Result := GetValue(ParamName, DefValue);
end;

function TParamsRec.U(Idx: Integer): Cardinal;
begin
  Result := GetValue(Idx);
end;

end.
