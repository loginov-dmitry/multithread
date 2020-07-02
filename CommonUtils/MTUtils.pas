{
Copyright (c) 2020, Loginov Dmitry Sergeevich
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

unit MTUtils;

interface

uses
  Windows, SysUtils, Classes, Contnrs, SyncObjs, DateUtils, StrUtils, 
  Math, TimeIntervals;

type
  TThreadAccessTerminated = class(TThread);

  // Подсказка самому себе, где находится TObjectList
  TObjectListHelp = class(Contnrs.TObjectList);

// Простой пример организации паузы в работе потока с контролем свойства Terminate
procedure ThreadWaitTimeout(AThread: TThread; ATimeout: Integer);

// Эмуляция полезной работы
procedure EmulateUsefullWork(WorkTime: Integer);

procedure ThreadShowMessageFmt(Msg: string; Args: array of const);
procedure ThreadShowMessage(Msg: string);

var
  StringProtectSection: TCriticalSection;
  ThreadWaitTimeoutSleepTime: Cardinal = 20;

implementation

procedure ThreadShowMessage(Msg: string);
begin
  Windows.MessageBox(0, PChar(Msg), '', MB_OK);
end;

procedure ThreadShowMessageFmt(Msg: string; Args: array of const);
begin
  ThreadShowMessage(Format(Msg, Args));
end;

procedure EmulateUsefullWork(WorkTime: Integer);
begin
  Sleep(WorkTime);
end;

// Реализация, основанная на функции GetTickCount
{procedure ThreadWaitTimeout(AThread: TThread; ATimeout: Integer);
var
  StartTime, Diff, tc: Cardinal;
  T: TThreadAccessTerminated;
begin
  // Получаем доступ к protected-свойству Terminated
  T := TThreadAccessTerminated(AThread);
  // Если поток нужно завершить, то сразу выходим из цикла
  if T.Terminated then Exit;

  // Запоминаем текущее время (в миллисекундах от включения компьютера)
  StartTime := GetTickCount;
  while True do
  begin
    tc := GetTickCount;

    // Прерываем ожидание, если функция GetTickCount начала новый круг
    // иначе наша функция ожидания может зависнуть
    if (tc < StartTime) then Exit;

    // Прерываем ожидание, если превысили указанный таймаут
    Diff := tc - StartTime;
    if (Diff >= ATimeout) or T.Terminated then
      Exit;

    // Замораживаем поток примерно на 20 мс
    Sleep(20);
  end;
end; }

// Реализация, основанная на TPerformance
procedure ThreadWaitTimeout(AThread: TThread; ATimeout: Integer);
var
  p: TTimeInterval;
  T: TThreadAccessTerminated;
begin
  // Получаем доступ к protected-свойству Terminated
  T := TThreadAccessTerminated(AThread);
  // Если поток нужно завершить, то сразу выходим из цикла
  if T.Terminated then Exit;

  p.Start; // Начинаем замер времени
  while True do
  begin
    if T.Terminated or (p.ElapsedMilliseconds >= ATimeout) then
      Exit;
    // Замораживаем поток примерно на 20 мс
    Sleep(ThreadWaitTimeoutSleepTime);
  end;
end;

initialization
  // Критическая секция для защиты строк от
  // одновременного доступа из разных потоков
  StringProtectSection := TCriticalSection.Create;

finalization
  StringProtectSection.Free;
end.
