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

unit TimeIntervals;

interface

uses
  Windows, SysUtils, Classes, DateUtils, StrUtils;

type
  { TTimeInterval is designed to accurately measure time,
     passed after the start of measurement.
     Attention! Microsecond measurements
     performed with an error of several microseconds (despite
     using the variable PerformanceIgnoredTicks }
  TTimeInterval = record
  private
    FStartCounter: Int64;
    FIsRunning: Boolean;
    FElapsedTicks: Int64;
  public
    // Starts measuring
    procedure Start;

    // For supporting inline variables (var ti := TTimeInterval.StartNew)
    class function StartNew: TTimeInterval; static;

    // Finishes measuring. Updates the FElapsedTicks field. After the Stop method
    // is called, the ElapsedXXX methods will return the value stored in FElapsedTicks
    procedure Stop;

    // Returns the number of milliseconds after the start of measurement
    function ElapsedMilliseconds(AStartNew: Boolean = False): Int64;

    // Returns the number of microseconds after the start of measurements
    function ElapsedMicroseconds(AStartNew: Boolean = False): Int64;

    // Returns the number of seconds after the start of measurement
    function ElapsedSeconds(AStartNew: Boolean = False): Double;

    // Returns the number of ticks from the beginning of measurements
    function ElapsedTicks(AStartNew: Boolean = False): Int64;

    // Returns the time (TDateTime) elapsed since the start of measurement
    function ElapsedTime(AStartNew: Boolean = False): TDateTime;

    // Returns True if measurements are running.
    property IsRunning: Boolean read FIsRunning;
  end;

  TTimeIntervalEvent = record
    // Date / time of the beginning and end of the event (according to the standard system timer)
    BegDateTime: TDateTime;
    EndDateTime: TDateTime;
    EventName: string;   // Event name
    BegCounter: Int64;   // Counter value at the beginning of the event
    EndCounter: Int64;   // Counter value at the end of the event

    function ElapsedTicks: Int64;
    function ElapsedMilliseconds: Int64;
    function ElapsedMicroseconds: Int64;
    function ElapsedSeconds: Double;
    procedure Start(AName: string);
    procedure Stop;
  end;

  TTimeIntervalGetEventsOption = (eoWriteStartTime, eoWriteStopTime, eoWriteAllTime,
    eoWriteBegTime, eoWriteEndTime, eoUseMicroSec, eoWriteFromStart, eoWriteDate);
  TTimeIntervalGetEventsOptions = set of TTimeIntervalGetEventsOption;

  // TTimeIntervalEvents - record for event duration logging
  TTimeIntervalEvents = record
    Events: array of TTimeIntervalEvent;

    // Starts measurement of a new event.
    procedure StartEvent(EventName: string);

    // Stops event measurement. You can indicate EventName for
    // illustration purposes only.
    procedure StopEvent(EventName: string = '');

    // Returns event duration measurement information
    function GetEventsAsString(EvOp: TTimeIntervalGetEventsOptions): string;
  end;

var
  PerformanceFrequency: Int64;
  UsePerformanceCounter: Boolean;

  // The number of ticks that must be ignored to more accurately measure time intervals
  PerformanceIgnoredTicks: Int64 = 0;

implementation

var
  GetTickCount64Ref: function: UInt64;
  GetTickCount64NotSupported: Boolean;
function InternalGetTickCount64: UInt64;
begin
  if (@GetTickCount64Ref = nil) and (not GetTickCount64NotSupported) then // Функция не реализована в WinXP
  begin
    @GetTickCount64Ref := GetProcAddress(GetModuleHandle('kernel32.dll'), 'GetTickCount64');
    if @GetTickCount64Ref = nil then
      GetTickCount64NotSupported := True;
  end;

  if Assigned(GetTickCount64Ref) then
    Result := GetTickCount64Ref
  else
    Result := GetTickCount;
end;

{ TTimeInterval }

function TTimeInterval.ElapsedMicroseconds(AStartNew: Boolean = False): Int64;
begin
  Result := Round(ElapsedSeconds(AStartNew) * 1000000);
end;

function TTimeInterval.ElapsedMilliseconds(AStartNew: Boolean = False): Int64;
begin
  Result := Round(ElapsedSeconds(AStartNew) * 1000);
end;

function TTimeInterval.ElapsedSeconds(AStartNew: Boolean = False): Double;
begin
  Result := ElapsedTicks(AStartNew) / PerformanceFrequency;
end;

function TTimeInterval.ElapsedTicks(AStartNew: Boolean = False): Int64;
var
  ACounter: Int64;
begin
  if FIsRunning then
  begin // If measurements are started, then return the current value
    if UsePerformanceCounter then
      QueryPerformanceCounter(ACounter)
    else
      ACounter := InternalGetTickCount64;
    Result := ACounter - FStartCounter - PerformanceIgnoredTicks;
    if Result < 0 then
      Result := 0;
  end else
  begin // Measurements stopped - return the value at the time of stop
    Result := FElapsedTicks
  end;

  if AStartNew then
    Start;
end;

function TTimeInterval.ElapsedTime(AStartNew: Boolean): TDateTime;
begin
  Result := IncMilliSecond(0, ElapsedMilliseconds(AStartNew));
end;

procedure TTimeInterval.Start;
begin
  FIsRunning    := True;
  FElapsedTicks := 0;
  // Request a counter at the very end of the method
  if UsePerformanceCounter then
    QueryPerformanceCounter(FStartCounter)
  else
    FStartCounter := InternalGetTickCount64;
end;

class function TTimeInterval.StartNew: TTimeInterval;
begin
  Result.Start;
end;

procedure TTimeInterval.Stop;
var
  ACounter: Int64;
begin
  // Request a counter at the very beginning of the method
  if UsePerformanceCounter then
    QueryPerformanceCounter(ACounter)
  else
    ACounter := InternalGetTickCount64;

  FIsRunning := False;
  FElapsedTicks := ACounter - FStartCounter - PerformanceIgnoredTicks;
  if FElapsedTicks < 0 then
    FElapsedTicks := 0;
end;

{ TTimeIntervalEvent }

function TTimeIntervalEvent.ElapsedMilliseconds: Int64;
begin
  Result := Round(ElapsedSeconds * 1000);
end;

function TTimeIntervalEvent.ElapsedMicroseconds: Int64;
begin
  Result := Round(ElapsedSeconds * 1000000);
end;

function TTimeIntervalEvent.ElapsedSeconds: Double;
begin
  Result := ElapsedTicks / PerformanceFrequency;
end;

function TTimeIntervalEvent.ElapsedTicks: Int64;
begin
  Result := EndCounter - BegCounter;
end;

procedure TTimeIntervalEvent.Start(AName: string);
begin
  BegDateTime := Now;
  EventName   := AName;
  if UsePerformanceCounter then
    QueryPerformanceCounter(BegCounter)
  else
    BegCounter := InternalGetTickCount64;
end;

procedure TTimeIntervalEvent.Stop;
begin
  if UsePerformanceCounter then
    QueryPerformanceCounter(EndCounter)
  else
    EndCounter := InternalGetTickCount64;
  EndCounter := EndCounter - PerformanceIgnoredTicks;
  if EndCounter < BegCounter then
    EndCounter := BegCounter;
  EndDateTime := Now;
end;

{ TTimeIntervalEvents }

function TTimeIntervalEvents.GetEventsAsString(
  EvOp: TTimeIntervalGetEventsOptions): string;
var
  Ev, EvFirst: TTimeIntervalEvent;
  s, sTimeMask: string;
  I: Integer;
  AllTime, AllTicks: Int64;
  Sec: Double;
begin
  Result := '';
  if Length(Events) = 0 then
  begin
    Result := 'Events array is empty';
    Exit;
  end;

  if eoWriteDate in EvOp then
    sTimeMask := 'dd.mm.yy hh:nn:ss.zzz'
  else
    sTimeMask := 'hh:nn:ss.zzz';

  EvFirst := Events[0];
  if eoWriteStartTime in EvOp then
  begin
    Result := 'StartTime: ' + FormatDateTime(sTimeMask, EvFirst.BegDateTime);
    if eoWriteAllTime in EvOp then
      Result := Result + '; ';
  end;

  if eoWriteAllTime in EvOp then
  begin
    Result := Result + 'AllTime: ';
    AllTicks := 0;
    for Ev in Events do
      AllTicks := AllTicks + Ev.ElapsedTicks;
    Sec := AllTicks / PerformanceFrequency;
    if eoUseMicroSec in EvOp then
      AllTime := Round(Sec * 1000000)
    else
      AllTime := Round(Sec * 1000);
    Result := Result + IntToStr(AllTime) + IfThen(eoUseMicroSec in EvOp, ' us', ' ms') + '; ';
  end;

  for I := 0 to High(Events) do
  begin
    Ev := Events[I];
    Result := Result + Ev.EventName + ':[';

    if eoUseMicroSec in EvOp then
      s := IntToStr(Ev.ElapsedMicroseconds) + ' us'
    else
      s := IntToStr(Ev.ElapsedMilliseconds) + ' ms';

    if eoWriteBegTime in EvOp then
      s := s + '; BegTime: ' + FormatDateTime(sTimeMask, Ev.BegDateTime);
    if eoWriteEndTime in EvOp then
      s := s + '; EndTime: ' + FormatDateTime(sTimeMask, Ev.EndDateTime);

    if eoWriteFromStart in EvOp then
    begin
      s := s + '; FromStart: ';
      AllTicks := Ev.EndCounter - EvFirst.BegCounter;
      Sec := AllTicks / PerformanceFrequency;
      if eoUseMicroSec in EvOp then
        AllTime := Round(Sec * 1000000)
      else
        AllTime := Round(Sec * 1000);

      s := s + IntToStr(AllTime) + IfThen(eoUseMicroSec in EvOp, ' us', ' ms');
    end;
    Result := Result + s + ']';
    if I < High(Events) then
      Result := Result + '; ';
  end;
end;

procedure TTimeIntervalEvents.StartEvent(EventName: string);
var
  Cnt: Integer;
begin
  Cnt := Length(Events);
  if (Cnt > 0) and (Events[Cnt - 1].EndCounter = 0) then
    StopEvent();
  SetLength(Events, Cnt + 1);
  Events[Cnt].Start(EventName);
end;

procedure TTimeIntervalEvents.StopEvent(EventName: string = '');
var
  Idx: Integer;
begin
  Idx := High(Events);
  if Idx >= 0 then
    Events[Idx].Stop;
end;

procedure CalcIgnoredPerformanceTicks;
var
  p1, p2: Int64;
begin
  QueryPerformanceCounter(p1);
  QueryPerformanceCounter(p2);
  PerformanceIgnoredTicks := p2 - p1;
  // If you do not need adjustment, then just assign:
  // PerformanceIgnoredTicks := 0
end;

initialization
  // We get the frequency of the high-frequency timer
  UsePerformanceCounter := QueryPerformanceFrequency(PerformanceFrequency);
  if UsePerformanceCounter then
    CalcIgnoredPerformanceTicks
  else
    PerformanceFrequency := 1000; // impossible condition ???
end.