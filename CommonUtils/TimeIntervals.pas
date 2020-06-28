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
  Windows, SysUtils, Classes, Contnrs, SyncObjs, DateUtils, StrUtils, Math;

type
  { TPerformance is designed to accurately measure time,
     passed after the start of measurement.
     Attention! Microsecond measurements
     performed with an error of several microseconds (despite
     using the variable PerformanceIgnoredTicks }
  TPerformance = record
  private
    FStartCounter: Int64;
    FIsRunning: Boolean;
    FElapsedTicks: Int64;
  public
    // Starts measuring
    procedure Start;

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

  TPerformanceEvent = record
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

  TGetPerformanceEventsOption = (eoWriteStartTime, eoWriteStopTime, eoWriteAllTime,
    eoWriteBegTime, eoWriteEndTime, eoUseMicroSec, eoWriteFromStart);
  TGetPerformanceEventsOptions = set of TGetPerformanceEventsOption;

  // TPerformanceEvents - record for event duration logging
  TPerformanceEvents = record
    Events: array of TPerformanceEvent;

    // Starts measurement of a new event.
    procedure StartEvent(EventName: string);

    //Stops event measurement. You can indicate EventName for
    // illustration purposes only.
    procedure StopEvent(EventName: string = '');

    // Returns event duration measurement information
    function GetEventsAsString(EvOp: TGetPerformanceEventsOptions): string;
  end;

var
  PerformanceFrequency: Int64;

  // The number of ticks that must be ignored to more accurately measure time intervals
  PerformanceIgnoredTicks: Int64;

implementation


{ TPerformance }

function TPerformance.ElapsedMicroseconds(AStartNew: Boolean = False): Int64;
begin
  Result := Round(ElapsedSeconds(AStartNew) * 1000000);
end;

function TPerformance.ElapsedMilliseconds(AStartNew: Boolean = False): Int64;
begin
  Result := Round(ElapsedSeconds(AStartNew) * 1000);
end;

function TPerformance.ElapsedSeconds(AStartNew: Boolean = False): Double;
begin
  Result := ElapsedTicks(AStartNew) / PerformanceFrequency;
end;

function TPerformance.ElapsedTicks(AStartNew: Boolean = False): Int64;
var
  ACounter: Int64;
begin
  if FIsRunning then
  begin // If measurements are started, then return the current value
    QueryPerformanceCounter(ACounter);
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

function TPerformance.ElapsedTime(AStartNew: Boolean): TDateTime;
begin
  Result := IncMilliSecond(0, ElapsedMilliseconds(AStartNew));
end;

procedure TPerformance.Start;
begin
  FIsRunning    := True;
  FElapsedTicks := 0;
  // Request a counter at the very end of the method
  QueryPerformanceCounter(FStartCounter);
end;

procedure TPerformance.Stop;
var
  ACounter: Int64;
begin
  // Request a counter at the very beginning of the method
  QueryPerformanceCounter(ACounter);
  FIsRunning := False;
  FElapsedTicks := ACounter - FStartCounter - PerformanceIgnoredTicks;
  if FElapsedTicks < 0 then
    FElapsedTicks := 0;
end;

{ TPerformanceEvent }

function TPerformanceEvent.ElapsedMilliseconds: Int64;
begin
  Result := Round(ElapsedSeconds * 1000);
end;

function TPerformanceEvent.ElapsedMicroseconds: Int64;
begin
  Result := Round(ElapsedSeconds * 1000000);
end;

function TPerformanceEvent.ElapsedSeconds: Double;
begin
  Result := ElapsedTicks / PerformanceFrequency;
end;

function TPerformanceEvent.ElapsedTicks: Int64;
begin
  Result := EndCounter - BegCounter;
end;

procedure TPerformanceEvent.Start(AName: string);
begin
  BegDateTime := Now;
  EventName   := AName;
  QueryPerformanceCounter(BegCounter);
end;

procedure TPerformanceEvent.Stop;
begin
  QueryPerformanceCounter(EndCounter);
  EndCounter := EndCounter - PerformanceIgnoredTicks;
  if EndCounter < BegCounter then
    EndCounter := BegCounter;
  EndDateTime := Now;
end;

{ TPerformanceEvents }

function TPerformanceEvents.GetEventsAsString(
  EvOp: TGetPerformanceEventsOptions): string;
var
  Ev, EvFirst: TPerformanceEvent;
  s: string;
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

  EvFirst := Events[0];
  if eoWriteStartTime in EvOp then
  begin
    Result := 'StartTime: ' + FormatDateTime('dd.mm.yy hh:nn:ss.zzz', EvFirst.BegDateTime);
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
      s := s + '; BegTime: ' + FormatDateTime('dd.mm.yy hh:nn:ss.zzz', Ev.BegDateTime);
    if eoWriteEndTime in EvOp then
      s := s + '; EndTime: ' + FormatDateTime('dd.mm.yy hh:nn:ss.zzz', Ev.EndDateTime);

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

procedure TPerformanceEvents.StartEvent(EventName: string);
var
  Cnt: Integer;
begin
  Cnt := Length(Events);
  if (Cnt > 0) and (Events[Cnt - 1].EndCounter = 0) then
    StopEvent();
  SetLength(Events, Cnt + 1);
  Events[Cnt].Start(EventName);
end;

procedure TPerformanceEvents.StopEvent(EventName: string = '');
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
  QueryPerformanceFrequency(PerformanceFrequency);
  if PerformanceFrequency = 0 then
    PerformanceFrequency := 1; // To avoid division by zero
  CalcIgnoredPerformanceTicks;
end.