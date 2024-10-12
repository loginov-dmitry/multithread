{
Copyright (c) 2024, Loginov Dmitry Sergeevich
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

unit MutexObjectUnit;

{$IFDEF FPC}{$MODE DELPHI}{$H+}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFDEF UNIX}Unix, BaseUnix, pthreads, TimeIntervals, unixtype, {$ENDIF}
  Classes, SysUtils, SyncObjs;

type
  EMutexError = class(Exception);

  { TMutexObject }

  TMutexObject = class
  private
    FMutexName: string;
    {$IFDEF MSWINDOWS}
    FMutex: THandle;         // Объект "мьютекс" будет создан через функцию CreateMutexShared
    {$ELSE MSWINDOWS}
    UsePthreadMutex: Boolean;
    FMutex: pthread_mutex_t; // Используется если не указано имя мьютекса (только внутри процесса)
                             // по сути, та же критическая секция, только с таймаутом

    FLockFD: cint;           // Дескриптор файла блокировки (если указано имя мьютекса)
    FLockCount: Integer;     // Если блокировка установлена, то FLockCount > 0
    FLockThreadId: UInt64;   // ID потока, который установил блокировку (только он может снять блокировку)
    
    function GetLockFileName: string;
    {$ENDIF MSWINDOWS}

  public
    constructor Create(MutexName: string; aInitialOwner: Boolean = False);
    destructor Destroy; override;
    function WaitFor(TimeOutMs: DWORD): TWaitResult;
    procedure ReleaseMutex;
  end;

implementation

{$IfDef MSWINDOWS}
function CreateMutexShared(AName: string; LastError: PCardinal = nil): THandle;
var
  SD:TSecurityDescriptor;
  SA:TSecurityAttributes;
  pSA: PSecurityAttributes;
begin
  if not InitializeSecurityDescriptor(@SD, SECURITY_DESCRIPTOR_REVISION) then
    raise EMutexError.CreateFmt('CreateMutexShared: Error InitializeSecurityDescriptor: %s', [SysErrorMessage(GetLastError)]);

  SA.nLength:=SizeOf(TSecurityAttributes);
  SA.lpSecurityDescriptor:=@SD;
  SA.bInheritHandle:=False;

  if not SetSecurityDescriptorDacl(SA.lpSecurityDescriptor, True, nil, False) then
    raise EMutexError.CreateFmt('CreateMutexShared: Error SetSecurityDescriptorDacl: %s', [SysErrorMessage(GetLastError)]);

  pSA := @SA;

  Result := CreateMutex(pSA, False, PChar('Global\' + AName)); // Пытаемся создать с директивой Global
  if Result = 0 then
    Result := CreateMutex(pSA, False, PChar(AName)); // Пытаемся создать без директивы Global

  if Assigned(LastError) then
    LastError^ := GetLastError;

  if Result = 0 then
    raise EMutexError.CreateFmt('CreateMutexShared: can not create mutex object: %s', [SysErrorMessage(GetLastError)]);
end;
{$EndIf MSWINDOWS}

{ TMutexObject }

{$IfDef UNIX}
function TMutexObject.GetLockFileName: string;
begin
  Result := '/var/lock/' + FMutexName + '.lock';
end;
{$ENDIF}

constructor TMutexObject.Create(MutexName: string; aInitialOwner: Boolean);
{$IfDef LINUX}
var
  Mattr: pthread_mutexattr_t;
  I: Integer;
{$ENDIF}
begin
  FMutexName := MutexName;
  {$IfDef MSWINDOWS}
  FMutex := CreateMutexShared(MutexName);
  {$Else MSWINDOWS}

  UsePthreadMutex := MutexName = '';

  if UsePthreadMutex then
  begin
    CheckOSError(pthread_mutexattr_init(@Mattr));
    try
      CheckOSError(pthread_mutexattr_settype(@Mattr,Ord(PTHREAD_MUTEX_RECURSIVE)));
      CheckOSError(pthread_mutex_init(@FMutex,@Mattr));
    finally
      pthread_mutexattr_destroy(@Mattr); // don't raise second error, it would hide the first
    end;

  end else
  begin
    // Удаляем все недопустимые символы из имени мьютекса
    for I := 1 to Length(FMutexName) do
      if AnsiChar(FMutexName[I]) in ['\', '/', ':', '*', '"', '?', '|', '<', '>'] then
        FMutexName[I] := '_';
    FMutexName := AnsiLowerCase(FMutexName);
  end;

  if aInitialOwner then
    WaitFor(0);

  {$EndIf MSWINDOWS}
end;

destructor TMutexObject.Destroy;
begin
  {$IfDef MSWINDOWS}
  if FMutex <> 0 then
  begin
    FileClose(FMutex);
    //CloseHandle(HMutex);
    FMutex := 0;
  end;
  {$ELSE MSWINDOWS}
  if UsePthreadMutex then
    pthread_mutex_destroy(@FMutex)
  else
  begin
    if FLockFD > 0 then
    begin
      if FLockCount > 0 then
        fpFlock(FLockFD, LOCK_UN); // Если блокировка установлена, то снимаем её
      fpClose(FLockFD);
    end;
  end;
  {$ENDIF MSWINDOWS}
  inherited;
end;

{$IfDef LINUX}
function pthread_mutex_timedlock(__mutex:Ppthread_mutex_t; __abstime: Ptimespec):cint;cdecl; external libthreads;

procedure MSecsFromNow (tNow : Timeval; aTimeout : Integer; out tfuture: TTimespec);

var
  td,tm : integer;

begin
  td:=aTimeout div 1000;
  tm:=aTimeout mod 1000;
  tfuture.tv_sec:=tnow.tv_sec+td;
  tfuture.tv_nsec:=tnow.tv_usec*1000+(tm*1000*1000);
end;
{$ENDIF LINUX}

function TMutexObject.WaitFor(TimeOutMs: DWORD): TWaitResult;
{$IfDef MSWINDOWS}
var
  WinRes: Cardinal;
{$Else MSWINDOWS}
var
  td,tm,Errno: Integer;
  tnow: ttimeval;
  Tmp: timespec;
  ti: TTimeInterval;
  TmpLockFD, RWMode: cint;
  sLockFile: string;
{$EndIf MSWINDOWS}
begin
  {$IfDef MSWINDOWS}
  WinRes := WaitForSingleObject(FMutex, TimeOutMs);
  case WinRes of
    WAIT_OBJECT_0: Result := wrSignaled;
    WAIT_TIMEOUT: Result := wrTimeout;
    WAIT_ABANDONED: Result := wrAbandoned;
  else
    //WAIT_FAILED:
    Result := wrError;
  end;
  {$Else MSWINDOWS}
  if UsePthreadMutex then
  begin
    Result:=wrError;
    if (TimeOutMs=0) then
      begin
        ErrNo:=pthread_mutex_trylock(@FMutex);
        if ErrNo=0 then
          Result:=wrSignaled
        else if (Errno=ESysEAGAIN) then
          Result:=wrTimeout
      end
    else if (TimeOutMs<>INFINITE) then
      begin
      fpgettimeofday(@tnow,Nil);

      MsecsFromNow(tnow, TimeOutMs,tmp);
      ErrNo:=pthread_mutex_timedlock(@FMutex,@tmp);

      if ErrNo=0 then
        Result:=wrSignaled
      else if (Errno=ESysEBUSY) or (errNo=ESysETIMEDOUT) then
        Result:=wrTimeout
      end
    else
      begin
      if (pthread_mutex_lock(@FMutex)=0) then
        Result:=wrSignaled
      end;
  end else
  begin

    if (FLockCount > 0) and (GetCurrentThreadId = FLockThreadId) then
    begin
      Inc(FLockCount);
      Result := wrSignaled;
      Exit;
    end;

    sLockFile := GetLockFileName;
    TmpLockFD := fpOpen(sLockFile, O_RDWR or O_CREAT);
    if TmpLockFD <= 0 then
      raise EMutexError.CreateFmt('can not open lock file "%s": %s ', [sLockFile, SysErrorMessage(GetLastOSError)]);

    if TimeOutMs = 0 then
    begin
      if fpFlock(TmpLockFD, LOCK_EX or LOCK_NB) = 0 then
        Result := wrSignaled
      else
        Result := wrTimeout;
    end else if TimeOutMs = INFINITE then
    begin
      if fpFlock(TmpLockFD, LOCK_EX) = 0 then
        Result := wrSignaled
      else
        Result := wrError;
    end else
    begin
      ti.Start;
      Result := wrTimeout;

      while ti.ElapsedMilliseconds() < TimeOutMs do
      begin
        // К сожалению, в линуксе невозможно указать время ожидания снятия блокировки
        // файла, поэтому организуем цикл ожидания снятия блокировки
        if fpFlock(TmpLockFD, LOCK_EX or LOCK_NB) = 0 then
        begin
          Result := wrSignaled;
          Break;
        end else if ti.ElapsedMilliseconds > 5 then // Первые 5 миллисекунд НЕ используем Sleep
          Sleep(10);
      end;

      if Result = wrTimeout then
        if fpFlock(TmpLockFD, LOCK_EX or LOCK_NB) = 0 then
          Result := wrSignaled;
    end;

    if Result = wrSignaled then // Если удалось захватить блокировку файла
    begin
      FLockThreadId := GetCurrentThreadId; // Внимание! Для Linux идентификаторы потоков задействуются повторно!!!
      FLockCount := 1;
      FLockFD := TmpLockFD; // Запоминаем дескриптор файла, чтобы сработал метод ReleaseMutex

      RWMode := S_IRUSR or S_IRGRP or S_IROTH or S_IWUSR or S_IWGRP or S_IWOTH;
      FpChmod(sLockFile, RWMode); // Настраиваем чтение/запись для всех пользователей
    end else
    begin
      fpClose(TmpLockFD);
    end;
  end;
  {$EndIf MSWINDOWS}
end;

procedure TMutexObject.ReleaseMutex;
{$IfDef UNIX}
var
  TmpLockFD: cint;
{$ENDIF}
begin
  {$IfDef MSWINDOWS}
  Windows.ReleaseMutex(FMutex);
  {$Else MSWINDOWS}
  if UsePthreadMutex then
    CheckOSError(pthread_mutex_unlock(@FMutex))
  else
  begin
    if FLockCount > 0 then
    begin
      if GetCurrentThreadId <> FLockThreadId then
        raise EMutexError.Create('TMutexWrapper: попытка вызова ReleaseMutex из другого потока для занятого мьютекса');
      if FLockCount = 1 then
      begin
        TmpLockFD := FLockFD;
        FLockFD := 0;
        FLockThreadId := 0;
        FLockCount := 0;
        fpFlock(TmpLockFD, LOCK_UN);
        fpClose(TmpLockFD);
        Exit;
      end;
      Dec(FLockCount);
    end;
  end;
  {$EndIf MSWINDOWS}
end;

end.

