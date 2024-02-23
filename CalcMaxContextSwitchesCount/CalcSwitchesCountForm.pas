unit CalcSwitchesCountForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SyncObjs, StdCtrls, ExtCtrls, DateUtils, Contnrs, TimeIntervals;

type
  TMyThread = class(TThread)
  private
    EvForSet, EvForWait: TEvent;
    ThreadNum: Integer;
  protected

  public
    procedure Execute; override;
    constructor Create(AffinityMask: Cardinal; EvForSet, EvForWait: TEvent);
  end;

  TForm1 = class(TForm)
    btnRun: TButton;
    Timer1: TTimer;
    Memo1: TMemo;
    btnClearResults: TButton;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnClearResultsClick(Sender: TObject);
  private
    FList: TObjectList;
    t1, t2: TMyThread;
    StartTime: TDateTime;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  //SwitchesCount: Int64;
  SwitchesCount: Integer;
  Event1, Event2: TEvent;
  MinSleepTime, MaxSleepTime: Integer;
  OrderErrorCnt: Integer;
  LastThreadId: Cardinal;
  LastThreadNum: Integer;
  Thread2WasRun: Boolean;
  Thread1WasRun: Boolean;
implementation

{$R *.dfm}

procedure TForm1.btnClearResultsClick(Sender: TObject);
begin
  StartTime := Now;
  SwitchesCount := 0;
  MinSleepTime := MaxInt;
  MaxSleepTime := 0;
  OrderErrorCnt := 0;
end;

procedure TForm1.btnRunClick(Sender: TObject);
var
  I: Integer;
begin
  //for I := 1 to 2 do
  //  FList.Add(TMyThread.Create());
  FList.Add(TMyThread.Create(2, Event1, Event2)); // 2-е ядро
  FList.Add(TMyThread.Create(4, Event2, Event1)); // 4-е ядро
  //t1 := TMyThread.Create();
  //t2 := TMyThread.Create();
  StartTime := Now;
  btnRun.Enabled := False;
  Timer1.Enabled := True;
end;

{ TMyThread }

constructor TMyThread.Create(AffinityMask: Cardinal; EvForSet, EvForWait: TEvent);
begin
  inherited Create(False);
  //AffinityMask := 2; // Только второе ядро!
  Self.EvForSet := EvForSet;
  Self.EvForWait := EvForWait;
  //SetThreadAffinityMask(Self.Handle, AffinityMask);
  ThreadNum := InterlockedIncrement(LastThreadNum);
end;

procedure TMyThread.Execute;
var
  ti: TTimeInterval;
begin

  {if ThreadNum = 2 then // Дожидаемся запуска первого потока
    while not (Thread1WasRun or Terminated) do Sleep(0); }
  while not Terminated do
  begin
    SetThreadAffinityMask(Self.Handle, Random(16));
    {ti.Start;
    Sleep(1);
    ti.Stop;
    if ti.ElapsedMilliseconds() < MinSleepTime then
      MinSleepTime := ti.ElapsedMilliseconds();
    if ti.ElapsedMilliseconds() > MaxSleepTime then
      MaxSleepTime := ti.ElapsedMilliseconds();}
    ti.Start;
    EvForWait.WaitFor(2000);
    ti.Stop;

    {if ThreadNum = 1 then
      Thread1WasRun := True
    else if ThreadNum = 2 then
      Thread2WasRun := True;   }

    {if ti.ElapsedMilliseconds() < MinSleepTime then
      MinSleepTime := ti.ElapsedMilliseconds();
    if (ti.ElapsedMilliseconds() > MaxSleepTime)  then
      MaxSleepTime := ti.ElapsedMilliseconds(); }

    {if LastThreadId = GetCurrentThreadId then
      InterlockedIncrement(OrderErrorCnt);
    LastThreadId := GetCurrentThreadId; }

    //Inc(SwitchesCount);
    InterlockedIncrement(SwitchesCount);
    
    //if ThreadNum = 1 then // Дожидаемся запуска второго потока
    //  while not (Thread2WasRun or Terminated) do Sleep(0);

    EvForSet.SetEvent;
    Sleep(0);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  //STATE_NONSIGNALED = FALSE;
  STATE_SIGNALED = TRUE;
  AUTO_RESET  = False;
begin
  // Создаём объект "Event" в состоянии "signaled", но просим, чтобы
  // он автоматически переходил в состояние "nonsignaled" после WaitFor
  Event1 := TEvent.Create(nil, AUTO_RESET, STATE_SIGNALED, '', False);
  Event2 := TEvent.Create(nil, AUTO_RESET, STATE_SIGNALED, '', False);

  FList := TObjectList.Create;
  MinSleepTime := MaxInt;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //t1.Free;
  //t2.Free;
  FList.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  MilSeconds: Int64;
begin
  Memo1.Lines.BeginUpdate;
  Memo1.Clear;
  Memo1.Lines.Add('Начало изменений: ' + DateTimeToStr(StartTime));
  MilSeconds := MilliSecondsBetween(Now, StartTime);
  if MilSeconds > 0 then
  begin
    Memo1.Lines.Add('Прошло секунд: ' + IntToStr(Round(MilSeconds / 1000)));
    Memo1.Lines.Add('Всего переключений: ' + IntToStr(SwitchesCount));
    Memo1.Lines.Add('Переключений в секунду: ' + IntToStr(Round(SwitchesCount / MilSeconds * 1000)));
    Memo1.Lines.Add(Format('MinWaitTime=%d, MaxWaitTime=%d; OrderErrorCnt=%d', [MinSleepTime, MaxSleepTime, OrderErrorCnt]));
  end;

  Memo1.Lines.EndUpdate;
end;

end.
