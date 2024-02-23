unit CalcGetMemSpeedUnit;

{
Результаты замеров.
# 1. Для стандартного менеджера памяти (на базе FastMM4):
Замеры выполнялись на Intel Core i3 9-gen.
Максимальная скорость обеспечивается в однопоточном режиме:
1 млн. обращений к менеджеру памяти выполняется за 9 мс (при IsMultiThread=False)
                                              и за 11 мс (при IsMultiThread=True)
Разница не очень большая.

1 млн. обращений параллельно в 2х потоквх (500тыс в каждом).
Расчётное (идеальное) время = 11 / 2 = 5 мс.
Реальное время: от 18 до 45 мс (NeverSleepOnMMThreadContention=False)
                от 32 до 33 мс (NeverSleepOnMMThreadContention=True)

1 млн. обращений параллельно в 3х потоквх (333тыс в каждом).
Расчётное (идеальное) время = 11 / 3 = 4 мс
Реальное время: от 33 до 70 мс (NeverSleepOnMMThreadContention=False)
                от 32 до 33 мс (NeverSleepOnMMThreadContention=True)

1 млн. обращений параллельно в 4х потоквх (250тыс в каждом)
Расчётное (идеальное) время = 11 / 4 = 3 мс
Реальное время: от 50 до 96 мс (NeverSleepOnMMThreadContention=False)
                от 32 до 33 мс (NeverSleepOnMMThreadContention=True)
Из результатов видно, что при увеличении кол-ва потоков ускорение работы менеджера памяти не
происходит. Наоборот, при задействовании 4-х ядер реальное время оторвалось от рассчётного более чем в 10 раз
Основные задержки связаны с тем, что Interlocked-операции выполняются не моментально, т.к.
выполняется блокировка шины адреса процессора. При этом, если два потока вызвали одновременно функцию
LockCmpxchg для одного и того же адреса памяти, то происходит дополнительная задержка, которая в несколько
раз превышает время вызова функции LockCmpxchg в однопоточном режиме работы.

Существует очень высокая вероятность (при NeverSleepOnMMThreadContention=False), что при вызове
FreeMem окажется, что блок уже заблокирован и, в худшем случае, будет выполнен Sleep(10).
К счастью, перед вызовом Sleep(10) стоит повторный вызов LockCmpxchg, благодаря которому
в большинстве случаев блок будет успешно заблокирован.

При вызове GetMem выполняется попытка заблокировать одну из трех областей, причем почти
всегда одну из этих трех областей удаётся заблокировать с первого раза. Т.е. слишком сильного
насилия над процессором не происходит. И вероятность вызова Sleep(10) в GetMem гораздо
меньше, чем при вызове FreeMem.



# 2. Для актуальной версии менеджера памяти FastMM4:

Результате практически те же самые. Может побыстрее на несколько процентов, но коперий.

# 3. Для актуальной версии менеджера памяти FastMM5 (для Default arenas):
1 млн. обращений к менеджеру памяти выполняется за 13 мс (при IsMultiThread=False)
                                                за 12 мс (при IsMultiThread=True) (что немного странно)
1 млн. обращений параллельно в 2х потоквх (500тыс в каждом)
от 12 до 25 мс (Default arenas)
от 29 до 30 мс(FastMM_16Arenas)

1 млн. обращений параллельно в 3х потоквх (333тыс в каждом)
от 35 до 55 мс (Default arenas)
от 29 до 30 мс (FastMM_16Arenas)

1 млн. обращений параллельно в 4х потоквх (250тыс в каждом)
от 45 до 80 мс (Default arenas)
от 28 до 28 мс (FastMM_16Arenas)

Результаты показывают, что возможности масштабирования FastMM5 ограничены скоростью
работы Interlocked-операций процессора. По сути он даёт мизерные преимущества по сравнению
с обычным менеджером памяти в режиме NeverSleepOnMMThreadContention=True. Однако данный
тест не позволяет раскрыть все преимущества FastMM4
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Contnrs, StdCtrls, SyncObjs, TimeIntervals, ExtCtrls;

type
  TCalcMMSpeedThread = class(TThread)
  private
    GetMemTime: Integer; // Длительность вызова GetMem (в микросекундах)
    AllGetMemTime: Integer;
    AllCount: Integer;
    GetMemAllCount: Integer;
    LongCount: Integer;
    LockFlag: Integer;
    Arr: array of Pointer;
  protected
    procedure Execute; override;
    destructor Destroy; override;
  end;

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edThreadCount: TEdit;
    btnCreateThreads: TButton;
    btnCalcSpeed: TButton;
    Memo1: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    edLoopCount: TEdit;
    cbCalcAnyMMCall: TCheckBox;
    cbNeverSleepOnMMThreadContention: TCheckBox;
    Timer1: TTimer;
    procedure btnCreateThreadsClick(Sender: TObject);
    procedure btnCalcSpeedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    L: TObjectList;
    InCS: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

const
  ARR_LEN = 10000;

var
  Form1: TForm1;
  cs: TCriticalSection;
  LoopCount: Integer;
  CalcAnyMMCall: Boolean;
  GlobalLockFlag: Integer;
implementation

{$R *.dfm}

procedure TForm1.btnCalcSpeedClick(Sender: TObject);
var
  I: Integer;
  t: TCalcMMSpeedThread;
  ti: TTimeInterval;
  AllGetMemTime, AllCount: Integer;
begin
  {При использовании стандартного (встроенного в Delphi) менеджера памяти использование
   NeverSleepOnMMThreadContention может дать хороший прирост производительности.
   Просто вместо тормозов на операции
   "Переключение контекста" и Sleep(10) будут тормоза на операции "спин-блокировка", причём
   эти тормоза будут ещё больше!
   - FastMM4 показывает немного лучше результат, но только при условии, если включены одновременно
   опции NeverSleepOnThreadContention и UseSwitchToThread
   - FastMM5 показывает примерно такой же результат, как и FastMM4 (с включенными опциями).
   Т.е. нельзя сказать, что FastMM5 умеет масштабироваться по кол-ву ядер. По крайней мере
   на процессоре Intel Core i3 9-gen такого явно не происходит. }
  NeverSleepOnMMThreadContention := cbNeverSleepOnMMThreadContention.Checked;
  {FailedLockCountOnGetMem := 0;
  FailedLockCountOnFreeMem := 0;
  FailedLockOperationsOnGetMem := 0;
  FailedLockOperationsOnGetMem1 := 0;
  FailedLockOperationsOnGetMem2 := 0;
  FailedLockOperationsOnGetMem3 := 0;
  IsLockedOnGetMem1 := 0;
  IsLockedOnGetMem2 := 0;
  IsLockedOnGetMem3 := 0;
  IsFreeOnGetMem1 := 0;
  IsFreeOnGetMem2 := 0;
  IsFreeOnGetMem3 := 0; }
  LoopCount := StrToInt(edLoopCount.Text);
  CalcAnyMMCall := cbCalcAnyMMCall.Checked;

  cs.Leave; // Даём команду потокам "Проснуться"
  InCS := False;

  AllGetMemTime := 0;
  AllCount := 0;
  ti.Start;
  if L.Count = 0 then
  begin
    t := TCalcMMSpeedThread.Create(True); // Создаём в НЕзапущенном состоянии
    L.Add(t);
    IsMultiThread := False;
    t.Execute;
    IsMultiThread := True;
    AllGetMemTime := AllGetMemTime + t.AllGetMemTime;
    AllCount := AllCount + t.AllCount;
  end else
  begin
    for I := 0 to L.Count - 1 do
    begin
      t := L[I] as TCalcMMSpeedThread;
      t.WaitFor;
      AllGetMemTime := AllGetMemTime + t.AllGetMemTime;
      AllCount := AllCount + t.AllCount;
    end;
  end;


  ti.Stop;
  Memo1.Clear;

  Memo1.Lines.Add(Format('Время замеров: %d мс; Общее время: %d us; Обращений: %d', [ti.ElapsedMilliseconds, AllGetMemTime, AllCount]));
  {Memo1.Lines.Add(Format('FailedLockCountOnGetMem=%d; FailedLockCountOnFreeMem=%d; FailedLockOperationsOnGetMem=%d; '+
  'm1=%d, m2=%d, m3=%d; L1=%d; L2=%d; L3=%d; F1=%d; F2=%d; F3=%d;',
    [FailedLockCountOnGetMem, FailedLockCountOnFreeMem, FailedLockOperationsOnGetMem,
    FailedLockOperationsOnGetMem1, FailedLockOperationsOnGetMem2, FailedLockOperationsOnGetMem3,
    IsLockedOnGetMem1, IsLockedOnGetMem2, IsLockedOnGetMem3, IsFreeOnGetMem1, IsFreeOnGetMem2, IsFreeOnGetMem3]));}
  for I := 0 to L.Count - 1 do  
  begin
    t := L[I] as TCalcMMSpeedThread;
    Memo1.Lines.Add(Format('Tr#%d: max=%d us; All=%d; GetMem=%d us [%d из %d]', [I+1, t.GetMemTime, t.AllGetMemTime, t.GetMemAllCount, t.LongCount, t.AllCount]));
  end;
  L.Clear;

  btnCalcSpeed.Enabled := False;
  btnCreateThreads.Enabled := True;  
end;

procedure TForm1.btnCreateThreadsClick(Sender: TObject);
var
  Cnt, I: Integer;
begin
  if L.Count > 0 then
    L.Clear;
  if not InCS then
  begin
    cs.Enter;
    InCS := True;
  end;
  Cnt := StrToInt(edThreadCount.Text);
  for I := 1 to Cnt do
    L.Add(TCalcMMSpeedThread.Create(False));

  btnCreateThreads.Enabled := False;
  btnCalcSpeed.Enabled := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  L := TObjectList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  L.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  btnCreateThreads.Click;
  btnCalcSpeed.Click;
end;

{Compare [AAddress], CompareVal:
 If Equal: [AAddress] := NewVal and result = CompareVal
 If Unequal: Result := [AAddress]}
function LockCmpxchg(CompareVal, NewVal: byte; AAddress: PByte): Byte;
asm
  {On entry:
    al = CompareVal,
    dl = NewVal,
    ecx = AAddress}
  lock cmpxchg [ecx], dl
end;

{ TCalcMMSpeedThread }

destructor TCalcMMSpeedThread.Destroy;
var
  J: Integer;
begin
  for J := 0 to ARR_LEN - 1 do
    if Assigned(Arr[J]) then
      FreeMem(Arr[J]);
  inherited;
end;

procedure TCalcMMSpeedThread.Execute;
const
  MAX_BLOCK_SIZE = 200; // "Средний" размер блока в Delphi-программах
var
  CounterBeg, CounterEnd, Freq: Int64;
  TimeInSec: Double;
  I: Integer;
  GetMemTimeTmp, J, ASize: Integer;
  ArrIdx: Integer;
  TestLockCmpxchg: Boolean;
begin
  GetMemTime := -1;
  TestLockCmpxchg := False;
  SetLength(Arr, ARR_LEN);
  ArrIdx := 0;

  cs.Enter;
  cs.Leave;

  QueryPerformanceFrequency(Freq);

  if not CalcAnyMMCall then
    QueryPerformanceCounter(CounterBeg);

  while AllCount < LoopCount do
  begin
    if CalcAnyMMCall then
      QueryPerformanceCounter(CounterBeg);

    if TestLockCmpxchg then
    begin
      //LockCmpxchg(0, 1, PByte(@ArrIdx));
      //LockCmpxchg(0, 1, PByte(@GlobalLockFlag));
      LockCmpxchg(0, 1, PByte(@LockFlag));
      Inc(AllCount);
    end else
    begin
      if ArrIdx >= ARR_LEN then
        ArrIdx := 0;

      if Assigned(Arr[ArrIdx]) then
      begin
        FreeMem(Arr[ArrIdx]);
        Arr[ArrIdx] := nil;
        Inc(AllCount);
      end;

      if AllCount < LoopCount then
      begin
        ASize := Random(MAX_BLOCK_SIZE);
        //ASize := 50;
        if ASize = 0 then ASize := 10;
        GetMem(Arr[ArrIdx], ASize);
        Inc(GetMemAllCount);
        Inc(AllCount);
        Inc(ArrIdx);
      end;

      if CalcAnyMMCall then
      begin
        QueryPerformanceCounter(CounterEnd);

        TimeInSec := (CounterEnd - CounterBeg) / Freq;
        GetMemTimeTmp := Round(TimeInSec * 1000000);
        AllGetMemTime := AllGetMemTime + GetMemTimeTmp;
        if GetMemTimeTmp > GetMemTime then
          GetMemTime := GetMemTimeTmp;
        if GetMemTimeTmp > 1000 then
          Inc(LongCount);
      end;
    end;
  end;

  if not CalcAnyMMCall then
  begin
    QueryPerformanceCounter(CounterEnd);
    TimeInSec := (CounterEnd - CounterBeg) / Freq;
    AllGetMemTime := Round(TimeInSec * 1000000);
  end;
end;

initialization
  cs := TCriticalSection.Create;
finalization
  cs.Free;
end.
