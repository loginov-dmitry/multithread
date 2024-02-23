unit CalcGetMemSpeedUnit;

{
Результаты замеров.
# 1. Для стандартного менеджера памяти (на базе FastMM4):
Замеры выполнялись на Intel Core i3 9-gen.
Максимальная скорость обеспечивается в однопоточном режиме:
1 млн. обращений к менеджеру памяти выполняется за 9 мс (при IsMultiThread=False)
                                              и за 11 мс (при IsMultiThread=True)
Разница не очень большая.
1 млн. обращений параллельно в 2х потоквх (500тыс в каждом) выполняется от 18 до 45 мс (NeverSleepOnMMThreadContention=False)
                                                                        от 32 до 33 мс (NeverSleepOnMMThreadContention=True)
1 млн. обращений параллельно в 3х потоквх (333тыс в каждом) выполняется от 33 до 70 мс (NeverSleepOnMMThreadContention=False)
                                                                        от 32 до 33 мс (NeverSleepOnMMThreadContention=True)
1 млн. обращений параллельно в 4х потоквх (250тыс в каждом) выполняется от 50 до 96 мс (NeverSleepOnMMThreadContention=False)
                                                                        от 32 до 33 мс (NeverSleepOnMMThreadContention=True)
Из результатов видно, что при увеличении кол-ва потоков ускорение работы менеджера памяти не
происходит. Наоборот, при задействовании 4-х ядер производительность просела до 10 раз.

# 2. Для актуальной версии менеджера памяти FastMM4:

Результате практически те же самые. Может побыстрее на несколько процентов, но коперий.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Contnrs, StdCtrls, SyncObjs, TimeIntervals;

type
  TCalcMMSpeedThread = class(TThread)
  private
    GetMemTime: Integer; // Длительность вызова GetMem (в микросекундах)
    AllGetMemTime: Integer;
    AllCount: Integer;
    LongCount: Integer;
  protected
    procedure Execute; override;
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
    procedure btnCreateThreadsClick(Sender: TObject);
    procedure btnCalcSpeedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    L: TObjectList;
    InCS: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  cs: TCriticalSection;
  LoopCount: Integer;
  CalcAnyMMCall: Boolean;
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
  for I := 0 to L.Count - 1 do
  begin
    t := L[I] as TCalcMMSpeedThread;
    Memo1.Lines.Add(Format('Tr#%d: max=%d us; All=%d us [%d из %d]', [I+1, t.GetMemTime, t.AllGetMemTime, t.LongCount, t.AllCount]));
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

{ TCalcMMSpeedThread }

procedure TCalcMMSpeedThread.Execute;
const
  ARR_LEN = 10000;
  MAX_BLOCK_SIZE = 200; // "Средний" размер блока в Delphi-программах
var
  CounterBeg, CounterEnd, Freq: Int64;
  TimeInSec: Double;
  I: Integer;
  GetMemTimeTmp, J: Integer;
  Arr: array of Pointer;
  ArrIdx: Integer;
begin
  GetMemTime := -1;
  SetLength(Arr, ARR_LEN);
  ArrIdx := 0;

  cs.Enter;
  cs.Leave;

  QueryPerformanceFrequency(Freq);

  if not CalcAnyMMCall then
    QueryPerformanceCounter(CounterBeg);

  for I := 1 to LoopCount do
  begin
    if CalcAnyMMCall then
      QueryPerformanceCounter(CounterBeg);

    if ArrIdx >= ARR_LEN then
      ArrIdx := 0;

    if Assigned(Arr[ArrIdx]) then
    begin
      FreeMem(Arr[ArrIdx]);
      Arr[ArrIdx] := nil;
    end else
      GetMem(Arr[ArrIdx], Random(MAX_BLOCK_SIZE));

    Inc(AllCount);
    Inc(ArrIdx);

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

  if not CalcAnyMMCall then
  begin
    QueryPerformanceCounter(CounterEnd);
    TimeInSec := (CounterEnd - CounterBeg) / Freq;
    AllGetMemTime := Round(TimeInSec * 1000000);
  end;

  for J := 0 to ARR_LEN - 1 do
    if Assigned(Arr[J]) then
      FreeMem(Arr[J]);
  {if Assigned(p) then
    FreeMem(p);}
end;

initialization
  cs := TCriticalSection.Create;
finalization
  cs.Free;
end.
