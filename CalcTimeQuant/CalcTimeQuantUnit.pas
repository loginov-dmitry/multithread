unit CalcTimeQuantUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CheckLst, MMSystem;

type
  TCalcQuantThread = class(TThread)
  private
    // Добавляет длительность интервала активности (квант времени)
    procedure AddToWorkList(WorkTime: Double);

    // Добавляет длительность интервала бездействия (в спящем состоянии)
    procedure AddToNotWorkList(NotWorkTime: Double);
  protected
    procedure Execute; override;
  public
    ThreadNum: Integer;           // Номер потока
    IsFinish: Boolean;            // Флаг "работа потока завершена"
    WorkAll: Double;              // Общее время работы
    NotWorkAll: Double;           // Общее время бездействия
    LoopCount: Integer;           // Количество циклов
    WorkList: array of Double;    // Длительность выделенных квантов времени
    NotWorkList: array of Double; // Длительность интервалов простоя
    constructor Create(ThreadNum: Integer; AffinityMask: DWORD; APriority: TThreadPriority);
  end;

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edThreadCount: TEdit;
    btnStartThreads: TButton;
    Label3: TLabel;
    Memo1: TMemo;
    Timer1: TTimer;
    Label4: TLabel;
    clbCPUList: TCheckListBox;
    cbUseDiffPriority: TCheckBox;
    Label5: TLabel;
    cbPriority: TComboBox;
    Label6: TLabel;
    edSysTimerInterval: TEdit;
    btnChangeSysTimerInterval: TButton;
    procedure btnStartThreadsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnChangeSysTimerIntervalClick(Sender: TObject);
  private
    { Private declarations }
    FList: TList; // Список запущенных потоков
    function GetAffinityMask: DWORD;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnChangeSysTimerIntervalClick(Sender: TObject);
var
  NewInterval, Res: Cardinal;
begin
  {Внимание! Перед изменением разрешения системного таймера проверьте его текущее
   разрешение с помощью утилиты Clockres.exe, её можно скачать с сайта Microsoft.
   Также желательно закрыть Delphi, т.к. она может принудительно выставлять
   разрешение в 1 мс. Браузеры также могут менять разрешение!}
  {
  This function affects a global Windows setting. Windows uses the lowest value
   (that is, highest resolution) requested by any process. Setting a higher
   resolution can improve the accuracy of time-out intervals in wait functions.
   However, it can also reduce overall system performance, because the thread
   scheduler switches tasks more often.
  }
  NewInterval := StrToInt(edSysTimerInterval.Text);
  Res := timeBeginPeriod(NewInterval);
  if Res = TIMERR_NOCANDO then
    raise Exception.Create('Задано недопустимое разрешение таймера!');
end;

procedure TForm1.btnStartThreadsClick(Sender: TObject);
var
  I: Integer;
  APriority: TThreadPriority;
begin
  Memo1.Clear;

  APriority := TThreadPriority(cbPriority.ItemIndex);
  for I := 1 to StrToInt(edThreadCount.Text) do
  begin
    if cbUseDiffPriority.Checked and (I > 1) then
    begin
      if APriority > tpIdle then
        Dec(APriority);
    end;
      
    FList.Add(TCalcQuantThread.Create(I, GetAffinityMask, APriority));
  end;
  btnStartThreads.Enabled := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Info: SYSTEM_INFO;
  I: Integer;
begin
  FList := TList.Create;
  DecimalSeparator := '.';

  GetSystemInfo(Info);
  for I := 1 to Info.dwNumberOfProcessors do
    clbCPUList.Items.Add('cpu #' + IntToStr(I));
  clbCPUList.Checked[0] := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TCalcQuantThread(FList[I]).Free;
  FList.Free;
end;

function TForm1.GetAffinityMask: DWORD;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to clbCPUList.Count - 1 do
    if clbCPUList.Checked[I] then
      Result := Result or (1 shl I);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  I: Integer;
  Q: Double;
  T: TCalcQuantThread;
  s: string;
begin
  for I := FList.Count - 1 downto 0 do
  begin
    T := TCalcQuantThread(FList[I]);
    if T.IsFinish then
    begin
      s := Format('Интервалы активности потока #%d (Общее время=%f; Число квантов=%d; '+
        'Число циклов=%d): ', [T.ThreadNum, T.WorkAll, Length(T.WorkList), T.LoopCount]);
      for Q in T.WorkList do
        s := s + FormatFloat('0.0000', Q) + ',';
      Memo1.Lines.Add(s);
      s := Format('Интервалы бездействия потока #%d (Общее время=%f; Число интервалов '+
        'бездействия=%d): ', [T.ThreadNum, T.NotWorkAll, Length(T.NotWorkList)]);
      for Q in T.NotWorkList do
        s := s + FormatFloat('0.0000', Q) + ',';
      Memo1.Lines.Add(s + sLineBreak);
      T.Free;
      FList.Delete(I);
    end;
  end;
  
  if FList.Count = 0 then
    btnStartThreads.Enabled := True;
end;

{ TCalcQuantThread }

procedure TCalcQuantThread.AddToNotWorkList(NotWorkTime: Double);
begin
  SetLength(NotWorkList, Length(NotWorkList) + 1);
  NotWorkList[High(NotWorkList)] := NotWorkTime;
  NotWorkAll := NotWorkAll + NotWorkTime;
end;

procedure TCalcQuantThread.AddToWorkList(WorkTime: Double);
begin
  SetLength(WorkList, Length(WorkList) + 1);
  WorkList[High(WorkList)] := WorkTime;
  WorkAll := WorkAll + WorkTime;
end;

constructor TCalcQuantThread.Create(ThreadNum: Integer; AffinityMask: DWORD;
  APriority: TThreadPriority);
begin
  inherited Create(False);
  Self.ThreadNum := ThreadNum;
  if AffinityMask > 0 then
    SetThreadAffinityMask(Self.Handle, AffinityMask);
  Priority := APriority;
end;

procedure TCalcQuantThread.Execute;
var
  StartTicks, BreakDiff, QuantDiff, CurQuantTime: Int64;
  PrevTicks, CurTicks, CurQuantStart: Int64;
  Freq: Int64;
begin
  QueryPerformanceFrequency(Freq);
  QueryPerformanceCounter(StartTicks);
  PrevTicks := StartTicks;
  CurQuantStart := StartTicks;
  BreakDiff := 10 * Freq;
  QuantDiff := Round(0.001 * Freq);
  CurQuantTime := 0;
  repeat
    QueryPerformanceCounter(CurTicks);
    Inc(LoopCount);
    if CurTicks - PrevTicks > QuantDiff then
    begin // Если разница оказалась больше 1 мс, значит ОС приостанавливала
          // работу потока и теперь начался отсчёт нового кванта
      AddToWorkList(CurQuantTime / Freq); // Сохраняем время работы потока
      AddToNotWorkList((CurTicks - PrevTicks) / Freq); // Сохраняем время простоя потока
      CurQuantStart := CurTicks;
      CurQuantTime := 0;
    end else
      CurQuantTime := CurTicks - CurQuantStart;
    PrevTicks := CurTicks;
  until (CurTicks - StartTicks) > BreakDiff;
  if CurQuantTime > 0 then // Обрабатываем длительность последнего кванта
    AddToWorkList(CurQuantTime / Freq);
  IsFinish := True;
end;

end.
