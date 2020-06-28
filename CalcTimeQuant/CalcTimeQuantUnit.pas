unit CalcTimeQuantUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

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
    constructor Create(ThreadNum: Integer);
  end;

  TSleepThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edThreadCount: TEdit;
    btnStartThreads: TButton;
    Label3: TLabel;
    Memo1: TMemo;
    Timer1: TTimer;
    Button1: TButton;
    procedure btnStartThreadsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FList: TList; // Список запущенных потоков
    SleepThread: TSleepThread;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnStartThreadsClick(Sender: TObject);
var
  I: Integer;
begin
  Memo1.Clear;
  for I := 1 to StrToInt(edThreadCount.Text) do
    FList.Add(TCalcQuantThread.Create(I));
  btnStartThreads.Enabled := False;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SleepThread := TSleepThread.Create(False);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FList := TList.Create;
  DecimalSeparator := '.';
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TCalcQuantThread(FList[I]).Free;
  FList.Free;
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

constructor TCalcQuantThread.Create(ThreadNum: Integer);
begin
  inherited Create(False);
  Self.ThreadNum := ThreadNum;
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
  BreakDiff := 5 * Freq;
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

{ TSleepThread }

procedure TSleepThread.Execute;
begin
  while not Terminated do Sleep(1);
end;

end.
