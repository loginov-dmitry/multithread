unit FastStopThreadUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SyncObjs, StdCtrls, TimeIntervals, ProgressViewer, ExtCtrls;

type
  TMyThread = class(TThread)
  private
    Event: TEvent;
    FUseProgressViewer: Boolean;
    FUseFullCalls: Integer;
    procedure DoUsefullWork;
  protected
    procedure Execute; override;
  public
    constructor Create(UseProgressViewer: Boolean);
    destructor Destroy; override;
    procedure WakeUp; // Команда "Проснуться"
    property UseFullCalls: Integer read FUseFullCalls;
  end;

  TForm2 = class(TForm)
    Label1: TLabel;
    btnRunThread: TButton;
    btnStopThread: TButton;
    Label2: TLabel;
    labFreeThreadTime: TLabel;
    btnWakeUp: TButton;
    cbUseProgressViewer: TCheckBox;
    Timer1: TTimer;
    Label3: TLabel;
    labUseFullCalls: TLabel;
    Label4: TLabel;
    labResumeThreadAfterSetEvent: TLabel;
    procedure btnRunThreadClick(Sender: TObject);
    procedure btnStopThreadClick(Sender: TObject);
    procedure btnWakeUpClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    MyThread: TMyThread;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

  tiSignalled: TTimeInterval;

implementation

{$R *.dfm}

{ TMyThread }

constructor TMyThread.Create(UseProgressViewer: Boolean);
const
  STATE_NONSIGNALED = FALSE;
  AUTO_RESET  = FALSE;
begin
  inherited Create(False);
  // Создаём объект "Event" в состоянии "nonsignaled" и просим, чтобы
  // он автоматически переходил в состояние "nonsignaled" после WaitFor
  Event := TEvent.Create(nil, AUTO_RESET, STATE_NONSIGNALED, '', False);
  FUseProgressViewer := UseProgressViewer;
end;

destructor TMyThread.Destroy;
begin
  // Очень важно, чтобы вызов Terminate был раньше вызова SetEvent!
  Terminate;      // 1. Выставляем флаг Terminated
  tiSignalled.Start;
  Event.SetEvent; // 2. Переводим Event в состояние SIGNALED
  inherited;      // 3. Дожидаемся выхода из метода Execute
  Event.Free;     // 4. Уничтожаем объект Event
end;

procedure TMyThread.DoUsefullWork;
begin
  // Любая полезная работа, выполняемая потоком
  Inc(FUseFullCalls); // Учитываем количество вызовов данного метода
end;

procedure TMyThread.Execute;
var
  WaitRes: TWaitResult;
  pv: TProgressViewer;
begin
  pv := nil; // Чтобы компилятор не выдавал Warning
  while not Terminated do
  begin
    // Внимание! Визуализация ожидания события вносит значительные накладные
    // расходы и значительно увеличивает время уничтожения потока!
    if FUseProgressViewer then
      pv := TProgressViewer.Create('Ожидание события');

    WaitRes := Event.WaitFor(5 * 1000); // Ожидание события - 5 секунд

    if WaitRes = wrSignaled then
      tiSignalled.Stop; // Останавливаем измерение времени

    if FUseProgressViewer then
      pv.TerminateProgress;

    // Выполняем полезную работу если окончился таймаут ожидания (wrTimeout),
    // либо если произошёл вызов метода WakeUp
    if (WaitRes = wrTimeout) or ((WaitRes = wrSignaled) and (not Terminated)) then
      DoUsefullWork;
  end;
end;

procedure TMyThread.WakeUp;
begin
  tiSignalled.Start;
  Event.SetEvent; // Просим поток проснуться и выполнить полезную работу
end;

procedure TForm2.btnRunThreadClick(Sender: TObject);
begin
  MyThread := TMyThread.Create(cbUseProgressViewer.Checked);
  btnRunThread.Enabled := False;
  btnStopThread.Enabled := True;
  btnWakeUp.Enabled := True;
end;

procedure TForm2.btnStopThreadClick(Sender: TObject);
var
  ti: TTimeInterval;
begin
  ti.Start;
  FreeAndNil(MyThread);
  ti.Stop;
  labFreeThreadTime.Caption := Format('%d мкс', [ti.ElapsedMicroseconds]);
  btnRunThread.Enabled  := True;
  btnStopThread.Enabled := False;
  btnWakeUp.Enabled     := False;
end;

procedure TForm2.btnWakeUpClick(Sender: TObject);
begin
  MyThread.WakeUp;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  if Assigned(MyThread) then
    labUseFullCalls.Caption := IntToStr(MyThread.UseFullCalls);

  if tiSignalled.ElapsedMicroseconds() > 0 then
    labResumeThreadAfterSetEvent.Caption := Format('%d мкс', [tiSignalled.ElapsedMicroseconds]);
end;

end.
