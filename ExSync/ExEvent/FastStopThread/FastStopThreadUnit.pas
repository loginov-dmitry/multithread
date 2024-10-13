{$IFDEF FPC}{$CODEPAGE UTF8}{$H+}{$MODE DELPHI}{$ENDIF}
unit FastStopThreadUnit;

interface

uses
  {$IFnDEF FPC}
    Windows, Messages, ProgressViewer, 
  {$ELSE}
    LCLIntf, LCLType, LMessages, 
  {$ENDIF}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, SyncObjs, StdCtrls, TimeIntervals, 
    ExtCtrls;

type
  TMyThread = class(TThread)
  private
    Event: TEvent;
    FUseProgressViewer: Boolean;
    FUseFullCalls: Integer;
    FGuiMsg: string;
    procedure DoUsefullWork;
    procedure ShowMsg(Msg: string);
    procedure UpdateGui;
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
    labMsgFromThread: TLabel;
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

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

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
  {$IFnDEF FPC}
  pv: TProgressViewer;
  {$ENDIF}
begin
  {$IFnDEF FPC}
  pv := nil; // Чтобы компилятор не выдавал Warning
  {$ENDIF}
  
  while not Terminated do
  begin
    ShowMsg('Ожидание события...');

    // Внимание! Визуализация ожидания события вносит значительные накладные
    // расходы и значительно увеличивает время уничтожения потока!
    {$IFnDEF FPC}
    if FUseProgressViewer then
      pv := TProgressViewer.Create('Ожидание события');
    {$ENDIF}

    WaitRes := Event.WaitFor(5 * 1000); // Ожидание события - 5 секунд

    if WaitRes = wrSignaled then
      tiSignalled.Stop; // Останавливаем измерение времени

    ShowMsg('');

    {$IFnDEF FPC}
    if FUseProgressViewer then
      pv.TerminateProgress;
    {$ENDIF}  

    // Выполняем полезную работу если окончился таймаут ожидания (wrTimeout),
    // либо если произошёл вызов метода WakeUp
    if (WaitRes = wrTimeout) or ((WaitRes = wrSignaled) and (not Terminated)) then
      DoUsefullWork;
  end;
end;

procedure TMyThread.ShowMsg(Msg: string);
begin
  FGuiMsg := Msg;
  TThread.Synchronize(nil, UpdateGui);
end;

procedure TMyThread.UpdateGui;
begin
  Form2.labMsgFromThread.Visible := FGuiMsg <> '';
  Form2.labMsgFromThread.Caption := FGuiMsg;
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
