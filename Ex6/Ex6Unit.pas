{$IFDEF FPC}{$CODEPAGE UTF8}{$H+}{$MODE DELPHI}{$ENDIF}
unit Ex6Unit;

interface

uses
  {$IFnDEF FPC}
    Windows, Messages, 
  {$ELSE}
    LCLIntf, LCLType, LMessages, 
  {$ENDIF}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, MTUtils, TimeIntervals, 
    LDSWaitFrm, LDSWaitIntf, ParamsUtils;

type
  TMyThread = class(TThread)
  private
    FThreadNum: Integer;
  public
    procedure Execute; override;
    constructor Create;
    destructor Destroy; override;
  end;

  TForm1 = class(TForm)
    btnRunInParallelThread: TButton;
    procedure btnRunInParallelThreadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    function StopThreads(OperType: Integer; AParams: TParamsRec; AResParams: PParamsRec; wsi: IWaitStatusInterface): Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ThreadCount: Integer;
  StopThreadsFlag: Boolean;
implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.btnRunInParallelThreadClick(Sender: TObject);
begin
  // Запускаем параллельный поток
  TMyThread.Create;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Выставляем флаг StopThreadsFlag, чтобы все потоки завершились
  StopThreadsFlag := True;
  if ThreadCount > 0 then
    DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Ожидаем завершение потоков...', ParamsEmpty, StopThreads, NOT_SHOW_STOP_BTN);
end;

procedure TForm1.FormDestroy(Sender: TObject);
{var
  pv: TProgressViewer;}
begin
  {// Выставляем флаг StopThreadsFlag, чтобы все потоки завершились
  StopThreadsFlag := True;

  // Задерживаем выход из программы, пока не будут завершены все потоки
  if ThreadCount > 0 then
  begin
    pv := TProgressViewer.Create('Ожидаем завершение потоков');
    while ThreadCount > 0 do
      Sleep(10);
    pv.TerminateProgress;
  end;}
end;

function TForm1.StopThreads(OperType: Integer; AParams: TParamsRec;
  AResParams: PParamsRec; wsi: IWaitStatusInterface): Boolean;
begin
  // Задерживаем выход из программы, пока не будут завершены все потоки
  while ThreadCount > 0 do
    Sleep(10);
end;

{ TMyThread }

constructor TMyThread.Create;
begin
  inherited Create(False);
  // Увеличиваем глобальную переменную ThreadCount на 1 и запоминаем
  // полученное значение
  FThreadNum := InterlockedIncrement(ThreadCount);
end;

destructor TMyThread.Destroy;
begin
  inherited;
  // Уменьшаем глобальную переменную ThreadCount на 1
  InterlockedDecrement(ThreadCount);
end;

procedure TMyThread.Execute;
var
  ti: TTimeInterval;
begin
  FreeOnTerminate := True;

  // Организуем паузу 10 секунд. При этом каждые 20 мс
  // проверяем флаг StopThreadsFlag
  ti.Start;
  while ti.ElapsedSeconds < 10 do
  begin
    // Заканчиваем ожидание, если выставлен флаг StopThreadsFlag
    if StopThreadsFlag then Break;
    Sleep(20);
  end;

  ThreadShowMessageFmt('Работа потока #%d завершена!', [FThreadNum]);
end;

end.
