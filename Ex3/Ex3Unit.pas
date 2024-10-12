{$IFDEF FPC}{$CODEPAGE UTF8}{$H+}{$MODE DELPHI}{$ENDIF}
unit Ex3Unit;

interface

uses
  {$IFnDEF FPC}
    Windows, Messages, ProgressViewer, 
  {$ELSE}
    LCLIntf, LCLType, LMessages, 
  {$ENDIF}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TMyShortThread = class(TThread)
  private
    FStatus: string;
    procedure DoUsefullTask; // Процедура для имитации полезной работы
    procedure UpdateGui;
    procedure SetThreadStatus(AStatus: string);
  public
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    btnRunParallelThread: TButton;
    labStatus: TLabel;
    procedure btnRunParallelThreadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    MyThread: TMyShortThread;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.btnRunParallelThreadClick(Sender: TObject);
begin
  // Запускает параллельный поток. Если объект потока уже создан,
  // то уничтожает его.
  if MyThread <> nil then
    FreeAndNil(MyThread);
  MyThread := TMyShortThread.Create(False);
end;

{ TMyShortThread }

{$IFnDEF FPC}
procedure TMyShortThread.DoUsefullTask;
var
 AProgress: TProgressViewer;
begin
  // Реальный поток может выполнять какую угодно полезную работу
  // В учебных целях делаем паузу 5 секунд для имитации задержки, которая
  // может возникнуть при выполнении полезной работы
  AProgress := TProgressViewer.Create('Выполняется поток TMyShortThread');
  Sleep(5000);
  AProgress.TerminateProgress;
end;
{$ELSE FPC}
// Вариант метода DoUsefullTask для Лазаруса (модуль ProgressViewer для Лазаруса отсутствует)
procedure TMyShortThread.DoUsefullTask;
begin
  SetThreadStatus('Выполняется поток TMyShortThread...');
  Sleep(5000);
  SetThreadStatus('');
end;
{$ENDIF FPC}

procedure TMyShortThread.Execute;
begin
  DoUsefullTask;
end;

procedure TMyShortThread.SetThreadStatus(AStatus: string);
begin
  FStatus := AStatus;
  UpdateGui;
end;

procedure TMyShortThread.UpdateGui;
begin
  Form1.labStatus.Visible := FStatus <> '';
  Form1.labStatus.Caption := FStatus;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // При закрытии программы необходимо завершить работу потока
  // и уничтожить объект потока MyThread
  if MyThread <> nil then
    MyThread.Free;
end;

end.
