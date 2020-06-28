unit Ex3Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ProgressViewer;

type
  TMyShortThread = class(TThread)
  private
    procedure DoUsefullTask; // Процедура для имитации полезной работы
  public
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    btnRunParallelThread: TButton;
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

{$R *.dfm}

procedure TForm1.btnRunParallelThreadClick(Sender: TObject);
begin
  // Запускает параллельный поток. Если объект потока уже создан,
  // то уничтожает его.
  if MyThread <> nil then
    FreeAndNil(MyThread);
  MyThread := TMyShortThread.Create(False);
end;

{ TMyShortThread }

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

procedure TMyShortThread.Execute;
begin
  DoUsefullTask;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // При закрытии программы необходимо завершить работу потока
  // и уничтожить объект потока MyThread
  if MyThread <> nil then
    MyThread.Free;
end;

end.
