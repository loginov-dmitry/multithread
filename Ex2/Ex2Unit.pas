unit Ex2Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMyLongThread = class(TThread)
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
    MyThread: TMyLongThread;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnRunParallelThreadClick(Sender: TObject);
begin
  // Запускает параллельный поток
  if MyThread = nil then
    MyThread := TMyLongThread.Create(False)
  else
    raise Exception.Create('Дополнительный поток уже запущен!');
end;

{ TMyLongThread }

procedure TMyLongThread.DoUsefullTask;
begin
  // Реальный поток может выполнять какую угодно полезную работу
  // В учебных целях делаем паузу 5 секунд для имитации задержки, которая
  // может возникнуть при выполнении полезной работы
  Sleep(5000);
end;

procedure TMyLongThread.Execute;
  procedure WaitTimeout(ATimeOut: Integer);
  begin
    Sleep(ATimeOut);
  end;
begin
  while not Terminated do
  begin
    DoUsefullTask;
    WaitTimeout(10000); // Ожидаем таймаут 10 сек.
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // При закрытии программы необходимо завершить работу потока
  // и уничтожить объект потока MyThread
  if MyThread <> nil then
    MyThread.Free;
end;

end.
