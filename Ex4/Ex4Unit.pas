unit Ex4Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ProgressViewer;

type
  TMyLongThread = class(TThread)
  private
    FTaskNum: Integer;
    procedure DoUsefullTask1; // Первая задача
    procedure DoUsefullTask2; // Вторая задача
    procedure DoFinalizeTask; // Задача запускается при завершении работы потока
  public
    constructor Create(TaskNum: Integer);
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    btnRunParallelThreads: TButton;
    Label1: TLabel;
    cbTerminateMode: TComboBox;
    procedure btnRunParallelThreadsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    MyThread1: TMyLongThread; // Поток для первой задачи
    MyThread2: TMyLongThread; // Поток для второй задачи
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnRunParallelThreadsClick(Sender: TObject);
begin
  // Запускает параллельный поток для задачи 1
  if MyThread1 = nil then
    MyThread1 := TMyLongThread.Create(1);

  // Запускает параллельный поток для задачи 2
  if MyThread2 = nil then
    MyThread2 := TMyLongThread.Create(2);
end;

{ TMyLongThread }

constructor TMyLongThread.Create(TaskNum: Integer);
begin
  inherited Create(False); // Вызываем родительский конструктор

  // Запоминаем параметр TaskNum. Он нужен в методе Execute
  FTaskNum := TaskNum;
end;

procedure TMyLongThread.DoFinalizeTask;
begin
  Sleep(5000); // Данная условная задача занимает 5 секунд
end;

procedure TMyLongThread.DoUsefullTask1;
begin
  Sleep(1000); // Данная условная задача занимает 1 секунду
end;

procedure TMyLongThread.DoUsefullTask2;
begin
  Sleep(2000); // Данная условная задача занимает 2 секунды
end;

procedure TMyLongThread.Execute;
  procedure WaitTimeout(ATimeOut: Integer);
  begin
    Sleep(ATimeOut);
  end;
begin
  while True do
  begin
    if Terminated then
    begin
      DoFinalizeTask; // Некоторые действия при завершении потока
      Exit; // Завершаем работу потока
    end else
    begin
      if FTaskNum = 1 then
        DoUsefullTask1  // Запускаем задачу 1
      else
        DoUsefullTask2; // Запускаем задачу 2

      if not Terminated then // Дополнительная проверка не повредит!
        WaitTimeout(1000); // Ожидаем таймаут 1 сек
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  AProgress: TProgressViewer;
begin
  AProgress := TProgressViewer.Create('Выход из программы');
  try
    if cbTerminateMode.ItemIndex = 1 then
    begin // Выбран режим "Одновременно (быстрее)"
      if Assigned(MyThread1) then
        MyThread1.Terminate; // Выставляем флаг Terminated
      if Assigned(MyThread2) then
        MyThread2.Terminate; // Выставляем флаг Terminated
    end;
    MyThread1.Free;
    MyThread2.Free;
  finally
    AProgress.TerminateProgress;
  end;  
end;

end.
