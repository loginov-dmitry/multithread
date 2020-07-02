unit Ex5Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ProgressViewer, Contnrs, MTUtils;

type
  TMyLongThread1 = class(TThread)
  private
    FUsefullTaskTime: Integer;
  public
    constructor Create(UsefullTaskTime: Integer);
    procedure Execute; override;
  end;

  TMyLongThread2 = class(TThread)
  public
    procedure Execute; override;
  end;

  TMyLongThread3 = class(TThread)
  private
    FUsefullTaskTime: Integer;
  public
    constructor Create(UsefullTaskTime: Integer);
    procedure Execute; override;
  end;

  TMyLongThread4 = class(TThread)
  public
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    btnRunParallelThreads: TButton;
    Label1: TLabel;
    cbTerminateMode: TComboBox;
    procedure btnRunParallelThreadsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FList: TObjectList; // Потоки для первой и второй задачи
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnRunParallelThreadsClick(Sender: TObject);
begin
  // Запускаем 4 параллельных потока
  if FList.Count = 0 then
  begin
    FList.Add(TMyLongThread1.Create(1000));
    FList.Add(TMyLongThread2.Create(False));
    FList.Add(TMyLongThread3.Create(2000));
    FList.Add(TMyLongThread4.Create(False));
  end;
end;

{ TMyLongThread }

constructor TMyLongThread1.Create(UsefullTaskTime: Integer);
begin
  inherited Create(False); // Вызываем родительский конструктор
  FUsefullTaskTime := UsefullTaskTime;
end;

procedure TMyLongThread1.Execute;
begin
  while not Terminated do
  begin
    EmulateUsefullWork(FUsefullTaskTime);
    ThreadWaitTimeout(Self, 60000); // Ожидаем таймаут 60 сек
  end;
  Sleep(5000); // Оставлено для демонстрации режима "Одновременно"  
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FList := TObjectList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  AProgress: TProgressViewer;
  I: Integer;
begin
  AProgress := TProgressViewer.Create('Выход из программы');
  try
    if cbTerminateMode.ItemIndex = 1 then
    begin // Выбран режим "Одновременно (быстрее)"
      // Выставляем флаг Terminated для всех потоков. Можно использовать
      // родительский класс TThread для операции приведения типов.
      for I := 0 to FList.Count - 1 do
        TThread(FList[I]).Terminate;
    end;
    // При уничтожении списка TObjectList будут уничтожены все объекты потоков
    FList.Free;
  finally
    AProgress.TerminateProgress;
  end;  
end;

{ TMyLongThread2 }

procedure TMyLongThread2.Execute;
begin
  while not Terminated do
  begin
    EmulateUsefullWork(2000);
    ThreadWaitTimeout(Self, 60000); // Ожидаем таймаут 60 сек
  end;
  Sleep(5000); // Оставлено для демонстрации режима "Одновременно"
end;

{ TMyLongThread3 }

constructor TMyLongThread3.Create(UsefullTaskTime: Integer);
begin
  inherited Create(False);
  FUsefullTaskTime := UsefullTaskTime;
end;

procedure TMyLongThread3.Execute;
begin
  while not Terminated do
  begin
    EmulateUsefullWork(FUsefullTaskTime);
    ThreadWaitTimeout(Self, 60000); // Ожидаем таймаут 60 сек
  end;
  Sleep(5000); // Оставлено для демонстрации режима "Одновременно"
end;

{ TMyLongThread4 }

procedure TMyLongThread4.Execute;
begin
  while not Terminated do
  begin
    EmulateUsefullWork(1000);
    ThreadWaitTimeout(Self, 60000); // Ожидаем таймаут 60 сек
  end;
  Sleep(5000); // Оставлено для демонстрации режима "Одновременно"
end;

end.
