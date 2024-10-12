{$IFDEF FPC}{$CODEPAGE UTF8}{$H+}{$MODE DELPHI}{$ENDIF}
unit Ex5Unit;

interface

uses
  {$IFnDEF FPC}
    Windows, Messages, 
  {$ELSE}
    LCLIntf, LCLType, LMessages, 
  {$ENDIF}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Contnrs, MTUtils, 
    LDSWaitFrm, LDSWaitIntf, ParamsUtils;

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FList: TObjectList; // Потоки для первой и второй задачи

    function StopThreads(OperType: Integer; AParams: TParamsRec; AResParams: PParamsRec; wsi: IWaitStatusInterface): Boolean;
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

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DoOperationInThread(Self, cbTerminateMode.ItemIndex, 'Выход из программы...', ParamsEmpty, StopThreads, NOT_SHOW_STOP_BTN);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FList := TObjectList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
{
var
  AProgress: TProgressViewer;
  I: Integer;}
begin
  {Пример использования TProgressViewer для визуализации.
   Визуализация была переделана на вызов функции StopThreads через функцию DoOperationInThread
   для совместимости в Лазарусом
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
  end;}
end;

function TForm1.StopThreads(OperType: Integer; AParams: TParamsRec;
  AResParams: PParamsRec; wsi: IWaitStatusInterface): Boolean;
var
  I: Integer;
begin
  if OperType = 1 then
  begin // Выбран режим "Одновременно (быстрее)"
    // Выставляем флаг Terminated для всех потоков. Можно использовать
    // родительский класс TThread для операции приведения типов.
    for I := 0 to FList.Count - 1 do
      TThread(FList[I]).Terminate;
  end;
  // При уничтожении списка TObjectList будут уничтожены все объекты потоков
  FList.Free;
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
