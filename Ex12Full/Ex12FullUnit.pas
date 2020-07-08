unit Ex12FullUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MTUtils, ComCtrls, ExtCtrls, StrUtils, Contnrs,
  Generics.Collections;

type
  TMyThread = class(TThread)
  private
    procedure LogEvent(EventText: string);
    procedure CallMyFuncInMainThread(param1: Integer; param2: string);
  protected
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    btnRunInParallelThread: TButton;
    Button1: TButton;
    ListBox1: TListBox;
    labLabLastThreadTime: TLabel;
    Label1: TLabel;
    cbLinkThreadToQueue: TComboBox;
    btnClearListBox: TButton;
    procedure btnRunInParallelThreadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbLinkThreadToQueueSelect(Sender: TObject);
    procedure btnClearListBoxClick(Sender: TObject);
  private
    { Private declarations }
    FList: TObjectList<TMyThread>;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  LinkThreadToQueue: Boolean = True;
implementation

{$R *.dfm}

procedure TForm1.btnClearListBoxClick(Sender: TObject);
begin
  ListBox1.Clear;
end;

procedure TForm1.btnRunInParallelThreadClick(Sender: TObject);
begin
  // Создаём и запускаем новый поток
  FList.Add(TMyThread.Create(False));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  t: TMyThread;
begin
  for t in FList do
    t.Terminate; // Сообщаем потокам о необходимости завершаться
  FList.Clear; // Уничтожаем потоки
end;

procedure TForm1.cbLinkThreadToQueueSelect(Sender: TObject);
begin
  LinkThreadToQueue := cbLinkThreadToQueue.ItemIndex = 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FList := TObjectList<TMyThread>.Create;
  ThreadWaitTimeoutSleepTime := 1;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FList.Free;
end;

{ TMyThread }

procedure TMyThread.CallMyFuncInMainThread(param1: Integer; param2: string);
begin
  TThread.Queue(nil,
    procedure
    var
      s: string;
    begin
      s := Format('param1=%d; param2=%s', [param1, param2]);
      Form1.ListBox1.Items.Add(s);
    end);
end;

procedure TMyThread.Execute;
var
  I: Integer;
  CurTime: TDateTime;
  sVal: string;
begin
  // Делаем низкий приоритет потоку, чтобы выделялим минимальные
  // кванты времени. Так быстрее проявляется проблема очистки
  // очереди при уничтожении потока
  Priority := tpLowest;

  CurTime := Now; // Запоминаем время ДО вызова Queue
  Queue(
    procedure
    begin
      Form1.labLabLastThreadTime.Caption :=
        'Последний поток был запущен: ' + DateTimeToStr(CurTime);
    end);


  {I := 111;
  sVal := 'Text 1';
  CallMyFuncInMainThread(I, sVal);
  I := 222;
  sVal := 'Text 2';
  CallMyFuncInMainThread(I, sVal); }


  I := 111;
  sVal := 'Text 1';
  TThread.Queue(nil,
    procedure
    var
      s: string;
    begin
      s := Format('param1=%d; param2=%s', [I, sVal]);
      Form1.ListBox1.Items.Add(s);
    end);

  I := 222;
  sVal := 'Text 2';
  TThread.Queue(nil,
    procedure
    var
      s: string;
    begin
      s := Format('param1=%d; param2=%s', [I, sVal]);
      Form1.ListBox1.Items.Add(s);
    end);


  LogEvent('Thread start');
  while not Terminated do
  begin
    Inc(I);
    LogEvent('Event #' + IntToStr(I));
    ThreadWaitTimeout(Self, 500);
  end;

  LogEvent('Thread stop');
  for I := 1 to 100 do
  begin
    LogEvent('Thread stop' + I.ToString);
    Sleep(0); // Делаем активным другой поток
  end;
end;

procedure TMyThread.LogEvent(EventText: string);
var
  ThreadId: Cardinal;
  EventTime: TDateTime;
  ThreadRef: TMyThread;
begin
  // Запоминаем ID потока и текущее время ДО вызова Queue
  ThreadId := GetCurrentThreadId;
  EventTime := Now;

  if LinkThreadToQueue then
    ThreadRef := Self
  else
    ThreadRef := nil;

  TThread.Queue(ThreadRef,
    procedure
    begin
      Form1.ListBox1.Items.Add(Format('%s [T:%d] - %s',
        [FormatDateTime('hh:nn:ss.zzz', EventTime), ThreadId, EventText]));
      Form1.ListBox1.ItemIndex := Form1.ListBox1.Count - 1;

      // Подвешиваем основной поток
      Sleep(5);
    end);
end;

end.
