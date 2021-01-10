{
Данный пример демонстрирует использование очереди TThreadedQueue.
При запуске программы создаётся один Consumer-поток, который исполняет команды,
которые другие потоки кладут в очередь ConsumerQueue.
Также есть несколько Producer-потоков, которые хотят получить у Consumer-потока
какие-то данные. Producer-потоки периодически кладут в очередь ConsumerQueue
команды, а затем читают ответ на команду из своей очереди. В каждом Producer-потоке
создаётся своя очередь ResQueue. Ссылка на ResQueue передаётся в очередь ConsumerQueue
вместе с идентификатором команды CmdId.
Может быть любое количество Producer-потоков. Также можно запросить данные у
Consumer-потока путём нажатия соответствующей кнопки на форме.
Consumer-поток выполняет команды и возвращает ответ немедленно!

Обратите внимание, что при нажатии кнопок "Запросить состояние" и "Запросить данные"
программа сообщает время выполнения методов PushItem и PopItem в микросекундах.
Вызов метода PushItem занимает от 2х до 4х микросекунд.
Вызов метода PopItem занимает от 22 до 100 микросекунд (в среднем 80 мкс).
Это время складывается из:
  1) Consumer-поток ожидает на вызове ConsumerQueue.PopItem
  2) Consumer-поток кладёт результат в очередь ResQueue
  3) Producer-поток ожидает на вызове ResQueue.PopItem
В лучшем случае три переключения между Producer-потоком и Consumer-потоком выполняются
в рамках одного ядра CPU (в этом случае достигается максимальная скорость: 22 мкс).
Если при переключении между Producer-потоком и Consumer-потоком используются разные
ядра CPU, то скорость будет ниже (до 100 мкс).

Информацию о ходе своей работы потоки пишут в лог-файл.

Другие примеры использования очереди заданий:
- организация очереди callback-функций / анонимных функций / задач
- очередь команд контроллеру, подключенному через USB / RS-232 / RS-485
- очередь команд фискальному регистратору (очередь чеков)
- очередь на запрос информации из базы данных (если принципиально важно обойтись одним коннектом к БД)
- очередь на запрос информации у DCOM-сервера (в случае, если весь обмен с DCOM-сервером
  должен выполняться через один поток)
- очередь платежей (например через платёжный шлюз CreditPilot)
- и многое другое

}

unit ThreadedQueueUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections, MTLogger,
  MTUtils, TimeIntervals;

const
  Q_CMD_STOP_THREAD = 0;
  Q_CMD_GET_CURR_STATE = 1;
  Q_CMD_GET_LAST_DATA = 2;

type
  TQueueResult = record
    ResStr: string;
  end;

  TQueueCommand = record
    CmdId: Integer;    // Команда, которую должен исполнить поток-consumer
    //CmpParams: Variant; // При необходимости у команды могут быть доп. параметры
    ResQueue: TThreadedQueue<TQueueResult>; // Очередь, куда будет отправляться результат
  end;

  TProducerType = (
    ptQueryCurState,   // Запрос текущего времени
    ptQueryLastData);  // Запрос последних данных

  TProducerThread = class(TThread)
  private
    FProducerType: TProducerType;
    FConsumerThreadRef: TThread;
    ResQueue: TThreadedQueue<TQueueResult>;
  protected
    procedure Execute; override;
  public
    constructor Create(ProducerType: TProducerType; ConsumerThread: TThread);
    destructor Destroy; override;
  end;

  TConsumerThread = class(TThread)
  protected
    procedure Execute; override;
  public
    ConsumerQueue: TThreadedQueue<TQueueCommand>;
    constructor Create;
    destructor Destroy; override;
  end;

  TMainForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    labProducersCount: TLabel;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    // Список потоков, которые запрашивают текущее время
    QueryCurStateThreads: TObjectList<TProducerThread>;
    // Список потоков, которые запрашивают последние данные
    QueryLastDataThreads: TObjectList<TProducerThread>;
    // Поток, который исполняет команды и возвращает результат
    ConsumerThread: TConsumerThread;

    // Создаёт пару Producer-потоков
    procedure CreateProducerThreads;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
begin
  CreateProducerThreads;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  Cmd: TQueueCommand;
  Res: TQueueResult;
  ResQueue: TThreadedQueue<TQueueResult>;
  tmEv: TTimeIntervalEvents;
  QSize: Integer;
begin
  ResQueue := TThreadedQueue<TQueueResult>.Create(10);
  if TButton(Sender).Tag = 1 then
    Cmd.CmdId := Q_CMD_GET_CURR_STATE
  else
    Cmd.CmdId := Q_CMD_GET_LAST_DATA;
  Cmd.ResQueue := ResQueue;
  tmEv.StartEvent('PushItem'); // Замер времени
  ConsumerThread.ConsumerQueue.PushItem(Cmd, QSize); // Уходит от 2х до 4х мкс
  tmEv.StartEvent('PopItem');  // Замер времени
  Res := ResQueue.PopItem;                           // Уходит от 30 до 100 мкс
  tmEv.StopEvent();
  ShowMessageFmt('Consumer-поток вернул данные: %s. Ушло времени: %s; Длина очереди: %d',
    [Res.ResStr, tmEv.GetEventsAsString([eoUseMicroSec]), QSize]);
  ResQueue.Free;
end;

procedure TMainForm.CreateProducerThreads;
begin
  QueryCurStateThreads.Add(TProducerThread.Create(ptQueryCurState, ConsumerThread));
  QueryLastDataThreads.Add(TProducerThread.Create(ptQueryLastData, ConsumerThread));
  labProducersCount.Caption := IntToStr(QueryCurStateThreads.Count + QueryLastDataThreads.Count);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CreateDefLogger(Application.ExeName + '.log');
  DefLogger.AddToLog('Программа запущена');

  // Создаём consumer-поток
  ConsumerThread := TConsumerThread.Create;

  // Создаем producer-потоки
  QueryCurStateThreads := TObjectList<TProducerThread>.Create;
  QueryLastDataThreads := TObjectList<TProducerThread>.Create;
  CreateProducerThreads;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Внимание! Очень важно уничтожать потоки в правильном порядке! Сначала уничтожаем
  // producer-потоки, а затем consumer-поток

  // Останавливаем producer-потоки
  FreeAndNil(QueryCurStateThreads);
  FreeAndNil(QueryLastDataThreads);

  // Останавливаем consumer-поток
  FreeAndNil(ConsumerThread);

  DefLogger.AddToLog('Программа остановлена');
  FreeDefLogger;
end;

{ TProducerThread }

constructor TProducerThread.Create(ProducerType: TProducerType; ConsumerThread: TThread);
begin
  ResQueue := TThreadedQueue<TQueueResult>.Create(10);
  FProducerType := ProducerType;
  FConsumerThreadRef := ConsumerThread;
  inherited Create(False);
end;

destructor TProducerThread.Destroy;
begin
  Terminate;
  inherited; // Дожидаемся, когда consumer-поток вернёт результат
  ResQueue.Free;
end;

procedure TProducerThread.Execute;
var
  Cmd: TQueueCommand;
  Res: TQueueResult;
begin
  DefLogger.AddToLog('TProducerThread: Поток запущен. ProducerType=' + IntToStr(Integer(FProducerType)));
  Cmd.ResQueue := ResQueue;
  while not Terminated do
  begin
    if FProducerType = ptQueryCurState then
      Cmd.CmdId := Q_CMD_GET_CURR_STATE
    else
      Cmd.CmdId := Q_CMD_GET_LAST_DATA;

    // Добавляем команду в очередь consumer-потока
    TConsumerThread(FConsumerThreadRef).ConsumerQueue.PushItem(Cmd);
    DefLogger.AddToLog('TProducerThread: Отправлена команда в consumer-поток. CmdId=' + IntToStr(Cmd.CmdId));

    // Получаем результат обработки команды consumer-потоком. Считаем, что  consumer-поток
    // гарантированно вернет ответ на команду
    Res := ResQueue.PopItem;

    DefLogger.AddToLog('TProducerThread: consumer-поток вернул результат: ' + Res.ResStr);

    ThreadWaitTimeout(Self, Random(5000));
  end;
  DefLogger.AddToLog('TProducerThread: Поток остановлен. ProducerType=' + IntToStr(Integer(FProducerType)));
end;

{ TConsumerThread }

constructor TConsumerThread.Create;
begin
  ConsumerQueue := TThreadedQueue<TQueueCommand>.Create(10);
  inherited Create(False);
end;

destructor TConsumerThread.Destroy;
var
  Cmd: TQueueCommand;
begin
  // Для того, чтобы остановить этот поток, необходимо добавить команду в очередь
  Cmd.CmdId := Q_CMD_STOP_THREAD;
  ConsumerQueue.PushItem(Cmd);
  inherited; // Дожидаемся, когда поток обработает все команды, в том числе Q_CMD_STOP_THREAD
  ConsumerQueue.Free;
end;

procedure TConsumerThread.Execute;
var
  Cmd: TQueueCommand;
  Res: TQueueResult;
begin
  DefLogger.AddToLog('TConsumerThread запущен');
  while True do
  begin
    Cmd := ConsumerQueue.PopItem;
    DefLogger.AddToLog('TConsumerThread: принята команда CmdId=' + IntToStr(Cmd.CmdId));
    case Cmd.CmdId of
      Q_CMD_STOP_THREAD: Break; // Завершили работу потока
      Q_CMD_GET_CURR_STATE:
        begin
          Res.ResStr := 'CurrDateTime: ' + DateTimeToStr(Now);
        end;
      Q_CMD_GET_LAST_DATA:
        begin
          Res.ResStr := 'Current data count: ' + IntToStr(Random(1000));
        end;
    end;
    // Кладём результат в очередь Producer-потока
    Cmd.ResQueue.PushItem(Res);
    DefLogger.AddToLog('TConsumerThread: подготовлен ответ: ResStr=' + Res.ResStr);
  end;
  DefLogger.AddToLog('TConsumerThread остановлен');
end;

end.
