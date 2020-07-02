unit Ex8Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MTUtils, ComCtrls, ExtCtrls, SyncObjs, TimeIntervals;

type
  TMyThread = class(TThread)
  private
    FMaxValue: Integer;
    FResult: Int64;
    FCurrValue: Integer;

    // Информация о текущем состоянии потока
    FThreadStateInfo: string;

    function GetThreadStateInfo: string;
    procedure SetThreadStateInfo(const Value: string);
  public
    constructor Create(MaxValue: Integer);
    procedure Execute; override;
    property CalcResult: Int64 read FResult;
    property CurrValue: Integer read FCurrValue;

    // Свойство для доступа к строке FThreadStateInfo с помощью
    // потокозащищенных методов GetThreadStateInfo и SetThreadStateInfo
    property ThreadStateInfo: string read GetThreadStateInfo write SetThreadStateInfo;
  end;

  TForm1 = class(TForm)
    btnRunInParallelThread: TButton;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    labResult: TLabel;
    edMaxValue: TEdit;
    Timer1: TTimer;
    Label2: TLabel;
    labThreadStateInfo: TLabel;
    procedure btnRunInParallelThreadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    FMyThread: TMyThread;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
implementation

{$R *.dfm}

procedure TForm1.btnRunInParallelThreadClick(Sender: TObject);
var
  MaxValue: Integer;
begin
  // Уничтожаем запущенный поток
  if Assigned(FMyThread) then
    FreeAndNil(FMyThread);

  MaxValue := StrToInt(edMaxValue.Text);
  ProgressBar1.Max := MaxValue;
  ProgressBar1.Position := 0;
  labResult.Caption := '0';
  labThreadStateInfo.Caption := '???';

  FMyThread := TMyThread.Create(MaxValue);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMyThread);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if Assigned(FMyThread) then
  begin
    ProgressBar1.Position := FMyThread.CurrValue;
    labResult.Caption     := IntToStr(FMyThread.CalcResult);
    labThreadStateInfo.Caption := FMyThread.ThreadStateInfo;
  end;
end;

{ TMyThread }

constructor TMyThread.Create(MaxValue: Integer);
begin
  FMaxValue := MaxValue;
  inherited Create(False);
end;

procedure TMyThread.Execute;
begin
  ThreadStateInfo := 'Start';

  while FCurrValue < FMaxValue do
  begin
    if Terminated then Break;
    Inc(FCurrValue);
    FResult := FResult + FCurrValue;
    ThreadStateInfo := Format('Progress: %f%%',
      [FCurrValue / FMaxValue * 100]);
  end;
  ThreadStateInfo := 'Complete';
end;

function TMyThread.GetThreadStateInfo: string;
begin
  // Защищаем строку с помощью критической секции. Если её убрать,
  // то в главном потоке периодически будет возникать ошибка
  // "Invalid pointer operation" либо "Out of memory"
  StringProtectSection.Enter; // Входим в режим защиты
  Result := FThreadStateInfo;
  StringProtectSection.Leave; // Выходим из режима защиты
end;

procedure TMyThread.SetThreadStateInfo(const Value: string);
begin
  StringProtectSection.Enter; // Входим в режим защиты
  FThreadStateInfo := Value;
  StringProtectSection.Leave; // Выходим из режима защиты
end;

end.
