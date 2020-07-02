unit Ex7Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MTUtils, ComCtrls;

type
  TMyThread = class(TThread)
  private
    FResult: Int64;
    FCurrValue: Integer;
    procedure SetProgressParams;
    procedure SetProgressCurrValue;
  public
    MaxValue: Integer;
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    btnRunInParallelThread: TButton;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    labResult: TLabel;
    edMaxValue: TEdit;
    procedure btnRunInParallelThreadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
begin
  // Уничтожаем запущенный поток
  if Assigned(FMyThread) then
    FreeAndNil(FMyThread);

  // Создаём поток в спящем состоянии
  FMyThread := TMyThread.Create(True);

  // Запоминаем длину ряда в поле MaxValue
  FMyThread.MaxValue := StrToIntDef(edMaxValue.Text, 0);

  // Пробуждаем поток для выполнения вычислений
  FMyThread.Resume;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FMyThread.Free;
end;

{ TMyThread }

procedure TMyThread.Execute;
var
  Res: Int64;
  CurrVal: Integer;
begin
  // Выставляем параметры компонента ProgressBar1
  Synchronize(SetProgressParams);

  // Выполняем некоторые вычисления
  Res := 0;
  CurrVal := 0;
  while CurrVal < MaxValue do
  begin
    if Terminated then Break;
    Inc(CurrVal);
    Res := Res + CurrVal;

    if CurrVal mod 10000 = 0 then
    begin // Обновление прогресса выполняется только 1 раз из 10000
      FCurrValue := CurrVal;
      FResult    := Res;
      Synchronize(SetProgressCurrValue);
    end;
  end;

  // Обновляем прогресс в конце вычислений
  FCurrValue := CurrVal;
  FResult    := Res;
  Synchronize(SetProgressCurrValue);
end;

procedure TMyThread.SetProgressCurrValue;
begin
  Form1.ProgressBar1.Position := FCurrValue;
  Form1.labResult.Caption := IntToStr(FResult);
end;

procedure TMyThread.SetProgressParams;
begin
  Form1.ProgressBar1.Max := MaxValue;
  Form1.ProgressBar1.Position := 0;
  Form1.labResult.Caption := '0';
end;

end.
