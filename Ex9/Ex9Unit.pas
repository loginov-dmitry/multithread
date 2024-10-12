{$IFDEF FPC}{$CODEPAGE UTF8}{$H+}{$MODE DELPHI}{$ENDIF}
unit Ex9Unit;

interface

uses
  {$IFnDEF FPC}
    Windows, Messages, 
  {$ELSE}
    LCLIntf, LCLType, LMessages, {$IFDEF MSWINDOWS}Windows, Messages,{$ENDIF}
  {$ENDIF}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, MTUtils, ComCtrls;

const
  UM_PROGRESS_INIT   = WM_USER + 1;
  UM_PROGRESS_CHANGE = WM_USER + 2;

type
  TProgressData = class
    CurrValue: Integer;
    CalcResult: Int64;
    ThreadStateInfo: string;
  end;

  TMyThread = class(TThread)
  private
    ProgressData: TProgressData;
    FFormHandle: THandle;
    FMaxValue: Integer;
  public
    procedure Execute; override;
    constructor Create(AMaxValue: Integer; AFormHandle: THandle);
    destructor Destroy; override;
  end;

  TForm1 = class(TForm)
    btnRunInParallelThread: TButton;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    labResult: TLabel;
    edMaxValue: TEdit;
    Label2: TLabel;
    labThreadStateInfo: TLabel;
    procedure btnRunInParallelThreadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FMyThread: TMyThread;
    procedure UMProgressInit(var Msg: {$IFnDEF FPC}TMessage{$ELSE}TLMessage{$ENDIF}); message UM_PROGRESS_INIT;
    procedure UMProgressChange(var Msg: {$IFnDEF FPC}TMessage{$ELSE}TLMessage{$ENDIF}); message UM_PROGRESS_CHANGE;
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

procedure TForm1.btnRunInParallelThreadClick(Sender: TObject);
begin
  // Уничтожаем запущенный поток
  if Assigned(FMyThread) then
    FreeAndNil(FMyThread);

  // Создаём и запускаем новый поток
  FMyThread := TMyThread.Create(StrToInt(edMaxValue.Text), Handle);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FMyThread.Free;
end;

procedure TForm1.UMProgressChange(var Msg: {$IFnDEF FPC}TMessage{$ELSE}TLMessage{$ENDIF});
var
  ProgressData: TProgressData;
begin
  ProgressData := TProgressData(Msg.WParam);
  ProgressBar1.Position      := ProgressData.CurrValue;
  labResult.Caption          := IntToStr(ProgressData.CalcResult);
  labThreadStateInfo.Caption := ProgressData.ThreadStateInfo;
end;

procedure TForm1.UMProgressInit(var Msg: {$IFnDEF FPC}TMessage{$ELSE}TLMessage{$ENDIF});
var
  MaxValue: Integer;
begin
  MaxValue := Msg.WParam;
  ProgressBar1.Max := MaxValue;
  ProgressBar1.Position := 0;
  labResult.Caption := '0';
  labThreadStateInfo.Caption := 'Start';
end;

{ TMyThread }

constructor TMyThread.Create(AMaxValue: Integer; AFormHandle: THandle);
begin
  inherited Create(False);
  FMaxValue := AMaxValue;
  FFormHandle := AFormHandle;
  ProgressData := TProgressData.Create;
end;

destructor TMyThread.Destroy;
begin
  //ProgressData.Free; - НЕЛЬЗЯ ТУТ!
  inherited;
  ProgressData.Free;
end;

procedure TMyThread.Execute;
var
  CurrVal: Integer;
begin
  {$IFDEF LINUX}
   Внимание! Пример не будет работать корректно, т.к. в Линуксе вызовы SendMessage,
   отправленные из параллельного потока будут обработаны в этом же потоке, а не в
   главном потоке!
  {$ENDIF}
  // Выставляем параметры компонента ProgressBar1
  SendMessage(FFormHandle, UM_PROGRESS_INIT, FMaxValue, 0);
  ThreadWaitTimeout(Self, 1000); // Просто пауза 1 сек.
  CurrVal := 0;
  // Выполняем некоторые вычисления
  while CurrVal < FMaxValue do
  begin
    if Terminated then Break;
    Inc(CurrVal);
    ProgressData.CurrValue := CurrVal;
    ProgressData.CalcResult := ProgressData.CalcResult + CurrVal;
    ProgressData.ThreadStateInfo := Format('Progress: %f%%',
      [CurrVal / FMaxValue * 100]);

    // Обновление прогресса выполняется только 1 раз из 10000
    if CurrVal mod 10000 = 0 then
      SendMessage(FFormHandle, UM_PROGRESS_CHANGE, WPARAM(ProgressData), 0);
  end;

  // Обновляем прогресс в конце вычислений
  SendMessage(FFormHandle, UM_PROGRESS_CHANGE, WPARAM(ProgressData), 0);
end;

end.
