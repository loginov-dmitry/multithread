{$IFDEF FPC}{$CODEPAGE UTF8}{$H+}{$MODE DELPHI}{$ENDIF}
unit Ex10Unit;

interface

uses
  {$IFnDEF FPC}
    Windows, Messages, 
  {$ELSE}
    LCLIntf, LCLType, LMessages, {$IFDEF MSWINDOWS}Windows, Messages,{$ENDIF}
  {$ENDIF}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, MTUtils, ComCtrls, 
    ExtCtrls, StrUtils;

const
  UM_PROGRESS_CHANGE = WM_USER + 2;

type
  TProgressData = class
  public
    CurrValue: Integer;
    CalcResult: Int64;
    ThreadStateInfo: string;
    constructor Create(ACurrValue: Integer; ACalcResult: Int64;
      AThreadStateInfo: string);
  end;

  TMyThread = class(TThread)
  private
    FFormHandle: THandle;
    FMaxValue: Integer;
  public
    EndWork: Boolean;
    procedure Execute; override;
    constructor Create(AMaxValue: Integer; AFormHandle: THandle);
  end;

  TForm1 = class(TForm)
    btnRunInParallelThread: TButton;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    labResult: TLabel;
    edMaxValue: TEdit;
    Label2: TLabel;
    labThreadStateInfo: TLabel;
    Label3: TLabel;
    labPostMsgProcessCount: TLabel;
    Label4: TLabel;
    labPostMsgCount: TLabel;
    Timer1: TTimer;
    Label5: TLabel;
    labEndWork: TLabel;
    procedure btnRunInParallelThreadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    FMyThread: TMyThread;
    FPostMsgProcessCount: Integer;
    procedure UMProgressChange(var Msg: {$IFnDEF FPC}TMessage{$ELSE}TLMessage{$ENDIF}); message UM_PROGRESS_CHANGE;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  PostMessageCount: Integer;
implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

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
  FPostMsgProcessCount := 0;
  PostMessageCount := 0;
  labResult.Caption := '0';
  labThreadStateInfo.Caption := 'Start';

  // Создаём и запускаем новый поток
  FMyThread := TMyThread.Create(MaxValue, Handle);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FMyThread.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  labPostMsgProcessCount.Caption := IntToStr(FPostMsgProcessCount);
  labPostMsgCount.Caption := IntToStr(PostMessageCount);
  if Assigned(FMyThread) then
    labEndWork.Caption := IfThen(FMyThread.EndWork, 'ДА', 'НЕТ');
end;

procedure TForm1.UMProgressChange(var Msg: {$IFnDEF FPC}TMessage{$ELSE}TLMessage{$ENDIF});
var
  ProgressData: TProgressData;
begin
  ProgressData := TProgressData(Msg.WParam);
  ProgressBar1.Position      := ProgressData.CurrValue;
  labResult.Caption          := IntToStr(ProgressData.CalcResult);
  labThreadStateInfo.Caption := ProgressData.ThreadStateInfo;
  Inc(FPostMsgProcessCount);
  // Здесь необходимо уничтожить объект TProgressData:
  ProgressData.Free;
end;

{ TMyThread }

constructor TMyThread.Create(AMaxValue: Integer; AFormHandle: THandle);
begin
  inherited Create(False);
  FMaxValue := AMaxValue;
  FFormHandle := AFormHandle;
end;

procedure TMyThread.Execute;
var
  CurrVal: Integer;
  CalcResult: Int64;
  ThreadStateInfo: string;
  ProgressData: TProgressData;
begin
  CurrVal := 0;
  CalcResult := 0;
  // Выполняем некоторые вычисления
  while CurrVal < FMaxValue do
  begin
    if Terminated then Break;
    Inc(CurrVal);
    CalcResult := CalcResult + CurrVal;
    ThreadStateInfo := Format('Progress: %f%%', [CurrVal / FMaxValue * 100]);

    // Обновление прогресса выполняется только 1 раз из 10000
    if CurrVal mod 10000 = 0 then
    begin
      // Создаём объект ProgressData непосредственно перед PostMessage
      ProgressData := TProgressData.Create(CurrVal, CalcResult, ThreadStateInfo);
      PostMessage(FFormHandle, UM_PROGRESS_CHANGE, WPARAM(ProgressData), 0);
      Inc(PostMessageCount);
    end;
  end;

  // Обновляем прогресс в конце вычислений
  ProgressData := TProgressData.Create(CurrVal, CalcResult, ThreadStateInfo);
  PostMessage(FFormHandle, UM_PROGRESS_CHANGE, WPARAM(ProgressData), 0);
  Inc(PostMessageCount);

  // Этот флаг необходим, чтобы главный поток мог убедиться, что
  // доп. поток отработал корректно до последнего
  EndWork := True;
end;

{ TProgressData }

constructor TProgressData.Create(ACurrValue: Integer; ACalcResult: Int64;
  AThreadStateInfo: string);
begin
  CurrValue       := ACurrValue;
  CalcResult      := ACalcResult;
  ThreadStateInfo := AThreadStateInfo;
end;

end.
