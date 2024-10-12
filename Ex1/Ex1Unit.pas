{$IFDEF FPC}{$CODEPAGE UTF8}{$H+}{$MODE DELPHI}{$ENDIF}
unit Ex1Unit;

interface

uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  {$IFDEF FPC}LCLIntf, LCLType, LMessages, LConvEncoding,{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TMyThread = class(TThread)
  public
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    btnRunInParallelThread: TButton;
    btnRunInMainThread: TButton;
    procedure btnRunInParallelThreadClick(Sender: TObject);
    procedure btnRunInMainThreadClick(Sender: TObject);
  private
    { Private declarations }
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

function DoLongCalculations: Int64;
var
  I: Integer;
begin
  Result := 0;
  // Очень длинный цикл. Имитирует длительные вычисления.
  for I := 1 to MaxInt do
    Result := Result + Random(1000);
end;

procedure MyShowMessage(Msg: string);
begin
  {$IFDEF FPC}Msg := UTF8ToCP1251(Msg);{$ENDIF}
  Windows.MessageBox(0, PChar(Msg), '', MB_OK);
end;

procedure TForm1.btnRunInParallelThreadClick(Sender: TObject);
begin
  // Запускает параллельный поток
  TMyThread.Create(False);
end;

{ TMyThread }

procedure TMyThread.Execute;
var
  V: Int64;
begin
  FreeOnTerminate := True;
  V := DoLongCalculations;
  MyShowMessage('Результат: ' + IntToStr(V));
end;

procedure TForm1.btnRunInMainThreadClick(Sender: TObject);
var
  V: Int64;
begin
  V := DoLongCalculations;
  MyShowMessage('Результат: ' + IntToStr(V));
end;

end.
