{
Для того, чтобы определить стоимость Sleep, следует задать значение
1000 мс и наблюдать в программе ProcessExplorer за колонкой Cycles Delta - это
количество потраченных процессором тактов с секунду. Также нужно наблюдать
на показателем Context Switches - количество переключений контекста в секунду.
Каждый вызов функции Sleep генерирует одно переключение контекста.
Нужно разделить "Cycles Delta" на "Context Switches Delta", при этом
"Context Switches Delta" мы должны определить самостоятельно.
Мои тесты показывают, что стоимость Sleep(1000) составляет примерно 50000 тактов.
Вызов Sleep(10) в бесконечном цикле потребляет 2500000 тактов в секунду.
Но это может сильно зависеть от разрешения системного таймера. 
}

unit CalcWaitCostUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, RXSpin;

type
  TMyThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    RxSpinEdit1: TRxSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure RxSpinEdit1Change(Sender: TObject);
  private
    { Private declarations }
    t: TMyThread;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  APause: Integer = 50;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  t := TMyThread.Create(False);
end;

{ TMyThread }

procedure TMyThread.Execute;
begin
  inherited;
  while not Terminated do
  begin
    Sleep(APause);
  end;

end;

procedure TForm1.RxSpinEdit1Change(Sender: TObject);
begin
  APause := RxSpinEdit1.AsInteger;
end;

end.
