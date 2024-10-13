{$IFDEF FPC}{$CODEPAGE UTF8}{$H+}{$MODE DELPHI}{$ENDIF}
unit Ex12Unit;

interface

uses
  {$IFnDEF FPC}
    Windows, Messages, 
  {$ELSE}
    LCLIntf, LCLType, LMessages, 
  {$ENDIF}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, MTUtils, ComCtrls, 
    ExtCtrls, StrUtils, Contnrs, Generics.Collections;

type
  TMyThread = class(TThread)
  private
    procedure LogEvent(EventText: string);
  protected
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    btnRunInParallelThread: TButton;
    Button1: TButton;
    ListBox1: TListBox;
    labLabLastThreadTime: TLabel;
    btnClearListBox: TButton;
    procedure btnRunInParallelThreadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnClearListBoxClick(Sender: TObject);
  private
    { Private declarations }
    FList: TObjectList<TMyThread>;
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  FList := TObjectList<TMyThread>.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FList.Free;
end;

{ TMyThread }

procedure TMyThread.Execute;
var
  I: Integer;
  CurTime: TDateTime;
begin
  CurTime := Now; // Запоминаем время ДО вызова Queue
  Queue(
    procedure
    begin
      Form1.labLabLastThreadTime.Caption :=
        'Последний поток был запущен: ' + DateTimeToStr(CurTime);
    end);

  LogEvent('Thread start');
  I := 0;
  while not Terminated do
  begin
    Inc(I);
    LogEvent('Event #' + IntToStr(I));
    ThreadWaitTimeout(Self, 500);
  end;
  LogEvent('Thread stop');
end;

procedure TMyThread.LogEvent(EventText: string);
var
  ThreadId: Cardinal;
  EventTime: TDateTime;
begin
  // Запоминаем ID потока и текущее время ДО вызова Queue
  ThreadId := GetCurrentThreadId;
  EventTime := Now;

  TThread.Queue(nil,
    procedure
    begin
      Form1.ListBox1.Items.Add(Format('%s [T:%d] - %s',
        [FormatDateTime('hh:nn:ss.zzz', EventTime), ThreadId, EventText]));
      Form1.ListBox1.ItemIndex := Form1.ListBox1.Count - 1;
    end);
end;

end.
