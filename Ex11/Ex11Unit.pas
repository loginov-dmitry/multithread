{$IFDEF FPC}{$CODEPAGE UTF8}{$H+}{$MODE DELPHI}{$ENDIF}
unit Ex11Unit;

interface

uses
  {$IFnDEF FPC}
    Windows, Messages, 
  {$ELSE}
    LCLIntf, LCLType, LMessages, 
  {$ENDIF}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, MTUtils, ComCtrls, 
    ExtCtrls, StrUtils, Contnrs;

type
  TLogMessage = class
    ThreadId: Integer;
    EventTime: TDateTime;
    EventText: string;
    constructor Create(AThreadId: Integer; AEventTime: TDateTime;
      AEventText: string);
  end;

  TMyThread = class(TThread)
  private
    procedure LogEvent(s: string);
  protected
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    btnRunInParallelThread: TButton;
    Timer1: TTimer;
    Button1: TButton;
    ListBox1: TListBox;
    procedure btnRunInParallelThreadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FList: TObjectList;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ThreadCounter: Integer;
  EventList: TThreadList;
implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.btnRunInParallelThreadClick(Sender: TObject);
begin
  // Создаём и запускаем новый поток
  FList.Add(TMyThread.Create(False));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FList.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FList := TObjectList.Create;
  EventList := TThreadList.Create;;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FList.Free;

  // В списке могут остаться элементы. Это не страшно, поскольку
  // выполняется выход из программы
  EventList.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  L, TmpList: TList;
  I: Integer;
  m: TLogMessage;
  Cnt: Integer;
begin
  // Определяем, есть ли элементы в списке EventList
  L := EventList.LockList;
  try
    Cnt := L.Count;
  finally
    EventList.UnlockList;
  end;

  if Cnt = 0 then Exit;

  TmpList := TList.Create;
  try
    L := EventList.LockList;
    try
      // Переносим все элементы во временный список
      TmpList.Assign(L);
      // Очищаем список EventList
      L.Clear;
    finally
      // Как можно быстрее снимаем блокировку списка
      EventList.UnlockList;
    end;

    // Дальше обрабатываем элементы из временного списка
    for I := 0 to TmpList.Count - 1 do
    begin
      m := TLogMessage(TmpList[I]);
      if ListBox1.Count < 50000 then
        ListBox1.Items.Add(Format('%s [T:%d] - %s',
          [FormatDateTime('hh:nn:ss.zzz', m.EventTime), m.ThreadId, m.EventText]));
      // Здесь необходимо уничтожить объект TLogMessage
      m.Free;
    end;

    ListBox1.ItemIndex := ListBox1.Count - 1;
  finally
    TmpList.Free;
  end;
end;

{ TMyThread }

procedure TMyThread.Execute;
var
  I: Integer;
begin
  I := 0;
  LogEvent('Thread start');
  while not Terminated do
  begin
    Inc(I);
    LogEvent('Event #' + IntToStr(I));
    ThreadWaitTimeout(Self, 1000);
  end;
  LogEvent('Thread stop');
end;

procedure TMyThread.LogEvent(s: string);
var
  L: TList;
  m: TLogMessage;
begin
  m := TLogMessage.Create(GetCurrentThreadId, Now, s);
  L := EventList.LockList;
  L.Add(m);
  EventList.UnlockList;
end;

{ TLogMessage }

constructor TLogMessage.Create(AThreadId: Integer; AEventTime: TDateTime;
  AEventText: string);
begin
  ThreadId  := AThreadId;
  EventTime := AEventTime;
  EventText := AEventText;
end;

end.
