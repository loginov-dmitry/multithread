unit SimpleLoggerUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SyncObjs, MTUtils, TimeIntervals, MTLogger;

type
  TDemoLoggerForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Button2: TButton;
    Label2: TLabel;
    Label3: TLabel;
    cbCloseApp: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DemoLoggerForm: TDemoLoggerForm;

implementation

{$R *.dfm}

procedure TDemoLoggerForm.Button1Click(Sender: TObject);
var
  ti: TTimeInterval;
  I: Integer;
  AFileName: string;
begin
  AFileName := ExtractFilePath(Application.ExeName) + 'EventsNoThread.log';
  ti.Start;
  for I := 1 to StrToInt(Edit1.Text) do
    WriteStringToTextFile(AFileName, Format('%s [P:%d T:%d] - Событие №%d',
      [FormatDateTime('dd.mm.yyyy hh:nn:ss.zzz', Now), GetCurrentProcessId, GetCurrentThreadId, I]));

  ShowMessageFmt('Время добавления событий в лог-файл: %d мс', [ti.ElapsedMilliseconds]);
end;

procedure TDemoLoggerForm.Button2Click(Sender: TObject);
var
  ti: TTimeInterval;
  I: Integer;
begin
  ti.Start;
  for I := 1 to StrToInt(Edit1.Text) do
    DefLogger.AddToLog(Format('Событие №%d', [I]));
  if cbCloseApp.Checked then
    Close
  else
    ShowMessageFmt('Время добавления событий в лог-файл: %d мс', [ti.ElapsedMilliseconds]);
end;

procedure TDemoLoggerForm.FormCreate(Sender: TObject);
begin
  AllowMessageBoxIfError := True; // Только в демонстрационных целях!!!
  CreateDefLogger(ExtractFilePath(Application.ExeName) + 'EventsInThread.log');
end;

procedure TDemoLoggerForm.FormDestroy(Sender: TObject);
begin
  FreeDefLogger;
end;

end.
