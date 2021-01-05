unit ExceptionsFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MTLogger, MTUtils;

type
  TMyThread = class(TThread)
  private
    LastErrTime: TDateTime;
    LastErrMsg: string;
    LastErrClass: string;
    procedure AddExceptionToGUI;
    procedure DoUsefullWork; // "Полезная" работа
  protected
    procedure Execute; override;
  end;

  TExceptionsForm = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    MyThread: TMyThread;
  public
    { Public declarations }
    procedure AddExceptionToListBox(dt: TDateTime; Msg: string; ErrClass: string);
  end;

var
  ExceptionsForm: TExceptionsForm;

implementation

{$R *.dfm}

{ TMyThread }

procedure TMyThread.AddExceptionToGUI;
begin
  ExceptionsForm.AddExceptionToListBox(LastErrTime, LastErrMsg, LastErrClass);
end;

procedure TMyThread.DoUsefullWork;
begin
  raise Exception.Create('DoUsefullWork - произошла ошибка!');
end;

procedure TMyThread.Execute;
begin
  DefLogger.AddToLog('Доп. поток запущен');
  while not Terminated do
  try
    ThreadWaitTimeout(Self, 5000);
    if not Terminated then
      DoUsefullWork;
  except
    on E: Exception do
    begin
      // Выводим ошибку в лог:
      DefLogger.AddToLog(Format('%s [%s]', [E.Message, E.ClassName]));

      // Показываем ошибку пользователю:
      LastErrTime := Now;
      LastErrMsg  := E.Message;
      LastErrClass := E.ClassName;
      Synchronize(AddExceptionToGUI);
    end;
  end;
  DefLogger.AddToLog('Доп. поток остановлен');
end;

procedure TExceptionsForm.AddExceptionToListBox(dt: TDateTime; Msg,
  ErrClass: string);
begin
  Msg := StringReplace(Msg, sLineBreak, ' ', [rfReplaceAll]);
  ListBox1.Items.Add(Format('%s - %s [%s]', [DateTimeToStr(dt), Msg, ErrClass]));
end;

procedure TExceptionsForm.Button1Click(Sender: TObject);
begin
  if MyThread = nil then
    MyThread := TMyThread.Create(False);
end;

procedure TExceptionsForm.FormCreate(Sender: TObject);
begin
  CreateDefLogger(Application.ExeName + '.log');
  DefLogger.AddToLog('Программа запущена');
end;

procedure TExceptionsForm.FormDestroy(Sender: TObject);
begin
  MyThread.Free;
  DefLogger.AddToLog('Программа остановлена');
  FreeDefLogger;
end;

end.
