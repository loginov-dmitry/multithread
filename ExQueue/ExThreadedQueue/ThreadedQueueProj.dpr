program ThreadedQueueProj;

uses
  Vcl.Forms,
  ThreadedQueueUnit in 'ThreadedQueueUnit.pas' {MainForm},
  MTLogger in '..\..\CommonUtils\MTLogger.pas',
  MTUtils in '..\..\CommonUtils\MTUtils.pas',
  TimeIntervals in '..\..\CommonUtils\TimeIntervals.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
