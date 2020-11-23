program SimpleLogger;

uses
  Forms,
  SimpleLoggerUnit in 'SimpleLoggerUnit.pas' {DemoLoggerForm},
  MTUtils in '..\..\..\CommonUtils\MTUtils.pas',
  TimeIntervals in '..\..\..\CommonUtils\TimeIntervals.pas',
  MTLogger in '..\..\..\CommonUtils\MTLogger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDemoLoggerForm, DemoLoggerForm);
  Application.Run;
end.
