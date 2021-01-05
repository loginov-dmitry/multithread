program ExExceptions;

uses
  Forms,
  ExceptionsFormUnit in 'ExceptionsFormUnit.pas' {ExceptionsForm},
  MTUtils in '..\CommonUtils\MTUtils.pas',
  MTLogger in '..\CommonUtils\MTLogger.pas',
  TimeIntervals in '..\CommonUtils\TimeIntervals.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TExceptionsForm, ExceptionsForm);
  Application.Run;
end.
