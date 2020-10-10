program FastStopThread;

uses
  Forms,
  FastStopThreadUnit in 'FastStopThreadUnit.pas' {Form2},
  TimeIntervals in '..\..\..\CommonUtils\TimeIntervals.pas',
  ProgressViewer in '..\..\..\CommonUtils\ProgressViewer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
