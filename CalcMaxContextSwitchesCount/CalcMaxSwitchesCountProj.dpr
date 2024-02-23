program CalcMaxSwitchesCountProj;

uses
  Forms,
  CalcSwitchesCountForm in 'CalcSwitchesCountForm.pas' {btnRun},
  TimeIntervals in '..\CommonUtils\TimeIntervals.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
