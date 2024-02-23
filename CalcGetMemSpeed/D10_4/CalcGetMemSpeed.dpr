program CalcGetMemSpeed;

uses
  FastMM4,
  Forms,
  CalcGetMemSpeedUnit in 'CalcGetMemSpeedUnit.pas' {Form1},
  TimeIntervals in '..\..\CommonUtils\TimeIntervals.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
