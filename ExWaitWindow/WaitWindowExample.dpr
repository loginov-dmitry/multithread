program WaitWindowExample;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  WaitFrm in 'WaitFrm.pas' {WaitForm},
  TimeIntervals in '..\CommonUtils\TimeIntervals.pas',
  ParamsUtils in '..\CommonUtils\ParamsUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
