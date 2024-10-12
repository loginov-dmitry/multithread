program Ex5Proj;

uses
  Forms,
  Ex5Unit in 'Ex5Unit.pas' {Form1},
  MTUtils in '..\CommonUtils\MTUtils.pas',
  TimeIntervals in '..\CommonUtils\TimeIntervals.pas',
  ProgressViewer in '..\CommonUtils\ProgressViewer.pas',
  LDSWaitFrm in '..\ExWaitWindow\LDSWaitFrm.pas' {LDSWaitForm},
  LDSWaitIntf in '..\ExWaitWindow\LDSWaitIntf.pas',
  ParamsUtils in '..\CommonUtils\ParamsUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
