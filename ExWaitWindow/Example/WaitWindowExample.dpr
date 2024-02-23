// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program WaitWindowExample;

uses
{$IFDEF FPC}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF} 
  Interfaces,
{$ENDIF}

  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  TimeIntervals,
  ParamsUtils,
  LDSWaitFrm in '..\LDSWaitFrm.pas' {LDSWaitForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
