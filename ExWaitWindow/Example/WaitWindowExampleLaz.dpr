program WaitWindowExampleLaz;

uses
{$IFDEF FPC}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF} 
  Interfaces,
{$ENDIF}

  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  LDSWaitFrm in '..\LDSWaitFrm.pas', LDSWaitIntf, TimeIntervals {LDSWaitForm};

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
