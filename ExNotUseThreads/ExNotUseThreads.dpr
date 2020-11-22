program ExNotUseThreads;

uses
  Forms,
  NotUseThreadsUnit in 'NotUseThreadsUnit.pas' {Form1},
  SplashFormUnit in 'SplashFormUnit.pas' {SplashForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
