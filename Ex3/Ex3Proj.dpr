program Ex3Proj;

uses
  Forms,
  Ex3Unit in 'Ex3Unit.pas' {Form1},
  ProgressViewer in '..\CommonUtils\ProgressViewer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
