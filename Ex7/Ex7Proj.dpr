program Ex7Proj;

uses
  Forms,
  Ex7Unit in 'Ex7Unit.pas' {Form1},
  MTUtils in '..\CommonUtils\MTUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
