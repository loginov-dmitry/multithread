program Ex5Proj;

uses
  Forms,
  Ex5Unit in 'Ex5Unit.pas' {Form1},
  MTUtils in '..\CommonUtils\MTUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
