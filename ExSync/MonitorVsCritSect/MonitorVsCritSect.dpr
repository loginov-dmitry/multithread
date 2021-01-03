program MonitorVsCritSect;

uses
  Vcl.Forms,
  MonitorVsCritSectUnit1 in 'MonitorVsCritSectUnit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
