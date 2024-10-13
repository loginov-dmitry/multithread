{$IFDEF FPC}{$CODEPAGE UTF8}{$H+}{$MODE DELPHI}{$ENDIF}
unit NotUseThreadsUnit;

interface

uses
  {$IFnDEF FPC}
    Windows, Messages, 
  {$ELSE}
    LCLIntf, LCLType, LMessages, 
  {$ENDIF}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, SplashFormUnit;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    btnRun1: TButton;
    Label2: TLabel;
    btnRun2: TButton;
    procedure btnRun1Click(Sender: TObject);
    procedure btnRun2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.btnRun1Click(Sender: TObject);
begin
  btnRun1.Caption := 'Ждите...';
  Screen.Cursor := crSQLWait;
  try
    Sleep(10000);
  finally
    btnRun1.Caption := 'Выполнить';
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.btnRun2Click(Sender: TObject);
begin
  ShowSplashForm(Self);
  try
    Sleep(10000);
  finally
    HideSplashForm;
  end;
end;

end.
