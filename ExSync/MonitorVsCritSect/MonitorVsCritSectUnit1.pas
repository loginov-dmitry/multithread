unit MonitorVsCritSectUnit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, SyncObjs;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    labTCritSecTime: TLabel;
    Button2: TButton;
    labRTLCritSecTime: TLabel;
    Button3: TButton;
    labMonitorTime: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  cs: TCriticalSection;
  tc: DWORD;
begin
  cs := TCriticalSection.Create;
  tc := GetTickCount;
  for I := 1 to StrToInt(Edit1.Text) do
  begin
    cs.Enter;
    cs.Leave;
  end;
  tc := GetTickCount - tc;
  cs.Free;

  labTCritSecTime.Caption := tc.ToString + ' ms';

end;

procedure TForm1.Button2Click(Sender: TObject);
var
  cs: TRTLCriticalSection;
  I: Integer;
  tc: DWORD;
begin
  InitializeCriticalSection(cs);
  tc := GetTickCount;
  for I := 1 to StrToInt(Edit1.Text) do
  begin
    EnterCriticalSection(cs);
    LeaveCriticalSection(cs)
  end;
  tc := GetTickCount - tc;
  DeleteCriticalSection(cs);
  labRTLCritSecTime.Caption := tc.ToString + ' ms';
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  I: Integer;
  Lock: TObject;
  tc: DWORD;
begin
  Lock := TObject.Create;
  tc := GetTickCount;
  for I := 1 to StrToInt(Edit1.Text) do
  begin
    System.TMonitor.Enter(Lock);
    System.TMonitor.Exit(Lock);
  end;
  tc := GetTickCount - tc;
  Lock.Free;

  labMonitorTime.Caption := tc.ToString + ' ms';
end;

end.
