{
��� ����, ����� ���������� ��������� Sleep, ������� ������ ��������
1000 �� � ��������� � ��������� ProcessExplorer �� �������� Cycles Delta - ���
���������� ����������� ����������� ������ � �������. ����� ����� ���������
�� ����������� Context Switches - ���������� ������������ ��������� � �������.
������ ����� ������� Sleep ���������� ���� ������������ ���������.
����� ��������� "Cycles Delta" �� "Context Switches Delta", ��� ����
"Context Switches Delta" �� ������ ���������� ��������������.
��� ����� ����������, ��� ��������� Sleep(1000) ���������� �������� 50000 ������.
����� Sleep(10) � ����������� ����� ���������� 2500000 ������ � �������.
�� ��� ����� ������ �������� �� ���������� ���������� �������. 
}

unit CalcWaitCostUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, RXSpin;

type
  TMyThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    RxSpinEdit1: TRxSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure RxSpinEdit1Change(Sender: TObject);
  private
    { Private declarations }
    t: TMyThread;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  APause: Integer = 50;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  t := TMyThread.Create(False);
end;

{ TMyThread }

procedure TMyThread.Execute;
begin
  inherited;
  while not Terminated do
  begin
    Sleep(APause);
  end;

end;

procedure TForm1.RxSpinEdit1Change(Sender: TObject);
begin
  APause := RxSpinEdit1.AsInteger;
end;

end.
