{$IFDEF FPC}{$CODEPAGE UTF8}{$H+}{$MODE DELPHI}{$ENDIF}
unit SplashFormUnit;

interface

uses
  {$IFnDEF FPC}
    Windows, Messages, 
  {$ELSE}
    LCLIntf, LCLType, LMessages, 
  {$ENDIF}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TSplashForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowSplashForm(AOwner: TForm);
procedure HideSplashForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

var
  GlobalSplashForm: TSplashForm;

procedure ShowSplashForm(AOwner: TForm);
begin
  GlobalSplashForm := TSplashForm.Create(AOwner);
  GlobalSplashForm.Show;
  GlobalSplashForm.Refresh;
end;

procedure HideSplashForm;
begin
  FreeAndNil(GlobalSplashForm);
end;

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  BorderStyle := bsNone;          // Форма без рамки
  Position    := poOwnerFormCenter;  // Форма отобразиться в середине формы-владельца
  FormStyle   := fsStayOnTop;
end;

end.
