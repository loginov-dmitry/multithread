{
Copyright (c) 2021, Loginov Dmitry Sergeevich
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

unit WaitFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActiveX, ExtCtrls, Buttons, SyncObjs, ComCtrls;

{$IF RTLVersion >= 20.00}
   {$DEFINE D2009PLUS}
{$IFEND}

const
  NEED_SHOW_STOP_BTN = True;
  NOT_SHOW_STOP_BTN = False;
  OPERATION_TYPE_NONE = 0;

type
  {Интерфейс можно вынести (при необходимости) в отдельный файл. Интерфейсную
   ссылку можно передавать в DLL. IID не указан, т.к. технология COM здесь не
   используется }
  TWaitStatusInterface = interface
    // private
    function GetOperationName: string;
    procedure SetOperationName(const Value: string);
    function GetStatusText: string;
    procedure SetStatusText(const Value: string);
    function GetNeedStop: Boolean;
    procedure SetNeedStop(const Value: Boolean);
    function GetStatusLine(LineNum: Integer): string;
    procedure SetStatusLine(LineNum: Integer; const Value: string);
    procedure SetProgressPosition(Value: Double);
    function GetProgressPosition: Double;

    // public
    property OperationName: string read GetOperationName write SetOperationName;
    property StatusText: string read GetStatusText write SetStatusText;
    property NeedStop: Boolean read GetNeedStop write SetNeedStop;
    property ProgressPosition: Double read GetProgressPosition write SetProgressPosition;
    property StatusLine[LineNum: Integer]: string read GetStatusLine write SetStatusLine;
    procedure ClearStatusText;
    procedure CheckNeedStop;
    procedure SetProgressMinMax(AMin, AMax: Double);
    function GetProgressMin: Integer;
    function GetProgressMax: Integer;
  end;
  
  {$IFDEF D2009PLUS}
  // Для современных версий Delphi используется механизм анонимных функций.
  TWorkFunction = reference to function (OperType: Integer; AParams: Variant; var AResParams: Variant; wsi: TWaitStatusInterface): Boolean;
  {$ELSE}
  // Для старых версий Delphi приходится объявлять отдельно TWorkFunction и TWorkMethod
  TWorkFunction = function (OperType: Integer; AParams: Variant; var AResParams: Variant; wsi: TWaitStatusInterface): Boolean;
  TWorkMethod = function (OperType: Integer; AParams: Variant; var AResParams: Variant; wsi: TWaitStatusInterface): Boolean of object;
  {$ENDIF}

  TWaitForm = class(TForm)
    Label1: TLabel;
    labOperationName: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbTime: TLabel;
    Timer1: TTimer;
    btnStop: TSpeedButton;
    labWaitStatus: TLabel;
    ProgressBar1: TProgressBar;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnStopClick(Sender: TObject);
  private
    AThread: TThread;
    FResParams: Variant;
    FError: string;
    FIsSuccess: Boolean;
    FStartTime: TDateTime;
    FCanClose: Boolean;
    FStatusInterface: TWaitStatusInterface;
  public
    { Public declarations }
  end;

function DoOperationInThread(AOwner: TForm; OperType: Integer; OperationName: string; AParams: Variant;
  WorkFunc: TWorkFunction; ShowStopButton: Boolean; var AResParams: Variant): Boolean; overload;

{$IFNDEF D2009PLUS}
// Данный вариант используется только для поддержки старых версий Delphi
function DoOperationInThread(AOwner: TForm; OperType: Integer; OperationName: string; AParams: Variant;
  WorkMethod: TWorkMethod; ShowStopButton: Boolean; var AResParams: Variant): Boolean; overload;
{$ENDIF}

implementation

{$R *.dfm}
type
  TWaitStatusControl = class(TInterfacedObject, TWaitStatusInterface)
  private
    FStatusText: TStringList;
    FOperationName: string;
    FCritSect: TCriticalSection;
    FNeedStop: Boolean;
    FProgressPosition: Integer;
    FProgressMin: Integer;
    FProgressMax: Integer;
  public
    function GetOperationName: string;
    procedure SetOperationName(const Value: string);
    function GetStatusText: string;
    procedure SetStatusText(const Value: string);
    function GetNeedStop: Boolean;
    procedure SetNeedStop(const Value: Boolean);
    function GetStatusLine(LineNum: Integer): string;
    procedure SetStatusLine(LineNum: Integer; const Value: string);
    procedure ClearStatusText;
    procedure CheckNeedStop;
    procedure SetProgressPosition(Value: Double);
    function GetProgressPosition: Double;
    procedure SetProgressMinMax(AMin, AMax: Double);
    function GetProgressMin: Integer;
    function GetProgressMax: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TBackgroundOperationsThread = class(TThread)
  public
    FParams: Variant;
    FWorkFunc: TWorkFunction;
    {$IFNDEF D2009PLUS}
    FWorkMethod: TWorkMethod;
    {$ENDIF}
    FForm: TForm;

    // Эвент нужен для того, чтобы доп. поток немедленно отреагирован на отображения
    // окна ожидания на экране.
    FEvent: TEvent;
    FStatusInterface: TWaitStatusInterface;
    FOperType: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AParams: Variant; AWorkFunc: TWorkFunction; {$IFNDEF D2009PLUS}AWorkMethod: TWorkMethod; {$ENDIF}
      AForm: TForm; AStatusInterface: TWaitStatusInterface; OperType: Integer);
    destructor Destroy; override;
  end;

function DoOperationInThreadInternal(AOwner: TForm; OperType: Integer; OperationName: string; AParams: Variant;
  WorkFunc: TWorkFunction; {$IFNDEF D2009PLUS}WorkMethod: TWorkMethod; {$ENDIF}ShowStopButton: Boolean; var AResParams: Variant): Boolean;
var
  AForm: TWaitForm;
begin
  if GetCurrentThreadId <> MainThreadID then
    raise Exception.Create('DoOperationInThreadInternal: Вызов должен происходить из главного потока');

  AForm := TWaitForm.Create(AOwner);
  try
    AForm.btnStop.Visible := ShowStopButton;

    AForm.labOperationName.Caption := OperationName;
    AForm.labOperationName.Left := (AForm.Width - AForm.labOperationName.Width) div 2;

    AForm.labWaitStatus.Caption := '';

    AForm.FStatusInterface := TWaitStatusControl.Create;

    AForm.AThread := TBackgroundOperationsThread.Create(AParams, WorkFunc, {$IFNDEF D2009PLUS}WorkMethod, {$ENDIF}AForm, AForm.FStatusInterface, OperType);
    AForm.ShowModal;

    if AForm.FError <> '' then
      raise Exception.Create(AForm.FError);

    Result     := AForm.FIsSuccess;
    AResParams := AForm.FResParams;
  finally
    AForm.AThread.Free;
    AForm.Free;
  end;
end;

function DoOperationInThread(AOwner: TForm; OperType: Integer; OperationName: string; AParams: Variant;
  WorkFunc: TWorkFunction; ShowStopButton: Boolean; var AResParams: Variant): Boolean;
begin
  Result := DoOperationInThreadInternal(AOwner, OperType, OperationName, AParams,
    WorkFunc, {$IFNDEF D2009PLUS}nil, {$ENDIF}ShowStopButton, AResParams);
end;

{$IFNDEF D2009PLUS}
function DoOperationInThread(AOwner: TForm; OperType: Integer; OperationName: string; AParams: Variant;
  WorkMethod: TWorkMethod; ShowStopButton: Boolean; var AResParams: Variant): Boolean; overload;
begin
  Result := DoOperationInThreadInternal(AOwner, OperType, OperationName, AParams,
    nil, WorkMethod, ShowStopButton, AResParams);
end;
{$ENDIF}

{ TBackgroundOperationsThread }

constructor TBackgroundOperationsThread.Create(AParams: Variant;
  AWorkFunc: TWorkFunction; {$IFNDEF D2009PLUS}AWorkMethod: TWorkMethod; {$ENDIF}AForm: TForm;
  AStatusInterface: TWaitStatusInterface; OperType: Integer);
const
  STATE_NONSIGNALED = FALSE;
  NOT_AUTO_RESET    = TRUE;
begin
  inherited Create(False);
  FParams   := AParams;
  FWorkFunc := AWorkFunc;
  {$IFNDEF D2009PLUS}
  FWorkMethod := AWorkMethod;
  {$ENDIF}
  FForm     := AForm;
  FEvent    := TEvent.Create(nil, NOT_AUTO_RESET, STATE_NONSIGNALED, '', False);
  FStatusInterface := AStatusInterface;
  FOperType := OperType;
end;

destructor TBackgroundOperationsThread.Destroy;
begin
  FEvent.SetEvent; // На всякий случай
  inherited;
  FEvent.Free;
end;

procedure TBackgroundOperationsThread.Execute;
begin
  try
    CoInitialize(nil);
    try
      if Assigned(FWorkFunc) then
        TWaitForm(FForm).FIsSuccess := FWorkFunc(FOperType, FParams, TWaitForm(FForm).FResParams, TWaitForm(FForm).FStatusInterface)
      {$IFNDEF D2009PLUS}
      else if Assigned(FWorkMethod) then
        TWaitForm(FForm).FIsSuccess := FWorkMethod(FOperType, FParams, TWaitForm(FForm).FResParams, TWaitForm(FForm).FStatusInterface)
      {$ENDIF}
    finally
      CoUnInitialize();
    end;

  except
    on E: Exception do
      TWaitForm(FForm).FError := 'Ошибка в потоке: ' + E.Message;
  end;

  // Ожидаем, когда форма появится на экране
  FEvent.WaitFor(INFINITE);

  // Выставляем форме разрешение за закрытие
  TWaitForm(FForm).FCanClose := True;

  // Посылаем форме сообщение о закрытии
  SendMessage(TWaitForm(FForm).Handle, WM_CLOSE, 0, 0);
end;

procedure TWaitForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;
end;

procedure TWaitForm.FormShow(Sender: TObject);
begin
  FStartTime := Now;

  // Сообщаем потоку о появлении формы на экране
  TBackgroundOperationsThread(AThread).FEvent.SetEvent;
end;

procedure TWaitForm.btnStopClick(Sender: TObject);
begin
  FStatusInterface.NeedStop := True;
end;

procedure TWaitForm.Timer1Timer(Sender: TObject);
begin
  lbTime.Caption := FormatDateTime('nn:ss', Now - FStartTime);
  if FStatusInterface.OperationName <> '' then
    labOperationName.Caption := FStatusInterface.OperationName;
  labOperationName.Left := (Width - labOperationName.Width) div 2;
  labWaitStatus.Caption := FStatusInterface.StatusText;
  ProgressBar1.Visible := FStatusInterface.ProgressPosition > 0;
  ProgressBar1.Min := FStatusInterface.GetProgressMin;
  ProgressBar1.Max := FStatusInterface.GetProgressMax;
  ProgressBar1.Position := Round(FStatusInterface.ProgressPosition);
end;

{ TWaitStatusControl }

procedure TWaitStatusControl.CheckNeedStop;
begin
  if FNeedStop then
    raise Exception.Create('Пользователь нажал кнопку "Отмена"');
end;

procedure TWaitStatusControl.ClearStatusText;
begin
  SetStatusText('');
end;

constructor TWaitStatusControl.Create;
begin
  inherited;
  FStatusText := TStringList.Create;
  FCritSect := TCriticalSection.Create;
  FProgressMax := 100;
end;

destructor TWaitStatusControl.Destroy;
begin
  FStatusText.Free;
  FCritSect.Free;
  inherited;
end;

function TWaitStatusControl.GetNeedStop: Boolean;
begin
  Result := FNeedStop;
end;

function TWaitStatusControl.GetOperationName: string;
begin
  FCritSect.Enter;
  Result := FOperationName;
  FCritSect.Leave;
end;

function TWaitStatusControl.GetProgressMax: Integer;
begin
  Result := FProgressMax;
end;

function TWaitStatusControl.GetProgressMin: Integer;
begin
  Result := FProgressMin;
end;

function TWaitStatusControl.GetProgressPosition: Double;
begin
  Result := FProgressPosition;
end;

function TWaitStatusControl.GetStatusLine(LineNum: Integer): string;
begin
  Result := '';
  if (LineNum < 1) or (LineNum > 4) then Exit;

  FCritSect.Enter;
  try
    if FStatusText.Count < LineNum then Exit;
    Result := FStatusText[LineNum - 1];
  finally
    FCritSect.Leave;
  end;
end;

function TWaitStatusControl.GetStatusText: string;
begin
  FCritSect.Enter;
  Result := FStatusText.Text;
  FCritSect.Leave;
end;

procedure TWaitStatusControl.SetNeedStop(const Value: Boolean);
begin
  FNeedStop := Value;
end;

procedure TWaitStatusControl.SetOperationName(const Value: string);
begin
  FCritSect.Enter;
  FOperationName := Value;
  FCritSect.Leave;
end;

procedure TWaitStatusControl.SetProgressMinMax(AMin, AMax: Double);
begin
  FProgressMin := Round(AMin);
  FProgressMax := Round(AMax);
end;

procedure TWaitStatusControl.SetProgressPosition(Value: Double);
begin
  FProgressPosition := Round(Value);
end;

procedure TWaitStatusControl.SetStatusLine(LineNum: Integer;
  const Value: string);
begin
  if (LineNum < 1) or (LineNum > 4) then Exit;

  FCritSect.Enter;
  try
    while FStatusText.Count < LineNum do
      FStatusText.Add('');
    FStatusText[LineNum - 1] := Value;
  finally
    FCritSect.Leave;
  end;
end;

procedure TWaitStatusControl.SetStatusText(const Value: string);
begin
  FCritSect.Enter;
  FStatusText.Text := Value;
  FCritSect.Leave;
end;

end.
