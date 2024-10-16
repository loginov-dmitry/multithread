﻿{$IFDEF FPC}
{$MODE DELPHI}{$H+}{$CODEPAGE UTF8}
{$ENDIF}

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

unit LDSWaitFrm;
    
interface

uses
{$IFnDEF FPC}
  Windows, Messages,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, {$IfNDef FPC}ActiveX, {$EndIf}ExtCtrls, Buttons, SyncObjs, ComCtrls, ParamsUtils, LDSWaitIntf;

{$IFnDEF FPC}
{$IF RTLVersion >= 20.00}
   {$DEFINE D2009PLUS}
{$IFEND}
{$ENDIF}

type
  {$IFDEF D2009PLUS}
  // Для современных версий Delphi используется механизм анонимных функций.
  TWorkFunction = reference to function (OperType: Integer; AParams: TParamsRec; AResParams: PParamsRec; wsi: IWaitStatusInterface): Boolean;
  {$ELSE}
  // Для старых версий Delphi приходится объявлять отдельно TWorkFunction и TWorkMethod
  TWorkFunction = function (OperType: Integer; AParams: TParamsRec; AResParams: PParamsRec; wsi: IWaitStatusInterface): Boolean;
  TWorkMethod = function (OperType: Integer; AParams: TParamsRec; AResParams: PParamsRec; wsi: IWaitStatusInterface): Boolean of object;
  {$ENDIF}

  TLDSWaitForm = class(TForm)
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
    FError: string;
    FIsSuccess: Boolean;
    FStartTime: TDateTime;
    FCanClose: Boolean;
    FStatusInterface: IWaitStatusInterface;
  public
    { Public declarations }
  end;

function DoOperationInThread(AOwner: TForm; OperType: Integer; OperationName: string; AParams: TParamsRec;
  WorkFunc: TWorkFunction; ShowStopButton: Boolean; AResParams: PParamsRec = RES_PARAMS_NIL): Boolean; overload;

{$IFNDEF D2009PLUS}
// Данный вариант используется только для поддержки старых версий Delphi
function DoOperationInThread(AOwner: TForm; OperType: Integer; OperationName: string; AParams: TParamsRec;
  WorkMethod: TWorkMethod; ShowStopButton: Boolean; AResParams: PParamsRec = RES_PARAMS_NIL): Boolean; overload;
{$ENDIF} 

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

type
  TWaitStatusControl = class(TInterfacedObject, IWaitStatusInterface)
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
    FParams: TParamsRec;
    FResParams: PParamsRec;
    FWorkFunc: TWorkFunction;
    {$IFNDEF D2009PLUS}
    FWorkMethod: TWorkMethod;
    {$ENDIF}
    FForm: TForm;

    // Эвент нужен для того, чтобы доп. поток немедленно отреагирован на отображения
    // окна ожидания на экране.
    FEvent: TEvent;
    FStatusInterface: IWaitStatusInterface;
    FOperType: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AParams: TParamsRec; AResParams: PParamsRec; AWorkFunc: TWorkFunction; {$IFNDEF D2009PLUS}AWorkMethod: TWorkMethod; {$ENDIF}
      AForm: TForm; AStatusInterface: IWaitStatusInterface; OperType: Integer);
    destructor Destroy; override;
  end;

function DoOperationInThreadInternal(AOwner: TForm; OperType: Integer; OperationName: string; AParams: TParamsRec;
  WorkFunc: TWorkFunction; {$IFNDEF D2009PLUS}WorkMethod: TWorkMethod; {$ENDIF}ShowStopButton: Boolean; AResParams: PParamsRec): Boolean;
var
  AForm: TLDSWaitForm;
begin
  if GetCurrentThreadId <> MainThreadID then
    raise Exception.Create('DoOperationInThreadInternal: Вызов должен происходить из главного потока');
  if Assigned(AResParams) then
    AResParams^.Clear;

  AForm := TLDSWaitForm.Create(AOwner);
  try
    AForm.btnStop.Visible := ShowStopButton;

    AForm.labOperationName.Caption := OperationName;
    AForm.labOperationName.Left := (AForm.Width - AForm.labOperationName.Width) div 2;

    AForm.labWaitStatus.Caption := '';

    AForm.FStatusInterface := TWaitStatusControl.Create;

    AForm.AThread := TBackgroundOperationsThread.Create(AParams, AResParams, WorkFunc, {$IFNDEF D2009PLUS}WorkMethod, {$ENDIF}AForm, AForm.FStatusInterface, OperType);
    AForm.ShowModal;

    if AForm.FError <> '' then
      raise Exception.Create(AForm.FError);

    Result     := AForm.FIsSuccess;
  finally
    AForm.AThread.Free;
    AForm.Free;
  end;
end;

function DoOperationInThread(AOwner: TForm; OperType: Integer; OperationName: string; AParams: TParamsRec;
  WorkFunc: TWorkFunction; ShowStopButton: Boolean; AResParams: PParamsRec = RES_PARAMS_NIL): Boolean;
begin
  Result := DoOperationInThreadInternal(AOwner, OperType, OperationName, AParams,
    WorkFunc, {$IFNDEF D2009PLUS}nil, {$ENDIF}ShowStopButton, AResParams);
end;

{$IFNDEF D2009PLUS}
function DoOperationInThread(AOwner: TForm; OperType: Integer; OperationName: string; AParams: TParamsRec;
  WorkMethod: TWorkMethod; ShowStopButton: Boolean; AResParams: PParamsRec = RES_PARAMS_NIL): Boolean; overload;
begin
  Result := DoOperationInThreadInternal(AOwner, OperType, OperationName, AParams,
    nil, WorkMethod, ShowStopButton, AResParams);
end;
{$ENDIF}

{ TBackgroundOperationsThread }

constructor TBackgroundOperationsThread.Create(AParams: TParamsRec; AResParams: PParamsRec;
  AWorkFunc: TWorkFunction; {$IFNDEF D2009PLUS}AWorkMethod: TWorkMethod; {$ENDIF}AForm: TForm;
  AStatusInterface: IWaitStatusInterface; OperType: Integer);
const
  STATE_NONSIGNALED = FALSE;
  NOT_AUTO_RESET    = TRUE;
begin
  inherited Create(False);
  FParams    := AParams;
  FResParams := AResParams;
  FWorkFunc  := AWorkFunc;
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
    {$IfNDef FPC}CoInitialize(nil){$EndIf};
    try
      if Assigned(FWorkFunc) then
        TLDSWaitForm(FForm).FIsSuccess := FWorkFunc(FOperType, FParams, FResParams, TLDSWaitForm(FForm).FStatusInterface)
      {$IFNDEF D2009PLUS}
      else if Assigned(FWorkMethod) then
        TLDSWaitForm(FForm).FIsSuccess := FWorkMethod(FOperType, FParams, FResParams, TLDSWaitForm(FForm).FStatusInterface)
      {$ENDIF}
    finally
      {$IfNDef FPC}CoUnInitialize(){$EndIf};
    end;

  except
    on E: Exception do
      TLDSWaitForm(FForm).FError := 'Ошибка в потоке: ' + E.Message;
  end;

  // Ожидаем, когда форма появится на экране
  FEvent.WaitFor(INFINITE);

  // Выставляем форме разрешение за закрытие
  TLDSWaitForm(FForm).FCanClose := True;

  // Посылаем форме сообщение о закрытии
  SendMessage(TLDSWaitForm(FForm).Handle, {$IfDef FPC}LM_CLOSEQUERY{$Else}WM_CLOSE{$EndIf}, 0, 0);
end;

procedure TLDSWaitForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;
end;

procedure TLDSWaitForm.FormShow(Sender: TObject);
begin
  FStartTime := Now;

  // Сообщаем потоку о появлении формы на экране
  TBackgroundOperationsThread(AThread).FEvent.SetEvent;
end;

procedure TLDSWaitForm.btnStopClick(Sender: TObject);
begin
  FStatusInterface.NeedStop := True;
end;

procedure TLDSWaitForm.Timer1Timer(Sender: TObject);
var
  AMin, AMax: Integer;
begin
  lbTime.Caption := FormatDateTime('nn:ss', Now - FStartTime);
  if FStatusInterface.OperationName <> '' then
    labOperationName.Caption := FStatusInterface.OperationName;
  labOperationName.Left := (Width - labOperationName.Width) div 2;
  labWaitStatus.Caption := FStatusInterface.StatusText;
  ProgressBar1.Visible := FStatusInterface.ProgressPosition > 0;

  AMin := FStatusInterface.GetProgressMin();
  AMax := FStatusInterface.GetProgressMax();
  if (ProgressBar1.Min <> AMin) or (ProgressBar1.Max <> AMax) then
  begin
    if AMax <= AMin then AMax := AMin + 1;
    ProgressBar1.Max := MaxInt;
    ProgressBar1.Min := AMin;
    ProgressBar1.Max := AMax;
  end;
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
