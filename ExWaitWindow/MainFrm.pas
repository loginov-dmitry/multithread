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

unit MainFrm;

interface

{$IF RTLVersion >= 20.00}
   {$DEFINE D2009PLUS}
{$IFEND}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, WaitFrm, TimeIntervals, ParamsUtils;

type
  TMainForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    labCurTime: TLabel;
    Timer1: TTimer;
    Button1: TButton;
    Button2: TButton;
    Label3: TLabel;
    Button3: TButton;
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function PrintKKMCheck(OperType: Integer; par: TParamsRec; AResParams: PParamsRec;
      wsi: TWaitStatusInterface): Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

function BankOperation(OperType: Integer; AParams: TParamsRec; AResParams: PParamsRec; wsi: TWaitStatusInterface): Boolean;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function BankOperation(OperType: Integer; AParams: TParamsRec; AResParams: PParamsRec; wsi: TWaitStatusInterface): Boolean;
begin
  wsi.StatusLine[1] := 'Вставьте/приложите карту';
  Sleep(2000);
  //raise Exception.Create('BankOperation error!');
  wsi.CheckNeedStop();
  wsi.StatusLine[1] := 'Введите ПИН-КОД';
  Sleep(1000);
  wsi.CheckNeedStop();
  wsi.StatusLine[2] := '*';
  Sleep(500);
  wsi.CheckNeedStop();
  wsi.StatusLine[2] := '**';
  Sleep(500);
  wsi.CheckNeedStop();
  wsi.StatusLine[2] := '***';
  Sleep(500);
  wsi.CheckNeedStop();
  wsi.StatusLine[2] := '****';
  Sleep(500);
  wsi.CheckNeedStop();
  wsi.StatusLine[2] := '';
  wsi.StatusLine[1] := 'Выполняется соединение с банком...';
  Sleep(2000);
  wsi.StatusLine[1] := 'Операция проведена успешно!';
  Sleep(1000);
  wsi.StatusLine[1] := 'Извлеките карту';
  Sleep(1000);
  if Assigned(AResParams) then
  begin
    AResParams.SetParam('CardNum', 'VISA****8077');
    AResParams.SetParam('OperTime', Now);
    //AResParams.AddParams(['CardNum', 'VISA****8077']);
    //AResParams.AddParams(['OperTime', Now]);
  end;
  Result := True;
end;

function SaveTransactionInDB(OperType: Integer; par: TParamsRec; AResParams: PParamsRec; wsi: TWaitStatusInterface): Boolean;
begin
  wsi.StatusLine[1] := Format('Товар: %s; %fр.; Карта: %s', [par.S('TovarName'), par.C('Summa'), par.S('CardNum')]);
  if par.HasParam('OperTime') then
    wsi.StatusLine[2] := Format('Время банк. операции: %s', [par.S('OperTime')]);
  Sleep(2000);
  Result := True;
end;

function FastOperation(OperType: Integer; AParams: TParamsRec; AResParams: PParamsRec; wsi: TWaitStatusInterface): Boolean;
begin
  Result := True;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  ti: TTimeInterval;
begin
  ti.Start;
  {$IFDEF D2009PLUS}
  // Демонстрация использования анонимной функции
  DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Быстрая операция', ParamsEmpty,
    function (OperType: Integer; par: TParamsRec; AResParams: PParamsRec; wsi: TWaitStatusInterface): Boolean
    begin
      Result := True;
    end, NOT_SHOW_STOP_BTN);
  {$ELSE}
  DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Быстрая операция', ParamsEmpty, FastOperation, NOT_SHOW_STOP_BTN);
  {$ENDIF}
  ShowMessageFmt('Время выполнения операции: %d мс', [ti.ElapsedMilliseconds]);
end;

function ProgressOperation(OperType: Integer; par: TParamsRec; AResParams: PParamsRec; wsi: TWaitStatusInterface): Boolean;
var
  I: Integer;
begin
  wsi.SetProgressMinMax(par.I('Min'), par.I('Max'));
  wsi.StatusLine[2] := Format('Min=%d; Max=%d', [par.I('Min'), par.I('Max')]);
  for I := par.I('Min') to par.I('Max') do
  begin
    wsi.StatusLine[1] := 'Текущее значение: ' + IntToStr(I);
    wsi.ProgressPosition := I;
    Sleep(10);
    wsi.CheckNeedStop;
  end;
  Result := True;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Длительные вычисления',
    TParamsRec.Build(['Min', 300, 'Max', 700]),
    ProgressOperation, NEED_SHOW_STOP_BTN);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

function TMainForm.PrintKKMCheck(OperType: Integer; par: TParamsRec; AResParams: PParamsRec; wsi: TWaitStatusInterface): Boolean;
begin
  wsi.StatusLine[1] := Format('Товар: %s; %fр.; Карта: %s', [par.S('TovarName'), par.C('Summa'), par.S('CardNum')]);
  Sleep(2000);
  wsi.OperationName := 'Закрытие чека ККМ';
  Sleep(1000);
  Result := True;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  Summa: Currency;
  CardNum, TovarName: string;
  PayType: string;
  par, ResParams: TParamsRec;
begin
  TovarName := 'Молоко';
  Summa := 51.23;
  PayType := 'ByCard';
  if DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Операция с банковской картой',
    TParamsRec.Build(['Summa', Summa]), BankOperation, NEED_SHOW_STOP_BTN, @ResParams) then
  begin
    CardNum := ResParams.S('CardNum');
    par.AddParams(['TovarName', TovarName, 'Summa', Summa, 'PayType', PayType, 'CardNum', CardNum, 'OperTime', ResParams.DT('OperTime')]);
    DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Сохранение транзакции в БД', par, SaveTransactionInDB,  NOT_SHOW_STOP_BTN);
    DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Печать чека ККМ', par, PrintKKMCheck, NOT_SHOW_STOP_BTN);
  end;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin  
  labCurTime.Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss', Now);
end;

end.
