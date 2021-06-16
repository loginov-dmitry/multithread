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
  Dialogs, ExtCtrls, StdCtrls, WaitFrm, TimeIntervals;

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
    function PrintKKMCheck(OperType: Integer; AParams: Variant; var AResParams: Variant;
      wsi: TWaitStatusInterface): Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

function BankOperation(OperType: Integer; AParams: Variant; var AResParams: Variant; wsi: TWaitStatusInterface): Boolean;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function BankOperation(OperType: Integer; AParams: Variant; var AResParams: Variant; wsi: TWaitStatusInterface): Boolean;
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
  AResParams := VarArrayOf(['VISA****8077']);
  Result := True;
end;

function SaveTransactionInDB(OperType: Integer; AParams: Variant; var AResParams: Variant; wsi: TWaitStatusInterface): Boolean;
var
  TovarName, CardNum: string;
  Summa: Currency;
begin
  TovarName := AParams[0];
  Summa := AParams[1];
  CardNum := AParams[3];
  wsi.StatusLine[1] := Format('Товар: %s; %fр.; Карта: %s', [TovarName, Summa, CardNum]);
  Sleep(2000);
  Result := True;
end;

function FastOperation(OperType: Integer; AParams: Variant; var AResParams: Variant; wsi: TWaitStatusInterface): Boolean;
begin
  Result := True;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  ti: TTimeInterval;
  ResParams: Variant;
begin
  ti.Start;
  {$IFDEF D2009PLUS}
  // Демонстрация использования анонимной функции
  DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Быстрая операция', Null,
    function (OperType: Integer; AParams: Variant; var AResParams: Variant; wsi: TWaitStatusInterface): Boolean
    begin
      Result := True;
    end, NOT_SHOW_STOP_BTN, ResParams);
  {$ELSE}
  DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Быстрая операция', Null, FastOperation, NOT_SHOW_STOP_BTN, ResParams);
  {$ENDIF}
  ShowMessageFmt('Время выполнения операции: %d мс', [ti.ElapsedMilliseconds]);
end;

function ProgressOperation(OperType: Integer; AParams: Variant; var AResParams: Variant; wsi: TWaitStatusInterface): Boolean;
var
  I: Integer;
begin
  wsi.SetProgressMinMax(100, 600);
  for I := 100 to 600 do
  begin
    wsi.StatusLine[1] := 'Текущее значение: ' + IntToStr(I);
    wsi.ProgressPosition := I;
    Sleep(10);
    wsi.CheckNeedStop;
  end;
  Result := True;
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  ResParams: Variant;
begin
  DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Длительные вычисления', Null, ProgressOperation, NEED_SHOW_STOP_BTN, ResParams);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

function TMainForm.PrintKKMCheck(OperType: Integer; AParams: Variant; var AResParams: Variant; wsi: TWaitStatusInterface): Boolean;
var
  TovarName, CardNum: string;
  Summa: Currency;
begin
  TovarName := AParams[0];
  Summa := AParams[1];
  CardNum := AParams[3];
  wsi.StatusLine[1] := Format('Товар: %s; %fр.; Карта: %s', [TovarName, Summa, CardNum]);
  Sleep(2000);
  wsi.OperationName := 'Закрытие чека ККМ';
  Sleep(1000);
  Result := True;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  Summa: Currency;
  ResParams: Variant;
  CardNum, TovarName: string;
  PayType: string;
begin
  TovarName := 'Молоко';
  Summa := 51.23;
  PayType := 'ByCard';
  if DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Операция с банковской картой', VarArrayOf([Summa]), BankOperation, NEED_SHOW_STOP_BTN, ResParams) then
  begin
    CardNum := ResParams[0];
    DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Сохранение транзакции в БД', VarArrayOf([TovarName, Summa, PayType, CardNum]), SaveTransactionInDB,  NOT_SHOW_STOP_BTN, ResParams);
    DoOperationInThread(Self, OPERATION_TYPE_NONE, 'Печать чека ККМ', VarArrayOf([TovarName, Summa, PayType, CardNum]), PrintKKMCheck, NOT_SHOW_STOP_BTN, ResParams);
  end;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin  
  labCurTime.Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss', Now);
end;

end.
