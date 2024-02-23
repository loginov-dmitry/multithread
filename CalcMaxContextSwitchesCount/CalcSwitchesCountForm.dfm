object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 206
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object btnRun: TButton
    Left = 16
    Top = 16
    Width = 137
    Height = 33
    Caption = #1042#1099#1095#1080#1089#1083#1080#1090#1100
    TabOrder = 0
    OnClick = btnRunClick
  end
  object Memo1: TMemo
    Left = 16
    Top = 56
    Width = 601
    Height = 129
    TabOrder = 1
  end
  object btnClearResults: TButton
    Left = 216
    Top = 16
    Width = 153
    Height = 34
    Caption = #1056#1077#1089#1090#1072#1088#1090' '#1079#1072#1084#1077#1088#1086#1074
    TabOrder = 2
    OnClick = btnClearResultsClick
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 168
    Top = 24
  end
end
