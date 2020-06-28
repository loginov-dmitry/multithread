object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 127
  ClientWidth = 251
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object btnRunInParallelThread: TButton
    Left = 8
    Top = 64
    Width = 235
    Height = 33
    Caption = #1042#1099#1087#1086#1083#1085#1080#1090#1100' '#1074' '#1087#1072#1088#1072#1083#1083#1077#1083#1100#1085#1086#1084' '#1087#1086#1090#1086#1082#1077
    TabOrder = 0
    OnClick = btnRunInParallelThreadClick
  end
  object btnRunInMainThread: TButton
    Left = 8
    Top = 8
    Width = 235
    Height = 33
    Caption = #1042#1099#1087#1086#1083#1085#1080#1090#1100' '#1074' '#1075#1083#1072#1074#1085#1086#1084' '#1087#1086#1090#1086#1082#1077
    TabOrder = 1
    OnClick = btnRunInMainThreadClick
  end
end
