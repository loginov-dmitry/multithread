object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 299
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
  object Label1: TLabel
    Left = 16
    Top = 64
    Width = 144
    Height = 16
    Caption = #1042#1089#1077#1075#1086' producer-'#1087#1086#1090#1086#1082#1086#1074':'
  end
  object labProducersCount: TLabel
    Left = 176
    Top = 64
    Width = 7
    Height = 16
    Caption = '0'
  end
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 249
    Height = 41
    Caption = #1057#1086#1079#1076#1072#1090#1100' '#1077#1097#1105' '#1087#1072#1088#1091' '#1087#1086#1090#1086#1082#1086#1074
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Tag = 1
    Left = 16
    Top = 96
    Width = 249
    Height = 41
    Caption = #1047#1072#1087#1088#1086#1089#1080#1090#1100' '#1089#1086#1089#1090#1086#1103#1085#1080#1077
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Tag = 2
    Left = 16
    Top = 143
    Width = 249
    Height = 42
    Caption = #1047#1072#1087#1088#1086#1089#1080#1090#1100' '#1076#1072#1085#1085#1099#1077
    TabOrder = 2
    OnClick = Button2Click
  end
end
