object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 11
    Width = 99
    Height = 16
    Caption = #1063#1080#1089#1083#1086' '#1080#1090#1077#1088#1072#1094#1080#1081':'
  end
  object labTCritSecTime: TLabel
    Left = 255
    Top = 95
    Width = 18
    Height = 16
    Caption = '???'
  end
  object labRTLCritSecTime: TLabel
    Left = 255
    Top = 147
    Width = 18
    Height = 16
    Caption = '???'
  end
  object labMonitorTime: TLabel
    Left = 255
    Top = 198
    Width = 18
    Height = 16
    Caption = '???'
  end
  object Button1: TButton
    Left = 8
    Top = 88
    Width = 241
    Height = 33
    Caption = #1047#1072#1084#1077#1088' '#1089' TCriticalSection'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 121
    Top = 8
    Width = 121
    Height = 24
    TabOrder = 1
    Text = '100000000'
  end
  object Button2: TButton
    Left = 8
    Top = 136
    Width = 241
    Height = 41
    Caption = #1047#1072#1084#1077#1088' '#1089' TRTLCriticalSection'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 188
    Width = 241
    Height = 38
    Caption = #1047#1072#1084#1077#1088' '#1089' TMonitor'
    TabOrder = 3
    OnClick = Button3Click
  end
end
