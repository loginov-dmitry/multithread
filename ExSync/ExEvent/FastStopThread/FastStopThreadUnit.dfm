object Form2: TForm2
  Left = 0
  Top = 0
  Caption = #1044#1077#1084#1086#1085#1089#1090#1088#1072#1094#1080#1103' '#1088#1072#1073#1086#1090#1099' '#1089' '#1089#1086#1073#1099#1090#1080#1103#1084#1080
  ClientHeight = 321
  ClientWidth = 523
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 19
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 425
    Height = 57
    Caption = 
      #1055#1088#1080#1084#1077#1088' '#1076#1077#1084#1086#1085#1089#1090#1088#1080#1088#1091#1077#1090' '#1086#1088#1075#1072#1085#1080#1079#1072#1094#1080#1102' '#1084#1086#1084#1077#1085#1090#1072#1083#1100#1085#1086#1075#1086#13#10#1079#1072#1074#1077#1088#1096#1077#1085#1080#1103' '#1088#1072#1073#1086#1090 +
      #1099' '#1087#1086#1090#1086#1082#1072' '#1087#1088#1080' '#1080#1089#1087#1086#1083#1100#1079#1086#1074#1072#1085#1080#1080#13#10#1086#1073#1098#1077#1082#1090#1072' '#1089#1080#1085#1093#1088#1086#1085#1080#1079#1072#1094#1080#1080' '#1091#1088#1086#1074#1085#1103' '#1103#1076#1088#1072' '#1054#1057 +
      ': Event ('#1089#1086#1073#1099#1090#1080#1077')'
  end
  object Label2: TLabel
    Left = 8
    Top = 255
    Width = 359
    Height = 19
    Caption = #1054#1089#1090#1072#1085#1086#1074' '#1087#1086#1090#1086#1082#1072' '#1080' '#1091#1085#1080#1095#1090#1086#1078#1077#1085#1080#1077' '#1086#1073#1098#1077#1082#1090#1072' '#1079#1072#1085#1103#1083#1086':'
  end
  object labFreeThreadTime: TLabel
    Left = 376
    Top = 255
    Width = 24
    Height = 19
    Caption = '???'
  end
  object Label3: TLabel
    Left = 8
    Top = 224
    Width = 239
    Height = 19
    Caption = #1042#1099#1079#1086#1074#1086#1074' '#1084#1077#1090#1086#1076#1072'  DoUsefullWork:'
  end
  object labUseFullCalls: TLabel
    Left = 256
    Top = 224
    Width = 9
    Height = 19
    Caption = '0'
  end
  object Label4: TLabel
    Left = 8
    Top = 284
    Width = 342
    Height = 19
    Caption = #1042#1086#1079#1086#1073#1085#1086#1074#1083#1077#1085#1080#1077' '#1087#1086#1090#1086#1082#1072' '#1087#1086#1089#1083#1077' SetEvent '#1079#1072#1085#1103#1083#1086':'
  end
  object labResumeThreadAfterSetEvent: TLabel
    Left = 376
    Top = 284
    Width = 24
    Height = 19
    Caption = '???'
  end
  object btnRunThread: TButton
    Left = 72
    Top = 80
    Width = 289
    Height = 33
    Caption = #1047#1072#1087#1091#1089#1090#1080#1090#1100' '#1087#1086#1090#1086#1082
    TabOrder = 0
    OnClick = btnRunThreadClick
  end
  object btnStopThread: TButton
    Left = 72
    Top = 167
    Width = 289
    Height = 33
    Caption = #1054#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1087#1086#1090#1086#1082
    Enabled = False
    TabOrder = 1
    OnClick = btnStopThreadClick
  end
  object btnWakeUp: TButton
    Left = 72
    Top = 121
    Width = 289
    Height = 33
    Caption = #1056#1072#1079#1073#1091#1076#1080#1090#1100' '#1087#1086#1090#1086#1082
    Enabled = False
    TabOrder = 2
    OnClick = btnWakeUpClick
  end
  object cbUseProgressViewer: TCheckBox
    Left = 376
    Top = 88
    Width = 137
    Height = 17
    Caption = #1042#1080#1079#1091#1072#1083#1080#1079#1072#1094#1080#1103
    TabOrder = 3
  end
  object Timer1: TTimer
    Interval = 333
    OnTimer = Timer1Timer
    Left = 24
    Top = 80
  end
end
