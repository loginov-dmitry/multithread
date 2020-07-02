object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 181
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  DesignSize = (
    452
    181)
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 72
    Width = 64
    Height = 16
    Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090':'
  end
  object labResult: TLabel
    Left = 78
    Top = 72
    Width = 7
    Height = 16
    Caption = '0'
  end
  object Label2: TLabel
    Left = 8
    Top = 96
    Width = 119
    Height = 16
    Caption = #1057#1090#1072#1090#1091#1089' '#1074#1099#1095#1080#1089#1083#1077#1085#1080#1081':'
  end
  object labThreadStateInfo: TLabel
    Left = 132
    Top = 97
    Width = 18
    Height = 16
    Caption = '???'
  end
  object Label3: TLabel
    Left = 8
    Top = 136
    Width = 243
    Height = 16
    Caption = #1063#1080#1089#1083#1086' '#1074#1099#1079#1086#1074#1086#1074' '#1086#1073#1088#1072#1073#1086#1090#1095#1080#1082#1072' '#1089#1086#1086#1073#1097#1077#1085#1080#1103':'
  end
  object labPostMsgProcessCount: TLabel
    Left = 257
    Top = 136
    Width = 7
    Height = 16
    Caption = '0'
  end
  object Label4: TLabel
    Left = 8
    Top = 118
    Width = 198
    Height = 16
    Caption = #1063#1080#1089#1083#1086' '#1086#1090#1087#1088#1072#1074#1083#1077#1085#1085#1099#1093' '#1089#1086#1086#1073#1097#1077#1085#1080#1081':'
  end
  object labPostMsgCount: TLabel
    Left = 212
    Top = 118
    Width = 16
    Height = 16
    Caption = '0'
  end
  object Label5: TLabel
    Left = 8
    Top = 157
    Width = 114
    Height = 16
    Caption = #1055#1086#1090#1086#1082' '#1079#1072#1074#1077#1088#1096#1080#1083#1089#1103':'
  end
  object labEndWork: TLabel
    Left = 128
    Top = 157
    Width = 18
    Height = 16
    Caption = '???'
  end
  object btnRunInParallelThread: TButton
    Left = 8
    Top = 8
    Width = 217
    Height = 33
    Caption = #1042#1099#1095#1080#1089#1083#1080#1090#1100' '#1089#1091#1084#1084#1091' '#1088#1103#1076#1072' '#1095#1080#1089#1077#1083' '#1076#1086':'
    TabOrder = 0
    OnClick = btnRunInParallelThreadClick
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 49
    Width = 436
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object edMaxValue: TEdit
    Left = 232
    Top = 16
    Width = 89
    Height = 24
    TabOrder = 2
    Text = '10000000'
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 304
    Top = 120
  end
end
