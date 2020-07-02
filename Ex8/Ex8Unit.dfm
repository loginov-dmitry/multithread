object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 150
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
    150)
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
    Width = 170
    Height = 16
    Caption = #1058#1077#1082#1089#1090#1086#1074#1072#1103' '#1089#1090#1088#1086#1082#1072' '#1080#1079' '#1087#1086#1090#1086#1082#1072':'
  end
  object labThreadStateInfo: TLabel
    Left = 184
    Top = 96
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
    Left = 104
    Top = 72
  end
end
