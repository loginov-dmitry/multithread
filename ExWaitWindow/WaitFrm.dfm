object WaitForm: TWaitForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = #1042#1099#1087#1086#1083#1085#1103#1077#1090#1089#1103' '#1076#1083#1080#1090#1077#1083#1100#1085#1072#1103' '#1086#1087#1077#1088#1072#1094#1080#1103
  ClientHeight = 237
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    391
    237)
  PixelsPerInch = 96
  TextHeight = 19
  object Label1: TLabel
    Left = 21
    Top = 10
    Width = 353
    Height = 23
    Caption = #1042#1099#1087#1086#1083#1085#1103#1077#1090#1089#1103' '#1076#1083#1080#1090#1077#1083#1100#1085#1072#1103' '#1086#1087#1077#1088#1072#1094#1080#1103
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object labOperationName: TLabel
    Left = 8
    Top = 57
    Width = 248
    Height = 19
    Alignment = taCenter
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1090#1077#1082#1091#1097#1077#1081' '#1086#1087#1077#1088#1072#1094#1080#1080
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 159
    Top = 191
    Width = 83
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = #1046#1076#1080#1090#1077'...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitTop = 148
  end
  object Label4: TLabel
    Left = 8
    Top = 216
    Width = 105
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = #1055#1088#1086#1096#1083#1086' '#1074#1088#1077#1084#1077#1085#1080':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitTop = 173
  end
  object lbTime: TLabel
    Left = 119
    Top = 217
    Width = 33
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = '00:00'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitTop = 173
  end
  object btnStop: TSpeedButton
    Left = 296
    Top = 208
    Width = 82
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #1054#1090#1084#1077#1085#1072
    Visible = False
    OnClick = btnStopClick
    ExplicitTop = 165
  end
  object labWaitStatus: TLabel
    Left = 8
    Top = 86
    Width = 375
    Height = 76
    Alignment = taCenter
    AutoSize = False
    Caption = #1057#1090#1072#1090#1091#1089' 1'#13#10#1057#1090#1072#1090#1091#1089' 2'#13#10#1057#1090#1072#1090#1091#1089' 3'#13#10#1057#1090#1072#1090#1091#1089' 4'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object ProgressBar1: TProgressBar
    Left = 32
    Top = 171
    Width = 321
    Height = 17
    Smooth = True
    TabOrder = 0
    Visible = False
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 16
    Top = 40
  end
end
