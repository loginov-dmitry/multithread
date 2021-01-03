object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 
    #1055#1088#1086#1075#1088#1072#1084#1084#1072' '#1076#1083#1103' '#1086#1087#1088#1077#1076#1077#1083#1077#1085#1080#1103' '#1076#1083#1080#1090#1077#1083#1100#1085#1086#1089#1090#1080' '#1074#1099#1076#1077#1083#1103#1077#1084#1099#1093' '#1082#1074#1072#1085#1090#1086#1074' '#1074#1088#1077#1084#1077#1085 +
    #1080
  ClientHeight = 490
  ClientWidth = 730
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    730
    490)
  PixelsPerInch = 96
  TextHeight = 19
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 607
    Height = 38
    Caption = 
      #1044#1083#1103' '#1086#1087#1088#1077#1076#1077#1083#1077#1085#1080#1103' '#1076#1083#1080#1090#1077#1083#1100#1085#1086#1089#1090#1080' '#1082#1074#1072#1085#1090#1072' '#1074#1088#1077#1084#1077#1085#1080', '#1082#1086#1090#1086#1088#1099#1081' '#1074#1099#1076#1077#1083#1103#1077#1090#1089#1103' ' +
      #1076#1083#1103' '#1088#1072#1073#1086#1090#1099' '#1087#1086#1090#1086#1082#1086#1074', '#1087#1088#1086#1075#1088#1072#1084#1084#1072' '#1076#1086#1083#1078#1085#1072' '#1088#1072#1073#1086#1090#1072#1090#1100' '#1085#1072' '#1086#1076#1085#1086#1084' '#1103#1076#1088#1077' '#1087#1088#1086#1094 +
      #1077#1089#1089#1086#1088#1072'!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 32
    Top = 52
    Width = 146
    Height = 19
    Caption = #1057#1086#1079#1076#1072#1074#1072#1090#1100' '#1087#1086#1090#1086#1082#1086#1074':'
  end
  object Label3: TLabel
    Left = 9
    Top = 142
    Width = 90
    Height = 19
    Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090#1099':'
  end
  object Label4: TLabel
    Left = 3
    Top = 89
    Width = 169
    Height = 19
    Caption = #1055#1088#1080#1074#1103#1079#1072#1090#1100' '#1082' '#1103#1076#1088#1091' CPU:'
  end
  object Label5: TLabel
    Left = 296
    Top = 83
    Width = 206
    Height = 19
    Caption = #1055#1088#1080#1086#1088#1080#1090#1077#1090' '#1087#1077#1088#1074#1086#1075#1086' '#1087#1086#1090#1086#1082#1072':'
  end
  object Label6: TLabel
    Left = 8
    Top = 464
    Width = 348
    Height = 19
    Anchors = [akLeft]
    Caption = #1048#1079#1084#1077#1085#1080#1090#1100' '#1088#1072#1079#1088#1077#1096#1077#1085#1080#1077' '#1089#1080#1089#1090#1077#1084#1085#1086#1075#1086' '#1090#1072#1081#1084#1077#1088#1072', '#1084#1089':'
  end
  object edThreadCount: TEdit
    Left = 192
    Top = 52
    Width = 73
    Height = 27
    TabOrder = 0
    Text = '2'
  end
  object btnStartThreads: TButton
    Left = 304
    Top = 110
    Width = 297
    Height = 41
    Caption = #1047#1072#1087#1091#1089#1090#1080#1090#1100' '#1087#1086#1090#1086#1082#1080' '#1085#1072' 10 '#1089#1077#1082#1091#1085#1076
    TabOrder = 1
    OnClick = btnStartThreadsClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 167
    Width = 714
    Height = 288
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object clbCPUList: TCheckListBox
    Left = 179
    Top = 89
    Width = 104
    Height = 71
    ItemHeight = 19
    TabOrder = 3
  end
  object cbUseDiffPriority: TCheckBox
    Left = 296
    Top = 55
    Width = 289
    Height = 17
    Caption = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1088#1072#1079#1085#1099#1077' '#1087#1088#1080#1086#1088#1080#1090#1077#1090#1099
    TabOrder = 4
  end
  object cbPriority: TComboBox
    Left = 506
    Top = 80
    Width = 216
    Height = 27
    Style = csDropDownList
    ItemHeight = 19
    ItemIndex = 3
    TabOrder = 5
    Text = 'tpNormal '
    Items.Strings = (
      'tpIdle '
      'tpLowest '
      'tpLower '
      'tpNormal '
      'tpHigher '
      'tpHighest'
      'tpTimeCritical')
  end
  object edSysTimerInterval: TEdit
    Left = 367
    Top = 461
    Width = 42
    Height = 27
    Anchors = [akLeft, akBottom]
    TabOrder = 6
    Text = '16'
  end
  object btnChangeSysTimerInterval: TButton
    Left = 424
    Top = 462
    Width = 105
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #1048#1079#1084#1077#1085#1080#1090#1100
    TabOrder = 7
    OnClick = btnChangeSysTimerIntervalClick
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 24
    Top = 176
  end
end
