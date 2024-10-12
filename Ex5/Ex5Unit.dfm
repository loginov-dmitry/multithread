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
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 48
    Width = 218
    Height = 16
    Caption = #1056#1077#1078#1080#1084' '#1079#1072#1074#1077#1088#1096#1077#1085#1080#1103' '#1088#1072#1073#1086#1090#1099' '#1087#1086#1090#1086#1082#1086#1074':'
  end
  object btnRunParallelThreads: TButton
    Left = 8
    Top = 8
    Width = 235
    Height = 33
    Caption = #1047#1072#1087#1091#1089#1090#1080#1090#1100' '#1087#1072#1088#1072#1083#1083#1077#1083#1100#1085#1099#1077' '#1087#1086#1090#1086#1082#1080
    TabOrder = 0
    OnClick = btnRunParallelThreadsClick
  end
  object cbTerminateMode: TComboBox
    Left = 24
    Top = 72
    Width = 210
    Height = 24
    Style = csDropDownList
    ItemHeight = 16
    ItemIndex = 0
    TabOrder = 1
    Text = #1055#1086#1089#1083#1077#1076#1086#1074#1072#1090#1077#1083#1100#1085#1086' ('#1084#1077#1076#1083#1077#1085#1085#1086')'
    Items.Strings = (
      #1055#1086#1089#1083#1077#1076#1086#1074#1072#1090#1077#1083#1100#1085#1086' ('#1084#1077#1076#1083#1077#1085#1085#1086')'
      #1054#1076#1085#1086#1074#1088#1077#1084#1077#1085#1085#1086' ('#1073#1099#1089#1090#1088#1077#1077')')
  end
end
