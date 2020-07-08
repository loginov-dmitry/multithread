object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 390
  ClientWidth = 775
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    775
    390)
  PixelsPerInch = 96
  TextHeight = 16
  object labLabLastThreadTime: TLabel
    Left = 8
    Top = 47
    Width = 336
    Height = 16
    Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103' '#1086' '#1074#1088#1077#1084#1077#1085#1080' '#1087#1086#1089#1083#1077#1076#1085#1077#1075#1086' '#1079#1072#1087#1091#1097#1077#1085#1085#1086#1075#1086' '#1087#1086#1090#1086#1082#1072
  end
  object btnRunInParallelThread: TButton
    Left = 8
    Top = 8
    Width = 217
    Height = 33
    Caption = #1047#1072#1087#1091#1089#1090#1080#1090#1100' '#1076#1086#1087'. '#1087#1086#1090#1086#1082
    TabOrder = 0
    OnClick = btnRunInParallelThreadClick
  end
  object Button1: TButton
    Left = 248
    Top = 8
    Width = 177
    Height = 33
    Caption = #1054#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1087#1086#1090#1086#1082#1080
    TabOrder = 1
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 69
    Width = 759
    Height = 313
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
  object btnClearListBox: TButton
    Left = 431
    Top = 8
    Width = 75
    Height = 33
    Caption = #1054#1095#1080#1089#1090#1080#1090#1100
    TabOrder = 3
    OnClick = btnClearListBoxClick
  end
end
