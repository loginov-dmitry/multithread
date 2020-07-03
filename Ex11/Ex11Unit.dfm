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
    Top = 56
    Width = 759
    Height = 326
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 16
    TabOrder = 2
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 416
    Top = 72
  end
end
