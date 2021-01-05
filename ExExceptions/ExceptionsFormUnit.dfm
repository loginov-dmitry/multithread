object ExceptionsForm: TExceptionsForm
  Left = 0
  Top = 0
  Caption = #1055#1088#1080#1084#1077#1088' '#1087#1077#1088#1077#1093#1074#1072#1090#1072' '#1080#1089#1082#1083#1102#1095#1077#1085#1080#1103' '#1074' '#1076#1086#1087'. '#1087#1086#1090#1086#1082#1077
  ClientHeight = 327
  ClientWidth = 664
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
    664
    327)
  PixelsPerInch = 96
  TextHeight = 19
  object Button1: TButton
    Left = 32
    Top = 32
    Width = 225
    Height = 33
    Caption = #1047#1072#1087#1091#1089#1090#1080#1090#1100' '#1076#1086#1087'. '#1087#1086#1090#1086#1082
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 0
    Top = 88
    Width = 662
    Height = 237
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 19
    TabOrder = 1
    ExplicitWidth = 633
    ExplicitHeight = 209
  end
end
