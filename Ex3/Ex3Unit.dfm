object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 127
  ClientWidth = 381
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  DesignSize = (
    381
    127)
  PixelsPerInch = 96
  TextHeight = 16
  object labStatus: TLabel
    Left = 8
    Top = 56
    Width = 53
    Height = 16
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Caption = 'labStatus'
    Visible = False
  end
  object btnRunParallelThread: TButton
    Left = 8
    Top = 8
    Width = 365
    Height = 33
    Caption = #1047#1072#1087#1091#1089#1090#1080#1090#1100' '#1087#1072#1088#1072#1083#1083#1077#1083#1100#1085#1099#1081' '#1087#1086#1090#1086#1082
    TabOrder = 0
    OnClick = btnRunParallelThreadClick
  end
end
