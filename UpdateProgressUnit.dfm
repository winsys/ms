object UpdateProgress: TUpdateProgress
  Left = 336
  Top = 180
  BorderIcons = []
  BorderStyle = bsSingle
  ClientHeight = 102
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 473
    Height = 15
    Alignment = taCenter
    AutoSize = False
    Caption = 'Идёт обновление базы данных...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 12
    Top = 56
    Width = 473
    Height = 15
    Alignment = taCenter
    AutoSize = False
    Caption = '...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object P: TProgressBar
    Left = 12
    Top = 32
    Width = 473
    Height = 16
    Min = 0
    Max = 100
    TabOrder = 0
  end
  object P2: TProgressBar
    Left = 12
    Top = 76
    Width = 473
    Height = 16
    Min = 0
    Max = 1
    TabOrder = 1
  end
end
