object ReplaceConfirmForm: TReplaceConfirmForm
  Left = 242
  Top = 225
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'WARNING!'
  ClientHeight = 172
  ClientWidth = 259
  Color = clBtnFace
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 28
    Top = 40
    Width = 193
    Height = 16
    Caption = 'Проповедь уже в программе!'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 28
    Top = 64
    Width = 165
    Height = 16
    Caption = 'ЗАМЕНИТЬ ПРОПОВЕДЬ?'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 28
    Top = 16
    Width = 43
    Height = 16
    Caption = 'Label3'
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object CheckBox1: TCheckBox
    Left = 40
    Top = 96
    Width = 193
    Height = 17
    Caption = 'заменять все'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 28
    Top = 132
    Width = 75
    Height = 25
    Caption = 'Да'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ModalResult = 6
    ParentFont = False
    TabOrder = 1
  end
  object Button2: TButton
    Left = 120
    Top = 132
    Width = 75
    Height = 25
    Caption = 'Нет'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ModalResult = 7
    ParentFont = False
    TabOrder = 2
  end
end
