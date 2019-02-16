object eMailDeliveryForm: TeMailDeliveryForm
  Left = 361
  Top = 256
  BorderStyle = bsDialog
  Caption = 'e-Mail delivery settings'
  ClientHeight = 225
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 20
    Top = 16
    Width = 94
    Height = 13
    Caption = 'Your e-Mail address'
  end
  object Label2: TLabel
    Left = 20
    Top = 68
    Width = 103
    Height = 13
    Caption = 'Your e-Mail username'
  end
  object Label3: TLabel
    Left = 20
    Top = 120
    Width = 163
    Height = 13
    Caption = 'SMTP server for message delivery'
  end
  object Bevel1: TBevel
    Left = 20
    Top = 172
    Width = 229
    Height = 9
    Shape = bsTopLine
  end
  object Edit1: TEdit
    Left = 20
    Top = 32
    Width = 229
    Height = 21
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clInfoText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 20
    Top = 84
    Width = 229
    Height = 21
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clInfoText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 20
    Top = 136
    Width = 229
    Height = 21
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clInfoText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object BitBtn1: TBitBtn
    Left = 175
    Top = 188
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    TabOrder = 3
    OnClick = BitBtn1Click
    Glyph.Data = {
      4E010000424D4E01000000000000760000002800000012000000120000000100
      040000000000D800000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
      8888880000008888888888888888880000008888888808888888880000008888
      8880A0888888880000008888888AA0888888880000008888880AA08888888800
      0000888880AAAA0888888800000088800AAAAA08888888000000880AAAAFAAA0
      88888800000088AAAAF8FAA088888800000088FAAF88FAAA088888000000888F
      F8888FAAA088880000008888888888FAAA088800000088888888888FFAA08800
      00008888888888888FAA8800000088888888888888FFA8000000888888888888
      888888000000888888888888888888000000}
  end
  object BitBtn2: TBitBtn
    Left = 88
    Top = 188
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = BitBtn2Click
    Glyph.Data = {
      4E010000424D4E01000000000000760000002800000012000000120000000100
      040000000000D800000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
      8888880000008888888888888888880000008880008888880088880000008889
      9908888899F8880000008889990888809F88880000008889990888099F888800
      0000888899908099F88888000000888889990999F8888800000088888899999F
      8888880000008888880999088888880000008888009999908888880000008880
      9999F9990888880000008889999F8F9990088800000088F999F888FF99908800
      0000888F9F888888FF99080000008888F888888888FF98000000888888888888
      8888F8000000888888888888888888000000}
  end
end
