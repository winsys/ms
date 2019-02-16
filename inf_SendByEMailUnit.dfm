object inf_SendByEMail: Tinf_SendByEMail
  Left = 279
  Top = 238
  BorderStyle = bsDialog
  Caption = 'Send by E-Mail'
  ClientHeight = 198
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Label4: TLabel
    Left = 28
    Top = 76
    Width = 283
    Height = 14
    Caption = 'Укажите адрес для отправки заказа и имя получателя:'
  end
  object Label6: TLabel
    Left = 28
    Top = 18
    Width = 369
    Height = 47
    AutoSize = False
    Caption = 
      'Все адреса по которым отправлялись тексты автоматически сохраняю' +
      'тся в списке'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clBlue
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object SendTo: TComboBox
    Left = 28
    Top = 92
    Width = 149
    Height = 22
    Color = clInfoBk
    DropDownCount = 20
    ItemHeight = 14
    TabOrder = 0
    OnChange = SendToChange
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 179
    Width = 420
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object SendTo_Name: TComboBox
    Left = 184
    Top = 92
    Width = 213
    Height = 22
    Color = clInfoBk
    DropDownCount = 20
    ItemHeight = 14
    TabOrder = 2
    OnClick = SendTo_NameChange
  end
  object CancelButton: TBitBtn
    Left = 175
    Top = 136
    Width = 103
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = 'Отмена'
    ModalResult = 2
    TabOrder = 3
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
    Layout = blGlyphRight
  end
  object OkButton: TBitBtn
    Left = 295
    Top = 136
    Width = 101
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = 'Отправить'
    Default = True
    ModalResult = 1
    TabOrder = 4
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000CE0E0000C40E00001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00222222222222
      2222222222222222222222222222222222222200000000000002288888888888
      880228B7B7B7B7B7B802280B7B7B7B7B080228B0B7B0B7B0B80228FB0B0B0B0B
      780228B7B0B7B0B7B80228FB0B7B7B0B780228B0B7B7B7B0B802280BFBFBFBFB
      0802288888888888882222222222222222222222222222222222}
    Layout = blGlyphRight
  end
  object SettButton: TBitBtn
    Left = 27
    Top = 136
    Width = 110
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = 'Settings...'
    TabOrder = 5
    OnClick = SettButtonClick
    Glyph.Data = {
      66010000424D6601000000000000760000002800000014000000140000000100
      040000000000F000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
      8888888800008888888888888888888800008888888888888777777800008888
      888888887444444C0000888888888887444874C8000088888888887444874C88
      00008888888887444874C8880000888888777444874C88880000888877444448
      74C8888800008887444444444C88888800008884444444444888888800008844
      44884444C88888880000888488888444C88888880000888888877444C8888888
      0000888887744444C888888800008888844444CC888888880000888884CCCC88
      8888888800008888888888888888888800008888888888888888888800008888
      88888888888888880000}
  end
  object SMTP: TNMSMTP
    Port = 25
    ReportLevel = 0
    OnStatus = SMTPStatus
    EncodeType = uuMime
    ClearParams = False
    SubType = mtHtml
    Charset = 'us-ascii'
    OnSuccess = SMTPSuccess
    Left = 128
    Top = 104
  end
end
