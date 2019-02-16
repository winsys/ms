object frmMain: TfrmMain
  Left = 186
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Sql Scanner Test'
  ClientHeight = 345
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 417
    Top = 170
    Width = 49
    Height = 13
    Caption = 'Database:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object memSql: TMemo
    Left = 8
    Top = 8
    Width = 401
    Height = 89
    Lines.Strings = (
      '-- This is a comment'
      'select all * as "string" '
      'from /* xx */ mysql.db;'
      'show /* xxx*/ version')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnStart: TButton
    Left = 432
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Start'
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnGetToken: TButton
    Left = 432
    Top = 40
    Width = 75
    Height = 25
    Caption = '&Token'
    TabOrder = 2
    OnClick = btnGetTokenClick
  end
  object btnGetTokenEx: TButton
    Left = 432
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Token &Next'
    TabOrder = 3
    OnClick = btnGetTokenExClick
  end
  object memResult: TMemo
    Left = 8
    Top = 104
    Width = 401
    Height = 233
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object cbxShowComment: TCheckBox
    Left = 424
    Top = 216
    Width = 97
    Height = 17
    Caption = 'Show &Comment'
    TabOrder = 5
    OnClick = cbxShowCommentClick
  end
  object cbxShowEol: TCheckBox
    Left = 424
    Top = 240
    Width = 97
    Height = 17
    Caption = 'Show &Eol'
    TabOrder = 6
    OnClick = cbxShowCommentClick
  end
  object cbxShowString: TCheckBox
    Left = 424
    Top = 264
    Width = 97
    Height = 17
    Caption = 'Show &String'
    TabOrder = 7
    OnClick = cbxShowCommentClick
  end
  object cbxShowType: TCheckBox
    Left = 424
    Top = 288
    Width = 97
    Height = 17
    Caption = 'Show &Type'
    TabOrder = 8
    OnClick = cbxShowCommentClick
  end
  object cbxShowKeyword: TCheckBox
    Left = 424
    Top = 312
    Width = 97
    Height = 17
    Caption = 'Show &Keyword'
    TabOrder = 9
    OnClick = cbxShowCommentClick
  end
  object btnClearResult: TButton
    Left = 432
    Top = 136
    Width = 75
    Height = 25
    Caption = '&Clear Result'
    TabOrder = 10
    OnClick = btnClearResultClick
  end
  object cbDatabase: TComboBox
    Left = 416
    Top = 184
    Width = 115
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 11
    OnChange = btnStartClick
    Items.Strings = (
      'C'
      'Interbase'
      'MSSQL'
      'MySQL'
      'Oracle'
      'Pascal'
      'PostgreSQL')
  end
  object btnStatement: TButton
    Left = 432
    Top = 104
    Width = 75
    Height = 25
    Caption = '&Statement'
    TabOrder = 12
    OnClick = btnStatementClick
  end
end
