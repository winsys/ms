object Data: TData
  OldCreateOrder = False
  OnDestroy = DataModuleDestroy
  Left = 285
  Top = 89
  Height = 343
  Width = 663
  object DB: TZMySqlDatabase
    Host = 'localhost'
    Port = '3306'
    Database = 'ms'
    Encoding = etNone
    Login = 'ms'
    Password = 'ms'
    LoginPrompt = False
    Connected = False
    Left = 40
    Top = 24
  end
  object Transact1: TZMySqlTransact
    Options = [toHourGlass]
    AutoCommit = True
    Database = DB
    TransactSafe = False
    Left = 112
    Top = 24
  end
  object Query1: TZMySqlQuery
    Database = DB
    Transaction = Transact1
    CachedUpdates = False
    ShowRecordTypes = [ztModified, ztInserted, ztUnmodified]
    Options = [doHourGlass, doAutoFillDefs, doUseRowId]
    LinkOptions = [loAlwaysResync]
    Constraints = <>
    ExtraOptions = [moStoreResult]
    Macros = <>
    RequestLive = False
    Left = 184
    Top = 24
  end
  object DataSource1: TDataSource
    DataSet = Query1
    Left = 256
    Top = 24
  end
  object QTemp: TZMySqlQuery
    Database = DB
    Transaction = Transact1
    CachedUpdates = False
    ShowRecordTypes = [ztModified, ztInserted, ztUnmodified]
    Options = [doHourGlass, doAutoFillDefs, doUseRowId]
    LinkOptions = [loAlwaysResync]
    Constraints = <>
    ExtraOptions = [moStoreResult]
    Macros = <>
    RequestLive = False
    Left = 328
    Top = 24
  end
  object TITLES: TZMySqlQuery
    Database = DB
    Transaction = Transact1
    CachedUpdates = False
    ShowRecordTypes = [ztModified, ztInserted, ztUnmodified]
    Options = [doHourGlass, doAutoFillDefs, doUseRowId]
    LinkOptions = [loAlwaysResync]
    Constraints = <>
    ExtraOptions = [moStoreResult]
    Macros = <>
    RequestLive = False
    Left = 40
    Top = 96
  end
end
