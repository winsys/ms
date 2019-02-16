program ms;

uses
  Forms,
  Main in 'MAIN.PAS' {MainForm},
  Childwin in 'CHILDWIN.PAS' {MDIChild},
  about in 'about.pas' {AboutBox},
  Windows,
  Registry,
  Graphics,
  DataUnit in 'DataUnit.pas' {Data: TDataModule},
  SysUtils,
  QuotesFormUnit in 'QuotesFormUnit.pas' {QuotesForm},
  inf_SendByEMailUnit in 'inf_SendByEMailUnit.pas' {inf_SendByEMail},
  ReplaceConfirmUnit in 'ReplaceConfirmUnit.pas' {ReplaceConfirmForm},
  UpdateProgressUnit in 'UpdateProgressUnit.pas' {UpdateProgress},
  eMailDeliveryUnit in 'eMailDeliveryUnit.pas' {eMailDeliveryForm},
  HelpUnit in 'HelpUnit.pas' {frmHelp};

{$R *.RES}

begin
  Application.Initialize;
  Application.HelpFile := 'MS.HLP';
  Application.Title := 'MessageSearch';
  Application.CreateForm(TData, Data);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
