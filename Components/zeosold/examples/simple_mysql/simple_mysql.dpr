program simple_mysql;

uses
  Forms,
  simple_mysql1 in 'simple_mysql1.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
