unit UpdateProgressUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TUpdateProgress = class(TForm)
    P: TProgressBar;
    Label1: TLabel;
    P2: TProgressBar;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UpdateProgress: TUpdateProgress;

implementation

{$R *.DFM}

uses DataUnit;

procedure TUpdateProgress.FormShow(Sender: TObject);
begin
  SetLanguage(Label1, rplMsg4);
  SetLanguage(Label2);
end;

end.
