unit HelpUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OleCtrls, SHDocVw;

type
  TfrmHelp = class(TForm)
    WebBrowser1: TWebBrowser;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Lang: char;  // N - native, E - english
    constructor Create(AOwner: TComponent; ALanguage: char); reintroduce;
  end;

var
  frmHelp: TfrmHelp;

implementation

{$R *.DFM}

Uses DataUnit, Registry;

constructor TfrmHelp.Create(AOwner: TComponent; ALanguage: char);
begin
  inherited Create(AOwner);
  Lang:=ALanguage;
end;

procedure TfrmHelp.FormShow(Sender: TObject);
begin
  if Lang='N'
    then WebBrowser1.Navigate(ExtractFileDir(ParamStr(0))+'\Help\MSHelp.htm')
    else WebBrowser1.Navigate(ExtractFileDir(ParamStr(0))+'\Help\MSHelpEng.htm');
  WebBrowser1.SetFocus;
end;

procedure TfrmHelp.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveWindowPosition(Self, true);
end;

procedure TfrmHelp.FormCreate(Sender: TObject);
begin
  with TRegistry.Create do
  if OpenKey(SystemKey+'\Forms\'+Self.Name, true)
    then begin
      if not ValueExists('Left')
        then begin
          Self.Left:=Screen.Width div 3;
          Self.Top:=1;
          Self.Width:=Screen.Width-Self.Left;
          Self.Height:=Screen.Height-30;
        end
        else RestoreWindowPosition(Self, true);
      CloseKey;
      Free;
    end
    else Free;
end;

end.
