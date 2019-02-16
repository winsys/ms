unit About;

interface

uses Windows, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Shellapi;

{$I CONSTANTS.PAS}

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    OKButton: TButton;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Image1: TImage;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure Label2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.DFM}

uses DataUnit;

procedure TAboutBox.Label2Click(Sender: TObject);
var Cmd: string;
begin
  Cmd:='mailto:'+(Sender as TLabel).Caption+'?Subject=About MessageSearch 1.1';
  ShellExecute(0, nil, PChar(Cmd), nil, nil, SW_SHOWDEFAULT);
end;

procedure TAboutBox.FormShow(Sender: TObject);
begin
  Label4.Caption:=sysTranslationInfo;
  Label5.Caption:=sysTranslationEmail;
  Label4.Update;
  Label5.Left:=Label4.Left+Label4.Width+5;
  SetLanguage(Label4);
end;

procedure TAboutBox.Label7Click(Sender: TObject);
begin
  ShellExecute(0, nil, 'http://www.winsys.lv/ms', nil, nil, SW_SHOWDEFAULT);
end;

end.

