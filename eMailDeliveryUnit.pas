unit eMailDeliveryUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TeMailDeliveryForm = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Edit3: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Bevel1: TBevel;
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  eMailDeliveryForm: TeMailDeliveryForm;

implementation

uses Registry, DataUnit;

{$R *.DFM}

procedure TeMailDeliveryForm.FormShow(Sender: TObject);
begin
  with TRegistry.Create do
  begin
    OpenKey(SystemKey+'\EMail', false);
    try
      Edit1.Text:=ReadString('Owner EMail address');
    except
      Edit1.Text:='type your email address here';
    end;
    try
      Edit2.Text:=ReadString('Owner EMail user');
    except
      Edit2.Text:='type your name here';
    end;
    try
      Edit3.Text:=ReadString('SMTP Server');
    except
      Edit3.Text:='type SMTP server name or ip here';
    end;
    CloseKey;
    Free;
  end;
  Caption:=emlMsg7;
  SetLanguage(Label1, emlMsg8);
  SetLanguage(Label2, emlMsg9);
  SetLanguage(Label3, emlMsg10);
  SetLanguage(Edit1);
  SetLanguage(Edit2);
  SetLanguage(Edit3);
  SetLanguage(BitBtn2, emlMsg3);
  SetLanguage(BitBtn1, 'Ok');
end;

procedure TeMailDeliveryForm.BitBtn1Click(Sender: TObject);
begin
  with TRegistry.Create do
  begin
    OpenKey(SystemKey+'\EMail', true);
    WriteString('Owner EMail address', Edit1.Text);
    WriteString('Owner EMail user', Edit2.Text);
    WriteString('SMTP Server', Edit3.Text);
    CloseKey;
    Free;
  end;
  Close;
end;

procedure TeMailDeliveryForm.BitBtn2Click(Sender: TObject);
begin
  Close;
end;

procedure TeMailDeliveryForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SaveWindowPosition(Self);
end;

procedure TeMailDeliveryForm.FormCreate(Sender: TObject);
begin
  RestoreWindowPosition(Self);
end;

end.
