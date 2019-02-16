unit ReplaceConfirmUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TReplaceConfirmForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Label3: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ReplaceConfirmForm: TReplaceConfirmForm;

implementation

uses Main, DataUnit;

{$R *.DFM}

procedure TReplaceConfirmForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult=mrYes
    then ReplaceALL:=CheckBox1.Checked;
  CanClose:=true;
end;

procedure TReplaceConfirmForm.FormShow(Sender: TObject);
begin
  SetLanguage(Label1, rplMsg1);
  SetLanguage(Label2, rplMsg2);
  SetLanguage(Label3);
  SetLanguage(CheckBox1, rplMsg3);
  SetLanguage(Button1, msBtn1);
  SetLanguage(Button2, msBtn2);
  if Label3.Left+Label3.Width+30 > Label1.Left+Label1.Width+30
    then Width:=Label3.Left+Label3.Width+30
    else Width:=Label1.Left+Label1.Width+30;
  if Width<267 then Width:=267;
end;

end.
