unit inf_SendByEMailUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Psock, NMsmtp, StdCtrls, Buttons, ComCtrls, Registry, IniFiles;

type
  Tinf_SendByEMail = class(TForm)
    Label4: TLabel;
    Label6: TLabel;
    SendTo: TComboBox;
    StatusBar: TStatusBar;
    SendTo_Name: TComboBox;
    CancelButton: TBitBtn;
    OkButton: TBitBtn;
    SMTP: TNMSMTP;
    SettButton: TBitBtn;
    procedure SendToChange(Sender: TObject);
    procedure SendTo_NameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SMTPStatus(Sender: TComponent; Status: String);
    procedure SMTPSuccess(Sender: TObject);
    procedure SettButtonClick(Sender: TObject);
  private
    { Private declarations }
    FName    : string;
    IniFName : string;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent; AFName: string); reintroduce;

    procedure   SMTPAbortMsg(AStr: string);
  end;

var
  inf_SendByEMail : Tinf_SendByEMail;
  LastInd         : smallint;  // будет показывать последний индекс +1 загруженного из registry e-mail адреса.

implementation

uses DataUnit, eMailDeliveryUnit;

{$R *.DFM}

constructor Tinf_SendByEMail.Create(AOwner: TComponent; AFName: string);
begin
  inherited Create(AOwner);
  FName:=AFName;
end;

procedure Tinf_SendByEMail.SendToChange(Sender: TObject);
begin
  if SendTo.Focused then
    if SendTo.ItemIndex>=0
      then SendTo_Name.ItemIndex:=SendTo.ItemIndex;
end;

procedure Tinf_SendByEMail.SendTo_NameChange(Sender: TObject);
begin
  if SendTo_Name.Focused then
    if SendTo_Name.ItemIndex>=0
      then SendTo.ItemIndex:=SendTo_Name.ItemIndex;
end;

procedure Tinf_SendByEMail.FormCreate(Sender: TObject);
begin
  // Восстановление позиции окна из Registry.
  RestoreWindowPosition(Self, true);
end;

procedure Tinf_SendByEMail.FormShow(Sender: TObject);
var Ind: smallint;
    AppIni: TIniFile;
    AList: TStringList;
    i: smallint;

begin
  IniFName:=NormalizeFName(ExtractFilePath(Application.ExeName), 'ms.ini');

  Caption:=msMsg29;
  SetLanguage(Label6, emlMsg1);
  SetLanguage(Label4, emlMsg2);
  SetLanguage(CancelButton, emlMsg3);
  SetLanguage(OkButton, emlMsg4);
  SetLanguage(SettButton, emlMsg6);

  LastInd:=1;
  SendTo.Clear;
  SendTo_Name.Clear;

  // Загрузка информации (если она была в ini файле).
  AList:=TStringList.Create;
  AList.Clear;
  AppIni:=TIniFile.Create(IniFName);
  Ind:=AppIni.ReadInteger('COMMON', 'LASTEMAILUSER',0);
  if AppIni.SectionExists('EMAIL LIST')
    then AppIni.ReadSectionValues('EMAIL LIST', AList);
  AppIni.Free;
  for i:=0 to AList.Count-1 do
   begin
     SendTo.Items.Add(AList.Names[i]);
     SendTo_Name.Items.Add(AList.Values[AList.Names[i]]);
   end;
  AList.Free;
  SendTo.ItemIndex:=Ind;
  SendTo.SetFocus;
  SendToChange(SendTo);

  // Загрузка информации из Regestry.
  with TRegistry.Create do
   try
     RootKey:=HKEY_CURRENT_USER;
     OpenKey(SystemKey+'\EMail', False);
     // Установка информации для отправления почты.
     SMTP.Host:=ReadString('SMTP Server');
     SMTP.Port := 25;
     SMTP.ReportLevel := 8;
     SMTP.Timeout := 5000;
     SMTP.UserID := 'MessageSearch';
    finally
     CloseKey;
     Free;
   end;
end;

procedure Tinf_SendByEMail.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // Сохраняет позицию окна в Registry
  SaveWindowPosition(Self, true);
end;

procedure Tinf_SendByEMail.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var MM     : TPostMessage;
    AppIni : TIniFile;

begin
  if ModalResult=mrOk
    then try
      Screen.Cursor:=crHourGlass;
      MM:=SMTP.PostMessage;
      StatusBar.Panels[0].Text:='Connecting...';
      StatusBar.Update;
      try
        SMTP.Connect;
        repeat
          Application.ProcessMessages;
        until SMTP.Connected or SMTP.BeenCanceled or SMTP.BeenTimedOut;
        if SMTP.Connected
          then begin
            // Загрузка информации для отправления почты.
            with TRegistry.Create do
             try
               RootKey:=HKEY_CURRENT_USER;
               OpenKey(SystemKey+'\EMail', False);
               MM.FromAddress:=ReadString('Owner EMail address');   // Адрес отправителя.
               MM.FromName:=ReadString('Owner EMail user');         // Имя отправителя.
               SMTP.Host:=ReadString('SMTP server');
               CloseKey;
              finally
               Free;
             end;
            MM.ToAddress.Text:=SendTo.Text;                         // Адрес получателя.
            MM.Date:=FormatDateTime('mmm dd, yyyy', Date);
            MM.Subject:='The Message quotes';
            MM.LocalProgram:='MessageSearch 1.01';

            MM.Attachments.Clear;                                   // Присоединение файлов.
            MM.Attachments.Add(FName);

            // Посылка письма.
            StatusBar.Panels[0].Text:='Sending '+FName+' to: '+MM.ToAddress[0];
            StatusBar.Update;
            SMTP.UserID:='ms';
            try
              SMTP.SendMail;
              repeat
                Application.ProcessMessages;
              until StatusBar.Panels[0].Text='SUCCESS!';
             except
              raise;
              exit;
            end;
            SMTPAbortMsg(emlMsg5);
            // Сохранение информации в Registry.
            AppIni:=TIniFile.Create(IniFName);
            try
              if SendTo.ItemIndex>=0
                then AppIni.WriteInteger('COMMON', 'LASTEMAILUSER',SendTo.ItemIndex);
              if (SendTo.ItemIndex<0) and (trim(SendTo.Text)<>'') and (trim(SendTo_Name.Text)<>'')
                then AppIni.WriteString('EMAIL LIST', SendTo.Text, SendTo_Name.Text);
              AppIni.UpdateFile;
            finally
              AppIni.Free;
            end;
            ShowMessage(emlMsg5);
          end
          else if SMTP.BeenCanceled
                 then SMTPAbortMsg('Connection was cancelled!')
                 else SMTPAbortMsg('Connection was timed out!');
       except
        SMTPAbortMsg('Cannot connect to mail server!')
      end;
     finally
       Screen.Cursor:=crDefault;
    end;
end;

procedure Tinf_SendByEMail.SMTPAbortMsg(AStr: string);
begin
  if SMTP.Connected
    then SMTP.Disconnect;
  StatusBar.Panels[0].Text:=AStr;
  StatusBar.Update;
end;

procedure Tinf_SendByEMail.SMTPStatus(Sender: TComponent; Status: String);
begin
  StatusBar.Panels[0].Text:=Status;
  StatusBar.Update;
end;

procedure Tinf_SendByEMail.SMTPSuccess(Sender: TObject);
begin
  StatusBar.Panels[0].Text:='SUCCESS!';
end;

procedure Tinf_SendByEMail.SettButtonClick(Sender: TObject);
begin
  eMailDeliveryForm:=TeMailDeliveryForm.Create(Self);
  eMailDeliveryForm.ShowModal;
  eMailDeliveryForm.Free;
end;

end.
