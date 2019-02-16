program TXT2MSA;

{$APPTYPE CONSOLE}

uses Compress, Classes, SysUtils;

const
   mPressEnter = 'Press <Enter> to close window';
   mHeader     = 'TXT2MSA compression utility...'#13#10+
                 ' (c) 2000 WINSYS Ltd.'#13#10;

type
   TMyApp = class(TObject)
     procedure Run;
     procedure Progress(var PercentageDone: Longint);
     procedure CheckFile(var filepath: String;  mode: TCProcessMode);
   end;

var TC: TCompress;
    FL: TStringList;
    App: TMyApp;
    LastFile, CurFile: string;

procedure TMyApp.Progress(var PercentageDone: Longint);
begin
  if LastFile<>CurFile
    then writeln;
  Write(#13+CurFile+#9#9,PercentageDone,'% done...   '#13);
  LastFile:=CurFile;
end;

procedure TMyApp.CheckFile(var filepath: String;  mode: TCProcessMode);
begin
  CurFile:=FilePath;
end;

procedure TMyApp.Run;
var i: smallint;
    FName: string;

    function Quoted(ASrc: string): string;
    begin
      if pos(' ', ASrc)>0
        then Result:='"'+ASrc+'"'
        else Result:=ASrc;
    end;

begin
  if ParamCount<2
    then begin
      Writeln(mHeader);
      Writeln('SYNTAX:  TXT2MSA <command> <archive name> [<scope>]');
      Writeln('     <command> = <a/+> - add files');
      Writeln('                 <e/x/-> - extract files');
      Writeln('                 <l/v/?> - list files from archive');
      Writeln('     <archive name> = source/target archive filename[.ext]');
      Writeln('     <scope> = mask to source files, or file name[s]');
      Writeln;
      Writeln('EXAMPLES:  TXT2MSA + MyUpdate *.txt');
      Writeln('           TXT2MSA + MyUpdate.msa 57-0123.TXT 59-1234.TXT');
      Writeln('           TXT2MSA - MyUpdate.msa');
      Writeln('           TXT2MSA ? "Long archive name.msa"');
      Writeln;
      Writeln;
      Writeln(mPressEnter);
      Readln;
              exit;
    end;
  TC:=TCompress.Create(nil);
  TC.OnShowProgress:=Progress;
  TC.OnCheckFile:=CheckFile;
  FName:=ParamStr(2);
  if FName[1]='"'
    then FName:=copy(FName, 2, length(FName)-2);
  if paramStr(1)[1] in ['e','x','-']
    then begin
      Writeln(mHeader);
      Writeln('Extracting files from archive: '+AnsiUpperCase(ParamStr(2)));
      writeln('-----');
      LastFile:='';
      if FileExists(ParamStr(2))
        then TC.ExpandFiles(ParamStr(2), nil)
        else
      if FileExists(FName+'.msa')
        then TC.ExpandFiles(FName+'.msa', nil)
        else begin
          Writeln;
          Writeln('Error: file not found - '+Quoted(FName+'[.msa]'));
          Writeln;
          Writeln(mPressEnter);
          Readln;
          exit;
        end;
      Writeln;
      Writeln('Extraction done. '+mPressEnter);
//      Readln;
    end
    else
  if paramStr(1)[1] in ['a','+']
    then begin
      FL:=TStringList.Create;
      if ParamCount>2
        then begin
          if pos('*', ParamStr(3)) > 0
            then TC.GetAllFilesInDir(FL, '', ParamStr(3), true)
            else for i:=3 to ParamCount do
                   FL.Add(ParamStr(i));
        end
        else TC.GetAllFilesInDir(FL, '', '*.txt', true);
      if FL.Count=0
        then begin
          Writeln('Error: no files found');
          Writeln;
          Writeln(mPressEnter);
          Readln;
        end;
      Writeln(mHeader);
      if pos('.msa', ParamStr(2))=0
        then Writeln('Compressing files into archive: '+AnsiUpperCase(Quoted(FName+'.msa')))
        else Writeln('Compressing files into archive: '+AnsiUpperCase(Quoted(FName)));
      writeln('-----');
      LastFile:='';
      if pos('.msa', ParamStr(2))=0
        then TC.CompressFiles(FName+'.msa', FL,  coLZH5)
        else TC.CompressFiles(FName, FL,  coLZH5);
      FL.Free;
      Writeln;
      Writeln('Compression done. '+mPressEnter);
//      Readln;
    end else
  if paramStr(1)[1] in ['l','v','?']
    then begin
      Writeln(mHeader);
      FL:=TStringList.Create;
      if FileExists(ParamStr(2))
        then TC.ScanCompressedFile(ParamStr(2), FL)
        else
      if FileExists(FName+'.msa')
        then TC.ScanCompressedFile(FName+'.msa', FL)
        else begin
          Writeln;
          Writeln('Error: file not found - '+Quoted(FName+'[.msa]'));
          Writeln;
          Writeln(mPressEnter);
          Readln;
          exit;
        end;
      writeln('Contents of archive: '+AnsiUpperCase(ParamStr(2)));
      writeln('-----');
      i:=3;
      while i<FL.Count do
        begin
          writeln(FL[i-1]);
          if ((i mod 24 = 0) and (i>24)) or (i=20)
            then begin
              Write('Press <Enter> to more...');
              readln;
            end;
          inc(i);
        end;
      Writeln;
      Writeln;
      Writeln(mPressEnter);
      Readln;
    end
    else begin
      Writeln;
      Writeln(paramStr(1)+': unknown command');
      Writeln;
      Writeln(mPressEnter);
      Readln;
    end;
  TC.Free;
end;

begin
  App:=TMyApp.Create;
  App.Run;
  App.Free;
end.

