program LZH5;

{$APPTYPE CONSOLE}

uses Compress, Classes, SysUtils;

const
   mPressEnter = 'Press <Enter> to close window';
   mHeader     = 'TXT2MSA file compression utility...'#13#10+
                 ' (c) 2000 BIGIT Ltd. (based on freeware LZH5 library)'#13#10;

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
begin
  if ParamCount<2
    then begin
      Writeln(mHeader);
      Writeln('SYNTAX:  TXT2MSA <command> <archive name> [<scope>]');
      Writeln('     <command> = <a/+> - add files');
      Writeln('                 <e/x/-> - extract files');
      Writeln('                 <l/v/?> - list files from archive');
      Writeln('     <archive name> = source/target archive filename');
      Writeln('     <scope> = mask to compress files');
      Writeln;
      Writeln('EXAMPLES:  TXT2MSA + MyUpdate *.txt');
      Writeln('           TXT2MSA - MyUpdate.msa');
      Writeln('           TXT2MSA ? Name_Without_Spaces.msa');
      Writeln;
      Writeln;
      Writeln(mPressEnter);
      Readln;
      exit;
    end;
  TC:=TCompress.Create(nil);
  TC.Name:='TComp';
  TC.OnShowProgress:=Progress;
  TC.OnCheckFile:=CheckFile;
  if paramStr(1)[1] in ['e','x','-']
    then begin
      Writeln(mHeader);
      Writeln('Extracting files from archive: '+AnsiUpperCase(ParamStr(2)));
      writeln('-----');
      LastFile:='';
      if FileExists(ParamStr(2))
        then TC.ExpandFiles(ParamStr(2), nil)
        else
      if FileExists(ParamStr(2)+'.msa')
        then TC.ExpandFiles(ParamStr(2)+'.msa', nil)
        else begin
          Writeln;
          Writeln('Error: file not found - '+ParamStr(2)+'[.msa]');
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
        then for i:=3 to ParamCount do
               FL.Add(ParamStr(i))
        else TC.GetAllFilesInDir(FL, '', '*.*', true);
      if FL.Count=0
        then begin
          Writeln('Error: no files found');
          Writeln;
          Writeln(mPressEnter);
          Readln;
        end;
      Writeln(mHeader);
      Writeln('Compressing files into archive: '+AnsiUpperCase(ParamStr(2)));
      writeln('-----');
      LastFile:='';
      if pos('.ms', ParamStr(2))=0
        then TC.CompressFiles(ParamStr(2)+'.msa', FL,  coLZH5)
        else TC.CompressFiles(ParamStr(2), FL,  coLZH5);
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
      if FileExists(ParamStr(2)+'.msa')
        then TC.ScanCompressedFile(ParamStr(2)+'.msa', FL)
        else begin
          Writeln;
          Writeln('Error: file not found - '+ParamStr(2)+'[.msa]');
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
