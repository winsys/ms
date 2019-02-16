{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{               Abstract property editors                }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZProperty;

interface

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses Classes {$IFNDEF VERCLX}, DsgnIntf{$ELSE}, DesignIntf, DesignEditors{$ENDIF};

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { Abstract property editor class }
  TDbPropertyEditor = class(TPropertyEditor)
  protected
    FHost, FDatabase: string;
    FRead: Boolean;
    FList: TStrings;
  public
    destructor Destory;

    procedure GetValueList(Values: TStringList); virtual; abstract;

    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function  GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  { Property editor for TZDatabase IndexName }
  TZIndexNameProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

  { Property editor for TZDatabase IndexFieldNames }
  TZIndexFieldNamesProperty = class(TDbPropertyEditor)
    procedure GetValueList(Values: TStringList); override;
  end;

implementation

uses ZQuery;

{******************** TDbPropertyEditor implementation ************* }

destructor TDbPropertyEditor.Destory;
begin
  FList.Free;
end;

function TDbPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TDbPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TDbPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure TDbPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count-1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{************ TZIndexNameProperty class implementation **********}

procedure TZIndexNameProperty.GetValueList(Values: TStringList);
var
  I: Integer;
begin
  with GetComponent(0) as TZDataset do
  begin
    IndexDefs.Update;
    for I := 0 to IndexDefs.Count-1 do
      Values.Add(IndexDefs[I].Name);
  end;
end;

{********* TZIndexFieldNamesProperty class implementation *******}

procedure TZIndexFieldNamesProperty.GetValueList(Values: TStringList);
var
  I: Integer;
begin
  with GetComponent(0) as TZDataset do
  begin
    IndexDefs.Update;
    for I := 0 to IndexDefs.Count-1 do
      Values.Add(IndexDefs[I].Fields);
  end;
end;

end.
