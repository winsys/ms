{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{          Different encodings convertion tools          }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZConvert;

interface

type
  TEncodingType = (etNone, et866, etKoi8r, etKoi8u, etCp1251, etIso88592,
    etCp1250);

{ Convert following encodings }
function Convert(const Value: string; SrcEnc, DestEnc: TEncodingType): string;

implementation

uses SysUtils;

type
  TTransTable = array[0..127] of Char;

const
  Table_Koi8r_866: TTransTable =
    ( #225, #226, #247, #231, #228, #229, #246, #250, #233, #234, #235, #236,
      #237, #238, #239, #240, #242, #243, #244, #245, #230, #232, #227, #254,
      #251{}, #253, #255, #249, #248, #252, #224, #241, #193, #194, #215, #199,
      #196, #197, #214, #218, #201, #202, #203, #204, #205, #206, #207, #208,
      #144, #145, #146, #129, #135, #178, #180, #167, #166, #181, #161, #168,
      #174, #173, #172, #131, #132, #137, #136, #134, #128, #138, #175, #176,
      #171, #165, #187, #184, #177, #160, #190, #185, #186, #182, #183, #170,
      #169, #162, #163, #189, #188, #133, #130, #141, #140, #142, #143, #139,
      #210, #211, #212, #213, #198, #200, #195, #222, #219, #221, #223, #217,
      #216, #220, #192, #209, #179, #163, #153, #152, #147, #155, #159, #151,
      #156, #149, #158, #150, #191, #157, #148, #154);

  Table_866_Koi8r: TTransTable =
    ( #196, #179, #218, #191, #192, #217, #195, #180, #194, #193, #197, #223,
      #220, #219, #221, #222, #176, #177, #178, #244, #254, #249, #251, #247,
      #243, #242, #255, #245, #248, #253, #250, #246, #205, #186, #213, #241,
      #164, #201, #184, #183, #187, #212, #211, #200, #190, #189, #188, #198,
      #199, #204, #181, #240, #182, #185, #209, #210, #203, #207, #208, #202,
      #216, #215, #206, #252, #238, #160, #161, #230, #164, #165, #228, #163,
      #229, #168, #169, #170, #171, #172, #173, #174, #175, #239, #224, #225,
      #226, #227, #166, #162, #236, #235, #167, #232, #237, #233, #231, #234,
      #158, #128, #129, #150, #132, #133, #148, #131, #149, #136, #137, #138,
      #139, #140, #141, #142, #143, #159, #144, #145, #146, #147, #134, #130,
      #156, #155, #135, #152{251}, #157, #153, #151, #154);

  Table_866_Cp1251: TTransTable =
    ( #192, #193, #194, #195, #196, #197, #198, #199, #200, #201, #202, #203,
      #204, #205, #206, #207, #208, #209, #210, #211, #212, #213, #214, #215,
      #216, #217, #218, #219, #220, #221, #222, #223, #224, #225, #226, #227,
      #228, #229, #230, #231, #232, #233, #234, #235, #236, #237, #238, #239,
      #45, #45, #45, #166, #43, #166, #166, #172, #172, #166, #166, #172,
      #45, #45, #45, #172, #76, #43, #84, #43, #45, #43, #166, #166,
      #76, #227, #166, #84, #166, #61, #43, #166, #166, #84, #84, #76,
      #76, #45, #227, #43, #43, #45, #45, #45, #45, #166, #166, #45,
      #240, #241, #242, #243, #244, #245, #246, #247, #248, #249, #250, #251,
      #252, #253, #254, #255, #168, #184, #170, #186, #175, #191, #161, #162,
      #156, #155, #135, #152, #157, #153, #151, #154);

  Table_Cp1251_866: TTransTable =
    ( #63, #63, #39, #63, #34, #58, #197, #216, #63, #37, #63, #60,
      #63, #63, #63, #63, #63, #39, #39, #34, #34, #7, #45, #45,
      #63, #84, #63, #62, #63, #63, #63, #63, #255, #246, #247, #63,
      #253, #63, #179, #21, #240, #99, #242, #60, #191, #45, #82, #244,
      #248, #43, #73, #105, #63, #231, #20, #250, #241, #252, #243, #62,
      #63, #63, #63, #245, #128, #129, #130, #131, #132, #133, #134, #135,
      #136, #137, #138, #139, #140, #141, #142, #143, #144, #145, #146, #147,
      #148, #149, #150, #151, #152, #153, #154, #155, #156, #157, #158, #159,
      #160, #161, #162, #163, #164, #165, #166, #167, #168, #169, #170, #171,
      #172, #173, #174, #175, #224, #225, #226, #227, #228, #229, #230, #231,
      #232, #233, #234, #235, #236, #237, #238, #239 );

  Table_Cp1251_Koi8r: TTransTable =
    ( #63, #63, #39, #63, #34, #58, #138, #188, #63, #37, #63, #60,
      #63, #63, #63, #63, #63, #39, #39, #34, #34, #7, #45, #45,
      #63, #84, #63, #62, #63, #63, #63, #63, #154, #159, #151, #63,
      #157, #63, #129, #21, #179, #99, #153, #60, #131, #45, #82, #147,
      #156, #43, #73, #105, #63, #222, #20, #158, #163, #191, #152, #62,
      #63, #63, #63, #155, #225, #226, #247, #231, #228, #229, #246, #250,
      #233, #234, #235, #236, #237, #238, #239, #240, #242, #243, #244, #245,
      #230, #232, #227, #254, #251, #253, #255, #249, #248, #252, #224, #241,
      #193, #194, #215, #199, #196, #197, #214, #218, #201, #202, #203, #204,
      #205, #206, #207, #208, #210, #211, #212, #213, #198, #200, #195, #222,
      #219, #221, #223, #217, #216, #220, #192, #209 );

  Table_Koi8r_Cp1251: TTransTable =
    ( #45, #166, #45, #172, #76, #45, #43, #43, #84, #43, #43, #45,
      #45, #45, #166, #166, #45, #45, #45, #175, #151, #155, #152, #162,
      #186, #170, #154, #191, #156, #153, #135, #161, #61, #166, #45, #184,
      #228, #227, #172, #172, #172, #76, #76, #76, #45, #45, #45, #166,
      #166, #166, #166, #168, #166, #166, #84, #84, #84, #166, #166, #166,
      #43, #43, #43, #157, #254, #224, #225, #246, #228, #229, #244, #227,
      #245, #232, #233, #234, #235, #236, #237, #238, #239, #255, #240, #241,
      #242, #243, #230, #226, #252, #251, #231, #248, #253, #249, #247, #250,
      #222, #192, #193, #214, #196, #197, #212, #195, #213, #200, #201, #202,
      #203, #204, #205, #206, #207, #223, #208, #209, #210, #211, #198, #194,
      #220, #219, #199, #216, #221, #217, #215, #218 );

  Table_Iso88592_Cp1250: TTransTable =
    ( #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
      #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #165, #0, #0,
      #0, #188, #140, #0, #0, #138, #0, #141, #143, #0, #142, #0, #0, #185, #0, 
      #0, #0, #190, #156, #161, #0, #154, #0, #157, #159, #0, #158, #0, #0, #0, 
      #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, 
      #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, 
      #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, 
      #0, #0, #0, #0, #0, #0, #0, #0 );

  Table_Cp1250_Iso88592: TTransTable =
    ( #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #169, #0, #166, #171, #174, #172,
      #0, #96, #39, #0, #34, #0, #0, #0, #0, #0, #185, #0, #182, #187, #190,
      #188, #0, #183, #0, #0, #0, #161, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
      #0, #0, #0, #0, #0, #0, #0, #255, #0, #177, #0, #0, #165, #0, #181, #0,
      #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
      #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
      #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
      #0, #0, #0, #0, #0, #0, #0, #0, #0, #0 );

  Table_Koi8u_Cp1251 : TTransTable =
    ( #$80,#$81,#$82,#$83,#$84,#$85,#$86,#$87,#$88,#$89,#$8a,#$8b,#$8c,#$8d,#$8e,#$8f,
      #$90,#$91,#$92,#$93,#$94,#$95,#$96,#$97,#$98,#$99,#$9a,#$9b,#$9c,#$9d,#$9e,#$9f,
      #$a0,#$a1,#$a2,#$b8,#$ba,#$a5,#$b3,#$bf,#$a8,#$a9,#$aa,#$ab,#$ac,#$b4,#$ae,#$af,
      #$b0,#$b1,#$b2,#$a8,#$aa,#$b5,#$b2,#$af,#$b8,#$b9,#$ba,#$bb,#$bc,#$a5,#$be,#$bf,
      #$fe,#$e0,#$e1,#$f6,#$e4,#$e5,#$f4,#$e3,#$f5,#$e8,#$e9,#$ea,#$eb,#$ec,#$ed,#$ee,
      #$ef,#$ff,#$f0,#$f1,#$f2,#$f3,#$e6,#$e2,#$fc,#$fb,#$e7,#$f8,#$fd,#$f9,#$f7,#$fa,
      #$de,#$c0,#$c1,#$d6,#$c4,#$c5,#$d4,#$c3,#$d5,#$c8,#$c9,#$ca,#$cb,#$cc,#$cd,#$ce,
      #$cf,#$df,#$d0,#$d1,#$d2,#$d3,#$c6,#$c2,#$dc,#$db,#$c7,#$d8,#$dd,#$d9,#$d7,#$da );

  Table_Cp1251_Koi8u : TTransTable =
    ( #$80,#$81,#$82,#$83,#$84,#$85,#$86,#$87,#$88,#$89,#$8a,#$8b,#$8c,#$8d,#$8e,#$8f,
      #$90,#$91,#$92,#$93,#$94,#$95,#$96,#$97,#$98,#$99,#$9a,#$9b,#$9c,#$9d,#$9e,#$9f,
      #$a0,#$a1,#$a2,#$a3,#$a4,#$bd,#$a6,#$a7,#$b3,#$a9,#$b4,#$ab,#$ac,#$ad,#$ae,#$b7,
      #$b0,#$b1,#$b6,#$a6,#$ad,#$b5,#$b6,#$b7,#$a3,#$b9,#$a4,#$bb,#$bc,#$bd,#$be,#$a7,
      #$e1,#$e2,#$f7,#$e7,#$e4,#$e5,#$f6,#$fa,#$e9,#$ea,#$eb,#$ec,#$ed,#$ee,#$ef,#$f0,
      #$f2,#$f3,#$f4,#$f5,#$e6,#$e8,#$e3,#$fe,#$fb,#$fd,#$ff,#$f9,#$f8,#$fc,#$e0,#$f1,
      #$c1,#$c2,#$d7,#$c7,#$c4,#$c5,#$d6,#$da,#$c9,#$ca,#$cb,#$cc,#$cd,#$ce,#$cf,#$d0,
      #$d2,#$d3,#$d4,#$d5,#$c6,#$c8,#$c3,#$de,#$db,#$dd,#$df,#$d9,#$d8,#$dc,#$c0,#$d1 );

  Table_Iso88592_Koi8: TTransTable =
    ( #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
      #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
      #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
      #0, #236, #0, #0, #0, #243, #0, #244, #0, #0, #250, #0,
      #254, #0, #0, #0, #224, #204, #0, #220, #0, #211, #0, #212,
      #0, #0, #218, #0, #230, #225, #0, #248, #241, #235, #0, #0,
      #227, #247, #0, #0, #229, #233, #0, #228, #0, #0, #238, #239,
      #240, #0, #237, #0, #242, #234, #245, #0, #232, #249, #0, #0,
      #198, #193, #0, #0, #209, #203, #0, #0, #195, #215, #0, #0,
      #197, #201, #0, #196, #0, #0, #206, #207, #208, #0, #205, #0,
      #210, #202, #213, #0, #200, #217, #0, #0);

  Table_Koi8_Iso88592: TTransTable =
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #225, #0, #232, #239, #236, #224, #0,
     #252, #237, #249, #229, #181, #246, #242, #243, #244, #228, #248, #185,
     #187, #250, #0, #233, #0, #253, #190, #0, #183, #0, #0, #0,
     #180, #193, #0, #200, #207, #204, #192, #0, #220, #205, #217, #197,
     #165, #214, #210, #211, #212, #196, #216, #169, #171, #218, #0, #201,
     #195, #221, #174, #0, #0, #0, #176, #0);

{ this converts between windows charset (CentralEurope version)

  Table_Cp1250_Koi8: TTransTable =
     #0, #244, #250, #0, #0, #96, #39, #0, #34, #0, #0, #0,
     #0, #0, #211, #0, #0, #212, #218, #0, #0, #220, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #254, #0, #0, #0, #224, #0, #0, #0, #0, #0, #0, #0,
     #236, #0, #204, #0, #230, #225, #0, #248, #241, #235, #0, #0,
     #227, #247, #0, #0, #229, #233, #0, #228, #0, #0, #238, #239,
     #240, #0, #237, #0, #242, #234, #245, #0, #232, #249, #0, #0,
     #198, #193, #0, #0, #209, #203, #0, #0, #195, #215, #0, #0,
     #197, #201, #0, #196, #0, #0, #206, #207, #208, #0, #205, #0,
     #210, #202, #213, #0, #200, #217, #0, #0);

  Table_Koi8_Cp1250: TTransTable =
   ( #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #225, #0, #232, #239, #236, #224, #0,
     #252, #237, #249, #229, #190, #246, #242, #243, #244, #228, #248, #154,
     #157, #250, #0, #233, #0, #253, #158, #0, #161, #0, #0, #0,
     #180, #193, #0, #200, #207, #204, #192, #0, #220, #205, #217, #197,
     #188, #214, #210, #211, #212, #196, #216, #138, #141, #218, #0, #201,
     #195, #221, #142, #0, #0, #0, #176, #0);

{ Local convertion }
function XConvert(Value: string; Table: PChar): string;
var
  Ptr: PChar;
begin
  Ptr := PChar(Value);
  while Ptr^ <> #0 do
  begin
    if (Ptr^ >= #128) and (Table[Ord(Ptr^)-128] <> #0) then
      Ptr^ := Table[Ord(Ptr^)-128];
    Inc(Ptr);
  end;
  Result := Value;
end;

{ Convert following encodings }
function Convert(const Value: string; SrcEnc, DestEnc: TEncodingType): string;
var
  Table: PChar;
begin
  Result := Value;
  if Result = '' then Exit;
  if SrcEnc = DestEnc then Exit;
  {$IFDEF LINUX} // Make kylix compiler happy
  Table := nil;
  {$ENDIF}
  case SrcEnc of
    et866:
      case DestEnc of
        etKoi8r: Table := Table_866_Koi8r;
        etNone:  Table := Table_866_Cp1251;
        else     Exit;
      end;
    etKoi8r:
      case DestEnc of
        et866:  Table := Table_Koi8r_866;
        etCp1250: Table := Table_Koi8_Cp1250;
        etCp1251: Table := Table_Koi8r_Cp1251;
        etNone: Table := Table_Koi8r_Cp1251;
        etIso88592: Table := Table_Koi8_Iso88592;
        else    Exit;
      end;
    etKoi8u:
      case DestEnc of
        etNone: Table := Table_Koi8u_Cp1251;
        else    Exit;
      end;
    etIso88592:
      case DestEnc of
        etNone:  Table := Table_Iso88592_Cp1250;
        etKoi8r: Table := Table_Iso88592_Koi8;
        else     Exit;
      end;
    etCp1250:
      case DestEnc of
        etKoi8r:    Table := Table_Cp1250_Koi8;
        etIso88592: Table := Table_Cp1250_Iso88592;
        else        Exit;
      end;
    etCp1251:
      case DestEnc of
        et866:      Table := Table_Cp1251_866;
        etKoi8r:    Table := Table_Cp1251_Koi8r;
        etKoi8u:    Table := Table_Cp1251_Koi8u;
        else        Exit;
      end;
    etNone:
      case DestEnc of
        et866:      Table := Table_Cp1251_866;
        etKoi8r:    Table := Table_Cp1251_Koi8r;
        etKoi8u:    Table := Table_Cp1251_Koi8u;
        etIso88592: Table := Table_Cp1250_Iso88592;
        else        Exit;
      end;
    else
      Exit;
  end;
  if Table = nil then Exit;
  Result := XConvert(Result, Table);
end;

end.