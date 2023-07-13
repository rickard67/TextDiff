unit DiffTypes;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Generics.Collections, Windows,
{$ELSE}
  LCLIntf, LCLType, FGL,
{$ENDIF}
  SysUtils,
  Math,
  Forms,
  Classes;

const
  MAX_DIAGONAL = $FFFFFF; //~16 million

type
  TDiffAlgorithm = (algND,algNP);

  P8Bits = PByte;

  PDiags = ^TDiags;
  TDiags = array [-MAX_DIAGONAL .. MAX_DIAGONAL] of integer;

  TChangeKind = (ckNone, ckAdd, ckDelete, ckModify);

  PCompareRec = ^TCompareRec;
  TCompareRec = record
    Kind      : TChangeKind;
    oldIndex1,
    oldIndex2 : integer;
    case boolean of
      false   : (chr1, chr2 : Char);
      true    : (int1, int2 : integer);
  end;

  PDiffVars = ^TDiffVars;
  TDiffVars = record
    offset1 : integer;
    offset2 : integer;
    len1    : integer;
    len2    : integer;
  end;

  TDiffStats = record
    matches  : integer;
    adds     : integer;
    deletes  : integer;
    modifies : integer;
  end;

implementation



end.
