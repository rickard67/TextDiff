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
  DIFF_MAX_DIAGONAL = $FFFFFF; //~16 million

type
  TDiffAlgorithm = (algND,algNP);

  PDiffDiags = ^TDiffDiags;
  TDiffDiags = array [-DIFF_MAX_DIAGONAL .. DIFF_MAX_DIAGONAL] of integer;

  TDiffChangeKind = (ckNone, ckAdd, ckDelete, ckModify);

  PDiffCompareRec = ^TDiffCompareRec;
  TDiffCompareRec = record
    Kind      : TDiffChangeKind;
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
