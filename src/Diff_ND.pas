unit Diff_ND;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

(*******************************************************************************
* Component         TNDDiff                                                    *
* Version:          5.21                                                       *
* Date:             17 June 2025                                               *
* Compilers:        Delphi 10.x                                                *
* Author:           Angus Johnson - angusj-AT-myrealbox-DOT-com                *
* Copyright:        � 2001-2009 Angus Johnson                                  *
* Updated by:       Rickard Johansson (RJ TextEd)                              *
*                                                                              *
* Licence to use, terms and conditions:                                        *
*                   The code in the TNDDiff component is released as freeware  *
*                   provided you agree to the following terms & conditions:    *
*                   1. the copyright notice, terms and conditions are          *
*                   left unchanged                                             *
*                   2. modifications to the code by other authors must be      *
*                   clearly documented and accompanied by the modifier's name. *
*                   3. the TNDDiff component may be freely compiled into binary*
*                   format and no acknowledgement is required. However, a      *
*                   discrete acknowledgement would be appreciated (eg. in a    *
*                   program's 'About Box').                                    *
*                                                                              *
* Description:      Component to list differences between two integer arrays   *
*                   using a "longest common subsequence" algorithm.            *
*                   Typically, this component is used to diff 2 text files     *
*                   once their individuals lines have been hashed.             *
*                                                                              *
* Acknowledgements: The key algorithm in this component is based on:           *
*                   "An O(ND) Difference Algorithm and its Variations"         *
*                   By E Myers - Algorithmica Vol. 1 No. 2, 1986, pp. 251-266  *
*                   http://www.cs.arizona.edu/people/gene/                     *
*                   http://www.cs.arizona.edu/people/gene/PAPERS/diff.ps       *
*                                                                              *
*******************************************************************************)


(*******************************************************************************
* History:                                                                     *
* 13 December 2001 - Original Release                                          *
* 22 April 2008    - Complete rewrite to greatly improve the code and          *
*                    provide a much simpler view of differences through a new  *
*                    'Compares' property.                                      *
* 7 November 2009  - Updated so now compiles in newer versions of Delphi.      *
*                                                                              *
* 11 November 2018 - Added TList<Cardinal> to store hash values                *
*                    Made some minor code formatting and code changes          *
* 19 May 2020        Added Lazarus support                                     *
* 12 July 2023       Made some changes to enable switching algorithm between   *
*                    O(ND) and O(NP) and fixed several issues and range        *
*                    errors.                                                   *
*                                                                              *
* 16 Apr 2025        Fixed an issue in Execute(const s1, s2: string)           *
* 1 May 2025       - Added option to ignore case when comparing strings using  *
*                    Execute(s1, s2, bIgnoreCase).                             *
*                                                                              *
* 17 June 2025     - Minor fix in RecursiveDiffChr()                           *
*******************************************************************************)

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
  Classes,
  DiffTypes;

const
  //Maximum realistic deviation from centre diagonal vector ...
  MAX_DIAGONAL = $FFFFFF; //~16 million

type
  {$IFDEF FPC}
  TIntegerList = TFPGList<Cardinal>;
  {$ENDIF}

  TNDDiff = class(TComponent)
  private
    FCompareList: TList;
    FCancelled: boolean;
    FExecuting: boolean;
    FDiagBuffer, bDiagBuffer: pointer;
    FStr1: string;
    FStr2: string;
    {$IFDEF FPC}
    FList1: TIntegerList;
    FList2: TIntegerList;
    {$ELSE}
    FList1: TList<Cardinal>;
    FList2: TList<Cardinal>;
    {$ENDIF}
    LastCompareRec: TCompareRec;
    fDiag, bDiag: PDiags;
    fDiffStats: TDiffStats;
    FIgnoreCase: Boolean;
    procedure InitDiagArrays(MaxOscill, len1, len2: integer);
    //nb: To optimize speed, separate functions are called for either
    //integer or character compares ...
    procedure RecursiveDiffChr(offset1, offset2, len1, len2: integer);
    procedure AddChangeChrs(offset1, range: integer; ChangeKind: TChangeKind);
    procedure RecursiveDiffInt(offset1, offset2, len1, len2: integer);
    procedure AddChangeInts(offset1, range: integer; ChangeKind: TChangeKind);
    function CompareChr(const ch1, ch2: Char): Boolean;

    function GetCompareCount: integer;
    function GetCompare(index: integer): TCompareRec;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Compare strings or list of Cardinals ...
    {$IFDEF FPC}
    function Execute(const alist1, alist2: TIntegerList): boolean; overload;
    {$ELSE}
    function Execute(const alist1, alist2: TList<Cardinal>): boolean; overload;
    {$ENDIF}
    function Execute(const s1, s2: string; const bIgnoreCase: Boolean = False): boolean; overload;

    // Cancel allows interrupting excessively prolonged comparisons
    procedure Cancel;
    procedure Clear;

    property Cancelled: boolean read fCancelled;
    property CompareList: TList read FCompareList write FCompareList;
    property Count: integer read GetCompareCount;
    property Compares[index: integer]: TCompareRec read GetCompare; default;
    property DiffStats: TDiffStats read fDiffStats;
  end;

implementation

uses
  System.Character;

constructor TNDDiff.Create(aOwner: TComponent);
begin
  inherited;
  fCompareList := TList.create;
  FIgnoreCase := False;
end;
//------------------------------------------------------------------------------

destructor TNDDiff.Destroy;
begin
  Clear;
  fCompareList.free;
  inherited;
end;
//------------------------------------------------------------------------------

function TNDDiff.Execute(const s1, s2: string; const bIgnoreCase: Boolean = False): boolean;
var
  maxOscill, x1,x2, savedLen: integer;
  compareRec: PCompareRec;
  len1,len2: Integer;
  l1,l2: Integer;
begin
  result := not fExecuting;
  if not result then exit;
  fExecuting := true;
  fCancelled := false;
  FIgnoreCase := bIgnoreCase;
  try
    Clear;
    len1 := Length(s1);
    len2 := Length(s2);

    //save first string length for later (ie for any trailing matches) ...
    savedLen := len1;

    //setup the character arrays ...
    FStr1 := s1;
    FStr2 := s2;

    //ignore top matches ...
    x1:= 1; x2 := 1;
    while (len1 > 0) and (len2 > 0) and CompareChr(FStr1[len1], FStr2[len2]) do
    begin
      dec(len1); dec(len2);
    end;

    //if something doesn't match ...
    if (len1 <> 0) or (len2 <> 0) then
    begin
      //ignore bottom of matches too ...
      l1 := len1; l2 := len2;
      while (len1 > 0) and (len2 > 0) and (x1 > 0) and (x2 > 0) and (x1 <= Length(FStr1)) and (x2 <= Length(FStr2)) and CompareChr(FStr1[x1], FStr2[x2]) do
      begin
        if (x1 < Length(FStr1)) and (x2 < Length(FStr2)) and not CompareChr(FStr1[x1+1], FStr2[x2+1]) and (CompareChr(FStr1[x1], FStr1[x1+1]) or CompareChr(FStr1[x1], FStr2[x2+1])) then
        begin
          // Reset if we have strings like
          //
          // aaabc : aabcd
          //
          // Character 3 is still the same 'a' in string 1 but different 'b' in string 2. The algorithm needs to handle this
          // so reset x1 and x2 to 1.
          x1:= 1;
          x2 := 1;
          len1 := l1;
          len2 := l2;
          Break;
        end;
        dec(len1); dec(len2);
        inc(x1); inc(x2);
      end;

      maxOscill := min(max(len1,len2), MAX_DIAGONAL);
      fCompareList.Capacity := len1 + len2;

      //nb: the Diag arrays are extended by 1 at each end to avoid testing
      //for array limits. Hence '+3' because will also includes Diag[0] ...
      GetMem(fDiagBuffer, sizeof(integer)*(maxOscill*2+3));
      GetMem(bDiagBuffer, sizeof(integer)*(maxOscill*2+3));
      try
        RecursiveDiffChr(x1, x2, len1, len2);
      finally
        freeMem(fDiagBuffer);
        freeMem(bDiagBuffer);
      end;
    end;

    if fCancelled then
    begin
      result := false;
      Clear;
      exit;
    end;

    //finally, append any trailing matches onto compareList ...
    if LastCompareRec.oldIndex1 < 0 then LastCompareRec.oldIndex1 := 0;
    if LastCompareRec.oldIndex2 < 0 then LastCompareRec.oldIndex2 := 0;
    while (LastCompareRec.oldIndex1 < savedLen) do
    begin
      with LastCompareRec do
      begin
        Kind := ckNone;
        inc(oldIndex1);
        inc(oldIndex2);
        if (oldIndex1 > 0) and (oldIndex1 <= Length(FStr1)) then
          chr1 := FStr1[oldIndex1];
        if (oldIndex2 > 0) and (oldIndex2 <= Length(FStr2)) then
          chr2 := FStr2[oldIndex2];
      end;
      New(compareRec);
      compareRec^ := LastCompareRec;
      fCompareList.Add(compareRec);
      inc(fDiffStats.matches);
    end;
  finally
    fExecuting := false;
  end;

end;
//------------------------------------------------------------------------------

{$IFDEF FPC}
function TNDDiff.Execute(const alist1, alist2: TIntegerList): boolean;
{$ELSE}
function TNDDiff.Execute(const alist1, alist2: TList<Cardinal>): boolean;
{$ENDIF}
var
  maxOscill, x1,x2, savedLen: integer;
  compareRec: PCompareRec;
  len1,len2: Integer;
begin
  result := not fExecuting;
  if not result then exit;
  fExecuting := true;
  fCancelled := false;
  try
    Clear;

    //setup the character arrays ...
    FList1 := alist1;
    FList2 := alist2;
    len1 := FList1.Count;
    len2 := FList2.Count;

    //save first string length for later (ie for any trailing matches) ...
    savedLen := len1-1;

    //ignore top matches ...
    x1:= 0; x2 := 0;
    while (len1 > 0) and (len2 > 0) and (FList1[len1-1] = FList2[len2-1]) do
    begin
      dec(len1); dec(len2);
    end;

    //if something doesn't match ...
    if (len1 <> 0) or (len2 <> 0) then
    begin

      //ignore bottom of matches too ...
      while (len1 > 0) and (len2 > 0) and (FList1[x1] = FList2[x2]) do
      begin
        dec(len1); dec(len2);
        inc(x1); inc(x2);
      end;

      maxOscill := min(max(len1,len2), MAX_DIAGONAL);
      fCompareList.Capacity := len1 + len2;

      //nb: the Diag arrays are extended by 1 at each end to avoid testing
      //for array limits. Hence '+3' because will also includes Diag[0] ...
      GetMem(fDiagBuffer, sizeof(integer)*(maxOscill*2+3));
      GetMem(bDiagBuffer, sizeof(integer)*(maxOscill*2+3));
      try
        RecursiveDiffInt(x1, x2, len1, len2);
      finally
        freeMem(fDiagBuffer);
        freeMem(bDiagBuffer);
      end;
    end;

    if fCancelled then
    begin
      result := false;
      Clear;
      exit;
    end;

    //finally, append any trailing matches onto compareList ...
    while (LastCompareRec.oldIndex1 < savedLen) do
    begin
      with LastCompareRec do
      begin
        Kind := ckNone;
        inc(oldIndex1);
        inc(oldIndex2);
        int1 := Integer(FList1[oldIndex1]);
        int2 := Integer(FList2[oldIndex2]);
      end;
      New(compareRec);
      compareRec^ := LastCompareRec;
      fCompareList.Add(compareRec);
      inc(fDiffStats.matches);
    end;
  finally
    fExecuting := false;
  end;

end;
//------------------------------------------------------------------------------

procedure TNDDiff.InitDiagArrays(MaxOscill, len1, len2: integer);
var
  diag: integer;
begin
  inc(maxOscill); //for the extra diag at each end of the arrays ...
  P8Bits(fDiag) := P8Bits(fDiagBuffer) - sizeof(integer)*(MAX_DIAGONAL-maxOscill);
  P8Bits(bDiag) := P8Bits(bDiagBuffer) - sizeof(integer)*(MAX_DIAGONAL-maxOscill);
  //initialize Diag arrays (assumes 0 based arrays) ...
  for diag := - maxOscill to maxOscill do fDiag[diag] := -MAXINT;
  fDiag[0] := -1;
  for diag := - maxOscill to maxOscill do bDiag[diag] := MAXINT;
  bDiag[len1 - len2] := len1-1;
end;
//------------------------------------------------------------------------------

procedure TNDDiff.RecursiveDiffChr(offset1, offset2, len1, len2: integer);
var
  diag, lenDelta, Oscill, maxOscill, x1, x2: integer;
begin
  //nb: the possible depth of recursion here is most unlikely to cause
  //    problems with stack overflows.
//  application.processmessages;
  if fCancelled then exit;

  if (len1 = 0) then
  begin
    AddChangeChrs(offset1, len2, ckAdd);
    exit;
  end
  else if (len2 = 0) then
  begin
    AddChangeChrs(offset1, len1, ckDelete);
    exit;
  end
  else if (len1 = 1) and (len2 = 1) then
  begin
    AddChangeChrs(offset1, 1, ckDelete);
    AddChangeChrs(offset1, 1, ckAdd);
    exit;
  end
  else if (len1 < 0) or (len2 < 0) then
  begin
    Exit;
  end;

  maxOscill := min(max(len1,len2), MAX_DIAGONAL);
  InitDiagArrays(MaxOscill, len1, len2);
  lenDelta := len1 -len2;

  Oscill := 1; //ie assumes prior filter of top and bottom matches
  while Oscill <= maxOscill do
  begin

    if (Oscill mod 200) = 0 then
    begin
      application.processmessages;
      if fCancelled then exit;
    end;

    //do forward oscillation (keeping diag within assigned grid)...
    diag := Oscill;
    while diag > len1 do dec(diag,2);
    while diag >= max(- Oscill, -len2) do
    begin
      if fDiag[diag-1] < fDiag[diag+1] then
        x1 := fDiag[diag+1]
      else
        x1 := fDiag[diag-1]+1;
      x2 := x1 - diag;
      while (x1 < len1-1) and (x2 < len2-1) and (offset1+x1+1 > 0) and (offset2+x2+1 > 0) and
            (offset1+x1+1 <= Length(FStr1)) and (offset2+x2+1 <= Length(FStr2)) and CompareChr(FStr1[offset1+x1+1], FStr2[offset2+x2+1]) do
      begin
        inc(x1); inc(x2);
      end;
      fDiag[diag] := x1;

      //nb: (fDiag[diag] is always < bDiag[diag]) here when NOT odd(lenDelta) ...
      if odd(lenDelta) and (fDiag[diag] >= bDiag[diag]) then
      begin
        inc(x1);inc(x2);
        //save x1 & x2 for second recursive_diff() call by reusing no longer
        //needed variables (ie minimize variable allocation in recursive fn) ...
        diag := x1; Oscill := x2;
        while (x1 > 0) and (x2 > 0) and (offset1+x1+1 > 0) and (offset2+x2-1 > 0) and
              (offset1+x1-1 <= Length(FStr1)) and (offset2+x2-1 <= Length(FStr2)) and CompareChr(FStr1[offset1+x1-1], FStr2[offset2+x2-1]) do
        begin
          dec(x1); dec(x2);
        end;
        RecursiveDiffChr(offset1, offset2, x1, x2);
        x1 := diag; x2 := Oscill;
        RecursiveDiffChr(offset1+x1, offset2+x2, len1-x1, len2-x2);
        exit; //ALL DONE
      end;
      dec(diag,2);
    end;

    //do backward oscillation (keeping diag within assigned grid)...
    diag := lenDelta + Oscill;
    while diag > len1 do dec(diag,2);
    while diag >= max(lenDelta - Oscill, -len2)  do
    begin
      if bDiag[diag-1] < bDiag[diag+1] then
        x1 := bDiag[diag-1] else
        x1 := bDiag[diag+1]-1;
      x2 := x1 - diag;
      while (offset1+x1 > 0) and (offset2+x2 > 0) and (offset1+x1 <= Length(FStr1)) and (offset2+x2 <= Length(FStr1)) and
            CompareChr(FStr1[offset1+x1], FStr2[offset2+x2]) do
      begin
        dec(x1); dec(x2);
      end;
      bDiag[diag] := x1;

      if bDiag[diag] <= fDiag[diag] then
      begin
        //flag return value then ...
        inc(x1);inc(x2);
        RecursiveDiffChr(offset1, offset2, x1, x2);
        while (x1 < len1) and (x2 < len2) and (offset1+x1 <= Length(FStr1)) and (offset2+x2 <= Length(FStr1)) and
              CompareChr(FStr1[offset1+x1], FStr2[offset2+x2]) do
        begin
          inc(x1); inc(x2);
        end;
        RecursiveDiffChr(offset1+x1, offset2+x2, len1-x1, len2-x2);
        exit; //ALL DONE
      end;
      dec(diag,2);
    end;

    inc(Oscill);
  end; //while Oscill <= maxOscill

  raise Exception.create('oops - error in RecursiveDiffChr()');
end;
//------------------------------------------------------------------------------

procedure TNDDiff.RecursiveDiffInt(offset1, offset2, len1, len2: integer);
var
  diag, lenDelta, Oscill, maxOscill, x1, x2: integer;
begin
  //nb: the possible depth of recursion here is most unlikely to cause
  //    problems with stack overflows.
//  application.processmessages;
  if fCancelled then exit;

  if (len1 = 0) then
  begin
    assert(len2 > 0,'oops!');
    AddChangeInts(offset1, len2, ckAdd);
    exit;
  end
  else if (len2 = 0) then
  begin
    AddChangeInts(offset1, len1, ckDelete);
    exit;
  end
  else if (len1 = 1) and (len2 = 1) then
  begin
    assert(FList1[offset1] <> FList2[offset2],'oops!');
    AddChangeInts(offset1, 1, ckDelete);
    AddChangeInts(offset1, 1, ckAdd);
    exit;
  end;

  maxOscill := min(max(len1,len2), MAX_DIAGONAL);
  InitDiagArrays(MaxOscill, len1, len2);
  lenDelta := len1 -len2;

  Oscill := 1; //ie assumes prior filter of top and bottom matches
  while Oscill <= maxOscill do
  begin

    if (Oscill mod 200) = 0 then
    begin
      application.processmessages;
      if fCancelled then exit;
    end;

    //do forward oscillation (keeping diag within assigned grid)...
    diag := Oscill;
    while diag > len1 do dec(diag,2);
    while diag >= max(- Oscill, -len2) do
    begin
      if fDiag[diag-1] < fDiag[diag+1] then
        x1 := fDiag[diag+1] else
        x1 := fDiag[diag-1]+1;
      x2 := x1 - diag;
      while (x1 < len1-1) and (x2 < len2-1) and
        (FList1[offset1+x1+1] = FList2[offset2+x2+1]) do
      begin
        inc(x1); inc(x2);
      end;
      fDiag[diag] := x1;

      //nb: (fDiag[diag] is always < bDiag[diag]) here when NOT odd(lenDelta) ...
      if odd(lenDelta) and (fDiag[diag] >= bDiag[diag]) then
      begin
        inc(x1);inc(x2);
        //save x1 & x2 for second recursive_diff() call by reusing no longer
        //needed variables (ie minimize variable allocation in recursive fn) ...
        diag := x1; Oscill := x2;
        while (x1 > 0) and (x2 > 0) and (FList1[offset1+x1-1] = FList2[offset2+x2-1]) do
        begin
          dec(x1); dec(x2);
        end;
        RecursiveDiffInt(offset1, offset2, x1, x2);
        x1 := diag; x2 := Oscill;
        RecursiveDiffInt(offset1+x1, offset2+x2, len1-x1, len2-x2);
        exit; //ALL DONE
      end;
      dec(diag,2);
    end;

    //do backward oscillation (keeping diag within assigned grid)...
    diag := lenDelta + Oscill;
    while diag > len1 do dec(diag,2);
    while diag >= max(lenDelta - Oscill, -len2)  do
    begin
      if bDiag[diag-1] < bDiag[diag+1] then
        x1 := bDiag[diag-1] else
        x1 := bDiag[diag+1]-1;
      x2 := x1 - diag;
      while (x1 > -1) and (x2 > -1) and (FList1[offset1+x1] = FList2[offset2+x2]) do
      begin
        dec(x1); dec(x2);
      end;
      bDiag[diag] := x1;

      if bDiag[diag] <= fDiag[diag] then
      begin
        //flag return value then ...
        inc(x1);inc(x2);
        RecursiveDiffInt(offset1, offset2, x1, x2);
        while (x1 < len1) and (x2 < len2) and
          (FList1[offset1+x1] = FList2[offset2+x2]) do
        begin
          inc(x1); inc(x2);
        end;
        RecursiveDiffInt(offset1+x1, offset2+x2, len1-x1, len2-x2);
        exit; //ALL DONE
      end;
      dec(diag,2);
    end;

    inc(Oscill);
  end; //while Oscill <= maxOscill

  raise Exception.create('oops - error in RecursiveDiffInt()');
end;
//------------------------------------------------------------------------------

procedure TNDDiff.Clear;
var
  i: integer;
begin
  for i := 0 to fCompareList.Count-1 do
    dispose(PCompareRec(fCompareList[i]));
  fCompareList.clear;
  LastCompareRec.Kind := ckNone;
  LastCompareRec.oldIndex1 := -1;
  LastCompareRec.oldIndex2 := -1;
  fDiffStats.matches := 0;
  fDiffStats.adds := 0;
  fDiffStats.deletes :=0;
  fDiffStats.modifies :=0;
end;
//------------------------------------------------------------------------------

function TNDDiff.GetCompareCount: integer;
begin
  result := fCompareList.count;
end;
//------------------------------------------------------------------------------

function TNDDiff.GetCompare(index: integer): TCompareRec;
begin
  result := PCompareRec(fCompareList[index])^;
end;
//------------------------------------------------------------------------------

procedure TNDDiff.AddChangeChrs(offset1, range: integer; ChangeKind: TChangeKind);
var
  i,j: integer;
  compareRec: PCompareRec;
begin
  //first, add any unchanged items into this list ...
  if LastCompareRec.oldIndex1 < 0 then LastCompareRec.oldIndex1 := 0;
  if LastCompareRec.oldIndex2 < 0 then LastCompareRec.oldIndex2 := 0;
  while (LastCompareRec.oldIndex1 < offset1 -1) do
  begin
    with LastCompareRec do
    begin
      chr1 := #0;
      chr2 := #0;
      Kind := ckNone;
      inc(oldIndex1);
      inc(oldIndex2);
      if (oldIndex1 > 0) and (oldIndex1 <= Length(FStr1)) then
        chr1 := FStr1[oldIndex1];
      if (oldIndex2 > 0) and (oldIndex2 <= Length(FStr2)) then
        chr2 := FStr2[oldIndex2];
    end;
    New(compareRec);
    compareRec^ := LastCompareRec;
    fCompareList.Add(compareRec);
    inc(fDiffStats.matches);
  end;

  case ChangeKind of
    ckNone:
      for i := 1 to range do
      begin
        with LastCompareRec do
        begin
          Kind := ckNone;
          inc(oldIndex1);
          inc(oldIndex2);
          chr1 := FStr1[oldIndex1];
          chr2 := FStr2[oldIndex2];
        end;
        New(compareRec);
        compareRec^ := LastCompareRec;
        FCompareList.Add(compareRec);
        inc(FDiffStats.matches);
      end;
    ckAdd :
      begin
        for i := 1 to range do
        begin
          with LastCompareRec do
          begin

            //check if a range of adds are following a range of deletes
            //and convert them to modifies ...
            if Kind = ckDelete then
            begin
              j := fCompareList.Count -1;
              while (j > 0) and (PCompareRec(fCompareList[j-1]).Kind = ckDelete) do
                dec(j);
              PCompareRec(fCompareList[j]).Kind := ckModify;
              dec(fDiffStats.deletes);
              inc(fDiffStats.modifies);
              inc(LastCompareRec.oldIndex2);
              PCompareRec(fCompareList[j]).oldIndex2 := LastCompareRec.oldIndex2;
              if (oldIndex2 > 0) and (oldIndex2 <= Length(FStr2)) then
                PCompareRec(fCompareList[j]).chr2 := FStr2[oldIndex2];
              if j = fCompareList.Count-1 then LastCompareRec.Kind := ckModify;
              continue;
            end;

            Kind := ckAdd;
            chr1 := #0;
            inc(oldIndex2);
            if (oldIndex2 > 0) and (oldIndex2 <= Length(FStr2)) then
              chr2 := Char(FStr2[oldIndex2]); //ie what we added
          end;
          New(compareRec);
          compareRec^ := LastCompareRec;
          fCompareList.Add(compareRec);
          inc(fDiffStats.adds);
        end;
      end;
    ckDelete :
      begin
        for i := 1 to range do
        begin
          with LastCompareRec do
          begin

            //check if a range of deletes are following a range of adds
            //and convert them to modifies ...
            if Kind = ckAdd then
            begin
              j := fCompareList.Count -1;
              while (j > 0) and (PCompareRec(fCompareList[j-1]).Kind = ckAdd) do
                dec(j);
              PCompareRec(fCompareList[j]).Kind := ckModify;
              dec(fDiffStats.adds);
              inc(fDiffStats.modifies);
              inc(LastCompareRec.oldIndex1);
              PCompareRec(fCompareList[j]).oldIndex1 := LastCompareRec.oldIndex1;
              if (oldIndex1 > 0) and (oldIndex1 <= Length(FStr1)) then
                PCompareRec(fCompareList[j]).chr1 := FStr1[oldIndex1];
              if j = fCompareList.Count-1 then LastCompareRec.Kind := ckModify;
              continue;
            end;

            Kind := ckDelete;
            chr2 := #0;
            inc(oldIndex1);
            if (oldIndex1 > 0) and (oldIndex1 <= Length(FStr1)) then
              chr1 := FStr1[oldIndex1]; //ie what we deleted
          end;
          New(compareRec);
          compareRec^ := LastCompareRec;
          fCompareList.Add(compareRec);
          inc(fDiffStats.deletes);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TNDDiff.AddChangeInts(offset1, range: integer; ChangeKind: TChangeKind);
var
  i,j: integer;
  compareRec: PCompareRec;
begin
  //first, add any unchanged items into this list ...
  while (LastCompareRec.oldIndex1 < offset1 -1) do
  begin
    with LastCompareRec do
    begin
      Kind := ckNone;
      inc(oldIndex1);
      inc(oldIndex2);
      if (oldIndex1 >= 0) and (oldIndex1 < FList1.Count) then
        int1 := Integer(FList1[oldIndex1]);
      if (oldIndex2 >= 0) and (oldIndex2 < FList2.Count) then
        int2 := Integer(FList2[oldIndex2]);
    end;
    New(compareRec);
    compareRec^ := LastCompareRec;
    fCompareList.Add(compareRec);
    inc(fDiffStats.matches);
  end;

  case ChangeKind of
    ckAdd :
      begin
        for i := 1 to range do
        begin
          with LastCompareRec do
          begin

            //check if a range of adds are following a range of deletes
            //and convert them to modifies ...
            if Kind = ckDelete then
            begin
              j := fCompareList.Count -1;
              while (j > 0) and (PCompareRec(fCompareList[j-1]).Kind = ckDelete) do
                dec(j);
              PCompareRec(fCompareList[j]).Kind := ckModify;
              dec(fDiffStats.deletes);
              inc(fDiffStats.modifies);
              inc(LastCompareRec.oldIndex2);
              PCompareRec(fCompareList[j]).oldIndex2 := LastCompareRec.oldIndex2;
              PCompareRec(fCompareList[j]).int2 := Integer(FList2[oldIndex2]);
              if j = fCompareList.Count-1 then LastCompareRec.Kind := ckModify;
              continue;
            end;

            Kind := ckAdd;
            int1 := $0;
            inc(oldIndex2);
            if (oldIndex2 >= 0) and (oldIndex2 < FList2.Count) then
              int2 := Integer(FList2[oldIndex2]); //ie what we added
          end;
          New(compareRec);
          compareRec^ := LastCompareRec;
          fCompareList.Add(compareRec);
          inc(fDiffStats.adds);
        end;
      end;
    ckDelete :
      begin
        for i := 1 to range do
        begin
          with LastCompareRec do
          begin

            //check if a range of deletes are following a range of adds
            //and convert them to modifies ...
            if Kind = ckAdd then
            begin
              j := fCompareList.Count -1;
              while (j > 0) and (PCompareRec(fCompareList[j-1]).Kind = ckAdd) do
                dec(j);
              PCompareRec(fCompareList[j]).Kind := ckModify;
              dec(fDiffStats.adds);
              inc(fDiffStats.modifies);
              inc(LastCompareRec.oldIndex1);
              PCompareRec(fCompareList[j]).oldIndex1 := LastCompareRec.oldIndex1;
              PCompareRec(fCompareList[j]).int1 := Integer(FList1[oldIndex1]);
              if j = fCompareList.Count-1 then LastCompareRec.Kind := ckModify;
              continue;
            end;

            Kind := ckDelete;
            int2 := $0;
            inc(oldIndex1);
            if (oldIndex1 >= 0) and (oldIndex1 < FList1.Count) then
              int1 := Integer(FList1[oldIndex1]); //ie what we deleted
          end;
          New(compareRec);
          compareRec^ := LastCompareRec;
          fCompareList.Add(compareRec);
          inc(fDiffStats.deletes);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TNDDiff.Cancel;
begin
  fCancelled := true;
end;

function TNDDiff.CompareChr(const ch1, ch2: Char): Boolean;
begin
  if FIgnoreCase then
    Result := (ch1.ToLower = ch2.ToLower)
  else
    Result := (ch1 = ch2);
end;

//------------------------------------------------------------------------------

end.
