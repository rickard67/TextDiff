unit Diff;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

(*******************************************************************************
* Component         TDiff                                                      *
* Version:          1.0                                                        *
* Date:             12 July 2023                                               *
* Compilers:        Delphi 10.x                                                *
* Author:           Rickard Johansson                                          *
* Copyright:        © 2023 Rickard Johansson                                   *
*                                                                              *
* Licence to use, terms and conditions:                                        *
*                   The code in the TDiff component is released as freeware    *
*                   provided you agree to the following terms & conditions:    *
*                   1. the copyright notice, terms and conditions are          *
*                   left unchanged                                             *
*                   2. modifications to the code by other authors must be      *
*                   clearly documented and accompanied by the modifier's name. *
*                   3. the TDiff component may be freely compiled into binary  *
*                   format and no acknowledgement is required. However, a      *
*                   discrete acknowledgement would be appreciated (eg. in a    *
*                   program's 'About Box').                                    *
*                                                                              *
* Description:      Component to list differences between two integer arrays   *
*                   using a "longest common subsequence" algorithm.            *
*                   Typically, this component is used to diff 2 text files     *
*                   once their individuals lines have been hashed.             *
*                                                                              *
*                                                                              *
*******************************************************************************)


(*******************************************************************************
* History:                                                                     *
* 12 July 2023     - Original Release.                                         *
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
  DiffTypes,
  Diff_ND,
  Diff_NP;

type
  TDiff = class(TComponent)
  private
    FCancelled: boolean;
    FExecuting: boolean;
    FDiffAlgorithm: TDiffAlgorithm;
    FDiff_ND: TNDDiff;
    FDiff_NP: TNPDiff;
    function GetCompareCount: integer;
    function GetCompare(index: integer): TCompareRec;
    function GetDiffStats: TDiffStats;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Compare strings or list of Cardinals ...
    {$IFDEF FPC}
    function Execute(const alist1, alist2: TIntegerList): boolean; overload;
    {$ELSE}
    function Execute(const alist1, alist2: TList<Cardinal>; const aDiffAlgorithm: TDiffAlgorithm = algND): boolean;
        overload;
    {$ENDIF}
    function Execute(const s1, s2: string; const aDiffAlgorithm: TDiffAlgorithm = algND): boolean; overload;

    // Cancel allows interrupting excessively prolonged comparisons
    procedure Cancel;
    procedure Clear;
    property Cancelled: boolean read FCancelled;
    property Count: integer read GetCompareCount;
    property Compares[index: integer]: TCompareRec read GetCompare; default;
    property DiffAlgorithm: TDiffAlgorithm read FDiffAlgorithm write FDiffAlgorithm;
    property DiffStats: TDiffStats read GetDiffStats;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TDiff]);
end;

constructor TDiff.Create(aOwner: TComponent);
begin
  inherited;
  FDiff_ND := TNDDiff.Create(AOwner);
  FDiff_NP := TNPDiff.Create(AOwner);
  FDiffAlgorithm := algNP;
end;
//------------------------------------------------------------------------------

destructor TDiff.Destroy;
begin
//  FDiff_ND.Free;
//  FDiff_NP.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TDiff.Execute(const s1, s2: string; const aDiffAlgorithm: TDiffAlgorithm = algND): boolean;
begin
  Result := not FExecuting;
  if not Result then exit;
  FCancelled := false;
  FExecuting := true;
  FDiffAlgorithm := aDiffAlgorithm;
  try
    if aDiffAlgorithm = algND then
      FDiff_ND.Execute(s1, s2)
    else if aDiffAlgorithm = algNP then
      FDiff_NP.Execute(s1, s2);
  finally
    FExecuting := false;
  end;
end;
//------------------------------------------------------------------------------

{$IFDEF FPC}
function TDiff.Execute(const alist1, alist2: TIntegerList; const aDiffAlgorithm: TDiffAlgorithm): boolean;
{$ELSE}
function TDiff.Execute(const alist1, alist2: TList<Cardinal>; const aDiffAlgorithm: TDiffAlgorithm = algND): boolean;
{$ENDIF}
begin
  Result := not FExecuting;
  if not Result then exit;
  FCancelled := false;
  FExecuting := true;
  FDiffAlgorithm := aDiffAlgorithm;
  try
    if aDiffAlgorithm = algND then
      FDiff_ND.Execute(alist1, alist2)
    else if aDiffAlgorithm = algNP then
      FDiff_NP.Execute(alist1, alist2);
  finally
    FExecuting := false;
  end;
end;
//------------------------------------------------------------------------------

function TDiff.GetCompareCount: integer;
begin
  if FDiffAlgorithm = algND then
    Result := FDiff_ND.CompareList.Count
  else
    Result := FDiff_NP.CompareList.count;
end;
//------------------------------------------------------------------------------

function TDiff.GetCompare(index: integer): TCompareRec;
begin
  if FDiffAlgorithm = algND then
    Result := PCompareRec(FDiff_ND.CompareList[index])^
  else if FDiffAlgorithm = algNP then
    Result := PCompareRec(FDiff_NP.CompareList[index])^;
end;
//------------------------------------------------------------------------------

procedure TDiff.Cancel;
begin
  FCancelled := True;
  if FDiffAlgorithm = algND then
    FDiff_ND.Cancel
  else if FDiffAlgorithm = algNP then
    FDiff_NP.Cancel;
end;

procedure TDiff.Clear;
begin
  if FDiffAlgorithm = algND then
    FDiff_ND.Clear
  else if FDiffAlgorithm = algNP then
    FDiff_NP.Clear;
end;

function TDiff.GetDiffStats: TDiffStats;
begin
  if FDiffAlgorithm = algND then
    Result := FDiff_ND.DiffStats
  else if FDiffAlgorithm = algNP then
    Result := FDiff_NP.DiffStats;
end;

//------------------------------------------------------------------------------

end.
