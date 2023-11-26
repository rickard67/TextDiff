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
  DiffBase,
  Diff_ND,
  Diff_NP;

type
  TDiff = class(TComponent)
  private
    FCancelled: boolean;
    FExecuting: boolean;
    FDiffAlgorithm: TDiffAlgorithm;
    function GetCompareCount: integer;
    function GetCompare(index: integer): TCompareRec;
    function GetDiffStats: TDiffStats;
  private
    FAlgos: array[TDiffAlgorithm] of TDiffBase;
  public
    constructor Create(aOwner: TComponent); override;

    // Compare strings or list of Cardinals ...
    {$IFDEF FPC}
    function Execute(const alist1, alist2: TIntegerList): boolean; overload;
    {$ELSE}
    function Execute(const alist1, alist2: TCompareList; const aDiffAlgorithm: TDiffAlgorithm = algND): boolean;
        overload;
    {$ENDIF}
    function Execute(const s1, s2: string; const aDiffAlgorithm: TDiffAlgorithm = algND): boolean; overload;

    // Cancel allows interrupting excessively prolonged comparisons
    procedure Cancel;
    procedure Clear;
    property Cancelled: boolean read FCancelled;
    property Count: integer read GetCompareCount;
    property Compares[index: integer]: TCompareRec read GetCompare; default;
    property DiffAlgorithm: TDiffAlgorithm read FDiffAlgorithm write FDiffAlgorithm default algNP;
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
  FAlgos[algND] := TNDDiff.Create(Self);
  FAlgos[algNP] := TNPDiff.Create(Self);
  FDiffAlgorithm := algNP;
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
    Result := FAlgos[aDiffAlgorithm].Execute(s1,s2);
  finally
    FExecuting := false;
  end;
end;
//------------------------------------------------------------------------------

{$IFDEF FPC}
function TDiff.Execute(const alist1, alist2: TIntegerList; const aDiffAlgorithm: TDiffAlgorithm): boolean;
{$ELSE}
function TDiff.Execute(const alist1, alist2: TCompareList; const aDiffAlgorithm: TDiffAlgorithm = algND): boolean;
{$ENDIF}
begin
  Result := not FExecuting;
  if not Result then exit;
  FCancelled := false;
  FExecuting := true;
  FDiffAlgorithm := aDiffAlgorithm;
  try
    Result := FAlgos[FDiffAlgorithm].Execute(alist1,alist2);
  finally
    FExecuting := false;
  end;
end;
//------------------------------------------------------------------------------

function TDiff.GetCompareCount: integer;
begin
  Result := FAlgos[FDiffAlgorithm].CompareList.Count;
end;
//------------------------------------------------------------------------------

function TDiff.GetCompare(index: integer): TCompareRec;
begin
  Result := PCompareRec(FAlgos[FDiffAlgorithm].CompareList[index])^;
end;
//------------------------------------------------------------------------------

procedure TDiff.Cancel;
begin
  FCancelled := True;
  FAlgos[FDiffAlgorithm].Cancel;
end;

procedure TDiff.Clear;
begin
  FAlgos[FDiffAlgorithm].Clear;
end;

function TDiff.GetDiffStats: TDiffStats;
begin
  Result := FAlgos[FDiffAlgorithm].DiffStats;
end;

//------------------------------------------------------------------------------

end.
