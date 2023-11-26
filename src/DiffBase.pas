unit DiffBase;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

(*******************************************************************************
* Component         TNDDiff                                                    *
* Version:          5.0                                                        *
* Date:             18 May 2020                                                *
* Compilers:        Delphi 10.x                                                *
* Author:           Angus Johnson - angusj-AT-myrealbox-DOT-com                *
* Copyright:        © 2001-2009 Angus Johnson                                  *
* Updated by:       Rickard Johansson (RJ TextEd)                              *
*                   Andrea Raimondi (DeerBear)                                 *
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

interface

uses
{$IFNDEF FPC}
  Generics.Collections, Windows,
{$ELSE}
  LCLIntf, LCLType, FGL,
{$ENDIF}
  SysUtils,
  Math,
  Forms,
  Classes,
  DiffTypes;

Type
  {$IFNDEF FPC}
  TCompareList = class( TList<Cardinal> )

  end;
  {$ENDIF}

  TDiffBase = class( TComponent )
  private
    FCancelled,
    FExecuting: Boolean;
    FLastCompareRec: TCompareRec;
    FCompareList: TList;
    FDiffStats: TDiffStats;
    {$IFDEF FPC}
    FList1,
    FList2: TIntegerList;
    {$ELSE}
    FList1,
    FList2: TCompareList;
    {$ENDIF}
  protected
    {$IFDEF FPC}
    function InternalExecute(const alist1, alist2: TIntegerList): boolean; overload;virtual;abstract;
    {$ELSE}
    function InternalExecute(const alist1, alist2: TCompareList): boolean; overload;virtual;abstract;
    {$ENDIF}
    function InternalExecute(const s1, s2: string): boolean; overload;virtual;abstract;
    procedure IncMatches;
    procedure DecMatches;
    procedure IncDeletes;
    procedure DecDeletes;
    procedure IncAdds;
    procedure DecAdds;
    procedure IncModifies;
    procedure DecModifies;
    property LastCompareRec: TCompareRec read FLastCompareRec write FLastCompareRec;
    property List1: {$IFDEF FPC} TIntegerList {$ELSE} TCompareList {$ENDIF} read FList1 write FList1;
    property List2: {$IFDEF FPC} TIntegerList {$ELSE} TCompareList {$ENDIF} read FList2 write FList2;
    property Cancelled: Boolean read FCancelled;
    property Executing: Boolean read FExecuting write FExecuting;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
     // Compare strings or list of Cardinals ...
    {$IFDEF FPC}
    function Execute(const alist1, alist2: TIntegerList): boolean; overload;virtual;abstract;
    {$ELSE}
    function Execute(const alist1, alist2: TCompareList): boolean; overload;
    {$ENDIF}
    function Execute(const s1, s2: string): boolean; overload;
    procedure Clear;virtual;
    // Cancel allows interrupting excessively prolonged comparisons
    procedure Cancel;
    property CompareList: TList read FCompareList write FCompareList;
    property DiffStats: TDiffStats read FDiffStats;
  end;

const
  //Maximum realistic deviation from centre diagonal vector ...
  MAX_DIAGONAL = $FFFFFF; //~16 million

implementation

{ TDiffBase }

procedure TDiffBase.Cancel;
begin
  FCancelled := True;
end;

procedure TDiffBase.Clear;
var
  i: Integer;
begin
  FLastCompareRec.Kind := ckNone;
  FLastCompareRec.oldIndex1 := -1;
  FLastCompareRec.oldIndex2 := -1;
  FDiffStats.matches := 0;
  FDiffStats.adds := 0;
  FDiffStats.deletes :=0;
  FDiffStats.modifies :=0;
  for i := 0 to FCompareList.Count-1 do
    dispose(PCompareRec(FCompareList[i]));
  CompareList.clear;
end;

constructor TDiffBase.Create(aOwner: TComponent);
begin
  inherited;
  FCancelled := False;
  FCompareList := TList.Create;
end;

procedure TDiffBase.DecDeletes;
begin
  Dec(FDiffStats.deletes);
end;

procedure TDiffBase.DecMatches;
begin
  Dec(FDiffStats.matches);
end;

procedure TDiffBase.DecModifies;
begin
  Dec(FDiffStats.modifies);
end;

procedure TDiffBase.DecAdds;
begin
  Dec(FDiffStats.adds);
end;

destructor TDiffBase.Destroy;
begin
  FCompareList.Free;
  inherited;
end;

function TDiffBase.Execute(const s1, s2: string): boolean;
begin
  try
    Result := InternalExecute(s1,s2);
  except
    Result := False;
  end;
end;

procedure TDiffBase.IncAdds;
begin
  Inc(FDiffStats.adds);
end;

procedure TDiffBase.IncDeletes;
begin
  Inc(FDiffStats.deletes);
end;

procedure TDiffBase.IncMatches;
begin
  Inc( FDiffStats.matches );
end;

procedure TDiffBase.IncModifies;
begin
  Inc(FDiffStats.modifies);
end;

{$IFDEF FPC}
function TDiffBase.Execute(const alist1, alist2: TIntegerList): boolean;
{$ELSE}
function TDiffBase.Execute(const alist1, alist2: TCompareList): boolean;
{$ENDIF}
begin
  Result := not FExecuting;
  if not Result then
    exit;
  FCancelled := False;
  FExecuting := True;
  try
    Clear;
    //setup the character arrays ...
    FList1 := alist1;
    FList2 := alist2;
    Result := InternalExecute(alist1,alist2);
  finally
    FExecuting := False;
  end;
end;

end.
