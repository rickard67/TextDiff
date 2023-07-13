unit Unit1;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Generics.Collections,
{$ELSE}
  FGL,
{$ENDIF}
  SysUtils,
  Types,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Math,
  DiffTypes,
  Diff,
  ExtCtrls,
  Grids,
  Menus,
  ComCtrls;

type
  {$IFDEF FPC}
  TIntegerList = TFPGList<Cardinal>;
  {$ENDIF}

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open11: TMenuItem;
    Open21: TMenuItem;
    N1: TMenuItem;
    mnuCompare: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog1: TOpenDialog;
    mnuCancel: TMenuItem;
    Panel1: TPanel;
    lblFile1: TLabel;
    lblFile2: TLabel;
    StatusBar1: TStatusBar;
    ResultGrid: TStringGrid;
    Options1: TMenuItem;
    mnuIgnoreCase: TMenuItem;
    mnuIgnoreWhiteSpace: TMenuItem;
    mnuView: TMenuItem;
    PreviousChanges1: TMenuItem;
    NextChanges1: TMenuItem;
    N3: TMenuItem;
    mnuNP: TMenuItem;
    mnuND: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Open11Click(Sender: TObject);
    procedure Open21Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuCompareClick(Sender: TObject);
    procedure mnuCancelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ResultGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure mnuIgnoreCaseClick(Sender: TObject);
    procedure mnuIgnoreWhiteSpaceClick(Sender: TObject);
    procedure PreviousChanges1Click(Sender: TObject);
    procedure NextChanges1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mnuNDClick(Sender: TObject);
    procedure mnuNPClick(Sender: TObject);
  private
    Diff: TDiff;
    FDiffAlgorithm: TDiffAlgorithm;
    source1, source2: TStringList;
    result1, result2: TStringList;
    {$IFDEF FPC}
    hashlist1, hashlist2: TIntegerList;
    {$ELSE}
    hashlist1, hashlist2: TList<Cardinal>;
    {$ENDIF}
    procedure Clear(aleft,aright: boolean);
    procedure BuildHashList(left,right: boolean);
    procedure OpenFile1(const filename: string);
    procedure OpenFile2(const filename: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses HashUnit;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Diff := TDiff.Create(self);
  source1 := TStringList.Create;
  source2 := TStringList.Create;
  result1 := TStringList.Create;
  result2 := TStringList.Create;
  {$IFDEF FPC}
  hashlist1 := TIntegerList.Create;
  hashlist2 := TIntegerList.Create;
  {$ELSE}
  hashlist1 := TList<Cardinal>.Create;
  hashlist2 := TList<Cardinal>.Create;
  {$ENDIF}

  FDiffAlgorithm := algNP;
  ResultGrid.ColWidths[0] := 40;
  ResultGrid.ColWidths[2] := 40;
  ResultGrid.Canvas.Font := ResultGrid.Font;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormActivate(Sender: TObject);
begin
  if (paramcount > 0) then OpenFile1(paramstr(1));
  if (paramcount > 1) then OpenFile2(paramstr(2));
  mnuCompareClick(nil);
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  source1.Free;
  source2.Free;
  result1.Free;
  result2.Free;
  hashlist1.Free;
  hashlist2.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.Clear(aleft, aright: boolean);
begin
  if aleft then
  begin
    source1.Clear;
    result1.Clear;
    hashlist1.Clear;
    lblFile1.Caption := ' File1: ';
  end;
  if aright then
  begin
    source2.Clear;
    result2.Clear;
    hashlist2.Clear;
    lblFile2.Caption := ' File2: ';
  end;
  ResultGrid.RowCount := 0;
  Diff.Clear;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := '';
  StatusBar1.Panels[3].Text := '';
  mnuCompare.Enabled := false;
  mnuView.Enabled := false;
end;
//------------------------------------------------------------------------------

//Because it's SO MUCH EASIER AND FASTER comparing hashes (integers) than
//comparing whole lines of text, we'll build a list of hashes for each line
//in the source files. Each line is represented by a (virtually) unique
//hash that is based on the contents of that line. Also, since the
//likelihood of 2 different lines generating the same hash is so small,
//we can safely ignore that possibility.

procedure TForm1.BuildHashList(left,right: boolean);
var
  i: integer;
begin
  if left then
  begin
    hashlist1.Clear;
    for i := 0 to source1.Count -1 do
      hashlist1.Add(HashLine(source1[i],
        mnuIgnoreCase.Checked, mnuIgnoreWhiteSpace.checked));
  end;
  if right then
  begin
    hashlist2.Clear;
    for i := 0 to source2.Count -1 do
      hashlist2.Add(HashLine(source2[i],
        mnuIgnoreCase.Checked, mnuIgnoreWhiteSpace.checked));
  end;

  mnuCompare.Enabled := (hashlist1.Count > 0) and (hashlist2.Count > 0);
end;
//------------------------------------------------------------------------------

procedure TForm1.OpenFile1(const filename: string);
var
  i: integer;
begin
  if not fileExists(fileName) then exit;
  Clear(true,false);
  source1.LoadFromFile(fileName);
  lblFile1.Caption := ' File1: ' + ExtractFileName(fileName);

  BuildHashList(true,false);

  ResultGrid.RowCount := max(source1.Count, source2.Count);
  for i := 0 to 3 do ResultGrid.Cols[i].BeginUpdate;
  try
    for i := 0 to source1.Count -1 do
    begin
      ResultGrid.Cells[0,i] := inttostr(i+1);
      ResultGrid.Cells[1,i] := source1[i];
    end;
    for i := 0 to source2.Count -1 do
    begin
      ResultGrid.Cells[2,i] := inttostr(i+1);
      ResultGrid.Cells[3,i] := source2[i];
    end;
  finally
    for i := 0 to 3 do ResultGrid.Cols[i].EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.OpenFile2(const filename: string);
var
  i: integer;
begin
  if not fileExists(fileName) then exit;
  Clear(false,true);
  source2.LoadFromFile(fileName);
  lblFile2.Caption := ' File2: ' + ExtractFileName(fileName);

  BuildHashList(false,true);

  ResultGrid.RowCount := max(source1.Count, source2.Count);
  for i := 0 to 3 do ResultGrid.Cols[i].BeginUpdate;
  try
    for i := 0 to source1.Count -1 do
    begin
      ResultGrid.Cells[0,i] := inttostr(i+1);
      ResultGrid.Cells[1,i] := source1[i];
    end;
    for i := 0 to source2.Count -1 do
    begin
      ResultGrid.Cells[2,i] := inttostr(i+1);
      ResultGrid.Cells[3,i] := source2[i];
    end;
  finally
    for i := 0 to 3 do ResultGrid.Cols[i].EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.Open11Click(Sender: TObject);
begin
  if OpenDialog1.Execute then OpenFile1(OpenDialog1.FileName);
end;
//------------------------------------------------------------------------------

procedure TForm1.Open21Click(Sender: TObject);
begin
  if OpenDialog1.Execute then OpenFile2(OpenDialog1.FileName);
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuCompareClick(Sender: TObject);
var
  i: integer;
begin
  if (hashlist1.Count = 0) or (hashlist2.Count = 0) then exit;
  mnuCancel.Enabled := true;
  screen.Cursor := crHourGlass;
  try
    //this is where it all happens  ...

    //nb: TList.list is a pointer to the bottom of the list's integer array
    Diff.Execute(hashlist1, hashlist2, FDiffAlgorithm);

    if Diff.Cancelled then exit;

    //now fill ResultGrid with the differences ...
    for i := 0 to 3 do
    begin
      ResultGrid.Cols[i].BeginUpdate;
      ResultGrid.Cols[i].Clear;
    end;
    try
      ResultGrid.RowCount := Diff.Count;
      for i := 0 to Diff.Count -1 do
        with Diff.Compares[i], ResultGrid do
        begin
          if Kind <> ckAdd then
          begin
            Cells[0,i] := inttostr(oldIndex1+1);
            Cells[1,i] := source1[oldIndex1];
          end;
          if Kind <> ckDelete then
          begin
            Cells[2,i] := inttostr(oldIndex2+1);
            Cells[3,i] := source2[oldIndex2];
          end;
        end;
    finally
      for i := 0 to 3 do ResultGrid.Cols[i].EndUpdate;
    end;

    with Diff.DiffStats do
    begin
      StatusBar1.SimplePanel := False;
      StatusBar1.Panels[0].Text := ' Matches: ' + inttostr(matches);
      StatusBar1.Panels[1].Text := ' Modifies: ' + inttostr(modifies);
      StatusBar1.Panels[2].Text := ' Adds: ' + inttostr(adds);
      StatusBar1.Panels[3].Text := ' Deletes: ' + inttostr(deletes);
    end;

  finally
    screen.Cursor := crDefault;
    mnuCancel.Enabled := false;
  end;
  mnuView.Enabled := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuCancelClick(Sender: TObject);
begin
  Diff.Cancel;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormResize(Sender: TObject);
var
  i: integer;
begin
  with ResultGrid do
  begin
    i := (ClientWidth -80) div 2;
    ResultGrid.ColWidths[1] := i;
    ResultGrid.ColWidths[3] := i;
  end;
  lblFile2.Left := Panel1.ClientWidth div 2;
end;
//------------------------------------------------------------------------------

procedure AddCharToStr(var s: string; c: char; kind, lastkind: TChangeKind);
begin
  if (Kind = lastKind) then s := s + c
  else
  case kind of
    ckNone: s := s + '<BC:------>' + c;
    else s := s + '<BC:33FFFF>' + c;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.ResultGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
const
  PaleGreen: TColor = $AAFFAA;
  PaleRed  : TColor = $AAAAFF;
  PaleBlue : TColor = $FFAAAA;
var
  clr,ctxt: Tcolor;
begin
  if (gdSelected in State) and (ACol in [0,2]) then
  begin
     clr := clHighlight;
     ctxt := clHighlightText;
  end
  else if (Diff.Count = 0) then
  begin
    clr := clWhite;
    ctxt := clBlack;
  end
  else
  begin
    clr := clBtnFace;
    ctxt := clBlack;
  end;

  if (ACol in [1,3]) and (ARow < Diff.Count) then
  begin
    case Diff.Compares[ARow].Kind of
      ckNone: clr := clWhite;
      ckModify: clr := PaleGreen;
      ckDelete: clr := PaleRed;
      ckAdd: clr := PaleBlue;
    end;
  end;

  with ResultGrid.Canvas do
  begin
    Brush.Color := clr;
    Font.Color := ctxt;
    FillRect(Rect);
    TextRect(Rect, Rect.Left+3,Rect.Top+2, ResultGrid.Cells[ACol,ARow]);

    if (source1.Count = 0) and (source2.Count = 0) then exit;

    //now just some fancy coloring ...
    if (ACol in [0,2]) then
    begin
      Pen.Color := clWhite;
      MoveTo(Rect.Right-1,0);
      LineTo(Rect.Right-1,Rect.Bottom);
    end else
    begin
      if (ACol = 1) then
      begin
        Pen.Color := $333333;
        MoveTo(Rect.Right-1,0);
        LineTo(Rect.Right-1,Rect.Bottom);
      end;
      Pen.Color := clSilver;
      MoveTo(Rect.Left,0);
      LineTo(Rect.Left,Rect.Bottom);
    end;
    //finally, draw the focusRect ...
    if (gdSelected in State) and (ACol in [1,3]) then
    begin
      rect.Left := 0;
      DrawFocusRect(Rect);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuIgnoreCaseClick(Sender: TObject);
begin
  mnuIgnoreCase.Checked := not mnuIgnoreCase.Checked;
  Clear(false,false);
  BuildHashList(true,true);
  mnuCompareClick(nil);
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuIgnoreWhiteSpaceClick(Sender: TObject);
begin
  mnuIgnoreWhiteSpace.Checked := not mnuIgnoreWhiteSpace.Checked;
  Clear(false,false);
  BuildHashList(true,true);
  mnuCompareClick(nil);
end;
//------------------------------------------------------------------------------

function GridRect(Coord1, Coord2: TGridCoord): TGridRect;
begin
  with Result do
  begin
    Left := Coord2.X;
    if Coord1.X < Coord2.X then Left := Coord1.X;
    Right := Coord1.X;
    if Coord1.X < Coord2.X then Right := Coord2.X;
    Top := Coord2.Y;
    if Coord1.Y < Coord2.Y then Top := Coord1.Y;
    Bottom := Coord1.Y;
    if Coord1.Y < Coord2.Y then Bottom := Coord2.Y;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.PreviousChanges1Click(Sender: TObject);
var
  row: integer;
  Kind: TChangeKind;
begin
  row := ResultGrid.Selection.Top;
  if row = 0 then exit;
  Kind := Diff.Compares[row].Kind;
  while (row > 0) and (Diff.Compares[row].Kind = Kind) do dec(row);
  if Diff.Compares[row].Kind = ckNone then
  begin
    Kind := ckNone;
    while (row > 0) and
      (Diff.Compares[row].Kind = Kind) do dec(row);
  end;
  ResultGrid.Selection := TGridRect(Rect(0, row, 3, row));
  If row < ResultGrid.TopRow then
    ResultGrid.TopRow := Max(0, row - ResultGrid.VisibleRowCount +1);

  ResultGrid.Row := row;
end;
//------------------------------------------------------------------------------

procedure TForm1.NextChanges1Click(Sender: TObject);
var
  row: integer;
  Kind: TChangeKind;
begin
  row := ResultGrid.Selection.Top;
  if row = ResultGrid.RowCount -1 then exit;
  Kind := Diff.Compares[row].Kind;
  while (row < ResultGrid.RowCount -1) and
    (Diff.Compares[row].Kind = Kind) do inc(row);
  if Diff.Compares[row].Kind = ckNone then
  begin                                           
    Kind := ckNone;
    while (row < ResultGrid.RowCount -1) and
      (Diff.Compares[row].Kind = Kind) do inc(row);
  end;
  ResultGrid.Selection := TGridRect(Rect(0, row, 3, row));
  if row > ResultGrid.TopRow + ResultGrid.VisibleRowCount -1 then
    ResultGrid.TopRow := max(0,min(row, ResultGrid.RowCount - ResultGrid.VisibleRowCount));

  ResultGrid.Row := row;
end;

procedure TForm1.mnuNDClick(Sender: TObject);
begin
  mnuND.Checked := not mnuND.Checked;
  FDiffAlgorithm := algND;
  Clear(false,false);
  mnuCompareClick(nil);
end;

procedure TForm1.mnuNPClick(Sender: TObject);
begin
  mnuNP.Checked := not mnuNP.Checked;
  FDiffAlgorithm := algNP;
  Clear(false,false);
  mnuCompareClick(nil);
end;

//------------------------------------------------------------------------------

end.
