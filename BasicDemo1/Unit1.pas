unit Unit1;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  LCLIntf, LCLType,
{$ENDIF}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Diff,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Label1: TLabel;
    PaintBox1: TPaintBox;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    Diff: TDiff;
    FCharHeight: Integer;
    FCharWidth: Integer;
  public
    { Public declarations }
  end;


var
  Form1: TForm1;

implementation

{$R *.dfm}


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Diff := TDiff.Create(self);
  FCharHeight := 0;
  FCharWidth := 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  //do the 'diff' here ...
  Diff.Execute(edit1.text, edit2.text);

  PaintBox1.visible := true;
  PaintBox1.Invalidate;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  i: Integer;
  clBk: TColor;
  ch1,ch2: Char;
  x,y: Integer;
  LCanvas: TCanvas;
begin
  x := 1;
  y := 1;
  LCanvas := PaintBox1.Canvas;
  if FCharHeight = 0 then
    FCharHeight := LCanvas.TextHeight('W');
  if FCharWidth = 0 then
    FCharWidth := LCanvas.TextWidth('W');
  for i := 0 to Diff.count-1 do
  begin
    with Diff.Compares[i] do
    begin
      if Kind = ckAdd then
      begin
        ch1 := #32;
        ch2 := chr2;
        clBk := $FFAAAA;
      end
      else if Kind = ckDelete then
      begin
        ch1 := chr1;
        ch2 := #32;
        clBk := $AAAAFF;
      end
      else if Kind = ckModify then
      begin
        ch1 := chr1;
        ch2 := chr2;
        clBk := $AAFFAA;
      end
      else
      begin
        ch1 := chr1;
        ch2 := chr2;
        clBk := clBtnFace;
      end;

      LCanvas.Brush.Color := clBk;
      LCanvas.TextOut(x,y,ch1);
      LCanvas.TextOut(x,y+FCharHeight+2,ch2);
      Inc(x,FCharWidth);
    end;
  end;

  y := y+3*FCharHeight;
  LCanvas.Brush.Color := clBtnFace;
  LCanvas.TextOut(0,y,'Compare Statistics ...');
  with Diff.DiffStats do
  begin
    y := y+FCharHeight+5;
    LCanvas.Brush.Color := clBtnFace;
    LCanvas.TextOut(0,y,'  Matches : '+inttostr(matches));
    y := y+FCharHeight+2;
    LCanvas.Brush.Color := $AAFFAA;
    LCanvas.TextOut(0,y,'  Modifies: '+inttostr(modifies));
    y := y+FCharHeight+2;
    LCanvas.Brush.Color := $FFAAAA;
    LCanvas.TextOut(0,y,'  Adds    : '+inttostr(adds));
    y := y+FCharHeight+2;
    LCanvas.Brush.Color := $AAAAFF;
    LCanvas.TextOut(0,y,'  Deletes : '+inttostr(deletes));
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  close;
end;

end.
