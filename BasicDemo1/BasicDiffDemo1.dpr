// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
program BasicDiffDemo1;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFnDEF FPC}
  {$ELSE}
  Interfaces,
  {$ENDIF }
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  HashUnit in '..\src\HashUnit.pas',
  Diff in '..\src\Diff.pas',
  DiffTypes in '..\src\DiffTypes.pas',
  Diff_ND in '..\src\Diff_ND.pas',
  Diff_NP in '..\src\Diff_NP.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
