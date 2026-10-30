program clgen;

{ Command-line front end of the Clifford toolkit.

  Usage: clgen -p <positive> [-q <negative>] [-b <0|1>] [-n <basis prefix>] [-u <unit>] [-o <file>]
         [-t <test file>] [-i <test iterations>]

  Example, spacetime algebra with gamma notation and indices from 0:
    clgen -p 1 -q 3 -b 0 -n g -u cl13 -t cl13check.lpr }

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, clgencore;

var
  P, Q, IndexBase, Iterations: longint;
  UnitNameOpt, OutFile, TestFile, Prefix: string;
  Generator: TGenerator;
  Lines: TStringList;
  i: longint;
begin
  P := -1; Q := 0; IndexBase := 1; Iterations := 0;
  UnitNameOpt := ''; OutFile := ''; TestFile := ''; Prefix := 'e';
  i := 1;
  while i <= ParamCount do
  begin
    case ParamStr(i) of
      '-p': begin Inc(i); P := StrToInt(ParamStr(i)); end;
      '-q': begin Inc(i); Q := StrToInt(ParamStr(i)); end;
      '-b': begin Inc(i); IndexBase := StrToInt(ParamStr(i)); end;
      '-n': begin Inc(i); Prefix := ParamStr(i); end;
      '-u': begin Inc(i); UnitNameOpt := ParamStr(i); end;
      '-i': begin Inc(i); Iterations := StrToInt(ParamStr(i)); end;
      '-o': begin Inc(i); OutFile := ParamStr(i); end;
      '-t': begin Inc(i); TestFile := ParamStr(i); end;
    else
      begin
        Writeln(StdErr, 'Unknown option: ', ParamStr(i));
        Halt(2);
      end;
    end;
    Inc(i);
  end;
  if P < 0 then
  begin
    Writeln('Usage: clgen -p <positive> [-q <negative>] [-b <0|1>] [-n <basis prefix>] [-u <unit>] [-o <file>] [-t <test file>] [-i <test iterations>]');
    Halt(2);
  end;

  try
    Generator := TGenerator.Create(P, Q, IndexBase);
    Lines := TStringList.Create;
    try
      if UnitNameOpt <> '' then Generator.OutputUnitName := UnitNameOpt;
      Generator.Algebra.BasisPrefix := Prefix;
      if OutFile = '' then OutFile := Generator.OutputUnitName + '.pas';

      Generator.Generate(Lines);
      Lines.SaveToFile(OutFile);
      Writeln(Format('%s: %d lines', [OutFile, Lines.Count]));

      if TestFile <> '' then
      begin
        Generator.GenerateTest(Lines, Iterations);
        Lines.SaveToFile(TestFile);
        Writeln(Format('%s: %d lines', [TestFile, Lines.Count]));
      end;
    finally
      Lines.Free;
      Generator.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln(StdErr, E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
