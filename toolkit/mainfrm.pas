unit mainfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, SynHighlighterPas, SynEdit;

type

  { TMainForm }

  TMainForm = class(TForm)
    ExportBtn: TBitBtn;
    PageControl: TPageControl;
    NegativeLabel: TLabel;
    PositiveBox: TComboBox;
    NegativeBox: TComboBox;
    PositiveLabel: TLabel;
    SaveDialog: TSaveDialog;
    SynPasSyn: TSynPasSyn;
    WedgeProduct: TStringGrid;
    GeometricProduct: TStringGrid;
    WedgeProductSheet: TTabSheet;
    GeometricProductSheet: TTabSheet;
    SourceCode: TSynEdit;
    DotProduct: TStringGrid;
    DotProductSheet: TTabSheet;
    SourceCodeSheet: TTabSheet;
    procedure BoxChange(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DoPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
  private
    ClassList: TStringList;
    ClassNames: TStringList;
    ClassComponents: TStringList;

    SectionA0: TStringList;
    SectionA1: TStringList;
    SectionA2: TStringList;

    SectionB0: TStringList;
    SectionB1: TStringList;
    SectionB2: TStringList;

    function GetDotProduct(const A, B: string): string;
    function GetWedgeProduct(const A, B: string): string;
    function GetGeometricProduct(const A, B: string): string;

    procedure AddOperatorEqual(ALeftIndex, ARightIndex: longint);
    procedure AddOperatorNotEqual(ALeftIndex, ARightIndex: longint);
    procedure AddOperatorSum(ALeftIndex, ARightIndex: longint);
    procedure AddClass(AIndex: longint);
  public
    procedure Build;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math;
  
type
  TBasis = array of string;

const
  MaxDimensions = 6;

procedure GenerateCombinations(N, K: Integer; Prefix: string; Start: Integer; var Basis: TBasis);
var
  i: Integer;
begin
  if K = 0 then
  begin
    SetLength(Basis, Length(Basis) + 1);
    Basis[High(Basis)] := Prefix;
  end else
    for i := start to N do
    begin
      GenerateCombinations(N, K - 1, Prefix + 'e' + IntToStr(i), i + 1, Basis);
    end;
end;

function CheckInner(const A, B: string): boolean;
var
  i, Count: longint;
  Short, Long: string;
begin
  if Length(A) = Length(B) then
  begin
    result := A = B;
  end else
  begin
    if Length(A) < Length(B) then
    begin
      Short := StringReplace(A, 'e', '', [rfReplaceAll, rfIgnoreCase]);
      Long  := StringReplace(B, 'e', '', [rfReplaceAll, rfIgnoreCase]);
    end else
    begin
      Long  := StringReplace(A, 'e', '', [rfReplaceAll, rfIgnoreCase]);;
      Short := StringReplace(B, 'e', '', [rfReplaceAll, rfIgnoreCase]);;
    end;

    Count := 0;
    for i := Low(Short) to High(Short) do
      if Pos(Short[i], Long) > 0 then
        Inc(Count);

    result := Count = Length(Short);
  end;
end;

function CheckWedge(const A, B: string): boolean;
var
  i: longint;
  Short, Long: string;
begin
  Short := StringReplace(A, 'e', '', [rfReplaceAll, rfIgnoreCase]);
  Long  := StringReplace(B, 'e', '', [rfReplaceAll, rfIgnoreCase]);

  for i := Low(Short) to High(Short) do
    if Pos(Short[i], Long) > 0 then Exit(False);

  result := True;
end;

function TMainform.GetDotProduct(const A, B: string): string;
begin
  if CheckInner(A, B) then
    result := GetGeometricProduct(A, B)
  else
    result := '0';
end;

function TMainform.GetWedgeProduct(const A, B: string): string;
begin
  if CheckWedge(A, B) then
    result := GetGeometricProduct(A, B)
  else
    result := '0';
end;

function TMainform.GetGeometricProduct(const A, B: string): string;
var
  i, k, dk: longint;
  T: string;
begin
  result := StringReplace(A + B, 'e', '', [rfReplaceAll, rfIgnoreCase]);
  // Ordinamento
  k := 0;
  repeat
    dk := 0;
    for i := Low(result) to High(result) -1 do
    begin
      if result[i] > result[i + 1] then
      begin
        T := result[i] + result[i + 1];
        result[i    ] := T[2];
        result[i + 1] := T[1];
        Inc(dk);
      end;
    end;
    Inc(k, dk);
  until dk = 0;
  // Calcola il segno del risultato
  dk := 0;
  for i := 1 to PositiveBox.ItemIndex do
    if Pos(i.ToString + i.ToString, result) > 0 then
    begin
      result := StringReplace(result, i.ToString + i.ToString, '', [rfReplaceAll, rfIgnoreCase]);
      Inc(dk);
    end;

  for i := (PositiveBox.ItemIndex + 1) to (PositiveBox.ItemIndex + NegativeBox.ItemIndex) do
    if Pos(i.ToString + i.ToString, result) > 0 then
    begin
      result := StringReplace(result, i.ToString + i.ToString, '', [rfReplaceAll, rfIgnoreCase]);
      Inc(dk);
      Inc(k);
    end;
  // Definisce il risultato
  if k mod 2 = 0 then
  begin
    if result = '' then
      result := '1'
    else
      result := 'e' + result;
  end else
  begin
    if result = '' then
      result := '-1'
    else
      result := '-e' + result
  end;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  BoxChange(Sender);
end;

procedure TMainForm.DoPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
var
  S: string;
begin
  if TStringGrid(Sender).Cells[aCol, aRow] =  '0' then TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(238, 238, 238) else
  if TStringGrid(Sender).Cells[aCol, aRow] =  '1' then TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(0,   255,   0) else
  if TStringGrid(Sender).Cells[aCol, aRow] = '-1' then TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(255,   0,   0) else
  begin
    S := StringReplace(TStringGrid(Sender).Cells[aCol, aRow], '-', '', [rfReplaceAll, rfIgnoreCase]);
   case Length(S) of
     2: TStringGrid(Sender).Canvas.Brush.Color := RGBToColor( 81, 157, 255);
     3: TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(243, 166, 255);
     4: TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(255, 126,   0);
     5: TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(255, 228,   0);
     6: TStringGrid(Sender).Canvas.Brush.Color := clWhite;
   else TStringGrid(Sender).Canvas.Brush.Color := clWhite;
   end;
  end;
end;

procedure TMainForm.BoxChange(Sender: TObject);
var
  Style: TTextStyle;
  N, K, i, j: Integer;
  Basis: TBasis = nil;
begin
  N := PositiveBox.ItemIndex + NegativeBox.ItemIndex;
  if N <= MaxDimensions then
  begin
    DotProduct      .ColCount := 1 shl N;
    DotProduct      .RowCount := 1 shl N;
    WedgeProduct    .ColCount := 1 shl N;
    WedgeProduct    .RowCount := 1 shl N;
    GeometricProduct.ColCount := 1 shl N;
    GeometricProduct.RowCount := 1 shl N;

    j := 1;
    for K := 1 to N do
    begin
      SetLength(Basis, 0);
      GenerateCombinations(N, K, '', 1, Basis);

      for i := 0 to High(Basis) do
      begin
        DotProduct      .Cells[j, 0] := 'e' + StringReplace(Basis[i], 'e', '', [rfReplaceAll, rfIgnoreCase]);
        DotProduct      .Cells[0, j] := DotProduct.Cells[j, 0];
        WedgeProduct    .Cells[j, 0] := 'e' + StringReplace(Basis[i], 'e', '', [rfReplaceAll, rfIgnoreCase]);
        WedgeProduct    .Cells[0, j] := DotProduct.Cells[j, 0];
        GeometricProduct.Cells[j, 0] := 'e' + StringReplace(Basis[i], 'e', '', [rfReplaceAll, rfIgnoreCase]);
        GeometricProduct.Cells[0, j] := GeometricProduct.Cells[j, 0];
        Inc(j);
       end;
   end;
    Basis := nil;
    DotProduct      .Cells[0, 0] := '1';
    WedgeProduct    .Cells[0, 0] := '1';
    GeometricProduct.Cells[0, 0] := '1';

    for i := 1 to DotProduct.ColCount -1 do
      for j := 1 to DotProduct.RowCount -1 do
      begin
        DotProduct      .Cells[i, j] := GetDotProduct      (DotProduct.Cells[0, j], DotProduct.Cells[i, 0]);
        WedgeProduct    .Cells[i, j] := GetWedgeProduct    (DotProduct.Cells[0, j], DotProduct.Cells[i, 0]);
        GeometricProduct.Cells[i, j] := GetGeometricProduct(DotProduct.Cells[0, j], DotProduct.Cells[i, 0]);
      end;

    Style.Alignment        := taCenter;
    style.Layout           := tlCenter;
    DotProduct.DefaultTextStyle := Style;
    Build;
  end else
    MessageDlg('Warning', Format('Maximum %d dimensions!', [MaxDimensions]), mtWarning,  [mbOK], '');

  PageControl.TabIndex:= 0;
end;

procedure TMainForm.ExportBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    SourceCode.Lines.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TMainForm.AddOperatorEqual(ALeftIndex, ARightIndex: longint);
var
  i: longint;
  Index: longint;
  LeftComponent: string;
  RightComponent: string;
  Line: string;
begin
  if ALeftIndex <> ARightIndex then
  begin
    if (ClassNames[ALeftIndex ] <> 'TMultivector') and
       (ClassNames[ARightIndex] <> 'TMultivector') then Exit;
  end;

  Index := Min(ALeftIndex, ARightIndex);
  if Index = 0 then
  begin
    Index := Max(ALeftIndex, ARightIndex);
  end;

  if ALeftIndex <> ARightIndex then
  begin
    SectionA0.Add(Format('    class operator = (const ALeft: %s; ARight: %s): boolean;', [ClassNames[ALeftIndex], ClassNames[ARightIndex]]));
    SectionB0.Add(Format('class operator %s.=(const ALeft: %s; ARight: %s): boolean;', [ClassNames[Index], ClassNames[ALeftIndex], ClassNames[ARightIndex]]))
  end else
  begin
    SectionA0.Add(Format('    class operator = (const ALeft, ARight: %s): boolean;', [ClassNames[ALeftIndex]]));
    SectionB0.Add(Format('class operator %s.=(const ALeft, ARight: %s): boolean;', [ClassNames[Index], ClassNames[ALeftIndex]]));
  end;

  SectionB0.Add('begin');
  SectionB0.Add('  result :=');
  for i := 0 to ClassComponents.Count -1 do
  begin
    if i <> ClassComponents.Count -1 then
      Line := '    SameValue(%-14s, %-15s) and'
    else
      Line := '    SameValue(%-14s, %-15s);';

    if ClassNames[ALeftIndex] = 'double' then
    begin
      if i = 0 then
        LeftComponent := 'ALeft'
      else
        LeftComponent := '0'
    end else
      if ClassNames[ALeftIndex] = 'TMultivector' then
      begin
        LeftComponent := 'ALeft.' + ClassComponents[i];
      end else
      begin
        if Length(StringReplace(ClassComponents[i], 'fm', '', [rfReplaceAll, rfIgnoreCase])) = ALeftIndex then
          LeftComponent := 'ALeft.' + ClassComponents[i]
        else
          LeftComponent := '0';
      end;

    if ClassNames[ARightIndex] = 'double' then
    begin
      if i = 0 then
        RightComponent := 'ARight'
      else
        RightComponent := '0'
    end else
      if ClassNames[ARightIndex] = 'TMultivector' then
      begin
        RightComponent := 'ARight.' + ClassComponents[i];
      end else
      begin
        if Length(StringReplace(ClassComponents[i], 'fm', '', [rfReplaceAll, rfIgnoreCase])) = ARightIndex then
          RightComponent := 'ARight.' + ClassComponents[i]
        else
          RightComponent := '0';
      end;

    if (LeftComponent  <> '0') or
       (RightComponent <> '0') then
    begin
      SectionB0.Add(Format(Line, [LeftComponent, RightComponent]));
    end;
  end;
  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddOperatorNotEqual(ALeftIndex, ARightIndex: longint);
var
  i: longint;
  Index: longint;
  LeftComponent: string;
  RightComponent: string;
  Line: string;
begin
  if ALeftIndex <> ARightIndex then
  begin
    if (ClassNames[ALeftIndex ] <> 'TMultivector') and
       (ClassNames[ARightIndex] <> 'TMultivector') then Exit;
  end;

  Index := Min(ALeftIndex, ARightIndex);
  if Index = 0 then
  begin
    Index := Max(ALeftIndex, ARightIndex);
  end;

  if ALeftIndex <> ARightIndex then
   begin
     SectionA0.Add(Format('    class operator <>(const ALeft: %s; ARight: %s): boolean;', [ClassNames[ALeftIndex], ClassNames[ARightIndex]]));
     SectionB0.Add(Format('class operator %s.<>(const ALeft: %s; ARight: %s): boolean;', [ClassNames[Index], ClassNames[ALeftIndex], ClassNames[ARightIndex]]))
   end else
   begin
     SectionA0.Add(Format('    class operator <>(const ALeft, ARight: %s): boolean;', [ClassNames[ALeftIndex]]));
     SectionB0.Add(Format('class operator %s.<>(const ALeft, ARight: %s): boolean;', [ClassNames[Index], ClassNames[ALeftIndex]]));
   end;

  SectionB0.Add('begin');
  SectionB0.Add('  result :=');
  for i := 0 to ClassComponents.Count -1 do
  begin
    if i <> ClassComponents.Count -1 then
      Line := '    (not SameValue(%-14s, %-15s)) or'
    else
      Line := '    (not SameValue(%-14s, %-15s));';

    if ClassNames[ALeftIndex] = 'double' then
    begin
      if i = 0 then
        LeftComponent := 'ALeft'
      else
        LeftComponent := '0'
    end else
      if ClassNames[ALeftIndex] = 'TMultivector' then
      begin
        LeftComponent := 'ALeft.' + ClassComponents[i];
      end else
      begin
        if Length(StringReplace(ClassComponents[i], 'fm', '', [rfReplaceAll, rfIgnoreCase])) = ALeftIndex then
          LeftComponent := 'ALeft.' + ClassComponents[i]
        else
          LeftComponent := '0';
      end;

    if ClassNames[ARightIndex] = 'double' then
    begin
      if i = 0 then
        RightComponent := 'ARight'
      else
        RightComponent := '0'
    end else
      if ClassNames[ARightIndex] = 'TMultivector' then
      begin
        RightComponent := 'ARight.' + ClassComponents[i];
      end else
      begin
        if Length(StringReplace(ClassComponents[i], 'fm', '', [rfReplaceAll, rfIgnoreCase])) = ARightIndex then
          RightComponent := 'ARight.' + ClassComponents[i]
        else
          RightComponent := '0';
      end;

    if (LeftComponent  <> '0') or
       (RightComponent <> '0') then
    begin
      SectionB0.Add(Format(Line, [LeftComponent, RightComponent]));
    end;
  end;
  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddOperatorSum(ALeftIndex, ARightIndex: longint);
begin

end;

procedure TMainForm.AddClass(AIndex: longint);
var
  BaseComponents: string;
  i: longint;
begin
  if ClassList[AIndex] = 'TMultivector' then
  begin
    BaseComponents := '';
    for i := 0 to ClassComponents.Count -1 do
      BaseComponents := BaseComponents + ', ' + ClassComponents[i];
  end else
  begin
    BaseComponents := '';
    for i := 1 to ClassComponents.Count -1 do
      case Length(StringReplace(ClassComponents[i], 'fm', '', [rfReplaceAll, rfIgnoreCase])) of
        1: if ClassList[AIndex] = 'TVector'       then BaseComponents := BaseComponents + ', ' + ClassComponents[i];
        2: if ClassList[AIndex] = 'TBivector'     then BaseComponents := BaseComponents + ', ' + ClassComponents[i];
        3: if ClassList[AIndex] = 'TTrivector'    then BaseComponents := BaseComponents + ', ' + ClassComponents[i];
        4: if ClassList[AIndex] = 'TQuadrivector' then BaseComponents := BaseComponents + ', ' + ClassComponents[i];
        5: if ClassList[AIndex] = 'TPentavector'  then BaseComponents := BaseComponents + ', ' + ClassComponents[i];
        6: if ClassList[AIndex] = 'THexavector'   then BaseComponents := BaseComponents + ', ' + ClassComponents[i];
      end;
  end;
  if Pos(',', BaseComponents) = 1 then Delete(BaseComponents, 1, 1);
  if Pos(' ', BaseComponents) = 1 then Delete(BaseComponents, 1, 1);

  SectionA0.Add(Format('// %s', [ClassList[AIndex]]));
  SectionA0.Add('type');
  SectionA0.Add(Format('  %sComponent  = (%s);', [ClassList[AIndex], BaseComponents]));
  SectionA0.Add(Format('  %sComponents = set of %s', [ClassList[AIndex], ClassList[AIndex]]));
  SectionA0.Add('');
  SectionA0.Add(Format('  %s = record', [ClassList[AIndex]]));
  // Adding equal operator
  AddOperatorEqual(AIndex, 0);
  AddOperatorEqual(0, AIndex);
  i := AIndex;
  while i < ClassList.Count do
  begin
    AddOperatorEqual(AIndex, i);
    if AIndex <> i then
      AddOperatorEqual(i, AIndex);
    Inc(i);
  end;
  // Adding not equal operator
  AddOperatorNotEqual(AIndex, 0);
  AddOperatorNotEqual(0, AIndex);
  i := AIndex;
  while i < ClassList.Count do
  begin
    AddOperatorNotEqual(AIndex, i);
    if AIndex <> i then
      AddOperatorNotEqual(i, AIndex);
    Inc(i);
  end;
  // Adding sum operator
  AddOperatorSum(AIndex, 0);
  AddOperatorSum(0, AIndex);
  i := AIndex;
  while i < ClassList.Count do
  begin
    AddOperatorSum(AIndex, i);
    if AIndex <> i then
      AddOperatorSum(i, AIndex);
    Inc(i);
  end;




  SectionA0.Add('  end;');
  SectionA0.Add('');
end;

procedure TMainForm.Build;
var
  N: longint;
  Base: string;
  i: longint;
begin
  SourceCode.BeginUpdate();
  SourceCode.Lines.Clear;

  ClassComponents := TStringList.Create;
  ClassComponents.Add('fm0');
  for i := 1 to DotProduct.ColCount -1 do
  begin
    ClassComponents.Add(StringReplace(DotProduct.Cells[i, 0], 'e', 'fm', [rfReplaceAll, rfIgnoreCase]));
  end;

  ClassNames := TStringList.Create;
  N := PositiveBox.ItemIndex + NegativeBox.ItemIndex;
  if N >= 0 then ClassNames.Add('double');
  if N >= 1 then ClassNames.Add('TVector');
  if N >= 2 then ClassNames.Add('TBivector');
  if N >= 3 then ClassNames.Add('TTrivector');
  if N >= 4 then ClassNames.Add('TQuadrivector');
  if N >= 5 then ClassNames.Add('TPentavector');
  if N >= 6 then ClassNames.Add('THexavector');
                 ClassNames.Add('TMultivector');

  ClassList := TStringList.Create;
  ClassList.Add('double');
  for i := 1 to DotProduct.ColCount -1 do
  begin
    Base := StringReplace(DotProduct.Cells[i, 0], 'e', '', [rfReplaceAll, rfIgnoreCase]);
    if ClassList.IndexOf(ClassNames[Length(Base)]) = -1 then
    begin
      ClassList.Add(ClassNames[Length(Base)]);
    end;
  end;
  ClassList.Add('TMultivector');

  SectionA0 := TStringList.Create;
  SectionA0.Add(Format('unit Cl%d%d%d', [PositiveBox.ItemIndex, NegativeBox.ItemIndex, 0]));
  SectionA0.Add('');
  SectionA0.Add(Format('{ Geometric Algebra Cl%d%d%d for FreePascal.', [PositiveBox.ItemIndex, NegativeBox.ItemIndex, 0]));
  SectionA0.Add('');
  SectionA0.Add('  Copyright (c) 2024 Melchiorre Caruso');
  SectionA0.Add('  Permission is hereby granted, free of charge, to any person obtaining a copy');
  SectionA0.Add('  of this software and associated documentation files (the "Software"), to');
  SectionA0.Add('  deal in the Software without restriction, including without limitation the');
  SectionA0.Add('  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or');
  SectionA0.Add('  sell copies of the Software, and to permit persons to whom the Software is');
  SectionA0.Add('  furnished to do so, subject to the following conditions:');
  SectionA0.Add('');
  SectionA0.Add('  The above copyright notice and this permission notice shall be included in');
  SectionA0.Add('  all copies or substantial portions of the Software.');
  SectionA0.Add('');
  SectionA0.Add('  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR');
  SectionA0.Add('  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,');
  SectionA0.Add('  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE');
  SectionA0.Add('  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER');
  SectionA0.Add('  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING');
  SectionA0.Add('  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS');
  SectionA0.Add('  IN THE SOFTWARE.');
  SectionA0.Add('}');
  SectionA0.Add('');
  SectionA0.Add('{ References:');
  SectionA0.Add('  - Chris Doran, Anthony Lasenby,');
  SectionA0.Add('    Geometric algebra for Physicists, (2003) Cambridge University Press.');
  SectionA0.Add('  - Eckhard Hitzer, Stephen Sangwine,');
  SectionA0.Add('    Multivector and multivector matrix inverses in real Clifford algebras,');
  SectionA0.Add('    (2016) Preprint: Technical Report CES-534, ISSN: 1744-8050.');
  SectionA0.Add('  - James M. Chappell, Azhar Iqbal, Lachlan J. Gunn, Derek Abbott,');
  SectionA0.Add('    Function of multivector variables, (2015) PLoS One.');
  SectionA0.Add('}');
  SectionA0.Add('');
  SectionA0.Add('{$H+}{$J-}');
  SectionA0.Add('{$mode objfpc}{$h+}');
  SectionA0.Add('{$modeswitch advancedrecords}');
  SectionA0.Add('{$WARN 5024 OFF} // Suppress warning for unused routine parameter.');
  SectionA0.Add('{$WARN 5033 OFF} // Suppress warning for unassigned function''s return value.');
  SectionA0.Add('{$MACRO ON}');
  SectionA0.Add('');
  SectionA0.Add('interface');
  SectionA0.Add('');
  SectionA0.Add('uses');
  SectionA0.Add('  SysUtils;');
  SectionA0.Add('');

  SectionB0 := TStringList.Create;
  SectionB0.Add('implementation');
  SectionB0.Add('');

  for i := ClassList.Count -1 downto 1 do
  begin
    AddClass(i);
  end;

  for i := 0 to SectionA0.Count -1 do SourceCode.Lines.Add(SectionA0[i]);
  for i := 0 to SectionB0.Count -1 do SourceCode.Lines.Add(SectionB0[i]);

  SectionB0.Destroy;
  SectionA0.Destroy;
  ClassList.Destroy;
  ClassNames.Destroy;
  ClassComponents.Destroy;
  SourceCode.EndUpdate;
end;

end.

