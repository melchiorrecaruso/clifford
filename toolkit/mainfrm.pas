unit mainfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, SynHighlighterPas, SynEdit, IntegerList;

type
  TCLClass = record
    ClassName: string;
    ClassComponents: TIntegerList;
  end;

  TCLClassList = array of TCLClass;

  TCLProductType = (ptMul, ptGeometric, ptWedge, ptDot);

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
    ClassList: TCLClassList;
    SpaceDimension: longint;

    SectionA0: TStringList;
    SectionB0: TStringList;

    function GetIndex(const ABase: string): longint;
    function GetComp(const ABase: string): string;
    function GetComp(AIndex: longint): string;
    function GetCompSet(AIndex: longint): string;

    function GetDotProduct(const A, B: string): string;
    function GetWedgeProduct(const A, B: string): string;
    function GetGeometricProduct(const A, B: string): string;

    procedure AddOperatorEqual(ALeftIndex, ARightIndex: longint);
    procedure AddOperatorNotEqual(ALeftIndex, ARightIndex: longint);
    procedure AddOperatorAdd(ALeftIndex, ARightIndex: longint);
    procedure AddOperatorSubtract(ALeftIndex, ARightIndex: longint);
    procedure AddOperatorMul(ALeftIndex, ARightIndex: longint; AProductType: TCLProductType);
    procedure AddOperatorDiv(ALeftIndex, ARightIndex: longint);
    procedure AddPrivateSection(AIndex: longint);

    procedure AddClass(AIndex: longint);

    procedure AddHelperDual(AIndex: longint);
    procedure AddHelperInverse(AIndex: longint);
    procedure AddHelperReverse(AIndex: longint);
    procedure AddHelperConjugate(AIndex: longint);
    procedure AddHelperReciprocal(AIndex: longint);
    procedure AddHelperLeftReciprocal(AIndex: longint);
    procedure AddHelperNormalized(AIndex: longint);
    procedure AddHelperNorm(AIndex: longint);
    procedure AddHelperSquareNorm(AIndex: longint);
    procedure AddHelperProjection(AIndex: longint);
    procedure AddHelperRejection(AIndex: longint);
    procedure AddHelperRotation(AIndex: longint);
    procedure AddHelperSameValue(AIndex: longint);
    procedure AddHelperExtract(AIndex: longint);
    procedure AddHelperToMultivector(AIndex: longint);

    procedure AddClassHelper(AIndex: longint);
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

function TMainform.GetIndex(const ABase: string): longint;
var
  i: longint;
  Base: string;
begin
  Base := ABase;
  if Pos('-', Base) = 1 then
  begin
    Delete(Base, 1, 1);
  end;

  for i := 0 to DotProduct.ColCount -1 do
    if DotProduct.Cells[i, 0] = Base then
    begin
      Exit(i);
    end;
  result := -1;
end;

function TMainform.GetComp(const ABase: string): string;
begin
  if ABase <> '1' then
    result := StringReplace(ABase, 'e', '', [rfReplaceAll, rfIgnoreCase])
  else
    result := 'fm0';
end;

function TMainform.GetComp(AIndex: longint): string;
begin
  if AIndex > 0 then
    result := StringReplace(DotProduct.Cells[AIndex, 0], 'e', 'fm', [rfReplaceAll, rfIgnoreCase])
  else
    result := 'fm0';
end;

function TMainform.GetCompSet(AIndex: longint): string;
begin
  if AIndex > 0 then
    result := StringReplace(DotProduct.Cells[AIndex, 0], 'e', 'mc', [rfReplaceAll, rfIgnoreCase])
  else
    result := 'mc0';
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
  K, i, j: Integer;
  Basis: TBasis = nil;
begin
  SpaceDimension := PositiveBox.ItemIndex + NegativeBox.ItemIndex;
  if SpaceDimension <= MaxDimensions then
  begin
    DotProduct      .ColCount := 1 shl SpaceDimension;
    DotProduct      .RowCount := 1 shl SpaceDimension;
    WedgeProduct    .ColCount := 1 shl SpaceDimension;
    WedgeProduct    .RowCount := 1 shl SpaceDimension;
    GeometricProduct.ColCount := 1 shl SpaceDimension;
    GeometricProduct.RowCount := 1 shl SpaceDimension;

    j := 1;
    for K := 1 to SpaceDimension do
    begin
      SetLength(Basis, 0);
      GenerateCombinations(SpaceDimension, K, '', 1, Basis);

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

    Style.Alignment := taCenter;
    style.Layout    := tlCenter;
    DotProduct      .DefaultTextStyle := Style;
    WedgeProduct    .DefaultTextStyle := Style;
    GeometricProduct.DefaultTextStyle := Style;

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
  Line: string;
  List: TIntegerList;
  LeftComponent: string;
  RightComponent: string;
  BaseIndex: longint;
begin
  if ALeftIndex <> ARightIndex then
  begin
    if (ClassList[ALeftIndex ].ClassName <> 'TMultivector') and
       (ClassList[ARightIndex].ClassName <> 'TMultivector') then Exit;
  end;

  BaseIndex := Min(ALeftIndex, ARightIndex);
  if BaseIndex = 0 then
  begin
    BaseIndex := Max(ALeftIndex, ARightIndex);
  end;

  if ALeftIndex <> ARightIndex then
  begin
    SectionA0.Add(Format('    class operator = (const ALeft: %s; ARight: %s): boolean;', [
      ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName]));
    SectionB0.Add(Format('class operator %s.=(const ALeft: %s; ARight: %s): boolean;', [
      ClassList[BaseIndex].ClassName, ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName]))
  end else
  begin
    SectionA0.Add(Format('    class operator = (const ALeft, ARight: %s): boolean;', [
      ClassList[ALeftIndex].ClassName]));
    SectionB0.Add(Format('class operator %s.=(const ALeft, ARight: %s): boolean;', [
      ClassList[BaseIndex].ClassName, ClassList[ALeftIndex ].ClassName]));
  end;

  SectionB0.Add('begin');
  SectionB0.Add('  result := ');
  List := ClassList[High(ClassList)].ClassComponents;
  for i := 0 to List.Count -1 do
  begin
    if i <> List.Count -1 then
      Line := '    (%s = %s) and'
    else
      Line := '    (%s = %s);';

    LeftComponent := '0';
    if ClassList[ALeftIndex].ClassComponents.IndexOf(List[i]) <> -1 then
    begin
      LeftComponent := 'ALeft.' + GetComp(List[i]);
      if ClassList[ALeftIndex].ClassName = 'double' then
      begin
        LeftComponent := 'ALeft';
      end;
    end;

    RightComponent := '0';
    if ClassList[ARightIndex].ClassComponents.IndexOf(List[i]) <> -1 then
    begin
      RightComponent := 'ARight.' + GetComp(List[i]);
      if ClassList[ARightIndex].ClassName = 'double' then
      begin
        RightComponent := 'ARight'
      end;
    end;

    if (LeftComponent <> '0') or (RightComponent <> '0') then
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
  Line: string;
  List: TIntegerList;
  LeftComponent: string;
  RightComponent: string;
  BaseIndex: longint;
begin
  if ALeftIndex <> ARightIndex then
  begin
    if (ClassList[ALeftIndex ].ClassName <> 'TMultivector') and
       (ClassList[ARightIndex].ClassName <> 'TMultivector') then Exit;
  end;

  BaseIndex := Min(ALeftIndex, ARightIndex);
  if BaseIndex = 0 then
  begin
    BaseIndex := Max(ALeftIndex, ARightIndex);
  end;

  if ALeftIndex <> ARightIndex then
   begin
     SectionA0.Add(Format('    class operator <>(const ALeft: %s; ARight: %s): boolean;', [
       ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName]));
     SectionB0.Add(Format('class operator %s.<>(const ALeft: %s; ARight: %s): boolean;', [
       ClassList[BaseIndex].ClassName, ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName]))
   end else
   begin
     SectionA0.Add(Format('    class operator <>(const ALeft, ARight: %s): boolean;', [
       ClassList[ALeftIndex].ClassName]));
     SectionB0.Add(Format('class operator %s.<>(const ALeft, ARight: %s): boolean;', [
       ClassList[BaseIndex].ClassName, ClassList[ALeftIndex ].ClassName]));
   end;

  SectionB0.Add('begin');
  SectionB0.Add('  result := ');
  List := ClassList[High(ClassList)].ClassComponents;
  for i := 0 to List.Count -1 do
  begin
    if i <> List.Count -1 then
      Line := '    (%s <> %s) and'
    else
      Line := '    (%s <> %s);';

    LeftComponent := '0';
    if ClassList[ALeftIndex].ClassComponents.IndexOf(List[i]) <> -1 then
    begin
      LeftComponent := 'ALeft.' + GetComp(List[i]);
      if ClassList[ALeftIndex].ClassName = 'double' then
      begin
        LeftComponent := 'ALeft';
      end;
    end;

    RightComponent := '0';
    if ClassList[ARightIndex].ClassComponents.IndexOf(List[i]) <> -1 then
    begin
      RightComponent := 'ARight.' + GetComp(List[i]);
      if ClassList[ArightIndex].ClassName = 'double' then
      begin
        RightComponent := 'ARight'
      end;
    end;

    if (LeftComponent <> '0') or (RightComponent <> '0') then
    begin
      SectionB0.Add(Format(Line, [LeftComponent, RightComponent]));
    end;
  end;
  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddOperatorAdd(ALeftIndex, ARightIndex: longint);
var
  i: longint;
  Line: string;
  List: TIntegerList;
  LeftComponent: string;
  RightComponent: string;
  BaseIndex: longint;
begin
  BaseIndex := Min(ALeftIndex, ARightIndex);
  if BaseIndex = 0 then
  begin
    BaseIndex := Max(ALeftIndex, ARightIndex);
  end;

  if ALeftIndex <> ARightIndex then
  begin
    SectionA0.Add(Format('    class operator + (const ALeft: %s; ARight: %s): TMultivector;', [
      ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName]));
    SectionB0.Add(Format('class operator %s.+(const ALeft: %s; ARight: %s): TMultivector;', [
      ClassList[BaseIndex].ClassName, ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName]));
  end else
  begin
    SectionA0.Add(Format('    class operator + (const ALeft, ARight: %s): %s;', [
      ClassList[ALeftIndex].ClassName, ClassList[ALeftIndex].ClassName]));
    SectionB0.Add(Format('class operator %s.+(const ALeft, ARight: %s): %s;', [
      ClassList[BaseIndex].ClassName, ClassList[ALeftIndex ].ClassName, ClassList[ALeftIndex].ClassName]));
  end;

  SectionB0.Add('begin');
  List := ClassList[High(ClassList)].ClassComponents;
  for i := 0 to List.Count -1 do
  begin
    LeftComponent := '0';
    if ClassList[ALeftIndex].ClassComponents.IndexOf(List[i]) <> -1 then
    begin
      LeftComponent := 'ALeft.' + GetComp(List[i]);
      if ClassList[ALeftIndex].ClassName = 'double' then
      begin
        LeftComponent := 'ALeft';
      end;
    end;

    RightComponent := '0';
    if ClassList[ARightIndex].ClassComponents.IndexOf(List[i]) <> -1 then
    begin
      RightComponent := 'ARight.' + GetComp(List[i]);
      if ClassList[ArightIndex].ClassName = 'double' then
      begin
        RightComponent := 'ARight';
      end;
    end;

    Line := '';
    if ALeftIndex <> ARightIndex then
    begin
      Line := Format('  result.%s := ', [GetComp(List[i])]);
      if LeftComponent <> '0' then
      begin
        Line := Line + LeftComponent;
        if RightComponent <> '0' then
          Line := Line + ' + ' + RightComponent;
      end else
      begin
        Line := Line + RightComponent;
      end;
    end else
    begin
      if (LeftComponent  <> '0') and (RightComponent <> '0') then
      begin
        Line := Format('  result.%s := %s + %s', [GetComp(List[i]), LeftComponent, RightComponent]);
      end;
    end;

    if Line <> '' then SectionB0.Add(Line + ';');
  end;
  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddOperatorSubtract(ALeftIndex, ARightIndex: longint);
var
  i: longint;
  Line: string;
  List: TIntegerList;
  LeftComponent: string;
  RightComponent: string;
  BaseIndex: longint;
begin
  BaseIndex := Min(ALeftIndex, ARightIndex);
  if BaseIndex = 0 then
  begin
    BaseIndex := Max(ALeftIndex, ARightIndex);
  end;

  if ALeftIndex <> ARightIndex then
  begin
    SectionA0.Add(Format('    class operator - (const ALeft: %s; ARight: %s): TMultivector;', [
      ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName]));
    SectionB0.Add(Format('class operator %s.-(const ALeft: %s; ARight: %s): TMultivector;', [
      ClassList[BaseIndex].ClassName, ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName]));
  end else
  begin
    SectionA0.Add(Format('    class operator - (const ALeft, ARight: %s): %s;', [
      ClassList[ALeftIndex].ClassName, ClassList[ALeftIndex].ClassName]));
    SectionB0.Add(Format('class operator %s.-(const ALeft, ARight: %s): %s;', [
      ClassList[BaseIndex].ClassName, ClassList[ALeftIndex ].ClassName, ClassList[ALeftIndex].ClassName]));
  end;

  SectionB0.Add('begin');
  List := ClassList[High(ClassList)].ClassComponents;
  for i := 0 to List.Count -1 do
  begin
    LeftComponent := '0';
    if ClassList[ALeftIndex].ClassComponents.IndexOf(List[i]) <> -1 then
    begin
      LeftComponent := 'ALeft.' + GetComp(List[i]);
      if ClassList[ALeftIndex].ClassName = 'double' then
      begin
        LeftComponent := 'ALeft';
      end;
    end;

    RightComponent := '0';
    if ClassList[ARightIndex].ClassComponents.IndexOf(List[i]) <> -1 then
    begin
      RightComponent := 'ARight.' + GetComp(List[i]);
      if ClassList[ARightIndex].ClassName = 'double' then
      begin
        RightComponent := 'ARight';
      end;
    end;

    Line := '';
    if ALeftIndex <> ARightIndex then
    begin
      Line := Format('  result.%s := ', [GetComp(List[i])]);
      if LeftComponent <> '0' then
      begin
        Line := Line + LeftComponent;
        if RightComponent <> '0' then
          Line := Line + ' - ' + RightComponent;
      end else
      begin
        if RightComponent <> '0' then
          Line := Line + '-' + RightComponent
        else
          Line := Line + '0';
      end;
    end else
    begin
      if (LeftComponent  <> '0') and (RightComponent <> '0') then
      begin
        Line := Format('  result.%s := %s - %s', [GetComp(List[i]), LeftComponent, RightComponent]);
      end;
    end;

    if Line <> '' then SectionB0.Add(Line + ';');
  end;
  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddOperatorMul(ALeftIndex, ARightIndex: longint; AProductType: TCLProductType);
type
  TReturnComponent = (rtScalar, rtVector, rtBivector, rtTrivector, rtQuadrivector, rtPentavector, rtHexavector);
  TReturnComponents = set of TReturnComponent;
var
  i, j, k: longint;
  Base: string;
  BaseIndex: longint;
  Return: array of TStringList = nil;
  ReturnIndex: longint;
  ReturnComponents: TReturnComponents;
  OperatorSymbol: string;
begin
  BaseIndex := Min(ALeftIndex, ARightIndex);
  if BaseIndex = 0 then
  begin
    BaseIndex := Max(ALeftIndex, ARightIndex);
  end;

  SetLength(Return, 1 shl SpaceDimension);
  for i := Low(Return) to High(Return) do
    Return[i] := TStringList.Create;

  for i := 0 to ClassList[ALeftIndex].ClassComponents.Count -1 do
    for j := 0 to ClassList[ARightIndex].ClassComponents.Count -1 do
    begin
      case AProductType of
        ptMul:       Base := GeometricProduct.Cells[ClassList[ARightIndex].ClassComponents[j], ClassList[ALeftIndex ].ClassComponents[i]];
        ptGeometric: Base := GeometricProduct.Cells[ClassList[ARightIndex].ClassComponents[j], ClassList[ALeftIndex ].ClassComponents[i]];
        ptWedge:     Base :=     WedgeProduct.Cells[ClassList[ARightIndex].ClassComponents[j], ClassList[ALeftIndex ].ClassComponents[i]];
        ptDot:       Base :=       DotProduct.Cells[ClassList[ARightIndex].ClassComponents[j], ClassList[ALeftIndex ].ClassComponents[i]];
      end;

      if Base <> '0' then
      begin
        ReturnIndex := GetIndex(Base);
        if Pos('-', Base) = 1 then
        begin
          if ALeftIndex = 0 then
            Return[ReturnIndex].Add(Format('-ALeft * ARight.%s', [GetComp(ClassList[ARightIndex].ClassComponents[j])]))
          else
            if ARightIndex = 0 then
              Return[ReturnIndex].Add(Format('-ALeft.%s * ARight', [GetComp(ClassList[ALeftIndex].ClassComponents[i])]))
            else
              Return[ReturnIndex].Add(Format('-ALeft.%s * ARight.%s', [GetComp(ClassList[ALeftIndex].ClassComponents[i]), GetComp(ClassList[ARightIndex].ClassComponents[j])]));
        end else
        begin
          if ALeftIndex = 0 then
            Return[ReturnIndex].Add(Format('+ALeft * ARight.%s', [GetComp(ClassList[ARightIndex].ClassComponents[j])]))
          else
            if ARightIndex = 0 then
              Return[ReturnIndex].Add(Format('+ALeft.%s * ARight', [GetComp(ClassList[ALeftIndex].ClassComponents[i])]))
            else
              Return[ReturnIndex].Add(Format('+ALeft.%s * ARight.%s', [GetComp(ClassList[ALeftIndex].ClassComponents[i]), GetComp(ClassList[ARightIndex].ClassComponents[j])]));
        end;
      end;
    end;

  ReturnIndex := 0;
  ReturnComponents := [];
  for i := Low(Return) to High(Return) do
  begin
    if Return[i].Count > 0 then
    begin
      case Length(DotProduct.Cells[i, 0]) -1 of
        0: Include(ReturnComponents, rtScalar);
        1: Include(ReturnComponents, rtVector);
        2: Include(ReturnComponents, rtBivector);
        3: Include(ReturnComponents, rtTrivector);
        4: Include(ReturnComponents, rtQuadrivector);
        5: Include(ReturnComponents, rtPentavector);
        6: Include(ReturnComponents, rtHexavector);
      end;
    end;
  end;

  if ReturnComponents = [              ] then ReturnIndex := 0 else
  if ReturnComponents = [rtScalar      ] then ReturnIndex := 0 else
  if ReturnComponents = [rtVector      ] then ReturnIndex := 1 else
  if ReturnComponents = [rtBivector    ] then ReturnIndex := 2 else
  if ReturnComponents = [rtTrivector   ] then ReturnIndex := 3 else
  if ReturnComponents = [rtQuadrivector] then ReturnIndex := 4 else
  if ReturnComponents = [rtPentavector ] then ReturnIndex := 5 else
  if ReturnComponents = [rtHexavector  ] then ReturnIndex := 6 else ReturnIndex := High(ClassList);

  case AProductType of
    ptMul:       OperatorSymbol  := '*';
    ptGeometric: OperatorSymbol  := 'Geometric';
    ptWedge:     OperatorSymbol  := 'Wedge';
    ptDot:       OperatorSymbol  := 'Dot';
  end;

  if AProductType = ptMul then
  begin
    if ALeftIndex = ARightIndex then
    begin
      SectionA0.Add(Format('    class operator * (const ALeft, ARight: %s): %s;', [ClassList[ALeftIndex].ClassName, ClassList[ReturnIndex].ClassName]));
      SectionB0.Add(Format('class operator %s.*(const ALeft, ARight: %s): %s;', [ClassList[BaseIndex].ClassName, ClassList[ALeftIndex].ClassName, ClassList[ReturnIndex].ClassName]));
    end else
    begin
      SectionA0.Add(Format('    class operator * (const ALeft: %s; const ARight: %s): %s;', [ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName, ClassList[ReturnIndex].ClassName]));
      SectionB0.Add(Format('class operator %s.*(const ALeft: %s; const ARight: %s): %s;', [ClassList[BaseIndex].ClassName, ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName, ClassList[ReturnIndex].ClassName]));
    end
  end else
  begin
    if ALeftIndex = ARightIndex then
    begin
      SectionA0.Add(Format('    class function %s(const ALeft, ARight: %s): %s; static;', [OperatorSymbol, ClassList[ALeftIndex].ClassName, ClassList[ReturnIndex].ClassName]));
      SectionB0.Add(Format('class function %s.%s(const ALeft, ARight: %s): %s; static;', [ClassList[BaseIndex].ClassName, OperatorSymbol, ClassList[ALeftIndex].ClassName, ClassList[ReturnIndex].ClassName]));
    end else
    begin
      SectionA0.Add(Format('    class function %s(const ALeft: %s; const ARight: %s): %s; static;', [OperatorSymbol, ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName, ClassList[ReturnIndex].ClassName]));
      SectionB0.Add(Format('class function %s.%s(const ALeft: %s; const ARight: %s): %s; static;', [ClassList[BaseIndex].ClassName, OperatorSymbol, ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName, ClassList[ReturnIndex].ClassName]));
    end
  end;

  k := 0;
  SectionB0.Add('begin');
  for i := Low(Return) to High(Return) do
  begin
    if Return[i].Count > 0 then
    begin

      if ClassList[ReturnIndex].ClassName <> 'double' then
        SectionB0.Add(Format('  result.%s := ', [GetComp(i)]))
      else
        SectionB0.Add('  result := ');

      for j := 0 to Return[i].Count -1 do
      begin
        if j = Return[i].Count -1 then
          SectionB0.Add('    ' + Return[i][j] + ';')
        else
          SectionB0.Add('    ' + Return[i][j]);
        Inc(k);
      end;
      SectionB0.Add('');
    end else
      if ClassList[ReturnIndex].ClassName = 'TMultivector' then
      begin
        SectionB0.Add(Format('  result.%s := 0;', [GetComp(i)]));
        Inc(k);
      end;
  end;

  if k = 0 then
    SectionB0.Add('  result := 0;');
  SectionB0.Add('end;');
  SectionB0.Add('');

  for i := Low(Return) to High(Return) do
    Return[i].Destroy;
  Return := nil;
end;

procedure TMainForm.AddOperatorDiv(ALeftIndex, ARightIndex: longint);
type
  TReturnComponent = (rtScalar, rtVector, rtBivector, rtTrivector, rtQuadrivector, rtPentavector, rtHexavector);
  TReturnComponents = set of TReturnComponent;
var
  i, j: longint;
  Base: string;
  BaseIndex: longint;
  Return: array of TStringList = nil;
  ReturnIndex: longint;
  ReturnComponents: TReturnComponents;
begin
  BaseIndex := Min(ALeftIndex, ARightIndex);
  if BaseIndex = 0 then
  begin
    BaseIndex := Max(ALeftIndex, ARightIndex);
  end;

  SetLength(Return, 1 shl SpaceDimension);
  for i := Low(Return) to High(Return) do
    Return[i] := TStringList.Create;

  for i := 0 to ClassList[ALeftIndex].ClassComponents.Count -1 do
    for j := 0 to ClassList[ARightIndex].ClassComponents.Count -1 do
    begin
      Base := GeometricProduct.Cells[
        ClassList[ARightIndex].ClassComponents[j],
        ClassList[ALeftIndex ].ClassComponents[i]];

      if Base = '0' then
      begin
        // nothing to do
      end else
      if Base = '1' then
      begin
        Return[0].Add(Format('+ALeft.%s * ARight.%s', [GetComp(ClassList[ALeftIndex].ClassComponents[i]), GetComp(ClassList[ARightIndex].ClassComponents[j])]));
      end else
      if Base = '-1' then
      begin
        Return[0].Add(Format('-ALeft.%s * ARight.%s', [GetComp(ClassList[ALeftIndex].ClassComponents[i]), GetComp(ClassList[ARightIndex].ClassComponents[j])]));
      end else
      begin
        ReturnIndex := GetIndex(Base);
        if Pos('-', Base) = 0 then
        begin
          Return[ReturnIndex].Add(Format('+ALeft.%s * ARight.%s', [GetComp(ClassList[ALeftIndex].ClassComponents[i]), GetComp(ClassList[ARightIndex].ClassComponents[j])]));
        end else
        begin
          Return[ReturnIndex].Add(Format('-ALeft.%s * ARight.%s', [GetComp(ClassList[ALeftIndex].ClassComponents[i]), GetComp(ClassList[ARightIndex].ClassComponents[j])]));
        end;
      end;
    end;

  ReturnIndex := -1;
  ReturnComponents := [];
  for i := Low(Return) to High(Return) do
  begin
    case i of
      0: if Return[0].Count > 0 then Include(ReturnComponents, rtScalar);
      1: if Return[1].Count > 0 then Include(ReturnComponents, rtVector);
      2: if Return[2].Count > 0 then Include(ReturnComponents, rtBivector);
      3: if Return[3].Count > 0 then Include(ReturnComponents, rtTrivector);
      4: if Return[4].Count > 0 then Include(ReturnComponents, rtQuadrivector);
      5: if Return[5].Count > 0 then Include(ReturnComponents, rtPentavector);
      6: if Return[6].Count > 0 then Include(ReturnComponents, rtHexavector);
    end;
  end;

  if ReturnComponents = [rtScalar      ] then ReturnIndex := 0 else
  if ReturnComponents = [rtVector      ] then ReturnIndex := 1 else
  if ReturnComponents = [rtBivector    ] then ReturnIndex := 2 else
  if ReturnComponents = [rtTrivector   ] then ReturnIndex := 3 else
  if ReturnComponents = [rtQuadrivector] then ReturnIndex := 4 else
  if ReturnComponents = [rtPentavector ] then ReturnIndex := 5 else
  if ReturnComponents = [rtHexavector  ] then ReturnIndex := 6 else ReturnIndex := High(ClassList);


  if ALeftIndex = ARightIndex then
  begin
    SectionA0.Add(Format('    class operator / (const ALeft, ARight: %s): %s;', [
      ClassList[ALeftIndex].ClassName, ClassList[ReturnIndex].ClassName]));
    SectionB0.Add(Format('class operator %s./(const ALeft, ARight: %s): %s;', [
      ClassList[BaseIndex].ClassName, ClassList[ALeftIndex].ClassName, ClassList[ReturnIndex].ClassName]));
  end else
  begin
    SectionA0.Add(Format('    class operator / (const ALeft: %s; const ARight: %s): %s;', [
      ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName, ClassList[ReturnIndex].ClassName]));
    SectionB0.Add(Format('class operator %s./(const ALeft: %s; const ARight: %s): %s;', [
      ClassList[BaseIndex].ClassName, ClassList[ALeftIndex].ClassName, ClassList[ARightIndex].ClassName, ClassList[ReturnIndex].ClassName]));
  end;

  SectionB0.Add('begin');
  if ARightIndex = 0 then
    SectionB0.Add('  result := ALeft / ARight;')
  else
    SectionB0.Add('  result := ALeft * ARight.Reciprocal;');
  SectionB0.Add('end;');
  SectionB0.Add('');

  for i := Low(Return) to High(Return) do
    Return[i].Destroy;
  Return := nil;
end;

procedure TMainForm.AddPrivateSection(AIndex: longint);
var
  i: longint;
begin
  SectionA0.Add('  private');
  for i := 0 to ClassList[AIndex].ClassComponents.Count -1 do
  begin
    SectionA0.Add(Format('    %s: double;', [GetComp(ClassList[AIndex].ClassComponents[i])]));
  end;
  SectionA0.Add('  public');
end;

procedure TMainForm.AddClass(AIndex: longint);
var
  i: longint;
begin
  SectionA0.Add(Format('  // %s', [ClassList[AIndex].ClassName]));
  SectionA0.Add(Format('  %s = record', [ClassList[AIndex].ClassName]));
  // Adding private section
  AddPrivateSection(AIndex);
  // Adding = operator
  AddOperatorEqual(AIndex, 0);
  AddOperatorEqual(0, AIndex);
  i := AIndex;
  while i < Length(ClassList) do
  begin
    AddOperatorEqual(AIndex, i);
    if AIndex <> i then
      AddOperatorEqual(i, AIndex);
    Inc(i);
  end;
  // Adding <> operator
  AddOperatorNotEqual(AIndex, 0);
  AddOperatorNotEqual(0, AIndex);
  i := AIndex;
  while i < Length(ClassList) do
  begin
    AddOperatorNotEqual(AIndex, i);
    if AIndex <> i then
      AddOperatorNotEqual(i, AIndex);
    Inc(i);
  end;
  // Adding + operator
  AddOperatorAdd(AIndex, 0);
  AddOperatorAdd(0, AIndex);
  i := AIndex;
  while i < Length(ClassList) do
  begin
    AddOperatorAdd(AIndex, i);
    if AIndex <> i then
      AddOperatorAdd(i, AIndex);
    Inc(i);
  end;
  // Adding - operator
  AddOperatorSubtract(AIndex, 0);
  AddOperatorSubtract(0, AIndex);
  i := AIndex;
  while i < Length(ClassList) do
  begin
    AddOperatorSubtract(AIndex, i);
    if AIndex <> i then
      AddOperatorSubtract(i, AIndex);
    Inc(i);
  end;
  // Adding product (geometric)
  AddOperatorMul(AIndex, 0, ptMul);
  AddOperatorMul(0, AIndex, ptMul);
  i := AIndex;
  while i < Length(ClassList) do
  begin
    AddOperatorMul(AIndex, i, ptMul);
    if AIndex <> i then
      AddOperatorMul(i, AIndex, ptMul);
    Inc(i);
  end;
  // Adding / operator
  AddOperatorDiv(AIndex, 0);
  AddOperatorDiv(0, AIndex);
  i := AIndex;
  while i < Length(ClassList) do
  begin
    AddOperatorDiv(AIndex, i);
    if AIndex <> i then
      AddOperatorDiv(i, AIndex);
    Inc(i);
  end;
  // Adding Wedge product
  AddOperatorMul(AIndex, 0, ptWedge);
  AddOperatorMul(0, AIndex, ptWedge);
  i := AIndex;
  while i < Length(ClassList) do
  begin
    AddOperatorMul(AIndex, i, ptWedge);
    if AIndex <> i then
      AddOperatorMul(i, AIndex, ptWedge);
    Inc(i);
  end;
  // Adding Dot product
  AddOperatorMul(AIndex, 0, ptDot);
  AddOperatorMul(0, AIndex, ptDot);
  i := AIndex;
  while i < Length(ClassList) do
  begin
    AddOperatorMul(AIndex, i, ptDot);
    if AIndex <> i then
      AddOperatorMul(i, AIndex, ptDot);
    Inc(i);
  end;

  SectionA0.Add('  end;');
  SectionA0.Add('');
end;

procedure TMainForm.AddHelperDual(AIndex: longint);
type
  TReturnComponent = (rtScalar, rtVector, rtBivector, rtTrivector, rtQuadrivector, rtPentavector, rtHexavector);
  TReturnComponents = set of TReturnComponent;
var
  i, j, k: longint;
  Base: string;
  LeftIndex: longint;
  RightIndex: longint;
  Return: array of TStringList = nil;
  ReturnIndex: longint;
  ReturnComponents: TReturnComponents;
begin
  LeftIndex  := AIndex;
  RightIndex := High(ClassList) -1;

  SetLength(Return, 1 shl SpaceDimension);
  for i := Low(Return) to High(Return) do
    Return[i] := TStringList.Create;

  for i := 0 to ClassList[LeftIndex].ClassComponents.Count -1 do
    for j := 0 to ClassList[RightIndex].ClassComponents.Count -1 do
    begin
      Base := GeometricProduct.Cells[ClassList[RightIndex].ClassComponents[j], ClassList[LeftIndex ].ClassComponents[i]];

      if Base <> '0' then
      begin
        ReturnIndex := GetIndex(Base);
        if Pos('-', Base) = 1 then
        begin
          if LeftIndex = 0 then
            Return[ReturnIndex].Add(Format('-ALeft * ARight.%s', [GetComp(ClassList[RightIndex].ClassComponents[j])]))
          else
            if RightIndex = 0 then
              Return[ReturnIndex].Add(Format('-ALeft.%s * ARight', [GetComp(ClassList[LeftIndex].ClassComponents[i])]))
            else
              Return[ReturnIndex].Add(Format('-ALeft.%s * ARight.%s', [GetComp(ClassList[LeftIndex].ClassComponents[i]), GetComp(ClassList[RightIndex].ClassComponents[j])]));
        end else
        begin
          if LeftIndex = 0 then
            Return[ReturnIndex].Add(Format('+ALeft * ARight.%s', [GetComp(ClassList[RightIndex].ClassComponents[j])]))
          else
            if RightIndex = 0 then
              Return[ReturnIndex].Add(Format('+ALeft.%s * ARight', [GetComp(ClassList[LeftIndex].ClassComponents[i])]))
            else
              Return[ReturnIndex].Add(Format('+ALeft.%s * ARight.%s', [GetComp(ClassList[LeftIndex].ClassComponents[i]), GetComp(ClassList[RightIndex].ClassComponents[j])]));
        end;
      end;
    end;

  ReturnIndex := 0;
  ReturnComponents := [];
  for i := Low(Return) to High(Return) do
  begin
    if Return[i].Count > 0 then
    begin
      case Length(DotProduct.Cells[i, 0]) -1 of
        0: Include(ReturnComponents, rtScalar);
        1: Include(ReturnComponents, rtVector);
        2: Include(ReturnComponents, rtBivector);
        3: Include(ReturnComponents, rtTrivector);
        4: Include(ReturnComponents, rtQuadrivector);
        5: Include(ReturnComponents, rtPentavector);
        6: Include(ReturnComponents, rtHexavector);
      end;
    end;
  end;

  if ReturnComponents = [              ] then ReturnIndex := 0 else
  if ReturnComponents = [rtScalar      ] then ReturnIndex := 0 else
  if ReturnComponents = [rtVector      ] then ReturnIndex := 1 else
  if ReturnComponents = [rtBivector    ] then ReturnIndex := 2 else
  if ReturnComponents = [rtTrivector   ] then ReturnIndex := 3 else
  if ReturnComponents = [rtQuadrivector] then ReturnIndex := 4 else
  if ReturnComponents = [rtPentavector ] then ReturnIndex := 5 else
  if ReturnComponents = [rtHexavector  ] then ReturnIndex := 6 else ReturnIndex := High(ClassList);

  SectionA0.Add(Format('    function Dual: %s;', [ClassList[ReturnIndex].ClassName]));
  SectionB0.Add(Format('function %sHelper.Dual: %s;', [ClassList[LeftIndex].ClassName, ClassList[ReturnIndex].ClassName]));
  SectionB0.Add('begin');

  k := 0;
  SectionB0.Add('begin');
  for i := Low(Return) to High(Return) do
  begin
    if Return[i].Count > 0 then
    begin
      if ClassList[ReturnIndex].ClassName <> 'double' then
        SectionB0.Add(Format('  result.%s := ', [GetComp(i)]))
      else
        SectionB0.Add('  result := ');

      for j := 0 to Return[i].Count -1 do
      begin
        if j = Return[i].Count -1 then
          SectionB0.Add('    ' + Return[i][j] + ';')
        else
          SectionB0.Add('    ' + Return[i][j]);
        Inc(k);
      end;
      SectionB0.Add('');
    end else
      if ClassList[ReturnIndex].ClassName = 'TMultivector' then
      begin
        SectionB0.Add(Format('  result.%s := 0;', [GetComp(i)]));
        Inc(k);
      end;
  end;

  if k = 0 then
    SectionB0.Add('  result := 0;');
  SectionB0.Add('end;');
  SectionB0.Add('');

  for i := Low(Return) to High(Return) do
    Return[i].Destroy;
  Return := nil;
end;

procedure TMainForm.AddHelperInverse(AIndex: longint);
var
  i: longint;
begin
  SectionA0.Add(Format('    function Inverse: %s;', [ClassList[AIndex].ClassName]));
  SectionB0.Add(Format('function %sHelper.Inverse: %s;', [ClassList[AIndex].ClassName, ClassList[AIndex].ClassName]));
  SectionB0.Add('begin');

  if AIndex = High(ClassList) then
  begin
    for AIndex := Low(ClassList) to High(ClassList) -1 do
      for i := 0 to ClassList[AIndex].ClassComponents.Count -1 do
      begin
        if IntPower(-1, AIndex) < 0 then
          SectionB0.Add(Format('  result.%s := -%s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]))
        else
          SectionB0.Add(Format('  result.%s := %s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]));
      end;
  end else
  begin
    for i := 0 to ClassList[AIndex].ClassComponents.Count -1 do
    begin
      if IntPower(-1, AIndex) < 0 then
        SectionB0.Add(Format('  result.%s := -%s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]))
      else
        SectionB0.Add(Format('  result.%s := %s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]));
    end;
  end;

  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddHelperReverse(AIndex: longint);
var
  i: longint;
begin
  SectionA0.Add(Format('    function Reverse: %s;', [ClassList[AIndex].ClassName]));
  SectionB0.Add(Format('function %sHelper.Reverse: %s;', [ClassList[AIndex].ClassName, ClassList[AIndex].ClassName]));
  SectionB0.Add('begin');

  if AIndex = High(ClassList) then
  begin
    for AIndex := Low(ClassList) to High(ClassList) -1 do
      for i := 0 to ClassList[AIndex].ClassComponents.Count -1 do
      begin
        if IntPower(-1, AIndex div 2) < 0 then
          SectionB0.Add(Format('  result.%s := -%s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]))
        else
          SectionB0.Add(Format('  result.%s := %s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]));
      end;
  end else
  begin
    for i := 0 to ClassList[AIndex].ClassComponents.Count -1 do
    begin
      if IntPower(-1, AIndex div 2) < 0 then
        SectionB0.Add(Format('  result.%s := -%s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]))
      else
        SectionB0.Add(Format('  result.%s := %s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]));
    end;
  end;

  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddHelperConjugate(AIndex: longint);
var
  i: longint;
begin
  SectionA0.Add(Format('    function Conjugate: %s;', [ClassList[AIndex].ClassName]));
  SectionB0.Add(Format('function %sHelper.Conjugate: %s;', [ClassList[AIndex].ClassName, ClassList[AIndex].ClassName]));
  SectionB0.Add('begin');

  if AIndex = High(ClassList) then
  begin
    for AIndex := Low(ClassList) to High(ClassList) -1 do
      for i := 0 to ClassList[AIndex].ClassComponents.Count -1 do
      begin
        if IntPower(-1, AIndex + AIndex div 2) < 0 then
          SectionB0.Add(Format('  result.%s := -%s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]))
        else
          SectionB0.Add(Format('  result.%s := %s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]));
      end;
  end else
  begin
    for i := 0 to ClassList[AIndex].ClassComponents.Count -1 do
    begin
      if IntPower(-1, AIndex + AIndex div 2) < 0 then
        SectionB0.Add(Format('  result.%s := -%s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]))
      else
        SectionB0.Add(Format('  result.%s := %s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]));
    end;
  end;

  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddHelperReciprocal(AIndex: longint);
begin
  SectionA0.Add(Format('    function Reciprocal: %s;', [ClassList[AIndex].ClassName]));
  SectionB0.Add(Format('function %sHelper.Reciprocal: %s;', [ClassList[AIndex].ClassName, ClassList[AIndex].ClassName]));
  SectionB0.Add('begin');

  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddHelperLeftReciprocal(AIndex: longint);
begin
  SectionA0.Add(Format('    function LeftReciprocal: %s;', [ClassList[AIndex].ClassName]));
  SectionB0.Add(Format('function %sHelper.LeftReciprocal: %s;', [ClassList[AIndex].ClassName, ClassList[AIndex].ClassName]));
  SectionB0.Add('begin');

  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddHelperNormalized(AIndex: longint);
begin
  SectionA0.Add(Format('    function Normalized: %s;', [ClassList[AIndex].ClassName]));
  SectionB0.Add(Format('function %sHelper.Normalized: %s;', [ClassList[AIndex].ClassName, ClassList[AIndex].ClassName]));
  SectionB0.Add('begin');
  SectionB0.Add('  result := Self / Norm;');
  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddHelperNorm(AIndex: longint);
begin
  SectionA0.Add(Format('    function Norm: double;', []));
  SectionB0.Add(Format('function %sHelper.Norm: double;', [ClassList[AIndex].ClassName]));
  SectionB0.Add('begin');
  SectionB0.Add('  result := sqrt(SquareNorm);');
  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddHelperSquareNorm(AIndex: longint);
var
  i: longint;
begin
  SectionA0.Add(Format('    function SquareNorm: double;', []));
  SectionB0.Add(Format('function %sHelper.SquareNorm: double;', [ClassList[AIndex].ClassName]));
  SectionB0.Add('begin');
  SectionB0.Add('  result := ');

  for i := 0 to ClassList[AIndex].ClassComponents.Count -1 do
  begin
    SectionB0.Add(Format('    +%s * %s', [GetComp(ClassList[AIndex].ClassComponents[i]), GetComp(ClassList[AIndex].ClassComponents[i])]));
  end;
  SectionB0[SectionB0.Count -1] := SectionB0[SectionB0.Count -1] + ';';

  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddHelperProjection(AIndex: longint);
var
  i: longint;
begin
  for i := Low(CLassList) + 1 to High(ClassList) do
  begin
    SectionA0.Add(Format('    function Projection(const AVector: %s): %s;', [ClassList[i].ClassName, ClassList[AIndex].ClassName]));
    SectionB0.Add(Format('function %sHelper.Projection(const AVector: %s): %s;', [ClassList[AIndex].ClassName, ClassList[i].ClassName, ClassList[AIndex].ClassName]));
    SectionB0.Add('begin');
    SectionB0.Add('  result := Dot(AVector) * AVector.Reciprocal;');
    SectionB0.Add('end;');
    SectionB0.Add('');
  end;
end;

procedure TMainForm.AddHelperRejection(AIndex: longint);
var
  i: longint;
begin
  for i := Low(CLassList) + 1 to High(ClassList) do
  begin
    SectionA0.Add(Format('    function Rejection(const AVector: %s): %s;', [ClassList[i].ClassName, ClassList[AIndex].ClassName]));
    SectionB0.Add(Format('function %sHelper.Rejection(const AVector: %s): %s;', [ClassList[AIndex].ClassName, ClassList[i].ClassName, ClassList[AIndex].ClassName]));
    SectionB0.Add('begin');
    SectionB0.Add('  result := Wedge(AVector) * AVector.Reciprocal;');
    SectionB0.Add('end;');
    SectionB0.Add('');
  end;
end;

procedure TMainForm.AddHelperRotation(AIndex: longint);
var
  i: longint;
begin
  for i := Low(CLassList) + 1 to High(ClassList) do
  begin
    SectionA0.Add(Format('    function Rotation(const AVector: %s): %s;', [ClassList[i].ClassName, ClassList[AIndex].ClassName]));
    SectionB0.Add(Format('function %sHelper.Rotation(const AVector: %s): %s;', [ClassList[AIndex].ClassName, ClassList[i].ClassName, ClassList[AIndex].ClassName]));
    SectionB0.Add('begin');
    SectionB0.Add('  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;');
    SectionB0.Add('end;');
    SectionB0.Add('');
  end;
end;

procedure TMainForm.AddHelperSameValue(AIndex: longint);
var
  i: longint;
  Line: string;
  List: TIntegerList;
  LeftComponent: string;
  RightComponent: string;
begin
  SectionA0.Add(Format('    function SameValue(const AVector: %s): boolean;', [ClassList[AIndex].ClassName]));
  SectionB0.Add(Format('function %sHelper.SameValue(const AVector: %s): boolean;', [ClassList[AIndex].ClassName, ClassList[AIndex].ClassName]));

  SectionB0.Add('begin');
  SectionB0.Add('  result := ');
  List := ClassList[AIndex].ClassComponents;
  for i := 0 to List.Count -1 do
  begin
    if i <> List.Count -1 then
      Line := '    SameValue(%s, %s) and'
    else
      Line := '    SameValue(%s, %s);';

    LeftComponent := '0';
    if ClassList[AIndex].ClassComponents.IndexOf(List[i]) <> -1 then
    begin
      LeftComponent := 'Self.' + GetComp(List[i]);
      if ClassList[AIndex].ClassName = 'double' then
      begin
        LeftComponent := 'Self';
      end;
    end;

    RightComponent := '0';
    if ClassList[AIndex].ClassComponents.IndexOf(List[i]) <> -1 then
    begin
      RightComponent := 'AVector.' + GetComp(List[i]);
      if ClassList[AIndex].ClassName = 'double' then
      begin
        RightComponent := 'AVector'
      end;
    end;

    if (LeftComponent <> '0') or (RightComponent <> '0') then
    begin
      SectionB0.Add(Format(Line, [LeftComponent, RightComponent]));
    end;
  end;
  SectionB0.Add('end;');
  SectionB0.Add('');
end;

procedure TMainForm.AddHelperExtract(AIndex: longint);
var
  i, j: longint;
begin
  if AIndex <> High(ClassList) then
  begin
    for i := Low(ClassList) to High(ClassList) -1 do
    begin
      SectionA0.Add(Format('    function Extract: %s;', [ClassList[i].ClassName]));
      SectionB0.Add(Format('function TMultivectorHelper.Extract: %s;', [ClassList[i].ClassName]));
      SectionB0.Add('begin');

      for j := 0 to ClassList[i].ClassComponents.Count -1 do
      begin
        SectionB0.Add(Format('  result.%s := %s;', [GetComp(ClassList[i].ClassComponents[j]), GetComp(ClassList[i].ClassComponents[j])]));
      end;

      SectionB0.Add('end;');
      SectionB0.Add('');
    end;
  end;
end;

procedure TMainForm.AddHelperToMultivector(AIndex: longint);
var
  i: longint;
begin
  if AIndex <> High(ClassList) then
  begin
    SectionA0.Add(Format('    function ToMultivector: TMultivector;', []));
    SectionB0.Add(Format('function %sHelper.ToMultivector: TMultivector;', [ClassList[AIndex].ClassName]));
    SectionB0.Add('begin');

    for i := 0 to ClassList[High(ClassList)].ClassComponents.Count -1 do
    begin
      if ClassList[AIndex].ClassComponents.IndexOf(ClassList[High(ClassList)].ClassComponents[i]) <> -1 then
        SectionB0.Add(Format('  result.%s := %s;', [Getcomp(ClassList[High(ClassList)].ClassComponents[i]), GetComp(ClassList[High(ClassList)].ClassComponents[i])]))
      else
        SectionB0.Add(Format('  result.%s := 0;',  [GetComp(ClassList[High(ClassList)].ClassComponents[i])]));
    end;

    SectionB0.Add('end;');
    SectionB0.Add('');
  end;
end;

procedure TMainForm.AddClassHelper(AIndex: longint);
begin
  SectionA0.Add(Format('  // %s', [ClassList[AIndex].ClassName]));
  SectionA0.Add(Format('  %s = record helper for %s', [ClassList[AIndex].ClassName + 'Helper', ClassList[AIndex].ClassName]));

  AddHelperDual          (AIndex);
  AddHelperInverse       (AIndex);
  AddHelperReverse       (AIndex);
  AddHelperConjugate     (AIndex);
  AddHelperReciprocal    (AIndex);
  AddHelperLeftReciprocal(AIndex);
  AddHelperNormalized    (AIndex);
  AddHelperNorm          (AIndex);
  AddHelperSquareNorm    (AIndex);
  AddHelperProjection    (AIndex);
  AddHelperRejection     (AIndex);
  AddHelperRotation      (AIndex);
  AddHelperSameValue     (AIndex);
  AddHelperExtract       (AIndex);
  AddHelperToMultivector (AIndex);

  SectionA0.Add('  end;');
end;

procedure TMainForm.Build;
var
  Subscript: string;
  i: longint;
begin
  SourceCode.BeginUpdate();
  SourceCode.Lines.Clear;

  ClassList := nil;
  SetLength(ClassList, SpaceDimension + 2);
  for i := Low(ClassList) to High(ClassList) do
  begin
    case i of
      0: ClassList[i].ClassName := 'double';
      1: ClassList[i].ClassName := 'TVector';
      2: ClassList[i].ClassName := 'TBivector';
      3: ClassList[i].ClassName := 'TTrivector';
      4: ClassList[i].ClassName := 'TQuadrivector';
      5: ClassList[i].ClassName := 'TPentavector';
      6: ClassList[i].ClassName := 'THexavector';
    end;
    ClassList[i].ClassComponents := TIntegerList.Create;
  end;
  ClassList[High(ClassList)].ClassName := 'TMultivector';

  for i := 0 to DotProduct.ColCount -1 do
  begin
    if i > 0 then
    begin
      Subscript := StringReplace(DotProduct.Cells[i, 0], 'e', '', [rfReplaceAll, rfIgnoreCase])
    end else
      Subscript := '';

    ClassList[Length(Subscript)].ClassComponents.Add(i);
    ClassList[High  (ClassList)].ClassComponents.Add(i);
  end;

  SectionA0 := TStringList.Create;
  SectionA0.Add(Format('unit CL%d%d;', [PositiveBox.ItemIndex, NegativeBox.ItemIndex]));
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
  SectionB0.Add('uses');
  SectionB0.Add('  Math;');
  SectionB0.Add('');

  if SpaceDimension > 0 then
  begin
    Subscript := GetCompSet(0);
    for i := 1 to DotProduct.ColCount -1 do
      Subscript := Subscript + ', ' + GetCompSet(i);

    SectionA0.Add('type');
    SectionA0.Add('  // TMultivector components');
    SectionA0.Add(Format('  TCLComponent  = (%s);', [Subscript]));
    SectionA0.Add(Format('  TCLComponents = set of TCLComponent;', []));
    SectionA0.Add('');

    for i := High(ClassList) downto Low(ClassList) + 1 do AddClass(i);
    for i := High(ClassList) downto Low(ClassList) + 1 do AddClassHelper(i);
  end;

  SectionB0.Add('');
  SectionB0.Add('end.');

  for i := 0 to SectionA0.Count -1 do SourceCode.Lines.Add(SectionA0[i]);
  for i := 0 to SectionB0.Count -1 do SourceCode.Lines.Add(SectionB0[i]);

  SectionA0.Destroy;
  SectionB0.Destroy;
  for i := Low(ClassList) to high(ClassList) do
    ClassList[i].ClassComponents.Destroy;
  ClassList := nil;
  SourceCode.EndUpdate;
end;

end.

