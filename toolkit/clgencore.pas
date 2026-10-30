unit clgencore;

{ Geometric algebra source generator for Free Pascal.

  Copyright (c) 2024-2026 Melchiorre Caruso
  MIT License, see LICENSE.

  This unit contains everything needed to generate a complete Cl(p,q)
  unit: the algebra model (basis blades and their products), the type
  inference used to choose the return type of every routine, and the Pascal
  code emitter. It has no LCL dependency, so it is shared by the GUI (clgengui)
  and by the command-line generator (clgen).

  Return types are derived in two ways:
  - bilinear products of independent operands (operators, Dot, Wedge, Dual)
    cannot cancel, so the exact support is read from the product table;
  - formulas that use the same operand more than once (Inverse,
    Projection, Rejection, Reflection, Rotation) can cancel. Their support is
    found by evaluating the formula on random operands in modular arithmetic
    (polynomial identity testing), so it is exact with overwhelming
    probability and fully deterministic for a given seed.

  The multivector inverse uses the Faddeev-LeVerrier recurrence in the form
  given by D. S. Shirokov, which is valid for every signature and dimension. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MaxDimension = 6;

type
  EAlgebraError = class(Exception);

  // Basis blade as a bit set: bit i set means basis vector number i.
  TBlade = longword;

  // Set of components, as a bit set over the canonical blade indices.
  TSupport = qword;

  TIntArray = array of longint;

  // Signed product of two basis blades; Sign = 0 means the product vanishes.
  TBladeProduct = function(A, B: TBlade; out Sign: longint): TBlade of object;

  { TAlgebra }

  TAlgebra = class
  private
    FP: longint;
    FQ: longint;
    FIndexBase: longint;
    FBasisPrefix: string;
    FBlades: array of TBlade;
    FIndex: TIntArray;
  public
    constructor Create(AP, AQ: longint; AIndexBase: longint = 1);

    function Dimension: longint;
    function Count: longint;
    function Blade(AIndex: longint): TBlade;
    function IndexOf(ABlade: TBlade): longint;
    function Grade(ABlade: TBlade): longint;
    function Pseudoscalar: TBlade;
    function VectorSquare(ABit: longint): longint;

    function Suffix(ABlade: TBlade): string;
    function Name(ABlade: TBlade): string;

    function Geometric(A, B: TBlade; out Sign: longint): TBlade;
    function Inner(A, B: TBlade; out Sign: longint): TBlade;
    function Outer(A, B: TBlade; out Sign: longint): TBlade;
    function Cross(A, B: TBlade; out Sign: longint): TBlade;

    function InvolutionSign(AGrade: longint): longint;
    function ReverseSign(AGrade: longint): longint;
    function ConjugateSign(AGrade: longint): longint;

    property P: longint read FP;
    property Q: longint read FQ;
    property IndexBase: longint read FIndexBase;
    // Prefix of the basis names: 'e' gives e1, e12...; 'g' gives the gamma
    // notation of spacetime algebra (g0, g01...)
    property BasisPrefix: string read FBasisPrefix write FBasisPrefix;
  end;

  { TGenerator }

  TGenerator = class
  private type
    TTypeInfo = record
      Name: string;
      Grade: longint;           // -1 for the multivector
      Blades: TIntArray;        // canonical indices of the components
      Support: TSupport;
    end;

    TFormula = (fmInverse, fmVersorTest, fmProjection, fmRejection,
      fmReflection, fmRotation);

    TModVector = array of int64;
  private
    FAlgebra: TAlgebra;
    FTypes: array of TTypeInfo;
    FIntf: TStringList;
    FImpl: TStringList;
    FUnitName: string;
    FAuthor: string;
    FYear: longint;
    FSeed: qword;
    FPopPending: boolean;
    // Cached inference results, one entry per type
    FInverseType: TIntArray;
    FFastInverse: array of boolean;

    function MV: longint;
    function Rank(AType: longint): longint;
    function Owner(ALeft, ARight: longint): longint;
    procedure CheckDeclared(AOwner, AType: longint);
    function IsScalarType(AType: longint): boolean;
    function TypeOfSupport(ASupport: TSupport): longint;
    function ProductSupport(ALeft, ARight: longint; AProduct: TBladeProduct): TSupport;
    function ProductType(ALeft, ARight: longint; AProduct: TBladeProduct): longint;
    function SumType(ALeft, ARight: longint): longint;

    // Modular evaluation used for type inference
    function NextRandom: int64;
    function ModRandom(AType: longint): TModVector;
    function ModProduct(const A, B: TModVector; AProduct: TBladeProduct): TModVector;
    function ModReverse(const A: TModVector): TModVector;
    function ModInverseNumerator(const A: TModVector): TModVector;
    function ModSupport(const A: TModVector): TSupport;
    function ProbeSupport(AFormula: TFormula; ASelf, AArg: longint): TSupport;

    // Naming
    function Field(ABladeIndex: longint): string;
    function Access(const AVar: string; AType, ABladeIndex: longint): string;
    function ComponentEnum(AType, ABladeIndex: longint): string;
    function GradeWord(AGrade: longint): string;
    function SetTypeName(AType: longint): string;
    function HasComponentSet(AType: longint): boolean;
    function NullConstName(AType: longint): string;

    // Emission
    procedure Intf(const ALine: string);
    procedure Impl(const ALine: string);
    procedure EmitSum(const ATarget: string; ATerms: TStrings; const ATerminator: string = ';');
    procedure EmitBilinear(const ALeftVar: string; ALeft: longint;
      const ARightVar: string; ARight: longint; AProduct: TBladeProduct; AResult: longint);
    procedure EmitSignMap(AType: longint; ASign: TIntArray);
    function TypedExpression(const AExpr: string; AStatic, ADeclared: longint): string;
    procedure BeginRoutine(const AIntfDecl, AImplDecl: string; AUnusedParams: boolean = False);
    procedure EndRoutine;

    procedure EmitHeader;
    procedure EmitComponentSets;
    procedure EmitRecord(AType: longint);
    procedure EmitConversions(AType: longint);
    procedure EmitComparison(ALeft, ARight: longint; AEqual: boolean);
    procedure EmitAddSub(ALeft, ARight: longint; AAdd: boolean);
    procedure EmitNegation(AType: longint);
    procedure EmitMul(ALeft, ARight: longint);
    procedure EmitDiv(ALeft, ARight: longint);
    procedure EmitHelper(AType: longint);
    procedure EmitHelperInvolutions(AType: longint);
    procedure EmitHelperInverse(AType: longint);
    procedure EmitHelperNorms(AType: longint);
    procedure EmitHelperNormalized(AType: longint);
    procedure EmitHelperProducts(AType: longint);
    procedure EmitHelperGeometry(AType: longint);
    procedure EmitHelperComparison(AType: longint);
    procedure EmitHelperExtraction(AType: longint);
    procedure EmitHelperPredicates(AType: longint);
    procedure EmitHelperStrings(AType: longint);
    procedure EmitBasisConstants;
    procedure EmitSupportRoutines;
  public
    constructor Create(AP, AQ: longint; AIndexBase: longint = 1);
    destructor Destroy; override;

    procedure Generate(ADest: TStrings);
    // AIterations <= 0 picks a count that keeps the test run short for every dimension
    procedure GenerateTest(ADest: TStrings; AIterations: longint = 0);

    function DefaultUnitName: string;
    function TypeName(AType: longint): string;
    function TypeCount: longint;

    property Algebra: TAlgebra read FAlgebra;
    property OutputUnitName: string read FUnitName write FUnitName;
    property Author: string read FAuthor write FAuthor;
    property Year: longint read FYear write FYear;
  end;

implementation

uses
  Math;

const
  Prime = 2147483647; // 2^31 - 1, products of two residues fit in int64
  FirstYear = 2024;   // first year of the copyright interval
  ProbeTrials = 4;

  GradeWords: array[0..MaxDimension] of string = ('Scalar', 'Vector', 'Bivector',
    'Trivector', 'Quadrivector', 'Pentavector', 'Hexavector');
  VersorWords: array[1..MaxDimension] of string = ('Versor', 'Biversor',
    'Triversor', 'Quadriversor', 'Pentaversor', 'Hexaversor');
  EnumPrefixes: array[1..MaxDimension] of string = ('vc', 'bc', 'tc', 'qc', 'pc', 'hc');

function Iif(ACondition: boolean; const ATrue, AFalse: string): string;
begin
  if ACondition then result := ATrue else result := AFalse;
end;

function Ints(const AValues: array of longint): TIntArray;
var
  i: longint;
begin
  result := nil;
  SetLength(result, Length(AValues));
  for i := 0 to High(AValues) do result[i] := AValues[i];
end;

function BitCount(AValue: qword): longint;
begin
  result := 0;
  while AValue <> 0 do
  begin
    AValue := AValue and (AValue - 1);
    Inc(result);
  end;
end;

function ModInverse(AValue: int64): int64;
var
  Base, Exponent: int64;
begin
  // Fermat: a^(p-2) mod p
  result := 1;
  Base := AValue mod Prime;
  Exponent := Prime - 2;
  while Exponent > 0 do
  begin
    if Exponent and 1 = 1 then
      result := result * Base mod Prime;
    Base := Base * Base mod Prime;
    Exponent := Exponent shr 1;
  end;
end;

{ TAlgebra }

constructor TAlgebra.Create(AP, AQ: longint; AIndexBase: longint);

  function Before(A, B: TBlade): boolean;
  begin
    // Canonical order: by grade, then lexicographic on the index lists
    if Grade(A) <> Grade(B) then
      result := Grade(A) < Grade(B)
    else
      result := Suffix(A) < Suffix(B);
  end;

var
  i, j: longint;
  B: TBlade;
begin
  inherited Create;
  if (AP < 0) or (AQ < 0) or (AP + AQ < 1) or (AP + AQ > MaxDimension) then
    raise EAlgebraError.CreateFmt('Invalid signature Cl(%d,%d): dimension must be 1..%d',
      [AP, AQ, MaxDimension]);
  if (AIndexBase < 0) or (AIndexBase > 1) then
    raise EAlgebraError.Create('Index base must be 0 or 1');

  FP := AP;
  FQ := AQ;
  FIndexBase := AIndexBase;
  FBasisPrefix := 'e';

  SetLength(FBlades, Count);
  for i := 0 to Count - 1 do
  begin
    B := i;
    j := i;
    while (j > 0) and Before(B, FBlades[j - 1]) do
    begin
      FBlades[j] := FBlades[j - 1];
      Dec(j);
    end;
    FBlades[j] := B;
  end;

  SetLength(FIndex, Count);
  for i := 0 to Count - 1 do
    FIndex[FBlades[i]] := i;
end;

function TAlgebra.Dimension: longint;
begin
  result := FP + FQ;
end;

function TAlgebra.Count: longint;
begin
  result := 1 shl Dimension;
end;

function TAlgebra.Blade(AIndex: longint): TBlade;
begin
  result := FBlades[AIndex];
end;

function TAlgebra.IndexOf(ABlade: TBlade): longint;
begin
  result := FIndex[ABlade];
end;

function TAlgebra.Grade(ABlade: TBlade): longint;
begin
  result := BitCount(ABlade);
end;

function TAlgebra.Pseudoscalar: TBlade;
begin
  result := Count - 1;
end;

function TAlgebra.VectorSquare(ABit: longint): longint;
begin
  if ABit < FP then
    result := 1
  else
    result := -1;
end;

function TAlgebra.Suffix(ABlade: TBlade): string;
var
  i: longint;
begin
  result := '';
  for i := 0 to Dimension - 1 do
    if (ABlade shr i) and 1 = 1 then
      result := result + IntToStr(i + FIndexBase);

  if result = '' then
  begin
    // With index base 0, '0' already names a basis vector
    if FIndexBase = 0 then
      result := 's'
    else
      result := '0';
  end;
end;

function TAlgebra.Name(ABlade: TBlade): string;
begin
  if ABlade = 0 then
    result := '1'
  else
    result := FBasisPrefix + Suffix(ABlade);
end;

function TAlgebra.Geometric(A, B: TBlade; out Sign: longint): TBlade;
var
  i, Swaps: longint;
begin
  // Transpositions needed to move every vector of B past the larger ones of A
  Swaps := 0;
  for i := 0 to Dimension - 1 do
    if (A shr i) and 1 = 1 then
      Inc(Swaps, BitCount(B and ((TBlade(1) shl i) - 1)));

  if Odd(Swaps) then
    Sign := -1
  else
    Sign := 1;

  // Contract repeated vectors using the metric
  for i := 0 to Dimension - 1 do
    if ((A and B) shr i) and 1 = 1 then
      Sign := Sign * VectorSquare(i);

  result := A xor B;
end;

function TAlgebra.Inner(A, B: TBlade; out Sign: longint): TBlade;
begin
  // Fat dot product: non-zero when one index set contains the other
  if ((A and B) = A) or ((A and B) = B) then
    result := Geometric(A, B, Sign)
  else
  begin
    Sign := 0;
    result := 0;
  end;
end;

function TAlgebra.Outer(A, B: TBlade; out Sign: longint): TBlade;
begin
  if (A and B) = 0 then
    result := Geometric(A, B, Sign)
  else
  begin
    Sign := 0;
    result := 0;
  end;
end;

function TAlgebra.Cross(A, B: TBlade; out Sign: longint): TBlade;
var
  S1, S2, S3: longint;
begin
  // a x b = (a ^ b) I^-1, defined for vectors of a 3-dimensional space
  Sign := 0;
  result := 0;
  if (Dimension <> 3) or (Grade(A) <> 1) or (Grade(B) <> 1) then Exit;

  result := Outer(A, B, S1);
  if S1 = 0 then Exit;
  result := Geometric(result, Pseudoscalar, S2);
  Geometric(Pseudoscalar, Pseudoscalar, S3); // I^-1 = (I*I) I, since I*I = +-1
  Sign := S1 * S2 * S3;
end;

function TAlgebra.InvolutionSign(AGrade: longint): longint;
begin
  if Odd(AGrade) then result := -1 else result := 1;
end;

function TAlgebra.ReverseSign(AGrade: longint): longint;
begin
  if Odd((AGrade * (AGrade - 1)) div 2) then result := -1 else result := 1;
end;

function TAlgebra.ConjugateSign(AGrade: longint): longint;
begin
  if Odd((AGrade * (AGrade + 1)) div 2) then result := -1 else result := 1;
end;

{ TGenerator }

constructor TGenerator.Create(AP, AQ: longint; AIndexBase: longint);
var
  i, k: longint;
  Grade: longint;
begin
  inherited Create;
  FAlgebra := TAlgebra.Create(AP, AQ, AIndexBase);
  FIntf := TStringList.Create;
  FImpl := TStringList.Create;
  FUnitName := DefaultUnitName;
  FAuthor := 'Melchiorre Caruso';
  FYear := StrToInt(FormatDateTime('yyyy', Now));

  // Types: 0 = double (scalar), 1..n = k-vectors, n+1 = multivector
  SetLength(FTypes, FAlgebra.Dimension + 2);
  for i := 0 to High(FTypes) do
  begin
    if i = 0 then
      FTypes[i].Name := 'double'
    else if i = High(FTypes) then
      FTypes[i].Name := 'TMultivector'
    else
      FTypes[i].Name := 'T' + GradeWords[i];

    if i = High(FTypes) then FTypes[i].Grade := -1 else FTypes[i].Grade := i;

    SetLength(FTypes[i].Blades, 0);
    FTypes[i].Support := 0;
    for k := 0 to FAlgebra.Count - 1 do
    begin
      Grade := FAlgebra.Grade(FAlgebra.Blade(k));
      if (FTypes[i].Grade = -1) or (FTypes[i].Grade = Grade) then
      begin
        SetLength(FTypes[i].Blades, Length(FTypes[i].Blades) + 1);
        FTypes[i].Blades[High(FTypes[i].Blades)] := k;
        FTypes[i].Support := FTypes[i].Support or (TSupport(1) shl k);
      end;
    end;
  end;

  // Inverse analysis, done once per type
  FSeed := qword($9E3779B97F4A7C15);
  SetLength(FInverseType, Length(FTypes));
  SetLength(FFastInverse, Length(FTypes));
  for i := 0 to High(FTypes) do
  begin
    FInverseType[i] := TypeOfSupport(ProbeSupport(fmInverse, i, -1));
    FFastInverse[i] := (i <> MV) and (ProbeSupport(fmVersorTest, i, -1) = 1);
  end;
end;

destructor TGenerator.Destroy;
begin
  FIntf.Free;
  FImpl.Free;
  FAlgebra.Free;
  inherited Destroy;
end;

function TGenerator.MV: longint;
begin
  result := High(FTypes);
end;

function TGenerator.Rank(AType: longint): longint;
begin
  // Declaration order of the records: TMultivector first, then k-vectors by
  // increasing grade. With this order every result type of an operator is
  // declared before the record that owns the operator.
  if AType = MV then
    result := 0
  else
    result := AType;
end;

function TGenerator.Owner(ALeft, ARight: longint): longint;
begin
  // A mixed operator lives in the record declared last
  if IsScalarType(ALeft) then
    result := ARight
  else if IsScalarType(ARight) or (Rank(ALeft) > Rank(ARight)) then
    result := ALeft
  else
    result := ARight;
end;

procedure TGenerator.CheckDeclared(AOwner, AType: longint);
begin
  if (not IsScalarType(AType)) and (Rank(AType) > Rank(AOwner)) then
    raise EAlgebraError.CreateFmt('Internal error: %s is used by %s before its declaration',
      [FTypes[AType].Name, FTypes[AOwner].Name]);
end;

function TGenerator.TypeCount: longint;
begin
  result := Length(FTypes);
end;

function TGenerator.TypeName(AType: longint): string;
begin
  result := FTypes[AType].Name;
end;

function TGenerator.IsScalarType(AType: longint): boolean;
begin
  result := AType = 0;
end;

function TGenerator.DefaultUnitName: string;
begin
  if FAlgebra.Q = 0 then
    result := Format('cl%d', [FAlgebra.P])
  else
    result := Format('cl%d%d', [FAlgebra.P, FAlgebra.Q]);
end;

function TGenerator.TypeOfSupport(ASupport: TSupport): longint;
var
  i: longint;
begin
  // An empty support is the null scalar
  if ASupport = 0 then Exit(0);
  for i := 0 to MV - 1 do
    if (ASupport and not FTypes[i].Support) = 0 then Exit(i);
  result := MV;
end;

function TGenerator.ProductSupport(ALeft, ARight: longint; AProduct: TBladeProduct): TSupport;
var
  i, j, Sign: longint;
  B: TBlade;
begin
  result := 0;
  for i in FTypes[ALeft].Blades do
    for j in FTypes[ARight].Blades do
    begin
      B := AProduct(FAlgebra.Blade(i), FAlgebra.Blade(j), Sign);
      if Sign <> 0 then
        result := result or (TSupport(1) shl FAlgebra.IndexOf(B));
    end;
end;

function TGenerator.ProductType(ALeft, ARight: longint; AProduct: TBladeProduct): longint;
begin
  result := TypeOfSupport(ProductSupport(ALeft, ARight, AProduct));
end;

function TGenerator.SumType(ALeft, ARight: longint): longint;
begin
  result := TypeOfSupport(FTypes[ALeft].Support or FTypes[ARight].Support);
end;

{ Modular evaluation }

function TGenerator.NextRandom: int64;
begin
  // xorshift64*, deterministic so that generated sources are reproducible
  FSeed := FSeed xor (FSeed shr 12);
  FSeed := FSeed xor (FSeed shl 25);
  FSeed := FSeed xor (FSeed shr 27);
  result := int64((FSeed * qword(2685821657736338717)) shr 33) mod Prime;
end;

function TGenerator.ModRandom(AType: longint): TModVector;
var
  i: longint;
begin
  result := nil;
  SetLength(result, FAlgebra.Count);
  for i := 0 to High(result) do result[i] := 0;
  for i in FTypes[AType].Blades do
    result[i] := NextRandom;
end;

function TGenerator.ModProduct(const A, B: TModVector; AProduct: TBladeProduct): TModVector;
var
  i, j, Sign, k: longint;
  T: int64;
begin
  result := nil;
  SetLength(result, FAlgebra.Count);
  for i := 0 to High(result) do result[i] := 0;
  for i := 0 to High(A) do
    if A[i] <> 0 then
      for j := 0 to High(B) do
        if B[j] <> 0 then
        begin
          k := FAlgebra.IndexOf(AProduct(FAlgebra.Blade(i), FAlgebra.Blade(j), Sign));
          if Sign = 0 then Continue;
          T := A[i] * B[j] mod Prime;
          if Sign < 0 then T := Prime - T;
          result[k] := (result[k] + T) mod Prime;
        end;
end;

function TGenerator.ModReverse(const A: TModVector): TModVector;
var
  i: longint;
begin
  result := Copy(A);
  for i := 0 to High(result) do
    if (FAlgebra.ReverseSign(FAlgebra.Grade(FAlgebra.Blade(i))) < 0) and (result[i] <> 0) then
      result[i] := Prime - result[i];
end;

function TGenerator.ModInverseNumerator(const A: TModVector): TModVector;
var
  N, k: longint;
  C: int64;
  U, V: TModVector;
begin
  // Shirokov: U1 = A, Uk = A (U(k-1) - C(k-1)), Ck = (N/k) <Uk>_0,
  // A^-1 = (U(N-1) - C(N-1)) / C(N). Here only the numerator is needed.
  N := 1 shl ((FAlgebra.Dimension + 1) div 2);
  U := Copy(A);
  C := N mod Prime * U[0] mod Prime;
  for k := 2 to N - 1 do
  begin
    V := Copy(U);
    V[0] := (V[0] - C + Prime) mod Prime;
    U := ModProduct(A, V, @FAlgebra.Geometric);
    C := N mod Prime * ModInverse(k) mod Prime * U[0] mod Prime;
  end;
  result := Copy(U);
  result[0] := (result[0] - C + Prime) mod Prime;
end;

function TGenerator.ModSupport(const A: TModVector): TSupport;
var
  i: longint;
begin
  result := 0;
  for i := 0 to High(A) do
    if A[i] <> 0 then
      result := result or (TSupport(1) shl i);
end;

function TGenerator.ProbeSupport(AFormula: TFormula; ASelf, AArg: longint): TSupport;
var
  Trial: longint;
  X, Y, Z, R: TModVector;
begin
  result := 0;
  for Trial := 1 to ProbeTrials do
  begin
    X := ModRandom(ASelf);
    case AFormula of
      fmInverse:
        R := ModInverseNumerator(X);
      fmVersorTest:
        R := ModProduct(X, ModReverse(X), @FAlgebra.Geometric);
      fmProjection:
        begin
          Y := ModRandom(AArg);
          R := ModProduct(ModProduct(X, Y, @FAlgebra.Inner), ModInverseNumerator(Y), @FAlgebra.Geometric);
        end;
      fmRejection:
        begin
          Y := ModRandom(AArg);
          R := ModProduct(ModProduct(X, Y, @FAlgebra.Outer), ModInverseNumerator(Y), @FAlgebra.Geometric);
        end;
      fmReflection:
        begin
          Y := ModRandom(AArg);
          R := ModProduct(ModProduct(Y, X, @FAlgebra.Geometric), ModInverseNumerator(Y), @FAlgebra.Geometric);
        end;
      fmRotation:
        begin
          Y := ModRandom(AArg);
          Z := ModRandom(AArg);
          R := ModProduct(Z, Y, @FAlgebra.Geometric);
          R := ModProduct(R, X, @FAlgebra.Geometric);
          R := ModProduct(R, ModInverseNumerator(Y), @FAlgebra.Geometric);
          R := ModProduct(R, ModInverseNumerator(Z), @FAlgebra.Geometric);
        end;
    end;
    result := result or ModSupport(R);
  end;
end;

{ Naming }

function TGenerator.Field(ABladeIndex: longint): string;
begin
  result := 'fm' + FAlgebra.Suffix(FAlgebra.Blade(ABladeIndex));
end;

function TGenerator.Access(const AVar: string; AType, ABladeIndex: longint): string;
begin
  if IsScalarType(AType) then
    result := AVar
  else if AVar = '' then
    result := Field(ABladeIndex)
  else
    result := AVar + '.' + Field(ABladeIndex);
end;

function TGenerator.ComponentEnum(AType, ABladeIndex: longint): string;
begin
  if AType = MV then
    result := 'mc'
  else
    result := EnumPrefixes[FTypes[AType].Grade];
  result := result + FAlgebra.Suffix(FAlgebra.Blade(ABladeIndex));
end;

function TGenerator.GradeWord(AGrade: longint): string;
begin
  result := GradeWords[AGrade];
end;

function TGenerator.SetTypeName(AType: longint): string;
begin
  result := FTypes[AType].Name + 'Components';
end;

function TGenerator.HasComponentSet(AType: longint): boolean;
begin
  result := AType > 0;
end;

function TGenerator.NullConstName(AType: longint): string;
begin
  if AType = 0 then
    result := 'NullScalar'
  else
    result := 'Null' + Copy(FTypes[AType].Name, 2, MaxInt);
end;

{ Emission }

procedure TGenerator.Intf(const ALine: string);
begin
  FIntf.Add(ALine);
end;

procedure TGenerator.Impl(const ALine: string);
begin
  FImpl.Add(ALine);
end;

procedure TGenerator.BeginRoutine(const AIntfDecl, AImplDecl: string; AUnusedParams: boolean);
begin
  Intf('    ' + AIntfDecl);
  // Routines whose result vanishes identically do not read their parameters
  FPopPending := AUnusedParams;
  if FPopPending then
    Impl('{$push}{$warn 5024 off}');
  Impl(AImplDecl);
  Impl('begin');
end;

procedure TGenerator.EndRoutine;
begin
  Impl('end;');
  if FPopPending then
    Impl('{$pop}');
  FPopPending := False;
  Impl('');
end;

procedure TGenerator.EmitSum(const ATarget: string; ATerms: TStrings; const ATerminator: string);
var
  i: longint;
  Prefix, Indent, Term: string;
begin
  // Terms are stored with a leading sign: '+x' or '-x'
  Prefix := '  ' + ATarget + ' := ';
  if ATerms.Count = 0 then
  begin
    Impl(Prefix + '0.0' + ATerminator);
    Exit;
  end;

  Indent := StringOfChar(' ', Length(Prefix) - 2);
  for i := 0 to ATerms.Count - 1 do
  begin
    Term := ATerms[i];
    if i = 0 then
    begin
      if Term[1] = '+' then Delete(Term, 1, 1);
      Term := Prefix + Term;
    end else
      Term := Indent + Term[1] + ' ' + Copy(Term, 2, MaxInt);

    if i = ATerms.Count - 1 then
      Term := Term + ATerminator;
    Impl(Term);
  end;
end;

procedure TGenerator.EmitBilinear(const ALeftVar: string; ALeft: longint;
  const ARightVar: string; ARight: longint; AProduct: TBladeProduct; AResult: longint);
var
  Terms: array of TStringList;
  i, j, k, Sign: longint;
  Term: string;
begin
  Terms := nil;
  // Terms are collected in canonical (left, right) order for every component,
  // so a sparse operand produces exactly the same floating-point sums as the
  // multivector version of the same operation.
  SetLength(Terms, FAlgebra.Count);
  for k := 0 to High(Terms) do Terms[k] := TStringList.Create;
  try
    for i in FTypes[ALeft].Blades do
      for j in FTypes[ARight].Blades do
      begin
        k := FAlgebra.IndexOf(AProduct(FAlgebra.Blade(i), FAlgebra.Blade(j), Sign));
        if Sign = 0 then Continue;
        Term := Access(ALeftVar, ALeft, i) + ' * ' + Access(ARightVar, ARight, j);
        if Sign > 0 then Term := '+' + Term else Term := '-' + Term;
        Terms[k].Add(Term);
      end;

    if IsScalarType(AResult) then
      EmitSum('result', Terms[0])
    else
      for k in FTypes[AResult].Blades do
        EmitSum('result.' + Field(k), Terms[k]);
  finally
    for k := 0 to High(Terms) do Terms[k].Free;
  end;
end;

procedure TGenerator.EmitSignMap(AType: longint; ASign: TIntArray);
var
  k: longint;
begin
  for k in FTypes[AType].Blades do
    if ASign[FAlgebra.Grade(FAlgebra.Blade(k))] > 0 then
      Impl(Format('  result.%s := %s;', [Field(k), Field(k)]))
    else
      Impl(Format('  result.%s := -%s;', [Field(k), Field(k)]));
end;

function TGenerator.TypedExpression(const AExpr: string; AStatic, ADeclared: longint): string;
begin
  // Narrowing is only emitted when inference proved the dropped components zero
  if AStatic = ADeclared then
    result := AExpr
  else if AStatic = MV then
    result := Format('%s(%s)', [FTypes[ADeclared].Name, AExpr])
  else
    raise EAlgebraError.CreateFmt('Cannot convert %s to %s',
      [FTypes[AStatic].Name, FTypes[ADeclared].Name]);
end;

procedure TGenerator.EmitHeader;
var
  Signature: string;
begin
  if FAlgebra.Q = 0 then
    Signature := Format('Cl(%d)', [FAlgebra.P])
  else
    Signature := Format('Cl(%d,%d)', [FAlgebra.P, FAlgebra.Q]);

  Intf(Format('unit %s;', [FUnitName]));
  Intf('');
  Intf(Format('{ Geometric Algebra %s for FreePascal.', [Signature]));
  Intf('');
  Intf(Format('  Copyright (c) %d-%d %s', [FirstYear, FYear, FAuthor]));
  Intf('');
  Intf('  Permission is hereby granted, free of charge, to any person obtaining a copy');
  Intf('  of this software and associated documentation files (the "Software"), to');
  Intf('  deal in the Software without restriction, including without limitation the');
  Intf('  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or');
  Intf('  sell copies of the Software, and to permit persons to whom the Software is');
  Intf('  furnished to do so, subject to the following conditions:');
  Intf('');
  Intf('  The above copyright notice and this permission notice shall be included in');
  Intf('  all copies or substantial portions of the Software.');
  Intf('');
  Intf('  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR');
  Intf('  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,');
  Intf('  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE');
  Intf('  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER');
  Intf('  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING');
  Intf('  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS');
  Intf('  IN THE SOFTWARE.');
  Intf('}');
  Intf('');
  Intf('{ This file is generated by the Clifford toolkit (clgencore.pas).');
  Intf('  Do not edit it by hand: change the generator and regenerate instead.');
  Intf('');
  Intf(Format('  Signature: %d basis vectors square to +1, %d to -1.',
    [FAlgebra.P, FAlgebra.Q]));
  Intf('');
  Intf('  Conventions:');
  Intf('  - Dot is the "fat" dot product: e_A . e_B = e_A e_B when one index set');
  Intf('    contains the other, 0 otherwise (scalars included).');
  Intf('  - Involute is the grade involution, Conjugate the Clifford conjugate.');
  Intf('  - SquaredNorm is <X ~X>_0; Norm is Sqrt(Abs(SquaredNorm)).');
  Intf('  - Dual is X * I, with I the unit pseudoscalar.');
  Intf('  - Inverse is the multiplicative inverse (left and right inverses coincide);');
  Intf('    dividing by a non-invertible value raises a floating-point exception.');
  Intf('  - X.Rotation(A, B) is B A X A^-1 B^-1: the composition of two reflections,');
  Intf('    i.e. a rotation in the plane A^B by TWICE the angle from A to B.');
  Intf('}');
  Intf('');
  Intf('{ References:');
  Intf('  - Chris Doran, Anthony Lasenby,');
  Intf('    Geometric algebra for Physicists, (2003) Cambridge University Press.');
  Intf('  - Eckhard Hitzer, Stephen Sangwine,');
  Intf('    Multivector and multivector matrix inverses in real Clifford algebras,');
  Intf('    (2016) Preprint: Technical Report CES-534, ISSN: 1744-8050.');
  Intf('  - Dmitry Shirokov, On computing the determinant, other characteristic');
  Intf('    polynomial coefficients, and inverse in Clifford algebras of arbitrary');
  Intf('    dimension, (2021) Computational and Applied Mathematics 40.');
  Intf('}');
  Intf('');
  Intf('{$mode objfpc}{$H+}{$J-}');
  Intf('{$modeswitch advancedrecords}');
  Intf('');
  Intf('interface');
  Intf('');
  Intf('uses');
  Intf('  SysUtils;');
  Intf('');
  Intf('type');

  Impl('implementation');
  Impl('');
  Impl('uses');
  Impl('  Math;');
  Impl('');
end;

procedure TGenerator.EmitComponentSets;
var
  t, k: longint;
  Items: string;
begin
  for t := MV downto 1 do
    if HasComponentSet(t) then
    begin
      Items := '';
      for k in FTypes[t].Blades do
      begin
        if Items <> '' then Items := Items + ', ';
        Items := Items + ComponentEnum(t, k);
      end;
      Intf(Format('  // %s components', [FTypes[t].Name]));
      Intf(Format('  %sComponent  = (%s);', [FTypes[t].Name, Items]));
      Intf(Format('  %s = set of %sComponent;', [SetTypeName(t), FTypes[t].Name]));
      Intf('');
    end;
end;

procedure TGenerator.EmitConversions(AType: longint);
var
  k: longint;
begin
  Impl(Format('// %s', [FTypes[AType].Name]));
  Impl('');
  if AType = MV then
  begin
    BeginRoutine('class operator :=(const AValue: double): TMultivector;',
      'class operator TMultivector.:=(const AValue: double): TMultivector;');
    for k in FTypes[MV].Blades do
      if k = 0 then
        Impl(Format('  result.%s := AValue;', [Field(k)]))
      else
        Impl(Format('  result.%s := 0.0;', [Field(k)]));
    EndRoutine;

    BeginRoutine('class operator Explicit(const AValue: TMultivector): double;',
      'class operator TMultivector.Explicit(const AValue: TMultivector): double;');
    Impl(Format('  result := AValue.%s;', [Field(0)]));
    EndRoutine;
  end else
  begin
    BeginRoutine(Format('class operator :=(const AValue: %s): TMultivector;', [FTypes[AType].Name]),
      Format('class operator %s.:=(const AValue: %s): TMultivector;', [FTypes[AType].Name, FTypes[AType].Name]));
    for k in FTypes[MV].Blades do
      if (FTypes[AType].Support shr k) and 1 = 1 then
        Impl(Format('  result.%s := AValue.%s;', [Field(k), Field(k)]))
      else
        Impl(Format('  result.%s := 0.0;', [Field(k)]));
    EndRoutine;

    BeginRoutine(Format('class operator Explicit(const AValue: TMultivector): %s;', [FTypes[AType].Name]),
      Format('class operator %s.Explicit(const AValue: TMultivector): %s;', [FTypes[AType].Name, FTypes[AType].Name]));
    for k in FTypes[AType].Blades do
      Impl(Format('  result.%s := AValue.%s;', [Field(k), Field(k)]));
    EndRoutine;
  end;
end;

function OperatorParams(const ALeft, ARight: string): string;
begin
  if ALeft = ARight then
    result := Format('const ALeft, ARight: %s', [ALeft])
  else
    result := Format('const ALeft: %s; const ARight: %s', [ALeft, ARight]);
end;

procedure TGenerator.EmitComparison(ALeft, ARight: longint; AEqual: boolean);
var
  OwnerName, Params, Op, L, R: string;
  Union: TSupport;
  k: longint;
  Terms: TStringList;
begin
  OwnerName := FTypes[Owner(ALeft, ARight)].Name;
  Params := OperatorParams(FTypes[ALeft].Name, FTypes[ARight].Name);
  if AEqual then Op := '=' else Op := '<>';

  BeginRoutine(Format('class operator %-2s(%s): boolean;', [Op, Params]),
    Format('class operator %s.%s(%s): boolean;', [OwnerName, Op, Params]));

  if not AEqual then
    Impl('  result := not (ALeft = ARight);')
  else
  begin
    Union := FTypes[ALeft].Support or FTypes[ARight].Support;
    Terms := TStringList.Create;
    try
      for k := 0 to FAlgebra.Count - 1 do
        if (Union shr k) and 1 = 1 then
        begin
          if (FTypes[ALeft].Support shr k) and 1 = 1 then L := Access('ALeft', ALeft, k) else L := '0.0';
          if (FTypes[ARight].Support shr k) and 1 = 1 then R := Access('ARight', ARight, k) else R := '0.0';
          Terms.Add(Format('(%s = %s)', [L, R]));
        end;
      for k := 0 to Terms.Count - 2 do Terms[k] := Terms[k] + ' and';
      Impl('  result := ' + Terms[0] + Iif(Terms.Count = 1, ';', ''));
      for k := 1 to Terms.Count - 1 do
        Impl('            ' + Terms[k] + Iif(k = Terms.Count - 1, ';', ''));
    finally
      Terms.Free;
    end;
  end;
  EndRoutine;
end;

procedure TGenerator.EmitAddSub(ALeft, ARight: longint; AAdd: boolean);
var
  OwnerName, Params, Op: string;
  ResultType, k: longint;
  HasL, HasR: boolean;
  Expr: string;
begin
  OwnerName := FTypes[Owner(ALeft, ARight)].Name;
  Params := OperatorParams(FTypes[ALeft].Name, FTypes[ARight].Name);
  if AAdd then Op := '+' else Op := '-';
  ResultType := SumType(ALeft, ARight);
  CheckDeclared(Owner(ALeft, ARight), ResultType);

  BeginRoutine(Format('class operator %-2s(%s): %s;', [Op, Params, FTypes[ResultType].Name]),
    Format('class operator %s.%s(%s): %s;', [OwnerName, Op, Params, FTypes[ResultType].Name]));
  for k in FTypes[ResultType].Blades do
  begin
    HasL := (FTypes[ALeft].Support shr k) and 1 = 1;
    HasR := (FTypes[ARight].Support shr k) and 1 = 1;
    if HasL and HasR then
      Expr := Format('%s %s %s', [Access('ALeft', ALeft, k), Op, Access('ARight', ARight, k)])
    else if HasL then
      Expr := Access('ALeft', ALeft, k)
    else if HasR then
    begin
      if AAdd then
        Expr := Access('ARight', ARight, k)
      else
        Expr := '-' + Access('ARight', ARight, k);
    end else
      Expr := '0.0';
    Impl(Format('  result.%s := %s;', [Field(k), Expr]));
  end;
  EndRoutine;
end;

procedure TGenerator.EmitNegation(AType: longint);
var
  k: longint;
  N: string;
begin
  N := FTypes[AType].Name;
  BeginRoutine(Format('class operator - (const AValue: %s): %s;', [N, N]),
    Format('class operator %s.-(const AValue: %s): %s;', [N, N, N]));
  for k in FTypes[AType].Blades do
    Impl(Format('  result.%s := -AValue.%s;', [Field(k), Field(k)]));
  EndRoutine;
end;

procedure TGenerator.EmitMul(ALeft, ARight: longint);
var
  OwnerName, Params: string;
  ResultType: longint;
begin
  OwnerName := FTypes[Owner(ALeft, ARight)].Name;
  Params := OperatorParams(FTypes[ALeft].Name, FTypes[ARight].Name);
  ResultType := ProductType(ALeft, ARight, @FAlgebra.Geometric);
  CheckDeclared(Owner(ALeft, ARight), ResultType);

  BeginRoutine(Format('class operator * (%s): %s;', [Params, FTypes[ResultType].Name]),
    Format('class operator %s.*(%s): %s;', [OwnerName, Params, FTypes[ResultType].Name]));
  EmitBilinear('ALeft', ALeft, 'ARight', ARight, @FAlgebra.Geometric, ResultType);
  EndRoutine;
end;

procedure TGenerator.EmitDiv(ALeft, ARight: longint);
var
  OwnerName, Params: string;
  ResultType, k: longint;
begin
  OwnerName := FTypes[Owner(ALeft, ARight)].Name;
  Params := OperatorParams(FTypes[ALeft].Name, FTypes[ARight].Name);

  if IsScalarType(ARight) then
    ResultType := ALeft
  else
    ResultType := ProductType(ALeft, FInverseType[ARight], @FAlgebra.Geometric);
  CheckDeclared(Owner(ALeft, ARight), ResultType);

  BeginRoutine(Format('class operator / (%s): %s;', [Params, FTypes[ResultType].Name]),
    Format('class operator %s./(%s): %s;', [OwnerName, Params, FTypes[ResultType].Name]));
  if IsScalarType(ARight) then
  begin
    for k in FTypes[ALeft].Blades do
      Impl(Format('  result.%s := ALeft.%s / ARight;', [Field(k), Field(k)]));
  end else
    Impl('  result := ALeft * ARight.Inverse;');
  EndRoutine;
end;

procedure TGenerator.EmitRecord(AType: longint);
var
  k, Other: longint;
  Partners: TIntArray;
begin
  Intf(Format('  // %s', [FTypes[AType].Name]));
  Intf(Format('  %s = record', [FTypes[AType].Name]));
  Intf('  private');
  for k in FTypes[AType].Blades do
    Intf(Format('    %s: double;', [Field(k)]));
  Intf(Format('    function GetComponent(AComponent: %sComponent): double;', [FTypes[AType].Name]));
  Intf(Format('    procedure SetComponent(AComponent: %sComponent; AValue: double);', [FTypes[AType].Name]));
  Intf('  public');

  // Partners of this record: the scalar, itself and every type declared
  // before it. Mixed operators live in the record declared last.
  Partners := Ints([0, AType]);
  for Other := 1 to MV do
    if Rank(Other) < Rank(AType) then
      Partners := Concat(Partners, Ints([Other]));

  EmitConversions(AType);
  if AType <> MV then Intf('');

  for Other in Partners do
  begin
    EmitComparison(AType, Other, True);
    if Other <> AType then EmitComparison(Other, AType, True);
  end;
  for Other in Partners do
  begin
    EmitComparison(AType, Other, False);
    if Other <> AType then EmitComparison(Other, AType, False);
  end;
  Intf('');

  EmitNegation(AType);
  for Other in Partners do
  begin
    EmitAddSub(AType, Other, True);
    if Other <> AType then EmitAddSub(Other, AType, True);
  end;
  for Other in Partners do
  begin
    EmitAddSub(AType, Other, False);
    if Other <> AType then EmitAddSub(Other, AType, False);
  end;
  Intf('');

  for Other in Partners do
  begin
    EmitMul(AType, Other);
    if Other <> AType then EmitMul(Other, AType);
  end;
  for Other in Partners do
  begin
    EmitDiv(AType, Other);
    if Other <> AType then EmitDiv(Other, AType);
  end;

  Intf('');
  Intf(Format('    property Components[AComponent: %sComponent]: double read GetComponent write SetComponent;',
    [FTypes[AType].Name]));
  Intf('  end;');
  Intf('');

  Impl(Format('function %s.GetComponent(AComponent: %sComponent): double;', [FTypes[AType].Name, FTypes[AType].Name]));
  Impl('begin');
  Impl('  case AComponent of');
  for k in FTypes[AType].Blades do
    Impl(Format('    %s: result := %s;', [ComponentEnum(AType, k), Field(k)]));
  Impl('  end;');
  EndRoutine;

  Impl(Format('procedure %s.SetComponent(AComponent: %sComponent; AValue: double);', [FTypes[AType].Name, FTypes[AType].Name]));
  Impl('begin');
  Impl('  case AComponent of');
  for k in FTypes[AType].Blades do
    Impl(Format('    %s: %s := AValue;', [ComponentEnum(AType, k), Field(k)]));
  Impl('  end;');
  EndRoutine;
end;

procedure TGenerator.EmitHelperInvolutions(AType: longint);
var
  N: string;
  Signs: TIntArray;
  g, k, ResultType: longint;
  S: TSupport;
  Sign: longint;
  I: TBlade;
begin
  N := FTypes[AType].Name;
  Signs := nil;

  // Dual: X * I
  I := FAlgebra.Pseudoscalar;
  S := 0;
  for k in FTypes[AType].Blades do
    S := S or (TSupport(1) shl FAlgebra.IndexOf(FAlgebra.Blade(k) xor I));
  ResultType := TypeOfSupport(S);
  BeginRoutine(Format('function Dual: %s;', [FTypes[ResultType].Name]),
    Format('function %sHelper.Dual: %s;', [N, FTypes[ResultType].Name]));
  for k in FTypes[ResultType].Blades do
  begin
    g := FAlgebra.IndexOf(FAlgebra.Blade(k) xor I);
    FAlgebra.Geometric(FAlgebra.Blade(g), I, Sign);
    if IsScalarType(ResultType) then
      Impl(Format('  result := %s%s;', [Iif(Sign < 0, '-', ''), Field(g)]))
    else
      Impl(Format('  result.%s := %s%s;', [Field(k), Iif(Sign < 0, '-', ''), Field(g)]));
  end;
  EndRoutine;

  SetLength(Signs, FAlgebra.Dimension + 1);

  for g := 0 to FAlgebra.Dimension do Signs[g] := FAlgebra.InvolutionSign(g);
  BeginRoutine(Format('function Involute: %s;', [N]), Format('function %sHelper.Involute: %s;', [N, N]));
  EmitSignMap(AType, Signs);
  EndRoutine;

  for g := 0 to FAlgebra.Dimension do Signs[g] := FAlgebra.ReverseSign(g);
  BeginRoutine(Format('function Reverse: %s;', [N]), Format('function %sHelper.Reverse: %s;', [N, N]));
  EmitSignMap(AType, Signs);
  EndRoutine;

  for g := 0 to FAlgebra.Dimension do Signs[g] := FAlgebra.ConjugateSign(g);
  BeginRoutine(Format('function Conjugate: %s;', [N]), Format('function %sHelper.Conjugate: %s;', [N, N]));
  EmitSignMap(AType, Signs);
  EndRoutine;
end;

procedure TGenerator.EmitHelperNormalized(AType: longint);
var
  N: string;
begin
  N := FTypes[AType].Name;
  BeginRoutine(Format('function Normalized: %s;', [N]), Format('function %sHelper.Normalized: %s;', [N, N]));
  Impl('  result := Self / Norm;');
  EndRoutine;
end;

procedure TGenerator.EmitHelperInverse(AType: longint);
var
  N, R, Cond: string;
  g, k: longint;
begin
  N := FTypes[AType].Name;
  R := FTypes[FInverseType[AType]].Name;

  BeginRoutine(Format('function Inverse: %s;', [R]), Format('function %sHelper.Inverse: %s;', [N, R]));
  if FFastInverse[AType] then
  begin
    // X ~X is a scalar for every element of this grade
    Impl('  result := Reverse / SquaredNorm;');
  end else if AType <> MV then
  begin
    // Same code path as the multivector, so results are identical
    Impl(Format('  result := %s;', [TypedExpression('ToMultivector.Inverse', MV, FInverseType[AType])]));
  end else
  begin
    // Single-grade values take the grade-specific path, so the result does not
    // depend on the static type used to store the value.
    Impl('  case GradeMask of');
    for g := 0 to FAlgebra.Dimension do
      if FFastInverse[g] then
      begin
        Cond := IntToStr(1 shl g);
        if g = 0 then
          Impl(Format('    %s: Exit(1.0 / %s);', [Cond, Field(0)]))
        else
          Impl(Format('    %s: Exit(%s(Self).Inverse);', [Cond, FTypes[g].Name]));
      end;
    Impl('  end;');
    Impl('  result := ShirokovInverse(Self);');
  end;
  EndRoutine;

  if AType = MV then
  begin
    BeginRoutine('function GradeMask: longword;', 'function TMultivectorHelper.GradeMask: longword;');
    Impl('  // Bit g is set when some component of grade g is non-zero');
    Impl('  result := 0;');
    for g := 0 to FAlgebra.Dimension do
    begin
      Cond := '';
      for k in FTypes[g].Blades do
      begin
        if Cond <> '' then Cond := Cond + ' or ';
        Cond := Cond + Format('(%s <> 0.0)', [Field(k)]);
      end;
      Impl(Format('  if %s then result := result or %d;', [Cond, 1 shl g]));
    end;
    EndRoutine;
  end;
end;

procedure TGenerator.EmitHelperNorms(AType: longint);
var
  N: string;
  k, Sign: longint;
  B: TBlade;
  Terms: TStringList;
begin
  N := FTypes[AType].Name;

  BeginRoutine('function Norm: double;', Format('function %sHelper.Norm: double;', [N]));
  Impl('  result := Sqrt(Abs(SquaredNorm));');
  EndRoutine;

  BeginRoutine('function SquaredNorm: double;', Format('function %sHelper.SquaredNorm: double;', [N]));
  Terms := TStringList.Create;
  try
    for k in FTypes[AType].Blades do
    begin
      // <e_A ~e_A>_0 = sign of e_A times its reverse
      B := FAlgebra.Blade(k);
      FAlgebra.Geometric(B, B, Sign);
      Sign := Sign * FAlgebra.ReverseSign(FAlgebra.Grade(B));
      Terms.Add(Iif(Sign > 0, '+', '-') + Field(k) + ' * ' + Field(k));
    end;
    EmitSum('result', Terms);
  finally
    Terms.Free;
  end;
  EndRoutine;
end;

procedure TGenerator.EmitHelperProducts(AType: longint);
var
  N: string;
  Other, ResultType: longint;
begin
  N := FTypes[AType].Name;

  for Other := 1 to MV do
  begin
    ResultType := ProductType(AType, Other, @FAlgebra.Inner);
    BeginRoutine(Format('function Dot(const AVector: %s): %s;', [FTypes[Other].Name, FTypes[ResultType].Name]),
      Format('function %sHelper.Dot(const AVector: %s): %s;', [N, FTypes[Other].Name, FTypes[ResultType].Name]),
      ProductSupport(AType, Other, @FAlgebra.Inner) = 0);
    EmitBilinear('', AType, 'AVector', Other, @FAlgebra.Inner, ResultType);
    EndRoutine;
  end;

  for Other := 1 to MV do
  begin
    ResultType := ProductType(AType, Other, @FAlgebra.Outer);
    BeginRoutine(Format('function Wedge(const AVector: %s): %s;', [FTypes[Other].Name, FTypes[ResultType].Name]),
      Format('function %sHelper.Wedge(const AVector: %s): %s;', [N, FTypes[Other].Name, FTypes[ResultType].Name]),
      ProductSupport(AType, Other, @FAlgebra.Outer) = 0);
    EmitBilinear('', AType, 'AVector', Other, @FAlgebra.Outer, ResultType);
    EndRoutine;
  end;

  if (FAlgebra.Dimension = 3) and (AType = 1) then
  begin
    ResultType := ProductType(1, 1, @FAlgebra.Cross);
    BeginRoutine(Format('function Cross(const AVector: TVector): %s;', [FTypes[ResultType].Name]),
      Format('function TVectorHelper.Cross(const AVector: TVector): %s;', [FTypes[ResultType].Name]));
    EmitBilinear('', 1, 'AVector', 1, @FAlgebra.Cross, ResultType);
    EndRoutine;
  end;
end;

procedure TGenerator.EmitHelperGeometry(AType: longint);
type
  TItem = record
    Formula: TFormula;
    Name: string;
    Params: string;
    Expr: string;
  end;
const
  Items: array[0..3] of TItem = (
    (Formula: fmProjection; Name: 'Projection'; Params: 'const AVector: %s';
     Expr: 'Dot(AVector) * AVector.Inverse'),
    (Formula: fmRejection;  Name: 'Rejection';  Params: 'const AVector: %s';
     Expr: 'Wedge(AVector) * AVector.Inverse'),
    (Formula: fmReflection; Name: 'Reflection'; Params: 'const AVector: %s';
     Expr: 'AVector * Self * AVector.Inverse'),
    (Formula: fmRotation;   Name: 'Rotation';   Params: 'const AVector1, AVector2: %s';
     Expr: 'AVector2 * AVector1 * Self * AVector1.Inverse * AVector2.Inverse'));
var
  N, Params: string;
  Item: TItem;
  Other, Declared, Static, Inv: longint;
  Support: TSupport;
begin
  N := FTypes[AType].Name;
  for Item in Items do
    for Other := 1 to MV do
    begin
      Support := ProbeSupport(Item.Formula, AType, Other);
      Declared := TypeOfSupport(Support);
      Inv := FInverseType[Other];

      // Static type of the expression as the compiler will see it
      case Item.Formula of
        fmProjection: Static := ProductType(ProductType(AType, Other, @FAlgebra.Inner), Inv, @FAlgebra.Geometric);
        fmRejection:  Static := ProductType(ProductType(AType, Other, @FAlgebra.Outer), Inv, @FAlgebra.Geometric);
        fmReflection: Static := ProductType(ProductType(Other, AType, @FAlgebra.Geometric), Inv, @FAlgebra.Geometric);
      else
        Static := ProductType(Other, Other, @FAlgebra.Geometric);
        Static := ProductType(Static, AType, @FAlgebra.Geometric);
        Static := ProductType(Static, Inv, @FAlgebra.Geometric);
        Static := ProductType(Static, Inv, @FAlgebra.Geometric);
      end;

      Params := Format(Item.Params, [FTypes[Other].Name]);
      BeginRoutine(Format('function %s(%s): %s;', [Item.Name, Params, FTypes[Declared].Name]),
        Format('function %sHelper.%s(%s): %s;', [N, Item.Name, Params, FTypes[Declared].Name]),
        Support = 0);
      if Support = 0 then
        Impl('  result := 0.0;')
      else
        Impl(Format('  result := %s;', [TypedExpression(Item.Expr, Static, Declared)]));
      EndRoutine;
    end;
end;

procedure TGenerator.EmitHelperComparison(AType: longint);
var
  N: string;
  Other, k: longint;
  Union: TSupport;
  Terms: TStringList;
  L, R: string;
  Partners: TIntArray;
begin
  N := FTypes[AType].Name;
  Partners := nil;
  if AType = MV then
  begin
    for Other := MV downto 0 do Partners := Concat(Partners, Ints([Other]));
  end else
    Partners := Ints([MV, AType]);

  for Other in Partners do
  begin
    BeginRoutine(Format('function SameValue(const AValue: %s): boolean;', [FTypes[Other].Name]),
      Format('function %sHelper.SameValue(const AValue: %s): boolean;', [N, FTypes[Other].Name]));
    Union := FTypes[AType].Support or FTypes[Other].Support;
    Terms := TStringList.Create;
    try
      for k := 0 to FAlgebra.Count - 1 do
        if (Union shr k) and 1 = 1 then
        begin
          if (FTypes[AType].Support shr k) and 1 = 1 then L := Field(k) else L := '0.0';
          if (FTypes[Other].Support shr k) and 1 = 1 then R := Access('AValue', Other, k) else R := '0.0';
          Terms.Add(Format('Math.SameValue(%s, %s)', [L, R]));
        end;
      for k := 0 to Terms.Count - 1 do
        Impl(Iif(k = 0, '  result := ', '            ') + Terms[k] +
          Iif(k = Terms.Count - 1, ';', ' and'));
    finally
      Terms.Free;
    end;
    EndRoutine;
  end;
end;

procedure TGenerator.EmitHelperExtraction(AType: longint);
var
  N: string;
  t, k: longint;
begin
  N := FTypes[AType].Name;

  // Extract(set) for the own type and, on the multivector, for every k-vector
  for t := MV downto 1 do
    if HasComponentSet(t) and ((t = AType) or (AType = MV)) then
    begin
      BeginRoutine(Format('function Extract(AComponents: %s): %s;', [SetTypeName(t), FTypes[t].Name]),
        Format('function %sHelper.Extract(AComponents: %s): %s;', [N, SetTypeName(t), FTypes[t].Name]));
      Impl(Format('  result := %s;', [NullConstName(t)]));
      for k in FTypes[t].Blades do
        Impl(Format('  if %s in AComponents then result.%s := %s;', [ComponentEnum(t, k), Field(k), Field(k)]));
      EndRoutine;
    end;

  if AType = MV then
  begin
    for t := MV - 1 downto 0 do
    begin
      BeginRoutine(Format('function Extract%s: %s;', [GradeWord(t), FTypes[t].Name]),
        Format('function TMultivectorHelper.Extract%s: %s;', [GradeWord(t), FTypes[t].Name]));
      if t = 0 then
        Impl(Format('  result := %s;', [Field(0)]))
      else
        for k in FTypes[t].Blades do
          Impl(Format('  result.%s := %s;', [Field(k), Field(k)]));
      EndRoutine;
    end;
  end else
  begin
    BeginRoutine('function ToMultivector: TMultivector;', Format('function %sHelper.ToMultivector: TMultivector;', [N]));
    Impl('  result := Self;');
    EndRoutine;
  end;
end;

procedure TGenerator.EmitHelperPredicates(AType: longint);
var
  g, k: longint;
  Terms: TStringList;
  First: boolean;
begin
  if AType <> MV then Exit;

  BeginRoutine('function IsNull: boolean;', 'function TMultivectorHelper.IsNull: boolean;');
  Impl('  result := SameValue(NullMultivector);');
  EndRoutine;

  for g := 0 to FAlgebra.Dimension do
  begin
    BeginRoutine(Format('function Is%s: boolean;', [GradeWord(g)]),
      Format('function TMultivectorHelper.Is%s: boolean;', [GradeWord(g)]));
    Terms := TStringList.Create;
    try
      // Some component of grade g is non-zero, all the others are zero
      First := True;
      Terms.Add('(');
      for k in FTypes[g].Blades do
      begin
        Terms[0] := Terms[0] + Iif(First, '', ' or ') + Format('not Math.SameValue(%s, 0.0)', [Field(k)]);
        First := False;
      end;
      Terms[0] := Terms[0] + ')';
      for k := 0 to FAlgebra.Count - 1 do
        if FAlgebra.Grade(FAlgebra.Blade(k)) <> g then
          Terms.Add(Format('Math.SameValue(%s, 0.0)', [Field(k)]));
      for k := 0 to Terms.Count - 1 do
        Impl(Iif(k = 0, '  result := ', '            ') + Terms[k] +
          Iif(k = Terms.Count - 1, ';', ' and'));
    finally
      Terms.Free;
    end;
    EndRoutine;
  end;

  BeginRoutine('function IsA: string;', 'function TMultivectorHelper.IsA: string;');
  Impl('  if IsNull then Exit(''Null'');');
  for g := FAlgebra.Dimension downto 1 do
    Impl(Format('  if Is%s then Exit(''%s'');', [GradeWord(g), FTypes[g].Name]));
  Impl('  if IsScalar then Exit(''TScalar'');');
  Impl('  result := ''TMultivector'';');
  EndRoutine;
end;

procedure TGenerator.EmitHelperStrings(AType: longint);
var
  N, Values, Names: string;
  k: longint;
begin
  N := FTypes[AType].Name;
  Values := '';
  Names := '';
  for k in FTypes[AType].Blades do
  begin
    if Values <> '' then
    begin
      Values := Values + ', ';
      Names := Names + ', ';
    end;
    Values := Values + Field(k);
    // The scalar component has no basis name
    Names := Names + '''' + Iif(k = 0, '', FAlgebra.Name(FAlgebra.Blade(k))) + '''';
  end;

  BeginRoutine('function ToVerboseString(APrecision, ADigits: longint): string;',
    Format('function %sHelper.ToVerboseString(APrecision, ADigits: longint): string;', [N]));
  Impl(Format('  result := ComponentsToString([%s], [%s], APrecision, ADigits);', [Values, Names]));
  EndRoutine;

  BeginRoutine('function ToString: string;', Format('function %sHelper.ToString: string;', [N]));
  Impl(Format('  result := ComponentsToString([%s], [%s], -1, -1);', [Values, Names]));
  EndRoutine;
end;

procedure TGenerator.EmitHelper(AType: longint);
begin
  Intf(Format('  // %sHelper', [FTypes[AType].Name]));
  Intf(Format('  %sHelper = record helper for %s', [FTypes[AType].Name, FTypes[AType].Name]));
  Impl(Format('// %sHelper', [FTypes[AType].Name]));
  Impl('');

  // Member order: conversions, predicates, scalar measures,
  // transformations, products, text
  EmitHelperExtraction(AType);
  Intf('');
  EmitHelperComparison(AType);
  EmitHelperPredicates(AType);
  Intf('');
  EmitHelperNorms(AType);
  Intf('');
  EmitHelperInvolutions(AType);
  EmitHelperInverse(AType);
  EmitHelperNormalized(AType);
  Intf('');
  EmitHelperGeometry(AType);
  Intf('');
  EmitHelperProducts(AType);
  Intf('');
  EmitHelperStrings(AType);

  Intf('  end;');
  Intf('');
end;

procedure TGenerator.EmitBasisConstants;
var
  g, k, j: longint;
  VersorName, Values: string;
begin
  // Basis blades: 3*e12 builds a TBivector
  for g := 1 to FAlgebra.Dimension do
  begin
    Intf(Format('  // T%s', [VersorWords[g]]));
    for k in FTypes[g].Blades do
    begin
      VersorName := 'T' + VersorWords[g] + FAlgebra.Suffix(FAlgebra.Blade(k));
      Intf(Format('  %s = record', [VersorName]));
      Intf(Format('    class operator *(const AValue: double; const ASelf: %s): %s;', [VersorName, FTypes[g].Name]));
      Intf(Format('    class operator *(const ASelf: %s; const AValue: double): %s;', [VersorName, FTypes[g].Name]));
      Intf('  end;');

      FPopPending := True; // ASelf only selects the basis blade
      Impl('{$push}{$warn 5024 off}');
      Impl(Format('class operator %s.*(const AValue: double; const ASelf: %s): %s;', [VersorName, VersorName, FTypes[g].Name]));
      Impl('begin');
      for j in FTypes[g].Blades do
        Impl(Format('  result.%s := %s;', [Field(j), Iif(j = k, 'AValue', '0.0')]));
      EndRoutine;

      FPopPending := True; // ASelf only selects the basis blade
      Impl('{$push}{$warn 5024 off}');
      Impl(Format('class operator %s.*(const ASelf: %s; const AValue: double): %s;', [VersorName, VersorName, FTypes[g].Name]));
      Impl('begin');
      for j in FTypes[g].Blades do
        Impl(Format('  result.%s := %s;', [Field(j), Iif(j = k, 'AValue', '0.0')]));
      EndRoutine;
    end;
    Intf('');
  end;

  Intf('const');
  for g := 1 to FAlgebra.Dimension do
    for k in FTypes[g].Blades do
      Intf(Format('  %s: T%s%s = ();', [FAlgebra.Name(FAlgebra.Blade(k)), VersorWords[g], FAlgebra.Suffix(FAlgebra.Blade(k))]));
  Intf('');

  // Unit basis values
  for g := 1 to FAlgebra.Dimension do
  begin
    for k in FTypes[g].Blades do
    begin
      Values := '';
      for j in FTypes[g].Blades do
        Values := Values + Iif(Values <> '', '; ', '') + Format('%s: %s', [Field(j), Iif(j = k, '1.0', '0.0')]);
      Intf(Format('  u%s: %s = (%s);', [FAlgebra.Suffix(FAlgebra.Blade(k)), FTypes[g].Name, Values]));
    end;
    Intf('');
  end;

  // Null values
  for g := MV downto 1 do
  begin
    Values := '';
    for j in FTypes[g].Blades do
      Values := Values + Iif(Values <> '', '; ', '') + Format('%s: 0.0', [Field(j)]);
    Intf(Format('  %s: %s = (%s);', [NullConstName(g), FTypes[g].Name, Values]));
  end;
  Intf('  NullScalar: double = 0.0;');
  Intf('');
end;

procedure TGenerator.EmitSupportRoutines;
var
  Steps: longint;
begin
  Impl('function Fmt(const AValue: double; APrecision, ADigits: longint): string;');
  Impl('begin');
  Impl('  if APrecision < 0 then');
  Impl('    result := FloatToStr(AValue)');
  Impl('  else');
  Impl('    result := FloatToStrF(AValue, ffGeneral, APrecision, ADigits);');
  Impl('  if AValue >= 0.0 then');
  Impl('    result := ''+'' + result;');
  Impl('end;');
  Impl('');
  Impl('function ComponentsToString(const AValues: array of double; const ANames: array of string;');
  Impl('  APrecision, ADigits: longint): string;');
  Impl('var');
  Impl('  i: longint;');
  Impl('begin');
  Impl('  result := '''';');
  Impl('  for i := Low(AValues) to High(AValues) do');
  Impl('    if not Math.SameValue(AValues[i], 0.0) then');
  Impl('    begin');
  Impl('      if result <> '''' then result := result + '' '';');
  Impl('      result := result + Fmt(AValues[i], APrecision, ADigits) + ANames[i];');
  Impl('    end;');
  Impl('  if result = '''' then result := ''0'';');
  Impl('end;');
  Impl('');

  // Implementation-only routines; GeometricScalar is <A B>_0 without the full product
  Impl('function GeometricScalar(const ALeft, ARight: TMultivector): double;');
  Impl('begin');
  EmitBilinear('ALeft', MV, 'ARight', MV, @FAlgebra.Geometric, 0);
  Impl('end;');
  Impl('');

  Steps := 1 shl ((FAlgebra.Dimension + 1) div 2);
  Impl('function ShirokovInverse(const AValue: TMultivector): TMultivector;');
  Impl('const');
  Impl(Format('  N = %d; // 2^ceil(n/2), n = %d', [Steps, FAlgebra.Dimension]));
  Impl('var');
  Impl('  U: TMultivector;');
  Impl('  k: longint;');
  Impl('begin');
  Impl('  // U(1) = A; U(k) = A (U(k-1) - C(k-1)); C(k) = N/k <U(k)>_0');
  Impl('  // A^-1 = (U(N-1) - C(N-1)) / C(N)');
  Impl('  U := AValue;');
  Impl(Format('  U.%s := U.%s - N * U.%s;', [Field(0), Field(0), Field(0)]));
  Impl('  for k := 2 to N - 1 do');
  Impl('  begin');
  Impl('    U := AValue * U;');
  Impl(Format('    U.%s := U.%s - (N / k) * U.%s;', [Field(0), Field(0), Field(0)]));
  Impl('  end;');
  Impl('  result := U / GeometricScalar(AValue, U);');
  Impl('end;');
  Impl('');
end;

procedure TGenerator.Generate(ADest: TStrings);
var
  t: longint;
begin
  FIntf.Clear;
  FImpl.Clear;

  EmitHeader;
  EmitSupportRoutines;
  EmitComponentSets;
  EmitRecord(MV);
  for t := 1 to MV - 1 do EmitRecord(t);
  EmitHelper(MV);
  for t := 1 to MV - 1 do EmitHelper(t);
  EmitBasisConstants;
  Impl('end.');

  ADest.BeginUpdate;
  try
    ADest.Clear;
    ADest.AddStrings(FIntf);
    ADest.AddStrings(FImpl);
  finally
    ADest.EndUpdate;
  end;
end;

procedure TGenerator.GenerateTest(ADest: TStrings; AIterations: longint);
var
  L: TStrings;

  procedure Add(const ALine: string);
  begin
    L.Add(ALine);
  end;

  function V(APrefix: char; AType: longint): string;
  begin
    // Test variable of a given type, e.g. X2 is a TBivector
    result := APrefix + IntToStr(AType);
  end;

  function UnitConst(ABlade: TBlade): string;
  begin
    result := 'u' + FAlgebra.Suffix(ABlade);
  end;

  procedure AddGuarded(const ALine: string);
  begin
    // Random integer operands can be exactly singular: those cases are skipped
    Add('  try ' + Trim(ALine) + ' except on EMathError do Inc(Skipped); end;');
  end;

var
  t, u, i, j, k, Sign: longint;
  B: TBlade;
  Chain: string;
begin
  if AIterations <= 0 then
    AIterations := Max(100, 100000 div FAlgebra.Count);
  L := ADest;
  L.BeginUpdate;
  try
    L.Clear;
    Add(Format('program %scheck;', [FUnitName]));
    Add('');
    Add(Format('{ Self-check of the generated unit %s: algebraic laws, and consistency', [FUnitName]));
    Add('  between the k-vector types and TMultivector.');
    Add('  Generated by the Clifford toolkit (clgencore.pas): do not edit by hand. }');
    Add('');
    Add('{$mode objfpc}{$H+}');
    Add('');
    Add('uses');
    Add(Format('  SysUtils, Math, %s;', [FUnitName]));
    Add('');
    Add('var');
    Add('  Checks: longint = 0;');
    Add('  Failures: longint = 0;');
    Add('  Skipped: longint = 0;');
    Add('');
    Add('procedure Check(ACondition: boolean; const AName: string);');
    Add('begin');
    Add('  Inc(Checks);');
    Add('  if ACondition then Exit;');
    Add('  Inc(Failures);');
    Add('  if Failures <= 20 then Writeln(''FAILED: '', AName);');
    Add('end;');
    Add('');
    Add('function Rnd: double;');
    Add('begin');
    Add('  // Small integers keep sums of products exact in double precision');
    Add('  result := RandomRange(-9, 10);');
    Add('end;');
    Add('');
    Add('function ToMV(const AValue: TMultivector): TMultivector;');
    Add('begin');
    Add('  result := AValue;');
    Add('end;');
    Add('');
    Add('function Same(const A, B: TMultivector): boolean;');
    Add('begin');
    Add('  result := A = B;');
    Add('end;');
    Add('');
    Add('function Near(const A, B: TMultivector; ATolerance: double = 1e-7): boolean;');
    Add('var');
    Add('  C: TMultivectorComponent;');
    Add('  Scale: double;');
    Add('begin');
    Add('  Scale := 1.0;');
    Add('  for C := Low(TMultivectorComponent) to High(TMultivectorComponent) do');
    Add('    Scale := Max(Scale, Max(Abs(A.Components[C]), Abs(B.Components[C])));');
    Add('  for C := Low(TMultivectorComponent) to High(TMultivectorComponent) do');
    Add('    if Abs(A.Components[C] - B.Components[C]) > ATolerance * Scale then Exit(False);');
    Add('  result := True;');
    Add('end;');
    Add('');

    for t := 1 to MV do
    begin
      Add(Format('function Random%s: %s;', [Copy(FTypes[t].Name, 2, MaxInt), FTypes[t].Name]));
      Add('var');
      Add(Format('  C: %sComponent;', [FTypes[t].Name]));
      Add('begin');
      Add(Format('  result := %s;', [NullConstName(t)]));
      Add(Format('  for C := Low(%sComponent) to High(%sComponent) do', [FTypes[t].Name, FTypes[t].Name]));
      Add('    result.Components[C] := Rnd;');
      Add('end;');
      Add('');
    end;

    Add('procedure CheckBasis;');
    Add('begin');
    // Metric and anticommutation of the basis vectors
    for i := 0 to FAlgebra.Dimension - 1 do
    begin
      B := TBlade(1) shl i;
      Add(Format('  Check(Same(%s * %s, %d), ''%s^2 = %d'');',
        [UnitConst(B), UnitConst(B), FAlgebra.VectorSquare(i), FAlgebra.Name(B), FAlgebra.VectorSquare(i)]));
    end;
    for i := 0 to FAlgebra.Dimension - 1 do
      for j := i + 1 to FAlgebra.Dimension - 1 do
        Add(Format('  Check(Same(%s * %s, -(%s * %s)), ''%s %s = -%s %s'');',
          [UnitConst(1 shl i), UnitConst(1 shl j), UnitConst(1 shl j), UnitConst(1 shl i),
           FAlgebra.Name(1 shl i), FAlgebra.Name(1 shl j), FAlgebra.Name(1 shl j), FAlgebra.Name(1 shl i)]));
    // Every basis blade is the ordered product of its vectors
    for k := 0 to FAlgebra.Count - 1 do
    begin
      B := FAlgebra.Blade(k);
      if FAlgebra.Grade(B) < 2 then Continue;
      Chain := '';
      for i := 0 to FAlgebra.Dimension - 1 do
        if (B shr i) and 1 = 1 then
          Chain := Chain + Iif(Chain = '', 'ToMV(', ' * ') + UnitConst(1 shl i) + Iif(Chain = '', ')', '');
      Add(Format('  Check(Same(%s, %s), ''%s as a product of vectors'');', [Chain, UnitConst(B), FAlgebra.Name(B)]));
    end;
    if FAlgebra.Dimension = 3 then
    begin
      // a x b = (a ^ b) I^-1; in Cl(3) this is the usual right-handed cross product
      FAlgebra.Cross(1, 2, Sign);
      Add(Format('  Check(Same(u%s.Cross(u%s), %d * u%s), ''cross product of the first two basis vectors'');',
        [FAlgebra.Suffix(1), FAlgebra.Suffix(2), Sign, FAlgebra.Suffix(4)]));
    end;
    Add('end;');
    Add('');

    Add('procedure CheckRandom;');
    Add('var');
    for t := 1 to MV do
      Add(Format('  %s, %s, %s: %s;', [V('X', t), V('Y', t), V('Z', t), FTypes[t].Name]));
    Add('begin');
    for t := 1 to MV do
      Add(Format('  %s := Random%s; %s := Random%s; %s := Random%s;', [V('X', t), Copy(FTypes[t].Name, 2, MaxInt),
        V('Y', t), Copy(FTypes[t].Name, 2, MaxInt), V('Z', t), Copy(FTypes[t].Name, 2, MaxInt)]));
    Add('');

    // Laws on general multivectors (exact with integer inputs)
    Add(Format('  Check(Same((%0:s * %1:s) * %2:s, %0:s * (%1:s * %2:s)), ''associativity'');', [V('X', MV), V('Y', MV), V('Z', MV)]));
    Add(Format('  Check(Same((%0:s * %1:s).Reverse, %1:s.Reverse * %0:s.Reverse), ''reverse of a product'');', [V('X', MV), V('Y', MV)]));
    Add(Format('  Check(Same((%0:s * %1:s).Involute, %0:s.Involute * %1:s.Involute), ''involution of a product'');', [V('X', MV), V('Y', MV)]));
    Add(Format('  Check(Same(%0:s.Conjugate, %0:s.Reverse.Involute), ''conjugate = reverse of involution'');', [V('X', MV)]));
    Add(Format('  Check(%0:s.SquaredNorm = (%0:s * %0:s.Reverse).ExtractScalar, ''squared norm'');', [V('X', MV)]));
    Add(Format('  Check(Same(%0:s.Dual, %0:s * %1:s), ''dual'');', [V('X', MV), UnitConst(FAlgebra.Pseudoscalar)]));
    Add(Format('  Check(Same(%0:s.Wedge(%1:s), (%0:s * %1:s - %1:s * %0:s) / 2), ''wedge of vectors'');', [V('X', 1), V('Y', 1)]));
    Add(Format('  Check(Same(%0:s.Dot(%1:s), (%0:s * %1:s + %1:s * %0:s) / 2), ''dot of vectors'');', [V('X', 1), V('Y', 1)]));
    AddGuarded(Format('  Check(Near(%0:s * %0:s.Inverse, 1), ''right inverse'');', [V('X', MV)]));
    AddGuarded(Format('  Check(Near(%0:s.Inverse * %0:s, 1), ''left inverse'');', [V('X', MV)]));
    AddGuarded(Format('  Check(Near(%0:s.Projection(%1:s) + %0:s.Rejection(%1:s), %0:s), ''projection + rejection'');', [V('X', 1), V('Y', 1)]));
    AddGuarded(Format('  Check(Near(%0:s.Rotation(%1:s, %2:s).SquaredNorm, %0:s.SquaredNorm), ''rotation preserves the norm'');',
      [V('X', 1), V('Y', 1), V('Z', 1)]));
    Add('');

    // Every specialised type agrees with the multivector
    for t := 1 to MV do
    begin
      Add(Format('  // %s', [FTypes[t].Name]));
      AddGuarded(Format('  Check(Near(%0:s * %0:s.Inverse, 1), ''%1:s inverse'');', [V('X', t), FTypes[t].Name]));
      AddGuarded(Format('  Check(Same(%0:s.Inverse, ToMV(%0:s).Inverse), ''%1:s inverse type'');', [V('X', t), FTypes[t].Name]));
      Add(Format('  Check(Same(%0:s.Dual, ToMV(%0:s).Dual), ''%1:s dual'');', [V('X', t), FTypes[t].Name]));
      Add(Format('  Check(Same(%0:s.Reverse, ToMV(%0:s).Reverse), ''%1:s reverse'');', [V('X', t), FTypes[t].Name]));
      Add(Format('  Check(Same(%0:s.Conjugate, ToMV(%0:s).Conjugate), ''%1:s conjugate'');', [V('X', t), FTypes[t].Name]));
      Add(Format('  Check(Same(%0:s.Involute, ToMV(%0:s).Involute), ''%1:s involution'');', [V('X', t), FTypes[t].Name]));
      Add(Format('  Check(%0:s.SquaredNorm = ToMV(%0:s).SquaredNorm, ''%1:s squared norm'');', [V('X', t), FTypes[t].Name]));
      Add(Format('  Check(Same(-%0:s, -ToMV(%0:s)), ''%1:s negation'');', [V('X', t), FTypes[t].Name]));
      Add(Format('  Check(Same(2.5 * %0:s, 2.5 * ToMV(%0:s)), ''%1:s scaling'');', [V('X', t), FTypes[t].Name]));
      for u := 1 to MV do
      begin
        Add(Format('  Check(Same(%0:s * %1:s, ToMV(%0:s) * ToMV(%1:s)), ''%2:s * %3:s'');', [V('X', t), V('Y', u), FTypes[t].Name, FTypes[u].Name]));
        Add(Format('  Check(Same(%0:s + %1:s, ToMV(%0:s) + ToMV(%1:s)), ''%2:s + %3:s'');', [V('X', t), V('Y', u), FTypes[t].Name, FTypes[u].Name]));
        Add(Format('  Check(Same(%0:s - %1:s, ToMV(%0:s) - ToMV(%1:s)), ''%2:s - %3:s'');', [V('X', t), V('Y', u), FTypes[t].Name, FTypes[u].Name]));
        Add(Format('  Check(Same(%0:s.Dot(%1:s), ToMV(%0:s).Dot(ToMV(%1:s))), ''%2:s dot %3:s'');', [V('X', t), V('Y', u), FTypes[t].Name, FTypes[u].Name]));
        Add(Format('  Check(Same(%0:s.Wedge(%1:s), ToMV(%0:s).Wedge(ToMV(%1:s))), ''%2:s wedge %3:s'');', [V('X', t), V('Y', u), FTypes[t].Name, FTypes[u].Name]));
        AddGuarded(Format('  Check(Near(%0:s / %1:s, ToMV(%0:s) / ToMV(%1:s)), ''%2:s / %3:s'');', [V('X', t), V('Y', u), FTypes[t].Name, FTypes[u].Name]));
        AddGuarded(Format('  Check(Near(%0:s.Projection(%1:s), ToMV(%0:s).Projection(ToMV(%1:s))), ''%2:s projection on %3:s'');', [V('X', t), V('Y', u), FTypes[t].Name, FTypes[u].Name]));
        AddGuarded(Format('  Check(Near(%0:s.Rejection(%1:s), ToMV(%0:s).Rejection(ToMV(%1:s))), ''%2:s rejection on %3:s'');', [V('X', t), V('Y', u), FTypes[t].Name, FTypes[u].Name]));
        AddGuarded(Format('  Check(Near(%0:s.Reflection(%1:s), ToMV(%0:s).Reflection(ToMV(%1:s))), ''%2:s reflection by %3:s'');', [V('X', t), V('Y', u), FTypes[t].Name, FTypes[u].Name]));
        AddGuarded(Format('  Check(Near(%0:s.Rotation(%1:s, %2:s), ToMV(%0:s).Rotation(ToMV(%1:s), ToMV(%2:s))), ''%3:s rotation by %4:s'');', [V('X', t), V('Y', u), V('Z', u), FTypes[t].Name, FTypes[u].Name]));
      end;
    end;
    Add('end;');
    Add('');

    Add('var');
    Add('  Iteration: longint;');
    Add('begin');
    Add('  RandSeed := 1;');
    Add('  CheckBasis;');
    Add(Format('  for Iteration := 1 to %d do', [AIterations]));
    Add('    CheckRandom;');
    Add(Format('  Writeln(Format(''%s: %%d checks, %%d failures, %%d skipped (singular operands)'', [Checks, Failures, Skipped]));', [FUnitName]));
    Add('  if Failures > 0 then Halt(1);');
    Add('end.');
  finally
    L.EndUpdate;
  end;
end;

end.
