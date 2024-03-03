unit Cl3;

{ Geometric Algebra for FreePascal.

  Copyright (c) 2024 Melchiorre Caruso

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

// Wedge product <-> Cross product
//
// xi ^ eta =  (xi x eta) I
// xi x eta = -(xi ^ eta) I
//

// References:
// - Chris Doran, Anthony Lasenby,
//   Geometric algebra for Physicists, (2003) Cambridge University Press.
// - Eckhard Hitzer, Stephen Sangwine,
//   Multivector and multivector matrix inverses in real Clifford algebras,
//   (2016) Preprint: Technical Report CES-534, ISSN: 1744-8050.
// - James M. Chappell, Azhar Iqbal, Lachlan J. Gunn, Derek Abbott,
//   Function of multivector variables, (2015) PLoS One.

{$mode objfpc}{$h+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils;

type
  // TMultivectorGrade
  TMultivectorGrade = (gScalar, gVector, gBivector, gTrivector);

  // TMultivector
  TMultivector = record
  private
    fm0: double;
    fm1: double;
    fm2: double;
    fm3: double;
    fm12: double;
    fm23: double;
    fm31: double;
    fm123: double;
  public
    class operator <>(const ALeft, ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; const ARight: double): boolean;
    class operator <>(const ALeft: double; const ARight: TMultivector): boolean;

    class operator = (const ALeft, ARight: TMultivector): boolean;
    class operator = (const ALeft: TMultivector; const ARight: double): boolean;
    class operator = (const ALeft: double; const ARight: TMultivector): boolean;

    class operator + (const ALeft, ARight: TMultivector): TMultivector;
    class operator + (const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator + (const ALeft: double; const ARight: TMultivector): TMultivector;

    class operator - (const ASelf: TMultivector): TMultivector;
    class operator - (const ALeft, ARight: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator - (const ALeft: double; const ARight: TMultivector): TMultivector;

    class operator * (const ALeft, ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator * (const ALeft: double; const ARight: TMultivector): TMultivector;

    class operator / (const ALeft, ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator / (const ALeft: double; const ARight: TMultivector): TMultivector;
  end;

  // TTrivector
  TTrivector = record
  private
    fm123: double;
  public
    class operator <>(const ALeft, ARight: TTrivector): boolean;
    class operator <>(const ALeft: TTrivector; const ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; const ARight: TTrivector): boolean;

    class operator = (const ALeft, ARight: TTrivector): boolean;
    class operator = (const ALeft: TTrivector; const ARight: TMultivector): boolean;
    class operator = (const ALeft: TMultivector; const ARight: TTrivector): boolean;

    class operator + (const ALeft, ARight: TTrivector): TTrivector;
    class operator + (const ALeft: TTrivector; const ARight: double): TMultivector;
    class operator + (const ALeft: double; const ARight: TTrivector): TMultivector;
    class operator + (const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator + (const ALeft: TMultivector; const ARight: TTrivector): TMultivector;

    class operator - (const ASelf: TTrivector): TTrivector;
    class operator - (const ALeft, ARight: TTrivector): TTrivector;
    class operator - (const ALeft: TTrivector; const ARight: double): TMultivector;
    class operator - (const ALeft: double; const ARight: TTrivector): TMultivector;
    class operator - (const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: TTrivector): TMultivector;

    class operator * (const ALeft: double; const ARight: TTrivector): TTrivector;
    class operator * (const ALeft: TTrivector; const ARight: double): TTrivector;
    //geometric product
    class operator * (const ALeft, ARight: TTrivector): double;
    class operator * (const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: TTrivector): TMultivector;

    class operator / (const ALeft, ARight: TTrivector): double;
    class operator / (const ALeft: TTrivector; const ARight: double): TTrivector;
    class operator / (const ALeft: double; const ARight: TTrivector): TTrivector;
    class operator / (const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: TTrivector): TMultivector;
  end;

  // TBivector
  TBivector = record
  private
    fm12: double;
    fm23: double;
    fm31: double;
  public
    class operator <>(const ALeft, ARight: TBivector): boolean;
    class operator <>(const ALeft: TBivector; const ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; const ARight: TBivector): boolean;

    class operator = (const ALeft, ARight: TBivector): boolean;
    class operator = (const ALeft: TBivector; const ARight: TMultivector): boolean;
    class operator = (const ALeft: TMultivector; const ARight: TBivector): boolean;

    class operator + (const ALeft, ARight: TBivector): TBivector;
    class operator + (const ALeft: TBivector; const ARight: double): TMultivector;
    class operator + (const ALeft: double; const ARight: TBivector): TMultivector;
    class operator + (const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator + (const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator + (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator + (const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    class operator - (const ASelf: TBivector): TBivector;
    class operator - (const ALeft, ARight: TBivector): TBivector;
    class operator - (const ALeft: TBivector; const ARight: double): TMultivector;
    class operator - (const ALeft: double; const ARight: TBivector): TMultivector;
    class operator - (const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator - (const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator - (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    class operator * (const ALeft: double; const ARight: TBivector): TBivector;
    class operator * (const ALeft: TBivector; const ARight: double): TBivector;
    // geometric product
    class operator * (const ALeft, ARight: TBivector): TMultivector;
    class operator * (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator * (const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    class operator / (const ALeft, ARight: TBivector): TMultivector;
    class operator / (const ALeft: TBivector; const ARight: double): TBivector;
    class operator / (const ALeft: double; const ARight: TBivector): TBivector;
    class operator / (const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator / (const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator / (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: TBivector): TMultivector;
  end;

  // TVector
  TVector = record
  private
    fm1: double;
    fm2: double;
    fm3: double;
  public
    class operator <>(const ALeft, ARight: TVector): boolean;
    class operator <>(const ALeft: TVector; const ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; const ARight: TVector): boolean;

    class operator = (const ALeft, ARight: TVector): boolean;
    class operator = (const ALeft: TVector; const ARight: TMultivector): boolean;
    class operator = (const ALeft: TMultivector; const ARight: TVector): boolean;

    class operator + (const ALeft, ARight: TVector): TVector;
    class operator + (const ALeft: TVector; const ARight: double): TMultivector;
    class operator + (const ALeft: double; const ARight: TVector): TMultivector;
    class operator + (const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator + (const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator + (const ALeft: TVector; const ARight: TTrivector): TMultivector;
    class operator + (const ALeft: TTrivector; const ARight: TVector): TMultivector;
    class operator + (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator + (const ALeft: TMultivector; const ARight: TVector): TMultivector;

    class operator - (const ASelf: TVector): TVector;
    class operator - (const ALeft, ARight: TVector): TVector;
    class operator - (const ALeft: TVector; const ARight: double): TMultivector;
    class operator - (const ALeft: double; const ARight: TVector): TMultivector;
    class operator - (const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator - (const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator - (const ALeft: TVector; const ARight: TTrivector): TMultivector;
    class operator - (const ALeft: TTrivector; const ARight: TVector): TMultivector;
    class operator - (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: TVector): TMultivector;

    class operator * (const ALeft: double; const ARight: TVector): TVector;
    class operator * (const ALeft: TVector; const ARight: double): TVector;
    // geometric product
    class operator * (const ALeft, ARight: TVector): TMultivector;
    class operator * (const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator * (const ALeft: TVector; const ARight: TTrivector): TBivector;
    class operator * (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator * (const ALeft: TTrivector; const ARight: TVector): TBivector;
    class operator * (const ALeft: TMultivector; const ARight: TVector): TMultivector;

    class operator / (const ALeft: double; const ARight: TVector): TVector;
    class operator / (const ALeft: TVector; const ARight: double): TVector;
    class operator / (const ALeft, ARight: TVector): TMultivector;
    class operator / (const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator / (const ALeft: TVector; const ARight: TTrivector): TBivector;
    class operator / (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator / (const ALeft: TTrivector; const ARight: TVector): TBivector;
    class operator / (const ALeft: TMultivector; const ARight: TVector): TMultivector;
  end;

  // TMultivectorHelper
  TMultivectorHelper = record helper for TMultivector
    function Dual: TMultivector;
    function Inverse: TMultivector;
    function Reverse: TMultivector;
    function Conjugate: TMultivector;
    function Reciprocal: TMultivector;
    function Norm(AGrade: TMultivectorGrade): double;
    function SquaredNorm: TMultivector;

    function Dot(const AVector: TVector): TMultivector; overload;
    function Dot(const AVector: TBivector): TMultivector; overload;
    function Dot(const AVector: TTrivector): TMultivector; overload;
    function Dot(const AVector: TMultivector): TMultivector; overload;

    function Wedge(const AVector: TVector): TMultivector; overload;
    function Wedge(const AVector: TBivector): TMultivector; overload;
    function Wedge(const AVector: TTrivector): TTrivector; overload;
    function Wedge(const AVector: TMultivector): TMultivector; overload;

    function Projection(const AVector: TVector): TMultivector; overload;
    function Projection(const AVector: TBivector): TMultivector; overload;
    function Projection(const AVector: TTrivector): TMultivector; overload;
    function Projection(const AVector: TMultivector): TMultivector; overload;

    function Rejection(const AVector: TVector): TMultivector; overload;
    function Rejection(const AVector: TBivector): TMultivector; overload;
    function Rejection(const AVector: TTrivector): double; overload;
    function Rejection(const AVector: TMultivector): TMultivector; overload;

    function Reflection(const AVector: TVector): TMultivector; overload;
    function Reflection(const AVector: TBivector): TMultivector; overload;
    function Reflection(const AVector: TTrivector): TMultivector; overload;
    function Reflection(const AVector: TMultivector): TMultivector; overload;

    function Rotation(const AVector1, AVector2: TVector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TBivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TTrivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;overload;

    function SameValue(const AVector: TMultivector): boolean;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;

    function ExtractTrivector: TTrivector;
    function ExtractBivector: TBivector;
    function ExtractVector: TVector;
    function ExtractScalar: double;

    function IsZero: boolean;
    function IsScalar: boolean;
    function IsVector: boolean;
    function IsBiVector: boolean;
    function IsTrivector: boolean;
    function IsA: string;
  end;

  // TTrivectorHelper
  TTrivectorHelper = record helper for TTrivector
    function Dual: double;
    function Inverse: TTrivector;
    function Reverse: TTrivector;
    function Conjugate: TTrivector;
    function Reciprocal: TTrivector;
    function Normalized: TTrivector;
    function Norm: double;
    function SquaredNorm: double;

    function Dot(const AVector: TVector): TBivector; overload;
    function Dot(const AVector: TBivector): TVector; overload;
    function Dot(const AVector: TTrivector): double; overload;
    function Dot(const AVector: TMultivector): TMultivector; overload;

    function Wedge(const AVector: TVector): double; overload; // zero
    function Wedge(const AVector: TBivector): double; overload; // zero
    function Wedge(const AVector: TTrivector): double; overload; // zero
    function Wedge(const AVector: TMultivector): TTrivector; overload;

    function Projection(const AVector: TVector): TTrivector; overload;
    function Projection(const AVector: TBivector): TTrivector; overload;
    function Projection(const AVector: TTrivector): TTrivector; overload;
    function Projection(const AVector: TMultivector): TTrivector; overload;

    function Rejection(const AVector: TVector): double; overload; // zero
    function Rejection(const AVector: TBivector): double; overload; // zero
    function Rejection(const AVector: TTrivector): double; overload; // zero
    function Rejection(const AVector: TMultivector): TMultivector; overload;

    function Reflection(const AVector: TVector): TTrivector; overload;
    function Reflection(const AVector: TBivector): TTrivector; overload;
    function Reflection(const AVector: TTrivector): TTrivector; overload;
    function Reflection(const AVector: TMultivector): TTrivector; overload;

    function Rotation(const AVector1, AVector2: TVector): TTrivector; overload;
    function Rotation(const AVector1, AVector2: TBivector): TTrivector; overload;
    function Rotation(const AVector1, AVector2: TTrivector): TTrivector; overload;
    function Rotation(const AVector1, AVector2: TMultivector): TTrivector; overload;

    function SameValue(const AVector: TTrivector): boolean;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
    function ToMultivector: TMultivector;
  end;

  // TBivectorHelper
  TBivectorHelper = record helper for TBivector
    function Dual: TVector;
    function Inverse: TBivector;
    function Reverse: TBivector;
    function Conjugate: TBivector;
    function Reciprocal: TBivector;
    function Normalized: TBivector;
    function Norm: double;
    function SquaredNorm: double;

    function Dot(const AVector: TVector): TVector; overload;
    function Dot(const AVector: TBivector): double; overload;
    function Dot(const AVector: TTrivector): TVector; overload;
    function Dot(const AVector: TMultivector): TMultivector; overload;

    function Wedge(const AVector: TVector): TTrivector; overload;
    function Wedge(const AVector: TBivector): double; overload; //zero
    function Wedge(const AVector: TTrivector): double; overload; //zero
    function Wedge(const AVector: TMultivector): TMultivector; overload;

    function Projection(const AVector: TVector): TBivector; overload;
    function Projection(const AVector: TBivector): TBivector; overload;
    function Projection(const AVector: TTrivector): TBivector; overload;
    function Projection(const AVector: TMultivector): TMultivector; overload;

    function Rejection(const AVector: TVector): TBivector; overload;
    function Rejection(const AVector: TBivector): double; overload; //zero
    function Rejection(const AVector: TTrivector): double; overload; //zero
    function Rejection(const AVector: TMultivector): TMultivector; overload;

    function Reflection(const AVector: TVector): TBivector; overload;
    function Reflection(const AVector: TBivector): TBivector; overload;
    function Reflection(const AVector: TTrivector): TBivector; overload;
    function Reflection(const AVector: TMultivector): TMultivector; overload;

    function Rotation(const AVector1, AVector2: TVector): TBivector; overload;
    function Rotation(const AVector1, AVector2: TBivector): TBivector; overload;
    function Rotation(const AVector1, AVector2: TTrivector): TBivector; overload;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector; overload;

    function SameValue(const AVector: TBivector): boolean;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
    function ToMultivector: TMultivector;
  end;

  // TVectorHelper
  TVectorHelper = record helper for TVector
    function Dual: TBivector;
    function Inverse: TVector;
    function Reverse: TVector;
    function Conjugate: TVector;
    function SquaredNorm: double;
    function Norm: double;
    function Normalized: TVector;
    function Reciprocal: TVector;

    function Dot(const AVector: TVector): double; overload;
    function Dot(const AVector: TBivector): TVector; overload;
    function Dot(const AVector: TTrivector): TBivector; overload;
    function Dot(const AVector: TMultivector): TMultivector; overload;

    function Wedge(const AVector: TVector): TBivector; overload;
    function Wedge(const AVector: TBivector): TTrivector; overload;
    function Wedge(const AVector: TTrivector): double; overload; // zero
    function Wedge(const AVector: TMultivector): TMultivector; overload;

    function Cross(const AVector: TVector): TVector;

    function Projection(const AVector: TVector): TVector; overload;
    function Projection(const AVector: TBivector): TVector; overload;
    function Projection(const AVector: TTrivector): TVector; overload;
    function Projection(const AVector: TMultivector): TMultivector; overload;

    function Rejection(const AVector: TVector): TVector; overload;
    function Rejection(const AVector: TBivector): TVector; overload;
    function Rejection(const AVector: TTrivector): double; overload; // zero
    function Rejection(const AVector: TMultivector): TMultivector; overload;

    function Reflection(const AVector: TVector): TVector; overload;
    function Reflection(const AVector: TBivector): TVector; overload;
    function Reflection(const AVector: TTrivector): TVector; overload;
    function Reflection(const AVector: TMultivector): TMultivector; overload;

    function Rotation(const AVector1, AVector2: TVector): TVector; overload;
    function Rotation(const AVector1, AVector2: TBivector): TVector; overload;
    function Rotation(const AVector1, AVector2: TTrivector): TVector; overload;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector; overload;

    function LERP(const AVector1, AVector2: TVector; const AFactor: double = 0.5): TVector;
    function SLERP(const AVector1, AVector2: TVector; const AFactor: double = 0.5): TVector;


    function SameValue(const AVector: TVector): boolean;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
    function ToMultivector: TMultivector;
  end;

  // TVersor
  TVersor1 = record class operator *(const AValue: double; const ASelf: TVersor1): TVector; end;
  TVersor2 = record class operator *(const AValue: double; const ASelf: TVersor2): TVector; end;
  TVersor3 = record class operator *(const AValue: double; const ASelf: TVersor3): TVector; end;

  // TBiversor
  TBiversor12 = record class operator *(const AValue: double; const ASelf: TBiversor12): TBivector; end;
  TBiversor23 = record class operator *(const AValue: double; const ASelf: TBiversor23): TBivector; end;
  TBiversor31 = record class operator *(const AValue: double; const ASelf: TBiversor31): TBivector; end;

  // TTriversor
  TTriversor123 = record class operator *(const AValue: double; const ASelf: TTriversor123): TTrivector; end;

const
  e1   : TVersor1      = ();
  e2   : TVersor2      = ();
  e3   : TVersor3      = ();
  e12  : TBiversor12   = ();
  e23  : TBiversor23   = ();
  e31  : TBiversor31   = ();
  e123 : TTriversor123 = ();

  u1   : TVector = (fm1:1; fm2:0; fm3:0);
  u2   : TVector = (fm1:0; fm2:1; fm3:0);
  u3   : TVector = (fm1:0; fm2:0; fm3:1);

  u12  : TBivector = (fm12:1; fm23:0; fm31:0);
  u23  : TBivector = (fm12:0; fm23:1; fm31:0);
  u31  : TBivector = (fm12:0; fm23:0; fm31:1);

  u123 : TTrivector = (fm123:1);

  NullMultivector : TMultivector = (fm0:0; fm1:0; fm2:0; fm3:0; fm12:0; fm23:0; fm31:0; fm123:0);
  NullTrivector   : TTrivector   = (fm123:0);
  NullBivector    : TBivector    = (fm12:0; fm23:0; fm31:0);
  NullVector      : TVector      = (fm1:0; fm2:0; fm3:0);
  NullScalar      : double       = (0);

implementation

uses
  Math;

function Fmt(const AValue: double): string;
begin
  if AValue < 0 then
    result := FloatToStr(AValue)
  else
    result := '+' + FloatToStr(AValue);
end;

function Fmt(const AValue: double; APrecision, ADigits: longint): string;
begin
  if AValue < 0 then
    result := FloatToStrF(AValue, ffGeneral, APrecision, ADigits)
  else
    result := '+' + FloatToStrF(AValue, ffGeneral, APrecision, ADigits);
end;

// TMultivector

class operator TMultivector.<>(const ALeft, ARight: TMultivector): boolean;
begin
  result := (ALeft.fm0   <> ARight.fm0  ) or
            (ALeft.fm1   <> ARight.fm1  ) or
            (ALeft.fm2   <> ARight.fm2  ) or
            (ALeft.fm3   <> ARight.fm3  ) or
            (ALeft.fm12  <> ARight.fm12 ) or
            (ALeft.fm23  <> ARight.fm23 ) or
            (ALeft.fm31  <> ARight.fm31 ) or
            (ALeft.fm123 <> ARight.fm123);
end;

class operator TMultivector.<>(const ALeft: TMultivector; const ARight: double): boolean;
begin
  result := (ALeft.fm0   <> ARight) or
            (ALeft.fm1   <>      0) or
            (ALeft.fm2   <>      0) or
            (ALeft.fm3   <>      0) or
            (ALeft.fm12  <>      0) or
            (ALeft.fm23  <>      0) or
            (ALeft.fm31  <>      0) or
            (ALeft.fm123 <>      0);
end;

class operator TMultivector.<>(const ALeft: double; const ARight: TMultivector): boolean;
begin
  result := (ALeft <> ARight.fm0  ) or
            (0     <> ARight.fm1  ) or
            (0     <> ARight.fm2  ) or
            (0     <> ARight.fm3  ) or
            (0     <> ARight.fm12 ) or
            (0     <> ARight.fm23 ) or
            (0     <> ARight.fm31 ) or
            (0     <> ARight.fm123);
end;

class operator TMultivector.=(const ALeft: TMultivector; const ARight: double): boolean;
begin
  result := not (ALeft <> ARight);
end;

class operator TMultivector.=(const ALeft: double; const ARight: TMultivector): boolean;
begin
  result := not (ALeft <> ARight);
end;

class operator TMultivector.=(const ALeft, ARight: TMultivector): boolean;
begin
  result := not (ALeft <> ARight);
end;

class operator TMultivector.+(const ALeft: TMultivector; const ARight: double): TMultivector;
begin
  result.fm0   := ALeft.fm0 + ARight;
  result.fm1   := ALeft.fm1;
  result.fm2   := ALeft.fm2;
  result.fm3   := ALeft.fm3;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := ALeft.fm123;
end;

class operator TMultivector.+(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result.fm0   := ARight.fm0 + ALeft;
  result.fm1   := ARight.fm1;
  result.fm2   := ARight.fm2;
  result.fm3   := ARight.fm3;
  result.fm12  := ARight.fm12;
  result.fm23  := ARight.fm23;
  result.fm31  := ARight.fm31;
  result.fm123 := ARight.fm123;
end;

class operator TMultivector.+(const ALeft, ARight: TMultivector): TMultivector;
begin
  result.fm0   := ALeft.fm0   + ARight.fm0;
  result.fm1   := ALeft.fm1   + ARight.fm1;
  result.fm2   := ALeft.fm2   + ARight.fm2;
  result.fm3   := ALeft.fm3   + ARight.fm3;
  result.fm12  := ALeft.fm12  + ARight.fm12;
  result.fm23  := ALeft.fm23  + ARight.fm23;
  result.fm31  := ALeft.fm31  + ARight.fm31;
  result.fm123 := ALeft.fm123 + ARight.fm123;
end;

class operator TMultivector.-(const ASelf: TMultivector): TMultivector;
begin
  result.fm0   := -ASelf.fm0;
  result.fm1   := -ASelf.fm1;
  result.fm2   := -ASelf.fm2;
  result.fm3   := -ASelf.fm3;
  result.fm12  := -ASelf.fm12;
  result.fm23  := -ASelf.fm23;
  result.fm31  := -ASelf.fm31;
  result.fm123 := -ASelf.fm123;
end;

class operator TMultivector.-(const ALeft: TMultivector; const ARight: double): TMultivector;
begin
  result.fm0   := ALeft.fm0 - ARight;
  result.fm1   := ALeft.fm1;
  result.fm2   := ALeft.fm2;
  result.fm3   := ALeft.fm3;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := ALeft.fm123;
end;

class operator TMultivector.-(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result.fm0   := ALeft - ARight.fm0;
  result.fm1   :=       - ARight.fm1;
  result.fm2   :=       - ARight.fm2;
  result.fm3   :=       - ARight.fm3;
  result.fm12  :=       - ARight.fm12;
  result.fm23  :=       - ARight.fm23;
  result.fm31  :=       - ARight.fm31;
  result.fm123 :=       - ARight.fm123;
end;

class operator TMultivector.-(const ALeft, ARight: TMultivector): TMultivector;
begin
  result.fm0   := ALeft.fm0   - ARight.fm0;
  result.fm1   := ALeft.fm1   - ARight.fm1;
  result.fm2   := ALeft.fm2   - ARight.fm2;
  result.fm3   := ALeft.fm3   - ARight.fm3;
  result.fm12  := ALeft.fm12  - ARight.fm12;
  result.fm23  := ALeft.fm23  - ARight.fm23;
  result.fm31  := ALeft.fm31  - ARight.fm31;
  result.fm123 := ALeft.fm123 - ARight.fm123;
end;

class operator TMultivector.*(const ALeft: TMultivector; const ARight: double): TMultivector;
begin
  result.fm0   := ALeft.fm0   * ARight;
  result.fm1   := ALeft.fm1   * ARight;
  result.fm2   := ALeft.fm2   * ARight;
  result.fm3   := ALeft.fm3   * ARight;
  result.fm12  := ALeft.fm12  * ARight;
  result.fm23  := ALeft.fm23  * ARight;
  result.fm31  := ALeft.fm31  * ARight;
  result.fm123 := ALeft.fm123 * ARight;
end;

class operator TMultivector.*(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result.fm0   := ALeft * ARight.fm0;
  result.fm1   := ALeft * ARight.fm1;
  result.fm2   := ALeft * ARight.fm2;
  result.fm3   := ALeft * ARight.fm3;
  result.fm12  := ALeft * ARight.fm12;
  result.fm23  := ALeft * ARight.fm23;
  result.fm31  := ALeft * ARight.fm31;
  result.fm123 := ALeft * ARight.fm123;
end;

class operator TMultivector.*(const ALeft, ARight: TMultivector): TMultivector;
begin
  result.fm0 :=     ALeft.fm0   * ARight.fm0
                  + ALeft.fm1   * ARight.fm1
                  + ALeft.fm2   * ARight.fm2
                  + ALeft.fm3   * ARight.fm3
                  - ALeft.fm12  * ARight.fm12
                  - ALeft.fm23  * ARight.fm23
                  - ALeft.fm31  * ARight.fm31
                  - ALeft.fm123 * ARight.fm123;

  result.fm1 :=     ALeft.fm0   * ARight.fm1
                  + ALeft.fm1   * ARight.fm0
                  - ALeft.fm2   * ARight.fm12
                  + ALeft.fm3   * ARight.fm31
                  + ALeft.fm12  * ARight.fm2
                  - ALeft.fm23  * ARight.fm123
                  - ALeft.fm31  * ARight.fm3
                  - ALeft.fm123 * ARight.fm23;

  result.fm2 :=     ALeft.fm0   * ARight.fm2
                  + ALeft.fm1   * ARight.fm12
                  + ALeft.fm2   * ARight.fm0
                  - ALeft.fm3   * ARight.fm23
                  - ALeft.fm12  * ARight.fm1
                  + ALeft.fm23  * ARight.fm3
                  - ALeft.fm31  * ARight.fm123
                  - ALeft.fm123 * ARight.fm31;

  result.fm3 :=     ALeft.fm0   * ARight.fm3
                  - ALeft.fm1   * ARight.fm31
                  + ALeft.fm2   * ARight.fm23
                  + ALeft.fm3   * ARight.fm0
                  - ALeft.fm12  * ARight.fm123
                  - ALeft.fm23  * ARight.fm2
                  + ALeft.fm31  * ARight.fm1
                  - ALeft.fm123 * ARight.fm12;

  result.fm12 :=    ALeft.fm0   * ARight.fm12
                  + ALeft.fm1   * ARight.fm2
                  - ALeft.fm2   * ARight.fm1
                  + ALeft.fm3   * ARight.fm123
                  + ALeft.fm12  * ARight.fm0
                  - ALeft.fm23  * ARight.fm31
                  + ALeft.fm31  * ARight.fm23
                  + ALeft.fm123 * ARight.fm3;

  result.fm23 :=    ALeft.fm0   * ARight.fm23
                  + ALeft.fm1   * ARight.fm123
                  + ALeft.fm2   * ARight.fm3
                  - ALeft.fm3   * ARight.fm2
                  + ALeft.fm12  * ARight.fm31
                  + ALeft.fm23  * ARight.fm0
                  - ALeft.fm31  * ARight.fm12
                  + ALeft.fm123 * ARight.fm1;

  result.fm31 :=    ALeft.fm0   * ARight.fm31
                  - ALeft.fm1   * ARight.fm3
                  + ALeft.fm2   * ARight.fm123
                  + ALeft.fm3   * ARight.fm1
                  - ALeft.fm12  * ARight.fm23
                  + ALeft.fm23  * ARight.fm12
                  + ALeft.fm31  * ARight.fm0
                  + ALeft.fm123 * ARight.fm2;

  result.fm123 := + ALeft.fm0   * ARight.fm123
                  + ALeft.fm1   * ARight.fm23
                  + ALeft.fm2   * ARight.fm31
                  + ALeft.fm3   * ARight.fm12
                  + ALeft.fm12  * ARight.fm3
                  + ALeft.fm23  * ARight.fm1
                  + ALeft.fm31  * ARight.fm2
                  + ALeft.fm123 * ARight.fm0;
end;

class operator TMultivector./(const ALeft: TMultivector; const ARight: double): TMultivector;
begin
  result.fm0   := ALeft.fm0   / ARight;
  result.fm1   := ALeft.fm1   / ARight;
  result.fm2   := ALeft.fm2   / ARight;
  result.fm3   := ALeft.fm3   / ARight;
  result.fm12  := ALeft.fm12  / ARight;
  result.fm23  := ALeft.fm23  / ARight;
  result.fm31  := ALeft.fm31  / ARight;
  result.fm123 := ALeft.fm123 / ARight;
end;

class operator TMultivector./(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TMultivector./(const ALeft, ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

// TTrivector

class operator TTrivector.<>(const ALeft, ARight: TTrivector): boolean;
begin
  result := ALeft.fm123 <> ARight.fm123;
end;

class operator TTrivector.<>(const ALeft: TMultivector; const ARight: TTrivector): boolean;
begin
  result := (ALeft.fm0   <>            0) or
            (ALeft.fm1   <>            0) or
            (ALeft.fm2   <>            0) or
            (ALeft.fm3   <>            0) or
            (ALeft.fm12  <>            0) or
            (ALeft.fm23  <>            0) or
            (ALeft.fm31  <>            0) or
            (ALeft.fm123 <> ARight.fm123);
end;

class operator TTrivector.<>(const ALeft: TTrivector; const ARight: TMultivector): boolean;
begin
  result := (0           <> ARight.fm0  ) or
            (0           <> ARight.fm1  ) or
            (0           <> ARight.fm2  ) or
            (0           <> ARight.fm3  ) or
            (0           <> ARight.fm12 ) or
            (0           <> ARight.fm23 ) or
            (0           <> ARight.fm31 ) or
            (ALeft.fm123 <> ARight.fm123);
end;

class operator TTrivector.=(const ALeft: TMultivector; const ARight: TTrivector): boolean;
begin
  result := not (ALeft <> ARight);
end;

class operator TTrivector.=(const ALeft: TTrivector; const ARight: TMultivector): boolean;
begin
  result := not (ALeft <> ARight);
end;

class operator TTrivector.=(const ALeft, ARight: TTrivector): boolean;
begin
  result := not (ALeft <> ARight);
end;

class operator TTrivector.+(const ALeft, ARight: TTrivector): TTrivector;
begin
  result.fm123 := ALeft.fm123 + ARight.fm123;
end;

class operator TTrivector.+(const ALeft: TTrivector; const ARight: double): TMultivector;
begin
  result.fm0   := ARight;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := ALeft.fm123;
end;

class operator TTrivector.+(const ALeft: double; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := ALeft;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := ARight.fm123;
end;

class operator TTrivector.+(const ALeft: TMultivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := ALeft.fm0;
  result.fm1   := ALeft.fm1;
  result.fm2   := ALeft.fm2;
  result.fm3   := ALeft.fm3;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := ALeft.fm123 + ARight.fm123;
end;

class operator TTrivector.+(const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0   := ARight.fm0;
  result.fm1   := ARight.fm1;
  result.fm2   := ARight.fm2;
  result.fm3   := ARight.fm3;
  result.fm12  := ARight.fm12;
  result.fm23  := ARight.fm23;
  result.fm31  := ARight.fm31;
  result.fm123 := ARight.fm123 + ALeft.fm123;
end;

class operator TTrivector.-(const ASelf: TTrivector): TTrivector;
begin
  result.fm123 := -ASelf.fm123;
end;

class operator TTrivector.-(const ALeft, ARight: TTrivector): TTrivector;
begin
  result.fm123 := ALeft.fm123 - ARight.fm123;
end;

class operator TTrivector.-(const ALeft: TTrivector; const ARight: double): TMultivector;
begin
  result.fm0   := -ARight;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := ALeft.fm123;
end;

class operator TTrivector.-(const ALeft: double; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := ALeft;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := -ARight.fm123;
end;

class operator TTrivector.-(const ALeft: TMultivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := ALeft.fm0;
  result.fm1   := ALeft.fm1;
  result.fm2   := ALeft.fm2;
  result.fm3   := ALeft.fm3;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := ALeft.fm123 - ARight.fm123;
end;

class operator TTrivector.-(const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0   := ARight.fm0;
  result.fm1   := ARight.fm1;
  result.fm2   := ARight.fm2;
  result.fm3   := ARight.fm3;
  result.fm12  := ARight.fm12;
  result.fm23  := ARight.fm23;
  result.fm31  := ARight.fm31;
  result.fm123 := ARight.fm123 - ALeft.fm123;
end;

class operator TTrivector.*(const ALeft: double; const ARight: TTrivector): TTrivector;
begin
  result.fm123 := ALeft * ARight.fm123;
end;

class operator TTrivector.*(const ALeft: TTrivector; const ARight: double): TTrivector;
begin
  result.fm123 := ALeft.fm123 * ARight;
end;

class operator TTrivector.*(const ALeft, ARight: TTrivector): double;
begin
  result := -ALeft.fm123 * ARight.fm123;
end;

class operator TTrivector.*(const ALeft: TMultivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := -ALeft.fm123 * ARight.fm123;
  result.fm1   := -ALeft.fm23  * ARight.fm123;
  result.fm2   := -ALeft.fm31  * ARight.fm123;
  result.fm3   := -ALeft.fm12  * ARight.fm123;
  result.fm12  :=  ALeft.fm3   * ARight.fm123;
  result.fm23  :=  ALeft.fm1   * ARight.fm123;
  result.fm31  :=  ALeft.fm2   * ARight.fm123;
  result.fm123 :=  ALeft.fm0   * ARight.fm123;
end;

class operator TTrivector.*(const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0   := -ALeft.fm123 * ARight.fm123;
  result.fm1   := -ALeft.fm123 * ARight.fm23;
  result.fm2   := -ALeft.fm123 * ARight.fm31;
  result.fm3   := -ALeft.fm123 * ARight.fm12;
  result.fm12  :=  ALeft.fm123 * ARight.fm3;
  result.fm23  :=  ALeft.fm123 * ARight.fm1;
  result.fm31  := -ALeft.fm123 * ARight.fm2;
  result.fm123 :=  ALeft.fm123 * ARight.fm0;
end;

class operator TTrivector./(const ALeft, ARight: TTrivector): double;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TTrivector./(const ALeft: TTrivector; const ARight: double): TTrivector;
begin
  result.fm123 := ALeft.fm123 / ARight;
end;

class operator TTrivector./(const ALeft: double; const ARight: TTrivector): TTrivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TTrivector./(const ALeft: TMultivector; const ARight: TTrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TTrivector./(const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

// TBivector

class operator TBivector.<>(const ALeft, ARight: TBivector): boolean;
begin
  result := (ALeft.fm12 <> ARight.fm12) or
            (ALeft.fm23 <> ARight.fm23) or
            (ALeft.fm31 <> ARight.fm31);
end;

class operator TBivector.<>(const ALeft: TMultivector; const ARight: TBivector): boolean;
begin
  result := (ALeft.fm0   <> 0) or
            (ALeft.fm1   <> 0) or
            (ALeft.fm2   <> 0) or
            (ALeft.fm3   <> 0) or
            (ALeft.fm12  <> ARight.fm12) or
            (ALeft.fm23  <> ARight.fm23) or
            (ALeft.fm31  <> ARight.fm31) or
            (ALeft.fm123 <> 0);
end;

class operator TBivector.<>(const ALeft: TBivector; const ARight: TMultivector): boolean;
begin
  result := (ARight.fm0   <> 0) or
            (ARight.fm1   <> 0) or
            (ARight.fm2   <> 0) or
            (ARight.fm3   <> 0) or
            (ARight.fm12  <> ALeft.fm12) or
            (ARight.fm23  <> ALeft.fm23) or
            (ARight.fm31  <> ALeft.fm31) or
            (ARight.fm123 <> 0);
end;

class operator TBivector.=(const ALeft, ARight: TBivector): boolean;
begin
  result := not (ALeft <> ARight);
end;

class operator TBivector.=(const ALeft: TMultivector; const ARight: TBivector): boolean;
begin
  result := not (ALeft <> ARight);
end;

class operator TBivector.=(const ALeft: TBivector; const ARight: TMultivector): boolean;
begin
  result := not (ARight = ALeft);
end;

class operator TBivector.+(const ALeft, ARight: TBivector): TBivector;
begin
  result.fm12 := ALeft.fm12 + ARight.fm12;
  result.fm23 := ALeft.fm23 + ARight.fm23;
  result.fm31 := ALeft.fm31 + ARight.fm31;
end;

class operator TBivector.+(const ALeft: TBivector; const ARight: double): TMultivector;
begin
  result.fm0   := ARight;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := 0;
end;

class operator TBivector.+(const ALeft: double; const ARight: TBivector): TMultivector;
begin
  result := ARight + ALeft;
end;

class operator TBivector.+(const ALeft: TBivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := ARight.fm123;
end;

class operator TBivector.+(const ALeft: TTrivector; const ARight: TBivector): TMultivector;
begin
  result := ARight + ALeft;
end;

class operator TBivector.+(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0   := ARight.fm0;
  result.fm1   := ARight.fm1;
  result.fm2   := ARight.fm2;
  result.fm3   := ARight.fm3;
  result.fm12  := ARight.fm12 + ALeft.fm12;
  result.fm23  := ARight.fm23 + ALeft.fm23;
  result.fm31  := ARight.fm31 + ALeft.fm31;
  result.fm123 := ARight.fm123;
end;

class operator TBivector.+(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
begin
  result := ARight + ALeft;
end;

class operator TBivector.-(const ASelf: TBivector): TBivector;
begin
  result.fm12 := -ASelf.fm12;
  result.fm23 := -ASelf.fm23;
  result.fm31 := -ASelf.fm31;
end;

class operator TBivector.-(const ALeft, ARight: TBivector): TBivector;
begin
  result.fm12  := ALeft.fm12 - ARight.fm12;
  result.fm23  := ALeft.fm23 - ARight.fm23;
  result.fm31  := ALeft.fm31 - ARight.fm31;
end;

class operator TBivector.-(const ALeft: TBivector; const ARight: double): TMultivector;
begin
  result.fm0   := -ARight;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := 0;
end;

class operator TBivector.-(const ALeft: double; const ARight: TBivector): TMultivector;
begin
  result := ARight - ALeft;
end;

class operator TBivector.-(const ALeft: TBivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := -ARight.fm123;
end;

class operator TBivector.-(const ALeft: TTrivector; const ARight: TBivector): TMultivector;
begin
  result := ARight - ALeft;
end;

class operator TBivector.-(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0   := -ARight.fm0;
  result.fm1   := -ARight.fm1;
  result.fm2   := -ARight.fm2;
  result.fm3   := -ARight.fm3;
  result.fm12  := ALeft.fm12 - ARight.fm12;
  result.fm23  := ALeft.fm23 - ARight.fm23;
  result.fm31  := ALeft.fm31 - ARight.fm31;
  result.fm123 := -ARight.fm123;
end;

class operator TBivector.-(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
begin
  result := ARight - ALeft;
end;

class operator TBivector.*(const ALeft: double; const ARight: TBivector): TBivector;
begin
  result.fm12 := ALeft * ARight.fm12;
  result.fm23 := ALeft * ARight.fm23;
  result.fm31 := ALeft * ARight.fm31;
end;

class operator TBivector.*(const ALeft: TBivector; const ARight: double): TBivector;
begin
  result := ARight * ALeft;
end;

class operator TBivector.*(const ALeft, ARight: TBivector): TMultivector;
begin
  result.fm0   := - ALeft.fm12 * ARight.fm12
                  - ALeft.fm23 * ARight.fm23
                  - ALeft.fm31 * ARight.fm31;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  :=   ALeft.fm31 * ARight.fm23
                  - ALeft.fm23 * ARight.fm31;
  result.fm23  :=   ALeft.fm12 * ARight.fm31
                  - ALeft.fm31 * ARight.fm12;
  result.fm31  :=   ALeft.fm23 * ARight.fm12
                  - ALeft.fm12 * ARight.fm23;
  result.fm123 := 0;
end;

class operator TBivector.*(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0 :=   -ALeft.fm12  * ARight.fm12
                  -ALeft.fm23  * ARight.fm23
                  -ALeft.fm31  * ARight.fm31;

  result.fm1 :=    ALeft.fm12  * ARight.fm2
                  -ALeft.fm23  * ARight.fm123
                  -ALeft.fm31  * ARight.fm3;

  result.fm2 :=   -ALeft.fm12  * ARight.fm1
                  +ALeft.fm23  * ARight.fm3
                  -ALeft.fm31  * ARight.fm123;

  result.fm3 :=   -ALeft.fm12  * ARight.fm123
                  -ALeft.fm23  * ARight.fm2
                  +ALeft.fm31  * ARight.fm1;

  result.fm12 :=   ALeft.fm12  * ARight.fm0
                  -ALeft.fm23  * ARight.fm31
                  +ALeft.fm31  * ARight.fm23;

  result.fm23 :=   ALeft.fm12  * ARight.fm31
                  +ALeft.fm23  * ARight.fm0
                  -ALeft.fm31  * ARight.fm12;

  result.fm31 :=  -ALeft.fm12  * ARight.fm23
                  +ALeft.fm23  * ARight.fm12
                  +ALeft.fm31  * ARight.fm0;

  result.fm123 :=  ALeft.fm12  * ARight.fm3
                  +ALeft.fm23  * ARight.fm1
                  +ALeft.fm31  * ARight.fm2;
end;

class operator TBivector.*(const ALeft: TBivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := -ALeft.fm23 * ARight.fm123;
  result.fm2   := -ALeft.fm31 * ARight.fm123;
  result.fm3   := -ALeft.fm12 * ARight.fm123;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := 0;
end;

class operator TBivector.*(const ALeft: TTrivector; const ARight: TBivector): TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := -ARight.fm23 * ALeft.fm123;
  result.fm2   := -ARight.fm31 * ALeft.fm123;
  result.fm3   := -ARight.fm12 * ALeft.fm123;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := 0;
end;

class operator TBivector.*(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
begin
  result.fm0 :=   -ALeft.fm12  * ARight.fm12
                  -ALeft.fm23  * ARight.fm23
                  -ALeft.fm31  * ARight.fm31;

  result.fm1 :=   -ALeft.fm2   * ARight.fm12
                  +ALeft.fm3   * ARight.fm31
                  -ALeft.fm123 * ARight.fm23;

  result.fm2 :=    ALeft.fm1   * ARight.fm12
                  -ALeft.fm3   * ARight.fm23
                  -ALeft.fm123 * ARight.fm31;

  result.fm3 :=   -ALeft.fm1   * ARight.fm31
                  +ALeft.fm2   * ARight.fm23
                  -ALeft.fm123 * ARight.fm12;

  result.fm12 :=   ALeft.fm0   * ARight.fm12
                  -ALeft.fm23  * ARight.fm31
                  +ALeft.fm31  * ARight.fm23;

  result.fm23 :=  -ALeft.fm31  * ARight.fm12
                  +ALeft.fm0   * ARight.fm23
                  +ALeft.fm12  * ARight.fm31;

  result.fm31 :=  +ALeft.fm23  * ARight.fm12
                  -ALeft.fm12  * ARight.fm23
                  +ALeft.fm0   * ARight.fm31;

  result.fm123 :=  ALeft.fm3   * ARight.fm12
                  +ALeft.fm1   * ARight.fm23
                  +ALeft.fm2   * ARight.fm31;
end;

class operator TBivector./(const ALeft, ARight: TBivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TBivector; const ARight: double): TBivector;
begin
  result.fm12 := ALeft.fm12 / ARight;
  result.fm23 := ALeft.fm23 / ARight;
  result.fm31 := ALeft.fm31 / ARight;
end;

class operator TBivector./(const ALeft: double; const ARight: TBivector): TBivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TBivector; const ARight: TTrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TTrivector; const ARight: TBivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

// TVector

class operator TVector.<>(const ALeft, ARight: TVector): boolean;
begin
  result := (ALeft.fm1 <> ARight.fm1) or
            (ALeft.fm2 <> ARight.fm2) or
            (ALeft.fm3 <> ARight.fm3);
end;

class operator TVector.<>(const ALeft: TMultivector; const ARight: TVector): boolean;
begin
  result := (ALeft.fm0   <> 0) or
            (ALeft.fm1   <> ARight.fm1) or
            (ALeft.fm2   <> ARight.fm2) or
            (ALeft.fm3   <> ARight.fm3) or
            (ALeft.fm12  <> 0) or
            (ALeft.fm23  <> 0) or
            (ALeft.fm31  <> 0) or
            (ALeft.fm123 <> 0);
end;

class operator TVector.<>(const ALeft: TVector; const ARight: TMultivector): boolean;
begin
  result := ARight <> ALeft;
end;

class operator TVector.=(const ALeft, ARight: TVector): boolean;
begin
  result := not (ARight <> ALeft);
end;

class operator TVector.=(const ALeft: TVector; const ARight: TMultivector): boolean;
begin
  result := not (ARight <> ALeft);
end;

class operator TVector.=(const ALeft: TMultivector; const ARight: TVector): boolean;
begin
  result := not (ARight <> ALeft);
end;

class operator TVector.+(const ALeft, ARight: TVector): TVector;
begin
  result.fm1 := ALeft.fm1 + ARight.fm1;
  result.fm2 := ALeft.fm2 + ARight.fm2;
  result.fm3 := ALeft.fm3 + ARight.fm3;
end;

class operator TVector.+(const ALeft: TVector; const ARight: double): TMultivector;
begin
  result.fm0   := ARight;
  result.fm1   := ALeft.fm1;
  result.fm2   := ALeft.fm2;
  result.fm3   := ALeft.fm3;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := 0;
end;

class operator TVector.+(const ALeft: double; const ARight: TVector): TMultivector;
begin
  result.fm0   := ALeft;
  result.fm1   := ARight.fm1;
  result.fm2   := ARight.fm2;
  result.fm3   := ARight.fm3;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := 0;
end;

class operator TVector.+(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := ALeft.fm1;
  result.fm2   := ALeft.fm2;
  result.fm3   := ALeft.fm3;
  result.fm12  := ARight.fm12;
  result.fm23  := ARight.fm23;
  result.fm31  := ARight.fm31;
  result.fm123 := 0;
end;

class operator TVector.+(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := ARight.fm1;
  result.fm2   := ARight.fm2;
  result.fm3   := ARight.fm3;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := 0;
end;

class operator TVector.+(const ALeft: TVector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := ALeft.fm1;
  result.fm2   := ALeft.fm2;
  result.fm3   := ALeft.fm3;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := ARight.fm123;
end;

class operator TVector.+(const ALeft: TTrivector; const ARight: TVector): TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := ARight.fm1;
  result.fm2   := ARight.fm2;
  result.fm3   := ARight.fm3;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := ALeft.fm123;
end;


class operator TVector.+(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result.fm0   := ARight.fm0;
  result.fm1   := ARight.fm1 + ALeft.fm1;
  result.fm2   := ARight.fm2 + ALeft.fm2;
  result.fm3   := ARight.fm3 + ALeft.fm3;
  result.fm12  := ARight.fm12;
  result.fm23  := ARight.fm23;
  result.fm31  := ARight.fm31;
  result.fm123 := ARight.fm123;
end;

class operator TVector.+(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result.fm0   := ALeft.fm0;
  result.fm1   := ALeft.fm1 + ARight.fm1;
  result.fm2   := ALeft.fm2 + ARight.fm2;
  result.fm3   := ALeft.fm3 + ARight.fm3;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := ALeft.fm123;
end;

class operator TVector.-(const ASelf: TVector): TVector;
begin
  result.fm1 := -ASelf.fm1;
  result.fm2 := -ASelf.fm2;
  result.fm3 := -ASelf.fm3;
end;

class operator TVector.-(const ALeft, ARight: TVector): TVector;
begin
  result.fm1 := ALeft.fm1 - ARight.fm1;
  result.fm2 := ALeft.fm2 - ARight.fm2;
  result.fm3 := ALeft.fm3 - ARight.fm3;
end;

class operator TVector.-(const ALeft: TVector; const ARight: double): TMultivector;
begin
  result.fm0   := -ARight;
  result.fm1   :=  ALeft.fm1;
  result.fm2   :=  ALeft.fm2;
  result.fm3   :=  ALeft.fm3;
  result.fm12  :=  0;
  result.fm23  :=  0;
  result.fm31  :=  0;
  result.fm123 :=  0;
end;

class operator TVector.-(const ALeft: double; const ARight: TVector): TMultivector;
begin
  result.fm0   :=  ALeft;
  result.fm1   := -ARight.fm1;
  result.fm2   := -ARight.fm2;
  result.fm3   := -ARight.fm3;
  result.fm12  :=  0;
  result.fm23  :=  0;
  result.fm31  :=  0;
  result.fm123 :=  0;
end;

class operator TVector.-(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fm0   :=  0;
  result.fm1   :=  ALeft.fm1;
  result.fm2   :=  ALeft.fm2;
  result.fm3   :=  ALeft.fm3;
  result.fm12  := -ARight.fm12;
  result.fm23  := -ARight.fm23;
  result.fm31  := -ARight.fm31;
  result.fm123 :=  0;
end;

class operator TVector.-(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fm0   :=  0;
  result.fm1   := -ARight.fm1;
  result.fm2   := -ARight.fm2;
  result.fm3   := -ARight.fm3;
  result.fm12  :=  ALeft.fm12;
  result.fm23  :=  ALeft.fm23;
  result.fm31  :=  ALeft.fm31;
  result.fm123 :=  0;
end;

class operator TVector.-(const ALeft: TVector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   :=  0;
  result.fm1   :=  ALeft.fm1;
  result.fm2   :=  ALeft.fm2;
  result.fm3   :=  ALeft.fm3;
  result.fm12  :=  0;
  result.fm23  :=  0;
  result.fm31  :=  0;
  result.fm123 := -ARight.fm123;
end;

class operator TVector.-(const ALeft: TTrivector; const ARight: TVector): TMultivector;
begin
  result.fm0   :=  0;
  result.fm1   := -ARight.fm1;
  result.fm2   := -ARight.fm2;
  result.fm3   := -ARight.fm3;
  result.fm12  :=  0;
  result.fm23  :=  0;
  result.fm31  :=  0;
  result.fm123 :=  ALeft.fm123;
end;

class operator TVector.-(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result.fm0   := -ARight.fm0;
  result.fm1   :=  ALeft.fm1 - ARight.fm1;
  result.fm2   :=  ALeft.fm2 - ARight.fm2;
  result.fm3   :=  ALeft.fm3 - ARight.fm3;
  result.fm12  := -ARight.fm12;
  result.fm23  := -ARight.fm23;
  result.fm31  := -ARight.fm31;
  result.fm123 := -ARight.fm123;
end;

class operator TVector.-(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result.fm0   := ALeft.fm0;
  result.fm1   := ALeft.fm1 - ARight.fm1;
  result.fm2   := ALeft.fm2 - ARight.fm2;
  result.fm3   := ALeft.fm3 - ARight.fm3;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := ALeft.fm123;
end;

class operator TVector.*(const ALeft: double; const ARight: TVector): TVector;
begin
  result.fm1 := ALeft * ARight.fm1;
  result.fm2 := ALeft * ARight.fm2;
  result.fm3 := ALeft * ARight.fm3;
end;

class operator TVector.*(const ALeft: TVector; const ARight: double): TVector;
begin
  result.fm1 := ALeft.fm1 * ARight;
  result.fm2 := ALeft.fm2 * ARight;
  result.fm3 := ALeft.fm3 * ARight;
end;

class operator TVector.*(const ALeft, ARight: TVector): TMultivector;
begin
  result.fm0   := ALeft.fm1 * ARight.fm1 + ALeft.fm2 * ARight.fm2 + ALeft.fm3 * ARight.fm3;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  := ALeft.fm1 * ARight.fm2 - ALeft.fm2 * ARight.fm1;
  result.fm23  := ALeft.fm2 * ARight.fm3 - ALeft.fm3 * ARight.fm2;
  result.fm31  := ALeft.fm3 * ARight.fm1 - ALeft.fm1 * ARight.fm3;
  result.fm123 := 0;
end;

class operator TVector.*(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fm0   :=  0;
  result.fm1   :=  ALeft.fm3 * ARight.fm31 - ALeft.fm2 * ARight.fm12;
  result.fm2   :=  ALeft.fm1 * ARight.fm12 - ALeft.fm3 * ARight.fm23;
  result.fm3   :=  ALeft.fm2 * ARight.fm23 - ALeft.fm1 * ARight.fm31;
  result.fm12  :=  0;
  result.fm23  :=  0;
  result.fm31  :=  0;
  result.fm123 :=  ALeft.fm1 * ARight.fm23 + ALeft.fm2 * ARight.fm31 + ALeft.fm3 * ARight.fm12;
end;

class operator TVector.*(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := ALeft.fm12 * ARight.fm2 - ALeft.fm31 * ARight.fm3;
  result.fm2   := ALeft.fm23 * ARight.fm3 - ALeft.fm12 * ARight.fm1;
  result.fm3   := ALeft.fm31 * ARight.fm1 - ALeft.fm23 * ARight.fm2;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := ALeft.fm12 * ARight.fm3 + ALeft.fm23 * ARight.fm1 + ALeft.fm31 * ARight.fm2;
end;

class operator TVector.*(const ALeft: TVector; const ARight: TTrivector): TBivector;
begin
  result.fm12 := ALeft.fm3 * ARight.fm123;
  result.fm23 := ALeft.fm1 * ARight.fm123;
  result.fm31 := ALeft.fm2 * ARight.fm123;
end;

class operator TVector.*(const ALeft: TTrivector; const ARight: TVector): TBivector;
begin
  result.fm12 := ALeft.fm123 * ARight.fm3;
  result.fm23 := ALeft.fm123 * ARight.fm1;
  result.fm31 := ALeft.fm123 * ARight.fm2;
end;

class operator TVector.*(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result.fm0   :=   ALeft.fm1 * ARight.fm1
                  + ALeft.fm2 * ARight.fm2
                  + ALeft.fm3 * ARight.fm3;

  result.fm1   :=   ALeft.fm1 * ARight.fm0
                  - ALeft.fm2 * ARight.fm12
                  + ALeft.fm3 * ARight.fm31;

  result.fm2   :=   ALeft.fm1 * ARight.fm12
                  + ALeft.fm2 * ARight.fm0
                  - ALeft.fm3 * ARight.fm23;

  result.fm3   :=   ALeft.fm3 * ARight.fm0
                  - ALeft.fm1 * ARight.fm31
                  + ALeft.fm2 * ARight.fm23;

  result.fm12  :=   ALeft.fm1 * ARight.fm2
                  - ALeft.fm2 * ARight.fm1
                  + ALeft.fm3 * ARight.fm123;

  result.fm23  :=   ALeft.fm1 * ARight.fm123
                  + ALeft.fm2 * ARight.fm3
                  - ALeft.fm3 * ARight.fm2;

  result.fm31  :=   ALeft.fm3 * ARight.fm1
                  - ALeft.fm1 * ARight.fm3
                  + ALeft.fm2 * ARight.fm123;

  result.fm123 :=   ALeft.fm1 * ARight.fm23
                  + ALeft.fm2 * ARight.fm31
                  + ALeft.fm3 * ARight.fm12;
end;

class operator TVector.*(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result.fm0   :=   ALeft.fm1   * ARight.fm1
                  + ALeft.fm2   * ARight.fm2
                  + ALeft.fm3   * ARight.fm3;

  result.fm1   :=   ALeft.fm0   * ARight.fm1
                  + ALeft.fm12  * ARight.fm2
                  - ALeft.fm31  * ARight.fm3;

  result.fm2   :=   ALeft.fm0   * ARight.fm2
                  - ALeft.fm12  * ARight.fm1
                  + ALeft.fm23  * ARight.fm3;

  result.fm3   :=   ALeft.fm0   * ARight.fm3
                  - ALeft.fm23  * ARight.fm2
                  + ALeft.fm31  * ARight.fm1;

  result.fm12  :=   ALeft.fm1   * ARight.fm2
                  - ALeft.fm2   * ARight.fm1
                  + ALeft.fm123 * ARight.fm3;

  result.fm23  :=   ALeft.fm2   * ARight.fm3
                  - ALeft.fm3   * ARight.fm2
                  + ALeft.fm123 * ARight.fm1;

  result.fm31  :=   ALeft.fm123 * ARight.fm2
                  - ALeft.fm1   * ARight.fm3
                  + ALeft.fm3   * ARight.fm1;

  result.fm123 :=   ALeft.fm12  * ARight.fm3
                  + ALeft.fm23  * ARight.fm1
                  + ALeft.fm31  * ARight.fm2;
end;


class operator TVector./(const ALeft, ARight: TVector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./ (const ALeft: TVector; const ARight: double): TVector;
begin
  result.fm1 := ALeft.fm1 / ARight;
  result.fm2 := ALeft.fm2 / ARight;
  result.fm3 := ALeft.fm3 / ARight;
end;

class operator TVector./(const ALeft: double; const ARight: TVector): TVector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TVector; const ARight: TTrivector): TBivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TTrivector; const ARight: TVector): TBivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

// TMultivectorHelper

function TMultivectorHelper.Dual: TMultivector; // Self * -e123
begin
  result.fm0   := -fm123;
  result.fm1   := -fm23;
  result.fm2   := -fm31;
  result.fm3   := -fm12;
  result.fm12  :=  fm3;
  result.fm23  :=  fm1;
  result.fm31  :=  fm2;
  result.fm123 :=  fm0;
end;

function TMultivectorHelper.Inverse: TMultivector; // inversion
begin
  result.fm0   :=  fm0;
  result.fm1   := -fm1;
  result.fm2   := -fm2;
  result.fm3   := -fm3;
  result.fm12  :=  fm12;
  result.fm23  :=  fm23;
  result.fm31  :=  fm31;
  result.fm123 := -fm123;
end;

function TMultivectorHelper.Reverse: TMultivector; // reversion
begin
  result.fm0   :=  fm0;
  result.fm1   :=  fm1;
  result.fm2   :=  fm2;
  result.fm3   :=  fm3;
  result.fm12  := -fm12;
  result.fm23  := -fm23;
  result.fm31  := -fm31;
  result.fm123 := -fm123;
end;

function TMultivectorHelper.Conjugate: TMultivector; // conjugate
begin
  result.fm0   :=  fm0;
  result.fm1   := -fm1;
  result.fm2   := -fm2;
  result.fm3   := -fm3;
  result.fm12  := -fm12;
  result.fm23  := -fm23;
  result.fm31  := -fm31;
  result.fm123 :=  fm123;
end;

function TMultivectorHelper.Reciprocal: TMultivector;
var
  Numerator: TMultivector;
begin
  Numerator := Conjugate*Inverse*Reverse;
  result    := Numerator / (Self*Numerator).ExtractScalar;
end;

function TMultivectorHelper.Norm(AGrade: TMultivectorGrade): double;
begin
  case AGrade of
    gScalar    : result := abs(fm0);
    gVector    : result := sqrt(sqr(fm1)  + sqr(fm2)  + sqr(fm3));
    gBivector  : result := sqrt(sqr(fm12) + sqr(fm23) + sqr(fm31));
    gTrivector : result := abs(fm123);
  end;
end;

function TMultivectorHelper.SquaredNorm: TMultivector;
begin
  result := Self * Reverse;
end;

function TMultivectorHelper.Dot(const AVector: TVector): TMultivector;
begin
  result.fm0   :=  fm1   * AVector.fm1
                  +fm2   * AVector.fm2
                  +fm3   * AVector.fm3;

  result.fm1   :=  fm0   * AVector.fm1
                  +fm12  * AVector.fm2
                  -fm31  * AVector.fm3;

  result.fm2   :=  fm0   * AVector.fm2
                  -fm12  * AVector.fm1
                  +fm23  * AVector.fm3;

  result.fm3   :=  fm0   * AVector.fm3
                  -fm23  * AVector.fm2
                  +fm31  * AVector.fm1;

  result.fm12  :=  fm123 * AVector.fm3;
  result.fm23  :=  fm123 * AVector.fm1;
  result.fm31  :=  fm123 * AVector.fm2;
  result.fm123 :=  0;
end;

function TMultivectorHelper.Dot(const AVector: TBivector): TMultivector;
begin
  result.fm0   := -fm12  * AVector.fm12
                  -fm23  * AVector.fm23
                  -fm31  * AVector.fm31;

  result.fm1   := -fm2   * AVector.fm12
                  +fm3   * AVector.fm31
                  -fm123 * AVector.fm23;

  result.fm2   :=  fm1   * AVector.fm12
                  -fm3   * AVector.fm23
                  -fm123 * AVector.fm31;

  result.fm3   := -fm1   * AVector.fm31
                  +fm2   * AVector.fm23
                  -fm123 * AVector.fm12;

  result.fm12  :=  fm0   * AVector.fm12;
  result.fm23  :=  fm0   * AVector.fm23;
  result.fm31  :=  fm0   * AVector.fm31;
  result.fm123 :=  0;
end;

function TMultivectorHelper.Dot(const AVector: TTrivector): TMultivector;
begin
  result.fm0   := -fm123 * AVector.fm123;
  result.fm1   := -fm23  * AVector.fm123;
  result.fm2   := -fm31  * AVector.fm123;
  result.fm3   := -fm12  * AVector.fm123;
  result.fm12  :=  fm3   * AVector.fm123;
  result.fm23  :=  fm1   * AVector.fm123;
  result.fm31  :=  fm2   * AVector.fm123;
  result.fm123 :=  fm0   * AVector.fm123;
end;

function TMultivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0 :=     fm0   * AVector.fm0
                  + fm1   * AVector.fm1
                  + fm2   * AVector.fm2
                  + fm3   * AVector.fm3
                  - fm12  * AVector.fm12
                  - fm23  * AVector.fm23
                  - fm31  * AVector.fm31
                  - fm123 * AVector.fm123;

  result.fm1 :=     fm0   * AVector.fm1
                  + fm1   * AVector.fm0
                  - fm2   * AVector.fm12
                  + fm3   * AVector.fm31
                  + fm12  * AVector.fm2
                  - fm23  * AVector.fm123
                  - fm31  * AVector.fm3
                  - fm123 * AVector.fm23;

  result.fm2 :=     fm0   * AVector.fm2
                  + fm1   * AVector.fm12
                  + fm2   * AVector.fm0
                  - fm3   * AVector.fm23
                  - fm12  * AVector.fm1
                  + fm23  * AVector.fm3
                  - fm31  * AVector.fm123
                  - fm123 * AVector.fm31;

  result.fm3 :=     fm0   * AVector.fm3
                  - fm1   * AVector.fm31
                  + fm2   * AVector.fm23
                  + fm3   * AVector.fm0
                  - fm12  * AVector.fm123
                  - fm23  * AVector.fm2
                  + fm31  * AVector.fm1
                  - fm123 * AVector.fm12;

  result.fm12 :=    fm0   * AVector.fm12
                  + fm3   * AVector.fm123
                  + fm12  * AVector.fm0
                  + fm123 * AVector.fm3;

  result.fm23 :=    fm0   * AVector.fm23
                  + fm1   * Avector.fm123
                  + fm23  * AVector.fm0
                  + fm123 * AVector.fm1;

  result.fm31 :=    fm0   * AVector.fm31
                  + fm2   * AVector.fm123
                  + fm31  * AVector.fm0
                  + fm123 * AVector.fm2;

  result.fm123 :=   fm0   * AVector.fm123
                  + fm123 * AVector.fm0;
end;

function TMultivectorHelper.Wedge(const AVector: TVector): TMultivector;
begin
  result.fm0   :=  0;
  result.fm1   :=  fm0  * AVector.fm1;
  result.fm2   :=  fm0  * AVector.fm2;
  result.fm3   :=  fm0  * AVector.fm3;

  result.fm12  :=  fm1  * AVector.fm2
                  -fm2  * AVector.fm1;

  result.fm23  :=  fm2  * AVector.fm3
                  -fm3  * AVector.fm2;

  result.fm31  :=  fm3  * AVector.fm1
                  -fm1  * AVector.fm3;

  result.fm123 :=  fm12 * AVector.fm3
                  +fm23 * AVector.fm1
                  +fm31 * AVector.fm2;
end;

function TMultivectorHelper.Wedge(const AVector: TBivector): TMultivector;
begin
  result.fm0   :=  0;
  result.fm1   :=  0;
  result.fm2   :=  0;
  result.fm3   :=  0;
  result.fm12  :=  fm0 * AVector.fm12;
  result.fm23  :=  fm0 * AVector.fm23;
  result.fm31  :=  fm0 * AVector.fm31;
  result.fm123 :=  fm1 * AVector.fm23
                  +fm2 * AVector.fm31
                  +fm3 * AVector.fm12
end;

function TMultivectorHelper.Wedge(const AVector: TTrivector): TTrivector;
begin
  result.fm123 := fm0 * AVector.fm123;
end;

function TMultivectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fm0   :=   fm0   * AVector.fm0;

  result.fm1   :=   fm0   * AVector.fm1
                  + fm1   * AVector.fm0;

  result.fm2   :=   fm0   * AVector.fm2
                  + fm2   * AVector.fm0;

  result.fm3   :=   fm0   * AVector.fm3
                  + fm3   * AVector.fm0;

  result.fm12  :=   fm0   * AVector.fm12
                  + fm1   * AVector.fm2
                  - fm2   * AVector.fm1
                  + fm12  * AVector.fm0;

  result.fm23  :=   fm0   * AVector.fm23
                  + fm2   * AVector.fm3
                  - fm3   * AVector.fm2
                  + fm23  * AVector.fm0;

  result.fm31  :=   fm0   * AVector.fm31
                  - fm1   * AVector.fm3
                  + fm3   * AVector.fm1
                  + fm31  * AVector.fm0;

  result.fm123 := + fm0   * AVector.fm123
                  + fm1   * AVector.fm23
                  + fm2   * AVector.fm31
                  + fm3   * AVector.fm12
                  + fm12  * AVector.fm3
                  + fm23  * AVector.fm1
                  + fm31  * AVector.fm2
                  + fm123 * AVector.fm0;
end;

function TMultivectorHelper.Projection(const AVector: TVector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TMultivectorHelper.Projection(const AVector: TBivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TMultivectorHelper.Projection(const AVector: TTrivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TMultivectorHelper.Projection(const AVector: TMultivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TMultivectorHelper.Rejection(const AVector: TVector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TMultivectorHelper.Rejection(const AVector: TBivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TMultivectorHelper.Rejection(const AVector: TTrivector): double;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TMultivectorHelper.Rejection(const AVector: TMultivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TMultivectorHelper.Reflection(const AVector: TVector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TMultivectorHelper.Reflection(const AVector: TBivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TMultivectorHelper.Reflection(const AVector: TTrivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TMultivectorHelper.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TMultivectorHelper.Rotation(const AVector1, AVector2: TVector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TMultivectorHelper.Rotation(const AVector1, AVector2: TBivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TMultivectorHelper.Rotation(const AVector1, AVector2: TTrivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TMultivectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TMultivectorHelper.SameValue(const AVector: TMultivector): boolean;
begin
  result := Math.SameValue(fm0,   Avector.fm0  ) and
            Math.SameValue(fm1,   Avector.fm1  ) and
            Math.SameValue(fm2,   Avector.fm2  ) and
            Math.SameValue(fm3,   Avector.fm3  ) and
            Math.SameValue(fm12,  Avector.fm12 ) and
            Math.SameValue(fm23,  Avector.fm23 ) and
            Math.SameValue(fm31,  Avector.fm31 ) and
            Math.SameValue(fm123, Avector.fm123);
end;

function TMultivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
begin
  result := Format('%s %se1 %se2 %se3 %se12 %se23 %se31 %se123',
    [Fmt(fm0, APrecision, ADigits),
     Fmt(fm1, APrecision, ADigits),
     Fmt(fm2, APrecision, ADigits),
     Fmt(fm3, APrecision, ADigits),
     Fmt(fm12, APrecision, ADigits),
     Fmt(fm23, APrecision, ADigits),
     Fmt(fm31, APrecision, ADigits),
     Fmt(fm123, APrecision, ADigits)]);
end;

function TMultivectorHelper.ToString: string;
begin
  result := Format('%s %se1 %se2 %se3 %se12 %se23 %se31 %se123',
    [Fmt(fm0), Fmt(fm1), Fmt(fm2), Fmt(fm3), Fmt(fm12), Fmt(fm23), Fmt(fm31), Fmt(fm123)]);
end;

function TMultivectorHelper.ExtractTrivector: TTrivector;
begin
  result.fm123 := fm123;;
end;

function TMultivectorHelper.ExtractBivector: TBivector;
begin
  result.fm12 := fm12;
  result.fm23 := fm23;
  result.fm31 := fm31;
end;

function TMultivectorHelper.ExtractVector: TVector;
begin
  result.fm1 := fm1;
  result.fm2 := fm2;
  result.fm3 := fm3;
end;

function TMultivectorHelper.ExtractScalar: double;
begin
  result := fm0;
end;

function TMultivectorHelper.IsZero: boolean;
begin
  result := SameValue(NullMultivector);
end;

function TMultivectorHelper.IsScalar: boolean;
begin
  result := Math.SameValue(fm1,   0) and
            Math.SameValue(fm2,   0) and
            Math.SameValue(fm3,   0) and
            Math.SameValue(fm12,  0) and
            Math.SameValue(fm23,  0) and
            Math.SameValue(fm31,  0) and
            Math.SameValue(fm123, 0);

  if result then
    result := not IsZero;
end;

function TMultivectorHelper.IsVector: boolean;
begin
  result := Math.SameValue(fm0,   0) and
            Math.SameValue(fm12,  0) and
            Math.SameValue(fm23,  0) and
            Math.SameValue(fm31,  0) and
            Math.SameValue(fm123, 0);

  if result then
    result := not IsZero;
end;

function TMultivectorHelper.IsBiVector: boolean;
begin
  result := Math.SameValue(fm0,   0) and
            Math.SameValue(fm1,   0) and
            Math.SameValue(fm2,   0) and
            Math.SameValue(fm3,   0) and
            Math.SameValue(fm123, 0);

  if result then
    result := not IsZero;
end;

function TMultivectorHelper.IsTrivector: boolean;
begin
  result := Math.SameValue(fm0,  0) and
            Math.SameValue(fm1,  0) and
            Math.SameValue(fm2,  0) and
            Math.SameValue(fm3,  0) and
            Math.SameValue(fm12, 0) and
            Math.SameValue(fm23, 0) and
            Math.SameValue(fm31, 0);

  if result then
    result := not IsZero;
end;

function TMultivectorHelper.IsA: string;
begin
  Result := '';

  if IsZero      then Result := Result + 'Null';
  if IsTrivector then Result := Result + 'TTrivector';
  if IsBivector  then Result := Result + 'TBivector';
  if IsVector    then Result := Result + 'TVector';
  if IsScalar    then Result := Result + 'TScalar';

  if Result = '' then Result := 'TMultivector'
end;

// TTrivectorHelper

function TTrivectorHelper.Dual: double; // Self * e123
begin
  result := -fm123;
end;

function TTrivectorHelper.Inverse: TTrivector;
begin
  result.fm123 := -fm123;
end;

function TTrivectorHelper.Reverse: TTrivector;
begin
  result.fm123 := -fm123;
end;

function TTrivectorHelper.Conjugate: TTrivector;
begin
  result.fm123 := fm123;
end;

function TTrivectorHelper.Reciprocal: TTrivector;
begin
  result := Reverse / SquaredNorm;
end;

function TTrivectorHelper.Normalized: TTrivector;
begin
  result := Self / Norm;
end;

function TTrivectorHelper.Norm: double;
begin
  result := abs(fm123);
end;

function TTrivectorHelper.SquaredNorm: double;
begin
  result := fm123 * fm123; // Self * Reverse;
end;

function TTrivectorHelper.Dot(const AVector: TVector): TBivector;
begin
  result.fm12 := fm123 * AVector.fm3;
  result.fm23 := fm123 * AVector.fm1;
  result.fm31 := fm123 * AVector.fm2;
end;

function TTrivectorHelper.Dot(const AVector: TBivector): TVector;
begin
  result.fm1 := -fm123 * AVector.fm23;
  result.fm2 := -fm123 * AVector.fm31;
  result.fm3 := -fm123 * AVector.fm12;
end;

function TTrivectorHelper.Dot(const AVector: TTrivector): double;
begin
  result := -fm123 * AVector.fm123;
end;

function TTrivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0   := -fm123 * AVector.fm123;
  result.fm1   := -fm123 * AVector.fm23;
  result.fm2   := -fm123 * AVector.fm31;
  result.fm3   := -fm123 * AVector.fm12;
  result.fm12  :=  fm123 * AVector.fm3;
  result.fm23  :=  fm123 * AVector.fm1;
  result.fm31  :=  fm123 * AVector.fm2;
  result.fm123 :=  fm123 * AVector.fm0;
end;

function TTrivectorHelper.Wedge(const AVector: TVector): double;
begin
  result := 0;
end;

function TTrivectorHelper.Wedge(const AVector: TBivector): double;
begin
  result := 0;
end;

function TTrivectorHelper.Wedge(const AVector: TTrivector): double;
begin
  result := 0;
end;

function TTrivectorHelper.Wedge(const AVector: TMultivector): TTrivector;
begin
  result.fm123 := fm123 * AVector.fm0;
end;

function TTrivectorHelper.Projection(const AVector: TVector): TTrivector;
begin
  result.fm123 := fm123 * AVector.Norm;
end;

function TTrivectorHelper.Projection(const AVector: TBivector): TTrivector;
begin
  result.fm123 := fm123 * AVector.Norm;
end;

function TTrivectorHelper.Projection(const AVector: TTrivector): TTrivector;
begin
  result.fm123 := fm123 * AVector.Norm;
end;

function TTrivectorHelper.Projection(const AVector: TMultivector): TTrivector;
begin
  result := (Dot(AVector) * AVector.Reciprocal).ExtractTrivector;
end;

function TTrivectorHelper.Rejection(const AVector: TVector): double;
begin
  result := 0;
end;

function TTrivectorHelper.Rejection(const AVector: TBivector): double;
begin
  result := 0;
end;

function TTrivectorHelper.Rejection(const AVector: TTrivector): double;
begin
  result := 0;
end;

function TTrivectorHelper.Rejection(const AVector: TMultivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Reflection(const AVector: TVector): TTrivector;
begin
  result := (AVector * Self * AVector.Reciprocal).ExtractTrivector;
end;

function TTrivectorHelper.Reflection(const AVector: TBivector): TTrivector;
begin
  result := (AVector * Self * AVector.Reciprocal).ExtractTrivector;
end;

function TTrivectorHelper.Reflection(const AVector: TTrivector): TTrivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TTrivectorHelper.Reflection(const AVector: TMultivector): TTrivector;
begin
  result := (AVector * Self * AVector.Reciprocal).ExtractTrivector;
end;

function TTrivectorHelper.Rotation(const AVector1, AVector2: TVector): TTrivector;
begin
  result := (AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal).ExtractTrivector;
end;

function TTrivectorHelper.Rotation(const AVector1, AVector2: TBivector): TTrivector;

begin
  result := (AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal).ExtractTrivector;
end;

function TTrivectorHelper.Rotation(const AVector1, AVector2: TTrivector): TTrivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TTrivectorHelper.Rotation(const AVector1, AVector2: TMultivector): TTrivector;
begin
  result := (AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal).ExtractTrivector;
end;

function TTrivectorHelper.SameValue(const AVector: TTrivector): boolean;
begin
  result := Math.SameValue(fm123, AVector.fm123);
end;

function TTrivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
begin
  result := Format('%se123', [Fmt(fm123, APrecision, ADigits)]);
end;

function TTrivectorHelper.ToString: string;
begin
  result := Format('%se123', [Fmt(fm123)]);
end;

function TTrivectorHelper.ToMultivector: TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := fm123;
end;

// TBivectorHelper

function TBivectorHelper.Dual: TVector;
begin
  result.fm1 := -fm23; // Self * e123;
  result.fm2 := -fm31;
  result.fm3 := -fm12;
end;

function TBivectorHelper.Inverse: TBivector;
begin
  result.fm12 := fm12;
  result.fm23 := fm23;
  result.fm31 := fm31;
end;

function TBivectorHelper.Conjugate: TBivector;
begin
  result.fm12 := -fm12;
  result.fm23 := -fm23;
  result.fm31 := -fm31;
end;

function TBivectorHelper.Reverse: TBivector;
begin
  result.fm12 := -fm12;
  result.fm23 := -fm23;
  result.fm31 := -fm31;
end;

function TBivectorHelper.Reciprocal: TBivector;
begin
  result := Conjugate / SquaredNorm;
end;

function TBivectorHelper.Normalized: TBivector;
begin
  result := Self / Norm;
end;

function TBivectorHelper.Norm: double;
begin
  result := sqrt(SquaredNorm);
end;

function TBivectorHelper.SquaredNorm: double;
begin
  result := fm12*fm12 + fm23*fm23 + fm31*fm31; // Self * Reverse;
end;

function TBivectorHelper.Dot(const AVector: TVector): TVector;
begin
  result.fm1 :=  fm12 * AVector.fm2
                -fm31 * AVector.fm3;
  result.fm2 :=  fm23 * AVector.fm3
                -fm12 * AVector.fm1;
  result.fm3 :=  fm31 * AVector.fm1
                -fm23 * AVector.fm2;
end;

function TBivectorHelper.Dot(const AVector: TBivector): double;
begin
  result := - fm12 * AVector.fm12
            - fm23 * AVector.fm23
            - fm31 * AVector.fm31;
end;

function TBivectorHelper.Dot(const AVector: TTrivector): TVector;
begin
  result.fm1 := -fm23 * AVector.fm123;
  result.fm2 := -fm31 * AVector.fm123;
  result.fm3 := -fm12 * AVector.fm123;
end;

function TBivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0   := - fm12 * AVector.fm12
                  - fm23 * AVector.fm23
                  - fm31 * AVector.fm31;

  result.fm1   := + fm12 * AVector.fm2
                  - fm23 * AVector.fm123
                  - fm31 * AVector.fm3;

  result.fm2   := - fm12 * AVector.fm1
                  - fm31 * AVector.fm123
                  + fm23 * AVector.fm3;

  result.fm3   := - fm12 * AVector.fm123
                  - fm23 * AVector.fm2
                  + fm31 * AVector.fm1;

  result.fm12  :=   fm12 * AVector.fm0;
  result.fm23  :=   fm23 * AVector.fm0;
  result.fm31  :=   fm31 * AVector.fm0;
  result.fm123 :=   0;
end;

function TBivectorHelper.Wedge(const AVector: TVector): TTrivector;
begin
  result.fm123 :=  fm12 * AVector.fm3
                  +fm23 * AVector.fm1
                  +fm31 * AVector.fm2;
end;

function TBivectorHelper.Wedge(const AVector: TBivector): double;
begin
  result := 0;
end;

function TBivectorHelper.Wedge(const AVector: TTrivector): double;
begin
  result := 0;
end;

function TBivectorHelper.Wedge (const AVector: TMultivector): TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  :=   fm12 * AVector.fm0;
  result.fm23  :=   fm23 * AVector.fm0;
  result.fm31  :=   fm31 * AVector.fm0;
  result.fm123 :=   fm12 * AVector.fm3
                  + fm23 * AVector.fm1
                  + fm31 * AVector.fm2;
end;

function TBivectorHelper.Projection(const AVector: TVector): TBivector;
begin
  result := (Dot(AVector) * AVector.Reciprocal).ExtractBivector;
end;

function TBivectorHelper.Projection(const AVector: TBivector): TBivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Projection(const AVector: TTrivector): TBivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Projection(const AVector: TMultivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Rejection(const AVector: TVector): TBivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Rejection (const AVector: TBivector): double;
begin
  result := 0;
end;

function TBivectorHelper.Rejection (const AVector: TTrivector): double;
begin
  result := 0;
end;

function TBivectorHelper.Rejection (const AVector: TMultivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Reflection(const AVector: TVector): TBivector;
begin
  result := (AVector * Self * AVector.Reciprocal).ExtractBivector;
end;

function TBivectorHelper.Reflection(const AVector: TBivector): TBivector;
begin
  result := (AVector * Self * AVector.Reciprocal).ExtractBivector;
end;

function TBivectorHelper.Reflection(const AVector: TTrivector): TBivector;
begin
  result := (AVector * Self * AVector.Reciprocal).ExtractBivector;
end;

function TBivectorHelper.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: TVector): TBivector;
begin
  result := (AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal).ExtractBivector;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: TBivector): TBivector;
begin
  result := (AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal).ExtractBivector;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: Ttrivector): TBivector;
begin
  result := (AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal).ExtractBivector;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TBivectorHelper.SameValue(const AVector: TBivector): boolean;
begin
  result := Math.SameValue(fm12, Avector.fm12) and
            Math.SameValue(fm23, Avector.fm23) and
            Math.SameValue(fm31, Avector.fm31);
end;

function TBivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
begin
  result := Format('%se12 %se23 %se31',
    [Fmt(fm12, APrecision, ADigits),
     Fmt(fm23, APrecision, ADigits),
     Fmt(fm31, APrecision, ADigits)]);
end;

function TBivectorHelper.ToString: string;
begin
  result := Format('%se12 %se23 %se31', [Fmt(fm12), Fmt(fm23), Fmt(fm31)]);
end;

function TBivectorHelper.ToMultivector: TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := 0;
  result.fm12  := fm12;
  result.fm23  := fm23;
  result.fm31  := fm31;
  result.fm123 := 0;
end;

// TVectorHelper

function TVectorHelper.Dual: TBivector;
begin
  result.fm12 := fm3; // Self * e123;
  result.fm23 := fm1;
  result.fm31 := fm2;
end;

function TVectorHelper.Inverse: TVector;
begin
  result.fm1 := -fm1;
  result.fm2 := -fm2;
  result.fm3 := -fm3;
end;

function TVectorHelper.Reverse: TVector;
begin
  result.fm1 := fm1;
  result.fm2 := fm2;
  result.fm3 := fm3;
end;

function TVectorHelper.Conjugate: TVector;
begin
  result.fm1 := -fm1;
  result.fm2 := -fm2;
  result.fm3 := -fm3;
end;

function TVectorHelper.Reciprocal: TVector;
begin
  result := Self / SquaredNorm;
end;

function TVectorHelper.Normalized: TVector;
begin
  result := Self / Norm;
end;

function TVectorHelper.Norm: double;
begin
  result := sqrt(SquaredNorm);
end;

function TVectorHelper.SquaredNorm: double;
begin
  result := fm1*fm1 + fm2*fm2 + fm3*fm3;
end;

function TVectorHelper.Dot(const AVector: TVector): double;
begin
 result := fm1 * AVector.fm1 + fm2 * AVector.fm2 + fm3 * AVector.fm3;
end;

function TVectorHelper.Dot(const AVector: TBivector): TVector;
begin
  result.fm1 := fm3 * AVector.fm31 - fm2 * AVector.fm12;
  result.fm2 := fm1 * AVector.fm12 - fm3 * AVector.fm23;
  result.fm3 := fm2 * AVector.fm23 - fm1 * AVector.fm31;
end;

function TVectorHelper.Dot(const AVector: TTrivector): TBivector;
begin
  result.fm12 := fm3 * AVector.fm123;
  result.fm23 := fm1 * Avector.fm123;
  result.fm31 := fm2 * AVector.fm123;
end;

function TVectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0   := fm1 * AVector.fm1 + fm2 * AVector.fm2  + fm3 * AVector.fm3;
  result.fm1   := fm1 * AVector.fm0 - fm2 * AVector.fm12 + fm3 * AVector.fm31;
  result.fm2   := fm2 * AVector.fm0 + fm1 * AVector.fm12 - fm3 * AVector.fm23;
  result.fm3   := fm3 * AVector.fm0 - fm1 * AVector.fm31 + fm2 * AVector.fm23;
  result.fm12  := fm3 * AVector.fm123;
  result.fm23  := fm1 * Avector.fm123;
  result.fm31  := fm2 * AVector.fm123;
  result.fm123 := 0;
end;

function TVectorHelper.Wedge(const AVector: TVector): TBivector;
begin
  result.fm12 := fm1 * AVector.fm2 - fm2 * AVector.fm1;
  result.fm23 := fm2 * AVector.fm3 - fm3 * AVector.fm2;
  result.fm31 := fm3 * AVector.fm1 - fm1 * AVector.fm3;
end;

function TVectorHelper.Wedge(const AVector: TBivector): TTrivector;
begin
  result.fm123 := fm1 * AVector.fm23 + fm2 * AVector.fm31 + fm3 * AVector.fm12;
end;

function TVectorHelper.Wedge(const AVector: TTrivector): double;
begin
  result := 0;
end;

function TVectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := fm1 * AVector.fm0;
  result.fm2   := fm2 * AVector.fm0;
  result.fm3   := fm3 * AVector.fm0;
  result.fm12  := fm1 * AVector.fm2  - fm2 * AVector.fm1;
  result.fm23  := fm2 * AVector.fm3  - fm3 * AVector.fm2;
  result.fm31  := fm3 * AVector.fm1  - fm1 * AVector.fm3;
  result.fm123 := fm1 * AVector.fm23 + fm2 * AVector.fm31 + fm3 * AVector.fm12;
end;

function TVectorHelper.Projection(const AVector: TVector): TVector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Projection(const AVector: TBivector): TVector;
begin
  result := (Dot(AVector) * AVector.Reciprocal).ExtractVector;
end;

function TVectorHelper.Projection(const AVector: TTrivector): TVector;
begin
  result := (Dot(AVector) * AVector.Reciprocal).ExtractVector;
end;

function TVectorHelper.Projection(const AVector: TMultivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Rejection(const AVector: TVector): TVector;
begin
  result := (Wedge(AVector) * AVector.Reciprocal).ExtractVector;
end;

function  TVectorHelper.Rejection(const AVector: TBivector): TVector;
begin
  result := (Wedge(AVector) * AVector.Reciprocal).ExtractVector;
end;

function TVectorHelper.Rejection(const AVector: TTrivector): double;
begin
  result := 0;
end;

function TVectorHelper.Rejection (const AVector: TMultivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Reflection(const AVector: TVector): TVector;
begin
  result := (AVector * Self * AVector.Reciprocal).ExtractVector;
end;

function TVectorHelper.Reflection(const AVector: TBivector): TVector;
begin
  result := (AVector * Self * AVector.Reciprocal).ExtractVector;
end;

function TVectorHelper.Reflection(const AVector: TTrivector): TVector;
begin
  result := (AVector * Self * AVector.Reciprocal).ExtractVector;
end;

function TVectorHelper.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TVector): TVector;
begin
  result := (AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal).ExtractVector;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TBivector): TVector;
begin
  result := (AVector2 * AVector1 * Self * AVector1.Reciprocal  * AVector2.Reciprocal).ExtractVector;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TTrivector): TVector;
begin
  result := (AVector2 * AVector1 * Self * AVector1.Reciprocal  * AVector2.Reciprocal).ExtractVector;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal  * AVector2.Reciprocal;
end;

function TVectorHelper.LERP(const AVector1, AVector2: TVector; const AFactor: double = 0.5): TVector;
begin
  result := AVector1 + AFactor * (AVector2 - AVector1);
end;

function TVectorHelper.SLERP(const AVector1, AVector2: TVector; const AFactor: double = 0.5): TVector;
var
  angle: double;
begin
  angle := AVector1.Dot(AVector2)/(AVector1.Norm * AVector2.Norm);
  result := sin((1-AFactor)*angle)/sin(angle) * AVector1 + sin(AFactor*angle)/sin(angle) * AVector2;
end;

function TVectorHelper.Cross(const AVector: TVector): TVector;
begin
  result.fm1 := fm2*AVector.fm3 - fm3*AVector.fm2;
  result.fm2 := fm3*AVector.fm1 - fm1*AVector.fm3;
  result.fm3 := fm1*AVector.fm2 - fm2*AVector.fm1;
end;

function TVectorHelper.SameValue(const AVector: TVector): boolean;
begin
  result := Math.SameValue(fm1, Avector.fm1) and
            Math.SameValue(fm2, Avector.fm2) and
            Math.SameValue(fm3, Avector.fm3);
end;

function TVectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
begin
  result := Format('%se1 %se2 %se3',
    [Fmt(fm1, APrecision, ADigits),
     Fmt(fm2, APrecision, ADigits),
     Fmt(fm3, APrecision, ADigits)]);
end;

function TVectorHelper.ToString: string;
begin
  result := Format('%se1 %se2 %se3', [Fmt(fm1), Fmt(fm2), Fmt(fm3)]);
end;

function TVectorHelper.ToMultivector: TMultivector;
begin
  result.fm0   := 0;
  result.fm1   := fm1;
  result.fm2   := fm2;
  result.fm3   := fm3;
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := 0;
  result.fm123 := 0;
end;

// TVersor

class operator TVersor1.*(const AValue: double; const ASelf: TVersor1): TVector;
begin
  result.fm1   := AValue;
  result.fm2   := 0;
  result.fm3   := 0;
end;

class operator TVersor2.*(const AValue: double; const ASelf: TVersor2): TVector;
begin
  result.fm1   := 0;
  result.fm2   := AValue;
  result.fm3   := 0;
end;

class operator TVersor3.*(const AValue: double; const ASelf: TVersor3): TVector;
begin
  result.fm1   := 0;
  result.fm2   := 0;
  result.fm3   := AValue;
end;

// TBiversor

class operator TBiversor12.*(const AValue: double; const ASelf: TBiversor12): TBivector;
begin
  result.fm12  := AValue;
  result.fm23  := 0;
  result.fm31  := 0;
end;

class operator TBiversor23.*(const AValue: double; const ASelf: TBiversor23): TBivector;
begin
  result.fm12  := 0;
  result.fm23  := AValue;
  result.fm31  := 0;
end;

class operator TBiversor31.*(const AValue: double; const ASelf: TBiversor31): TBivector;
begin
  result.fm12  := 0;
  result.fm23  := 0;
  result.fm31  := AValue;
end;

// TTriversor

class operator TTriversor123.*(const AValue: double; const ASelf: TTriversor123): TTrivector;
begin
  result.fm123 := AValue;
end;

end.

