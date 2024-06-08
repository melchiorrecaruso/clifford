unit cl2;

{ Geometric Algebra Cl(2) for FreePascal.

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

// References:
// - Chris Doran, Anthony Lasenby,
//   Geometric algebra for Physicists, (2003) Cambridge University Press.
// - Eckhard Hitzer, Stephen Sangwine,
//   Multivector and multivector matrix inverses in real Clifford algebras,
//   (2016) Preprint: Technical Report CES-534, ISSN: 1744-8050.
// - James M. Chappell, Azhar Iqbal, Lachlan J. Gunn, Derek Abbott,
//   Function of multivector variables, (2015) PLoS One.

{$H+}{$J-}
{$mode objfpc}{$h+}
{$modeswitch advancedrecords}
{$WARN 05024 OFF} // Suppress warning for unused routine parameter.
{$WARN 05033 OFF} // Suppress warning for unassigned function's return value.
{$MACRO ON}

interface

uses
  SysUtils;

type
  // TMultivectorComponents
  TMultivectorComponent  = (mc0, mc1, mc2, mc12);
  TMultivectorComponents = set of TMultivectorComponent;

  // TMultivector
  TMultivector = record
  private
    fm0: double;
    fm1: double;
    fm2: double;
    fm12: double;
  public
    class operator :=(const AValue: double): TMultivector;
    class operator :=(const AValue: TMultivector): double;
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
    //geometric product
    class operator * (const ALeft, ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator * (const ALeft: double; const ARight: TMultivector): TMultivector;

    class operator / (const ALeft, ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator / (const ALeft: double; const ARight: TMultivector): TMultivector;
  end;

  // TBivector
  TBivector = record
  private
    fm12: double;
  public
    class operator :=(const AValue: TBivector): TMultivector;
    class operator :=(const AValue: TMultivector): TBivector;
    class operator <>(const ALeft, ARight: TBivector): boolean;
    class operator <>(const ALeft: TBivector; const ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; const ARight: TBivector): boolean;

    class operator = (const ALeft, ARight: TBivector): boolean;
    class operator = (const ALeft: TBivector; const ARight: TMultivector): boolean;
    class operator = (const ALeft: TMultivector; const ARight: TBivector): boolean;

    class operator + (const ALeft, ARight: TBivector): TBivector;
    class operator + (const ALeft: TBivector; const ARight: double): TMultivector;
    class operator + (const ALeft: double; const ARight: TBivector): TMultivector;
    class operator + (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator + (const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    class operator - (const ASelf: TBivector): TBivector;
    class operator - (const ALeft, ARight: TBivector): TBivector;
    class operator - (const ALeft: TBivector; const ARight: double): TMultivector;
    class operator - (const ALeft: double; const ARight: TBivector): TMultivector;
    class operator - (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    class operator * (const ALeft: double; const ARight: TBivector): TBivector;
    class operator * (const ALeft: TBivector; const ARight: double): TBivector;
    // geometric product
    class operator * (const ALeft, ARight: TBivector): TMultivector;
    class operator * (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    class operator / (const ALeft, ARight: TBivector): TMultivector;
    class operator / (const ALeft: TBivector; const ARight: double): TBivector;
    class operator / (const ALeft: double; const ARight: TBivector): TBivector;
    class operator / (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: TBivector): TMultivector;
  end;

  // TVectorComponents
  TVectorComponent  = (vc1, vc2);
  TVectorComponents = set of TVectorComponent;

  // TVector
  TVector = record
  private
    fm1: double;
    fm2: double;
  public
    class operator :=(const AValue: TVector): TMultivector;
    class operator :=(const AValue: TMultivector): TVector;
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
    class operator + (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator + (const ALeft: TMultivector; const ARight: TVector): TMultivector;

    class operator - (const ASelf: TVector): TVector;
    class operator - (const ALeft, ARight: TVector): TVector;
    class operator - (const ALeft: TVector; const ARight: double): TMultivector;
    class operator - (const ALeft: double; const ARight: TVector): TMultivector;
    class operator - (const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator - (const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator - (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: TVector): TMultivector;

    class operator * (const ALeft: double; const ARight: TVector): TVector;
    class operator * (const ALeft: TVector; const ARight: double): TVector;
    // geometric product
    class operator * (const ALeft, ARight: TVector): TMultivector;
    class operator * (const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator * (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: TVector): TMultivector;

    class operator / (const ALeft: double; const ARight: TVector): TVector;
    class operator / (const ALeft: TVector; const ARight: double): TVector;
    class operator / (const ALeft, ARight: TVector): TMultivector;
    class operator / (const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator / (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: TVector): TMultivector;
  end;

  // TMultivectorHelper
  TMultivectorHelper = record helper for TMultivector
    function Dual: TMultivector;
    function Inverse: TMultivector;
    function Reverse: TMultivector;
    function Conjugate: TMultivector;
    function Reciprocal: TMultivector;
    function Normalized: TMultivector;
    function Norm: double;
    function SquaredNorm: double;

    function Dot(const AVector: TVector): TMultivector; overload;
    function Dot(const AVector: TBivector): TMultivector; overload;
    function Dot(const AVector: TMultivector): TMultivector; overload;

    function Wedge(const AVector: TVector): TMultivector; overload;
    function Wedge(const AVector: TBivector): TMultivector; overload;
    function Wedge(const AVector: TMultivector): TMultivector; overload;

    function Projection(const AVector: TVector): TMultivector; overload;
    function Projection(const AVector: TBivector): TMultivector; overload;
    function Projection(const AVector: TMultivector): TMultivector; overload;

    function Rejection(const AVector: TVector): TMultivector; overload;
    function Rejection(const AVector: TBivector): TMultivector; overload;
    function Rejection(const AVector: TMultivector): TMultivector; overload;

    function Reflection(const AVector: TVector): TMultivector; overload;
    function Reflection(const AVector: TBivector): TMultivector; overload;
    function Reflection(const AVector: TMultivector): TMultivector; overload;

    function Rotation(const AVector1, AVector2: TVector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TBivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;overload;

    function SameValue(const AValue: TMultivector): boolean;
    function SameValue(const AValue: TBivector): boolean;
    function SameValue(const AValue: TVector): boolean;
    function SameValue(const AValue: double): boolean;

    function Extract(AComponents: TMultivectorComponents): TMultivector;
    function Extract(AComponents: TVectorComponents): TVector;

    function ExtractBivector: TBivector;
    function ExtractVector: TVector;
    function ExtractScalar: double;

    function IsNull: boolean;
    function IsScalar: boolean;
    function IsVector: boolean;
    function IsBiVector: boolean;
    function IsA: string;

    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
  end;

  // TBivectorHelper
  TBivectorHelper = record helper for TBivector
    function Dual: double;
    function Inverse: TBivector;
    function Reverse: TBivector;
    function Conjugate: TBivector;
    function Reciprocal: TBivector;
    function Normalized: TBivector;
    function Norm: double;
    function SquaredNorm: double;

    function Dot(const AVector: TVector): TVector; overload;
    function Dot(const AVector: TBivector): double; overload;
    function Dot(const AVector: TMultivector): TMultivector; overload;

    function Wedge(const AVector: TVector): double; overload; //zero
    function Wedge(const AVector: TBivector): double; overload; //zero
    function Wedge(const AVector: TMultivector): TMultivector; overload;

    function Projection(const AVector: TVector): TBivector; overload;
    function Projection(const AVector: TBivector): TBivector; overload;
    function Projection(const AVector: TMultivector): TMultivector; overload;

    function Rejection(const AVector: TVector): double; overload; //zero
    function Rejection(const AVector: TBivector): double; overload; //zero
    function Rejection(const AVector: TMultivector): TMultivector; overload;

    function Reflection(const AVector: TVector): TBivector; overload;
    function Reflection(const AVector: TBivector): TBivector; overload;
    function Reflection(const AVector: TMultivector): TMultivector; overload;

    function Rotation(const AVector1, AVector2: TVector): TBivector; overload;
    function Rotation(const AVector1, AVector2: TBivector): TBivector; overload;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector; overload;

    function SameValue(const AValue: TMultivector): boolean;
    function SameValue(const AValue: TBivector): boolean;

    function ToMultivector: TMultivector;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
  end;

  // TVectorHelper
  TVectorHelper = record helper for TVector
    function Dual: TVector;
    function Inverse: TVector;
    function Reverse: TVector;
    function Conjugate: TVector;
    function Reciprocal: TVector;
    function Normalized: TVector;
    function Norm: double;
    function SquaredNorm: double;

    function Dot(const AVector: TVector): double; overload;
    function Dot(const AVector: TBivector): TVector; overload;
    function Dot(const AVector: TMultivector): TMultivector; overload;

    function Wedge(const AVector: TVector): TBivector; overload;
    function Wedge(const AVector: TBivector): double; overload;
    function Wedge(const AVector: TMultivector): TMultivector; overload;

    function Projection(const AVector: TVector): TVector; overload;
    function Projection(const AVector: TBivector): TVector; overload;
    function Projection(const AVector: TMultivector): TMultivector; overload;

    function Rejection(const AVector: TVector): TVector; overload;
    function Rejection(const AVector: TBivector): TVector; overload;
    function Rejection(const AVector: TMultivector): TMultivector; overload;

    function Reflection(const AVector: TVector): TVector; overload;
    function Reflection(const AVector: TBivector): TVector; overload;
    function Reflection(const AVector: TMultivector): TMultivector; overload;

    function Rotation(const AVector1, AVector2: TVector): TVector; overload;
    function Rotation(const AVector1, AVector2: TBivector): TVector; overload;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector; overload;

    function SameValue(const AValue: TMultivector): boolean;
    function SameValue(const AValue: TVector): boolean;

    function Extract(AComponents: TVectorComponents): TVector;

    function ToMultivector: TMultivector;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
  end;

  // TVersor
  TVersor1 = record class operator *(const AValue: double; const ASelf: TVersor1): TVector; end;
  TVersor2 = record class operator *(const AValue: double; const ASelf: TVersor2): TVector; end;

  // TBiversor
  TBiversor12 = record class operator *(const AValue: double; const ASelf: TBiversor12): TBivector; end;

const
  e1   : TVersor1 = ();
  e2   : TVersor2 = ();
  e12  : TBiversor12 = ();

  u1   : TVector   = (fm1:1.0; fm2:0.0);
  u2   : TVector   = (fm1:0.0; fm2:1.0);
  u12  : TBivector = (fm12:1.0);

  NullMultivector : TMultivector = (fm0:0.0; fm1:0.0; fm2:0.0; fm12:0.0;);
  NullBivector    : TBivector    = (fm12:0.0;);
  NullVector      : TVector      = (fm1: 0.0; fm2: 0.0);
  NullScalar      : double       = (0.0);

implementation

uses
  Math;

function Fmt(const AValue: double): string;
begin
  if AValue < 0.0 then
    result := FloatToStr(AValue)
  else
    result := '+' + FloatToStr(AValue);
end;

function Fmt(const AValue: double; APrecision, ADigits: longint): string;
begin
  if AValue < 0.0 then
    result := FloatToStrF(AValue, ffGeneral, APrecision, ADigits)
  else
    result := '+' + FloatToStrF(AValue, ffGeneral, APrecision, ADigits);
end;

// TMultivector

class operator TMultivector.:=(const AValue: double): TMultivector;
begin
  result.fm0  := AValue;
  result.fm1  := 0.0;
  result.fm2  := 0.0;
  result.fm12 := 0.0;
end;

class operator TMultivector.:=(const AValue: TMultivector): double;
begin
  result := AValue;
end;

class operator TMultivector.<>(const ALeft, ARight: TMultivector): boolean;
begin
  result := (ALeft.fm0  <> ARight.fm0 ) or
            (ALeft.fm1  <> ARight.fm1 ) or
            (ALeft.fm2  <> ARight.fm2 ) or
            (ALeft.fm12 <> ARight.fm12);
end;

class operator TMultivector.<>(const ALeft: TMultivector; const ARight: double): boolean;
begin
  result := (ALeft.fm0  <> ARight) or
            (ALeft.fm1  <>    0.0) or
            (ALeft.fm2  <>    0.0) or
            (ALeft.fm12 <>    0.0);
end;

class operator TMultivector.<>(const ALeft: double; const ARight: TMultivector): boolean;
begin
  result := (ALeft <> ARight.fm0 ) or
            (0.0   <> ARight.fm1 ) or
            (0.0   <> ARight.fm2 ) or
            (0.0   <> ARight.fm12);
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
  result.fm0  := ALeft.fm0 + ARight;
  result.fm1  := ALeft.fm1;
  result.fm2  := ALeft.fm2;
  result.fm12 := ALeft.fm12;
end;

class operator TMultivector.+(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result.fm0  := ARight.fm0 + ALeft;
  result.fm1  := ARight.fm1;
  result.fm2  := ARight.fm2;
  result.fm12 := ARight.fm12;
end;

class operator TMultivector.+(const ALeft, ARight: TMultivector): TMultivector;
begin
  result.fm0  := ALeft.fm0  + ARight.fm0;
  result.fm1  := ALeft.fm1  + ARight.fm1;
  result.fm2  := ALeft.fm2  + ARight.fm2;
  result.fm12 := ALeft.fm12 + ARight.fm12;
end;

class operator TMultivector.-(const ASelf: TMultivector): TMultivector;
begin
  result.fm0  := -ASelf.fm0;
  result.fm1  := -ASelf.fm1;
  result.fm2  := -ASelf.fm2;
  result.fm12 := -ASelf.fm12;
end;

class operator TMultivector.-(const ALeft: TMultivector; const ARight: double): TMultivector;
begin
  result.fm0  := ALeft.fm0 - ARight;
  result.fm1  := ALeft.fm1;
  result.fm2  := ALeft.fm2;
  result.fm12 := ALeft.fm12;
end;

class operator TMultivector.-(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result.fm0  := ALeft - ARight.fm0;
  result.fm1  :=       - ARight.fm1;
  result.fm2  :=       - ARight.fm2;
  result.fm12 :=       - ARight.fm12;
end;

class operator TMultivector.-(const ALeft, ARight: TMultivector): TMultivector;
begin
  result.fm0  := ALeft.fm0  - ARight.fm0;
  result.fm1  := ALeft.fm1  - ARight.fm1;
  result.fm2  := ALeft.fm2  - ARight.fm2;
  result.fm12 := ALeft.fm12 - ARight.fm12;
end;

class operator TMultivector.*(const ALeft: TMultivector; const ARight: double): TMultivector;
begin
  result.fm0  := ALeft.fm0  * ARight;
  result.fm1  := ALeft.fm1  * ARight;
  result.fm2  := ALeft.fm2  * ARight;
  result.fm12 := ALeft.fm12 * ARight;
end;

class operator TMultivector.*(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result.fm0  := ALeft * ARight.fm0;
  result.fm1  := ALeft * ARight.fm1;
  result.fm2  := ALeft * ARight.fm2;
  result.fm12 := ALeft * ARight.fm12;
end;

class operator TMultivector.*(const ALeft, ARight: TMultivector): TMultivector;
begin
  result.fm0 :=    ALeft.fm0  * ARight.fm0
                 + ALeft.fm1  * ARight.fm1
                 + ALeft.fm2  * ARight.fm2
                 - ALeft.fm12 * ARight.fm12;

  result.fm1 :=    ALeft.fm0  * ARight.fm1
                 + ALeft.fm1  * ARight.fm0
                 - ALeft.fm2  * ARight.fm12
                 + ALeft.fm12 * ARight.fm2;

  result.fm2 :=    ALeft.fm0  * ARight.fm2
                 + ALeft.fm1  * ARight.fm12
                 + ALeft.fm2  * ARight.fm0
                 - ALeft.fm12 * ARight.fm1;

  result.fm12 :=   ALeft.fm0  * ARight.fm12
                 + ALeft.fm1  * ARight.fm2
                 - ALeft.fm2  * ARight.fm1
                 + ALeft.fm12 * ARight.fm0;
end;

class operator TMultivector./(const ALeft: TMultivector; const ARight: double): TMultivector;
begin
  result.fm0  := ALeft.fm0  / ARight;
  result.fm1  := ALeft.fm1  / ARight;
  result.fm2  := ALeft.fm2  / ARight;
  result.fm12 := ALeft.fm12 / ARight;
end;

class operator TMultivector./(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TMultivector./(const ALeft, ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

// TBivector

class operator TBivector.:=(const AValue: TBivector): TMultivector;
begin
  result.fm0  := 0.0;
  result.fm1  := 0.0;
  result.fm2  := 0.0;
  result.fm12 := AValue.fm12;
end;

class operator TBivector.:=(const AValue: TMultivector): TBivector;
begin
  result.fm12 := AValue.fm12;
end;

class operator TBivector.<>(const ALeft, ARight: TBivector): boolean;
begin
  result := (ALeft.fm12 <> ARight.fm12);
end;

class operator TBivector.<>(const ALeft: TMultivector; const ARight: TBivector): boolean;
begin
  result := (ALeft.fm0  <>         0.0) or
            (ALeft.fm1  <>         0.0) or
            (ALeft.fm2  <>         0.0) or
            (ALeft.fm12 <> ARight.fm12);
end;

class operator TBivector.<>(const ALeft: TBivector; const ARight: TMultivector): boolean;
begin
  result := (ARight.fm0  <>        0.0) or
            (ARight.fm1  <>        0.0) or
            (ARight.fm2  <>        0.0) or
            (ARight.fm12 <> ALeft.fm12);
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
end;

class operator TBivector.+(const ALeft: TBivector; const ARight: double): TMultivector;
begin
  result.fm0  := ARight;
  result.fm1  := 0.0;
  result.fm2  := 0.0;
  result.fm12 := ALeft.fm12;
end;

class operator TBivector.+(const ALeft: double; const ARight: TBivector): TMultivector;
begin
  result := ARight + ALeft;
end;

class operator TBivector.+(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0  := ARight.fm0;
  result.fm1  := ARight.fm1;
  result.fm2  := ARight.fm2;
  result.fm12 := ARight.fm12 + ALeft.fm12;
end;

class operator TBivector.+(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
begin
  result := ARight + ALeft;
end;

class operator TBivector.-(const ASelf: TBivector): TBivector;
begin
  result.fm12 := -ASelf.fm12;
end;

class operator TBivector.-(const ALeft, ARight: TBivector): TBivector;
begin
  result.fm12  := ALeft.fm12 - ARight.fm12;
end;

class operator TBivector.-(const ALeft: TBivector; const ARight: double): TMultivector;
begin
  result.fm0  := -ARight;
  result.fm1  :=  0.0;
  result.fm2  :=  0.0;
  result.fm12 :=  ALeft.fm12;
end;

class operator TBivector.-(const ALeft: double; const ARight: TBivector): TMultivector;
begin
  result := ARight - ALeft;
end;

class operator TBivector.-(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0  := -ARight.fm0;
  result.fm1  := -ARight.fm1;
  result.fm2  := -ARight.fm2;
  result.fm12 :=  ALeft.fm12 - ARight.fm12;
end;

class operator TBivector.-(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
begin
  result := ARight - ALeft;
end;

class operator TBivector.*(const ALeft: double; const ARight: TBivector): TBivector;
begin
  result.fm12 := ALeft * ARight.fm12;
end;

class operator TBivector.*(const ALeft: TBivector; const ARight: double): TBivector;
begin
  result := ARight * ALeft;
end;

class operator TBivector.*(const ALeft, ARight: TBivector): TMultivector;
begin
  result.fm0  := -ALeft.fm12 * ARight.fm12;
  result.fm1  :=  0.0;
  result.fm2  :=  0.0;
  result.fm12 :=  0.0;
end;

class operator TBivector.*(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0  := -ALeft.fm12 * ARight.fm12;
  result.fm1  :=  ALeft.fm12 * ARight.fm2;
  result.fm2  := -ALeft.fm12 * ARight.fm1;
  result.fm12 :=  ALeft.fm12 * ARight.fm0;
end;

class operator TBivector.*(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
begin
  result.fm0  := -ALeft.fm12 * ARight.fm12;
  result.fm1  := -ALeft.fm2  * ARight.fm12;
  result.fm2  :=  ALeft.fm1  * ARight.fm12;
  result.fm12 :=  ALeft.fm0  * ARight.fm12;
end;

class operator TBivector./(const ALeft, ARight: TBivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TBivector; const ARight: double): TBivector;
begin
  result.fm12 := ALeft.fm12 / ARight;
end;

class operator TBivector./(const ALeft: double; const ARight: TBivector): TBivector;
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

class operator TVector.:=(const AValue: TVector): TMultivector;
begin
  result.fm0  := 0.0;
  result.fm1  := AValue.fm1;
  result.fm2  := AValue.fm2;
  result.fm12 := 0.0;
end;

class operator TVector.:=(const AValue: TMultivector): TVector;
begin
  result.fm1 := AValue.fm1;
  result.fm2 := AValue.fm2;
end;

class operator TVector.<>(const ALeft, ARight: TVector): boolean;
begin
  result := (ALeft.fm1 <> ARight.fm1) or
            (ALeft.fm2 <> ARight.fm2);
end;

class operator TVector.<>(const ALeft: TMultivector; const ARight: TVector): boolean;
begin
  result := (ALeft.fm0  <>        0.0) or
            (ALeft.fm1  <> ARight.fm1) or
            (ALeft.fm2  <> ARight.fm2) or
            (ALeft.fm12 <>        0.0);
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
end;

class operator TVector.+(const ALeft: TVector; const ARight: double): TMultivector;
begin
  result.fm0  := ARight;
  result.fm1  := ALeft.fm1;
  result.fm2  := ALeft.fm2;
  result.fm12 := 0.0;
end;

class operator TVector.+(const ALeft: double; const ARight: TVector): TMultivector;
begin
  result.fm0  := ALeft;
  result.fm1  := ARight.fm1;
  result.fm2  := ARight.fm2;
  result.fm12 := 0.0;
end;

class operator TVector.+(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fm0  := 0.0;
  result.fm1  := ALeft.fm1;
  result.fm2  := ALeft.fm2;
  result.fm12 := ARight.fm12;
end;

class operator TVector.+(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fm0  := 0.0;
  result.fm1  := ARight.fm1;
  result.fm2  := ARight.fm2;
  result.fm12 := ALeft.fm12;
end;

class operator TVector.+(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result.fm0  := ARight.fm0;
  result.fm1  := ARight.fm1 + ALeft.fm1;
  result.fm2  := ARight.fm2 + ALeft.fm2;
  result.fm12 := ARight.fm12;
end;

class operator TVector.+(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result.fm0  := ALeft.fm0;
  result.fm1  := ALeft.fm1 + ARight.fm1;
  result.fm2  := ALeft.fm2 + ARight.fm2;
  result.fm12 := ALeft.fm12;
end;

class operator TVector.-(const ASelf: TVector): TVector;
begin
  result.fm1 := -ASelf.fm1;
  result.fm2 := -ASelf.fm2;
end;

class operator TVector.-(const ALeft, ARight: TVector): TVector;
begin
  result.fm1 := ALeft.fm1 - ARight.fm1;
  result.fm2 := ALeft.fm2 - ARight.fm2;
end;

class operator TVector.-(const ALeft: TVector; const ARight: double): TMultivector;
begin
  result.fm0  := -ARight;
  result.fm1  :=  ALeft.fm1;
  result.fm2  :=  ALeft.fm2;
  result.fm12 :=  0.0;
end;

class operator TVector.-(const ALeft: double; const ARight: TVector): TMultivector;
begin
  result.fm0  :=  ALeft;
  result.fm1  := -ARight.fm1;
  result.fm2  := -ARight.fm2;
  result.fm12 :=  0.0;
end;

class operator TVector.-(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fm0  :=  0.0;
  result.fm1  :=  ALeft.fm1;
  result.fm2  :=  ALeft.fm2;
  result.fm12 := -ARight.fm12;
end;

class operator TVector.-(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fm0  :=  0.0;
  result.fm1  := -ARight.fm1;
  result.fm2  := -ARight.fm2;
  result.fm12 :=  ALeft.fm12;
end;

class operator TVector.-(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result.fm0  := -ARight.fm0;
  result.fm1  :=  ALeft.fm1 - ARight.fm1;
  result.fm2  :=  ALeft.fm2 - ARight.fm2;
  result.fm12 := -ARight.fm12;
end;

class operator TVector.-(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result.fm0  := ALeft.fm0;
  result.fm1  := ALeft.fm1 - ARight.fm1;
  result.fm2  := ALeft.fm2 - ARight.fm2;
  result.fm12 := ALeft.fm12;
end;

class operator TVector.*(const ALeft: double; const ARight: TVector): TVector;
begin
  result.fm1 := ALeft * ARight.fm1;
  result.fm2 := ALeft * ARight.fm2;
end;

class operator TVector.*(const ALeft: TVector; const ARight: double): TVector;
begin
  result.fm1 := ALeft.fm1 * ARight;
  result.fm2 := ALeft.fm2 * ARight;
end;

class operator TVector.*(const ALeft, ARight: TVector): TMultivector;
begin
  result.fm0  := ALeft.fm1 * ARight.fm1 + ALeft.fm2 * ARight.fm2;
  result.fm1  := 0.0;
  result.fm2  := 0.0;
  result.fm12 := ALeft.fm1 * ARight.fm2 - ALeft.fm2 * ARight.fm1;
end;

class operator TVector.*(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fm0  :=  0.0;
  result.fm1  := -ALeft.fm2 * ARight.fm12;
  result.fm2  :=  ALeft.fm1 * ARight.fm12;
  result.fm12 :=  0.0;
end;

class operator TVector.*(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fm0  :=  0.0;
  result.fm1  :=  ALeft.fm12 * ARight.fm2;
  result.fm2  := -ALeft.fm12 * ARight.fm1;
  result.fm12 :=  0.0;
end;

class operator TVector.*(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result.fm0  :=  ALeft.fm1 * ARight.fm1
                 +ALeft.fm2 * ARight.fm2;

  result.fm1  :=  ALeft.fm1 * ARight.fm0
                 -ALeft.fm2 * ARight.fm12;

  result.fm2  :=  ALeft.fm1 * ARight.fm12
                 +ALeft.fm2 * ARight.fm0;

  result.fm12 :=  ALeft.fm1 * ARight.fm2
                 -ALeft.fm2 * ARight.fm1;
end;

class operator TVector.*(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result.fm0  :=  ALeft.fm1  * ARight.fm1
                 +ALeft.fm2  * ARight.fm2;

  result.fm1  :=  ALeft.fm0  * ARight.fm1
                 +ALeft.fm12 * ARight.fm2;

  result.fm2  :=  ALeft.fm0  * ARight.fm2
                 -ALeft.fm12 * ARight.fm1;

  result.fm12 :=  ALeft.fm1  * ARight.fm2
                 -ALeft.fm2  * ARight.fm1;
end;

class operator TVector./(const ALeft, ARight: TVector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./ (const ALeft: TVector; const ARight: double): TVector;
begin
  result.fm1 := ALeft.fm1 / ARight;
  result.fm2 := ALeft.fm2 / ARight;
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

class operator TVector./(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

// TMultivectorHelper

function TMultivectorHelper.Dual: TMultivector;
begin
  // Self * -e123
  result.fm0  := -fm12;
  result.fm1  :=  fm2;
  result.fm2  := -fm1;
  result.fm12 :=  fm0;
end;

function TMultivectorHelper.Inverse: TMultivector; // inversion
begin
  result.fm0  :=  fm0;
  result.fm1  := -fm1;
  result.fm2  := -fm2;
  result.fm12 :=  fm12;
end;

function TMultivectorHelper.Reverse: TMultivector; // reversion
begin
  result.fm0  :=  fm0;
  result.fm1  :=  fm1;
  result.fm2  :=  fm2;
  result.fm12 := -fm12;
end;

function TMultivectorHelper.Conjugate: TMultivector; // conjugate
begin
  result.fm0  :=  fm0;
  result.fm1  := -fm1;
  result.fm2  := -fm2;
  result.fm12 := -fm12;
end;

function TMultivectorHelper.Reciprocal: TMultivector;
var
  Numerator: TMultivector;
begin
  if (fm0 <>0) and ((fm1 <>0) or (fm2 <>0)) then
  begin
    Numerator := Conjugate * Inverse * Reverse;
    result    := Numerator / (Self * Numerator).fm0;
  end else
    result := Reverse / SquaredNorm;
end;

function TMultivectorHelper.Normalized: TMultivector;
begin
  result := Self / Norm;
end;

function TMultivectorHelper.Norm: double;
begin
  result := sqrt(SquaredNorm);
end;

function TMultivectorHelper.SquaredNorm: double;
begin
  result := sqr(fm0) + sqr(fm1) + sqr(fm2) + sqr(fm12);
end;

function TMultivectorHelper.Dot(const AVector: TVector): TMultivector;
begin
  result.fm0  :=  fm1 * AVector.fm1 + fm2  * AVector.fm2;
  result.fm1  :=  fm0 * AVector.fm1 + fm12 * AVector.fm2;
  result.fm2  :=  fm0 * AVector.fm2 - fm12 * AVector.fm1;
  result.fm12 :=  0.0;
end;

function TMultivectorHelper.Dot(const AVector: TBivector): TMultivector;
begin
  result.fm0  := -fm12 * AVector.fm12;
  result.fm1  := -fm2  * AVector.fm12;
  result.fm2  :=  fm1  * AVector.fm12;
  result.fm12 :=  fm0  * AVector.fm12;
end;

function TMultivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0  :=  fm0  * AVector.fm0
                 +fm1  * AVector.fm1
                 +fm2  * AVector.fm2
                 -fm12 * AVector.fm12;

  result.fm1  :=  fm0  * AVector.fm1
                 +fm1  * AVector.fm0
                 -fm2  * AVector.fm12
                 +fm12 * AVector.fm2;

  result.fm2  :=  fm0  * AVector.fm2
                 +fm1  * AVector.fm12
                 +fm2  * AVector.fm0
                 -fm12 * AVector.fm1;

  result.fm12 :=  fm0  * AVector.fm12
                 +fm12 * AVector.fm0;
end;

function TMultivectorHelper.Wedge(const AVector: TVector): TMultivector;
begin
  result.fm0  :=  0.0;
  result.fm1  :=  fm0 * AVector.fm1;
  result.fm2  :=  fm0 * AVector.fm2;
  result.fm12 :=  fm1 * AVector.fm2 - fm2 * AVector.fm1;
end;

function TMultivectorHelper.Wedge(const AVector: TBivector): TMultivector;
begin
  result.fm0  := 0.0;
  result.fm1  := 0.0;
  result.fm2  := 0.0;
  result.fm12 := fm0 * AVector.fm12;
end;

function TMultivectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fm0  :=  fm0  * AVector.fm0;
  result.fm1  :=  fm0  * AVector.fm1 + fm1 * AVector.fm0;
  result.fm2  :=  fm0  * AVector.fm2 + fm2 * AVector.fm0;

  result.fm12 :=  fm0  * AVector.fm12
                 +fm1  * AVector.fm2
                 -fm2  * AVector.fm1
                 +fm12 * AVector.fm0;
end;

function TMultivectorHelper.Projection(const AVector: TVector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TMultivectorHelper.Projection(const AVector: TBivector): TMultivector;
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

function TMultivectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TMultivectorHelper.SameValue(const AValue: TMultivector): boolean;
begin
  result := Math.SameValue(fm0,  AValue.fm0 ) and
            Math.SameValue(fm1,  AValue.fm1 ) and
            Math.SameValue(fm2,  AValue.fm2 ) and
            Math.SameValue(fm12, AValue.fm12);
end;

function TMultivectorHelper.SameValue(const AValue: TBivector): boolean;
begin
  result := Math.SameValue(fm0,          0.0) and
            Math.SameValue(fm1,          0.0) and
            Math.SameValue(fm2,          0.0) and
            Math.SameValue(fm12, AValue.fm12);
end;

function TMultivectorHelper.SameValue(const AValue: TVector): boolean;
begin
  result := Math.SameValue(fm0,         0.0) and
            Math.SameValue(fm1,  AValue.fm1) and
            Math.SameValue(fm2,  AValue.fm2) and
            Math.SameValue(fm12,        0.0);
end;

function TMultivectorHelper.SameValue(const AValue: double): boolean;
begin
  result := Math.SameValue(fm0,  AValue) and
            Math.SameValue(fm1,     0.0) and
            Math.SameValue(fm2,     0.0) and
            Math.SameValue(fm12,    0.0);
end;

function TMultivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm0,  0.0) then result := result + Fmt(fm0,   APrecision, ADigits) + ' ';
  if not Math.SameValue(fm1,  0.0) then result := result + Fmt(fm1,   APrecision, ADigits) + 'e1 ';
  if not Math.SameValue(fm2,  0.0) then result := result + Fmt(fm2,   APrecision, ADigits) + 'e2 ';
  if not Math.SameValue(fm12, 0.0) then result := result + Fmt(fm12,  APrecision, ADigits) + 'e12 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0';
end;

function TMultivectorHelper.ToString: string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm0,  0.0) then result := result + Fmt(fm0  ) + ' ';
  if not Math.SameValue(fm1,  0.0) then result := result + Fmt(fm1  ) + 'e1 ';
  if not Math.SameValue(fm2,  0.0) then result := result + Fmt(fm2  ) + 'e2 ';
  if not Math.SameValue(fm12, 0.0) then result := result + Fmt(fm12 ) + 'e12 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0';
end;

function TMultivectorHelper.Extract(AComponents: TMultivectorComponents): TMultivector;
begin
  Result := NullMultivector;
  if mc0  in AComponents then result.fm0  := fm0;
  if mc1  in AComponents then result.fm1  := fm1;
  if mc2  in AComponents then result.fm2  := fm2;
  if mc12 in AComponents then result.fm12 := fm12;
end;

function TMultivectorHelper.Extract(AComponents: TVectorComponents): TVector;
begin
  Result := NullVector;
  if vc1 in AComponents then result.fm1 := fm1;
  if vc2 in AComponents then result.fm2 := fm2;
end;

function TMultivectorHelper.ExtractBivector: TBivector;
begin
  result.fm12 := fm12;
end;

function TMultivectorHelper.ExtractVector: TVector;
begin
  result.fm1 := fm1;
  result.fm2 := fm2;
end;

function TMultivectorHelper.ExtractScalar: double;
begin
  result := fm0;
end;

function TMultivectorHelper.IsNull: boolean;
begin
  result := SameValue(NullMultivector);
end;

function TMultivectorHelper.IsScalar: boolean;
begin
  result := (not Math.SameValue(fm0,  0.0)) and
            (    Math.SameValue(fm1,  0.0)) and
            (    Math.SameValue(fm2,  0.0)) and
            (    Math.SameValue(fm12, 0.0));
end;

function TMultivectorHelper.IsVector: boolean;
begin
  result :=  (    Math.SameValue(fm0,  0.0))  and
            ((not Math.SameValue(fm1,  0.0))  or
             (not Math.SameValue(fm2,  0.0))) and
             (    Math.SameValue(fm12, 0.0));
end;

function TMultivectorHelper.IsBiVector: boolean;
begin
  result :=  (    Math.SameValue(fm0,  0.0)) and
             (    Math.SameValue(fm1,  0.0)) and
             (    Math.SameValue(fm2,  0.0)) and
             (not Math.SameValue(fm12, 0.0));
end;

function TMultivectorHelper.IsA: string;
begin
  if IsNull      then Result := 'Null'      else
  if IsBivector  then Result := 'TBivector' else
  if IsVector    then Result := 'TVector'   else
  if IsScalar    then Result := 'TScalar'   else Result := 'TMultivector';
end;

// TBivectorHelper

function TBivectorHelper.Dual: double;
begin
  // Self * e12
  result := -fm12;
end;

function TBivectorHelper.Inverse: TBivector;
begin
  result.fm12 := fm12;
end;

function TBivectorHelper.Conjugate: TBivector;
begin
  result.fm12 := -fm12;
end;

function TBivectorHelper.Reverse: TBivector;
begin
  result.fm12 := -fm12;
end;

function TBivectorHelper.Reciprocal: TBivector;
begin
  result := Reverse / SquaredNorm;
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
  // Self * Reverse;
  result := fm12 * fm12;
end;

function TBivectorHelper.Dot(const AVector: TVector): TVector;
begin
  result.fm1 :=  fm12 * AVector.fm2;
  result.fm2 := -fm12 * AVector.fm1;
end;

function TBivectorHelper.Dot(const AVector: TBivector): double;
begin
  result := -fm12 * AVector.fm12;
end;

function TBivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0  := -fm12 * AVector.fm12;
  result.fm1  := +fm12 * AVector.fm2;
  result.fm2  := -fm12 * AVector.fm1;
  result.fm12 :=  fm12 * AVector.fm0;
end;

function TBivectorHelper.Wedge(const AVector: TVector): double; overload;
begin
  result := NullScalar;
end;

function TBivectorHelper.Wedge(const AVector: TBivector): double;
begin
  result := NullScalar;
end;

function TBivectorHelper.Wedge (const AVector: TMultivector): TMultivector;
begin
  result.fm0  := 0.0;
  result.fm1  := 0.0;
  result.fm2  := 0.0;
  result.fm12 := fm12 * AVector.fm0;
end;

function TBivectorHelper.Projection(const AVector: TVector): TBivector;
begin
  // implicit ExtractBivector
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Projection(const AVector: TBivector): TBivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Projection(const AVector: TMultivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Rejection(const AVector: TVector): double; overload;
begin
  result := NullScalar;
end;

function TBivectorHelper.Rejection(const AVector: TBivector): double; overload;
begin
  result := NullScalar;
end;

function TBivectorHelper.Rejection (const AVector: TMultivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Reflection(const AVector: TVector): TBivector;
begin
  // implicit ExtractBivector
  result := AVector * Self * AVector.Reciprocal;
end;

function TBivectorHelper.Reflection(const AVector: TBivector): TBivector;
begin
  // implicit ExtractBivector
  result := AVector * Self * AVector.Reciprocal;
end;

function TBivectorHelper.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: TVector): TBivector;
begin
  // implicit ExtractBivector
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: TBivector): TBivector;
begin
  // implicit ExtractBivector
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TBivectorHelper.SameValue(const AValue: TMultivector): boolean;
begin
  result := Math.SameValue(0.0,  AValue.fm0 ) and
            Math.SameValue(0.0,  AValue.fm1 ) and
            Math.SameValue(0.0,  AValue.fm2 ) and
            Math.SameValue(fm12, AValue.fm12);
end;

function TBivectorHelper.SameValue(const AValue: TBivector): boolean;
begin
  result := Math.SameValue(fm12, AValue.fm12);
end;

function TBivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
begin
  if not Math.SameValue(fm12, 0.0) then
    Result := Fmt(fm12, APrecision, ADigits) + 'e12'
  else
    Result := '0e12';
end;

function TBivectorHelper.ToString: string;
begin
  if not Math.SameValue(fm12, 0.0) then
    Result := Fmt(fm12) + 'e12'
  else
    Result := '0e12';
end;

function TBivectorHelper.ToMultivector: TMultivector;
begin
  result.fm0  := 0.0;
  result.fm1  := 0.0;
  result.fm2  := 0.0;
  result.fm12 := fm12;
end;

// TVectorHelper

function TVectorHelper.Dual: TVector;
begin
  // Self * e12
  result.fm1 :=  fm2;
  result.fm2 := -fm1;
end;

function TVectorHelper.Inverse: TVector;
begin
  result.fm1 := -fm1;
  result.fm2 := -fm2;
end;

function TVectorHelper.Reverse: TVector;
begin
  result.fm1 := fm1;
  result.fm2 := fm2;
end;

function TVectorHelper.Conjugate: TVector;
begin
  result.fm1 := -fm1;
  result.fm2 := -fm2;
end;

function TVectorHelper.Reciprocal: TVector;
begin
  result := Reverse / SquaredNorm;
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
  result := fm1 * fm1 + fm2 * fm2;
end;

function TVectorHelper.Dot(const AVector: TVector): double;
begin
 result := fm1 * AVector.fm1 + fm2 * AVector.fm2;
end;

function TVectorHelper.Dot(const AVector: TBivector): TVector;
begin
  result.fm1 := -fm2 * AVector.fm12;
  result.fm2 :=  fm1 * AVector.fm12;
end;

function TVectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0  := fm1 * AVector.fm1 + fm2 * AVector.fm2;
  result.fm1  := fm1 * AVector.fm0 - fm2 * AVector.fm12;
  result.fm2  := fm2 * AVector.fm0 + fm1 * AVector.fm12;
  result.fm12 := 0.0;
end;

function TVectorHelper.Wedge(const AVector: TVector): TBivector;
begin
  result.fm12 := fm1 * AVector.fm2 - fm2 * AVector.fm1;
end;

function TVectorHelper.Wedge(const AVector: TBivector): double; overload;
begin
  result := NullScalar;
end;

function TVectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fm0  := 0.0;
  result.fm1  := fm1 * AVector.fm0;
  result.fm2  := fm2 * AVector.fm0;
  result.fm12 := fm1 * AVector.fm2 - fm2 * AVector.fm1;
end;

function TVectorHelper.Projection(const AVector: TVector): TVector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Projection(const AVector: TBivector): TVector;
begin
  // implicit ExtractVector
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Projection(const AVector: TMultivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Rejection(const AVector: TVector): TVector;
begin
  // implicit ExtractVector
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Rejection(const AVector: TBivector): TVector; overload;
begin
  result := NullVector;
end;

function TVectorHelper.Rejection (const AVector: TMultivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Reflection(const AVector: TVector): TVector;
begin
  // implicit ExtractVector
  result := AVector * Self * AVector.Reciprocal;
end;

function TVectorHelper.Reflection(const AVector: TBivector): TVector;
begin
  // implicit ExtractVector
  result := AVector * Self * AVector.Reciprocal;
end;

function TVectorHelper.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TVector): TVector;
begin
  // implicit ExtractVector
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TBivector): TVector;
begin
  // implicit ExtractVector
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TVectorHelper.SameValue(const AValue: TMultivector): boolean;
begin
  result := Math.SameValue(0.0, AValue.fm0 ) and
            Math.SameValue(fm1, AValue.fm1 ) and
            Math.SameValue(fm2, AValue.fm2 ) and
            Math.SameValue(0.0, AValue.fm12);
end;

function TVectorHelper.SameValue(const AValue: TVector): boolean;
begin
  result := Math.SameValue(fm1, AValue.fm1) and
            Math.SameValue(fm2, AValue.fm2);
end;

function TVectorHelper.Extract(AComponents: TVectorComponents): TVector;
begin
  Result := NullVector;
  if vc1 in AComponents then result.fm1 := fm1;
  if vc2 in AComponents then result.fm2 := fm2;
end;


function TVectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm1, 0.0) then Result := Result + Fmt(fm1,  APrecision, ADigits) + 'e1 ';
  if not Math.SameValue(fm2, 0.0) then Result := Result + Fmt(fm2,  APrecision, ADigits) + 'e2 ';

  i := Length(Result);
  if i > 0 then
    SetLength(Result, i - 1)
  else
    Result := '0e1';
end;

function TVectorHelper.ToString: string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm1, 0.0) then Result := Result + Fmt(fm1) + 'e1 ';
  if not Math.SameValue(fm2, 0.0) then Result := Result + Fmt(fm2) + 'e2 ';

  i := Length(Result);
  if i > 0 then
    SetLength(Result, i - 1)
  else
    Result := '0e1';
end;

function TVectorHelper.ToMultivector: TMultivector;
begin
  result.fm0  := 0.0;
  result.fm1  := fm1;
  result.fm2  := fm2;
  result.fm12 := 0.0;
end;

// TVersors

class operator TVersor1.*(const AValue: double; const ASelf: TVersor1): TVector;
begin
  result.fm1 := AValue;
  result.fm2 := 0.0;
end;

class operator TVersor2.*(const AValue: double; const ASelf: TVersor2): TVector;
begin
  result.fm1 := 0.0;
  result.fm2 := AValue;
end;

// TBiversors

class operator TBiversor12.*(const AValue: double; const ASelf: TBiversor12): TBivector;
begin
  result.fm12 := AValue;
end;

end.

