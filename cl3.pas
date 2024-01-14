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

    class operator + (const ASelf: TMultivector): TMultivector;
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

    function Inverse: TMultivector;
    function Reverse: TMultivector;
    function Conjugate: TMultivector;
    function SquaredNorm: TMultivector;
    function Norm(AGrade: longint): double;

    function Reciprocal: TMultivector;
    function Projection(const AVector: TMultivector): TMultivector;
    function Rejection (const AVector: TMultivector): TMultivector;
    function Reflection(const AVector: TMultivector): TMultivector;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;

    function ScalarProduct(const AVector: TMultivector): TMultivector;
    function WedgeProduct(const AVector: TMultivector): TMultivector;

    function ToString: string;
  end;

  // Trivector
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

    class operator + (const ASelf: TTrivector): TTrivector;
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

    class operator * (const ALeft, ARight: TTrivector): double;
    class operator * (const ALeft: TTrivector; const ARight: double): TTrivector;
    class operator * (const ALeft: double; const ARight: TTrivector): TTrivector;
    class operator * (const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: TTrivector): TMultivector;

    class operator / (const ALeft, ARight: TTrivector): double;
    class operator / (const ALeft: TTrivector; const ARight: double): TTrivector;
    class operator / (const ALeft: double; const ARight: TTrivector): TTrivector;
    class operator / (const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: TTrivector): TMultivector;

    function Inverse: TTrivector;
    function Reverse: TTrivector;
    function Conjugate: TTrivector;
    function SquaredNorm: double;
    function Norm: double;
    function Normalized: TTrivector;

    function Reciprocal: TTrivector;
    function Projection(const AVector: TMultivector): TMultivector;
    function Rejection (const AVector: TMultivector): TMultivector;
    function Reflection(const AVector: TMultivector): TMultivector;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;

    function ScalarProduct(const AVector: TMultivector): TMultivector;
    function ScalarProduct(const AVector: TTrivector): double;
    function WedgeProduct(const AVector: TMultivector): TMultivector;
    function WedgeProduct(const AVector: TTrivector): double;

    function ToMultivector: TMultivector;
    function ToString: string;
  end;

  // Bivector
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

    class operator - (const ALeft, ARight: TBivector): TBivector;
    class operator - (const ALeft: TBivector; const ARight: double): TMultivector;
    class operator - (const ALeft: double; const ARight: TBivector): TMultivector;
    class operator - (const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator - (const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator - (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    class operator * (const ALeft, ARight: TBivector): TMultivector;
    class operator * (const ALeft: TBivector; const ARight: double): TBivector;
    class operator * (const ALeft: double; const ARight: TBivector): TBivector;
    class operator * (const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator * (const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator * (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    class operator / (const ALeft, ARight: TBivector): TMultivector;
    class operator / (const ALeft: TBivector; const ARight: double): TBivector;
    class operator / (const ALeft: double; const ARight: TBivector): TBivector;
    class operator / (const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator / (const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator / (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    function Inverse: TBivector;
    function Reverse: TBivector;
    function Conjugate: TBivector;
    function SquaredNorm: double;
    function Norm: double;
    function Normalized: TBivector;

    function Reciprocal: TBivector;
    function Projection(const AVector: TMultivector): TMultivector;
    function Rejection (const AVector: TMultivector): TMultivector;
    function Reflection(const AVector: TMultivector): TMultivector;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;

    function ScalarProduct(const AVector: TMultivector): TMultivector;
    function ScalarProduct(const AVector: TTrivector): TMultivector;
    function ScalarProduct(const AVector: TBivector): double;
    function WedgeProduct(const AVector: TMultivector): TMultivector;
    function WedgeProduct(const AVector: TTrivector): TMultivector;
    function WedgeProduct(const AVector: TBivector): TMultivector;

    function ToMultivector: TMultivector;
    function ToString: string;
  end;

  // Vector
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

    class operator - (const ALeft, ARight: TVector): TVector;
    class operator - (const ALeft: TVector; const ARight: double): TMultivector;
    class operator - (const ALeft: double; const ARight: TVector): TMultivector;
    class operator - (const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator - (const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator - (const ALeft: TVector; const ARight: TTrivector): TMultivector;
    class operator - (const ALeft: TTrivector; const ARight: TVector): TMultivector;
    class operator - (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: TVector): TMultivector;

    class operator * (const ALeft, ARight: TVector): TMultivector;
    class operator * (const ALeft: TVector; const ARight: double): TVector;
    class operator * (const ALeft: double; const ARight: TVector): TVector;
    class operator * (const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator * (const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator * (const ALeft: TVector; const ARight: TTrivector): TBivector;
    class operator * (const ALeft: TTrivector; const ARight: TVector): TBivector;
    class operator * (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: TVector): TMultivector;

    class operator / (const ALeft, ARight: TVector): TMultivector;
    class operator / (const ALeft: TVector; const ARight: double): TVector;
    class operator / (const ALeft: double; const ARight: TVector): TVector;
    class operator / (const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator / (const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator / (const ALeft: TVector; const ARight: TTrivector): TBivector;
    class operator / (const ALeft: TTrivector; const ARight: TVector): TBivector;
    class operator / (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: TVector): TMultivector;

    function Inverse: TVector;
    function Reverse: TVector;
    function Conjugate: TVector;
    function SquaredNorm: double;
    function Norm: double;
    function Normalized: TVector;

    function Reciprocal: TVector;
    function Projection(const AVector: TMultivector): TMultivector;
    function Rejection (const AVector: TMultivector): TMultivector;
    function Reflection(const AVector: TMultivector): TMultivector;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;

    function ScalarProduct(const AVector: TMultivector): TMultivector;
    function ScalarProduct(const AVector: TTrivector): TBivector;
    function ScalarProduct(const AVector: TBivector): TVector;
    function ScalarProduct(const AVector: TVector): double;
    function WedgeProduct(const AVector: TMultivector): TMultivector;
    function WedgeProduct(const AVector: TTrivector): TMultivector;
    function WedgeProduct(const AVector: TBivector): TTrivector;
    function WedgeProduct(const AVector: TVector): TBivector;

    function ToMultivector: TMultivector;
    function ToString: string;
  end;

  // TMultivectorHelper
  TMultivectorHelper = record helper for TMultivector
    function Dual: TMultivector;

    function ScalarProduct(const AVector: TTrivector): double; overload;
    function WedgeProduct(const AVector: TTrivector): double; overload;

    function ExtractTrivector: TTrivector;
    function ExtractBivector: TBivector;
    function ExtractVector: TVector;
    function ExtractScalar: double;
  end;

  // TTrivectorHelper
  TTrivectorHelper = record helper for TTrivector
    function Dual: double;
  end;

  // TBivectorHelper
  TBivectorHelper = record helper for TBivector
    function Dual: TVector;
  end;

  // TVectorHelper
  TVectorHelper = record helper for TVector
    function Dual: TBivector;
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

  NullMultivector : TMultivector = (fm0:0; fm1:0; fm2:0; fm3:0; fm12:0; fm23:0; fm31:0; fm123:0);
  NullTrivector   : TTrivector   = (fm123:0);
  NullBivector    : TBivector    = (fm12:0; fm23:0; fm31:0);
  NullVector      : TVector      = (fm1:0; fm2:0; fm3:0);
  NullScalar      : double       = (0);

implementation

// TMultivector class: boolean operators

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
  result := ARight <> ALeft;
end;

class operator TMultivector.=(const ALeft: TMultivector; const ARight: double): boolean;
begin
  result := not (ALeft <> ARight);
end;

class operator TMultivector.=(const ALeft: double; const ARight: TMultivector): boolean;
begin
  result := not (ARight <> ALeft);
end;

class operator TMultivector.=(const ALeft, ARight: TMultivector): boolean;
begin
  result := not (ALeft <> ARight);
end;

// TMultivector class: "+" operator

class operator TMultivector.+(const ASelf: TMultivector): TMultivector;
begin
  result := ASelf;
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
  result := ARight + ALeft;
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

// TMultivector class: "-" operator

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
  result.fm1   := -ARight.fm1;
  result.fm2   := -ARight.fm2;
  result.fm3   := -ARight.fm3;
  result.fm12  := -ARight.fm12;
  result.fm23  := -ARight.fm23;
  result.fm31  := -ARight.fm31;
  result.fm123 := -ARight.fm123;
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

// TMultivector class: "*" operator

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
  result := ARight * ALeft;
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

// TMultivector class: "/" operator

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

// TMultivector class: methods

function TMultivector.SquaredNorm: TMultivector;
begin
  result := Self * Self.Conjugate;
end;

function TMultivector.Norm(AGrade: longint): double;
begin
  case AGrade of
    0: result := sqrt(sqr(fm0));
    1: result := sqrt(sqr(fm1)  + sqr(fm2)  + sqr(fm3));
    2: result := sqrt(sqr(fm12) + sqr(fm23) + sqr(fm31));
    3: result := sqrt(sqr(fm123));
  else raise Exception.Create('TMultivector.Norm: Out of bounds.');
  end;
end;

function TMultivector.Inverse: TMultivector;
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

function TMultivector.Reverse: TMultivector;
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

function TMultivector.Conjugate: TMultivector;
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

function TMultivector.Reciprocal: TMultivector;
var
  Numerator: TMultivector;
begin
  Numerator := Conjugate*Inverse*Reverse;
  result    := Numerator / (Self*Numerator).fm0;
end;

function TMultivector.Projection(const AVector: TMultivector): TMultivector;
begin
  result := ScalarProduct(AVector) * AVector.Reciprocal;
end;

function TMultivector.Rejection(const AVector: TMultivector): TMultivector;
begin
  result := WedgeProduct(AVector) * AVector.Reciprocal;
end;

function TMultivector.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TMultivector.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
var
  Rotor: TMultivector;
begin
  Rotor  := AVector2 * AVector1;
  result := Rotor * Self * Rotor.Reciprocal;
end;

function TMultivector.ScalarProduct(const AVector: TMultivector): TMultivector;
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

function TMultivector.WedgeProduct(const AVector: TMultivector): TMultivector;
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

function TMultivector.ToString: string;
begin
  result := Format('%s %se1 %se2 %se3 %se12 %s23 %s31 %s123',
    [FloatToStr(fm0  ),
     FloatToStr(fm1  ), FloatToStr(fm2 ), FloatToStr(fm3 ),
     FloatToStr(fm12 ), FloatToStr(fm23), FloatToStr(fm31),
     FloatToStr(fm123)
    ]);
end;

// Trivector

class operator TTrivector.<>(const ALeft, ARight: TTrivector): boolean;
begin
  result := ALeft.fm123 <> ARight.fm123;
end;

class operator TTrivector.<>(const ALeft: TMultivector; const ARight: TTrivector): boolean;
begin
  result := (ALeft.fm0   <> 0) or
            (ALeft.fm1   <> 0) or
            (ALeft.fm2   <> 0) or
            (ALeft.fm3   <> 0) or
            (ALeft.fm12  <> 0) or
            (ALeft.fm23  <> 0) or
            (ALeft.fm31  <> 0) or
            (ALeft.fm123 <> ARight.fm123);
end;

class operator TTrivector.<>(const ALeft: TTrivector; const ARight: TMultivector): boolean;
begin
  result := ARight <> ALeft;
end;

class operator TTrivector.=(const ALeft: TMultivector; const ARight: TTrivector): boolean;
begin
  result := not (ALeft <> ARight);
end;

class operator TTrivector.=(const ALeft: TTrivector; const ARight: TMultivector): boolean;
begin
  result := not (ARight <> ALeft);
end;

class operator TTrivector.=(const ALeft, ARight: TTrivector): boolean;
begin
  result := not (ALeft <> ARight);
end;

class operator TTrivector.+(const ASelf: TTrivector): TTrivector;
begin
  result := ASelf;
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
  result := ARight + ALeft;
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
  result := ARight + ALeft;
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
  result := ARight - ALeft;
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
  result := ARight - ALeft;
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
  result := ARight * ALeft;
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

function TTrivector.SquaredNorm: double;
begin
  result := -fm123 * fm123;
end;

function TTrivector.Norm: double;
begin
  result := abs(fm123);
end;

function TTrivector.Normalized: TTrivector;
begin
  result := Self / Self.Norm;
end;

function TTrivector.Inverse: TTrivector;
begin
  result.fm123 := -fm123;
end;

function TTrivector.Conjugate: TTrivector;
begin
  result.fm123 := fm123;
end;

function TTrivector.Reverse: TTrivector;
begin
  result.fm123 := -fm123;
end;

function TTrivectorHelper.Dual: double;
begin
  result := -fm123;
end;

function TTrivector.Reciprocal: TTrivector;
begin
  result.fm123 := -1/fm123;
end;

function TTrivector.Projection(const AVector: TMultivector): TMultivector;
begin
  result := ScalarProduct(AVector) * AVector.Reciprocal;
end;

function TTrivector.Rejection(const AVector: TMultivector): TMultivector;
begin
  result := WedgeProduct(AVector) * AVector.Reciprocal;
end;

function TTrivector.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TTrivector.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := (AVector2 * AVector1) * Self * (AVector1.Reciprocal * AVector2.Reciprocal);
end;

function TTrivector.ScalarProduct(const AVector: TMultivector): TMultivector;
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

function TTrivector.ScalarProduct(const AVector: TTrivector): double;
begin
  result := -fm123 * fm123;
end;

function TTrivector.WedgeProduct(const AVector: TMultivector): TMultivector;
begin
  result.fm123 := fm123 * AVector.fm0;
end;

function TTrivector.WedgeProduct(const AVector: TTrivector): double;
begin
  result := 0;
end;

function TTrivector.ToMultivector: TMultivector;
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

function TTrivector.ToString: string;
begin
  result := Format('%se123', [FloatToStr(fm123)]);
end;

// Bivector

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
  result := ARight <> ALeft;
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
  result.fm31  :=   ALeft.fm23 * ALeft.fm12
                  - ALeft.fm12 * ARight.fm23;
  result.fm123 := 0;
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
  result := ARight * ALeft;
end;

class operator TBivector.*(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0 :=   - ALeft.fm12  * ARight.fm12
                  - ALeft.fm23  * ARight.fm23
                  - ALeft.fm31  * ARight.fm31;

  result.fm1 :=     ALeft.fm12  * ARight.fm2
                  - ALeft.fm23  * ARight.fm123
                  - ALeft.fm31  * ARight.fm3;

  result.fm2 :=   - ALeft.fm12  * ARight.fm1
                  + ALeft.fm23  * ARight.fm3
                  - ALeft.fm31  * ARight.fm123;

  result.fm3 :=   - ALeft.fm12  * ARight.fm123
                  - ALeft.fm23  * ARight.fm2
                  + ALeft.fm31  * ARight.fm1;

  result.fm12 :=    ALeft.fm12  * ARight.fm0
                  - ALeft.fm23  * ARight.fm31
                  + ALeft.fm31  * ARight.fm23;

  result.fm23 :=    ALeft.fm12  * ARight.fm31
                  + ALeft.fm23  * ARight.fm0
                  - ALeft.fm31  * ARight.fm12;

  result.fm31 :=  - ALeft.fm12  * ARight.fm23
                  + ALeft.fm23  * ARight.fm12
                  + ALeft.fm31  * ARight.fm0;

  result.fm123 :=   ALeft.fm12  * ARight.fm3
                  + ALeft.fm23  * ARight.fm1
                  + ALeft.fm31  * ARight.fm2;
end;

class operator TBivector.*(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
begin
  result := ARight * ALeft;
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

function TBivector.SquaredNorm: double;
begin
  result := fm12*fm12 + fm23*fm23 + fm31*fm31;
end;

function TBivector.Norm: double;
begin
  result := sqrt(SquaredNorm);
end;

function TBivector.Normalized: TBivector;
begin
  result := Self / Self.Norm;
end;

function TBivector.Inverse: TBivector;
begin
  result.fm12 := fm12;
  result.fm23 := fm23;
  result.fm31 := fm31;
end;

function TBivector.Conjugate: TBivector;
begin
  result.fm12 := -fm12;
  result.fm23 := -fm23;
  result.fm31 := -fm31;
end;

function TBivector.Reverse: TBivector;
begin
  result.fm12 := -fm12;
  result.fm23 := -fm23;
  result.fm31 := -fm31;
end;

function TBivector.Reciprocal: TBivector;
begin
  result := Self / SquaredNorm;
end;

function TBivector.Projection(const AVector: TMultivector): TMultivector;
begin
  result := ScalarProduct(AVector) * AVector.Reciprocal;
end;

function TBivector.Rejection (const AVector: TMultivector): TMultivector;
begin
  result := WedgeProduct(AVector) * AVector.Reciprocal;
end;

function TBivector.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TBivector.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;


function TBivector.ScalarProduct(const AVector: TMultivector): TMultivector;
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

function TBivector.ScalarProduct(const AVector: TTrivector): TMultivector;
begin
  result.fm0   :=  0;
  result.fm1   := -fm23 * AVector.fm123;
  result.fm2   := -fm31 * AVector.fm123;
  result.fm3   := -fm12 * AVector.fm123;
  result.fm12  :=  0;
  result.fm23  :=  0;
  result.fm31  :=  0;
  result.fm123 :=  0;
end;


function TBivector.ScalarProduct(const AVector: TBivector): double;
begin
  result := - fm12 * AVector.fm12
            - fm23 * AVector.fm23
            - fm31 * AVector.fm31;
end;

function TBivector.WedgeProduct (const AVector: TMultivector): TMultivector;
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

function TBivector.WedgeProduct(const AVector: TTrivector): TMultivector;
begin
  result := NullMultivector;
end;

function TBivector.WedgeProduct(const AVector: TBivector): TMultivector;
begin
  result := NullMultivector;
end;

function TBivector.ToMultivector: TMultivector;
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

function TBivector.ToString: string;
begin
  result := Format('%se12 %s23 %s31', [FloatToStr(fm12), FloatToStr(fm23), FloatToStr(fm31)]);
end;

// Vector

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
            (ALeft.fm2   <> ARight.fm3) or
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
  result := ALeft / ARight;
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

function TVector.Inverse: TVector;
begin
  result.fm1 := -fm1;
  result.fm2 := -fm2;
  result.fm3 := -fm3;
end;

function TVector.Conjugate: TVector;
begin
  result.fm1 := -fm1;
  result.fm2 := -fm2;
  result.fm3 := -fm3;
end;

function TVector.Reverse: TVector;
begin
  result.fm1 := fm1;
  result.fm2 := fm2;
  result.fm3 := fm3;
end;

function TVector.SquaredNorm: double;
begin
  result := fm1*fm1 + fm2*fm2 + fm3*fm3;
end;

function TVector.Norm: double;
begin
  result := sqrt(SquaredNorm);
end;

function TVector.Normalized: TVector;
begin
  result := Self / Self.Norm;
end;

function TVector.Reciprocal: TVector;
begin
  result := Self / SquaredNorm;
end;

function TVector.Projection(const AVector: TMultivector): TMultivector;
begin
  result := ScalarProduct(AVector) * AVector.Reciprocal;
end;

function TVector.Rejection(const AVector: TMultivector): TMultivector;
begin
  result := WedgeProduct(AVector) * AVector.Reciprocal;
end;

function TVector.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TVector.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := (AVector2 * AVector1) * Self * (AVector1.Reciprocal * AVector2.Reciprocal);
end;

function TVector.ScalarProduct(const AVector: TMultivector): TMultivector;
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


function TVector.ScalarProduct(const AVector: TTrivector): TBivector;
begin
  result.fm12 := fm3 * AVector.fm123;
  result.fm23 := fm1 * Avector.fm123;
  result.fm31 := fm2 * AVector.fm123;
end;

function TVector.ScalarProduct(const AVector: TBivector): TVector;
begin
  result.fm1 := fm3 * AVector.fm31 - fm2 * AVector.fm12;
  result.fm2 := fm1 * AVector.fm12 - fm3 * AVector.fm23;
  result.fm3 := fm2 * AVector.fm23 - fm1 * AVector.fm31;
end;

function TVector.ScalarProduct(const AVector: TVector): double;
begin
 result := fm1 * AVector.fm1 + fm2 * AVector.fm2 + fm3 * AVector.fm3;
end;

function TVector.WedgeProduct(const AVector: TMultivector): TMultivector;
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

function TVector.WedgeProduct(const AVector: TTrivector): TMultivector;
begin
  result := NullMultivector;
end;

function TVector.WedgeProduct(const AVector: TBivector): TTrivector;
begin
  result.fm123 := fm1 * AVector.fm23 + fm2 * AVector.fm31 + fm3 * AVector.fm12;
end;

function TVector.WedgeProduct(const AVector: TVector): TBivector;
begin
  result.fm12 := fm1 * AVector.fm2 - fm2 * AVector.fm1;
  result.fm23 := fm2 * AVector.fm3 - fm3 * AVector.fm2;
  result.fm31 := fm3 * AVector.fm1 - fm1 * AVector.fm3;
end;

function TVector.ToMultivector: TMultivector;
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

function TVector.ToString: string;
begin
  result := Format('%se1 %se2 %se3', [FloatToStr(fm1), FloatToStr(fm2), FloatToStr(fm3)]);
end;

// MultivectorHelper

function TMultivectorHelper.ScalarProduct(const AVector: TTrivector): double;
begin
  result := -fm123 * AVector.fm123;
end;

function TMultivectorHelper.WedgeProduct (const AVector: TTrivector): double;
begin
  result := fm0 * AVector.fm123;
end;

function TMultivectorHelper.Dual: TMultivector;
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

// TBivectorHelper

function TBivectorHelper.Dual: TVector;
begin
  result.fm1 := -fm23;
  result.fm2 := -fm31;
  result.fm3 := -fm12;
end;

// TVectorHelper
function TVectorHelper.Dual: TBivector;
begin
  result.fm12 := fm3;
  result.fm23 := fm1;
  result.fm31 := fm2;
end;

// Versor

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

// Biversor

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

// Triversor

class operator TTriversor123.*(const AValue: double; const ASelf: TTriversor123): TTrivector;
begin
  result.fm123 := AValue;
end;

end.

