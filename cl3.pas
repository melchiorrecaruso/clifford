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

{$mode objfpc}{$h+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils;

type
  // TMultivector
  TMultivector = record
  private
    fm0   : double;
    fm1   : double;
    fm2   : double;
    fm3   : double;
    fm12  : double;
    fm23  : double;
    fm31  : double;
    fm123 : double;
  public
    class operator = (const ALeft, ARight: TMultivector): boolean;
    class operator = (const ALeft: TMultivector; const ARight: double): boolean;
    class operator = (const ALeft: double; const ARight: TMultivector): boolean;

    class operator <>(const ALeft, ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; const ARight: double): boolean;
    class operator <>(const ALeft: double; const ARight: TMultivector): boolean;

    class operator + (const ASelf: TMultivector): TMultivector;
    class operator + (const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator + (const ALeft: double; const ARight: TMultivector): TMultivector;
    class operator + (const ALeft, ARight: TMultivector): TMultivector;

    class operator - (const ASelf: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator - (const ALeft: double; const ARight: TMultivector): TMultivector;
    class operator - (const ALeft, ARight: TMultivector): TMultivector;

    class operator * (const ALeft: double; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator * (const ALeft, ARight: TMultivector): TMultivector;

    class operator / (const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator / (const ALeft: double; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft, ARight: TMultivector): TMultivector;

    function SquaredNorm: TMultivector;
    function Norm: double;

    function Involute: TMultivector;
    function Conjugate: TMultivector;
    function Reverse: TMultivector;
    function Dual: TMultivector;

    function Reciprocal: TMultivector;
    function Projection(const AVector: TMultivector): TMultivector;
    function Rejection (const AVector: TMultivector): TMultivector;
    function Reflection(const AVector: TMultivector): TMultivector;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;

    function ScalarProduct(const AVector: TMultivector): TMultivector;
    function WedgeProduct (const AVector: TMultivector): TMultivector;

    function ToString: string;
  end;

  // Trivector
  TTrivector = record
  private
    fm123 : double;
  public
    class operator = (const ALeft, ARight: TTrivector): boolean;
    class operator = (const ALeft: TTrivector; const ARight: TMultivector): boolean;
    class operator = (const ALeft: TMultivector; const ARight: TTrivector): boolean;

    class operator <>(const ALeft, ARight: TTrivector): boolean;
    class operator <>(const ALeft: TTrivector; const ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; const ARight: TTrivector): boolean;

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

    class operator * (const ALeft: double; const ARight: TTrivector): TTrivector;
    class operator * (const ALeft: TTrivector; const ARight: double): TTrivector;
    class operator * (const ALeft, ARight: TTrivector): double;
    class operator * (const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: TTrivector): TMultivector;


    class operator / (const ALeft, ARight: TTrivector): double;
    class operator / (const ALeft: TTrivector; const ARight: double): TTrivector;
    class operator / (const ALeft: double; const ARight: TTrivector): TTrivector;
    class operator / (const ALeft: TMultivector; const ARight: TTrivector): TMultivector;
    class operator / (const ALeft: TTrivector; const ARight: TMultivector): TMultivector;




    function SquaredNorm: double;
    function Norm: double;

    function Involute: TTrivector;
    function Conjugate: TTrivector;
    function Reverse: TTrivector;
    function Dual: double;

    function Reciprocal: TTrivector;
    function Projection(const AVector: TMultivector): TMultivector;
    function Rejection (const AVector: TMultivector): TMultivector;
    function Reflection(const AVector: TMultivector): TMultivector;
    function Rotation(const AVector1, AVector2: TTrivector): TTrivector;

    function ScalarProduct(const AVector: TMultivector): TMultivector;
    function ScalarProduct(const AVector: TTrivector): double;

    function WedgeProduct (const AVector: TMultivector): TTrivector;
    function WedgeProduct (const AVector: TTrivector): double;

    function ToString: string;
    function ToMultivector: TMultivector;
  end;

  // Bivector
  TBivector = record
  private
    fm12 : double;
    fm23 : double;
    fm31 : double;
  public

    class operator +(const ALeft, ARight: TBivector): TBivector;


    class operator +(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator +(const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    function ToMultivector: TMultivector;
  end;

  // Vector
  TVector = record
  private
    fm1 : double;
    fm2 : double;
    fm3 : double;
  public

    class operator +(const ALeft: TVector; const ARight: double): TMultivector;
    class operator +(const ALeft: double; const ARight: TVector): TMultivector;

    class operator +(const ALeft, ARight: TVector): TVector;

    class operator +(const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator +(const ALeft: TBivector; const ARight: TVector): TMultivector;

    class operator +(const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator +(const ALeft: TMultivector; const ARight: TVector): TMultivector;


    function ToMultivector: TMultivector;
  end;


  TTrivectorHelper = record helper for TTrivector
    //function ScalarProduct(A


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

implementation

// TMultivector class: boolean operators

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
  result.fm0   := - ASelf.fm0;
  result.fm1   := - ASelf.fm1;
  result.fm2   := - ASelf.fm2;
  result.fm3   := - ASelf.fm3;
  result.fm12  := - ASelf.fm12;
  result.fm23  := - ASelf.fm23;
  result.fm31  := - ASelf.fm31;
  result.fm123 := - ASelf.fm123;
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
  result := - ARight + ALeft;
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

function TMultivector.Norm: double;
begin
  result := sqrt(abs(SquaredNorm.fm0));
end;

function TMultivector.Involute: TMultivector;
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

function TMultivector.Dual: TMultivector;
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

function TMultivector.Reciprocal: TMultivector;
var
  Numerator: TMultivector;
begin
  Numerator := Conjugate*Involute*Reverse;
  result    := Numerator / (Self*Numerator).fm0;
  // the product "Self*Conjugate*Involute*Reverse" is always a scalar.
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
begin
  result := (AVector2 * AVector1) * Self * (AVector1.Reciprocal * AVector2.Reciprocal);
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
                  - fm31  * AVector.fm123
                  + fm23  * AVector.fm3
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
var
  len: longint;
begin
  result := '';
  if Abs(fm0)   > 1E-12 then result := result + FloatToStr(fm0  ) + ' '   ;
  if Abs(fm1)   > 1E-12 then result := result + FloatToStr(fm1  ) + 'e1 ' ;
  if Abs(fm2)   > 1E-12 then result := result + FloatToStr(fm2  ) + 'e2 ' ;
  if Abs(fm3)   > 1E-12 then result := result + FloatToStr(fm3  ) + 'e3 ' ;
  if Abs(fm12)  > 1E-12 then result := result + FloatToStr(fm12 ) + 'e12 ';
  if Abs(fm23)  > 1E-12 then result := result + FloatToStr(fm23 ) + 'e23 ';
  if Abs(fm31)  > 1E-12 then result := result + FloatToStr(fm31 ) + 'e31 ';
  if Abs(fm123) > 1E-12 then result := result + FloatToStr(fm123) + 'e123';

  len := Length(result);
  if (len > 0) then
    if result[len -1] = ' ' then
    begin
      SetLength(result, len -1);
    end;
end;

// Trivector

class operator TTrivector.<>(const ALeft, ARight: TTrivector): boolean;
begin
  result := ALeft <> ARight;
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
  result := ALeft = ARight;
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
  result.fm1   := ALeft.fm0;
  result.fm2   := ALeft.fm0;
  result.fm3   := ALeft.fm0;
  result.fm12  := ALeft.fm0;
  result.fm23  := ALeft.fm0;
  result.fm31  := ALeft.fm0;
  result.fm123 := ALeft.fm0 + ARight.fm123;
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
  result.fm123 :=  0;
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
  result := sqrt(abs(SquaredNorm));
end;

function TTrivector.Involute: TTrivector;
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

function TTrivector.Dual: double;
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

function TTrivector.Rotation(const AVector1, AVector2: TTrivector): TTrivector;
begin
  result := Self;
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

function TTrivector.WedgeProduct(const AVector: TMultivector): TTrivector;
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
  result := '';
  if Abs(fm123) > 1E-12 then result := result + FloatToStr(fm123) + 'e123';
end;

// Bivector

class operator TBivector.+(const ALeft, ARight: TBivector): TBivector;
begin
  result.fm12 := ALeft.fm12 + ARight.fm12;
  result.fm23 := ALeft.fm23 + ARight.fm23;
  result.fm31 := ALeft.fm31 + ARight.fm31;
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
  result.fm0   := ALeft.fm0;
  result.fm1   := ALeft.fm1;
  result.fm2   := ALeft.fm2;
  result.fm3   := ALeft.fm3;
  result.fm12  := ALeft.fm12 + ARight.fm12;
  result.fm23  := ALeft.fm23 + ARight.fm23;
  result.fm31  := ALeft.fm31 + ARight.fm31;
  result.fm123 := ALeft.fm123;
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




// Vector

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

