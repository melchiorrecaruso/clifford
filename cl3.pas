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
{$WARN 05024 OFF} // Suppress warning for unused routine parameter.
{$WARN 05033 OFF} // Suppress warning for unassigned function's return value.
{$MACRO ON}

interface

uses
  SysUtils;

type
  // TMultivectorComponents
  TMultivectorComponent  = (mc0, mc1, mc2, mc3, mc12, mc23, mc31, mc123);
  TMultivectorComponents = set of TMultivectorComponent;

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
    class operator :=(const AValue: double): TMultivector;
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

  // TTrivectorComponents
  TTrivectorComponent  = (tc123);
  TTrivectorComponents = set of TTrivectorComponent;

  // TTrivector
  TTrivector = record
  private
    fm123: double;
  public
    class operator :=(const ASelf: TTrivector): TMultivector;
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

  // TBivectorComponents
  TBivectorComponent  = (bc12, bc23, bc31);
  TBivectorComponents = set of TBivectorComponent;

  // TBivector
  TBivector = record
  private
    fm12: double;
    fm23: double;
    fm31: double;
  public
    class operator :=(const ASelf: TBivector): TMultivector;
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

  // TVectorComponents
  TVectorComponent  = (vc1, vc2, vc3);
  TVectorComponents = set of TVectorComponent;

  // TVector
  TVector = record
  private
    fm1: double;
    fm2: double;
    fm3: double;
  public
    class operator :=(const ASelf: TVector): TMultivector;
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

  // TScalarComponents
  TScalarComponent  = (sc0);
  TScalarComponents = set of TScalarComponent;

  // TMultivectorHelper
  TMultivectorHelper = record helper for TMultivector
    function Dual: TMultivector;
    function Inverse: TMultivector;
    function Reverse: TMultivector;
    function Conjugate: TMultivector;
    function Reciprocal: TMultivector;
    function Norm: double;
    function Norm(AComponents: TMultivectorComponents): double;
    function SquaredNorm: double;
    function SquaredNorm(AComponents: TMultivectorComponents): double;

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

    function SameValue(const AValue: TMultivector): boolean;
    function SameValue(const AValue: TTrivector): boolean;
    function SameValue(const AValue: TBivector): boolean;
    function SameValue(const AValue: TVector): boolean;
    function SameValue(const AValue: double): boolean;

    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;

    function Extract(AComponents: TMultivectorComponents): TMultivector;
    function Extract(AComponents: TBivectorComponents): TBivector;
    function Extract(AComponents: TVectorComponents): TVector;

    function ExtractTrivector: TTrivector;
    function ExtractBivector: TBivector;
    function ExtractVector: TVector;
    function ExtractScalar: double;

    function IsNull: boolean;
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

    function SameValue(const AValue: TMultivector): boolean;
    function SameValue(const AValue: TTrivector): boolean;

    function Extract(AComponents: TTrivectorComponents): TTrivector;

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

    function SameValue(const AValue: TMultivector): boolean;
    function SameValue(const AValue: TBivector): boolean;

    function Extract(AComponents: TBivectorComponents): TBivector;

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

    function SameValue(const AValue: TMultivector): boolean;
    function SameValue(const AValue: TVector): boolean;

    function Extract(AComponents: TVectorComponents): TVector;

    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
    function ToMultivector: TMultivector;
  end;

  // TVersor
  TVersor1 = record class operator *(const AValue: double; const ASelf: TVersor1): TVector; end;
  TVersor2 = record class operator *(const AValue: double; const ASelf: TVersor2): TVector; end;
  TVersor3 = record class operator *(const AValue: double; const ASelf: TVersor3): TVector; end;

  // TBiversor
  TVersor12 = record class operator *(const AValue: double; const ASelf: TVersor12): TBivector; end;
  TVersor23 = record class operator *(const AValue: double; const ASelf: TVersor23): TBivector; end;
  TVersor31 = record class operator *(const AValue: double; const ASelf: TVersor31): TBivector; end;

  // TTriversor
  TVersor123 = record class operator *(const AValue: double; const ASelf: TVersor123): TTrivector; end;

const
  e1   : TVersor1   = ();
  e2   : TVersor2   = ();
  e3   : TVersor3   = ();
  e12  : TVersor12  = ();
  e23  : TVersor23  = ();
  e31  : TVersor31  = ();
  e123 : TVersor123 = ();

  u1   : TVector = (fm1:1.0; fm2:0.0; fm3:0.0);
  u2   : TVector = (fm1:0.0; fm2:1.0; fm3:0.0);
  u3   : TVector = (fm1:0.0; fm2:0.0; fm3:1.0);

  u12  : TBivector = (fm12:1.0; fm23:0.0; fm31:0.0);
  u23  : TBivector = (fm12:0.0; fm23:1.0; fm31:0.0);
  u31  : TBivector = (fm12:0.0; fm23:0.0; fm31:1.0);

  u123 : TTrivector = (fm123:1);

  NullMultivector : TMultivector = (fm0:0.0; fm1:0.0; fm2:0.0; fm3:0.0; fm12:0.0; fm23:0.0; fm31:0.0; fm123:0.0);
  NullTrivector   : TTrivector   = (fm123:0.0);
  NullBivector    : TBivector    = (fm12:0.0; fm23:0.0; fm31:0.0);
  NullVector      : TVector      = (fm1: 0.0; fm2: 0.0; fm3: 0.0);
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
  result.fm0   := AValue;
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
  result.fm123 := 0.0;
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

class operator TMultivector.<>(const ALeft: TMultivector; const ARight: double): boolean;
begin
  result := (ALeft.fm0   <> ARight) or
            (ALeft.fm1   <>    0.0) or
            (ALeft.fm2   <>    0.0) or
            (ALeft.fm3   <>    0.0) or
            (ALeft.fm12  <>    0.0) or
            (ALeft.fm23  <>    0.0) or
            (ALeft.fm31  <>    0.0) or
            (ALeft.fm123 <>    0.0);
end;

class operator TMultivector.<>(const ALeft: double; const ARight: TMultivector): boolean;
begin
  result := (ALeft <> ARight.fm0  ) or
            (0.0   <> ARight.fm1  ) or
            (0.0   <> ARight.fm2  ) or
            (0.0   <> ARight.fm3  ) or
            (0.0   <> ARight.fm12 ) or
            (0.0   <> ARight.fm23 ) or
            (0.0   <> ARight.fm31 ) or
            (0.0   <> ARight.fm123);
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

class operator TTrivector.:=(const ASelf: TTrivector): TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
  result.fm123 := ASelf.fm123;
end;

class operator TTrivector.<>(const ALeft, ARight: TTrivector): boolean;
begin
  result := ALeft.fm123 <> ARight.fm123;
end;

class operator TTrivector.<>(const ALeft: TMultivector; const ARight: TTrivector): boolean;
begin
  result := (ALeft.fm0   <>          0.0) or
            (ALeft.fm1   <>          0.0) or
            (ALeft.fm2   <>          0.0) or
            (ALeft.fm3   <>          0.0) or
            (ALeft.fm12  <>          0.0) or
            (ALeft.fm23  <>          0.0) or
            (ALeft.fm31  <>          0.0) or
            (ALeft.fm123 <> ARight.fm123);
end;

class operator TTrivector.<>(const ALeft: TTrivector; const ARight: TMultivector): boolean;
begin
  result := (0.0         <> ARight.fm0  ) or
            (0.0         <> ARight.fm1  ) or
            (0.0         <> ARight.fm2  ) or
            (0.0         <> ARight.fm3  ) or
            (0.0         <> ARight.fm12 ) or
            (0.0         <> ARight.fm23 ) or
            (0.0         <> ARight.fm31 ) or
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
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
  result.fm123 := ALeft.fm123;
end;

class operator TTrivector.+(const ALeft: double; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := ALeft;
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
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
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
  result.fm123 := ALeft.fm123;
end;

class operator TTrivector.-(const ALeft: double; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := ALeft;
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
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

class operator TBivector.:=(const ASelf: TBivector): TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
  result.fm12  := ASelf.fm12;
  result.fm23  := ASelf.fm23;
  result.fm31  := ASelf.fm31;
  result.fm123 := 0.0;
end;

class operator TBivector.<>(const ALeft, ARight: TBivector): boolean;
begin
  result := (ALeft.fm12 <> ARight.fm12) or
            (ALeft.fm23 <> ARight.fm23) or
            (ALeft.fm31 <> ARight.fm31);
end;

class operator TBivector.<>(const ALeft: TMultivector; const ARight: TBivector): boolean;
begin
  result := (ALeft.fm0   <>         0.0) or
            (ALeft.fm1   <>         0.0) or
            (ALeft.fm2   <>         0.0) or
            (ALeft.fm3   <>         0.0) or
            (ALeft.fm12  <> ARight.fm12) or
            (ALeft.fm23  <> ARight.fm23) or
            (ALeft.fm31  <> ARight.fm31) or
            (ALeft.fm123 <>         0.0);
end;

class operator TBivector.<>(const ALeft: TBivector; const ARight: TMultivector): boolean;
begin
  result := (ARight.fm0   <>        0.0) or
            (ARight.fm1   <>        0.0) or
            (ARight.fm2   <>        0.0) or
            (ARight.fm3   <>        0.0) or
            (ARight.fm12  <> ALeft.fm12) or
            (ARight.fm23  <> ALeft.fm23) or
            (ARight.fm31  <> ALeft.fm31) or
            (ARight.fm123 <>        0.0);
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
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := 0.0;
end;

class operator TBivector.+(const ALeft: double; const ARight: TBivector): TMultivector;
begin
  result := ARight + ALeft;
end;

class operator TBivector.+(const ALeft: TBivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
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
  result.fm1   :=  0.0;
  result.fm2   :=  0.0;
  result.fm3   :=  0.0;
  result.fm12  :=  ALeft.fm12;
  result.fm23  :=  ALeft.fm23;
  result.fm31  :=  ALeft.fm31;
  result.fm123 :=  0.0;
end;

class operator TBivector.-(const ALeft: double; const ARight: TBivector): TMultivector;
begin
  result := ARight - ALeft;
end;

class operator TBivector.-(const ALeft: TBivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   :=  0.0;
  result.fm1   :=  0.0;
  result.fm2   :=  0.0;
  result.fm3   :=  0.0;
  result.fm12  :=  ALeft.fm12;
  result.fm23  :=  ALeft.fm23;
  result.fm31  :=  ALeft.fm31;
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
  result.fm12  :=  ALeft.fm12 - ARight.fm12;
  result.fm23  :=  ALeft.fm23 - ARight.fm23;
  result.fm31  :=  ALeft.fm31 - ARight.fm31;
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
  result.fm0   := -ALeft.fm12 * ARight.fm12
                  -ALeft.fm23 * ARight.fm23
                  -ALeft.fm31 * ARight.fm31;
  result.fm1   :=  0.0;
  result.fm2   :=  0.0;
  result.fm3   :=  0.0;
  result.fm12  :=  ALeft.fm31 * ARight.fm23
                  -ALeft.fm23 * ARight.fm31;
  result.fm23  :=  ALeft.fm12 * ARight.fm31
                  -ALeft.fm31 * ARight.fm12;
  result.fm31  :=  ALeft.fm23 * ARight.fm12
                  -ALeft.fm12 * ARight.fm23;
  result.fm123 :=  0.0;
end;

class operator TBivector.*(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0 :=   -ALeft.fm12 * ARight.fm12
                  -ALeft.fm23 * ARight.fm23
                  -ALeft.fm31 * ARight.fm31;

  result.fm1 :=    ALeft.fm12 * ARight.fm2
                  -ALeft.fm23 * ARight.fm123
                  -ALeft.fm31 * ARight.fm3;

  result.fm2 :=   -ALeft.fm12 * ARight.fm1
                  +ALeft.fm23 * ARight.fm3
                  -ALeft.fm31 * ARight.fm123;

  result.fm3 :=   -ALeft.fm12 * ARight.fm123
                  -ALeft.fm23 * ARight.fm2
                  +ALeft.fm31 * ARight.fm1;

  result.fm12 :=   ALeft.fm12 * ARight.fm0
                  -ALeft.fm23 * ARight.fm31
                  +ALeft.fm31 * ARight.fm23;

  result.fm23 :=   ALeft.fm12 * ARight.fm31
                  +ALeft.fm23 * ARight.fm0
                  -ALeft.fm31 * ARight.fm12;

  result.fm31 :=  -ALeft.fm12 * ARight.fm23
                  +ALeft.fm23 * ARight.fm12
                  +ALeft.fm31 * ARight.fm0;

  result.fm123 :=  ALeft.fm12 * ARight.fm3
                  +ALeft.fm23 * ARight.fm1
                  +ALeft.fm31 * ARight.fm2;
end;

class operator TBivector.*(const ALeft: TBivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   :=  0.0;
  result.fm1   := -ALeft.fm23 * ARight.fm123;
  result.fm2   := -ALeft.fm31 * ARight.fm123;
  result.fm3   := -ALeft.fm12 * ARight.fm123;
  result.fm12  :=  0.0;
  result.fm23  :=  0.0;
  result.fm31  :=  0.0;
  result.fm123 :=  0.0;
end;

class operator TBivector.*(const ALeft: TTrivector; const ARight: TBivector): TMultivector;
begin
  result.fm0   :=  0.0;
  result.fm1   := -ARight.fm23 * ALeft.fm123;
  result.fm2   := -ARight.fm31 * ALeft.fm123;
  result.fm3   := -ARight.fm12 * ALeft.fm123;
  result.fm12  :=  0.0;
  result.fm23  :=  0.0;
  result.fm31  :=  0.0;
  result.fm123 :=  0.0;
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

class operator TVector.:=(const ASelf: TVector): TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := ASelf.fm1;
  result.fm2   := ASelf.fm2;
  result.fm3   := ASelf.fm3;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
  result.fm123 := 0.0;
end;

class operator TVector.<>(const ALeft, ARight: TVector): boolean;
begin
  result := (ALeft.fm1 <> ARight.fm1) or
            (ALeft.fm2 <> ARight.fm2) or
            (ALeft.fm3 <> ARight.fm3);
end;

class operator TVector.<>(const ALeft: TMultivector; const ARight: TVector): boolean;
begin
  result := (ALeft.fm0   <>        0.0) or
            (ALeft.fm1   <> ARight.fm1) or
            (ALeft.fm2   <> ARight.fm2) or
            (ALeft.fm3   <> ARight.fm3) or
            (ALeft.fm12  <>        0.0) or
            (ALeft.fm23  <>        0.0) or
            (ALeft.fm31  <>        0.0) or
            (ALeft.fm123 <>        0.0);
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
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
  result.fm123 := 0.0;
end;

class operator TVector.+(const ALeft: double; const ARight: TVector): TMultivector;
begin
  result.fm0   := ALeft;
  result.fm1   := ARight.fm1;
  result.fm2   := ARight.fm2;
  result.fm3   := ARight.fm3;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
  result.fm123 := 0.0;
end;

class operator TVector.+(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := ALeft.fm1;
  result.fm2   := ALeft.fm2;
  result.fm3   := ALeft.fm3;
  result.fm12  := ARight.fm12;
  result.fm23  := ARight.fm23;
  result.fm31  := ARight.fm31;
  result.fm123 := 0.0;
end;

class operator TVector.+(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := ARight.fm1;
  result.fm2   := ARight.fm2;
  result.fm3   := ARight.fm3;
  result.fm12  := ALeft.fm12;
  result.fm23  := ALeft.fm23;
  result.fm31  := ALeft.fm31;
  result.fm123 := 0.0;
end;

class operator TVector.+(const ALeft: TVector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := ALeft.fm1;
  result.fm2   := ALeft.fm2;
  result.fm3   := ALeft.fm3;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
  result.fm123 := ARight.fm123;
end;

class operator TVector.+(const ALeft: TTrivector; const ARight: TVector): TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := ARight.fm1;
  result.fm2   := ARight.fm2;
  result.fm3   := ARight.fm3;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
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
  result.fm12  :=  0.0;
  result.fm23  :=  0.0;
  result.fm31  :=  0.0;
  result.fm123 :=  0.0;
end;

class operator TVector.-(const ALeft: double; const ARight: TVector): TMultivector;
begin
  result.fm0   :=  ALeft;
  result.fm1   := -ARight.fm1;
  result.fm2   := -ARight.fm2;
  result.fm3   := -ARight.fm3;
  result.fm12  :=  0.0;
  result.fm23  :=  0.0;
  result.fm31  :=  0.0;
  result.fm123 :=  0.0;
end;

class operator TVector.-(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fm0   :=  0.0;
  result.fm1   :=  ALeft.fm1;
  result.fm2   :=  ALeft.fm2;
  result.fm3   :=  ALeft.fm3;
  result.fm12  := -ARight.fm12;
  result.fm23  := -ARight.fm23;
  result.fm31  := -ARight.fm31;
  result.fm123 :=  0.0;
end;

class operator TVector.-(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fm0   :=  0.0;
  result.fm1   := -ARight.fm1;
  result.fm2   := -ARight.fm2;
  result.fm3   := -ARight.fm3;
  result.fm12  :=  ALeft.fm12;
  result.fm23  :=  ALeft.fm23;
  result.fm31  :=  ALeft.fm31;
  result.fm123 :=  0.0;
end;

class operator TVector.-(const ALeft: TVector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   :=  0.0;
  result.fm1   :=  ALeft.fm1;
  result.fm2   :=  ALeft.fm2;
  result.fm3   :=  ALeft.fm3;
  result.fm12  :=  0.0;
  result.fm23  :=  0.0;
  result.fm31  :=  0.0;
  result.fm123 := -ARight.fm123;
end;

class operator TVector.-(const ALeft: TTrivector; const ARight: TVector): TMultivector;
begin
  result.fm0   :=  0.0;
  result.fm1   := -ARight.fm1;
  result.fm2   := -ARight.fm2;
  result.fm3   := -ARight.fm3;
  result.fm12  :=  0.0;
  result.fm23  :=  0.0;
  result.fm31  :=  0.0;
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
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
  result.fm12  := ALeft.fm1 * ARight.fm2 - ALeft.fm2 * ARight.fm1;
  result.fm23  := ALeft.fm2 * ARight.fm3 - ALeft.fm3 * ARight.fm2;
  result.fm31  := ALeft.fm3 * ARight.fm1 - ALeft.fm1 * ARight.fm3;
  result.fm123 := 0.0;
end;

class operator TVector.*(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := ALeft.fm3 * ARight.fm31 - ALeft.fm2 * ARight.fm12;
  result.fm2   := ALeft.fm1 * ARight.fm12 - ALeft.fm3 * ARight.fm23;
  result.fm3   := ALeft.fm2 * ARight.fm23 - ALeft.fm1 * ARight.fm31;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
  result.fm123 := ALeft.fm1 * ARight.fm23 + ALeft.fm2 * ARight.fm31 + ALeft.fm3 * ARight.fm12;
end;

class operator TVector.*(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := ALeft.fm12 * ARight.fm2 - ALeft.fm31 * ARight.fm3;
  result.fm2   := ALeft.fm23 * ARight.fm3 - ALeft.fm12 * ARight.fm1;
  result.fm3   := ALeft.fm31 * ARight.fm1 - ALeft.fm23 * ARight.fm2;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
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
  result.fm0   :=  ALeft.fm1 * ARight.fm1
                  +ALeft.fm2 * ARight.fm2
                  +ALeft.fm3 * ARight.fm3;

  result.fm1   :=  ALeft.fm1 * ARight.fm0
                  -ALeft.fm2 * ARight.fm12
                  +ALeft.fm3 * ARight.fm31;

  result.fm2   :=  ALeft.fm1 * ARight.fm12
                  +ALeft.fm2 * ARight.fm0
                  -ALeft.fm3 * ARight.fm23;

  result.fm3   :=  ALeft.fm3 * ARight.fm0
                  -ALeft.fm1 * ARight.fm31
                  +ALeft.fm2 * ARight.fm23;

  result.fm12  :=  ALeft.fm1 * ARight.fm2
                  -ALeft.fm2 * ARight.fm1
                  +ALeft.fm3 * ARight.fm123;

  result.fm23  :=  ALeft.fm1 * ARight.fm123
                  +ALeft.fm2 * ARight.fm3
                  -ALeft.fm3 * ARight.fm2;

  result.fm31  :=  ALeft.fm3 * ARight.fm1
                  -ALeft.fm1 * ARight.fm3
                  +ALeft.fm2 * ARight.fm123;

  result.fm123 :=  ALeft.fm1 * ARight.fm23
                  +ALeft.fm2 * ARight.fm31
                  +ALeft.fm3 * ARight.fm12;
end;

class operator TVector.*(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result.fm0   :=  ALeft.fm1   * ARight.fm1
                  +ALeft.fm2   * ARight.fm2
                  +ALeft.fm3   * ARight.fm3;

  result.fm1   :=  ALeft.fm0   * ARight.fm1
                  +ALeft.fm12  * ARight.fm2
                  -ALeft.fm31  * ARight.fm3;

  result.fm2   :=  ALeft.fm0   * ARight.fm2
                  -ALeft.fm12  * ARight.fm1
                  +ALeft.fm23  * ARight.fm3;

  result.fm3   :=  ALeft.fm0   * ARight.fm3
                  -ALeft.fm23  * ARight.fm2
                  +ALeft.fm31  * ARight.fm1;

  result.fm12  :=  ALeft.fm1   * ARight.fm2
                  -ALeft.fm2   * ARight.fm1
                  +ALeft.fm123 * ARight.fm3;

  result.fm23  :=  ALeft.fm2   * ARight.fm3
                  -ALeft.fm3   * ARight.fm2
                  +ALeft.fm123 * ARight.fm1;

  result.fm31  :=  ALeft.fm123 * ARight.fm2
                  -ALeft.fm1   * ARight.fm3
                  +ALeft.fm3   * ARight.fm1;

  result.fm123 :=  ALeft.fm12  * ARight.fm3
                  +ALeft.fm23  * ARight.fm1
                  +ALeft.fm31  * ARight.fm2;
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
  if ((fm0  <>0) and ((fm1 <>0) or (fm2 <>0) or (fm3 <>0))) or
     ((fm123<>0) and ((fm12<>0) or (fm23<>0) or (fm31<>0))) then
  begin
    Numerator := Conjugate*Inverse*Reverse;
    result    := Numerator / (Self*Numerator).fm0;
  end else
    result := Reverse / SquaredNorm;
end;

function TMultivectorHelper.Norm(AComponents: TMultivectorComponents): double;
begin
  result := sqrt(SquaredNorm(AComponents));
end;

function TMultivectorHelper.Norm: double;
begin
  result := sqrt(SquaredNorm);
end;

function TMultivectorHelper.SquaredNorm(AComponents: TMultivectorComponents): double;
begin
  result := 0;
  if mc0   in AComponents then result := result + sqr(fm0);
  if mc1   in AComponents then result := result + sqr(fm1);
  if mc2   in AComponents then result := result + sqr(fm2);
  if mc3   in AComponents then result := result + sqr(fm3);
  if mc12  in AComponents then result := result + sqr(fm12);
  if mc23  in AComponents then result := result + sqr(fm23);
  if mc31  in AComponents then result := result + sqr(fm31);
  if mc123 in AComponents then result := result + sqr(fm123);
end;

function TMultivectorHelper.SquaredNorm: double;
begin
  result := sqr(fm0) + sqr(fm1) + sqr(fm2) + sqr(fm3) +sqr(fm12) + sqr(fm23) + sqr(fm31) + sqr(fm123);
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
  result.fm123 :=  0.0;
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
  result.fm0 :=    fm0   * AVector.fm0
                  +fm1   * AVector.fm1
                  +fm2   * AVector.fm2
                  +fm3   * AVector.fm3
                  -fm12  * AVector.fm12
                  -fm23  * AVector.fm23
                  -fm31  * AVector.fm31
                  -fm123 * AVector.fm123;

  result.fm1 :=    fm0   * AVector.fm1
                  +fm1   * AVector.fm0
                  -fm2   * AVector.fm12
                  +fm3   * AVector.fm31
                  +fm12  * AVector.fm2
                  -fm23  * AVector.fm123
                  -fm31  * AVector.fm3
                  -fm123 * AVector.fm23;

  result.fm2 :=    fm0   * AVector.fm2
                  +fm1   * AVector.fm12
                  +fm2   * AVector.fm0
                  -fm3   * AVector.fm23
                  -fm12  * AVector.fm1
                  +fm23  * AVector.fm3
                  -fm31  * AVector.fm123
                  -fm123 * AVector.fm31;

  result.fm3 :=    fm0   * AVector.fm3
                  -fm1   * AVector.fm31
                  +fm2   * AVector.fm23
                  +fm3   * AVector.fm0
                  -fm12  * AVector.fm123
                  -fm23  * AVector.fm2
                  +fm31  * AVector.fm1
                  -fm123 * AVector.fm12;

  result.fm12 :=   fm0   * AVector.fm12
                  +fm3   * AVector.fm123
                  +fm12  * AVector.fm0
                  +fm123 * AVector.fm3;

  result.fm23 :=   fm0   * AVector.fm23
                  +fm1   * Avector.fm123
                  +fm23  * AVector.fm0
                  +fm123 * AVector.fm1;

  result.fm31 :=   fm0   * AVector.fm31
                  +fm2   * AVector.fm123
                  +fm31  * AVector.fm0
                  +fm123 * AVector.fm2;

  result.fm123 :=  fm0   * AVector.fm123
                  +fm123 * AVector.fm0;
end;

function TMultivectorHelper.Wedge(const AVector: TVector): TMultivector;
begin
  result.fm0   :=  0.0;
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
  result.fm0   :=  0.0;
  result.fm1   :=  0.0;
  result.fm2   :=  0.0;
  result.fm3   :=  0.0;
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
  result.fm0   :=  fm0   * AVector.fm0;

  result.fm1   :=  fm0   * AVector.fm1
                  +fm1   * AVector.fm0;

  result.fm2   :=  fm0   * AVector.fm2
                  +fm2   * AVector.fm0;

  result.fm3   :=  fm0   * AVector.fm3
                  +fm3   * AVector.fm0;

  result.fm12  :=  fm0   * AVector.fm12
                  +fm1   * AVector.fm2
                  -fm2   * AVector.fm1
                  +fm12  * AVector.fm0;

  result.fm23  :=  fm0   * AVector.fm23
                  +fm2   * AVector.fm3
                  -fm3   * AVector.fm2
                  +fm23  * AVector.fm0;

  result.fm31  :=  fm0   * AVector.fm31
                  -fm1   * AVector.fm3
                  +fm3   * AVector.fm1
                  +fm31  * AVector.fm0;

  result.fm123 := +fm0   * AVector.fm123
                  +fm1   * AVector.fm23
                  +fm2   * AVector.fm31
                  +fm3   * AVector.fm12
                  +fm12  * AVector.fm3
                  +fm23  * AVector.fm1
                  +fm31  * AVector.fm2
                  +fm123 * AVector.fm0;
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

function TMultivectorHelper.SameValue(const AValue: TMultivector): boolean;
begin
  result := Math.SameValue(fm0,   AValue.fm0  ) and
            Math.SameValue(fm1,   AValue.fm1  ) and
            Math.SameValue(fm2,   AValue.fm2  ) and
            Math.SameValue(fm3,   AValue.fm3  ) and
            Math.SameValue(fm12,  AValue.fm12 ) and
            Math.SameValue(fm23,  AValue.fm23 ) and
            Math.SameValue(fm31,  AValue.fm31 ) and
            Math.SameValue(fm123, AValue.fm123);
end;

function TMultivectorHelper.SameValue(const AValue: TTrivector): boolean;
begin
  result := Math.SameValue(fm0,            0.0) and
            Math.SameValue(fm1,            0.0) and
            Math.SameValue(fm2,            0.0) and
            Math.SameValue(fm3,            0.0) and
            Math.SameValue(fm12,           0.0) and
            Math.SameValue(fm23,           0.0) and
            Math.SameValue(fm31,           0.0) and
            Math.SameValue(fm123, AValue.fm123);
end;

function TMultivectorHelper.SameValue(const AValue: TBivector): boolean;
begin
  result := Math.SameValue(fm0,           0.0) and
            Math.SameValue(fm1,           0.0) and
            Math.SameValue(fm2,           0.0) and
            Math.SameValue(fm3,           0.0) and
            Math.SameValue(fm12,  AValue.fm12) and
            Math.SameValue(fm23,  AValue.fm23) and
            Math.SameValue(fm31,  AValue.fm31) and
            Math.SameValue(fm123,         0.0);
end;

function TMultivectorHelper.SameValue(const AValue: TVector): boolean;
begin
  result := Math.SameValue(fm0,          0.0) and
            Math.SameValue(fm1,   AValue.fm1) and
            Math.SameValue(fm2,   AValue.fm2) and
            Math.SameValue(fm3,   AValue.fm3) and
            Math.SameValue(fm12,         0.0) and
            Math.SameValue(fm23,         0.0) and
            Math.SameValue(fm31,         0.0) and
            Math.SameValue(fm123,        0.0);
end;

function TMultivectorHelper.SameValue(const AValue: double): boolean;
begin
  result := Math.SameValue(fm0,   AValue) and
            Math.SameValue(fm1,      0.0) and
            Math.SameValue(fm2,      0.0) and
            Math.SameValue(fm3,      0.0) and
            Math.SameValue(fm12,     0.0) and
            Math.SameValue(fm23,     0.0) and
            Math.SameValue(fm31,     0.0) and
            Math.SameValue(fm123,    0.0);
end;

function TMultivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm0,   0.0) then result := result + Fmt(fm0,   APrecision, ADigits) + ' ';
  if not Math.SameValue(fm1,   0.0) then result := result + Fmt(fm1,   APrecision, ADigits) + 'e1 ';
  if not Math.SameValue(fm2,   0.0) then result := result + Fmt(fm2,   APrecision, ADigits) + 'e2 ';
  if not Math.SameValue(fm3,   0.0) then result := result + Fmt(fm3,   APrecision, ADigits) + 'e3 ';
  if not Math.SameValue(fm12,  0.0) then result := result + Fmt(fm12,  APrecision, ADigits) + 'e12 ';
  if not Math.SameValue(fm23,  0.0) then result := result + Fmt(fm23,  APrecision, ADigits) + 'e23 ';
  if not Math.SameValue(fm31,  0.0) then result := result + Fmt(fm31,  APrecision, ADigits) + 'e31 ';
  if not Math.SameValue(fm123, 0.0) then result := result + Fmt(fm123, APrecision, ADigits) + 'e123 ';

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
  if not Math.SameValue(fm0,   0.0) then result := result + Fmt(fm0  ) + ' ';
  if not Math.SameValue(fm1,   0.0) then result := result + Fmt(fm1  ) + 'e1 ';
  if not Math.SameValue(fm2,   0.0) then result := result + Fmt(fm2  ) + 'e2 ';
  if not Math.SameValue(fm3,   0.0) then result := result + Fmt(fm3  ) + 'e3 ';
  if not Math.SameValue(fm12,  0.0) then result := result + Fmt(fm12 ) + 'e12 ';
  if not Math.SameValue(fm23,  0.0) then result := result + Fmt(fm23 ) + 'e23 ';
  if not Math.SameValue(fm31,  0.0) then result := result + Fmt(fm31 ) + 'e31 ';
  if not Math.SameValue(fm123, 0.0) then result := result + Fmt(fm123) + 'e123 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0';
end;

function TMultivectorHelper.Extract(AComponents: TMultivectorComponents): TMultivector;
begin
  Result := NullMultivector;
  if mc0   in AComponents then result.fm0   := fm0;
  if mc1   in AComponents then result.fm1   := fm1;
  if mc2   in AComponents then result.fm2   := fm2;
  if mc3   in AComponents then result.fm3   := fm3;
  if mc12  in AComponents then result.fm12  := fm12;
  if mc23  in AComponents then result.fm23  := fm23;
  if mc31  in AComponents then result.fm31  := fm31;
  if mc123 in AComponents then result.fm123 := fm123;
end;

function TMultivectorHelper.Extract(AComponents: TBivectorComponents): TBivector;
begin
  Result := NullBivector;
  if bc12 in AComponents then result.fm12 := fm12;
  if bc23 in AComponents then result.fm23 := fm23;
  if bc31 in AComponents then result.fm31 := fm31;
end;

function TMultivectorHelper.Extract(AComponents: TVectorComponents): TVector;
begin
  Result := NullVector;
  if vc1 in AComponents then result.fm1 := fm1;
  if vc2 in AComponents then result.fm2 := fm2;
  if vc3 in AComponents then result.fm3 := fm3;
end;

function TMultivectorHelper.ExtractTrivector: TTrivector;
begin
  result.fm123 := fm123;
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

function TMultivectorHelper.IsNull: boolean;
begin
  result := SameValue(NullMultivector);
end;

function TMultivectorHelper.IsScalar: boolean;
begin
  result := (not Math.SameValue(fm0,   0.0)) and
            (    Math.SameValue(fm1,   0.0)) and
            (    Math.SameValue(fm2,   0.0)) and
            (    Math.SameValue(fm3,   0.0)) and
            (    Math.SameValue(fm12,  0.0)) and
            (    Math.SameValue(fm23,  0.0)) and
            (    Math.SameValue(fm31,  0.0)) and
            (    Math.SameValue(fm123, 0.0));
end;

function TMultivectorHelper.IsVector: boolean;
begin
  result :=  (    Math.SameValue(fm0,   0.0))  and
            ((not Math.SameValue(fm1,   0.0))  or
             (not Math.SameValue(fm2,   0.0))  or
             (not Math.SameValue(fm3,   0.0))) and
             (    Math.SameValue(fm12,  0.0))  and
             (    Math.SameValue(fm23,  0.0))  and
             (    Math.SameValue(fm31,  0.0))  and
             (    Math.SameValue(fm123, 0.0));
end;

function TMultivectorHelper.IsBiVector: boolean;
begin
  result :=  (    Math.SameValue(fm0,   0.0))  and
             (    Math.SameValue(fm1,   0.0))  and
             (    Math.SameValue(fm2,   0.0))  and
             (    Math.SameValue(fm3,   0.0))  and
            ((not Math.SameValue(fm12,  0.0))  or
             (not Math.SameValue(fm23,  0.0))  or
             (not Math.SameValue(fm31,  0.0))) and
             (    Math.SameValue(fm123, 0.0));
end;

function TMultivectorHelper.IsTrivector: boolean;
begin
  result := (    Math.SameValue(fm0,   0.0)) and
            (    Math.SameValue(fm1,   0.0)) and
            (    Math.SameValue(fm2,   0.0)) and
            (    Math.SameValue(fm3,   0.0)) and
            (    Math.SameValue(fm12,  0.0)) and
            (    Math.SameValue(fm23,  0.0)) and
            (    Math.SameValue(fm31,  0.0)) and
            (not Math.SameValue(fm123, 0.0));
end;

function TMultivectorHelper.IsA: string;
begin
  if IsNull      then Result := 'Null'       else
  if IsTrivector then Result := 'TTrivector' else
  if IsBivector  then Result := 'TBivector'  else
  if IsVector    then Result := 'TVector'    else
  if IsScalar    then Result := 'TScalar'    else Result := 'TMultivector';
end;

// TTrivectorHelper

function TTrivectorHelper.Dual: double;
begin
  result := -fm123; // Self * e123
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
  result := 0.0;
end;

function TTrivectorHelper.Wedge(const AVector: TBivector): double;
begin
  result := 0.0;
end;

function TTrivectorHelper.Wedge(const AVector: TTrivector): double;
begin
  result := 0.0;
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
  result := 0.0;
end;

function TTrivectorHelper.Rejection(const AVector: TBivector): double;
begin
  result := 0.0;
end;

function TTrivectorHelper.Rejection(const AVector: TTrivector): double;
begin
  result := 0.0;
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

function TTrivectorHelper.SameValue(const AValue: TMultivector): boolean;
begin
  result := Math.SameValue(0.0,   AValue.fm0  ) and
            Math.SameValue(0.0,   AValue.fm1  ) and
            Math.SameValue(0.0,   AValue.fm2  ) and
            Math.SameValue(0.0,   AValue.fm3  ) and
            Math.SameValue(0.0,   AValue.fm12 ) and
            Math.SameValue(0.0,   AValue.fm23 ) and
            Math.SameValue(0.0,   AValue.fm31 ) and
            Math.SameValue(fm123, AValue.fm123);
end;

function TTrivectorHelper.SameValue(const AValue: TTrivector): boolean;
begin
  result := Math.SameValue(fm123, AValue.fm123);
end;


function TTrivectorHelper.Extract(AComponents: TTrivectorComponents): TTrivector;
begin
  Result := NullTrivector;
  if tc123 in AComponents then result.fm123 := fm123;
end;


function TTrivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
begin
  if not Math.SameValue(fm123, 0.0) then
    result := Fmt(fm123, APrecision, ADigits) + 'e123'
  else
    result := '0e123';
end;

function TTrivectorHelper.ToString: string;
begin
  if not Math.SameValue(fm123, 0.0) then
    result := Fmt(fm123) + 'e123'
  else
    result := '0e123';
end;

function TTrivectorHelper.ToMultivector: TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
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
  result := fm12*fm12 + fm23*fm23 + fm31*fm31; // Self * Reverse;
end;

function TBivectorHelper.Dot(const AVector: TVector): TVector;
begin
  result.fm1 := fm12 * AVector.fm2 - fm31 * AVector.fm3;
  result.fm2 := fm23 * AVector.fm3 - fm12 * AVector.fm1;
  result.fm3 := fm31 * AVector.fm1 - fm23 * AVector.fm2;
end;

function TBivectorHelper.Dot(const AVector: TBivector): double;
begin
  result := -fm12 * AVector.fm12
            -fm23 * AVector.fm23
            -fm31 * AVector.fm31;
end;

function TBivectorHelper.Dot(const AVector: TTrivector): TVector;
begin
  result.fm1 := -fm23 * AVector.fm123;
  result.fm2 := -fm31 * AVector.fm123;
  result.fm3 := -fm12 * AVector.fm123;
end;

function TBivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0   := -fm12 * AVector.fm12  - fm23 * AVector.fm23  - fm31 * AVector.fm31;
  result.fm1   := +fm12 * AVector.fm2   - fm23 * AVector.fm123 - fm31 * AVector.fm3;
  result.fm2   := -fm12 * AVector.fm1   - fm31 * AVector.fm123 + fm23 * AVector.fm3;
  result.fm3   := -fm12 * AVector.fm123 - fm23 * AVector.fm2   + fm31 * AVector.fm1;
  result.fm12  :=  fm12 * AVector.fm0;
  result.fm23  :=  fm23 * AVector.fm0;
  result.fm31  :=  fm31 * AVector.fm0;
  result.fm123 :=  0.0;
end;

function TBivectorHelper.Wedge(const AVector: TVector): TTrivector;
begin
  result.fm123 := fm12 * AVector.fm3 + fm23 * AVector.fm1 + fm31 * AVector.fm2;
end;

function TBivectorHelper.Wedge(const AVector: TBivector): double;
begin
  result := 0.0;
end;

function TBivectorHelper.Wedge(const AVector: TTrivector): double;
begin
  result := 0.0;
end;

function TBivectorHelper.Wedge (const AVector: TMultivector): TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
  result.fm12  := fm12 * AVector.fm0;
  result.fm23  := fm23 * AVector.fm0;
  result.fm31  := fm31 * AVector.fm0;
  result.fm123 := fm12 * AVector.fm3 + fm23 * AVector.fm1 + fm31 * AVector.fm2;
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
  result := 0.0;
end;

function TBivectorHelper.Rejection (const AVector: TTrivector): double;
begin
  result := 0.0;
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

function TBivectorHelper.SameValue(const AValue: TMultivector): boolean;
begin
  result := Math.SameValue(0.0,  AValue.fm0  ) and
            Math.SameValue(0.0,  AValue.fm1  ) and
            Math.SameValue(0.0,  AValue.fm2  ) and
            Math.SameValue(0.0,  AValue.fm3  ) and
            Math.SameValue(fm12, AValue.fm12 ) and
            Math.SameValue(fm23, AValue.fm23 ) and
            Math.SameValue(fm31, AValue.fm31 ) and
            Math.SameValue(0.0,  AValue.fm123);
end;

function TBivectorHelper.SameValue(const AValue: TBivector): boolean;
begin
  result := Math.SameValue(fm12, AValue.fm12) and
            Math.SameValue(fm23, AValue.fm23) and
            Math.SameValue(fm31, AValue.fm31);
end;

function TBivectorHelper.Extract(AComponents: TBivectorComponents): TBivector;
begin
  Result := NullBivector;
  if bc12 in AComponents then result.fm12 := fm12;
  if bc23 in AComponents then result.fm23 := fm23;
  if bc31 in AComponents then result.fm31 := fm31;
end;

function TBivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  Result := '';
  if not Math.SameValue(fm12, 0.0) then Result := Result + Fmt(fm12,  APrecision, ADigits) + 'e12 ';
  if not Math.SameValue(fm23, 0.0) then Result := Result + Fmt(fm23,  APrecision, ADigits) + 'e23 ';
  if not Math.SameValue(fm31, 0.0) then Result := Result + Fmt(fm31,  APrecision, ADigits) + 'e31 ';

  i := Length(Result);
  if i > 0 then
    SetLength(Result, i - 1)
  else
    Result := '0e12';
end;

function TBivectorHelper.ToString: string;
var
  i: longint;
begin
  Result := '';
  if not Math.SameValue(fm12, 0.0) then Result := Result + Fmt(fm12) + 'e12 ';
  if not Math.SameValue(fm23, 0.0) then Result := Result + Fmt(fm23) + 'e23 ';
  if not Math.SameValue(fm31, 0.0) then Result := Result + Fmt(fm31) + 'e31 ';

  i := Length(Result);
  if i > 0 then
    SetLength(Result, i - 1)
  else
    Result := '0e12';
end;

function TBivectorHelper.ToMultivector: TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
  result.fm12  := fm12;
  result.fm23  := fm23;
  result.fm31  := fm31;
  result.fm123 := 0.0;
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
  result.fm123 := 0.0;
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
  result := 0.0;
end;

function TVectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fm0   := 0.0;
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
  result := 0.0;
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

function TVectorHelper.SameValue(const AValue: TMultivector): boolean;
begin
  result := Math.SameValue(0.0, AValue.fm0  ) and
            Math.SameValue(fm1, AValue.fm1  ) and
            Math.SameValue(fm2, AValue.fm2  ) and
            Math.SameValue(fm3, AValue.fm3  ) and
            Math.SameValue(0.0, AValue.fm12 ) and
            Math.SameValue(0.0, AValue.fm23 ) and
            Math.SameValue(0.0, AValue.fm31 ) and
            Math.SameValue(0.0, AValue.fm123);
end;

function TVectorHelper.SameValue(const AValue: TVector): boolean;
begin
  result := Math.SameValue(fm1, AValue.fm1) and
            Math.SameValue(fm2, AValue.fm2) and
            Math.SameValue(fm3, AValue.fm3);
end;

function TVectorHelper.Extract(AComponents: TVectorComponents): TVector;
begin
  Result := NullVector;
  if vc1 in AComponents then result.fm1 := fm1;
  if vc2 in AComponents then result.fm2 := fm2;
  if vc3 in AComponents then result.fm3 := fm3;
end;


function TVectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm1, 0.0) then Result := Result + Fmt(fm1,  APrecision, ADigits) + 'e1 ';
  if not Math.SameValue(fm2, 0.0) then Result := Result + Fmt(fm2,  APrecision, ADigits) + 'e2 ';
  if not Math.SameValue(fm3, 0.0) then Result := Result + Fmt(fm3,  APrecision, ADigits) + 'e3 ';

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
  if not Math.SameValue(fm3, 0.0) then Result := Result + Fmt(fm3) + 'e3 ';

  i := Length(Result);
  if i > 0 then
    SetLength(Result, i - 1)
  else
    Result := '0e1';
end;

function TVectorHelper.ToMultivector: TMultivector;
begin
  result.fm0   := 0.0;
  result.fm1   := fm1;
  result.fm2   := fm2;
  result.fm3   := fm3;
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
  result.fm123 := 0.0;
end;

// TVersor

class operator TVersor1.*(const AValue: double; const ASelf: TVersor1): TVector;
begin
  result.fm1   := AValue;
  result.fm2   := 0.0;
  result.fm3   := 0.0;
end;

class operator TVersor2.*(const AValue: double; const ASelf: TVersor2): TVector;
begin
  result.fm1   := 0.0;
  result.fm2   := AValue;
  result.fm3   := 0.0;
end;

class operator TVersor3.*(const AValue: double; const ASelf: TVersor3): TVector;
begin
  result.fm1   := 0.0;
  result.fm2   := 0.0;
  result.fm3   := AValue;
end;

class operator TVersor12.*(const AValue: double; const ASelf: TVersor12): TBivector;
begin
  result.fm12  := AValue;
  result.fm23  := 0.0;
  result.fm31  := 0.0;
end;

class operator TVersor23.*(const AValue: double; const ASelf: TVersor23): TBivector;
begin
  result.fm12  := 0.0;
  result.fm23  := AValue;
  result.fm31  := 0.0;
end;

class operator TVersor31.*(const AValue: double; const ASelf: TVersor31): TBivector;
begin
  result.fm12  := 0.0;
  result.fm23  := 0.0;
  result.fm31  := AValue;
end;

class operator TVersor123.*(const AValue: double; const ASelf: TVersor123): TTrivector;
begin
  result.fm123 := AValue;
end;

end.

