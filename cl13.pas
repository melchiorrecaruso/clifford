unit cl13;

{ Geometric Algebra Cl(13) for FreePascal.

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

{ References:
  - Chris Doran, Anthony Lasenby,
    Geometric algebra for Physicists, (2003) Cambridge University Press.
  - Eckhard Hitzer, Stephen Sangwine,
    Multivector and multivector matrix inverses in real Clifford algebras,
    (2016) Preprint: Technical Report CES-534, ISSN: 1744-8050.
  - James M. Chappell, Azhar Iqbal, Lachlan J. Gunn, Derek Abbott,
    Function of multivector variables, (2015) PLoS One.
}

{$H+}{$J-}
{$mode objfpc}{$h+}
{$modeswitch advancedrecords}
{$WARN 5024 OFF}// Suppress warning for unused routine parameter.
{$WARN 5033 OFF}// Suppress warning for unassigned function's return value.
{$MACRO ON}

interface

uses
  SysUtils;

type
  // TMultivector components
  TMultivectorComponent = (mcs, mc0, mc1, mc2, mc3, mc01, mc02, mc03, mc12, mc13, mc23, mc012, mc013, mc023, mc123, mc0123);
  TMultivectorComponents = set of TMultivectorComponent;

  // TMultivector
  TMultivector = record
  private
    fms:    double;
    fm0:    double;
    fm1:    double;
    fm2:    double;
    fm3:    double;
    fm01:   double;
    fm02:   double;
    fm03:   double;
    fm12:   double;
    fm13:   double;
    fm23:   double;
    fm012:  double;
    fm013:  double;
    fm023:  double;
    fm123:  double;
    fm0123: double;
  public
    class operator :=(const AValue: double): TMultivector;
    class operator =(const ALeft: TMultivector; ARight: double): boolean;
    class operator =(const ALeft: double; ARight: TMultivector): boolean;
    class operator =(const ALeft, ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; ARight: double): boolean;
    class operator <>(const ALeft: double; ARight: TMultivector): boolean;
    class operator <>(const ALeft, ARight: TMultivector): boolean;
    class operator +(const AValue: TMultivector): TMultivector;
    class operator +(const ALeft: TMultivector; ARight: double): TMultivector;
    class operator +(const ALeft: double; ARight: TMultivector): TMultivector;
    class operator +(const ALeft, ARight: TMultivector): TMultivector;
    class operator -(const AValue: TMultivector): TMultivector;
    class operator -(const ALeft: TMultivector; ARight: double): TMultivector;
    class operator -(const ALeft: double; ARight: TMultivector): TMultivector;
    class operator -(const ALeft, ARight: TMultivector): TMultivector;
    class operator *(const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator *(const ALeft: double; const ARight: TMultivector): TMultivector;
    class operator *(const ALeft, ARight: TMultivector): TMultivector;
    class operator /(const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator /(const ALeft: double; const ARight: TMultivector): TMultivector;
    class operator /(const ALeft, ARight: TMultivector): TMultivector;
  end;

  // TQuadrivector
  TQuadrivector = record
  private
    fm0123: double;
  public
    class operator =(const ALeft, ARight: TQuadrivector): boolean;
    class operator =(const ALeft: TQuadrivector; ARight: TMultivector): boolean;
    class operator =(const ALeft: TMultivector; ARight: TQuadrivector): boolean;
    class operator <>(const ALeft, ARight: TQuadrivector): boolean;
    class operator <>(const ALeft: TQuadrivector; ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; ARight: TQuadrivector): boolean;
    class operator +(const AValue: TQuadrivector): TQuadrivector;
    class operator +(const ALeft: TQuadrivector; ARight: double): TMultivector;
    class operator +(const ALeft: double; ARight: TQuadrivector): TMultivector;
    class operator +(const ALeft, ARight: TQuadrivector): TQuadrivector;
    class operator +(const ALeft: TQuadrivector; ARight: TMultivector): TMultivector;
    class operator +(const ALeft: TMultivector; ARight: TQuadrivector): TMultivector;
    class operator -(const AValue: TQuadrivector): TQuadrivector;
    class operator -(const ALeft: TQuadrivector; ARight: double): TMultivector;
    class operator -(const ALeft: double; ARight: TQuadrivector): TMultivector;
    class operator -(const ALeft, ARight: TQuadrivector): TQuadrivector;
    class operator -(const ALeft: TQuadrivector; ARight: TMultivector): TMultivector;
    class operator -(const ALeft: TMultivector; ARight: TQuadrivector): TMultivector;
    class operator *(const ALeft: TQuadrivector; const ARight: double): TQuadrivector;
    class operator *(const ALeft: double; const ARight: TQuadrivector): TQuadrivector;
    class operator *(const ALeft, ARight: TQuadrivector): double;
    class operator *(const ALeft: TQuadrivector; const ARight: TMultivector): TMultivector;
    class operator *(const ALeft: TMultivector; const ARight: TQuadrivector): TMultivector;
    class operator /(const ALeft: TQuadrivector; const ARight: double): TQuadrivector;
    class operator /(const ALeft: double; const ARight: TQuadrivector): TQuadrivector;
    class operator /(const ALeft, ARight: TQuadrivector): double;
    class operator /(const ALeft: TQuadrivector; const ARight: TMultivector): TMultivector;
    class operator /(const ALeft: TMultivector; const ARight: TQuadrivector): TMultivector;
  end;

  // TTrivectorComponents
  TTrivectorComponent  = (tc012, tc013, tc023, tc123);
  TTrivectorComponents = set of TTrivectorComponent;

  // TTrivector
  TTrivector = record
  private
    fm012: double;
    fm013: double;
    fm023: double;
    fm123: double;
  public
    class operator =(const ALeft, ARight: TTrivector): boolean;
    class operator =(const ALeft: TTrivector; ARight: TMultivector): boolean;
    class operator =(const ALeft: TMultivector; ARight: TTrivector): boolean;
    class operator <>(const ALeft, ARight: TTrivector): boolean;
    class operator <>(const ALeft: TTrivector; ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; ARight: TTrivector): boolean;
    class operator +(const AValue: TTrivector): TTrivector;
    class operator +(const ALeft: TTrivector; ARight: double): TMultivector;
    class operator +(const ALeft: double; ARight: TTrivector): TMultivector;
    class operator +(const ALeft, ARight: TTrivector): TTrivector;
    class operator +(const ALeft: TTrivector; ARight: TQuadrivector): TMultivector;
    class operator +(const ALeft: TQuadrivector; ARight: TTrivector): TMultivector;
    class operator +(const ALeft: TTrivector; ARight: TMultivector): TMultivector;
    class operator +(const ALeft: TMultivector; ARight: TTrivector): TMultivector;
    class operator -(const AValue: TTrivector): TTrivector;
    class operator -(const ALeft: TTrivector; ARight: double): TMultivector;
    class operator -(const ALeft: double; ARight: TTrivector): TMultivector;
    class operator -(const ALeft, ARight: TTrivector): TTrivector;
    class operator -(const ALeft: TTrivector; ARight: TQuadrivector): TMultivector;
    class operator -(const ALeft: TQuadrivector; ARight: TTrivector): TMultivector;
    class operator -(const ALeft: TTrivector; ARight: TMultivector): TMultivector;
    class operator -(const ALeft: TMultivector; ARight: TTrivector): TMultivector;
    class operator *(const ALeft: TTrivector; const ARight: double): TTrivector;
    class operator *(const ALeft: double; const ARight: TTrivector): TTrivector;
    class operator *(const ALeft, ARight: TTrivector): TMultivector;
    class operator *(const ALeft: TTrivector; const ARight: TQuadrivector): TMultivector;
    class operator *(const ALeft: TQuadrivector; const ARight: TTrivector): TMultivector;
    class operator *(const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator *(const ALeft: TMultivector; const ARight: TTrivector): TMultivector;
    class operator /(const ALeft: TTrivector; const ARight: double): TTrivector;
    class operator /(const ALeft: double; const ARight: TTrivector): TTrivector;
    class operator /(const ALeft, ARight: TTrivector): TMultivector;
    class operator /(const ALeft: TTrivector; const ARight: TQuadrivector): TMultivector;
    class operator /(const ALeft: TQuadrivector; const ARight: TTrivector): TMultivector;
    class operator /(const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator /(const ALeft: TMultivector; const ARight: TTrivector): TMultivector;
  end;

  // TBivectorComponents
  TBivectorComponent  = (bc01, bc02, bc03, bc12, bc13, bc23);
  TBivectorComponents = set of TBivectorComponent;

  // TBivector
  TBivector = record
  private
    fm01: double;
    fm02: double;
    fm03: double;
    fm12: double;
    fm13: double;
    fm23: double;
  public
    class operator =(const ALeft, ARight: TBivector): boolean;
    class operator =(const ALeft: TBivector; ARight: TMultivector): boolean;
    class operator =(const ALeft: TMultivector; ARight: TBivector): boolean;
    class operator <>(const ALeft, ARight: TBivector): boolean;
    class operator <>(const ALeft: TBivector; ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; ARight: TBivector): boolean;
    class operator +(const AValue: TBivector): TBivector;
    class operator +(const ALeft: TBivector; ARight: double): TMultivector;
    class operator +(const ALeft: double; ARight: TBivector): TMultivector;
    class operator +(const ALeft, ARight: TBivector): TBivector;
    class operator +(const ALeft: TBivector; ARight: TTrivector): TMultivector;
    class operator +(const ALeft: TTrivector; ARight: TBivector): TMultivector;
    class operator +(const ALeft: TBivector; ARight: TQuadrivector): TMultivector;
    class operator +(const ALeft: TQuadrivector; ARight: TBivector): TMultivector;
    class operator +(const ALeft: TBivector; ARight: TMultivector): TMultivector;
    class operator +(const ALeft: TMultivector; ARight: TBivector): TMultivector;
    class operator -(const AValue: TBivector): TBivector;
    class operator -(const ALeft: TBivector; ARight: double): TMultivector;
    class operator -(const ALeft: double; ARight: TBivector): TMultivector;
    class operator -(const ALeft, ARight: TBivector): TBivector;
    class operator -(const ALeft: TBivector; ARight: TTrivector): TMultivector;
    class operator -(const ALeft: TTrivector; ARight: TBivector): TMultivector;
    class operator -(const ALeft: TBivector; ARight: TQuadrivector): TMultivector;
    class operator -(const ALeft: TQuadrivector; ARight: TBivector): TMultivector;
    class operator -(const ALeft: TBivector; ARight: TMultivector): TMultivector;
    class operator -(const ALeft: TMultivector; ARight: TBivector): TMultivector;
    class operator *(const ALeft: TBivector; const ARight: double): TBivector;
    class operator *(const ALeft: double; const ARight: TBivector): TBivector;
    class operator *(const ALeft, ARight: TBivector): TMultivector;
    class operator *(const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator *(const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator *(const ALeft: TBivector; const ARight: TQuadrivector): TBivector;
    class operator *(const ALeft: TQuadrivector; const ARight: TBivector): TBivector;
    class operator *(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator *(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
    class operator /(const ALeft: TBivector; const ARight: double): TBivector;
    class operator /(const ALeft: double; const ARight: TBivector): TBivector;
    class operator /(const ALeft, ARight: TBivector): TMultivector;
    class operator /(const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator /(const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator /(const ALeft: TBivector; const ARight: TQuadrivector): TBivector;
    class operator /(const ALeft: TQuadrivector; const ARight: TBivector): TBivector;
    class operator /(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator /(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
  end;

  // TVectorComponents
  TVectorComponent  = (vc0, vc1, vc2, vc3);
  TVectorComponents = set of TVectorComponent;

  // TVector
  TVector = record
  private
    fm0: double;
    fm1: double;
    fm2: double;
    fm3: double;
  public
    class operator =(const ALeft, ARight: TVector): boolean;
    class operator =(const ALeft: TVector; ARight: TMultivector): boolean;
    class operator =(const ALeft: TMultivector; ARight: TVector): boolean;
    class operator <>(const ALeft, ARight: TVector): boolean;
    class operator <>(const ALeft: TVector; ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; ARight: TVector): boolean;
    class operator +(const AValue: TVector): TVector;
    class operator +(const ALeft: TVector; ARight: double): TMultivector;
    class operator +(const ALeft: double; ARight: TVector): TMultivector;
    class operator +(const ALeft, ARight: TVector): TVector;
    class operator +(const ALeft: TVector; ARight: TBivector): TMultivector;
    class operator +(const ALeft: TBivector; ARight: TVector): TMultivector;
    class operator +(const ALeft: TVector; ARight: TTrivector): TMultivector;
    class operator +(const ALeft: TTrivector; ARight: TVector): TMultivector;
    class operator +(const ALeft: TVector; ARight: TQuadrivector): TMultivector;
    class operator +(const ALeft: TQuadrivector; ARight: TVector): TMultivector;
    class operator +(const ALeft: TVector; ARight: TMultivector): TMultivector;
    class operator +(const ALeft: TMultivector; ARight: TVector): TMultivector;
    class operator -(const AValue: TVector): TVector;
    class operator -(const ALeft: TVector; ARight: double): TMultivector;
    class operator -(const ALeft: double; ARight: TVector): TMultivector;
    class operator -(const ALeft, ARight: TVector): TVector;
    class operator -(const ALeft: TVector; ARight: TBivector): TMultivector;
    class operator -(const ALeft: TBivector; ARight: TVector): TMultivector;
    class operator -(const ALeft: TVector; ARight: TTrivector): TMultivector;
    class operator -(const ALeft: TTrivector; ARight: TVector): TMultivector;
    class operator -(const ALeft: TVector; ARight: TQuadrivector): TMultivector;
    class operator -(const ALeft: TQuadrivector; ARight: TVector): TMultivector;
    class operator -(const ALeft: TVector; ARight: TMultivector): TMultivector;
    class operator -(const ALeft: TMultivector; ARight: TVector): TMultivector;
    class operator *(const ALeft: TVector; const ARight: double): TVector;
    class operator *(const ALeft: double; const ARight: TVector): TVector;
    class operator *(const ALeft, ARight: TVector): TMultivector;
    class operator *(const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator *(const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator *(const ALeft: TVector; const ARight: TTrivector): TMultivector;
    class operator *(const ALeft: TTrivector; const ARight: TVector): TMultivector;
    class operator *(const ALeft: TVector; const ARight: TQuadrivector): TTrivector;
    class operator *(const ALeft: TQuadrivector; const ARight: TVector): TTrivector;
    class operator *(const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator *(const ALeft: TMultivector; const ARight: TVector): TMultivector;
    class operator /(const ALeft: TVector; const ARight: double): TVector;
    class operator /(const ALeft: double; const ARight: TVector): TVector;
    class operator /(const ALeft, ARight: TVector): TMultivector;
    class operator /(const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator /(const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator /(const ALeft: TVector; const ARight: TTrivector): TMultivector;
    class operator /(const ALeft: TTrivector; const ARight: TVector): TMultivector;
    class operator /(const ALeft: TVector; const ARight: TQuadrivector): TTrivector;
    class operator /(const ALeft: TQuadrivector; const ARight: TVector): TTrivector;
    class operator /(const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator /(const ALeft: TMultivector; const ARight: TVector): TMultivector;
  end;

  // TMultivector
  TMultivectorHelper = record helper for TMultivector
    function Dual: TMultivector;
    function Inverse: TMultivector;
    function Reverse: TMultivector;
    function Conjugate: TMultivector;
    function Reciprocal: TMultivector;
    function LeftReciprocal: TMultivector;
    function Normalized: TMultivector;
    function Norm: double;
    function SquaredNorm: double;
    function Projection(const AVector: TVector): TMultivector;
    function Projection(const AVector: TBivector): TMultivector;
    function Projection(const AVector: TTrivector): TMultivector;
    function Projection(const AVector: TQuadrivector): TMultivector;
    function Projection(const AVector: TMultivector): TMultivector;
    function Rejection(const AVector: TVector): TMultivector;
    function Rejection(const AVector: TBivector): TMultivector;
    function Rejection(const AVector: TTrivector): TMultivector;
    function Rejection(const AVector: TQuadrivector): double;
    function Rejection(const AVector: TMultivector): TMultivector;
    function Reflection(const AVector: TVector): TMultivector;
    function Reflection(const AVector: TBivector): TMultivector;
    function Reflection(const AVector: TTrivector): TMultivector;
    function Reflection(const AVector: TQuadrivector): TMultivector;
    function Reflection(const AVector: TMultivector): TMultivector;
    function Rotation(const AVector1, AVector2: TVector): TMultivector;
    function Rotation(const AVector1, AVector2: TBivector): TMultivector;
    function Rotation(const AVector1, AVector2: TTrivector): TMultivector;
    function Rotation(const AVector1, AVector2: TQuadrivector): TMultivector;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;
    function Wedge(const AVector: TVector): TMultivector;
    function Wedge(const AVector: TBivector): TMultivector;
    function Wedge(const AVector: TTrivector): TMultivector;
    function Wedge(const AVector: TQuadrivector): TQuadrivector;
    function Wedge(const AVector: TMultivector): TMultivector;
    function Dot(const AVector: TVector): TMultivector;
    function Dot(const AVector: TBivector): TMultivector;
    function Dot(const AVector: TTrivector): TMultivector;
    function Dot(const AVector: TQuadrivector): TMultivector;
    function Dot(const AVector: TMultivector): TMultivector;

    function SameValue(const AVector: TMultivector): boolean;
    function SameValue(const AVector: TQuadrivector): boolean;
    function SameValue(const AVector: TTrivector): boolean;
    function SameValue(const AVector: TBivector): boolean;
    function SameValue(const AVector: TVector): boolean;
    function SameValue(const AVector: double): boolean;

    function Extract(AComponents: TMultivectorComponents): TMultivector;
    function Extract(AComponents: TTrivectorComponents): TTrivector;
    function Extract(AComponents: TBivectorComponents): TBivector;
    function Extract(AComponents: TVectorComponents): TVector;

    function ExtractScalar: double;
    function ExtractVector: TVector;
    function ExtractBivector: TBivector;
    function ExtractTrivector: TTrivector;
    function ExtractQuadrivector: TQuadrivector;

    function IsQuadrivector: boolean;
    function IsTrivector: boolean;
    function IsBiVector: boolean;
    function IsVector: boolean;
    function IsScalar: boolean;
    function IsNull: boolean;
    function IsA: string;

    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
  end;

  // TQuadrivector
  TQuadrivectorHelper = record helper for TQuadrivector
    function Dual: double;
    function Inverse: TQuadrivector;
    function Reverse: TQuadrivector;
    function Conjugate: TQuadrivector;
    function Reciprocal: TQuadrivector;
    function Normalized: TQuadrivector;
    function Norm: double;
    function SquaredNorm: double;
    function Projection(const AVector: TVector): TMultivector;
    function Projection(const AVector: TBivector): TMultivector;
    function Projection(const AVector: TTrivector): TMultivector;
    function Projection(const AVector: TQuadrivector): TQuadrivector;
    function Projection(const AVector: TMultivector): TMultivector;
    function Rejection(const AVector: TVector): TVector;
    function Rejection(const AVector: TBivector): TBivector;
    function Rejection(const AVector: TTrivector): TTrivector;
    function Rejection(const AVector: TQuadrivector): TQuadrivector;
    function Rejection(const AVector: TMultivector): TMultivector;
    function Reflection(const AVector: TVector): TMultivector;
    function Reflection(const AVector: TBivector): TMultivector;
    function Reflection(const AVector: TTrivector): TMultivector;
    function Reflection(const AVector: TQuadrivector): TQuadrivector;
    function Reflection(const AVector: TMultivector): TMultivector;
    function Rotation(const AVector1, AVector2: TVector): TMultivector;
    function Rotation(const AVector1, AVector2: TBivector): TMultivector;
    function Rotation(const AVector1, AVector2: TTrivector): TMultivector;
    function Rotation(const AVector1, AVector2: TQuadrivector): TQuadrivector;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;
    function Wedge(const AVector: TVector): double;
    function Wedge(const AVector: TBivector): double;
    function Wedge(const AVector: TTrivector): double;
    function Wedge(const AVector: TQuadrivector): double;
    function Wedge(const AVector: TMultivector): TQuadrivector;
    function Dot(const AVector: TVector): TTrivector;
    function Dot(const AVector: TBivector): TBivector;
    function Dot(const AVector: TTrivector): TMultivector;
    function Dot(const AVector: TQuadrivector): double;
    function Dot(const AVector: TMultivector): TMultivector;

    function SameValue(const AVector: TQuadrivector): boolean;
    function SameValue(const AVector: TMultivector): boolean;

    function ToMultivector: TMultivector;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
  end;

  // TTrivector
  TTrivectorHelper = record helper for TTrivector
    function Dual: TVector;
    function Inverse: TTrivector;
    function Reverse: TTrivector;
    function Conjugate: TTrivector;
    function Reciprocal: TTrivector;
    function Normalized: TTrivector;
    function Norm: double;
    function SquaredNorm: double;
    function Projection(const AVector: TVector): TMultivector;
    function Projection(const AVector: TBivector): TMultivector;
    function Projection(const AVector: TTrivector): TTrivector;
    function Projection(const AVector: TQuadrivector): TMultivector;
    function Projection(const AVector: TMultivector): TMultivector;
    function Rejection(const AVector: TVector): TTrivector;
    function Rejection(const AVector: TBivector): TBivector;
    function Rejection(const AVector: TTrivector): TTrivector;
    function Rejection(const AVector: TQuadrivector): TQuadrivector;
    function Rejection(const AVector: TMultivector): TMultivector;
    function Reflection(const AVector: TVector): TMultivector;
    function Reflection(const AVector: TBivector): TMultivector;
    function Reflection(const AVector: TTrivector): TMultivector;
    function Reflection(const AVector: TQuadrivector): TMultivector;
    function Reflection(const AVector: TMultivector): TMultivector;
    function Rotation(const AVector1, AVector2: TVector): TMultivector;
    function Rotation(const AVector1, AVector2: TBivector): TMultivector;
    function Rotation(const AVector1, AVector2: TTrivector): TMultivector;
    function Rotation(const AVector1, AVector2: TQuadrivector): TMultivector;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;
    function Wedge(const AVector: TVector): TQuadrivector;
    function Wedge(const AVector: TBivector): double;
    function Wedge(const AVector: TTrivector): double;
    function Wedge(const AVector: TQuadrivector): double;
    function Wedge(const AVector: TMultivector): TMultivector;
    function Dot(const AVector: TVector): TBivector;
    function Dot(const AVector: TBivector): TMultivector;
    function Dot(const AVector: TTrivector): double;
    function Dot(const AVector: TQuadrivector): TMultivector;
    function Dot(const AVector: TMultivector): TMultivector;

    function SameValue(const AVector: TTrivector): boolean;
    function SameValue(const AVector: TMultivector): boolean;

    function Extract(AComponents: TTrivectorComponents): TTrivector;

    function ToString: string;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToMultivector: TMultivector;
  end;

  // TBivector
  TBivectorHelper = record helper for TBivector
    function Dual: TBivector;
    function Inverse: TBivector;
    function Reverse: TBivector;
    function Conjugate: TBivector;
    function Reciprocal: TBivector;
    function Normalized: TBivector;
    function Norm: double;
    function SquaredNorm: double;
    function Projection(const AVector: TVector): TMultivector;
    function Projection(const AVector: TBivector): TBivector;
    function Projection(const AVector: TTrivector): TMultivector;
    function Projection(const AVector: TQuadrivector): TBivector;
    function Projection(const AVector: TMultivector): TMultivector;
    function Rejection(const AVector: TVector): TMultivector;
    function Rejection(const AVector: TBivector): TBivector;
    function Rejection(const AVector: TTrivector): TTrivector;
    function Rejection(const AVector: TQuadrivector): TQuadrivector;
    function Rejection(const AVector: TMultivector): TMultivector;
    function Reflection(const AVector: TVector): TMultivector;
    function Reflection(const AVector: TBivector): TMultivector;
    function Reflection(const AVector: TTrivector): TMultivector;
    function Reflection(const AVector: TQuadrivector): TBivector;
    function Reflection(const AVector: TMultivector): TMultivector;
    function Rotation(const AVector1, AVector2: TVector): TMultivector;
    function Rotation(const AVector1, AVector2: TBivector): TMultivector;
    function Rotation(const AVector1, AVector2: TTrivector): TMultivector;
    function Rotation(const AVector1, AVector2: TQuadrivector): TBivector;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;
    function Wedge(const AVector: TVector): TTrivector;
    function Wedge(const AVector: TBivector): TQuadrivector;
    function Wedge(const AVector: TTrivector): double;
    function Wedge(const AVector: TQuadrivector): double;
    function Wedge(const AVector: TMultivector): TMultivector;
    function Dot(const AVector: TVector): TVector;
    function Dot(const AVector: TBivector): double;
    function Dot(const AVector: TTrivector): TMultivector;
    function Dot(const AVector: TQuadrivector): TBivector;
    function Dot(const AVector: TMultivector): TMultivector;

    function SameValue(const AVector: TBivector): boolean;
    function SameValue(const AVector: TMultivector): boolean;

    function Extract(AComponents: TBivectorComponents): TBivector;

    function ToMultivector: TMultivector;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
  end;

  // TVector
  TVectorHelper = record helper for TVector
    function Dual: TTrivector;
    function Inverse: TVector;
    function Reverse: TVector;
    function Conjugate: TVector;
    function Reciprocal: TVector;
    function Normalized: TVector;
    function Norm: double;
    function SquaredNorm: double;
    function Projection(const AVector: TVector): TVector;
    function Projection(const AVector: TBivector): TMultivector;
    function Projection(const AVector: TTrivector): TMultivector;
    function Projection(const AVector: TQuadrivector): TMultivector;
    function Projection(const AVector: TMultivector): TMultivector;
    function Rejection(const AVector: TVector): TMultivector;
    function Rejection(const AVector: TBivector): TMultivector;
    function Rejection(const AVector: TTrivector): TMultivector;
    function Rejection(const AVector: TQuadrivector): TQuadrivector;
    function Rejection(const AVector: TMultivector): TMultivector;
    function Reflection(const AVector: TVector): TMultivector;
    function Reflection(const AVector: TBivector): TMultivector;
    function Reflection(const AVector: TTrivector): TMultivector;
    function Reflection(const AVector: TQuadrivector): TMultivector;
    function Reflection(const AVector: TMultivector): TMultivector;
    function Rotation(const AVector1, AVector2: TVector): TMultivector;
    function Rotation(const AVector1, AVector2: TBivector): TMultivector;
    function Rotation(const AVector1, AVector2: TTrivector): TMultivector;
    function Rotation(const AVector1, AVector2: TQuadrivector): TMultivector;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;
    function Wedge(const AVector: TVector): TBivector;
    function Wedge(const AVector: TBivector): TTrivector;
    function Wedge(const AVector: TTrivector): TQuadrivector;
    function Wedge(const AVector: TQuadrivector): double;
    function Wedge(const AVector: TMultivector): TMultivector;
    function Dot(const AVector: TVector): double;
    function Dot(const AVector: TBivector): TVector;
    function Dot(const AVector: TTrivector): TBivector;
    function Dot(const AVector: TQuadrivector): TTrivector;
    function Dot(const AVector: TMultivector): TMultivector;

    function SameValue(const AVector: TVector): boolean;
    function SameValue(const AVector: TMultivector): boolean;

    function Extract(AComponents: TVectorComponents): TVector;

    function ToMultivector: TMultivector;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
  end;

  TVersor0 = record class operator *(const AValue: double; const ASelf: TVersor0): TVector; end;
  TVersor1 = record class operator *(const AValue: double; const ASelf: TVersor1): TVector; end;
  TVersor2 = record class operator *(const AValue: double; const ASelf: TVersor2): TVector; end;
  TVersor3 = record class operator *(const AValue: double; const ASelf: TVersor3): TVector; end;

  TVersor01 = record class operator *(const AValue: double; const ASelf: TVersor01): TBivector; end;
  TVersor02 = record class operator *(const AValue: double; const ASelf: TVersor02): TBivector; end;
  TVersor03 = record class operator *(const AValue: double; const ASelf: TVersor03): TBivector; end;
  TVersor12 = record class operator *(const AValue: double; const ASelf: TVersor12): TBivector; end;
  TVersor13 = record class operator *(const AValue: double; const ASelf: TVersor13): TBivector; end;
  TVersor23 = record class operator *(const AValue: double; const ASelf: TVersor23): TBivector; end;

  TVersor012 = record class operator *(const AValue: double; const ASelf: TVersor012): TTrivector; end;
  TVersor013 = record class operator *(const AValue: double; const ASelf: TVersor013): TTrivector; end;
  TVersor023 = record class operator *(const AValue: double; const ASelf: TVersor023): TTrivector; end;
  TVersor123 = record class operator *(const AValue: double; const ASelf: TVersor123): TTrivector; end;

  TVersor0123 = record class operator *(const AValue: double; const ASelf: TVersor0123): TQuadrivector; end;

const
  g0: TVersor0 = ();
  g1: TVersor1 = ();
  g2: TVersor2 = ();
  g3: TVersor3 = ();
  g01: TVersor01 = ();
  g02: TVersor02 = ();
  g03: TVersor03 = ();
  g12: TVersor12 = ();
  g13: TVersor13 = ();
  g23: TVersor23 = ();
  g012: TVersor012 = ();
  g013: TVersor013 = ();
  g023: TVersor023 = ();
  g123: TVersor123 = ();
  g0123: TVersor0123 = ();

  u0: TVector = (fm0: 1.0; fm1: 0.0; fm2: 0.0; fm3: 0.0);
  u1: TVector = (fm0: 0.0; fm1: 1.0; fm2: 0.0; fm3: 0.0);
  u2: TVector = (fm0: 0.0; fm1: 0.0; fm2: 1.0; fm3: 0.0);
  u3: TVector = (fm0: 0.0; fm1: 0.0; fm2: 0.0; fm3: 1.0);
  u01: TBivector = (fm01: 1.0; fm02: 0.0; fm03: 0.0; fm12: 0.0; fm13: 0.0; fm23: 0.0);
  u02: TBivector = (fm01: 0.0; fm02: 1.0; fm03: 0.0; fm12: 0.0; fm13: 0.0; fm23: 0.0);
  u03: TBivector = (fm01: 0.0; fm02: 0.0; fm03: 1.0; fm12: 0.0; fm13: 0.0; fm23: 0.0);
  u12: TBivector = (fm01: 0.0; fm02: 0.0; fm03: 0.0; fm12: 1.0; fm13: 0.0; fm23: 0.0);
  u13: TBivector = (fm01: 0.0; fm02: 0.0; fm03: 0.0; fm12: 0.0; fm13: 1.0; fm23: 0.0);
  u23: TBivector = (fm01: 0.0; fm02: 0.0; fm03: 0.0; fm12: 0.0; fm13: 0.0; fm23: 1.0);
  u012: TTrivector = (fm012: 1.0; fm013: 0.0; fm023: 0.0; fm123: 0.0);
  u013: TTrivector = (fm012: 0.0; fm013: 1.0; fm023: 0.0; fm123: 0.0);
  u023: TTrivector = (fm012: 0.0; fm013: 0.0; fm023: 1.0; fm123: 0.0);
  u123: TTrivector = (fm012: 0.0; fm013: 0.0; fm023: 0.0; fm123: 1.0);
  u0123: TQuadrivector = (fm0123: 1.0);

  NullMultivector  : TMultivector  = (fms:0.0; fm0:0.0; fm1:0.0; fm2:0.0; fm3:0.0; fm01:0.0; fm02:0.0; fm03:0.0; fm12:0.0; fm13:0.0; fm23:0.0; fm012:0.0; fm013:0.0; fm023:0.0; fm123:0.0; fm0123:0.0);
  NullQuadrivector : TQuadrivector = (fm0123:0.0);
  NullTrivector    : TTrivector    = (fm012:0.0; fm013:0.0; fm023:0.0; fm123:0.0);
  NullBivector     : TBivector     = (fm01:0.0; fm02:0.0; fm03:0.0; fm12:0.0; fm13:0.0; fm23:0.0);
  NullVector       : TVector       = (fm0:0.0; fm1:0.0; fm2:0.0; fm3:0.0);
  NullScalar       : double        = (0.0);

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
  result.fms := AValue;
  result.fm0 := 0.0;
  result.fm1 := 0.0;
  result.fm2 := 0.0;
  result.fm3 := 0.0;
  result.fm01 := 0.0;
  result.fm02 := 0.0;
  result.fm03 := 0.0;
  result.fm12 := 0.0;
  result.fm13 := 0.0;
  result.fm23 := 0.0;
  result.fm012 := 0.0;
  result.fm013 := 0.0;
  result.fm023 := 0.0;
  result.fm123 := 0.0;
  result.fm0123 := 0.0;
end;

class operator TMultivector.=(const ALeft: TMultivector; ARight: double): boolean;
begin
  result :=
    (ALeft.fms = ARight) and (ALeft.fm0 = 0) and (ALeft.fm1 = 0) and
    (ALeft.fm2 = 0) and (ALeft.fm3 = 0) and (ALeft.fm01 = 0) and
    (ALeft.fm02 = 0) and (ALeft.fm03 = 0) and (ALeft.fm12 = 0) and
    (ALeft.fm13 = 0) and (ALeft.fm23 = 0) and (ALeft.fm012 = 0) and
    (ALeft.fm013 = 0) and (ALeft.fm023 = 0) and (ALeft.fm123 = 0) and
    (ALeft.fm0123 = 0);
end;

class operator TMultivector.=(const ALeft: double; ARight: TMultivector): boolean;
begin
  result :=
    (ALeft = ARight.fms) and (0 = ARight.fm0) and (0 = ARight.fm1) and
    (0 = ARight.fm2) and (0 = ARight.fm3) and (0 = ARight.fm01) and
    (0 = ARight.fm02) and (0 = ARight.fm03) and (0 = ARight.fm12) and
    (0 = ARight.fm13) and (0 = ARight.fm23) and (0 = ARight.fm012) and
    (0 = ARight.fm013) and (0 = ARight.fm023) and (0 = ARight.fm123) and
    (0 = ARight.fm0123);
end;

class operator TMultivector.=(const ALeft, ARight: TMultivector): boolean;
begin
  result :=
    (ALeft.fms = ARight.fms) and (ALeft.fm0 = ARight.fm0) and
    (ALeft.fm1 = ARight.fm1) and (ALeft.fm2 = ARight.fm2) and
    (ALeft.fm3 = ARight.fm3) and (ALeft.fm01 = ARight.fm01) and
    (ALeft.fm02 = ARight.fm02) and (ALeft.fm03 = ARight.fm03) and
    (ALeft.fm12 = ARight.fm12) and (ALeft.fm13 = ARight.fm13) and
    (ALeft.fm23 = ARight.fm23) and (ALeft.fm012 = ARight.fm012) and
    (ALeft.fm013 = ARight.fm013) and (ALeft.fm023 = ARight.fm023) and
    (ALeft.fm123 = ARight.fm123) and (ALeft.fm0123 = ARight.fm0123);
end;

class operator TMultivector.<>(const ALeft: TMultivector; ARight: double): boolean;
begin
  result :=
    (ALeft.fms <> ARight) and (ALeft.fm0 <> 0) and (ALeft.fm1 <> 0) and
    (ALeft.fm2 <> 0) and (ALeft.fm3 <> 0) and (ALeft.fm01 <> 0) and
    (ALeft.fm02 <> 0) and (ALeft.fm03 <> 0) and (ALeft.fm12 <> 0) and
    (ALeft.fm13 <> 0) and (ALeft.fm23 <> 0) and (ALeft.fm012 <> 0) and
    (ALeft.fm013 <> 0) and (ALeft.fm023 <> 0) and (ALeft.fm123 <> 0) and
    (ALeft.fm0123 <> 0);
end;

class operator TMultivector.<>(const ALeft: double; ARight: TMultivector): boolean;
begin
  result :=
    (ALeft <> ARight.fms) and (0 <> ARight.fm0) and (0 <> ARight.fm1) and
    (0 <> ARight.fm2) and (0 <> ARight.fm3) and (0 <> ARight.fm01) and
    (0 <> ARight.fm02) and (0 <> ARight.fm03) and (0 <> ARight.fm12) and
    (0 <> ARight.fm13) and (0 <> ARight.fm23) and (0 <> ARight.fm012) and
    (0 <> ARight.fm013) and (0 <> ARight.fm023) and (0 <> ARight.fm123) and
    (0 <> ARight.fm0123);
end;

class operator TMultivector.<>(const ALeft, ARight: TMultivector): boolean;
begin
  result :=
    (ALeft.fms <> ARight.fms) and (ALeft.fm0 <> ARight.fm0) and
    (ALeft.fm1 <> ARight.fm1) and (ALeft.fm2 <> ARight.fm2) and
    (ALeft.fm3 <> ARight.fm3) and (ALeft.fm01 <> ARight.fm01) and
    (ALeft.fm02 <> ARight.fm02) and (ALeft.fm03 <> ARight.fm03) and
    (ALeft.fm12 <> ARight.fm12) and (ALeft.fm13 <> ARight.fm13) and
    (ALeft.fm23 <> ARight.fm23) and (ALeft.fm012 <> ARight.fm012) and
    (ALeft.fm013 <> ARight.fm013) and (ALeft.fm023 <> ARight.fm023) and
    (ALeft.fm123 <> ARight.fm123) and (ALeft.fm0123 <> ARight.fm0123);
end;

class operator TMultivector.+(const AValue: TMultivector): TMultivector;
begin
  result.fms := Avalue.fms;
  result.fm0 := Avalue.fm0;
  result.fm1 := Avalue.fm1;
  result.fm2 := Avalue.fm2;
  result.fm3 := Avalue.fm3;
  result.fm01 := Avalue.fm01;
  result.fm02 := Avalue.fm02;
  result.fm03 := Avalue.fm03;
  result.fm12 := Avalue.fm12;
  result.fm13 := Avalue.fm13;
  result.fm23 := Avalue.fm23;
  result.fm012 := Avalue.fm012;
  result.fm013 := Avalue.fm013;
  result.fm023 := Avalue.fm023;
  result.fm123 := Avalue.fm123;
  result.fm0123 := Avalue.fm0123;
end;

class operator TMultivector.+(const ALeft: TMultivector; ARight: double): TMultivector;
begin
  result.fms := ALeft.fms + ARight;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := ALeft.fm0123;
end;

class operator TMultivector.+(const ALeft: double; ARight: TMultivector): TMultivector;
begin
  result.fms := ALeft + ARight.fms;
  result.fm0 := ARight.fm0;
  result.fm1 := ARight.fm1;
  result.fm2 := ARight.fm2;
  result.fm3 := ARight.fm3;
  result.fm01 := ARight.fm01;
  result.fm02 := ARight.fm02;
  result.fm03 := ARight.fm03;
  result.fm12 := ARight.fm12;
  result.fm13 := ARight.fm13;
  result.fm23 := ARight.fm23;
  result.fm012 := ARight.fm012;
  result.fm013 := ARight.fm013;
  result.fm023 := ARight.fm023;
  result.fm123 := ARight.fm123;
  result.fm0123 := ARight.fm0123;
end;

class operator TMultivector.+(const ALeft, ARight: TMultivector): TMultivector;
begin
  result.fms := ALeft.fms + ARight.fms;
  result.fm0 := ALeft.fm0 + ARight.fm0;
  result.fm1 := ALeft.fm1 + ARight.fm1;
  result.fm2 := ALeft.fm2 + ARight.fm2;
  result.fm3 := ALeft.fm3 + ARight.fm3;
  result.fm01 := ALeft.fm01 + ARight.fm01;
  result.fm02 := ALeft.fm02 + ARight.fm02;
  result.fm03 := ALeft.fm03 + ARight.fm03;
  result.fm12 := ALeft.fm12 + ARight.fm12;
  result.fm13 := ALeft.fm13 + ARight.fm13;
  result.fm23 := ALeft.fm23 + ARight.fm23;
  result.fm012 := ALeft.fm012 + ARight.fm012;
  result.fm013 := ALeft.fm013 + ARight.fm013;
  result.fm023 := ALeft.fm023 + ARight.fm023;
  result.fm123 := ALeft.fm123 + ARight.fm123;
  result.fm0123 := ALeft.fm0123 + ARight.fm0123;
end;

class operator TMultivector.-(const AValue: TMultivector): TMultivector;
begin
  result.fms := -Avalue.fms;
  result.fm0 := -Avalue.fm0;
  result.fm1 := -Avalue.fm1;
  result.fm2 := -Avalue.fm2;
  result.fm3 := -Avalue.fm3;
  result.fm01 := -Avalue.fm01;
  result.fm02 := -Avalue.fm02;
  result.fm03 := -Avalue.fm03;
  result.fm12 := -Avalue.fm12;
  result.fm13 := -Avalue.fm13;
  result.fm23 := -Avalue.fm23;
  result.fm012 := -Avalue.fm012;
  result.fm013 := -Avalue.fm013;
  result.fm023 := -Avalue.fm023;
  result.fm123 := -Avalue.fm123;
  result.fm0123 := -Avalue.fm0123;
end;

class operator TMultivector.-(const ALeft: TMultivector; ARight: double): TMultivector;
begin
  result.fms := ALeft.fms - ARight;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := ALeft.fm0123;
end;

class operator TMultivector.-(const ALeft: double; ARight: TMultivector): TMultivector;
begin
  result.fms := ALeft - ARight.fms;
  result.fm0 := -ARight.fm0;
  result.fm1 := -ARight.fm1;
  result.fm2 := -ARight.fm2;
  result.fm3 := -ARight.fm3;
  result.fm01 := -ARight.fm01;
  result.fm02 := -ARight.fm02;
  result.fm03 := -ARight.fm03;
  result.fm12 := -ARight.fm12;
  result.fm13 := -ARight.fm13;
  result.fm23 := -ARight.fm23;
  result.fm012 := -ARight.fm012;
  result.fm013 := -ARight.fm013;
  result.fm023 := -ARight.fm023;
  result.fm123 := -ARight.fm123;
  result.fm0123 := -ARight.fm0123;
end;

class operator TMultivector.-(const ALeft, ARight: TMultivector): TMultivector;
begin
  result.fms := ALeft.fms - ARight.fms;
  result.fm0 := ALeft.fm0 - ARight.fm0;
  result.fm1 := ALeft.fm1 - ARight.fm1;
  result.fm2 := ALeft.fm2 - ARight.fm2;
  result.fm3 := ALeft.fm3 - ARight.fm3;
  result.fm01 := ALeft.fm01 - ARight.fm01;
  result.fm02 := ALeft.fm02 - ARight.fm02;
  result.fm03 := ALeft.fm03 - ARight.fm03;
  result.fm12 := ALeft.fm12 - ARight.fm12;
  result.fm13 := ALeft.fm13 - ARight.fm13;
  result.fm23 := ALeft.fm23 - ARight.fm23;
  result.fm012 := ALeft.fm012 - ARight.fm012;
  result.fm013 := ALeft.fm013 - ARight.fm013;
  result.fm023 := ALeft.fm023 - ARight.fm023;
  result.fm123 := ALeft.fm123 - ARight.fm123;
  result.fm0123 := ALeft.fm0123 - ARight.fm0123;
end;

class operator TMultivector.*(const ALeft: TMultivector;
  const ARight: double): TMultivector;
begin
  result.fms := +ALeft.fms * ARight;

  result.fm0 := +ALeft.fm0 * ARight;

  result.fm1 := +ALeft.fm1 * ARight;

  result.fm2 := +ALeft.fm2 * ARight;

  result.fm3 := +ALeft.fm3 * ARight;

  result.fm01 := +ALeft.fm01 * ARight;

  result.fm02 := +ALeft.fm02 * ARight;

  result.fm03 := +ALeft.fm03 * ARight;

  result.fm12 := +ALeft.fm12 * ARight;

  result.fm13 := +ALeft.fm13 * ARight;

  result.fm23 := +ALeft.fm23 * ARight;

  result.fm012 := +ALeft.fm012 * ARight;

  result.fm013 := +ALeft.fm013 * ARight;

  result.fm023 := +ALeft.fm023 * ARight;

  result.fm123 := +ALeft.fm123 * ARight;

  result.fm0123 := +ALeft.fm0123 * ARight;

end;

class operator TMultivector.*(const ALeft: double;
  const ARight: TMultivector): TMultivector;
begin
  result.fms := +ALeft * ARight.fms;

  result.fm0 := +ALeft * ARight.fm0;

  result.fm1 := +ALeft * ARight.fm1;

  result.fm2 := +ALeft * ARight.fm2;

  result.fm3 := +ALeft * ARight.fm3;

  result.fm01 := +ALeft * ARight.fm01;

  result.fm02 := +ALeft * ARight.fm02;

  result.fm03 := +ALeft * ARight.fm03;

  result.fm12 := +ALeft * ARight.fm12;

  result.fm13 := +ALeft * ARight.fm13;

  result.fm23 := +ALeft * ARight.fm23;

  result.fm012 := +ALeft * ARight.fm012;

  result.fm013 := +ALeft * ARight.fm013;

  result.fm023 := +ALeft * ARight.fm023;

  result.fm123 := +ALeft * ARight.fm123;

  result.fm0123 := +ALeft * ARight.fm0123;

end;

class operator TMultivector.*(const ALeft, ARight: TMultivector): TMultivector;
begin
  result.fms := +ALeft.fms * ARight.fms + ALeft.fm0 * ARight.fm0 -
    ALeft.fm1 * ARight.fm1 - ALeft.fm2 * ARight.fm2 - ALeft.fm3 *
    ARight.fm3 + ALeft.fm01 * ARight.fm01 + ALeft.fm02 * ARight.fm02 +
    ALeft.fm03 * ARight.fm03 - ALeft.fm12 * ARight.fm12 - ALeft.fm13 *
    ARight.fm13 - ALeft.fm23 * ARight.fm23 - ALeft.fm012 * ARight.fm012 -
    ALeft.fm013 * ARight.fm013 - ALeft.fm023 * ARight.fm023 + ALeft.fm123 *
    ARight.fm123 - ALeft.fm0123 * ARight.fm0123;

  result.fm0 := +ALeft.fms * ARight.fm0 + ALeft.fm0 * ARight.fms +
    ALeft.fm1 * ARight.fm01 + ALeft.fm2 * ARight.fm02 + ALeft.fm3 *
    ARight.fm03 - ALeft.fm01 * ARight.fm1 - ALeft.fm02 * ARight.fm2 -
    ALeft.fm03 * ARight.fm3 - ALeft.fm12 * ARight.fm012 - ALeft.fm13 *
    ARight.fm013 - ALeft.fm23 * ARight.fm023 - ALeft.fm012 * ARight.fm12 -
    ALeft.fm013 * ARight.fm13 - ALeft.fm023 * ARight.fm23 - ALeft.fm123 *
    ARight.fm0123 + ALeft.fm0123 * ARight.fm123;

  result.fm1 := +ALeft.fms * ARight.fm1 + ALeft.fm0 * ARight.fm01 +
    ALeft.fm1 * ARight.fms + ALeft.fm2 * ARight.fm12 + ALeft.fm3 *
    ARight.fm13 - ALeft.fm01 * ARight.fm0 - ALeft.fm02 * ARight.fm012 -
    ALeft.fm03 * ARight.fm013 - ALeft.fm12 * ARight.fm2 - ALeft.fm13 *
    ARight.fm3 - ALeft.fm23 * ARight.fm123 - ALeft.fm012 * ARight.fm02 -
    ALeft.fm013 * ARight.fm03 - ALeft.fm023 * ARight.fm0123 - ALeft.fm123 *
    ARight.fm23 + ALeft.fm0123 * ARight.fm023;

  result.fm2 := +ALeft.fms * ARight.fm2 + ALeft.fm0 * ARight.fm02 -
    ALeft.fm1 * ARight.fm12 + ALeft.fm2 * ARight.fms + ALeft.fm3 *
    ARight.fm23 + ALeft.fm01 * ARight.fm012 - ALeft.fm02 * ARight.fm0 -
    ALeft.fm03 * ARight.fm023 + ALeft.fm12 * ARight.fm1 + ALeft.fm13 *
    ARight.fm123 - ALeft.fm23 * ARight.fm3 + ALeft.fm012 * ARight.fm01 +
    ALeft.fm013 * ARight.fm0123 - ALeft.fm023 * ARight.fm03 + ALeft.fm123 *
    ARight.fm13 - ALeft.fm0123 * ARight.fm013;

  result.fm3 := +ALeft.fms * ARight.fm3 + ALeft.fm0 * ARight.fm03 -
    ALeft.fm1 * ARight.fm13 - ALeft.fm2 * ARight.fm23 + ALeft.fm3 *
    ARight.fms + ALeft.fm01 * ARight.fm013 + ALeft.fm02 * ARight.fm023 -
    ALeft.fm03 * ARight.fm0 - ALeft.fm12 * ARight.fm123 + ALeft.fm13 *
    ARight.fm1 + ALeft.fm23 * ARight.fm2 - ALeft.fm012 * ARight.fm0123 +
    ALeft.fm013 * ARight.fm01 + ALeft.fm023 * ARight.fm02 - ALeft.fm123 *
    ARight.fm12 + ALeft.fm0123 * ARight.fm012;

  result.fm01 := +ALeft.fms * ARight.fm01 + ALeft.fm0 * ARight.fm1 -
    ALeft.fm1 * ARight.fm0 - ALeft.fm2 * ARight.fm012 - ALeft.fm3 *
    ARight.fm013 + ALeft.fm01 * ARight.fms + ALeft.fm02 * ARight.fm12 +
    ALeft.fm03 * ARight.fm13 - ALeft.fm12 * ARight.fm02 - ALeft.fm13 *
    ARight.fm03 - ALeft.fm23 * ARight.fm0123 - ALeft.fm012 * ARight.fm2 -
    ALeft.fm013 * ARight.fm3 - ALeft.fm023 * ARight.fm123 + ALeft.fm123 *
    ARight.fm023 - ALeft.fm0123 * ARight.fm23;

  result.fm02 := +ALeft.fms * ARight.fm02 + ALeft.fm0 * ARight.fm2 +
    ALeft.fm1 * ARight.fm012 - ALeft.fm2 * ARight.fm0 - ALeft.fm3 *
    ARight.fm023 - ALeft.fm01 * ARight.fm12 + ALeft.fm02 * ARight.fms +
    ALeft.fm03 * ARight.fm23 + ALeft.fm12 * ARight.fm01 + ALeft.fm13 *
    ARight.fm0123 - ALeft.fm23 * ARight.fm03 + ALeft.fm012 * ARight.fm1 +
    ALeft.fm013 * ARight.fm123 - ALeft.fm023 * ARight.fm3 - ALeft.fm123 *
    ARight.fm013 + ALeft.fm0123 * ARight.fm13;

  result.fm03 := +ALeft.fms * ARight.fm03 + ALeft.fm0 * ARight.fm3 +
    ALeft.fm1 * ARight.fm013 + ALeft.fm2 * ARight.fm023 - ALeft.fm3 *
    ARight.fm0 - ALeft.fm01 * ARight.fm13 - ALeft.fm02 * ARight.fm23 +
    ALeft.fm03 * ARight.fms - ALeft.fm12 * ARight.fm0123 + ALeft.fm13 *
    ARight.fm01 + ALeft.fm23 * ARight.fm02 - ALeft.fm012 * ARight.fm123 +
    ALeft.fm013 * ARight.fm1 + ALeft.fm023 * ARight.fm2 + ALeft.fm123 *
    ARight.fm012 - ALeft.fm0123 * ARight.fm12;

  result.fm12 := +ALeft.fms * ARight.fm12 + ALeft.fm0 * ARight.fm012 +
    ALeft.fm1 * ARight.fm2 - ALeft.fm2 * ARight.fm1 - ALeft.fm3 *
    ARight.fm123 - ALeft.fm01 * ARight.fm02 + ALeft.fm02 * ARight.fm01 +
    ALeft.fm03 * ARight.fm0123 + ALeft.fm12 * ARight.fms + ALeft.fm13 *
    ARight.fm23 - ALeft.fm23 * ARight.fm13 + ALeft.fm012 * ARight.fm0 +
    ALeft.fm013 * ARight.fm023 - ALeft.fm023 * ARight.fm013 - ALeft.fm123 *
    ARight.fm3 + ALeft.fm0123 * ARight.fm03;

  result.fm13 := +ALeft.fms * ARight.fm13 + ALeft.fm0 * ARight.fm013 +
    ALeft.fm1 * ARight.fm3 + ALeft.fm2 * ARight.fm123 - ALeft.fm3 *
    ARight.fm1 - ALeft.fm01 * ARight.fm03 - ALeft.fm02 * ARight.fm0123 +
    ALeft.fm03 * ARight.fm01 - ALeft.fm12 * ARight.fm23 + ALeft.fm13 *
    ARight.fms + ALeft.fm23 * ARight.fm12 - ALeft.fm012 * ARight.fm023 +
    ALeft.fm013 * ARight.fm0 + ALeft.fm023 * ARight.fm012 + ALeft.fm123 *
    ARight.fm2 - ALeft.fm0123 * ARight.fm02;

  result.fm23 := +ALeft.fms * ARight.fm23 + ALeft.fm0 * ARight.fm023 -
    ALeft.fm1 * ARight.fm123 + ALeft.fm2 * ARight.fm3 - ALeft.fm3 *
    ARight.fm2 + ALeft.fm01 * ARight.fm0123 - ALeft.fm02 * ARight.fm03 +
    ALeft.fm03 * ARight.fm02 + ALeft.fm12 * ARight.fm13 - ALeft.fm13 *
    ARight.fm12 + ALeft.fm23 * ARight.fms + ALeft.fm012 * ARight.fm013 -
    ALeft.fm013 * ARight.fm012 + ALeft.fm023 * ARight.fm0 - ALeft.fm123 *
    ARight.fm1 + ALeft.fm0123 * ARight.fm01;

  result.fm012 := +ALeft.fms * ARight.fm012 + ALeft.fm0 * ARight.fm12 -
    ALeft.fm1 * ARight.fm02 + ALeft.fm2 * ARight.fm01 + ALeft.fm3 *
    ARight.fm0123 + ALeft.fm01 * ARight.fm2 - ALeft.fm02 * ARight.fm1 -
    ALeft.fm03 * ARight.fm123 + ALeft.fm12 * ARight.fm0 + ALeft.fm13 *
    ARight.fm023 - ALeft.fm23 * ARight.fm013 + ALeft.fm012 * ARight.fms +
    ALeft.fm013 * ARight.fm23 - ALeft.fm023 * ARight.fm13 + ALeft.fm123 *
    ARight.fm03 - ALeft.fm0123 * ARight.fm3;

  result.fm013 := +ALeft.fms * ARight.fm013 + ALeft.fm0 * ARight.fm13 -
    ALeft.fm1 * ARight.fm03 - ALeft.fm2 * ARight.fm0123 + ALeft.fm3 *
    ARight.fm01 + ALeft.fm01 * ARight.fm3 + ALeft.fm02 * ARight.fm123 -
    ALeft.fm03 * ARight.fm1 - ALeft.fm12 * ARight.fm023 + ALeft.fm13 *
    ARight.fm0 + ALeft.fm23 * ARight.fm012 - ALeft.fm012 * ARight.fm23 +
    ALeft.fm013 * ARight.fms + ALeft.fm023 * ARight.fm12 - ALeft.fm123 *
    ARight.fm02 + ALeft.fm0123 * ARight.fm2;

  result.fm023 := +ALeft.fms * ARight.fm023 + ALeft.fm0 * ARight.fm23 +
    ALeft.fm1 * ARight.fm0123 - ALeft.fm2 * ARight.fm03 + ALeft.fm3 *
    ARight.fm02 - ALeft.fm01 * ARight.fm123 + ALeft.fm02 * ARight.fm3 -
    ALeft.fm03 * ARight.fm2 + ALeft.fm12 * ARight.fm013 - ALeft.fm13 *
    ARight.fm012 + ALeft.fm23 * ARight.fm0 + ALeft.fm012 * ARight.fm13 -
    ALeft.fm013 * ARight.fm12 + ALeft.fm023 * ARight.fms + ALeft.fm123 *
    ARight.fm01 - ALeft.fm0123 * ARight.fm1;

  result.fm123 := +ALeft.fms * ARight.fm123 + ALeft.fm0 * ARight.fm0123 +
    ALeft.fm1 * ARight.fm23 - ALeft.fm2 * ARight.fm13 + ALeft.fm3 *
    ARight.fm12 - ALeft.fm01 * ARight.fm023 + ALeft.fm02 * ARight.fm013 -
    ALeft.fm03 * ARight.fm012 + ALeft.fm12 * ARight.fm3 - ALeft.fm13 *
    ARight.fm2 + ALeft.fm23 * ARight.fm1 + ALeft.fm012 * ARight.fm03 -
    ALeft.fm013 * ARight.fm02 + ALeft.fm023 * ARight.fm01 + ALeft.fm123 *
    ARight.fms - ALeft.fm0123 * ARight.fm0;

  result.fm0123 := +ALeft.fms * ARight.fm0123 + ALeft.fm0 * ARight.fm123 -
    ALeft.fm1 * ARight.fm023 + ALeft.fm2 * ARight.fm013 - ALeft.fm3 *
    ARight.fm012 + ALeft.fm01 * ARight.fm23 - ALeft.fm02 * ARight.fm13 +
    ALeft.fm03 * ARight.fm12 + ALeft.fm12 * ARight.fm03 - ALeft.fm13 *
    ARight.fm02 + ALeft.fm23 * ARight.fm01 + ALeft.fm012 * ARight.fm3 -
    ALeft.fm013 * ARight.fm2 + ALeft.fm023 * ARight.fm1 - ALeft.fm123 *
    ARight.fm0 + ALeft.fm0123 * ARight.fms;

end;

class operator TMultivector./(const ALeft: TMultivector; const ARight: double): TMultivector;
begin
  result.fms    := ALeft.fms    / ARight;
  result.fm0    := ALeft.fm0    / ARight;
  result.fm1    := ALeft.fm1    / ARight;
  result.fm2    := ALeft.fm2    / ARight;
  result.fm3    := ALeft.fm3    / ARight;
  result.fm01   := ALeft.fm01   / ARight;
  result.fm02   := ALeft.fm02   / ARight;
  result.fm03   := ALeft.fm03   / ARight;
  result.fm12   := ALeft.fm12   / ARight;
  result.fm13   := ALeft.fm13   / ARight;
  result.fm23   := ALeft.fm23   / ARight;
  result.fm012  := ALeft.fm012  / ARight;
  result.fm013  := ALeft.fm013  / ARight;
  result.fm023  := ALeft.fm023  / ARight;
  result.fm123  := ALeft.fm123  / ARight;
  result.fm0123 := ALeft.fm0123 / ARight;
end;

class operator TMultivector./(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TMultivector./(const ALeft, ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TQuadrivector.=(const ALeft, ARight: TQuadrivector): boolean;
begin
  result :=
    (ALeft.fm0123 = ARight.fm0123);
end;

class operator TQuadrivector.=(const ALeft: TQuadrivector;
  ARight: TMultivector): boolean;
begin
  result :=
    (0 = ARight.fms) and (0 = ARight.fm0) and (0 = ARight.fm1) and
    (0 = ARight.fm2) and (0 = ARight.fm3) and (0 = ARight.fm01) and
    (0 = ARight.fm02) and (0 = ARight.fm03) and (0 = ARight.fm12) and
    (0 = ARight.fm13) and (0 = ARight.fm23) and (0 = ARight.fm012) and
    (0 = ARight.fm013) and (0 = ARight.fm023) and (0 = ARight.fm123) and
    (ALeft.fm0123 = ARight.fm0123);
end;

class operator TQuadrivector.=(const ALeft: TMultivector;
  ARight: TQuadrivector): boolean;
begin
  result :=
    (ALeft.fms = 0) and (ALeft.fm0 = 0) and (ALeft.fm1 = 0) and
    (ALeft.fm2 = 0) and (ALeft.fm3 = 0) and (ALeft.fm01 = 0) and
    (ALeft.fm02 = 0) and (ALeft.fm03 = 0) and (ALeft.fm12 = 0) and
    (ALeft.fm13 = 0) and (ALeft.fm23 = 0) and (ALeft.fm012 = 0) and
    (ALeft.fm013 = 0) and (ALeft.fm023 = 0) and (ALeft.fm123 = 0) and
    (ALeft.fm0123 = ARight.fm0123);
end;

class operator TQuadrivector.<>(const ALeft, ARight: TQuadrivector): boolean;
begin
  result :=
    (ALeft.fm0123 <> ARight.fm0123);
end;

class operator TQuadrivector.<>(const ALeft: TQuadrivector;
  ARight: TMultivector): boolean;
begin
  result :=
    (0 <> ARight.fms) and (0 <> ARight.fm0) and (0 <> ARight.fm1) and
    (0 <> ARight.fm2) and (0 <> ARight.fm3) and (0 <> ARight.fm01) and
    (0 <> ARight.fm02) and (0 <> ARight.fm03) and (0 <> ARight.fm12) and
    (0 <> ARight.fm13) and (0 <> ARight.fm23) and (0 <> ARight.fm012) and
    (0 <> ARight.fm013) and (0 <> ARight.fm023) and (0 <> ARight.fm123) and
    (ALeft.fm0123 <> ARight.fm0123);
end;

class operator TQuadrivector.<>(const ALeft: TMultivector;
  ARight: TQuadrivector): boolean;
begin
  result :=
    (ALeft.fms <> 0) and (ALeft.fm0 <> 0) and (ALeft.fm1 <> 0) and
    (ALeft.fm2 <> 0) and (ALeft.fm3 <> 0) and (ALeft.fm01 <> 0) and
    (ALeft.fm02 <> 0) and (ALeft.fm03 <> 0) and (ALeft.fm12 <> 0) and
    (ALeft.fm13 <> 0) and (ALeft.fm23 <> 0) and (ALeft.fm012 <> 0) and
    (ALeft.fm013 <> 0) and (ALeft.fm023 <> 0) and (ALeft.fm123 <> 0) and
    (ALeft.fm0123 <> ARight.fm0123);
end;

class operator TQuadrivector.+(const AValue: TQuadrivector): TQuadrivector;
begin
  result.fm0123 := AValue.fm0123;
end;

class operator TQuadrivector.+(const ALeft: TQuadrivector; ARight: double): TMultivector;
begin
  result.fms := ARight;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := ALeft.fm0123;
end;

class operator TQuadrivector.+(const ALeft: double; ARight: TQuadrivector): TMultivector;
begin
  result.fms := ALeft;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := ARight.fm0123;
end;

class operator TQuadrivector.+(const ALeft, ARight: TQuadrivector): TQuadrivector;
begin
  result.fm0123 := ALeft.fm0123 + ARight.fm0123;
end;

class operator TQuadrivector.+(const ALeft: TQuadrivector;
  ARight: TMultivector): TMultivector;
begin
  result.fms := ARight.fms;
  result.fm0 := ARight.fm0;
  result.fm1 := ARight.fm1;
  result.fm2 := ARight.fm2;
  result.fm3 := ARight.fm3;
  result.fm01 := ARight.fm01;
  result.fm02 := ARight.fm02;
  result.fm03 := ARight.fm03;
  result.fm12 := ARight.fm12;
  result.fm13 := ARight.fm13;
  result.fm23 := ARight.fm23;
  result.fm012 := ARight.fm012;
  result.fm013 := ARight.fm013;
  result.fm023 := ARight.fm023;
  result.fm123 := ARight.fm123;
  result.fm0123 := ALeft.fm0123 + ARight.fm0123;
end;

class operator TQuadrivector.+(const ALeft: TMultivector;
  ARight: TQuadrivector): TMultivector;
begin
  result.fms := ALeft.fms;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := ALeft.fm0123 + ARight.fm0123;
end;

class operator TQuadrivector.-(const AValue: TQuadrivector): TQuadrivector;
begin
  result.fm0123 := -AValue.fm0123;
end;

class operator TQuadrivector.-(const ALeft: TQuadrivector; ARight: double): TMultivector;
begin
  result.fms := -ARight;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := ALeft.fm0123;
end;

class operator TQuadrivector.-(const ALeft: double; ARight: TQuadrivector): TMultivector;
begin
  result.fms := ALeft;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := -ARight.fm0123;
end;

class operator TQuadrivector.-(const ALeft, ARight: TQuadrivector): TQuadrivector;
begin
  result.fm0123 := ALeft.fm0123 - ARight.fm0123;
end;

class operator TQuadrivector.-(const ALeft: TQuadrivector;
  ARight: TMultivector): TMultivector;
begin
  result.fms := -ARight.fms;
  result.fm0 := -ARight.fm0;
  result.fm1 := -ARight.fm1;
  result.fm2 := -ARight.fm2;
  result.fm3 := -ARight.fm3;
  result.fm01 := -ARight.fm01;
  result.fm02 := -ARight.fm02;
  result.fm03 := -ARight.fm03;
  result.fm12 := -ARight.fm12;
  result.fm13 := -ARight.fm13;
  result.fm23 := -ARight.fm23;
  result.fm012 := -ARight.fm012;
  result.fm013 := -ARight.fm013;
  result.fm023 := -ARight.fm023;
  result.fm123 := -ARight.fm123;
  result.fm0123 := ALeft.fm0123 - ARight.fm0123;
end;

class operator TQuadrivector.-(const ALeft: TMultivector;
  ARight: TQuadrivector): TMultivector;
begin
  result.fms := ALeft.fms;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := ALeft.fm0123 - ARight.fm0123;
end;

class operator TQuadrivector.*(const ALeft: TQuadrivector;
  const ARight: double): TQuadrivector;
begin
  result.fm0123 := +ALeft.fm0123 * ARight;

end;

class operator TQuadrivector.*(const ALeft: double;
  const ARight: TQuadrivector): TQuadrivector;
begin
  result.fm0123 := +ALeft * ARight.fm0123;

end;

class operator TQuadrivector.*(const ALeft, ARight: TQuadrivector): double;
begin
  result := -ALeft.fm0123 * ARight.fm0123;

end;

class operator TQuadrivector.*(const ALeft: TQuadrivector;
  const ARight: TMultivector): TMultivector;
begin
  result.fms := -ALeft.fm0123 * ARight.fm0123;

  result.fm0 := +ALeft.fm0123 * ARight.fm123;

  result.fm1 := +ALeft.fm0123 * ARight.fm023;

  result.fm2 := -ALeft.fm0123 * ARight.fm013;

  result.fm3 := +ALeft.fm0123 * ARight.fm012;

  result.fm01 := -ALeft.fm0123 * ARight.fm23;

  result.fm02 := +ALeft.fm0123 * ARight.fm13;

  result.fm03 := -ALeft.fm0123 * ARight.fm12;

  result.fm12 := +ALeft.fm0123 * ARight.fm03;

  result.fm13 := -ALeft.fm0123 * ARight.fm02;

  result.fm23 := +ALeft.fm0123 * ARight.fm01;

  result.fm012 := -ALeft.fm0123 * ARight.fm3;

  result.fm013 := +ALeft.fm0123 * ARight.fm2;

  result.fm023 := -ALeft.fm0123 * ARight.fm1;

  result.fm123 := -ALeft.fm0123 * ARight.fm0;

  result.fm0123 := +ALeft.fm0123 * ARight.fms;

end;

class operator TQuadrivector.*(const ALeft: TMultivector;
  const ARight: TQuadrivector): TMultivector;
begin
  result.fms := -ALeft.fm0123 * ARight.fm0123;

  result.fm0 := -ALeft.fm123 * ARight.fm0123;

  result.fm1 := -ALeft.fm023 * ARight.fm0123;

  result.fm2 := +ALeft.fm013 * ARight.fm0123;

  result.fm3 := -ALeft.fm012 * ARight.fm0123;

  result.fm01 := -ALeft.fm23 * ARight.fm0123;

  result.fm02 := +ALeft.fm13 * ARight.fm0123;

  result.fm03 := -ALeft.fm12 * ARight.fm0123;

  result.fm12 := +ALeft.fm03 * ARight.fm0123;

  result.fm13 := -ALeft.fm02 * ARight.fm0123;

  result.fm23 := +ALeft.fm01 * ARight.fm0123;

  result.fm012 := +ALeft.fm3 * ARight.fm0123;

  result.fm013 := -ALeft.fm2 * ARight.fm0123;

  result.fm023 := +ALeft.fm1 * ARight.fm0123;

  result.fm123 := +ALeft.fm0 * ARight.fm0123;

  result.fm0123 := +ALeft.fms * ARight.fm0123;

end;

class operator TQuadrivector./(const ALeft: TQuadrivector; const ARight: double): TQuadrivector;
begin
  result.fm0123 := ALeft.fm0123 / ARight;
end;

class operator TQuadrivector./(const ALeft: double; const ARight: TQuadrivector): TQuadrivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TQuadrivector./(const ALeft, ARight: TQuadrivector): double;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TQuadrivector./(const ALeft: TQuadrivector; const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TQuadrivector./(const ALeft: TMultivector; const ARight: TQuadrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TTrivector.=(const ALeft, ARight: TTrivector): boolean;
begin
  result :=
    (ALeft.fm012 = ARight.fm012) and (ALeft.fm013 = ARight.fm013) and
    (ALeft.fm023 = ARight.fm023) and (ALeft.fm123 = ARight.fm123);
end;

class operator TTrivector.=(const ALeft: TTrivector; ARight: TMultivector): boolean;
begin
  result :=
    (0 = ARight.fms) and (0 = ARight.fm0) and (0 = ARight.fm1) and
    (0 = ARight.fm2) and (0 = ARight.fm3) and (0 = ARight.fm01) and
    (0 = ARight.fm02) and (0 = ARight.fm03) and (0 = ARight.fm12) and
    (0 = ARight.fm13) and (0 = ARight.fm23) and (ALeft.fm012 = ARight.fm012) and
    (ALeft.fm013 = ARight.fm013) and (ALeft.fm023 = ARight.fm023) and
    (ALeft.fm123 = ARight.fm123) and (0 = ARight.fm0123);
end;

class operator TTrivector.=(const ALeft: TMultivector; ARight: TTrivector): boolean;
begin
  result :=
    (ALeft.fms = 0) and (ALeft.fm0 = 0) and (ALeft.fm1 = 0) and
    (ALeft.fm2 = 0) and (ALeft.fm3 = 0) and (ALeft.fm01 = 0) and
    (ALeft.fm02 = 0) and (ALeft.fm03 = 0) and (ALeft.fm12 = 0) and
    (ALeft.fm13 = 0) and (ALeft.fm23 = 0) and (ALeft.fm012 = ARight.fm012) and
    (ALeft.fm013 = ARight.fm013) and (ALeft.fm023 = ARight.fm023) and
    (ALeft.fm123 = ARight.fm123) and (ALeft.fm0123 = 0);
end;

class operator TTrivector.<>(const ALeft, ARight: TTrivector): boolean;
begin
  result :=
    (ALeft.fm012 <> ARight.fm012) and (ALeft.fm013 <> ARight.fm013) and
    (ALeft.fm023 <> ARight.fm023) and (ALeft.fm123 <> ARight.fm123);
end;

class operator TTrivector.<>(const ALeft: TTrivector; ARight: TMultivector): boolean;
begin
  result :=
    (0 <> ARight.fms) and (0 <> ARight.fm0) and (0 <> ARight.fm1) and
    (0 <> ARight.fm2) and (0 <> ARight.fm3) and (0 <> ARight.fm01) and
    (0 <> ARight.fm02) and (0 <> ARight.fm03) and (0 <> ARight.fm12) and
    (0 <> ARight.fm13) and (0 <> ARight.fm23) and (ALeft.fm012 <> ARight.fm012) and
    (ALeft.fm013 <> ARight.fm013) and (ALeft.fm023 <> ARight.fm023) and
    (ALeft.fm123 <> ARight.fm123) and (0 <> ARight.fm0123);
end;

class operator TTrivector.<>(const ALeft: TMultivector; ARight: TTrivector): boolean;
begin
  result :=
    (ALeft.fms <> 0) and (ALeft.fm0 <> 0) and (ALeft.fm1 <> 0) and
    (ALeft.fm2 <> 0) and (ALeft.fm3 <> 0) and (ALeft.fm01 <> 0) and
    (ALeft.fm02 <> 0) and (ALeft.fm03 <> 0) and (ALeft.fm12 <> 0) and
    (ALeft.fm13 <> 0) and (ALeft.fm23 <> 0) and (ALeft.fm012 <> ARight.fm012) and
    (ALeft.fm013 <> ARight.fm013) and (ALeft.fm023 <> ARight.fm023) and
    (ALeft.fm123 <> ARight.fm123) and (ALeft.fm0123 <> 0);
end;

class operator TTrivector.+(const AValue: TTrivector): TTrivector;
begin
  result.fm012 := AValue.fm012;
  result.fm013 := AValue.fm013;
  result.fm023 := AValue.fm023;
  result.fm123 := AValue.fm123;
end;

class operator TTrivector.+(const ALeft: TTrivector; ARight: double): TMultivector;
begin
  result.fms := ARight;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := 0;
end;

class operator TTrivector.+(const ALeft: double; ARight: TTrivector): TMultivector;
begin
  result.fms := ALeft;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := ARight.fm012;
  result.fm013 := ARight.fm013;
  result.fm023 := ARight.fm023;
  result.fm123 := ARight.fm123;
  result.fm0123 := 0;
end;

class operator TTrivector.+(const ALeft, ARight: TTrivector): TTrivector;
begin
  result.fm012 := ALeft.fm012 + ARight.fm012;
  result.fm013 := ALeft.fm013 + ARight.fm013;
  result.fm023 := ALeft.fm023 + ARight.fm023;
  result.fm123 := ALeft.fm123 + ARight.fm123;
end;

class operator TTrivector.+(const ALeft: TTrivector;
  ARight: TQuadrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := ARight.fm0123;
end;

class operator TTrivector.+(const ALeft: TQuadrivector;
  ARight: TTrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := ARight.fm012;
  result.fm013 := ARight.fm013;
  result.fm023 := ARight.fm023;
  result.fm123 := ARight.fm123;
  result.fm0123 := ALeft.fm0123;
end;

class operator TTrivector.+(const ALeft: TTrivector; ARight: TMultivector): TMultivector;
begin
  result.fms := ARight.fms;
  result.fm0 := ARight.fm0;
  result.fm1 := ARight.fm1;
  result.fm2 := ARight.fm2;
  result.fm3 := ARight.fm3;
  result.fm01 := ARight.fm01;
  result.fm02 := ARight.fm02;
  result.fm03 := ARight.fm03;
  result.fm12 := ARight.fm12;
  result.fm13 := ARight.fm13;
  result.fm23 := ARight.fm23;
  result.fm012 := ALeft.fm012 + ARight.fm012;
  result.fm013 := ALeft.fm013 + ARight.fm013;
  result.fm023 := ALeft.fm023 + ARight.fm023;
  result.fm123 := ALeft.fm123 + ARight.fm123;
  result.fm0123 := ARight.fm0123;
end;

class operator TTrivector.+(const ALeft: TMultivector; ARight: TTrivector): TMultivector;
begin
  result.fms := ALeft.fms;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := ALeft.fm012 + ARight.fm012;
  result.fm013 := ALeft.fm013 + ARight.fm013;
  result.fm023 := ALeft.fm023 + ARight.fm023;
  result.fm123 := ALeft.fm123 + ARight.fm123;
  result.fm0123 := ALeft.fm0123;
end;

class operator TTrivector.-(const AValue: TTrivector): TTrivector;
begin
  result.fm012 := -AValue.fm012;
  result.fm013 := -AValue.fm013;
  result.fm023 := -AValue.fm023;
  result.fm123 := -AValue.fm123;
end;

class operator TTrivector.-(const ALeft: TTrivector; ARight: double): TMultivector;
begin
  result.fms := -ARight;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := 0;
end;

class operator TTrivector.-(const ALeft: double; ARight: TTrivector): TMultivector;
begin
  result.fms := ALeft;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := -ARight.fm012;
  result.fm013 := -ARight.fm013;
  result.fm023 := -ARight.fm023;
  result.fm123 := -ARight.fm123;
  result.fm0123 := 0;
end;

class operator TTrivector.-(const ALeft, ARight: TTrivector): TTrivector;
begin
  result.fm012 := ALeft.fm012 - ARight.fm012;
  result.fm013 := ALeft.fm013 - ARight.fm013;
  result.fm023 := ALeft.fm023 - ARight.fm023;
  result.fm123 := ALeft.fm123 - ARight.fm123;
end;

class operator TTrivector.-(const ALeft: TTrivector;
  ARight: TQuadrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := -ARight.fm0123;
end;

class operator TTrivector.-(const ALeft: TQuadrivector;
  ARight: TTrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := -ARight.fm012;
  result.fm013 := -ARight.fm013;
  result.fm023 := -ARight.fm023;
  result.fm123 := -ARight.fm123;
  result.fm0123 := ALeft.fm0123;
end;

class operator TTrivector.-(const ALeft: TTrivector; ARight: TMultivector): TMultivector;
begin
  result.fms := -ARight.fms;
  result.fm0 := -ARight.fm0;
  result.fm1 := -ARight.fm1;
  result.fm2 := -ARight.fm2;
  result.fm3 := -ARight.fm3;
  result.fm01 := -ARight.fm01;
  result.fm02 := -ARight.fm02;
  result.fm03 := -ARight.fm03;
  result.fm12 := -ARight.fm12;
  result.fm13 := -ARight.fm13;
  result.fm23 := -ARight.fm23;
  result.fm012 := ALeft.fm012 - ARight.fm012;
  result.fm013 := ALeft.fm013 - ARight.fm013;
  result.fm023 := ALeft.fm023 - ARight.fm023;
  result.fm123 := ALeft.fm123 - ARight.fm123;
  result.fm0123 := -ARight.fm0123;
end;

class operator TTrivector.-(const ALeft: TMultivector; ARight: TTrivector): TMultivector;
begin
  result.fms := ALeft.fms;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := ALeft.fm012 - ARight.fm012;
  result.fm013 := ALeft.fm013 - ARight.fm013;
  result.fm023 := ALeft.fm023 - ARight.fm023;
  result.fm123 := ALeft.fm123 - ARight.fm123;
  result.fm0123 := ALeft.fm0123;
end;

class operator TTrivector.*(const ALeft: TTrivector; const ARight: double): TTrivector;
begin
  result.fm012 := +ALeft.fm012 * ARight;

  result.fm013 := +ALeft.fm013 * ARight;

  result.fm023 := +ALeft.fm023 * ARight;

  result.fm123 := +ALeft.fm123 * ARight;

end;

class operator TTrivector.*(const ALeft: double; const ARight: TTrivector): TTrivector;
begin
  result.fm012 := +ALeft * ARight.fm012;

  result.fm013 := +ALeft * ARight.fm013;

  result.fm023 := +ALeft * ARight.fm023;

  result.fm123 := +ALeft * ARight.fm123;

end;

class operator TTrivector.*(const ALeft, ARight: TTrivector): TMultivector;
begin
  result.fms := -ALeft.fm012 * ARight.fm012 - ALeft.fm013 * ARight.fm013 -
    ALeft.fm023 * ARight.fm023 + ALeft.fm123 * ARight.fm123;

  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := -ALeft.fm023 * ARight.fm123 + ALeft.fm123 * ARight.fm023;

  result.fm02 := +ALeft.fm013 * ARight.fm123 - ALeft.fm123 * ARight.fm013;

  result.fm03 := -ALeft.fm012 * ARight.fm123 + ALeft.fm123 * ARight.fm012;

  result.fm12 := +ALeft.fm013 * ARight.fm023 - ALeft.fm023 * ARight.fm013;

  result.fm13 := -ALeft.fm012 * ARight.fm023 + ALeft.fm023 * ARight.fm012;

  result.fm23 := +ALeft.fm012 * ARight.fm013 - ALeft.fm013 * ARight.fm012;

  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TTrivector.*(const ALeft: TTrivector;
  const ARight: TQuadrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := -ALeft.fm123 * ARight.fm0123;

  result.fm1 := -ALeft.fm023 * ARight.fm0123;

  result.fm2 := +ALeft.fm013 * ARight.fm0123;

  result.fm3 := -ALeft.fm012 * ARight.fm0123;

  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TTrivector.*(const ALeft: TQuadrivector;
  const ARight: TTrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := +ALeft.fm0123 * ARight.fm123;

  result.fm1 := +ALeft.fm0123 * ARight.fm023;

  result.fm2 := -ALeft.fm0123 * ARight.fm013;

  result.fm3 := +ALeft.fm0123 * ARight.fm012;

  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TTrivector.*(const ALeft: TTrivector;
  const ARight: TMultivector): TMultivector;
begin
  result.fms := -ALeft.fm012 * ARight.fm012 - ALeft.fm013 * ARight.fm013 -
    ALeft.fm023 * ARight.fm023 + ALeft.fm123 * ARight.fm123;

  result.fm0 := -ALeft.fm012 * ARight.fm12 - ALeft.fm013 * ARight.fm13 -
    ALeft.fm023 * ARight.fm23 - ALeft.fm123 * ARight.fm0123;

  result.fm1 := -ALeft.fm012 * ARight.fm02 - ALeft.fm013 * ARight.fm03 -
    ALeft.fm023 * ARight.fm0123 - ALeft.fm123 * ARight.fm23;

  result.fm2 := +ALeft.fm012 * ARight.fm01 + ALeft.fm013 * ARight.fm0123 -
    ALeft.fm023 * ARight.fm03 + ALeft.fm123 * ARight.fm13;

  result.fm3 := -ALeft.fm012 * ARight.fm0123 + ALeft.fm013 * ARight.fm01 +
    ALeft.fm023 * ARight.fm02 - ALeft.fm123 * ARight.fm12;

  result.fm01 := -ALeft.fm012 * ARight.fm2 - ALeft.fm013 * ARight.fm3 -
    ALeft.fm023 * ARight.fm123 + ALeft.fm123 * ARight.fm023;

  result.fm02 := +ALeft.fm012 * ARight.fm1 + ALeft.fm013 * ARight.fm123 -
    ALeft.fm023 * ARight.fm3 - ALeft.fm123 * ARight.fm013;

  result.fm03 := -ALeft.fm012 * ARight.fm123 + ALeft.fm013 * ARight.fm1 +
    ALeft.fm023 * ARight.fm2 + ALeft.fm123 * ARight.fm012;

  result.fm12 := +ALeft.fm012 * ARight.fm0 + ALeft.fm013 * ARight.fm023 -
    ALeft.fm023 * ARight.fm013 - ALeft.fm123 * ARight.fm3;

  result.fm13 := -ALeft.fm012 * ARight.fm023 + ALeft.fm013 * ARight.fm0 +
    ALeft.fm023 * ARight.fm012 + ALeft.fm123 * ARight.fm2;

  result.fm23 := +ALeft.fm012 * ARight.fm013 - ALeft.fm013 * ARight.fm012 +
    ALeft.fm023 * ARight.fm0 - ALeft.fm123 * ARight.fm1;

  result.fm012 := +ALeft.fm012 * ARight.fms + ALeft.fm013 * ARight.fm23 -
    ALeft.fm023 * ARight.fm13 + ALeft.fm123 * ARight.fm03;

  result.fm013 := -ALeft.fm012 * ARight.fm23 + ALeft.fm013 * ARight.fms +
    ALeft.fm023 * ARight.fm12 - ALeft.fm123 * ARight.fm02;

  result.fm023 := +ALeft.fm012 * ARight.fm13 - ALeft.fm013 * ARight.fm12 +
    ALeft.fm023 * ARight.fms + ALeft.fm123 * ARight.fm01;

  result.fm123 := +ALeft.fm012 * ARight.fm03 - ALeft.fm013 * ARight.fm02 +
    ALeft.fm023 * ARight.fm01 + ALeft.fm123 * ARight.fms;

  result.fm0123 := +ALeft.fm012 * ARight.fm3 - ALeft.fm013 * ARight.fm2 +
    ALeft.fm023 * ARight.fm1 - ALeft.fm123 * ARight.fm0;

end;

class operator TTrivector.*(const ALeft: TMultivector;
  const ARight: TTrivector): TMultivector;
begin
  result.fms := -ALeft.fm012 * ARight.fm012 - ALeft.fm013 * ARight.fm013 -
    ALeft.fm023 * ARight.fm023 + ALeft.fm123 * ARight.fm123;

  result.fm0 := -ALeft.fm12 * ARight.fm012 - ALeft.fm13 * ARight.fm013 -
    ALeft.fm23 * ARight.fm023 + ALeft.fm0123 * ARight.fm123;

  result.fm1 := -ALeft.fm02 * ARight.fm012 - ALeft.fm03 * ARight.fm013 -
    ALeft.fm23 * ARight.fm123 + ALeft.fm0123 * ARight.fm023;

  result.fm2 := +ALeft.fm01 * ARight.fm012 - ALeft.fm03 * ARight.fm023 +
    ALeft.fm13 * ARight.fm123 - ALeft.fm0123 * ARight.fm013;

  result.fm3 := +ALeft.fm01 * ARight.fm013 + ALeft.fm02 * ARight.fm023 -
    ALeft.fm12 * ARight.fm123 + ALeft.fm0123 * ARight.fm012;

  result.fm01 := -ALeft.fm2 * ARight.fm012 - ALeft.fm3 * ARight.fm013 -
    ALeft.fm023 * ARight.fm123 + ALeft.fm123 * ARight.fm023;

  result.fm02 := +ALeft.fm1 * ARight.fm012 - ALeft.fm3 * ARight.fm023 +
    ALeft.fm013 * ARight.fm123 - ALeft.fm123 * ARight.fm013;

  result.fm03 := +ALeft.fm1 * ARight.fm013 + ALeft.fm2 * ARight.fm023 -
    ALeft.fm012 * ARight.fm123 + ALeft.fm123 * ARight.fm012;

  result.fm12 := +ALeft.fm0 * ARight.fm012 - ALeft.fm3 * ARight.fm123 +
    ALeft.fm013 * ARight.fm023 - ALeft.fm023 * ARight.fm013;

  result.fm13 := +ALeft.fm0 * ARight.fm013 + ALeft.fm2 * ARight.fm123 -
    ALeft.fm012 * ARight.fm023 + ALeft.fm023 * ARight.fm012;

  result.fm23 := +ALeft.fm0 * ARight.fm023 - ALeft.fm1 * ARight.fm123 +
    ALeft.fm012 * ARight.fm013 - ALeft.fm013 * ARight.fm012;

  result.fm012 := +ALeft.fms * ARight.fm012 - ALeft.fm03 * ARight.fm123 +
    ALeft.fm13 * ARight.fm023 - ALeft.fm23 * ARight.fm013;

  result.fm013 := +ALeft.fms * ARight.fm013 + ALeft.fm02 * ARight.fm123 -
    ALeft.fm12 * ARight.fm023 + ALeft.fm23 * ARight.fm012;

  result.fm023 := +ALeft.fms * ARight.fm023 - ALeft.fm01 * ARight.fm123 +
    ALeft.fm12 * ARight.fm013 - ALeft.fm13 * ARight.fm012;

  result.fm123 := +ALeft.fms * ARight.fm123 - ALeft.fm01 * ARight.fm023 +
    ALeft.fm02 * ARight.fm013 - ALeft.fm03 * ARight.fm012;

  result.fm0123 := +ALeft.fm0 * ARight.fm123 - ALeft.fm1 * ARight.fm023 +
    ALeft.fm2 * ARight.fm013 - ALeft.fm3 * ARight.fm012;

end;

class operator TTrivector./(const ALeft: TTrivector; const ARight: double): TTrivector;
begin
  result.fm012 := ALeft.fm012 / ARight;
  result.fm013 := ALeft.fm013 / ARight;
  result.fm023 := ALeft.fm023 / ARight;
  result.fm123 := ALeft.fm123 / ARight;
end;

class operator TTrivector./(const ALeft: double; const ARight: TTrivector): TTrivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TTrivector./(const ALeft, ARight: TTrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TTrivector./(const ALeft: TTrivector;
  const ARight: TQuadrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TTrivector./(const ALeft: TQuadrivector;
  const ARight: TTrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TTrivector./(const ALeft: TTrivector;
  const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TTrivector./(const ALeft: TMultivector;
  const ARight: TTrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector.=(const ALeft, ARight: TBivector): boolean;
begin
  result :=
    (ALeft.fm01 = ARight.fm01) and (ALeft.fm02 = ARight.fm02) and
    (ALeft.fm03 = ARight.fm03) and (ALeft.fm12 = ARight.fm12) and
    (ALeft.fm13 = ARight.fm13) and (ALeft.fm23 = ARight.fm23);
end;

class operator TBivector.=(const ALeft: TBivector; ARight: TMultivector): boolean;
begin
  result :=
    (0 = ARight.fms) and (0 = ARight.fm0) and (0 = ARight.fm1) and
    (0 = ARight.fm2) and (0 = ARight.fm3) and (ALeft.fm01 = ARight.fm01) and
    (ALeft.fm02 = ARight.fm02) and (ALeft.fm03 = ARight.fm03) and
    (ALeft.fm12 = ARight.fm12) and (ALeft.fm13 = ARight.fm13) and
    (ALeft.fm23 = ARight.fm23) and (0 = ARight.fm012) and (0 = ARight.fm013) and
    (0 = ARight.fm023) and (0 = ARight.fm123) and (0 = ARight.fm0123);
end;

class operator TBivector.=(const ALeft: TMultivector; ARight: TBivector): boolean;
begin
  result :=
    (ALeft.fms = 0) and (ALeft.fm0 = 0) and (ALeft.fm1 = 0) and
    (ALeft.fm2 = 0) and (ALeft.fm3 = 0) and (ALeft.fm01 = ARight.fm01) and
    (ALeft.fm02 = ARight.fm02) and (ALeft.fm03 = ARight.fm03) and
    (ALeft.fm12 = ARight.fm12) and (ALeft.fm13 = ARight.fm13) and
    (ALeft.fm23 = ARight.fm23) and (ALeft.fm012 = 0) and (ALeft.fm013 = 0) and
    (ALeft.fm023 = 0) and (ALeft.fm123 = 0) and (ALeft.fm0123 = 0);
end;

class operator TBivector.<>(const ALeft, ARight: TBivector): boolean;
begin
  result :=
    (ALeft.fm01 <> ARight.fm01) and (ALeft.fm02 <> ARight.fm02) and
    (ALeft.fm03 <> ARight.fm03) and (ALeft.fm12 <> ARight.fm12) and
    (ALeft.fm13 <> ARight.fm13) and (ALeft.fm23 <> ARight.fm23);
end;

class operator TBivector.<>(const ALeft: TBivector; ARight: TMultivector): boolean;
begin
  result :=
    (0 <> ARight.fms) and (0 <> ARight.fm0) and (0 <> ARight.fm1) and
    (0 <> ARight.fm2) and (0 <> ARight.fm3) and (ALeft.fm01 <> ARight.fm01) and
    (ALeft.fm02 <> ARight.fm02) and (ALeft.fm03 <> ARight.fm03) and
    (ALeft.fm12 <> ARight.fm12) and (ALeft.fm13 <> ARight.fm13) and
    (ALeft.fm23 <> ARight.fm23) and (0 <> ARight.fm012) and
    (0 <> ARight.fm013) and (0 <> ARight.fm023) and (0 <> ARight.fm123) and
    (0 <> ARight.fm0123);
end;

class operator TBivector.<>(const ALeft: TMultivector; ARight: TBivector): boolean;
begin
  result :=
    (ALeft.fms <> 0) and (ALeft.fm0 <> 0) and (ALeft.fm1 <> 0) and
    (ALeft.fm2 <> 0) and (ALeft.fm3 <> 0) and (ALeft.fm01 <> ARight.fm01) and
    (ALeft.fm02 <> ARight.fm02) and (ALeft.fm03 <> ARight.fm03) and
    (ALeft.fm12 <> ARight.fm12) and (ALeft.fm13 <> ARight.fm13) and
    (ALeft.fm23 <> ARight.fm23) and (ALeft.fm012 <> 0) and
    (ALeft.fm013 <> 0) and (ALeft.fm023 <> 0) and (ALeft.fm123 <> 0) and
    (ALeft.fm0123 <> 0);
end;

class operator TBivector.+(const AValue: TBivector): TBivector;
begin
  result.fm01 := AValue.fm01;
  result.fm02 := AValue.fm02;
  result.fm03 := AValue.fm03;
  result.fm12 := AValue.fm12;
  result.fm13 := AValue.fm13;
  result.fm23 := AValue.fm23;
end;

class operator TBivector.+(const ALeft: TBivector; ARight: double): TMultivector;
begin
  result.fms := ARight;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TBivector.+(const ALeft: double; ARight: TBivector): TMultivector;
begin
  result.fms := ALeft;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := ARight.fm01;
  result.fm02 := ARight.fm02;
  result.fm03 := ARight.fm03;
  result.fm12 := ARight.fm12;
  result.fm13 := ARight.fm13;
  result.fm23 := ARight.fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TBivector.+(const ALeft, ARight: TBivector): TBivector;
begin
  result.fm01 := ALeft.fm01 + ARight.fm01;
  result.fm02 := ALeft.fm02 + ARight.fm02;
  result.fm03 := ALeft.fm03 + ARight.fm03;
  result.fm12 := ALeft.fm12 + ARight.fm12;
  result.fm13 := ALeft.fm13 + ARight.fm13;
  result.fm23 := ALeft.fm23 + ARight.fm23;
end;

class operator TBivector.+(const ALeft: TBivector; ARight: TTrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := ARight.fm012;
  result.fm013 := ARight.fm013;
  result.fm023 := ARight.fm023;
  result.fm123 := ARight.fm123;
  result.fm0123 := 0;
end;

class operator TBivector.+(const ALeft: TTrivector; ARight: TBivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := ARight.fm01;
  result.fm02 := ARight.fm02;
  result.fm03 := ARight.fm03;
  result.fm12 := ARight.fm12;
  result.fm13 := ARight.fm13;
  result.fm23 := ARight.fm23;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := 0;
end;

class operator TBivector.+(const ALeft: TBivector; ARight: TQuadrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := ARight.fm0123;
end;

class operator TBivector.+(const ALeft: TQuadrivector; ARight: TBivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := ARight.fm01;
  result.fm02 := ARight.fm02;
  result.fm03 := ARight.fm03;
  result.fm12 := ARight.fm12;
  result.fm13 := ARight.fm13;
  result.fm23 := ARight.fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := ALeft.fm0123;
end;

class operator TBivector.+(const ALeft: TBivector; ARight: TMultivector): TMultivector;
begin
  result.fms := ARight.fms;
  result.fm0 := ARight.fm0;
  result.fm1 := ARight.fm1;
  result.fm2 := ARight.fm2;
  result.fm3 := ARight.fm3;
  result.fm01 := ALeft.fm01 + ARight.fm01;
  result.fm02 := ALeft.fm02 + ARight.fm02;
  result.fm03 := ALeft.fm03 + ARight.fm03;
  result.fm12 := ALeft.fm12 + ARight.fm12;
  result.fm13 := ALeft.fm13 + ARight.fm13;
  result.fm23 := ALeft.fm23 + ARight.fm23;
  result.fm012 := ARight.fm012;
  result.fm013 := ARight.fm013;
  result.fm023 := ARight.fm023;
  result.fm123 := ARight.fm123;
  result.fm0123 := ARight.fm0123;
end;

class operator TBivector.+(const ALeft: TMultivector; ARight: TBivector): TMultivector;
begin
  result.fms := ALeft.fms;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := ALeft.fm01 + ARight.fm01;
  result.fm02 := ALeft.fm02 + ARight.fm02;
  result.fm03 := ALeft.fm03 + ARight.fm03;
  result.fm12 := ALeft.fm12 + ARight.fm12;
  result.fm13 := ALeft.fm13 + ARight.fm13;
  result.fm23 := ALeft.fm23 + ARight.fm23;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := ALeft.fm0123;
end;

class operator TBivector.-(const AValue: TBivector): TBivector;
begin
  result.fm01 := -AValue.fm01;
  result.fm02 := -AValue.fm02;
  result.fm03 := -AValue.fm03;
  result.fm12 := -AValue.fm12;
  result.fm13 := -AValue.fm13;
  result.fm23 := -AValue.fm23;
end;

class operator TBivector.-(const ALeft: TBivector; ARight: double): TMultivector;
begin
  result.fms := -ARight;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TBivector.-(const ALeft: double; ARight: TBivector): TMultivector;
begin
  result.fms := ALeft;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := -ARight.fm01;
  result.fm02 := -ARight.fm02;
  result.fm03 := -ARight.fm03;
  result.fm12 := -ARight.fm12;
  result.fm13 := -ARight.fm13;
  result.fm23 := -ARight.fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TBivector.-(const ALeft, ARight: TBivector): TBivector;
begin
  result.fm01 := ALeft.fm01 - ARight.fm01;
  result.fm02 := ALeft.fm02 - ARight.fm02;
  result.fm03 := ALeft.fm03 - ARight.fm03;
  result.fm12 := ALeft.fm12 - ARight.fm12;
  result.fm13 := ALeft.fm13 - ARight.fm13;
  result.fm23 := ALeft.fm23 - ARight.fm23;
end;

class operator TBivector.-(const ALeft: TBivector; ARight: TTrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := -ARight.fm012;
  result.fm013 := -ARight.fm013;
  result.fm023 := -ARight.fm023;
  result.fm123 := -ARight.fm123;
  result.fm0123 := 0;
end;

class operator TBivector.-(const ALeft: TTrivector; ARight: TBivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := -ARight.fm01;
  result.fm02 := -ARight.fm02;
  result.fm03 := -ARight.fm03;
  result.fm12 := -ARight.fm12;
  result.fm13 := -ARight.fm13;
  result.fm23 := -ARight.fm23;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := 0;
end;

class operator TBivector.-(const ALeft: TBivector; ARight: TQuadrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := -ARight.fm0123;
end;

class operator TBivector.-(const ALeft: TQuadrivector; ARight: TBivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := -ARight.fm01;
  result.fm02 := -ARight.fm02;
  result.fm03 := -ARight.fm03;
  result.fm12 := -ARight.fm12;
  result.fm13 := -ARight.fm13;
  result.fm23 := -ARight.fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := ALeft.fm0123;
end;

class operator TBivector.-(const ALeft: TBivector; ARight: TMultivector): TMultivector;
begin
  result.fms := -ARight.fms;
  result.fm0 := -ARight.fm0;
  result.fm1 := -ARight.fm1;
  result.fm2 := -ARight.fm2;
  result.fm3 := -ARight.fm3;
  result.fm01 := ALeft.fm01 - ARight.fm01;
  result.fm02 := ALeft.fm02 - ARight.fm02;
  result.fm03 := ALeft.fm03 - ARight.fm03;
  result.fm12 := ALeft.fm12 - ARight.fm12;
  result.fm13 := ALeft.fm13 - ARight.fm13;
  result.fm23 := ALeft.fm23 - ARight.fm23;
  result.fm012 := -ARight.fm012;
  result.fm013 := -ARight.fm013;
  result.fm023 := -ARight.fm023;
  result.fm123 := -ARight.fm123;
  result.fm0123 := -ARight.fm0123;
end;

class operator TBivector.-(const ALeft: TMultivector; ARight: TBivector): TMultivector;
begin
  result.fms := ALeft.fms;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := ALeft.fm01 - ARight.fm01;
  result.fm02 := ALeft.fm02 - ARight.fm02;
  result.fm03 := ALeft.fm03 - ARight.fm03;
  result.fm12 := ALeft.fm12 - ARight.fm12;
  result.fm13 := ALeft.fm13 - ARight.fm13;
  result.fm23 := ALeft.fm23 - ARight.fm23;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := ALeft.fm0123;
end;

class operator TBivector.*(const ALeft: TBivector; const ARight: double): TBivector;
begin
  result.fm01 := +ALeft.fm01 * ARight;

  result.fm02 := +ALeft.fm02 * ARight;

  result.fm03 := +ALeft.fm03 * ARight;

  result.fm12 := +ALeft.fm12 * ARight;

  result.fm13 := +ALeft.fm13 * ARight;

  result.fm23 := +ALeft.fm23 * ARight;

end;

class operator TBivector.*(const ALeft: double; const ARight: TBivector): TBivector;
begin
  result.fm01 := +ALeft * ARight.fm01;

  result.fm02 := +ALeft * ARight.fm02;

  result.fm03 := +ALeft * ARight.fm03;

  result.fm12 := +ALeft * ARight.fm12;

  result.fm13 := +ALeft * ARight.fm13;

  result.fm23 := +ALeft * ARight.fm23;

end;

class operator TBivector.*(const ALeft, ARight: TBivector): TMultivector;
begin
  result.fms := +ALeft.fm01 * ARight.fm01 + ALeft.fm02 * ARight.fm02 +
    ALeft.fm03 * ARight.fm03 - ALeft.fm12 * ARight.fm12 - ALeft.fm13 *
    ARight.fm13 - ALeft.fm23 * ARight.fm23;

  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := +ALeft.fm02 * ARight.fm12 + ALeft.fm03 * ARight.fm13 -
    ALeft.fm12 * ARight.fm02 - ALeft.fm13 * ARight.fm03;

  result.fm02 := -ALeft.fm01 * ARight.fm12 + ALeft.fm03 * ARight.fm23 +
    ALeft.fm12 * ARight.fm01 - ALeft.fm23 * ARight.fm03;

  result.fm03 := -ALeft.fm01 * ARight.fm13 - ALeft.fm02 * ARight.fm23 +
    ALeft.fm13 * ARight.fm01 + ALeft.fm23 * ARight.fm02;

  result.fm12 := -ALeft.fm01 * ARight.fm02 + ALeft.fm02 * ARight.fm01 +
    ALeft.fm13 * ARight.fm23 - ALeft.fm23 * ARight.fm13;

  result.fm13 := -ALeft.fm01 * ARight.fm03 + ALeft.fm03 * ARight.fm01 -
    ALeft.fm12 * ARight.fm23 + ALeft.fm23 * ARight.fm12;

  result.fm23 := -ALeft.fm02 * ARight.fm03 + ALeft.fm03 * ARight.fm02 +
    ALeft.fm12 * ARight.fm13 - ALeft.fm13 * ARight.fm12;

  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := +ALeft.fm01 * ARight.fm23 - ALeft.fm02 * ARight.fm13 +
    ALeft.fm03 * ARight.fm12 + ALeft.fm12 * ARight.fm03 - ALeft.fm13 *
    ARight.fm02 + ALeft.fm23 * ARight.fm01;

end;

class operator TBivector.*(const ALeft: TBivector;
  const ARight: TTrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := -ALeft.fm12 * ARight.fm012 - ALeft.fm13 * ARight.fm013 -
    ALeft.fm23 * ARight.fm023;

  result.fm1 := -ALeft.fm02 * ARight.fm012 - ALeft.fm03 * ARight.fm013 -
    ALeft.fm23 * ARight.fm123;

  result.fm2 := +ALeft.fm01 * ARight.fm012 - ALeft.fm03 * ARight.fm023 +
    ALeft.fm13 * ARight.fm123;

  result.fm3 := +ALeft.fm01 * ARight.fm013 + ALeft.fm02 * ARight.fm023 -
    ALeft.fm12 * ARight.fm123;

  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := -ALeft.fm03 * ARight.fm123 + ALeft.fm13 * ARight.fm023 -
    ALeft.fm23 * ARight.fm013;

  result.fm013 := +ALeft.fm02 * ARight.fm123 - ALeft.fm12 * ARight.fm023 +
    ALeft.fm23 * ARight.fm012;

  result.fm023 := -ALeft.fm01 * ARight.fm123 + ALeft.fm12 * ARight.fm013 -
    ALeft.fm13 * ARight.fm012;

  result.fm123 := -ALeft.fm01 * ARight.fm023 + ALeft.fm02 * ARight.fm013 -
    ALeft.fm03 * ARight.fm012;

  result.fm0123 := 0;
end;

class operator TBivector.*(const ALeft: TTrivector;
  const ARight: TBivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := -ALeft.fm012 * ARight.fm12 - ALeft.fm013 * ARight.fm13 -
    ALeft.fm023 * ARight.fm23;

  result.fm1 := -ALeft.fm012 * ARight.fm02 - ALeft.fm013 * ARight.fm03 -
    ALeft.fm123 * ARight.fm23;

  result.fm2 := +ALeft.fm012 * ARight.fm01 - ALeft.fm023 * ARight.fm03 +
    ALeft.fm123 * ARight.fm13;

  result.fm3 := +ALeft.fm013 * ARight.fm01 + ALeft.fm023 * ARight.fm02 -
    ALeft.fm123 * ARight.fm12;

  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := +ALeft.fm013 * ARight.fm23 - ALeft.fm023 * ARight.fm13 +
    ALeft.fm123 * ARight.fm03;

  result.fm013 := -ALeft.fm012 * ARight.fm23 + ALeft.fm023 * ARight.fm12 -
    ALeft.fm123 * ARight.fm02;

  result.fm023 := +ALeft.fm012 * ARight.fm13 - ALeft.fm013 * ARight.fm12 +
    ALeft.fm123 * ARight.fm01;

  result.fm123 := +ALeft.fm012 * ARight.fm03 - ALeft.fm013 * ARight.fm02 +
    ALeft.fm023 * ARight.fm01;

  result.fm0123 := 0;
end;

class operator TBivector.*(const ALeft: TBivector;
  const ARight: TQuadrivector): TBivector;
begin
  result.fm01 := -ALeft.fm23 * ARight.fm0123;

  result.fm02 := +ALeft.fm13 * ARight.fm0123;

  result.fm03 := -ALeft.fm12 * ARight.fm0123;

  result.fm12 := +ALeft.fm03 * ARight.fm0123;

  result.fm13 := -ALeft.fm02 * ARight.fm0123;

  result.fm23 := +ALeft.fm01 * ARight.fm0123;

end;

class operator TBivector.*(const ALeft: TQuadrivector;
  const ARight: TBivector): TBivector;
begin
  result.fm01 := -ALeft.fm0123 * ARight.fm23;

  result.fm02 := +ALeft.fm0123 * ARight.fm13;

  result.fm03 := -ALeft.fm0123 * ARight.fm12;

  result.fm12 := +ALeft.fm0123 * ARight.fm03;

  result.fm13 := -ALeft.fm0123 * ARight.fm02;

  result.fm23 := +ALeft.fm0123 * ARight.fm01;

end;

class operator TBivector.*(const ALeft: TBivector;
  const ARight: TMultivector): TMultivector;
begin
  result.fms := +ALeft.fm01 * ARight.fm01 + ALeft.fm02 * ARight.fm02 +
    ALeft.fm03 * ARight.fm03 - ALeft.fm12 * ARight.fm12 - ALeft.fm13 *
    ARight.fm13 - ALeft.fm23 * ARight.fm23;

  result.fm0 := -ALeft.fm01 * ARight.fm1 - ALeft.fm02 * ARight.fm2 -
    ALeft.fm03 * ARight.fm3 - ALeft.fm12 * ARight.fm012 - ALeft.fm13 *
    ARight.fm013 - ALeft.fm23 * ARight.fm023;

  result.fm1 := -ALeft.fm01 * ARight.fm0 - ALeft.fm02 * ARight.fm012 -
    ALeft.fm03 * ARight.fm013 - ALeft.fm12 * ARight.fm2 - ALeft.fm13 *
    ARight.fm3 - ALeft.fm23 * ARight.fm123;

  result.fm2 := +ALeft.fm01 * ARight.fm012 - ALeft.fm02 * ARight.fm0 -
    ALeft.fm03 * ARight.fm023 + ALeft.fm12 * ARight.fm1 + ALeft.fm13 *
    ARight.fm123 - ALeft.fm23 * ARight.fm3;

  result.fm3 := +ALeft.fm01 * ARight.fm013 + ALeft.fm02 * ARight.fm023 -
    ALeft.fm03 * ARight.fm0 - ALeft.fm12 * ARight.fm123 + ALeft.fm13 *
    ARight.fm1 + ALeft.fm23 * ARight.fm2;

  result.fm01 := +ALeft.fm01 * ARight.fms + ALeft.fm02 * ARight.fm12 +
    ALeft.fm03 * ARight.fm13 - ALeft.fm12 * ARight.fm02 - ALeft.fm13 *
    ARight.fm03 - ALeft.fm23 * ARight.fm0123;

  result.fm02 := -ALeft.fm01 * ARight.fm12 + ALeft.fm02 * ARight.fms +
    ALeft.fm03 * ARight.fm23 + ALeft.fm12 * ARight.fm01 + ALeft.fm13 *
    ARight.fm0123 - ALeft.fm23 * ARight.fm03;

  result.fm03 := -ALeft.fm01 * ARight.fm13 - ALeft.fm02 * ARight.fm23 +
    ALeft.fm03 * ARight.fms - ALeft.fm12 * ARight.fm0123 + ALeft.fm13 *
    ARight.fm01 + ALeft.fm23 * ARight.fm02;

  result.fm12 := -ALeft.fm01 * ARight.fm02 + ALeft.fm02 * ARight.fm01 +
    ALeft.fm03 * ARight.fm0123 + ALeft.fm12 * ARight.fms + ALeft.fm13 *
    ARight.fm23 - ALeft.fm23 * ARight.fm13;

  result.fm13 := -ALeft.fm01 * ARight.fm03 - ALeft.fm02 * ARight.fm0123 +
    ALeft.fm03 * ARight.fm01 - ALeft.fm12 * ARight.fm23 + ALeft.fm13 *
    ARight.fms + ALeft.fm23 * ARight.fm12;

  result.fm23 := +ALeft.fm01 * ARight.fm0123 - ALeft.fm02 * ARight.fm03 +
    ALeft.fm03 * ARight.fm02 + ALeft.fm12 * ARight.fm13 - ALeft.fm13 *
    ARight.fm12 + ALeft.fm23 * ARight.fms;

  result.fm012 := +ALeft.fm01 * ARight.fm2 - ALeft.fm02 * ARight.fm1 -
    ALeft.fm03 * ARight.fm123 + ALeft.fm12 * ARight.fm0 + ALeft.fm13 *
    ARight.fm023 - ALeft.fm23 * ARight.fm013;

  result.fm013 := +ALeft.fm01 * ARight.fm3 + ALeft.fm02 * ARight.fm123 -
    ALeft.fm03 * ARight.fm1 - ALeft.fm12 * ARight.fm023 + ALeft.fm13 *
    ARight.fm0 + ALeft.fm23 * ARight.fm012;

  result.fm023 := -ALeft.fm01 * ARight.fm123 + ALeft.fm02 * ARight.fm3 -
    ALeft.fm03 * ARight.fm2 + ALeft.fm12 * ARight.fm013 - ALeft.fm13 *
    ARight.fm012 + ALeft.fm23 * ARight.fm0;

  result.fm123 := -ALeft.fm01 * ARight.fm023 + ALeft.fm02 * ARight.fm013 -
    ALeft.fm03 * ARight.fm012 + ALeft.fm12 * ARight.fm3 - ALeft.fm13 *
    ARight.fm2 + ALeft.fm23 * ARight.fm1;

  result.fm0123 := +ALeft.fm01 * ARight.fm23 - ALeft.fm02 * ARight.fm13 +
    ALeft.fm03 * ARight.fm12 + ALeft.fm12 * ARight.fm03 - ALeft.fm13 *
    ARight.fm02 + ALeft.fm23 * ARight.fm01;

end;

class operator TBivector.*(const ALeft: TMultivector;
  const ARight: TBivector): TMultivector;
begin
  result.fms := +ALeft.fm01 * ARight.fm01 + ALeft.fm02 * ARight.fm02 +
    ALeft.fm03 * ARight.fm03 - ALeft.fm12 * ARight.fm12 - ALeft.fm13 *
    ARight.fm13 - ALeft.fm23 * ARight.fm23;

  result.fm0 := +ALeft.fm1 * ARight.fm01 + ALeft.fm2 * ARight.fm02 +
    ALeft.fm3 * ARight.fm03 - ALeft.fm012 * ARight.fm12 - ALeft.fm013 *
    ARight.fm13 - ALeft.fm023 * ARight.fm23;

  result.fm1 := +ALeft.fm0 * ARight.fm01 + ALeft.fm2 * ARight.fm12 +
    ALeft.fm3 * ARight.fm13 - ALeft.fm012 * ARight.fm02 - ALeft.fm013 *
    ARight.fm03 - ALeft.fm123 * ARight.fm23;

  result.fm2 := +ALeft.fm0 * ARight.fm02 - ALeft.fm1 * ARight.fm12 +
    ALeft.fm3 * ARight.fm23 + ALeft.fm012 * ARight.fm01 - ALeft.fm023 *
    ARight.fm03 + ALeft.fm123 * ARight.fm13;

  result.fm3 := +ALeft.fm0 * ARight.fm03 - ALeft.fm1 * ARight.fm13 -
    ALeft.fm2 * ARight.fm23 + ALeft.fm013 * ARight.fm01 + ALeft.fm023 *
    ARight.fm02 - ALeft.fm123 * ARight.fm12;

  result.fm01 := +ALeft.fms * ARight.fm01 + ALeft.fm02 * ARight.fm12 +
    ALeft.fm03 * ARight.fm13 - ALeft.fm12 * ARight.fm02 - ALeft.fm13 *
    ARight.fm03 - ALeft.fm0123 * ARight.fm23;

  result.fm02 := +ALeft.fms * ARight.fm02 - ALeft.fm01 * ARight.fm12 +
    ALeft.fm03 * ARight.fm23 + ALeft.fm12 * ARight.fm01 - ALeft.fm23 *
    ARight.fm03 + ALeft.fm0123 * ARight.fm13;

  result.fm03 := +ALeft.fms * ARight.fm03 - ALeft.fm01 * ARight.fm13 -
    ALeft.fm02 * ARight.fm23 + ALeft.fm13 * ARight.fm01 + ALeft.fm23 *
    ARight.fm02 - ALeft.fm0123 * ARight.fm12;

  result.fm12 := +ALeft.fms * ARight.fm12 - ALeft.fm01 * ARight.fm02 +
    ALeft.fm02 * ARight.fm01 + ALeft.fm13 * ARight.fm23 - ALeft.fm23 *
    ARight.fm13 + ALeft.fm0123 * ARight.fm03;

  result.fm13 := +ALeft.fms * ARight.fm13 - ALeft.fm01 * ARight.fm03 +
    ALeft.fm03 * ARight.fm01 - ALeft.fm12 * ARight.fm23 + ALeft.fm23 *
    ARight.fm12 - ALeft.fm0123 * ARight.fm02;

  result.fm23 := +ALeft.fms * ARight.fm23 - ALeft.fm02 * ARight.fm03 +
    ALeft.fm03 * ARight.fm02 + ALeft.fm12 * ARight.fm13 - ALeft.fm13 *
    ARight.fm12 + ALeft.fm0123 * ARight.fm01;

  result.fm012 := +ALeft.fm0 * ARight.fm12 - ALeft.fm1 * ARight.fm02 +
    ALeft.fm2 * ARight.fm01 + ALeft.fm013 * ARight.fm23 - ALeft.fm023 *
    ARight.fm13 + ALeft.fm123 * ARight.fm03;

  result.fm013 := +ALeft.fm0 * ARight.fm13 - ALeft.fm1 * ARight.fm03 +
    ALeft.fm3 * ARight.fm01 - ALeft.fm012 * ARight.fm23 + ALeft.fm023 *
    ARight.fm12 - ALeft.fm123 * ARight.fm02;

  result.fm023 := +ALeft.fm0 * ARight.fm23 - ALeft.fm2 * ARight.fm03 +
    ALeft.fm3 * ARight.fm02 + ALeft.fm012 * ARight.fm13 - ALeft.fm013 *
    ARight.fm12 + ALeft.fm123 * ARight.fm01;

  result.fm123 := +ALeft.fm1 * ARight.fm23 - ALeft.fm2 * ARight.fm13 +
    ALeft.fm3 * ARight.fm12 + ALeft.fm012 * ARight.fm03 - ALeft.fm013 *
    ARight.fm02 + ALeft.fm023 * ARight.fm01;

  result.fm0123 := +ALeft.fm01 * ARight.fm23 - ALeft.fm02 * ARight.fm13 +
    ALeft.fm03 * ARight.fm12 + ALeft.fm12 * ARight.fm03 - ALeft.fm13 *
    ARight.fm02 + ALeft.fm23 * ARight.fm01;

end;

class operator TBivector./(const ALeft: TBivector; const ARight: double): TBivector;
begin
  result.fm01 := ALeft.fm01 / ARight;
  result.fm02 := ALeft.fm02 / ARight;
  result.fm03 := ALeft.fm03 / ARight;
  result.fm12 := ALeft.fm12 / ARight;
  result.fm13 := ALeft.fm13 / ARight;
  result.fm23 := ALeft.fm23 / ARight;
end;

class operator TBivector./(const ALeft: double; const ARight: TBivector): TBivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft, ARight: TBivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TBivector;
  const ARight: TTrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TTrivector;
  const ARight: TBivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TBivector;
  const ARight: TQuadrivector): TBivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TQuadrivector;
  const ARight: TBivector): TBivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TBivector;
  const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TMultivector;
  const ARight: TBivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector.=(const ALeft, ARight: TVector): boolean;
begin
  result :=
    (ALeft.fm0 = ARight.fm0) and (ALeft.fm1 = ARight.fm1) and
    (ALeft.fm2 = ARight.fm2) and (ALeft.fm3 = ARight.fm3);
end;

class operator TVector.=(const ALeft: TVector; ARight: TMultivector): boolean;
begin
  result :=
    (0 = ARight.fms) and (ALeft.fm0 = ARight.fm0) and (ALeft.fm1 = ARight.fm1) and
    (ALeft.fm2 = ARight.fm2) and (ALeft.fm3 = ARight.fm3) and
    (0 = ARight.fm01) and (0 = ARight.fm02) and (0 = ARight.fm03) and
    (0 = ARight.fm12) and (0 = ARight.fm13) and (0 = ARight.fm23) and
    (0 = ARight.fm012) and (0 = ARight.fm013) and (0 = ARight.fm023) and
    (0 = ARight.fm123) and (0 = ARight.fm0123);
end;

class operator TVector.=(const ALeft: TMultivector; ARight: TVector): boolean;
begin
  result :=
    (ALeft.fms = 0) and (ALeft.fm0 = ARight.fm0) and (ALeft.fm1 = ARight.fm1) and
    (ALeft.fm2 = ARight.fm2) and (ALeft.fm3 = ARight.fm3) and
    (ALeft.fm01 = 0) and (ALeft.fm02 = 0) and (ALeft.fm03 = 0) and
    (ALeft.fm12 = 0) and (ALeft.fm13 = 0) and (ALeft.fm23 = 0) and
    (ALeft.fm012 = 0) and (ALeft.fm013 = 0) and (ALeft.fm023 = 0) and
    (ALeft.fm123 = 0) and (ALeft.fm0123 = 0);
end;

class operator TVector.<>(const ALeft, ARight: TVector): boolean;
begin
  result :=
    (ALeft.fm0 <> ARight.fm0) and (ALeft.fm1 <> ARight.fm1) and
    (ALeft.fm2 <> ARight.fm2) and (ALeft.fm3 <> ARight.fm3);
end;

class operator TVector.<>(const ALeft: TVector; ARight: TMultivector): boolean;
begin
  result :=
    (0 <> ARight.fms) and (ALeft.fm0 <> ARight.fm0) and
    (ALeft.fm1 <> ARight.fm1) and (ALeft.fm2 <> ARight.fm2) and
    (ALeft.fm3 <> ARight.fm3) and (0 <> ARight.fm01) and (0 <> ARight.fm02) and
    (0 <> ARight.fm03) and (0 <> ARight.fm12) and (0 <> ARight.fm13) and
    (0 <> ARight.fm23) and (0 <> ARight.fm012) and (0 <> ARight.fm013) and
    (0 <> ARight.fm023) and (0 <> ARight.fm123) and (0 <> ARight.fm0123);
end;

class operator TVector.<>(const ALeft: TMultivector; ARight: TVector): boolean;
begin
  result :=
    (ALeft.fms <> 0) and (ALeft.fm0 <> ARight.fm0) and
    (ALeft.fm1 <> ARight.fm1) and (ALeft.fm2 <> ARight.fm2) and
    (ALeft.fm3 <> ARight.fm3) and (ALeft.fm01 <> 0) and (ALeft.fm02 <> 0) and
    (ALeft.fm03 <> 0) and (ALeft.fm12 <> 0) and (ALeft.fm13 <> 0) and
    (ALeft.fm23 <> 0) and (ALeft.fm012 <> 0) and (ALeft.fm013 <> 0) and
    (ALeft.fm023 <> 0) and (ALeft.fm123 <> 0) and (ALeft.fm0123 <> 0);
end;

class operator TVector.+(const AValue: TVector): TVector;
begin
  result.fm0 := AValue.fm0;
  result.fm1 := AValue.fm1;
  result.fm2 := AValue.fm2;
  result.fm3 := AValue.fm3;
end;

class operator TVector.+(const ALeft: TVector; ARight: double): TMultivector;
begin
  result.fms := ARight;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TVector.+(const ALeft: double; ARight: TVector): TMultivector;
begin
  result.fms := ALeft;
  result.fm0 := ARight.fm0;
  result.fm1 := ARight.fm1;
  result.fm2 := ARight.fm2;
  result.fm3 := ARight.fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TVector.+(const ALeft, ARight: TVector): TVector;
begin
  result.fm0 := ALeft.fm0 + ARight.fm0;
  result.fm1 := ALeft.fm1 + ARight.fm1;
  result.fm2 := ALeft.fm2 + ARight.fm2;
  result.fm3 := ALeft.fm3 + ARight.fm3;
end;

class operator TVector.+(const ALeft: TVector; ARight: TBivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := ARight.fm01;
  result.fm02 := ARight.fm02;
  result.fm03 := ARight.fm03;
  result.fm12 := ARight.fm12;
  result.fm13 := ARight.fm13;
  result.fm23 := ARight.fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TVector.+(const ALeft: TBivector; ARight: TVector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := ARight.fm0;
  result.fm1 := ARight.fm1;
  result.fm2 := ARight.fm2;
  result.fm3 := ARight.fm3;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TVector.+(const ALeft: TVector; ARight: TTrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := ARight.fm012;
  result.fm013 := ARight.fm013;
  result.fm023 := ARight.fm023;
  result.fm123 := ARight.fm123;
  result.fm0123 := 0;
end;

class operator TVector.+(const ALeft: TTrivector; ARight: TVector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := ARight.fm0;
  result.fm1 := ARight.fm1;
  result.fm2 := ARight.fm2;
  result.fm3 := ARight.fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := 0;
end;

class operator TVector.+(const ALeft: TVector; ARight: TQuadrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := ARight.fm0123;
end;

class operator TVector.+(const ALeft: TQuadrivector; ARight: TVector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := ARight.fm0;
  result.fm1 := ARight.fm1;
  result.fm2 := ARight.fm2;
  result.fm3 := ARight.fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := ALeft.fm0123;
end;

class operator TVector.+(const ALeft: TVector; ARight: TMultivector): TMultivector;
begin
  result.fms := ARight.fms;
  result.fm0 := ALeft.fm0 + ARight.fm0;
  result.fm1 := ALeft.fm1 + ARight.fm1;
  result.fm2 := ALeft.fm2 + ARight.fm2;
  result.fm3 := ALeft.fm3 + ARight.fm3;
  result.fm01 := ARight.fm01;
  result.fm02 := ARight.fm02;
  result.fm03 := ARight.fm03;
  result.fm12 := ARight.fm12;
  result.fm13 := ARight.fm13;
  result.fm23 := ARight.fm23;
  result.fm012 := ARight.fm012;
  result.fm013 := ARight.fm013;
  result.fm023 := ARight.fm023;
  result.fm123 := ARight.fm123;
  result.fm0123 := ARight.fm0123;
end;

class operator TVector.+(const ALeft: TMultivector; ARight: TVector): TMultivector;
begin
  result.fms := ALeft.fms;
  result.fm0 := ALeft.fm0 + ARight.fm0;
  result.fm1 := ALeft.fm1 + ARight.fm1;
  result.fm2 := ALeft.fm2 + ARight.fm2;
  result.fm3 := ALeft.fm3 + ARight.fm3;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := ALeft.fm0123;
end;

class operator TVector.-(const AValue: TVector): TVector;
begin
  result.fm0 := -AValue.fm0;
  result.fm1 := -AValue.fm1;
  result.fm2 := -AValue.fm2;
  result.fm3 := -AValue.fm3;
end;

class operator TVector.-(const ALeft: TVector; ARight: double): TMultivector;
begin
  result.fms := -ARight;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TVector.-(const ALeft: double; ARight: TVector): TMultivector;
begin
  result.fms := ALeft;
  result.fm0 := -ARight.fm0;
  result.fm1 := -ARight.fm1;
  result.fm2 := -ARight.fm2;
  result.fm3 := -ARight.fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TVector.-(const ALeft, ARight: TVector): TVector;
begin
  result.fm0 := ALeft.fm0 - ARight.fm0;
  result.fm1 := ALeft.fm1 - ARight.fm1;
  result.fm2 := ALeft.fm2 - ARight.fm2;
  result.fm3 := ALeft.fm3 - ARight.fm3;
end;

class operator TVector.-(const ALeft: TVector; ARight: TBivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := -ARight.fm01;
  result.fm02 := -ARight.fm02;
  result.fm03 := -ARight.fm03;
  result.fm12 := -ARight.fm12;
  result.fm13 := -ARight.fm13;
  result.fm23 := -ARight.fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TVector.-(const ALeft: TBivector; ARight: TVector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := -ARight.fm0;
  result.fm1 := -ARight.fm1;
  result.fm2 := -ARight.fm2;
  result.fm3 := -ARight.fm3;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TVector.-(const ALeft: TVector; ARight: TTrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := -ARight.fm012;
  result.fm013 := -ARight.fm013;
  result.fm023 := -ARight.fm023;
  result.fm123 := -ARight.fm123;
  result.fm0123 := 0;
end;

class operator TVector.-(const ALeft: TTrivector; ARight: TVector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := -ARight.fm0;
  result.fm1 := -ARight.fm1;
  result.fm2 := -ARight.fm2;
  result.fm3 := -ARight.fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := 0;
end;

class operator TVector.-(const ALeft: TVector; ARight: TQuadrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := ALeft.fm0;
  result.fm1 := ALeft.fm1;
  result.fm2 := ALeft.fm2;
  result.fm3 := ALeft.fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := -ARight.fm0123;
end;

class operator TVector.-(const ALeft: TQuadrivector; ARight: TVector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := -ARight.fm0;
  result.fm1 := -ARight.fm1;
  result.fm2 := -ARight.fm2;
  result.fm3 := -ARight.fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := ALeft.fm0123;
end;

class operator TVector.-(const ALeft: TVector; ARight: TMultivector): TMultivector;
begin
  result.fms := -ARight.fms;
  result.fm0 := ALeft.fm0 - ARight.fm0;
  result.fm1 := ALeft.fm1 - ARight.fm1;
  result.fm2 := ALeft.fm2 - ARight.fm2;
  result.fm3 := ALeft.fm3 - ARight.fm3;
  result.fm01 := -ARight.fm01;
  result.fm02 := -ARight.fm02;
  result.fm03 := -ARight.fm03;
  result.fm12 := -ARight.fm12;
  result.fm13 := -ARight.fm13;
  result.fm23 := -ARight.fm23;
  result.fm012 := -ARight.fm012;
  result.fm013 := -ARight.fm013;
  result.fm023 := -ARight.fm023;
  result.fm123 := -ARight.fm123;
  result.fm0123 := -ARight.fm0123;
end;

class operator TVector.-(const ALeft: TMultivector; ARight: TVector): TMultivector;
begin
  result.fms := ALeft.fms;
  result.fm0 := ALeft.fm0 - ARight.fm0;
  result.fm1 := ALeft.fm1 - ARight.fm1;
  result.fm2 := ALeft.fm2 - ARight.fm2;
  result.fm3 := ALeft.fm3 - ARight.fm3;
  result.fm01 := ALeft.fm01;
  result.fm02 := ALeft.fm02;
  result.fm03 := ALeft.fm03;
  result.fm12 := ALeft.fm12;
  result.fm13 := ALeft.fm13;
  result.fm23 := ALeft.fm23;
  result.fm012 := ALeft.fm012;
  result.fm013 := ALeft.fm013;
  result.fm023 := ALeft.fm023;
  result.fm123 := ALeft.fm123;
  result.fm0123 := ALeft.fm0123;
end;

class operator TVector.*(const ALeft: TVector; const ARight: double): TVector;
begin
  result.fm0 := +ALeft.fm0 * ARight;

  result.fm1 := +ALeft.fm1 * ARight;

  result.fm2 := +ALeft.fm2 * ARight;

  result.fm3 := +ALeft.fm3 * ARight;

end;

class operator TVector.*(const ALeft: double; const ARight: TVector): TVector;
begin
  result.fm0 := +ALeft * ARight.fm0;

  result.fm1 := +ALeft * ARight.fm1;

  result.fm2 := +ALeft * ARight.fm2;

  result.fm3 := +ALeft * ARight.fm3;

end;

class operator TVector.*(const ALeft, ARight: TVector): TMultivector;
begin
  result.fms := +ALeft.fm0 * ARight.fm0 - ALeft.fm1 * ARight.fm1 -
    ALeft.fm2 * ARight.fm2 - ALeft.fm3 * ARight.fm3;

  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := +ALeft.fm0 * ARight.fm1 - ALeft.fm1 * ARight.fm0;

  result.fm02 := +ALeft.fm0 * ARight.fm2 - ALeft.fm2 * ARight.fm0;

  result.fm03 := +ALeft.fm0 * ARight.fm3 - ALeft.fm3 * ARight.fm0;

  result.fm12 := +ALeft.fm1 * ARight.fm2 - ALeft.fm2 * ARight.fm1;

  result.fm13 := +ALeft.fm1 * ARight.fm3 - ALeft.fm3 * ARight.fm1;

  result.fm23 := +ALeft.fm2 * ARight.fm3 - ALeft.fm3 * ARight.fm2;

  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TVector.*(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := +ALeft.fm1 * ARight.fm01 + ALeft.fm2 * ARight.fm02 +
    ALeft.fm3 * ARight.fm03;

  result.fm1 := +ALeft.fm0 * ARight.fm01 + ALeft.fm2 * ARight.fm12 +
    ALeft.fm3 * ARight.fm13;

  result.fm2 := +ALeft.fm0 * ARight.fm02 - ALeft.fm1 * ARight.fm12 +
    ALeft.fm3 * ARight.fm23;

  result.fm3 := +ALeft.fm0 * ARight.fm03 - ALeft.fm1 * ARight.fm13 -
    ALeft.fm2 * ARight.fm23;

  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := +ALeft.fm0 * ARight.fm12 - ALeft.fm1 * ARight.fm02 +
    ALeft.fm2 * ARight.fm01;

  result.fm013 := +ALeft.fm0 * ARight.fm13 - ALeft.fm1 * ARight.fm03 +
    ALeft.fm3 * ARight.fm01;

  result.fm023 := +ALeft.fm0 * ARight.fm23 - ALeft.fm2 * ARight.fm03 +
    ALeft.fm3 * ARight.fm02;

  result.fm123 := +ALeft.fm1 * ARight.fm23 - ALeft.fm2 * ARight.fm13 +
    ALeft.fm3 * ARight.fm12;

  result.fm0123 := 0;
end;

class operator TVector.*(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := -ALeft.fm01 * ARight.fm1 - ALeft.fm02 * ARight.fm2 -
    ALeft.fm03 * ARight.fm3;

  result.fm1 := -ALeft.fm01 * ARight.fm0 - ALeft.fm12 * ARight.fm2 -
    ALeft.fm13 * ARight.fm3;

  result.fm2 := -ALeft.fm02 * ARight.fm0 + ALeft.fm12 * ARight.fm1 -
    ALeft.fm23 * ARight.fm3;

  result.fm3 := -ALeft.fm03 * ARight.fm0 + ALeft.fm13 * ARight.fm1 +
    ALeft.fm23 * ARight.fm2;

  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := +ALeft.fm01 * ARight.fm2 - ALeft.fm02 * ARight.fm1 +
    ALeft.fm12 * ARight.fm0;

  result.fm013 := +ALeft.fm01 * ARight.fm3 - ALeft.fm03 * ARight.fm1 +
    ALeft.fm13 * ARight.fm0;

  result.fm023 := +ALeft.fm02 * ARight.fm3 - ALeft.fm03 * ARight.fm2 +
    ALeft.fm23 * ARight.fm0;

  result.fm123 := +ALeft.fm12 * ARight.fm3 - ALeft.fm13 * ARight.fm2 +
    ALeft.fm23 * ARight.fm1;

  result.fm0123 := 0;
end;

class operator TVector.*(const ALeft: TVector; const ARight: TTrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := -ALeft.fm2 * ARight.fm012 - ALeft.fm3 * ARight.fm013;

  result.fm02 := +ALeft.fm1 * ARight.fm012 - ALeft.fm3 * ARight.fm023;

  result.fm03 := +ALeft.fm1 * ARight.fm013 + ALeft.fm2 * ARight.fm023;

  result.fm12 := +ALeft.fm0 * ARight.fm012 - ALeft.fm3 * ARight.fm123;

  result.fm13 := +ALeft.fm0 * ARight.fm013 + ALeft.fm2 * ARight.fm123;

  result.fm23 := +ALeft.fm0 * ARight.fm023 - ALeft.fm1 * ARight.fm123;

  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := +ALeft.fm0 * ARight.fm123 - ALeft.fm1 * ARight.fm023 +
    ALeft.fm2 * ARight.fm013 - ALeft.fm3 * ARight.fm012;

end;

class operator TVector.*(const ALeft: TTrivector; const ARight: TVector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := -ALeft.fm012 * ARight.fm2 - ALeft.fm013 * ARight.fm3;

  result.fm02 := +ALeft.fm012 * ARight.fm1 - ALeft.fm023 * ARight.fm3;

  result.fm03 := +ALeft.fm013 * ARight.fm1 + ALeft.fm023 * ARight.fm2;

  result.fm12 := +ALeft.fm012 * ARight.fm0 - ALeft.fm123 * ARight.fm3;

  result.fm13 := +ALeft.fm013 * ARight.fm0 + ALeft.fm123 * ARight.fm2;

  result.fm23 := +ALeft.fm023 * ARight.fm0 - ALeft.fm123 * ARight.fm1;

  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := +ALeft.fm012 * ARight.fm3 - ALeft.fm013 * ARight.fm2 +
    ALeft.fm023 * ARight.fm1 - ALeft.fm123 * ARight.fm0;

end;

class operator TVector.*(const ALeft: TVector; const ARight: TQuadrivector): TTrivector;
begin
  result.fm012 := +ALeft.fm3 * ARight.fm0123;

  result.fm013 := -ALeft.fm2 * ARight.fm0123;

  result.fm023 := +ALeft.fm1 * ARight.fm0123;

  result.fm123 := +ALeft.fm0 * ARight.fm0123;

end;

class operator TVector.*(const ALeft: TQuadrivector; const ARight: TVector): TTrivector;
begin
  result.fm012 := -ALeft.fm0123 * ARight.fm3;

  result.fm013 := +ALeft.fm0123 * ARight.fm2;

  result.fm023 := -ALeft.fm0123 * ARight.fm1;

  result.fm123 := -ALeft.fm0123 * ARight.fm0;

end;

class operator TVector.*(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result.fms := +ALeft.fm0 * ARight.fm0 - ALeft.fm1 * ARight.fm1 -
    ALeft.fm2 * ARight.fm2 - ALeft.fm3 * ARight.fm3;

  result.fm0 := +ALeft.fm0 * ARight.fms + ALeft.fm1 * ARight.fm01 +
    ALeft.fm2 * ARight.fm02 + ALeft.fm3 * ARight.fm03;

  result.fm1 := +ALeft.fm0 * ARight.fm01 + ALeft.fm1 * ARight.fms +
    ALeft.fm2 * ARight.fm12 + ALeft.fm3 * ARight.fm13;

  result.fm2 := +ALeft.fm0 * ARight.fm02 - ALeft.fm1 * ARight.fm12 +
    ALeft.fm2 * ARight.fms + ALeft.fm3 * ARight.fm23;

  result.fm3 := +ALeft.fm0 * ARight.fm03 - ALeft.fm1 * ARight.fm13 -
    ALeft.fm2 * ARight.fm23 + ALeft.fm3 * ARight.fms;

  result.fm01 := +ALeft.fm0 * ARight.fm1 - ALeft.fm1 * ARight.fm0 -
    ALeft.fm2 * ARight.fm012 - ALeft.fm3 * ARight.fm013;

  result.fm02 := +ALeft.fm0 * ARight.fm2 + ALeft.fm1 * ARight.fm012 -
    ALeft.fm2 * ARight.fm0 - ALeft.fm3 * ARight.fm023;

  result.fm03 := +ALeft.fm0 * ARight.fm3 + ALeft.fm1 * ARight.fm013 +
    ALeft.fm2 * ARight.fm023 - ALeft.fm3 * ARight.fm0;

  result.fm12 := +ALeft.fm0 * ARight.fm012 + ALeft.fm1 * ARight.fm2 -
    ALeft.fm2 * ARight.fm1 - ALeft.fm3 * ARight.fm123;

  result.fm13 := +ALeft.fm0 * ARight.fm013 + ALeft.fm1 * ARight.fm3 +
    ALeft.fm2 * ARight.fm123 - ALeft.fm3 * ARight.fm1;

  result.fm23 := +ALeft.fm0 * ARight.fm023 - ALeft.fm1 * ARight.fm123 +
    ALeft.fm2 * ARight.fm3 - ALeft.fm3 * ARight.fm2;

  result.fm012 := +ALeft.fm0 * ARight.fm12 - ALeft.fm1 * ARight.fm02 +
    ALeft.fm2 * ARight.fm01 + ALeft.fm3 * ARight.fm0123;

  result.fm013 := +ALeft.fm0 * ARight.fm13 - ALeft.fm1 * ARight.fm03 -
    ALeft.fm2 * ARight.fm0123 + ALeft.fm3 * ARight.fm01;

  result.fm023 := +ALeft.fm0 * ARight.fm23 + ALeft.fm1 * ARight.fm0123 -
    ALeft.fm2 * ARight.fm03 + ALeft.fm3 * ARight.fm02;

  result.fm123 := +ALeft.fm0 * ARight.fm0123 + ALeft.fm1 * ARight.fm23 -
    ALeft.fm2 * ARight.fm13 + ALeft.fm3 * ARight.fm12;

  result.fm0123 := +ALeft.fm0 * ARight.fm123 - ALeft.fm1 * ARight.fm023 +
    ALeft.fm2 * ARight.fm013 - ALeft.fm3 * ARight.fm012;

end;

class operator TVector.*(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result.fms := +ALeft.fm0 * ARight.fm0 - ALeft.fm1 * ARight.fm1 -
    ALeft.fm2 * ARight.fm2 - ALeft.fm3 * ARight.fm3;

  result.fm0 := +ALeft.fms * ARight.fm0 - ALeft.fm01 * ARight.fm1 -
    ALeft.fm02 * ARight.fm2 - ALeft.fm03 * ARight.fm3;

  result.fm1 := +ALeft.fms * ARight.fm1 - ALeft.fm01 * ARight.fm0 -
    ALeft.fm12 * ARight.fm2 - ALeft.fm13 * ARight.fm3;

  result.fm2 := +ALeft.fms * ARight.fm2 - ALeft.fm02 * ARight.fm0 +
    ALeft.fm12 * ARight.fm1 - ALeft.fm23 * ARight.fm3;

  result.fm3 := +ALeft.fms * ARight.fm3 - ALeft.fm03 * ARight.fm0 +
    ALeft.fm13 * ARight.fm1 + ALeft.fm23 * ARight.fm2;

  result.fm01 := +ALeft.fm0 * ARight.fm1 - ALeft.fm1 * ARight.fm0 -
    ALeft.fm012 * ARight.fm2 - ALeft.fm013 * ARight.fm3;

  result.fm02 := +ALeft.fm0 * ARight.fm2 - ALeft.fm2 * ARight.fm0 +
    ALeft.fm012 * ARight.fm1 - ALeft.fm023 * ARight.fm3;

  result.fm03 := +ALeft.fm0 * ARight.fm3 - ALeft.fm3 * ARight.fm0 +
    ALeft.fm013 * ARight.fm1 + ALeft.fm023 * ARight.fm2;

  result.fm12 := +ALeft.fm1 * ARight.fm2 - ALeft.fm2 * ARight.fm1 +
    ALeft.fm012 * ARight.fm0 - ALeft.fm123 * ARight.fm3;

  result.fm13 := +ALeft.fm1 * ARight.fm3 - ALeft.fm3 * ARight.fm1 +
    ALeft.fm013 * ARight.fm0 + ALeft.fm123 * ARight.fm2;

  result.fm23 := +ALeft.fm2 * ARight.fm3 - ALeft.fm3 * ARight.fm2 +
    ALeft.fm023 * ARight.fm0 - ALeft.fm123 * ARight.fm1;

  result.fm012 := +ALeft.fm01 * ARight.fm2 - ALeft.fm02 * ARight.fm1 +
    ALeft.fm12 * ARight.fm0 - ALeft.fm0123 * ARight.fm3;

  result.fm013 := +ALeft.fm01 * ARight.fm3 - ALeft.fm03 * ARight.fm1 +
    ALeft.fm13 * ARight.fm0 + ALeft.fm0123 * ARight.fm2;

  result.fm023 := +ALeft.fm02 * ARight.fm3 - ALeft.fm03 * ARight.fm2 +
    ALeft.fm23 * ARight.fm0 - ALeft.fm0123 * ARight.fm1;

  result.fm123 := +ALeft.fm12 * ARight.fm3 - ALeft.fm13 * ARight.fm2 +
    ALeft.fm23 * ARight.fm1 - ALeft.fm0123 * ARight.fm0;

  result.fm0123 := +ALeft.fm012 * ARight.fm3 - ALeft.fm013 * ARight.fm2 +
    ALeft.fm023 * ARight.fm1 - ALeft.fm123 * ARight.fm0;

end;

class operator TVector./(const ALeft: TVector; const ARight: double): TVector;
begin
  result.fm0 := ALeft.fm0 / ARight;
  result.fm1 := ALeft.fm1 / ARight;
  result.fm2 := ALeft.fm2 / ARight;
  result.fm3 := ALeft.fm3 / ARight;
end;

class operator TVector./(const ALeft: double; const ARight: TVector): TVector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft, ARight: TVector): TMultivector;
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

class operator TVector./(const ALeft: TVector; const ARight: TTrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TTrivector; const ARight: TVector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TVector; const ARight: TQuadrivector): TTrivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TQuadrivector; const ARight: TVector): TTrivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

function TMultivectorHelper.Dual: TMultivector;
begin
  result.fms := -fm0123;
  result.fm0 := -fm123;
  result.fm1 := -fm023;
  result.fm2 :=  fm013;
  result.fm3 := -fm012;
  result.fm01 := -fm23;
  result.fm02 :=  fm13;
  result.fm03 := -fm12;
  result.fm12 :=  fm03;
  result.fm13 := -fm02;
  result.fm23 :=  fm01;
  result.fm012 :=  fm3;
  result.fm013 := -fm2;
  result.fm023 :=  fm1;
  result.fm123 :=  fm0;
  result.fm0123 :=  fms;
end;

function TMultivectorHelper.Inverse: TMultivector;
begin
  result.fms := fms;
  result.fm0 := -fm0;
  result.fm1 := -fm1;
  result.fm2 := -fm2;
  result.fm3 := -fm3;
  result.fm01 := fm01;
  result.fm02 := fm02;
  result.fm03 := fm03;
  result.fm12 := fm12;
  result.fm13 := fm13;
  result.fm23 := fm23;
  result.fm012 := -fm012;
  result.fm013 := -fm013;
  result.fm023 := -fm023;
  result.fm123 := -fm123;
  result.fm0123 := fm0123;
end;

function TMultivectorHelper.Reverse: TMultivector;
begin
  result.fms := fms;
  result.fm0 := fm0;
  result.fm1 := fm1;
  result.fm2 := fm2;
  result.fm3 := fm3;
  result.fm01 := -fm01;
  result.fm02 := -fm02;
  result.fm03 := -fm03;
  result.fm12 := -fm12;
  result.fm13 := -fm13;
  result.fm23 := -fm23;
  result.fm012 := -fm012;
  result.fm013 := -fm013;
  result.fm023 := -fm023;
  result.fm123 := -fm123;
  result.fm0123 := fm0123;
end;

function TMultivectorHelper.Conjugate: TMultivector;
begin
  result.fms := fms;
  result.fm0 := -fm0;
  result.fm1 := -fm1;
  result.fm2 := -fm2;
  result.fm3 := -fm3;
  result.fm01 := -fm01;
  result.fm02 := -fm02;
  result.fm03 := -fm03;
  result.fm12 := -fm12;
  result.fm13 := -fm13;
  result.fm23 := -fm23;
  result.fm012 := fm012;
  result.fm013 := fm013;
  result.fm023 := fm023;
  result.fm123 := fm123;
  result.fm0123 := fm0123;
end;

function TMultivectorHelper.Reciprocal: TMultivector;
var
  map34: TMultivector;
begin
  map34 := Self * Conjugate;
  map34.fm012  := -map34.fm012;
  map34.fm013  := -map34.fm013;
  map34.fm023  := -map34.fm023;
  map34.fm123  := -map34.fm123;
  map34.fm0123 := -map34.fm0123;

  map34  := Conjugate * map34;
  result := map34 / (Self * map34).fms;
end;

function TMultivectorHelper.LeftReciprocal: TMultivector;
var
  map34: TMultivector;
begin
  map34 := Conjugate * Self;
  map34.fm012  := -map34.fm012;
  map34.fm013  := -map34.fm013;
  map34.fm023  := -map34.fm023;
  map34.fm123  := -map34.fm123;
  map34.fm0123 := -map34.fm0123;

  map34  := map34 * Conjugate;
  result := map34 / (map34 * Self).fms;
end;

function TMultivectorHelper.Normalized: TMultivector;
begin
  result := Self / Norm;
end;

function TMultivectorHelper.Norm: double;
begin
  result := SquaredNorm;
  if result > 0 then
    result := sqrt(result)
  else
    result := -sqrt(-result);
end;

function TMultivectorHelper.SquaredNorm: double;
begin
  result :=  fms    * fms   + fm0   * fm0   - fm1   * fm1   - fm2   * fm2   - fm3  * fm3
            +fm01   * fm01  + fm02  * fm02  + fm03  * fm03  - fm12  * fm12  - fm13 * fm13 - fm23 * fm23
            -fm012  * fm012 - fm013 * fm013 - fm023 * fm023 + fm123 * fm123
            -fm0123 * fm0123;
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

function TMultivectorHelper.Projection(const AVector: TQuadrivector): TMultivector;
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

function TMultivectorHelper.Rejection(const AVector: TTrivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TMultivectorHelper.Rejection(const AVector: TQuadrivector): double;
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

function TMultivectorHelper.Reflection(const AVector: TQuadrivector): TMultivector;
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

function TMultivectorHelper.Rotation(const AVector1, AVector2: TQuadrivector):
TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TMultivectorHelper.Rotation(const AVector1, AVector2: TMultivector):
TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TMultivectorHelper.Wedge(const AVector: TVector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := +fms * AVector.fm0;

  result.fm1 := +fms * AVector.fm1;

  result.fm2 := +fms * AVector.fm2;

  result.fm3 := +fms * AVector.fm3;

  result.fm01 := +fm0 * AVector.fm1 - fm1 * AVector.fm0;

  result.fm02 := +fm0 * AVector.fm2 - fm2 * AVector.fm0;

  result.fm03 := +fm0 * AVector.fm3 - fm3 * AVector.fm0;

  result.fm12 := +fm1 * AVector.fm2 - fm2 * AVector.fm1;

  result.fm13 := +fm1 * AVector.fm3 - fm3 * AVector.fm1;

  result.fm23 := +fm2 * AVector.fm3 - fm3 * AVector.fm2;

  result.fm012 := +fm01 * AVector.fm2 - fm02 * AVector.fm1 + fm12 * AVector.fm0;

  result.fm013 := +fm01 * AVector.fm3 - fm03 * AVector.fm1 + fm13 * AVector.fm0;

  result.fm023 := +fm02 * AVector.fm3 - fm03 * AVector.fm2 + fm23 * AVector.fm0;

  result.fm123 := +fm12 * AVector.fm3 - fm13 * AVector.fm2 + fm23 * AVector.fm1;

  result.fm0123 := +fm012 * AVector.fm3 - fm013 * AVector.fm2 + fm023 *
    AVector.fm1 - fm123 * AVector.fm0;

end;

function TMultivectorHelper.Wedge(const AVector: TBivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := +fms * AVector.fm01;

  result.fm02 := +fms * AVector.fm02;

  result.fm03 := +fms * AVector.fm03;

  result.fm12 := +fms * AVector.fm12;

  result.fm13 := +fms * AVector.fm13;

  result.fm23 := +fms * AVector.fm23;

  result.fm012 := +fm0 * AVector.fm12 - fm1 * AVector.fm02 + fm2 * AVector.fm01;

  result.fm013 := +fm0 * AVector.fm13 - fm1 * AVector.fm03 + fm3 * AVector.fm01;

  result.fm023 := +fm0 * AVector.fm23 - fm2 * AVector.fm03 + fm3 * AVector.fm02;

  result.fm123 := +fm1 * AVector.fm23 - fm2 * AVector.fm13 + fm3 * AVector.fm12;

  result.fm0123 := +fm01 * AVector.fm23 - fm02 * AVector.fm13 + fm03 *
    AVector.fm12 + fm12 * AVector.fm03 - fm13 * AVector.fm02 + fm23 * AVector.fm01;

end;

function TMultivectorHelper.Wedge(const AVector: TTrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := +fms * AVector.fm012;

  result.fm013 := +fms * AVector.fm013;

  result.fm023 := +fms * AVector.fm023;

  result.fm123 := +fms * AVector.fm123;

  result.fm0123 := +fm0 * AVector.fm123 - fm1 * AVector.fm023 + fm2 *
    AVector.fm013 - fm3 * AVector.fm012;

end;

function TMultivectorHelper.Wedge(const AVector: TQuadrivector): TQuadrivector;
begin
  result.fm0123 := +fms * AVector.fm0123;

end;

function TMultivectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fms := +fms * AVector.fms;

  result.fm0 := +fms * AVector.fm0 + fm0 * AVector.fms;

  result.fm1 := +fms * AVector.fm1 + fm1 * AVector.fms;

  result.fm2 := +fms * AVector.fm2 + fm2 * AVector.fms;

  result.fm3 := +fms * AVector.fm3 + fm3 * AVector.fms;

  result.fm01 := +fms * AVector.fm01 + fm0 * AVector.fm1 - fm1 *
    AVector.fm0 + fm01 * AVector.fms;

  result.fm02 := +fms * AVector.fm02 + fm0 * AVector.fm2 - fm2 *
    AVector.fm0 + fm02 * AVector.fms;

  result.fm03 := +fms * AVector.fm03 + fm0 * AVector.fm3 - fm3 *
    AVector.fm0 + fm03 * AVector.fms;

  result.fm12 := +fms * AVector.fm12 + fm1 * AVector.fm2 - fm2 *
    AVector.fm1 + fm12 * AVector.fms;

  result.fm13 := +fms * AVector.fm13 + fm1 * AVector.fm3 - fm3 *
    AVector.fm1 + fm13 * AVector.fms;

  result.fm23 := +fms * AVector.fm23 + fm2 * AVector.fm3 - fm3 *
    AVector.fm2 + fm23 * AVector.fms;

  result.fm012 := +fms * AVector.fm012 + fm0 * AVector.fm12 - fm1 *
    AVector.fm02 + fm2 * AVector.fm01 + fm01 * AVector.fm2 - fm02 *
    AVector.fm1 + fm12 * AVector.fm0 + fm012 * AVector.fms;

  result.fm013 := +fms * AVector.fm013 + fm0 * AVector.fm13 - fm1 *
    AVector.fm03 + fm3 * AVector.fm01 + fm01 * AVector.fm3 - fm03 *
    AVector.fm1 + fm13 * AVector.fm0 + fm013 * AVector.fms;

  result.fm023 := +fms * AVector.fm023 + fm0 * AVector.fm23 - fm2 *
    AVector.fm03 + fm3 * AVector.fm02 + fm02 * AVector.fm3 - fm03 *
    AVector.fm2 + fm23 * AVector.fm0 + fm023 * AVector.fms;

  result.fm123 := +fms * AVector.fm123 + fm1 * AVector.fm23 - fm2 *
    AVector.fm13 + fm3 * AVector.fm12 + fm12 * AVector.fm3 - fm13 *
    AVector.fm2 + fm23 * AVector.fm1 + fm123 * AVector.fms;

  result.fm0123 := +fms * AVector.fm0123 + fm0 * AVector.fm123 - fm1 *
    AVector.fm023 + fm2 * AVector.fm013 - fm3 * AVector.fm012 + fm01 *
    AVector.fm23 - fm02 * AVector.fm13 + fm03 * AVector.fm12 + fm12 *
    AVector.fm03 - fm13 * AVector.fm02 + fm23 * AVector.fm01 + fm012 *
    AVector.fm3 - fm013 * AVector.fm2 + fm023 * AVector.fm1 - fm123 *
    AVector.fm0 + fm0123 * AVector.fms;

end;

function TMultivectorHelper.Dot(const AVector: TVector): TMultivector;
begin
  result.fms := +fm0 * AVector.fm0 - fm1 * AVector.fm1 - fm2 *
    AVector.fm2 - fm3 * AVector.fm3;

  result.fm0 := +fms * AVector.fm0 - fm01 * AVector.fm1 - fm02 *
    AVector.fm2 - fm03 * AVector.fm3;

  result.fm1 := +fms * AVector.fm1 - fm01 * AVector.fm0 - fm12 *
    AVector.fm2 - fm13 * AVector.fm3;

  result.fm2 := +fms * AVector.fm2 - fm02 * AVector.fm0 + fm12 *
    AVector.fm1 - fm23 * AVector.fm3;

  result.fm3 := +fms * AVector.fm3 - fm03 * AVector.fm0 + fm13 *
    AVector.fm1 + fm23 * AVector.fm2;

  result.fm01 := -fm012 * AVector.fm2 - fm013 * AVector.fm3;

  result.fm02 := +fm012 * AVector.fm1 - fm023 * AVector.fm3;

  result.fm03 := +fm013 * AVector.fm1 + fm023 * AVector.fm2;

  result.fm12 := +fm012 * AVector.fm0 - fm123 * AVector.fm3;

  result.fm13 := +fm013 * AVector.fm0 + fm123 * AVector.fm2;

  result.fm23 := +fm023 * AVector.fm0 - fm123 * AVector.fm1;

  result.fm012 := -fm0123 * AVector.fm3;

  result.fm013 := +fm0123 * AVector.fm2;

  result.fm023 := -fm0123 * AVector.fm1;

  result.fm123 := -fm0123 * AVector.fm0;

  result.fm0123 := 0;
end;

function TMultivectorHelper.Dot(const AVector: TBivector): TMultivector;
begin
  result.fms := +fm01 * AVector.fm01 + fm02 * AVector.fm02 + fm03 *
    AVector.fm03 - fm12 * AVector.fm12 - fm13 * AVector.fm13 - fm23 * AVector.fm23;

  result.fm0 := +fm1 * AVector.fm01 + fm2 * AVector.fm02 + fm3 *
    AVector.fm03 - fm012 * AVector.fm12 - fm013 * AVector.fm13 - fm023 * AVector.fm23;

  result.fm1 := +fm0 * AVector.fm01 + fm2 * AVector.fm12 + fm3 *
    AVector.fm13 - fm012 * AVector.fm02 - fm013 * AVector.fm03 - fm123 * AVector.fm23;

  result.fm2 := +fm0 * AVector.fm02 - fm1 * AVector.fm12 + fm3 *
    AVector.fm23 + fm012 * AVector.fm01 - fm023 * AVector.fm03 + fm123 * AVector.fm13;

  result.fm3 := +fm0 * AVector.fm03 - fm1 * AVector.fm13 - fm2 *
    AVector.fm23 + fm013 * AVector.fm01 + fm023 * AVector.fm02 - fm123 * AVector.fm12;

  result.fm01 := +fms * AVector.fm01 - fm0123 * AVector.fm23;

  result.fm02 := +fms * AVector.fm02 + fm0123 * AVector.fm13;

  result.fm03 := +fms * AVector.fm03 - fm0123 * AVector.fm12;

  result.fm12 := +fms * AVector.fm12 + fm0123 * AVector.fm03;

  result.fm13 := +fms * AVector.fm13 - fm0123 * AVector.fm02;

  result.fm23 := +fms * AVector.fm23 + fm0123 * AVector.fm01;

  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

function TMultivectorHelper.Dot(const AVector: TTrivector): TMultivector;
begin
  result.fms := -fm012 * AVector.fm012 - fm013 * AVector.fm013 -
    fm023 * AVector.fm023 + fm123 * AVector.fm123;

  result.fm0 := -fm12 * AVector.fm012 - fm13 * AVector.fm013 - fm23 *
    AVector.fm023 + fm0123 * AVector.fm123;

  result.fm1 := -fm02 * AVector.fm012 - fm03 * AVector.fm013 - fm23 *
    AVector.fm123 + fm0123 * AVector.fm023;

  result.fm2 := +fm01 * AVector.fm012 - fm03 * AVector.fm023 + fm13 *
    AVector.fm123 - fm0123 * AVector.fm013;

  result.fm3 := +fm01 * AVector.fm013 + fm02 * AVector.fm023 - fm12 *
    AVector.fm123 + fm0123 * AVector.fm012;

  result.fm01 := -fm2 * AVector.fm012 - fm3 * AVector.fm013;

  result.fm02 := +fm1 * AVector.fm012 - fm3 * AVector.fm023;

  result.fm03 := +fm1 * AVector.fm013 + fm2 * AVector.fm023;

  result.fm12 := +fm0 * AVector.fm012 - fm3 * AVector.fm123;

  result.fm13 := +fm0 * AVector.fm013 + fm2 * AVector.fm123;

  result.fm23 := +fm0 * AVector.fm023 - fm1 * AVector.fm123;

  result.fm012 := +fms * AVector.fm012;

  result.fm013 := +fms * AVector.fm013;

  result.fm023 := +fms * AVector.fm023;

  result.fm123 := +fms * AVector.fm123;

  result.fm0123 := 0;
end;

function TMultivectorHelper.Dot(const AVector: TQuadrivector): TMultivector;
begin
  result.fms := -fm0123 * AVector.fm0123;

  result.fm0 := -fm123 * AVector.fm0123;

  result.fm1 := -fm023 * AVector.fm0123;

  result.fm2 := +fm013 * AVector.fm0123;

  result.fm3 := -fm012 * AVector.fm0123;

  result.fm01 := -fm23 * AVector.fm0123;

  result.fm02 := +fm13 * AVector.fm0123;

  result.fm03 := -fm12 * AVector.fm0123;

  result.fm12 := +fm03 * AVector.fm0123;

  result.fm13 := -fm02 * AVector.fm0123;

  result.fm23 := +fm01 * AVector.fm0123;

  result.fm012 := +fm3 * AVector.fm0123;

  result.fm013 := -fm2 * AVector.fm0123;

  result.fm023 := +fm1 * AVector.fm0123;

  result.fm123 := +fm0 * AVector.fm0123;

  result.fm0123 := +fms * AVector.fm0123;

end;

function TMultivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fms :=  fms   * AVector.fms   +fm0   * AVector.fm0   -fm1   * AVector.fm1   -fm2    * AVector.fm2
                -fm3   * AVector.fm3   +fm01  * AVector.fm01  +fm02  * AVector.fm02  +fm03   * AVector.fm03
                -fm12  * AVector.fm12  -fm13  * AVector.fm13  -fm23  * AVector.fm23  -fm012  * AVector.fm012
                -fm013 * AVector.fm013 -fm023 * AVector.fm023 +fm123 * AVector.fm123 -fm0123 * AVector.fm0123;

  result.fm0 :=  fms   * AVector.fm0   +fm0   * AVector.fms   +fm1   * AVector.fm01   +fm2    * AVector.fm02
                +fm3   * AVector.fm03  -fm01  * AVector.fm1   -fm02  * AVector.fm2    -fm03   * AVector.fm3
                -fm12  * AVector.fm012 -fm13  * AVector.fm013 -fm23  * AVector.fm023  -fm012  * AVector.fm12
                -fm013 * AVector.fm13  -fm023 * AVector.fm23  -fm123 * AVector.fm0123 +fm0123 * AVector.fm123;

  result.fm1 :=  fms   * AVector.fm1   +fm0   * AVector.fm01   +fm1   * AVector.fms    +fm2    * AVector.fm12
                +fm3   * AVector.fm13  -fm01  * AVector.fm0    -fm02  * AVector.fm012  -fm03   * AVector.fm013
                -fm12  * AVector.fm2   -fm13  * AVector.fm3    -fm23  * AVector.fm123  -fm012  * AVector.fm02
                -fm013 * AVector.fm03  -fm023 * AVector.fm0123 -fm123 * AVector.fm23   +fm0123 * AVector.fm023;

  result.fm2 :=  fms   * AVector.fm2    +fm0   * AVector.fm02   -fm1   * AVector.fm12   +fm2    * AVector.fms
                +fm3   * AVector.fm23   +fm01  * AVector.fm012  -fm02  * AVector.fm0    -fm03   * AVector.fm023
                +fm12  * AVector.fm1    +fm13  * AVector.fm123  -fm23  * AVector.fm3    +fm012  * AVector.fm01
                +fm013 * AVector.fm0123 -fm023 * AVector.fm03   +fm123 * AVector.fm13   -fm0123 * AVector.fm013;

  result.fm3 :=  fms   * AVector.fm3    +fm0   * AVector.fm03   -fm1   * AVector.fm13   -fm2    * AVector.fm23
                +fm3   * AVector.fms    +fm01  * AVector.fm013  +fm02  * AVector.fm023  -fm03   * AVector.fm0
                -fm12  * AVector.fm123  +fm13  * AVector.fm1    +fm23  * AVector.fm2    -fm012  * AVector.fm0123
                +fm013 * AVector.fm01   +fm023 * AVector.fm02   -fm123 * AVector.fm12   +fm0123 * AVector.fm012;

  result.fm01 := +fms * AVector.fm01 - fm2 * AVector.fm012 - fm3 *
    AVector.fm013 + fm01 * AVector.fms - fm23 * AVector.fm0123 -
    fm012 * AVector.fm2 - fm013 * AVector.fm3 - fm0123 * AVector.fm23;

  result.fm02 := +fms * AVector.fm02 + fm1 * AVector.fm012 - fm3 *
    AVector.fm023 + fm02 * AVector.fms + fm13 * AVector.fm0123 +
    fm012 * AVector.fm1 - fm023 * AVector.fm3 + fm0123 * AVector.fm13;

  result.fm03 := +fms * AVector.fm03 + fm1 * AVector.fm013 + fm2 *
    AVector.fm023 + fm03 * AVector.fms - fm12 * AVector.fm0123 +
    fm013 * AVector.fm1 + fm023 * AVector.fm2 - fm0123 * AVector.fm12;

  result.fm12 := +fms * AVector.fm12 + fm0 * AVector.fm012 - fm3 *
    AVector.fm123 + fm03 * AVector.fm0123 + fm12 * AVector.fms +
    fm012 * AVector.fm0 - fm123 * AVector.fm3 + fm0123 * AVector.fm03;

  result.fm13 := +fms * AVector.fm13 + fm0 * AVector.fm013 + fm2 *
    AVector.fm123 - fm02 * AVector.fm0123 + fm13 * AVector.fms +
    fm013 * AVector.fm0 + fm123 * AVector.fm2 - fm0123 * AVector.fm02;

  result.fm23 := +fms * AVector.fm23 + fm0 * AVector.fm023 - fm1 *
    AVector.fm123 + fm01 * AVector.fm0123 + fm23 * AVector.fms +
    fm023 * AVector.fm0 - fm123 * AVector.fm1 + fm0123 * AVector.fm01;

  result.fm012 := +fms * AVector.fm012 + fm3 * AVector.fm0123 + fm012 *
    AVector.fms - fm0123 * AVector.fm3;

  result.fm013 := +fms * AVector.fm013 - fm2 * AVector.fm0123 + fm013 *
    AVector.fms + fm0123 * AVector.fm2;

  result.fm023 := +fms * AVector.fm023 + fm1 * AVector.fm0123 + fm023 *
    AVector.fms - fm0123 * AVector.fm1;

  result.fm123 := +fms * AVector.fm123 + fm0 * AVector.fm0123 + fm123 *
    AVector.fms - fm0123 * AVector.fm0;

  result.fm0123 := +fms * AVector.fm0123 + fm0123 * AVector.fms;

end;

function TMultivectorHelper.SameValue(const AVector: TMultivector): boolean;
begin
  result := Math.SameValue(Self.fms,    AVector.fms    ) and
            Math.SameValue(Self.fm0,    AVector.fm0    ) and
            Math.SameValue(Self.fm1,    AVector.fm1    ) and
            Math.SameValue(Self.fm2,    AVector.fm2    ) and
            Math.SameValue(Self.fm3,    AVector.fm3    ) and
            Math.SameValue(Self.fm01,   AVector.fm01   ) and
            Math.SameValue(Self.fm02,   AVector.fm02   ) and
            Math.SameValue(Self.fm03,   AVector.fm03   ) and
            Math.SameValue(Self.fm12,   AVector.fm12   ) and
            Math.SameValue(Self.fm13,   AVector.fm13   ) and
            Math.SameValue(Self.fm23,   AVector.fm23   ) and
            Math.SameValue(Self.fm012,  AVector.fm012  ) and
            Math.SameValue(Self.fm013,  AVector.fm013  ) and
            Math.SameValue(Self.fm023,  AVector.fm023  ) and
            Math.SameValue(Self.fm123,  AVector.fm123  ) and
            Math.SameValue(Self.fm0123, AVector.fm0123);
end;

function TMultivectorHelper.SameValue(const AVector: TQuadrivector): boolean;
begin
  result := Math.SameValue(fms,               0.0) and
            Math.SameValue(fm0,               0.0) and
            Math.SameValue(fm1,               0.0) and
            Math.SameValue(fm2,               0.0) and
            Math.SameValue(fm3,               0.0) and
            Math.SameValue(fm01,              0.0) and
            Math.SameValue(fm02,              0.0) and
            Math.SameValue(fm03,              0.0) and
            Math.SameValue(fm12,              0.0) and
            Math.SameValue(fm13,              0.0) and
            Math.SameValue(fm23,              0.0) and
            Math.SameValue(fm012,             0.0) and
            Math.SameValue(fm013,             0.0) and
            Math.SameValue(fm023,             0.0) and
            Math.SameValue(fm123,             0.0) and
            Math.SameValue(fm0123, AVector.fm0123);
end;

function TMultivectorHelper.SameValue(const AVector: TTrivector): boolean;
begin
  result := Math.SameValue(fms,              0.0) and
            Math.SameValue(fm0,              0.0) and
            Math.SameValue(fm1,              0.0) and
            Math.SameValue(fm2,              0.0) and
            Math.SameValue(fm3,              0.0) and
            Math.SameValue(fm01,             0.0) and
            Math.SameValue(fm02,             0.0) and
            Math.SameValue(fm03,             0.0) and
            Math.SameValue(fm12,             0.0) and
            Math.SameValue(fm13,             0.0) and
            Math.SameValue(fm23,             0.0) and
            Math.SameValue(fm012,  AVector.fm012) and
            Math.SameValue(fm013,  AVector.fm013) and
            Math.SameValue(fm023,  AVector.fm023) and
            Math.SameValue(fm123,  AVector.fm123) and
            Math.SameValue(fm0123,           0.0);
end;

function TMultivectorHelper.SameValue(const AVector: TBivector): boolean;
begin
  result := Math.SameValue(fms,            0.0) and
            Math.SameValue(fm0,            0.0) and
            Math.SameValue(fm1,            0.0) and
            Math.SameValue(fm2,            0.0) and
            Math.SameValue(fm3,            0.0) and
            Math.SameValue(fm01,  AVector.fm01) and
            Math.SameValue(fm02,  AVector.fm02) and
            Math.SameValue(fm03,  AVector.fm03) and
            Math.SameValue(fm12,  AVector.fm12) and
            Math.SameValue(fm13,  AVector.fm13) and
            Math.SameValue(fm23,  AVector.fm23) and
            Math.SameValue(fm012,          0.0) and
            Math.SameValue(fm013,          0.0) and
            Math.SameValue(fm023,          0.0) and
            Math.SameValue(fm123,          0.0) and
            Math.SameValue(fm0123,         0.0);
end;

function TMultivectorHelper.SameValue(const AVector: TVector): boolean;
begin
  result := Math.SameValue(fms,           0.0) and
            Math.SameValue(fm0,   AVector.fm0) and
            Math.SameValue(fm1,   AVector.fm1) and
            Math.SameValue(fm2,   AVector.fm2) and
            Math.SameValue(fm3,   AVector.fm3) and
            Math.SameValue(fm01,          0.0) and
            Math.SameValue(fm02,          0.0) and
            Math.SameValue(fm03,          0.0) and
            Math.SameValue(fm12,          0.0) and
            Math.SameValue(fm13,          0.0) and
            Math.SameValue(fm23,          0.0) and
            Math.SameValue(fm012,         0.0) and
            Math.SameValue(fm013,         0.0) and
            Math.SameValue(fm023,         0.0) and
            Math.SameValue(fm123,         0.0) and
            Math.SameValue(fm0123,        0.0);
end;

function TMultivectorHelper.SameValue(const AVector: double): boolean;
begin
  result := Math.SameValue(fms,    AVector) and
            Math.SameValue(fm0,        0.0) and
            Math.SameValue(fm1,        0.0) and
            Math.SameValue(fm2,        0.0) and
            Math.SameValue(fm3,        0.0) and
            Math.SameValue(fm01,       0.0) and
            Math.SameValue(fm02,       0.0) and
            Math.SameValue(fm03,       0.0) and
            Math.SameValue(fm12,       0.0) and
            Math.SameValue(fm13,       0.0) and
            Math.SameValue(fm23,       0.0) and
            Math.SameValue(fm012,      0.0) and
            Math.SameValue(fm013,      0.0) and
            Math.SameValue(fm023,      0.0) and
            Math.SameValue(fm123,      0.0) and
            Math.SameValue(fm0123,     0.0);
end;

function TMultivectorHelper.Extract(AComponents: TMultivectorComponents): TMultivector;
begin
  result := NullMultivector;
  if mcs    in AComponents then result.fms    := fms;
  if mc0    in AComponents then result.fm0    := fm0;
  if mc1    in AComponents then result.fm1    := fm1;
  if mc2    in AComponents then result.fm2    := fm2;
  if mc3    in AComponents then result.fm3    := fm3;
  if mc01   in AComponents then result.fm01   := fm01;
  if mc02   in AComponents then result.fm02   := fm02;
  if mc03   in AComponents then result.fm03   := fm03;
  if mc12   in AComponents then result.fm12   := fm12;
  if mc13   in AComponents then result.fm13   := fm13;
  if mc23   in AComponents then result.fm23   := fm23;
  if mc012  in AComponents then result.fm012  := fm012;
  if mc013  in AComponents then result.fm013  := fm013;
  if mc023  in AComponents then result.fm023  := fm023;
  if mc123  in AComponents then result.fm123  := fm123;
  if mc0123 in AComponents then result.fm0123 := fm0123;
end;

function TMultivectorHelper.Extract(AComponents: TTrivectorComponents): TTrivector;
begin
  result := NullTrivector;
  if tc012  in AComponents then result.fm012 := fm012;
  if tc013  in AComponents then result.fm013 := fm013;
  if tc023  in AComponents then result.fm023 := fm023;
  if tc123  in AComponents then result.fm123 := fm123;
end;

function TMultivectorHelper.Extract(AComponents: TBivectorComponents): TBivector;
begin
  result := NullBivector;
  if bc01 in AComponents then result.fm01 := fm01;
  if bc02 in AComponents then result.fm02 := fm02;
  if bc03 in AComponents then result.fm03 := fm03;
  if bc12 in AComponents then result.fm12 := fm12;
  if bc13 in AComponents then result.fm13 := fm13;
  if bc23 in AComponents then result.fm23 := fm23;
end;

function TMultivectorHelper.Extract(AComponents: TVectorComponents): TVector;
begin
  result := NullVector;
  if vc0 in AComponents then result.fm0 := fm0;
  if vc1 in AComponents then result.fm1 := fm1;
  if vc2 in AComponents then result.fm2 := fm2;
  if vc3 in AComponents then result.fm3 := fm3;
end;

function TMultivectorHelper.ExtractScalar: double;
begin
  result := fms;
end;

function TMultivectorHelper.ExtractVector: TVector;
begin
  result.fm0 := fm0;
  result.fm1 := fm1;
  result.fm2 := fm2;
  result.fm3 := fm3;
end;

function TMultivectorHelper.ExtractBivector: TBivector;
begin
  result.fm01 := fm01;
  result.fm02 := fm02;
  result.fm03 := fm03;
  result.fm12 := fm12;
  result.fm13 := fm13;
  result.fm23 := fm23;
end;

function TMultivectorHelper.ExtractTrivector: TTrivector;
begin
  result.fm012 := fm012;
  result.fm013 := fm013;
  result.fm023 := fm023;
  result.fm123 := fm123;
end;

function TMultivectorHelper.ExtractQuadrivector: TQuadrivector;
begin
  result.fm0123 := fm0123;
end;

function TMultivectorHelper.IsQuadrivector: boolean;
begin
  result := (Math.SameValue(fms,    0.0)) and
            (Math.SameValue(fm0,    0.0)) and
            (Math.SameValue(fm1,    0.0)) and
            (Math.SameValue(fm2,    0.0)) and
            (Math.SameValue(fm3,    0.0)) and
            (Math.SameValue(fm01,   0.0)) and
            (Math.SameValue(fm02,   0.0)) and
            (Math.SameValue(fm03,   0.0)) and
            (Math.SameValue(fm12,   0.0)) and
            (Math.SameValue(fm13,   0.0)) and
            (Math.SameValue(fm23,   0.0)) and
            (Math.SameValue(fm012,  0.0)) and
            (Math.SameValue(fm013,  0.0)) and
            (Math.SameValue(fm023,  0.0)) and
            (Math.SameValue(fm123,  0.0));

  if result then result := (not Math.SameValue(fm0123, 0.0));
end;

function TMultivectorHelper.IsTrivector: boolean;
begin
  result := (Math.SameValue(fms,    0.0)) and
            (Math.SameValue(fm0,    0.0)) and
            (Math.SameValue(fm1,    0.0)) and
            (Math.SameValue(fm2,    0.0)) and
            (Math.SameValue(fm3,    0.0)) and
            (Math.SameValue(fm01,   0.0)) and
            (Math.SameValue(fm02,   0.0)) and
            (Math.SameValue(fm03,   0.0)) and
            (Math.SameValue(fm12,   0.0)) and
            (Math.SameValue(fm13,   0.0)) and
            (Math.SameValue(fm23,   0.0)) and
            (Math.SameValue(fm0123, 0.0));

  if result then result := (not Math.SameValue(fm012, 0.0)) or
                           (not Math.SameValue(fm013, 0.0)) or
                           (not Math.SameValue(fm023, 0.0)) or
                           (not Math.SameValue(fm123, 0.0));
end;

function TMultivectorHelper.IsBiVector: boolean;
begin
  result := (Math.SameValue(fms,    0.0)) and
            (Math.SameValue(fm0,    0.0)) and
            (Math.SameValue(fm1,    0.0)) and
            (Math.SameValue(fm2,    0.0)) and
            (Math.SameValue(fm3,    0.0)) and
            (Math.SameValue(fm012,  0.0)) and
            (Math.SameValue(fm013,  0.0)) and
            (Math.SameValue(fm023,  0.0)) and
            (Math.SameValue(fm123,  0.0)) and
            (Math.SameValue(fm0123, 0.0));

  if result then result := (not Math.SameValue(fm01, 0.0)) or
                           (not Math.SameValue(fm02, 0.0)) or
                           (not Math.SameValue(fm03, 0.0)) or
                           (not Math.SameValue(fm12, 0.0)) or
                           (not Math.SameValue(fm13, 0.0)) or
                           (not Math.SameValue(fm23, 0.0));
end;

function TMultivectorHelper.IsVector: boolean;
begin
  result := (Math.SameValue(fms,    0.0)) and
            (Math.SameValue(fm01,   0.0)) and
            (Math.SameValue(fm02,   0.0)) and
            (Math.SameValue(fm03,   0.0)) and
            (Math.SameValue(fm12,   0.0)) and
            (Math.SameValue(fm13,   0.0)) and
            (Math.SameValue(fm23,   0.0)) and
            (Math.SameValue(fm012,  0.0)) and
            (Math.SameValue(fm013,  0.0)) and
            (Math.SameValue(fm023,  0.0)) and
            (Math.SameValue(fm123,  0.0)) and
            (Math.SameValue(fm0123, 0.0));

  if result then result := (not Math.SameValue(fm0, 0.0)) or
                           (not Math.SameValue(fm1, 0.0)) or
                           (not Math.SameValue(fm2, 0.0)) or
                           (not Math.SameValue(fm3, 0.0));
end;

function TMultivectorHelper.IsScalar: boolean;
begin
  result := (Math.SameValue(fm0,    0.0)) and
            (Math.SameValue(fm1,    0.0)) and
            (Math.SameValue(fm2,    0.0)) and
            (Math.SameValue(fm3,    0.0)) and
            (Math.SameValue(fm01,   0.0)) and
            (Math.SameValue(fm02,   0.0)) and
            (Math.SameValue(fm03,   0.0)) and
            (Math.SameValue(fm12,   0.0)) and
            (Math.SameValue(fm13,   0.0)) and
            (Math.SameValue(fm23,   0.0)) and
            (Math.SameValue(fm012,  0.0)) and
            (Math.SameValue(fm013,  0.0)) and
            (Math.SameValue(fm023,  0.0)) and
            (Math.SameValue(fm123,  0.0)) and
            (Math.SameValue(fm0123, 0.0));

  if result then result := (not Math.SameValue(fms, 0.0));
end;

function TMultivectorHelper.IsNull: boolean;
begin
  result := SameValue(NullMultivector);
end;

function TMultivectorHelper.IsA: string;
begin
  if IsNull         then result := 'Null'          else
  if IsQuadrivector then result := 'TQuadrivector' else
  if IsTrivector    then result := 'TTrivector'    else
  if IsBivector     then result := 'TBivector'     else
  if IsVector       then result := 'TVector'       else
  if IsScalar       then result := 'TScalar'       else result := 'TMultivector';
end;

function TMultivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fms,    0.0) then result := result + Fmt(fms,    APrecision, ADigits) + ' ';
  if not Math.SameValue(fm0,    0.0) then result := result + Fmt(fm0,    APrecision, ADigits) + 'e0 ';
  if not Math.SameValue(fm1,    0.0) then result := result + Fmt(fm1,    APrecision, ADigits) + 'e1 ';
  if not Math.SameValue(fm2,    0.0) then result := result + Fmt(fm2,    APrecision, ADigits) + 'e2 ';
  if not Math.SameValue(fm3,    0.0) then result := result + Fmt(fm3,    APrecision, ADigits) + 'e3 ';
  if not Math.SameValue(fm01,   0.0) then result := result + Fmt(fm01,   APrecision, ADigits) + 'e01 ';
  if not Math.SameValue(fm02,   0.0) then result := result + Fmt(fm02,   APrecision, ADigits) + 'e02 ';
  if not Math.SameValue(fm03,   0.0) then result := result + Fmt(fm03,   APrecision, ADigits) + 'e03 ';
  if not Math.SameValue(fm12,   0.0) then result := result + Fmt(fm12,   APrecision, ADigits) + 'e12 ';
  if not Math.SameValue(fm13,   0.0) then result := result + Fmt(fm13,   APrecision, ADigits) + 'e13 ';
  if not Math.SameValue(fm23,   0.0) then result := result + Fmt(fm23,   APrecision, ADigits) + 'e23 ';
  if not Math.SameValue(fm012,  0.0) then result := result + Fmt(fm012,  APrecision, ADigits) + 'e012 ';
  if not Math.SameValue(fm013,  0.0) then result := result + Fmt(fm013,  APrecision, ADigits) + 'e013 ';
  if not Math.SameValue(fm023,  0.0) then result := result + Fmt(fm023,  APrecision, ADigits) + 'e023 ';
  if not Math.SameValue(fm123,  0.0) then result := result + Fmt(fm123,  APrecision, ADigits) + 'e123 ';
  if not Math.SameValue(fm0123, 0.0) then result := result + Fmt(fm0123, APrecision, ADigits) + 'e0123 ';

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
  if not Math.SameValue(fms,    0.0) then result := result + Fmt(fms   ) + ' ';
  if not Math.SameValue(fm0,    0.0) then result := result + Fmt(fm0   ) + 'e0 ';
  if not Math.SameValue(fm1,    0.0) then result := result + Fmt(fm1   ) + 'e1 ';
  if not Math.SameValue(fm2,    0.0) then result := result + Fmt(fm2   ) + 'e2 ';
  if not Math.SameValue(fm3,    0.0) then result := result + Fmt(fm3   ) + 'e3 ';
  if not Math.SameValue(fm01,   0.0) then result := result + Fmt(fm01  ) + 'e01 ';
  if not Math.SameValue(fm02,   0.0) then result := result + Fmt(fm02  ) + 'e02 ';
  if not Math.SameValue(fm03,   0.0) then result := result + Fmt(fm03  ) + 'e03 ';
  if not Math.SameValue(fm12,   0.0) then result := result + Fmt(fm12  ) + 'e12 ';
  if not Math.SameValue(fm13,   0.0) then result := result + Fmt(fm13  ) + 'e13 ';
  if not Math.SameValue(fm23,   0.0) then result := result + Fmt(fm23  ) + 'e23 ';
  if not Math.SameValue(fm012,  0.0) then result := result + Fmt(fm012 ) + 'e012 ';
  if not Math.SameValue(fm013,  0.0) then result := result + Fmt(fm013 ) + 'e013 ';
  if not Math.SameValue(fm023,  0.0) then result := result + Fmt(fm023 ) + 'e023 ';
  if not Math.SameValue(fm123,  0.0) then result := result + Fmt(fm123 ) + 'e123 ';
  if not Math.SameValue(fm0123, 0.0) then result := result + Fmt(fm0123) + 'e0123 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0';
end;

// TQuadrivectorHelper

function TQuadrivectorHelper.Dual: double;
begin
  result := -fm0123;
end;

function TQuadrivectorHelper.Inverse: TQuadrivector;
begin
  result.fm0123 := fm0123;
end;

function TQuadrivectorHelper.Reverse: TQuadrivector;
begin
  result.fm0123 := fm0123;
end;

function TQuadrivectorHelper.Conjugate: TQuadrivector;
begin
  result.fm0123 := fm0123;
end;

function TQuadrivectorHelper.Reciprocal: TQuadrivector;
begin
  result := Reverse / SquaredNorm;
end;

function TQuadrivectorHelper.Normalized: TQuadrivector;
begin
  result := Self / Norm;
end;

function TQuadrivectorHelper.Norm: double;
begin
  result := SquaredNorm;
  if result > 0 then
    result := sqrt(result)
  else
    result := -sqrt(-result);
end;

function TQuadrivectorHelper.SquaredNorm: double;
begin
  result := -fm0123 * fm0123;
end;

function TQuadrivectorHelper.Projection(const AVector: TVector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Projection(const AVector: TBivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Projection(const AVector: TTrivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Projection(const AVector: TQuadrivector): TQuadrivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Projection(const AVector: TMultivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Rejection(const AVector: TVector): TVector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Rejection(const AVector: TBivector): TBivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Rejection(const AVector: TTrivector): TTrivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Rejection(const AVector: TQuadrivector): TQuadrivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Rejection(const AVector: TMultivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Reflection(const AVector: TVector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Reflection(const AVector: TBivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Reflection(const AVector: TTrivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Reflection(const AVector: TQuadrivector): TQuadrivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Rotation(const AVector1, AVector2: TVector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TQuadrivectorHelper.Rotation(const AVector1, AVector2: TBivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TQuadrivectorHelper.Rotation(const AVector1, AVector2: TTrivector):
TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TQuadrivectorHelper.Rotation(const AVector1, AVector2: TQuadrivector):
TQuadrivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TQuadrivectorHelper.Rotation(const AVector1, AVector2: TMultivector):
TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TQuadrivectorHelper.Wedge(const AVector: TVector): double;
begin
  result := 0;
end;

function TQuadrivectorHelper.Wedge(const AVector: TBivector): double;
begin
  result := 0;
end;

function TQuadrivectorHelper.Wedge(const AVector: TTrivector): double;
begin
  result := 0;
end;

function TQuadrivectorHelper.Wedge(const AVector: TQuadrivector): double;
begin
  result := 0;
end;

function TQuadrivectorHelper.Wedge(const AVector: TMultivector): TQuadrivector;
begin
  result.fm0123 := fm0123 * AVector.fms;
end;

function TQuadrivectorHelper.Dot(const AVector: TVector): TTrivector;
begin
  result.fm012 := -fm0123 * AVector.fm3;
  result.fm013 :=  fm0123 * AVector.fm2;
  result.fm023 := -fm0123 * AVector.fm1;
  result.fm123 := -fm0123 * AVector.fm0;
end;

function TQuadrivectorHelper.Dot(const AVector: TBivector): TBivector;
begin
  result.fm01 := -fm0123 * AVector.fm23;
  result.fm02 :=  fm0123 * AVector.fm13;
  result.fm03 := -fm0123 * AVector.fm12;
  result.fm12 :=  fm0123 * AVector.fm03;
  result.fm13 := -fm0123 * AVector.fm02;
  result.fm23 :=  fm0123 * AVector.fm01;
end;

function TQuadrivectorHelper.Dot(const AVector: TTrivector): TMultivector;
begin
  result.fms := 0;

  result.fm0 :=  fm0123 * AVector.fm123;
  result.fm1 :=  fm0123 * AVector.fm023;
  result.fm2 := -fm0123 * AVector.fm013;
  result.fm3 :=  fm0123 * AVector.fm012;

  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

function TQuadrivectorHelper.Dot(const AVector: TQuadrivector): double;
begin
  result := -fm0123 * AVector.fm0123;
end;

function TQuadrivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fms := -fm0123 * AVector.fm0123;
  result.fm0 :=  fm0123 * AVector.fm123;
  result.fm1 :=  fm0123 * AVector.fm023;
  result.fm2 := -fm0123 * AVector.fm013;
  result.fm3 :=  fm0123 * AVector.fm012;
  result.fm01 := -fm0123 * AVector.fm23;
  result.fm02 :=  fm0123 * AVector.fm13;
  result.fm03 := -fm0123 * AVector.fm12;
  result.fm12 :=  fm0123 * AVector.fm03;
  result.fm13 := -fm0123 * AVector.fm02;
  result.fm23 :=  fm0123 * AVector.fm01;
  result.fm012 := -fm0123 * AVector.fm3;
  result.fm013 :=  fm0123 * AVector.fm2;
  result.fm023 := -fm0123 * AVector.fm1;
  result.fm123 := -fm0123 * AVector.fm0;
  result.fm0123 :=  fm0123 * AVector.fms;
end;

function TQuadrivectorHelper.SameValue(const AVector: TQuadrivector): boolean;
begin
  result := Math.SameValue(fm0123, AVector.fm0123);
end;

function TQuadrivectorHelper.SameValue(const AVector: TMultivector): boolean;
begin
  result := Math.SameValue(0.0,    AVector.fms   ) and
            Math.SameValue(0.0,    AVector.fm0   ) and
            Math.SameValue(0.0,    AVector.fm1   ) and
            Math.SameValue(0.0,    AVector.fm2   ) and
            Math.SameValue(0.0,    AVector.fm3   ) and
            Math.SameValue(0.0,    AVector.fm01  ) and
            Math.SameValue(0.0,    AVector.fm02  ) and
            Math.SameValue(0.0,    AVector.fm03  ) and
            Math.SameValue(0.0,    AVector.fm12  ) and
            Math.SameValue(0.0,    AVector.fm13  ) and
            Math.SameValue(0.0,    AVector.fm23  ) and
            Math.SameValue(0.0,    AVector.fm012 ) and
            Math.SameValue(0.0,    AVector.fm013 ) and
            Math.SameValue(0.0,    AVector.fm023 ) and
            Math.SameValue(0.0,    AVector.fm123 ) and
            Math.SameValue(fm0123, AVector.fm0123);
end;

function TQuadrivectorHelper.ToMultivector: TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := fm0123;
end;

function TQuadrivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
begin
  if not Math.SameValue(fm0123, 0.0) then
    result := Fmt(fm0123, APrecision, ADigits) + 'g0123'
  else
    result := '0g0123';
end;

function TQuadrivectorHelper.ToString: string;
begin
  if not Math.SameValue(fm0123, 0.0) then
    result := Fmt(fm0123) + 'g0123'
  else
    result := '0g0123';
end;

// TTrivectorHelper

function TTrivectorHelper.Dual: TVector;
begin
  result.fm0 := -fm123;
  result.fm1 := -fm023;
  result.fm2 :=  fm013;
  result.fm3 := -fm012;
end;

function TTrivectorHelper.Inverse: TTrivector;
begin
  result.fm012 := -fm012;
  result.fm013 := -fm013;
  result.fm023 := -fm023;
  result.fm123 := -fm123;
end;

function TTrivectorHelper.Reverse: TTrivector;
begin
  result.fm012 := -fm012;
  result.fm013 := -fm013;
  result.fm023 := -fm023;
  result.fm123 := -fm123;
end;

function TTrivectorHelper.Conjugate: TTrivector;
begin
  result.fm012 := fm012;
  result.fm013 := fm013;
  result.fm023 := fm023;
  result.fm123 := fm123;
end;

function TTrivectorHelper.Reciprocal: TTrivector;
begin
  result := -Reverse / SquaredNorm;
end;

function TTrivectorHelper.Normalized: TTrivector;
begin
  result := Self / Norm;
end;

function TTrivectorHelper.Norm: double;
begin
  result := SquaredNorm;
  if result > 0 then
    result := sqrt(result)
  else
    result := -sqrt(-result);
end;

function TTrivectorHelper.SquaredNorm: double;
begin
  result := -fm012 * fm012 -fm013 * fm013 -fm023 * fm023 +fm123 * fm123;
end;

function TTrivectorHelper.Projection(const AVector: TVector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Projection(const AVector: TBivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Projection(const AVector: TTrivector): TTrivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Projection(const AVector: TQuadrivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Projection(const AVector: TMultivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Rejection(const AVector: TVector): TTrivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Rejection(const AVector: TBivector): TBivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Rejection(const AVector: TTrivector): TTrivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Rejection(const AVector: TQuadrivector): TQuadrivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Rejection(const AVector: TMultivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Reflection(const AVector: TVector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TTrivectorHelper.Reflection(const AVector: TBivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TTrivectorHelper.Reflection(const AVector: TTrivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TTrivectorHelper.Reflection(const AVector: TQuadrivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TTrivectorHelper.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TTrivectorHelper.Rotation(const AVector1, AVector2: TVector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TTrivectorHelper.Rotation(const AVector1, AVector2: TBivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TTrivectorHelper.Rotation(const AVector1, AVector2: TTrivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TTrivectorHelper.Rotation(const AVector1, AVector2: TQuadrivector):
TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TTrivectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TTrivectorHelper.Wedge(const AVector: TVector): TQuadrivector;
begin
  result.fm0123 := +fm012 * AVector.fm3 - fm013 * AVector.fm2 + fm023 * AVector.fm1 - fm123 * AVector.fm0;
end;

function TTrivectorHelper.Wedge(const AVector: TBivector): double;
begin
  result := 0;
end;

function TTrivectorHelper.Wedge(const AVector: TTrivector): double;
begin
  result := 0;
end;

function TTrivectorHelper.Wedge(const AVector: TQuadrivector): double;
begin
  result := 0;
end;

function TTrivectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;

  result.fm012 := fm012 * AVector.fms;
  result.fm013 := fm013 * AVector.fms;
  result.fm023 := fm023 * AVector.fms;
  result.fm123 := fm123 * AVector.fms;

  result.fm0123 := fm012 * AVector.fm3 - fm013 * AVector.fm2 + fm023 * AVector.fm1 - fm123 * AVector.fm0;
end;

function TTrivectorHelper.Dot(const AVector: TVector): TBivector;
begin
  result.fm01 := -fm012 * AVector.fm2 - fm013 * AVector.fm3;
  result.fm02 :=  fm012 * AVector.fm1 - fm023 * AVector.fm3;
  result.fm03 :=  fm013 * AVector.fm1 + fm023 * AVector.fm2;
  result.fm12 :=  fm012 * AVector.fm0 - fm123 * AVector.fm3;
  result.fm13 :=  fm013 * AVector.fm0 + fm123 * AVector.fm2;
  result.fm23 :=  fm023 * AVector.fm0 - fm123 * AVector.fm1;
end;

function TTrivectorHelper.Dot(const AVector: TBivector): TMultivector;
begin
  result.fms := 0;

  result.fm0 := -fm012 * AVector.fm12 - fm013 * AVector.fm13 - fm023 * AVector.fm23;
  result.fm1 := -fm012 * AVector.fm02 - fm013 * AVector.fm03 - fm123 * AVector.fm23;
  result.fm2 := +fm012 * AVector.fm01 - fm023 * AVector.fm03 + fm123 * AVector.fm13;
  result.fm3 := +fm013 * AVector.fm01 + fm023 * AVector.fm02 - fm123 * AVector.fm12;

  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

function TTrivectorHelper.Dot(const AVector: TTrivector): double;
begin
  result := -fm012 * AVector.fm012 - fm013 * AVector.fm013 - fm023 * AVector.fm023 + fm123 * AVector.fm123;
end;

function TTrivectorHelper.Dot(const AVector: TQuadrivector): TMultivector;
begin
  result.fms := 0;

  result.fm0 := -fm123 * AVector.fm0123;
  result.fm1 := -fm023 * AVector.fm0123;
  result.fm2 :=  fm013 * AVector.fm0123;
  result.fm3 := -fm012 * AVector.fm0123;

  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

function TTrivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fms := -fm012 * AVector.fm012  - fm013 * AVector.fm013  - fm023 * AVector.fm023  + fm123 * AVector.fm123;
  result.fm0 := -fm012 * AVector.fm12   - fm013 * AVector.fm13   - fm023 * AVector.fm23   - fm123 * AVector.fm0123;
  result.fm1 := -fm012 * AVector.fm02   - fm013 * AVector.fm03   - fm023 * AVector.fm0123 - fm123 * AVector.fm23;
  result.fm2 := +fm012 * AVector.fm01   + fm013 * AVector.fm0123 - fm023 * AVector.fm03   + fm123 * AVector.fm13;
  result.fm3 := -fm012 * AVector.fm0123 + fm013 * AVector.fm01   + fm023 * AVector.fm02   - fm123 * AVector.fm12;

  result.fm01 := -fm012 * AVector.fm2 - fm013 * AVector.fm3;
  result.fm02 := +fm012 * AVector.fm1 - fm023 * AVector.fm3;
  result.fm03 := +fm013 * AVector.fm1 + fm023 * AVector.fm2;
  result.fm12 := +fm012 * AVector.fm0 - fm123 * AVector.fm3;
  result.fm13 := +fm013 * AVector.fm0 + fm123 * AVector.fm2;
  result.fm23 := +fm023 * AVector.fm0 - fm123 * AVector.fm1;

  result.fm012 := +fm012 * AVector.fms;
  result.fm013 := +fm013 * AVector.fms;
  result.fm023 := +fm023 * AVector.fms;
  result.fm123 := +fm123 * AVector.fms;

  result.fm0123 := 0;
end;

function TTrivectorHelper.SameValue(const AVector: TTrivector): boolean;
begin
  result := Math.SameValue(Self.fm012, AVector.fm012) and
            Math.SameValue(Self.fm013, AVector.fm013) and
            Math.SameValue(Self.fm023, AVector.fm023) and
            Math.SameValue(Self.fm123, AVector.fm123);
end;

function TTrivectorHelper.SameValue(const AVector: TMultivector): boolean;
begin
  result := Math.SameValue(0.0,   AVector.fms   ) and
            Math.SameValue(0.0,   AVector.fm0   ) and
            Math.SameValue(0.0,   AVector.fm1   ) and
            Math.SameValue(0.0,   AVector.fm2   ) and
            Math.SameValue(0.0,   AVector.fm3   ) and
            Math.SameValue(0.0,   AVector.fm01  ) and
            Math.SameValue(0.0,   AVector.fm02  ) and
            Math.SameValue(0.0,   AVector.fm03  ) and
            Math.SameValue(0.0,   AVector.fm12  ) and
            Math.SameValue(0.0,   AVector.fm13  ) and
            Math.SameValue(0.0,   AVector.fm23  ) and
            Math.SameValue(fm012, AVector.fm012 ) and
            Math.SameValue(fm013, AVector.fm013 ) and
            Math.SameValue(fm023, AVector.fm023 ) and
            Math.SameValue(fm123, AVector.fm123 ) and
            Math.SameValue(0.0,   AVector.fm0123);
end;

function TTrivectorHelper.Extract(AComponents: TTrivectorComponents): TTrivector;
begin
  result := NullTrivector;
  if tc012 in AComponents then result.fm012 := fm012;
  if tc013 in AComponents then result.fm013 := fm013;
  if tc023 in AComponents then result.fm023 := fm023;
  if tc123 in AComponents then result.fm123 := fm123;
end;

function TTrivectorHelper.ToString: string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm012, 0.0) then result := Fmt(fm012) + 'g012 ';
  if not Math.SameValue(fm013, 0.0) then result := Fmt(fm013) + 'g013 ';
  if not Math.SameValue(fm023, 0.0) then result := Fmt(fm023) + 'g023 ';
  if not Math.SameValue(fm123, 0.0) then result := Fmt(fm123) + 'g123 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0g012';
end;

function TTrivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm012, 0.0) then result := result + Fmt(fm012, APrecision, ADigits) + 'g012 ';
  if not Math.SameValue(fm013, 0.0) then result := result + Fmt(fm013, APrecision, ADigits) + 'g013 ';
  if not Math.SameValue(fm023, 0.0) then result := result + Fmt(fm023, APrecision, ADigits) + 'g023 ';
  if not Math.SameValue(fm123, 0.0) then result := result + Fmt(fm123, APrecision, ADigits) + 'g123 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0g012';
end;

function TTrivectorHelper.ToMultivector: TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := fm012;
  result.fm013 := fm013;
  result.fm023 := fm023;
  result.fm123 := fm123;
  result.fm0123 := 0;
end;

// TBivectorHelper

function TBivectorHelper.Dual: TBivector;
begin
  result.fm01 := -fm23;
  result.fm02 :=  fm13;
  result.fm03 := -fm12;
  result.fm12 :=  fm03;
  result.fm13 := -fm02;
  result.fm23 :=  fm01;
end;

function TBivectorHelper.Inverse: TBivector;
begin
  result.fm01 := fm01;
  result.fm02 := fm02;
  result.fm03 := fm03;
  result.fm12 := fm12;
  result.fm13 := fm13;
  result.fm23 := fm23;
end;

function TBivectorHelper.Reverse: TBivector;
begin
  result.fm01 := -fm01;
  result.fm02 := -fm02;
  result.fm03 := -fm03;
  result.fm12 := -fm12;
  result.fm13 := -fm13;
  result.fm23 := -fm23;
end;

function TBivectorHelper.Conjugate: TBivector;
begin
  result.fm01 := -fm01;
  result.fm02 := -fm02;
  result.fm03 := -fm03;
  result.fm12 := -fm12;
  result.fm13 := -fm13;
  result.fm23 := -fm23;
end;

function TBivectorHelper.Reciprocal: TBivector;
begin
//result := Reverse / SquaredNorm;
  result := ToMultivector.Reciprocal.ExtractBivector;
end;

function TBivectorHelper.Normalized: TBivector;
begin
  result := Self / Norm;
end;

function TBivectorHelper.Norm: double;
begin
  result := SquaredNorm;
  if result > 0 then
    result := sqrt(result)
  else
    result := -sqrt(-result);
end;

function TBivectorHelper.SquaredNorm: double;
begin
  result := +fm01 * fm01 +fm02 * fm02 +fm03 * fm03 -fm12 * fm12 -fm13 * fm13 -fm23 * fm23;
end;

function TBivectorHelper.Projection(const AVector: TVector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Projection(const AVector: TBivector): TBivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Projection(const AVector: TTrivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Projection(const AVector: TQuadrivector): TBivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Projection(const AVector: TMultivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Rejection(const AVector: TVector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Rejection(const AVector: TBivector): TBivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Rejection(const AVector: TTrivector): TTrivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Rejection(const AVector: TQuadrivector): TQuadrivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Rejection(const AVector: TMultivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Reflection(const AVector: TVector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TBivectorHelper.Reflection(const AVector: TBivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TBivectorHelper.Reflection(const AVector: TTrivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TBivectorHelper.Reflection(const AVector: TQuadrivector): TBivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TBivectorHelper.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: TVector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: TBivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: TTrivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: TQuadrivector): TBivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TBivectorHelper.Wedge(const AVector: TVector): TTrivector;
begin
  result.fm012 := fm01 * AVector.fm2 - fm02 * AVector.fm1 + fm12 * AVector.fm0;
  result.fm013 := fm01 * AVector.fm3 - fm03 * AVector.fm1 + fm13 * AVector.fm0;
  result.fm023 := fm02 * AVector.fm3 - fm03 * AVector.fm2 + fm23 * AVector.fm0;
  result.fm123 := fm12 * AVector.fm3 - fm13 * AVector.fm2 + fm23 * AVector.fm1;
end;

function TBivectorHelper.Wedge(const AVector: TBivector): TQuadrivector;
begin
  result.fm0123 := +fm01 * AVector.fm23 - fm02 * AVector.fm13 + fm03 * AVector.fm12 + fm12 * AVector.fm03 - fm13 * AVector.fm02 + fm23 * AVector.fm01;
end;

function TBivectorHelper.Wedge(const AVector: TTrivector): double;
begin
  result := 0;
end;

function TBivectorHelper.Wedge(const AVector: TQuadrivector): double;
begin
  result := 0;
end;

function TBivectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := fm01 * AVector.fms;

  result.fm02 := fm02 * AVector.fms;
  result.fm03 := fm03 * AVector.fms;
  result.fm12 := fm12 * AVector.fms;
  result.fm13 := fm13 * AVector.fms;
  result.fm23 := fm23 * AVector.fms;

  result.fm012 := fm01 * AVector.fm2 - fm02 * AVector.fm1 + fm12 * AVector.fm0;
  result.fm013 := fm01 * AVector.fm3 - fm03 * AVector.fm1 + fm13 * AVector.fm0;
  result.fm023 := fm02 * AVector.fm3 - fm03 * AVector.fm2 + fm23 * AVector.fm0;
  result.fm123 := fm12 * AVector.fm3 - fm13 * AVector.fm2 + fm23 * AVector.fm1;

  result.fm0123 := fm01 * AVector.fm23 - fm02 * AVector.fm13 + fm03 * AVector.fm12 + fm12 * AVector.fm03 - fm13 * AVector.fm02 + fm23 * AVector.fm01;
end;

function TBivectorHelper.Dot(const AVector: TVector): TVector;
begin
  result.fm0 := -fm01 * AVector.fm1 - fm02 * AVector.fm2 - fm03 * AVector.fm3;
  result.fm1 := -fm01 * AVector.fm0 - fm12 * AVector.fm2 - fm13 * AVector.fm3;
  result.fm2 := -fm02 * AVector.fm0 + fm12 * AVector.fm1 - fm23 * AVector.fm3;
  result.fm3 := -fm03 * AVector.fm0 + fm13 * AVector.fm1 + fm23 * AVector.fm2;
end;

function TBivectorHelper.Dot(const AVector: TBivector): double;
begin
  result := fm01 * AVector.fm01 + fm02 * AVector.fm02 + fm03 * AVector.fm03 - fm12 * AVector.fm12 - fm13 * AVector.fm13 - fm23 * AVector.fm23;
end;

function TBivectorHelper.Dot(const AVector: TTrivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := -fm12 * AVector.fm012 - fm13 * AVector.fm013 - fm23 * AVector.fm023;
  result.fm1 := -fm02 * AVector.fm012 - fm03 * AVector.fm013 - fm23 * AVector.fm123;
  result.fm2 :=  fm01 * AVector.fm012 - fm03 * AVector.fm023 + fm13 * AVector.fm123;
  result.fm3 :=  fm01 * AVector.fm013 + fm02 * AVector.fm023 - fm12 * AVector.fm123;

  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

function TBivectorHelper.Dot(const AVector: TQuadrivector): TBivector;
begin
  result.fm01 := -fm23 * AVector.fm0123;
  result.fm02 :=  fm13 * AVector.fm0123;
  result.fm03 := -fm12 * AVector.fm0123;
  result.fm12 :=  fm03 * AVector.fm0123;
  result.fm13 := -fm02 * AVector.fm0123;
  result.fm23 :=  fm01 * AVector.fm0123;
end;

function TBivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fms :=  fm01 * AVector.fm01  + fm02 * AVector.fm02  + fm03 * AVector.fm03  - fm12 * AVector.fm12  - fm13 * AVector.fm13  - fm23 * AVector.fm23;
  result.fm0 := -fm01 * AVector.fm1   - fm02 * AVector.fm2   - fm03 * AVector.fm3   - fm12 * AVector.fm012 - fm13 * AVector.fm013 - fm23 * AVector.fm023;
  result.fm1 := -fm01 * AVector.fm0   - fm02 * AVector.fm012 - fm03 * AVector.fm013 - fm12 * AVector.fm2   - fm13 * AVector.fm3   - fm23 * AVector.fm123;
  result.fm2 :=  fm01 * AVector.fm012 - fm02 * AVector.fm0   - fm03 * AVector.fm023 + fm12 * AVector.fm1   + fm13 * AVector.fm123 - fm23 * AVector.fm3;
  result.fm3 :=  fm01 * AVector.fm013 + fm02 * AVector.fm023 - fm03 * AVector.fm0   - fm12 * AVector.fm123 + fm13 * AVector.fm1   + fm23 * AVector.fm2;

  result.fm01 :=  fm01 * AVector.fms    - fm23 * AVector.fm0123;
  result.fm02 :=  fm02 * AVector.fms    + fm13 * AVector.fm0123;
  result.fm03 :=  fm03 * AVector.fms    - fm12 * AVector.fm0123;
  result.fm12 :=  fm03 * AVector.fm0123 + fm12 * AVector.fms;
  result.fm13 := -fm02 * AVector.fm0123 + fm13 * AVector.fms;
  result.fm23 :=  fm01 * AVector.fm0123 + fm23 * AVector.fms;

  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;

  result.fm0123 := 0;
end;

function TBivectorHelper.SameValue(const AVector: TBivector): boolean;
begin
  result := Math.SameValue(Self.fm01, AVector.fm01) and
            Math.SameValue(Self.fm02, AVector.fm02) and
            Math.SameValue(Self.fm03, AVector.fm03) and
            Math.SameValue(Self.fm12, AVector.fm12) and
            Math.SameValue(Self.fm13, AVector.fm13) and
            Math.SameValue(Self.fm23, AVector.fm23);
end;

function TBivectorHelper.SameValue(const AVector: TMultivector): boolean;
begin
  result := Math.SameValue(0.0,  AVector.fms   ) and
            Math.SameValue(0.0,  AVector.fm0   ) and
            Math.SameValue(0.0,  AVector.fm1   ) and
            Math.SameValue(0.0,  AVector.fm2   ) and
            Math.SameValue(0.0,  AVector.fm3   ) and
            Math.SameValue(fm01, AVector.fm01  ) and
            Math.SameValue(fm02, AVector.fm02  ) and
            Math.SameValue(fm03, AVector.fm03  ) and
            Math.SameValue(fm12, AVector.fm12  ) and
            Math.SameValue(fm13, AVector.fm13  ) and
            Math.SameValue(fm23, AVector.fm23  ) and
            Math.SameValue(0.0,  AVector.fm012 ) and
            Math.SameValue(0.0,  AVector.fm013 ) and
            Math.SameValue(0.0,  AVector.fm023 ) and
            Math.SameValue(0.0,  AVector.fm123 ) and
            Math.SameValue(0.0,  AVector.fm0123);
end;

function TBivectorHelper.Extract(AComponents: TBivectorComponents): TBivector;
begin
  result := NullBivector;
  if bc01 in AComponents then result.fm01 := fm01;
  if bc02 in AComponents then result.fm02 := fm02;
  if bc03 in AComponents then result.fm03 := fm03;
  if bc12 in AComponents then result.fm12 := fm12;
  if bc13 in AComponents then result.fm13 := fm13;
  if bc23 in AComponents then result.fm23 := fm23;
end;

function TBivectorHelper.ToString: string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm01, 0.0) then result := result + Fmt(fm01) + 'g01 ';
  if not Math.SameValue(fm02, 0.0) then result := result + Fmt(fm02) + 'g02 ';
  if not Math.SameValue(fm03, 0.0) then result := result + Fmt(fm03) + 'g03 ';
  if not Math.SameValue(fm12, 0.0) then result := result + Fmt(fm12) + 'g12 ';
  if not Math.SameValue(fm13, 0.0) then result := result + Fmt(fm13) + 'g13 ';
  if not Math.SameValue(fm23, 0.0) then result := result + Fmt(fm23) + 'g23 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0g01';
end;

function TBivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm01, 0.0) then result := result + Fmt(fm01,  APrecision, ADigits) + 'g01 ';
  if not Math.SameValue(fm02, 0.0) then result := result + Fmt(fm02,  APrecision, ADigits) + 'g02 ';
  if not Math.SameValue(fm03, 0.0) then result := result + Fmt(fm03,  APrecision, ADigits) + 'g03 ';
  if not Math.SameValue(fm12, 0.0) then result := result + Fmt(fm12,  APrecision, ADigits) + 'g12 ';
  if not Math.SameValue(fm13, 0.0) then result := result + Fmt(fm13,  APrecision, ADigits) + 'g13 ';
  if not Math.SameValue(fm23, 0.0) then result := result + Fmt(fm23,  APrecision, ADigits) + 'g23 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0g01';
end;

function TBivectorHelper.ToMultivector: TMultivector;
begin
  result.fms := 0;
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
  result.fm01 := fm01;
  result.fm02 := fm02;
  result.fm03 := fm03;
  result.fm12 := fm12;
  result.fm13 := fm13;
  result.fm23 := fm23;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

// TVectorHelper

function TVectorHelper.Dual: TTrivector;
begin
  result.fm012 :=  fm3;
  result.fm013 := -fm2;
  result.fm023 :=  fm1;
  result.fm123 :=  fm0;
end;

function TVectorHelper.Inverse: TVector;
begin
  result.fm0 := -fm0;
  result.fm1 := -fm1;
  result.fm2 := -fm2;
  result.fm3 := -fm3;
end;

function TVectorHelper.Reverse: TVector;
begin
  result.fm0 := fm0;
  result.fm1 := fm1;
  result.fm2 := fm2;
  result.fm3 := fm3;
end;

function TVectorHelper.Conjugate: TVector;
begin
  result.fm0 := -fm0;
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
  result := SquaredNorm;
  if result > 0 then
    result := sqrt(result)
  else
    result := -sqrt(-result);
end;

function TVectorHelper.SquaredNorm: double;
begin
  result := fm0 * fm0 - fm1 * fm1 - fm2 * fm2 - fm3 * fm3;
end;

function TVectorHelper.Projection(const AVector: TVector): TVector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Projection(const AVector: TBivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Projection(const AVector: TTrivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Projection(const AVector: TQuadrivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Projection(const AVector: TMultivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Rejection(const AVector: TVector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Rejection(const AVector: TBivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Rejection(const AVector: TTrivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Rejection(const AVector: TQuadrivector): TQuadrivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Rejection(const AVector: TMultivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Reflection(const AVector: TVector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TVectorHelper.Reflection(const AVector: TBivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TVectorHelper.Reflection(const AVector: TTrivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TVectorHelper.Reflection(const AVector: TQuadrivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TVectorHelper.Reflection(const AVector: TMultivector): TMultivector;
begin
  result := AVector * Self * AVector.Reciprocal;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TVector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TBivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TTrivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TQuadrivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TVectorHelper.Wedge(const AVector: TVector): TBivector;
begin
  result.fm01 := fm0 * AVector.fm1 - fm1 * AVector.fm0;
  result.fm02 := fm0 * AVector.fm2 - fm2 * AVector.fm0;
  result.fm03 := fm0 * AVector.fm3 - fm3 * AVector.fm0;
  result.fm12 := fm1 * AVector.fm2 - fm2 * AVector.fm1;
  result.fm13 := fm1 * AVector.fm3 - fm3 * AVector.fm1;
  result.fm23 := fm2 * AVector.fm3 - fm3 * AVector.fm2;
end;

function TVectorHelper.Wedge(const AVector: TBivector): TTrivector;
begin
  result.fm012 := fm0 * AVector.fm12 - fm1 * AVector.fm02 + fm2 * AVector.fm01;
  result.fm013 := fm0 * AVector.fm13 - fm1 * AVector.fm03 + fm3 * AVector.fm01;
  result.fm023 := fm0 * AVector.fm23 - fm2 * AVector.fm03 + fm3 * AVector.fm02;
  result.fm123 := fm1 * AVector.fm23 - fm2 * AVector.fm13 + fm3 * AVector.fm12;
end;

function TVectorHelper.Wedge(const AVector: TTrivector): TQuadrivector;
begin
  result.fm0123 := fm0 * AVector.fm123 - fm1 * AVector.fm023 + fm2 * AVector.fm013 - fm3 * AVector.fm012;
end;

function TVectorHelper.Wedge(const AVector: TQuadrivector): double;
begin
  result := 0;
end;

function TVectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fms := 0;
  result.fm0 := fm0 * AVector.fms;
  result.fm1 := fm1 * AVector.fms;
  result.fm2 := fm2 * AVector.fms;
  result.fm3 := fm3 * AVector.fms;

  result.fm01 := fm0 * AVector.fm1 - fm1 * AVector.fm0;
  result.fm02 := fm0 * AVector.fm2 - fm2 * AVector.fm0;
  result.fm03 := fm0 * AVector.fm3 - fm3 * AVector.fm0;
  result.fm12 := fm1 * AVector.fm2 - fm2 * AVector.fm1;
  result.fm13 := fm1 * AVector.fm3 - fm3 * AVector.fm1;
  result.fm23 := fm2 * AVector.fm3 - fm3 * AVector.fm2;

  result.fm012 := fm0 * AVector.fm12 - fm1 * AVector.fm02 + fm2 * AVector.fm01;
  result.fm013 := fm0 * AVector.fm13 - fm1 * AVector.fm03 + fm3 * AVector.fm01;
  result.fm023 := fm0 * AVector.fm23 - fm2 * AVector.fm03 + fm3 * AVector.fm02;
  result.fm123 := fm1 * AVector.fm23 - fm2 * AVector.fm13 + fm3 * AVector.fm12;

  result.fm0123 := fm0 * AVector.fm123 - fm1 * AVector.fm023 + fm2 * AVector.fm013 - fm3 * AVector.fm012;
end;

function TVectorHelper.Dot(const AVector: TVector): double;
begin
  result := fm0 * AVector.fm0 - fm1 * AVector.fm1 - fm2 * AVector.fm2 - fm3 * AVector.fm3;
end;

function TVectorHelper.Dot(const AVector: TBivector): TVector;
begin
  result.fm0 := fm1 * AVector.fm01 + fm2 * AVector.fm02 + fm3 * AVector.fm03;
  result.fm1 := fm0 * AVector.fm01 + fm2 * AVector.fm12 + fm3 * AVector.fm13;
  result.fm2 := fm0 * AVector.fm02 - fm1 * AVector.fm12 + fm3 * AVector.fm23;
  result.fm3 := fm0 * AVector.fm03 - fm1 * AVector.fm13 - fm2 * AVector.fm23;
end;

function TVectorHelper.Dot(const AVector: TTrivector): TBivector;
begin
  result.fm01 := -fm2 * AVector.fm012 - fm3 * AVector.fm013;
  result.fm02 :=  fm1 * AVector.fm012 - fm3 * AVector.fm023;
  result.fm03 :=  fm1 * AVector.fm013 + fm2 * AVector.fm023;
  result.fm12 :=  fm0 * AVector.fm012 - fm3 * AVector.fm123;
  result.fm13 :=  fm0 * AVector.fm013 + fm2 * AVector.fm123;
  result.fm23 :=  fm0 * AVector.fm023 - fm1 * AVector.fm123;
end;

function TVectorHelper.Dot(const AVector: TQuadrivector): TTrivector;
begin
  result.fm012 :=  fm3 * AVector.fm0123;
  result.fm013 := -fm2 * AVector.fm0123;
  result.fm023 :=  fm1 * AVector.fm0123;
  result.fm123 :=  fm0 * AVector.fm0123;
end;

function TVectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fms := fm0 * AVector.fm0  - fm1 * AVector.fm1  - fm2 * AVector.fm2  - fm3 * AVector.fm3;
  result.fm0 := fm0 * AVector.fms  + fm1 * AVector.fm01 + fm2 * AVector.fm02 + fm3 * AVector.fm03;
  result.fm1 := fm0 * AVector.fm01 + fm1 * AVector.fms  + fm2 * AVector.fm12 + fm3 * AVector.fm13;
  result.fm2 := fm0 * AVector.fm02 - fm1 * AVector.fm12 + fm2 * AVector.fms  + fm3 * AVector.fm23;
  result.fm3 := fm0 * AVector.fm03 - fm1 * AVector.fm13 - fm2 * AVector.fm23 + fm3 * AVector.fms;

  result.fm01 := -fm2 * AVector.fm012 - fm3 * AVector.fm013;
  result.fm02 :=  fm1 * AVector.fm012 - fm3 * AVector.fm023;
  result.fm03 :=  fm1 * AVector.fm013 + fm2 * AVector.fm023;
  result.fm12 :=  fm0 * AVector.fm012 - fm3 * AVector.fm123;
  result.fm13 :=  fm0 * AVector.fm013 + fm2 * AVector.fm123;
  result.fm23 :=  fm0 * AVector.fm023 - fm1 * AVector.fm123;

  result.fm012 :=  fm3 * AVector.fm0123;
  result.fm013 := -fm2 * AVector.fm0123;
  result.fm023 :=  fm1 * AVector.fm0123;
  result.fm123 :=  fm0 * AVector.fm0123;

  result.fm0123 := 0;
end;

function TVectorHelper.SameValue(const AVector: TVector): boolean;
begin
  result := Math.SameValue(Self.fm0, AVector.fm0) and
            Math.SameValue(Self.fm1, AVector.fm1) and
            Math.SameValue(Self.fm2, AVector.fm2) and
            Math.SameValue(Self.fm3, AVector.fm3);
end;

function TVectorHelper.SameValue(const AVector: TMultivector): boolean;
begin
  result := Math.SameValue(0.0, AVector.fms   ) and
            Math.SameValue(fm0, AVector.fm0   ) and
            Math.SameValue(fm1, AVector.fm1   ) and
            Math.SameValue(fm2, AVector.fm2   ) and
            Math.SameValue(fm3, AVector.fm3   ) and
            Math.SameValue(0.0, AVector.fm01  ) and
            Math.SameValue(0.0, AVector.fm02  ) and
            Math.SameValue(0.0, AVector.fm03  ) and
            Math.SameValue(0.0, AVector.fm12  ) and
            Math.SameValue(0.0, AVector.fm13  ) and
            Math.SameValue(0.0, AVector.fm23  ) and
            Math.SameValue(0.0, AVector.fm012 ) and
            Math.SameValue(0.0, AVector.fm013 ) and
            Math.SameValue(0.0, AVector.fm023 ) and
            Math.SameValue(0.0, AVector.fm123 ) and
            Math.SameValue(0.0, AVector.fm0123);
end;

function TVectorHelper.Extract(AComponents: TVectorComponents): TVector;
begin
  result := NullVector;
  if vc0 in AComponents then result.fm0 := fm0;
  if vc1 in AComponents then result.fm1 := fm1;
  if vc2 in AComponents then result.fm2 := fm2;
  if vc3 in AComponents then result.fm3 := fm3;
end;

function TVectorHelper.ToString: string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm0, 0.0) then result := result + Fmt(fm0) + 'g0 ';
  if not Math.SameValue(fm1, 0.0) then result := result + Fmt(fm1) + 'g1 ';
  if not Math.SameValue(fm2, 0.0) then result := result + Fmt(fm2) + 'g2 ';
  if not Math.SameValue(fm3, 0.0) then result := result + Fmt(fm3) + 'g3 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0g0';
end;

function TVectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm0, 0.0) then result := result + Fmt(fm0,  APrecision, ADigits) + 'g0 ';
  if not Math.SameValue(fm1, 0.0) then result := result + Fmt(fm1,  APrecision, ADigits) + 'g1 ';
  if not Math.SameValue(fm2, 0.0) then result := result + Fmt(fm2,  APrecision, ADigits) + 'g2 ';
  if not Math.SameValue(fm3, 0.0) then result := result + Fmt(fm3,  APrecision, ADigits) + 'g3 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0g0';
end;

function TVectorHelper.ToMultivector: TMultivector;
begin
  result.fms := 0;
  result.fm0 := fm0;
  result.fm1 := fm1;
  result.fm2 := fm2;
  result.fm3 := fm3;
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
  result.fm0123 := 0;
end;

class operator TVersor0.*(const AValue: double; const ASelf: TVersor0): TVector;
begin
  result.fm0 := AValue;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := 0;
end;

class operator TVersor1.*(const AValue: double; const ASelf: TVersor1): TVector;
begin
  result.fm0 := 0;
  result.fm1 := AValue;
  result.fm2 := 0;
  result.fm3 := 0;
end;

class operator TVersor2.*(const AValue: double; const ASelf: TVersor2): TVector;
begin
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := AValue;
  result.fm3 := 0;
end;

class operator TVersor3.*(const AValue: double; const ASelf: TVersor3): TVector;
begin
  result.fm0 := 0;
  result.fm1 := 0;
  result.fm2 := 0;
  result.fm3 := AValue;
end;

class operator TVersor01.*(const AValue: double; const ASelf: TVersor01): TBivector;
begin
  result.fm01 := AValue;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
end;

class operator TVersor02.*(const AValue: double; const ASelf: TVersor02): TBivector;
begin
  result.fm01 := 0;
  result.fm02 := AValue;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
end;

class operator TVersor03.*(const AValue: double; const ASelf: TVersor03): TBivector;
begin
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := AValue;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := 0;
end;

class operator TVersor12.*(const AValue: double; const ASelf: TVersor12): TBivector;
begin
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := AValue;
  result.fm13 := 0;
  result.fm23 := 0;
end;

class operator TVersor13.*(const AValue: double; const ASelf: TVersor13): TBivector;
begin
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := AValue;
  result.fm23 := 0;
end;

class operator TVersor23.*(const AValue: double; const ASelf: TVersor23): TBivector;
begin
  result.fm01 := 0;
  result.fm02 := 0;
  result.fm03 := 0;
  result.fm12 := 0;
  result.fm13 := 0;
  result.fm23 := AValue;
end;

class operator TVersor012.*(const AValue: double; const ASelf: TVersor012): TTrivector;
begin
  result.fm012 := AValue;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := 0;
end;

class operator TVersor013.*(const AValue: double; const ASelf: TVersor013): TTrivector;
begin
  result.fm012 := 0;
  result.fm013 := AValue;
  result.fm023 := 0;
  result.fm123 := 0;
end;

class operator TVersor023.*(const AValue: double; const ASelf: TVersor023): TTrivector;
begin
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := AValue;
  result.fm123 := 0;
end;

class operator TVersor123.*(const AValue: double; const ASelf: TVersor123): TTrivector;
begin
  result.fm012 := 0;
  result.fm013 := 0;
  result.fm023 := 0;
  result.fm123 := AValue;
end;

class operator TVersor0123.*(const AValue: double; const ASelf: TVersor0123): TQuadrivector;
begin
  result.fm0123 := AValue;
end;

end.
