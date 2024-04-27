unit Cl4;

{ Geometric Algebra Cl(4) for FreePascal.

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
  TMultivectorComponent  = (mc0, mc1, mc2, mc3, mc4, mc12, mc13, mc14, mc23, mc24, mc34, mc123, mc124, mc134, mc234, mc1234);
  TMultivectorComponents = set of TMultivectorComponent;

  // TMultivector
  TMultivector = record
  private
    fm0: double;
    fm1: double;
    fm2: double;
    fm3: double;
    fm4: double;
    fm12: double;
    fm13: double;
    fm14: double;
    fm23: double;
    fm24: double;
    fm34: double;
    fm123: double;
    fm124: double;
    fm134: double;
    fm234: double;
    fm1234: double;
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

    class operator * (const ALeft, ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator * (const ALeft: double; const ARight: TMultivector): TMultivector;

    class operator / (const ALeft, ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: double): TMultivector;
    class operator / (const ALeft: double; const ARight: TMultivector): TMultivector;
  end;
  
  // TQuadrivector
  TQuadrivector = record
  private
    fm1234: double;
  public
    class operator :=(const AValue: TQuadrivector): TMultivector;
    class operator :=(const AValue: TMultivector): TQuadrivector;
    class operator <>(const ALeft, ARight: TQuadrivector): boolean;
    class operator <>(const ALeft: TQuadrivector; const ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; const ARight: TQuadrivector): boolean;

    class operator = (const ALeft, ARight: TQuadrivector): boolean;
    class operator = (const ALeft: TQuadrivector; const ARight: TMultivector): boolean;
    class operator = (const ALeft: TMultivector; const ARight: TQuadrivector): boolean;

    class operator + (const ALeft, ARight: TQuadrivector): TQuadrivector;
    class operator + (const ALeft: TQuadrivector; const ARight: double): TMultivector;
    class operator + (const ALeft: double; const ARight: TQuadrivector): TMultivector;
    class operator + (const ALeft: TQuadrivector; const ARight: TMultivector): TMultivector;
    class operator + (const ALeft: TMultivector; const ARight: TQuadrivector): TMultivector;

    class operator - (const ASelf: TQuadrivector): TQuadrivector;
    class operator - (const ALeft, ARight: TQuadrivector): TQuadrivector;
    class operator - (const ALeft: TQuadrivector; const ARight: double): TMultivector;
    class operator - (const ALeft: double; const ARight: TQuadrivector): TMultivector;
    class operator - (const ALeft: TQuadrivector; const ARight: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: TQuadrivector): TMultivector;

    class operator * (const ALeft, ARight: TQuadrivector): double;
    class operator * (const ALeft: TQuadrivector; const ARight: double): TQuadrivector;
    class operator * (const ALeft: double; const ARight: TQuadrivector): TQuadrivector;
    class operator * (const ALeft: TQuadrivector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: TQuadrivector): TMultivector;

    class operator / (const ALeft, ARight: TQuadrivector): double;
    class operator / (const ALeft: TQuadrivector; const ARight: double): TQuadrivector;
    class operator / (const ALeft: double; const ARight: TQuadrivector): TQuadrivector;
    class operator / (const ALeft: TQuadrivector; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: TQuadrivector): TMultivector;
  end;  

  // TTrivectorComponents
  TTrivectorComponent  = (tc123, tc124, tc134, tc234);
  TTrivectorComponents = set of TTrivectorComponent;

  // TTrivector
  TTrivector = record
  private
    fm123: double;
    fm124: double;
    fm134: double;
    fm234: double;
  public
    class operator :=(const AValue: TTrivector): TMultivector;
    class operator :=(const AValue: TMultivector): TTrivector;
    class operator <>(const ALeft, ARight: TTrivector): boolean;
    class operator <>(const ALeft: TTrivector; const ARight: TMultivector): boolean;
    class operator <>(const ALeft: TMultivector; const ARight: TTrivector): boolean;

    class operator = (const ALeft, ARight: TTrivector): boolean;
    class operator = (const ALeft: TTrivector; const ARight: TMultivector): boolean;
    class operator = (const ALeft: TMultivector; const ARight: TTrivector): boolean;

    class operator + (const ALeft, ARight: TTrivector): TTrivector;
    class operator + (const ALeft: TTrivector; const ARight: double): TMultivector;
    class operator + (const ALeft: double; const ARight: TTrivector): TMultivector;
    class operator + (const ALeft: TTrivector; const ARight: TQuadrivector): TMultivector;
    class operator + (const ALeft: TQuadrivector; const ARight: TTrivector): TMultivector;
    class operator + (const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator + (const ALeft: TMultivector; const ARight: TTrivector): TMultivector;

    class operator - (const ASelf: TTrivector): TTrivector;
    class operator - (const ALeft, ARight: TTrivector): TTrivector;
    class operator - (const ALeft: TTrivector; const ARight: double): TMultivector;
    class operator - (const ALeft: double; const ARight: TTrivector): TMultivector;
    class operator - (const ALeft: TTrivector; const ARight: TQuadrivector): TMultivector;
    class operator - (const ALeft: TQuadrivector; const ARight: TTrivector): TMultivector;
    class operator - (const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: TTrivector): TMultivector;

    class operator * (const ALeft, ARight: TTrivector): TMultivector;
    class operator * (const ALeft: TTrivector; const ARight: double): TTrivector;
    class operator * (const ALeft: double; const ARight: TTrivector): TTrivector;
    class operator * (const ALeft: TTrivector; const ARight: TQuadrivector): TMultivector;
    class operator * (const ALeft: TQuadrivector; const ARight: TTrivector): TMultivector;
    class operator * (const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: TTrivector): TMultivector;

    class operator / (const ALeft, ARight: TTrivector): TMultivector;
    class operator / (const ALeft: TTrivector; const ARight: double): TTrivector;
    class operator / (const ALeft: double; const ARight: TTrivector): TTrivector;
    class operator / (const ALeft: TTrivector; const ARight: TQuadrivector): TMultivector;
    class operator / (const ALeft: TQuadrivector; const ARight: TTrivector): TMultivector;
    class operator / (const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: TTrivector): TMultivector;
  end;

  // TBivectorComponents
  TBivectorComponent  = (bc12, bc13, bc14, bc23, bc24, bc34);
  TBivectorComponents = set of TBivectorComponent;

  // TBivector
  TBivector = record
  private
    fm12: double;
    fm13: double;
    fm14: double;
    fm23: double;
    fm24: double;
    fm34: double;
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
    class operator + (const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator + (const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator + (const ALeft: TBivector; const ARight: TQuadrivector): TMultivector;
    class operator + (const ALeft: TQuadrivector; const ARight: TBivector): TMultivector;
    class operator + (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator + (const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    class operator - (const ASelf: TBivector): TBivector;
    class operator - (const ALeft, ARight: TBivector): TBivector;
    class operator - (const ALeft: TBivector; const ARight: double): TMultivector;
    class operator - (const ALeft: double; const ARight: TBivector): TMultivector;
    class operator - (const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator - (const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator - (const ALeft: TBivector; const ARight: TQuadrivector): TMultivector;
    class operator - (const ALeft: TQuadrivector; const ARight: TBivector): TMultivector;
    class operator - (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    class operator * (const ALeft, ARight: TBivector): TMultivector;
    class operator * (const ALeft: TBivector; const ARight: double): TBivector;
    class operator * (const ALeft: double; const ARight: TBivector): TBivector;
    class operator * (const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator * (const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator * (const ALeft: TBivector; const ARight: TQuadrivector): TBivector;
    class operator * (const ALeft: TQuadrivector; const ARight: TBivector): TBivector;
    class operator * (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: TBivector): TMultivector;

    class operator / (const ALeft, ARight: TBivector): TMultivector;
    class operator / (const ALeft: TBivector; const ARight: double): TBivector;
    class operator / (const ALeft: double; const ARight: TBivector): TBivector;
    class operator / (const ALeft: TBivector; const ARight: TTrivector): TMultivector;
    class operator / (const ALeft: TTrivector; const ARight: TBivector): TMultivector;
    class operator / (const ALeft: TBivector; const ARight: TQuadrivector): TMultivector;
    class operator / (const ALeft: TQuadrivector; const ARight: TBivector): TMultivector;
    class operator / (const ALeft: TBivector; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: TBivector): TMultivector;
  end;

  // TVectorComponents
  TVectorComponent  = (vc1, vc2, vc3, vc4);
  TVectorComponents = set of TVectorComponent;

  // TVector
  TVector = record
  private
    fm1: double;
    fm2: double;
    fm3: double;
    fm4: double;
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
    class operator + (const ALeft: TVector; const ARight: TTrivector): TMultivector;
    class operator + (const ALeft: TTrivector; const ARight: TVector): TMultivector;
    class operator + (const ALeft: TVector; const ARight: TQuadrivector): TMultivector;
    class operator + (const ALeft: TQuadrivector; const ARight: TVector): TMultivector;
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
    class operator - (const ALeft: TVector; const ARight: TQuadrivector): TMultivector;
    class operator - (const ALeft: TQuadrivector; const ARight: TVector): TMultivector;
    class operator - (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator - (const ALeft: TMultivector; const ARight: TVector): TMultivector;

    class operator * (const ALeft, ARight: TVector): TMultivector;
    class operator * (const ALeft: TVector; const ARight: double): TVector;
    class operator * (const ALeft: double; const ARight: TVector): TVector;
    class operator * (const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator * (const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator * (const ALeft: TVector; const ARight: TTrivector): TMultivector;
    class operator * (const ALeft: TTrivector; const ARight: TVector): TMultivector;
    class operator * (const ALeft: TVector; const ARight: TQuadrivector): TTrivector;
    class operator * (const ALeft: TQuadrivector; const ARight: TVector): TTrivector;
    class operator * (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator * (const ALeft: TMultivector; const ARight: TVector): TMultivector;

    class operator / (const ALeft, ARight: TVector): TMultivector;
    class operator / (const ALeft: TVector; const ARight: double): TVector;
    class operator / (const ALeft: double; const ARight: TVector): TVector;
    class operator / (const ALeft: TVector; const ARight: TBivector): TMultivector;
    class operator / (const ALeft: TBivector; const ARight: TVector): TMultivector;
    class operator / (const ALeft: TVector; const ARight: TTrivector): TMultivector;
    class operator / (const ALeft: TTrivector; const ARight: TVector): TMultivector;
    class operator / (const ALeft: TVector; const ARight: TQuadrivector): TMultivector;
    class operator / (const ALeft: TQuadrivector; const ARight: TVector): TMultivector;
    class operator / (const ALeft: TVector; const ARight: TMultivector): TMultivector;
    class operator / (const ALeft: TMultivector; const ARight: TVector): TMultivector;
  end;

  // TMultivectorHelper
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

    function Dot(const AVector: TVector): TMultivector; overload;
    function Dot(const AVector: TBivector): TMultivector; overload;
    function Dot(const AVector: TTrivector): TMultivector; overload;
    function Dot(const AVector: TQuadrivector): TMultivector; overload;
    function Dot(const AVector: TMultivector): TMultivector; overload;

    function Wedge(const AVector: TVector): TMultivector; overload;
    function Wedge(const AVector: TBivector): TMultivector; overload;
    function Wedge(const AVector: TTrivector): TMultivector; overload;
    function Wedge(const AVector: TQuadrivector): TQuadrivector; overload;
    function Wedge(const AVector: TMultivector): TMultivector; overload;

    function Projection(const AVector: TVector): TMultivector; overload;
    function Projection(const AVector: TBivector): TMultivector; overload;
    function Projection(const AVector: TTrivector): TMultivector; overload;
    function Projection(const AVector: TQuadrivector): TMultivector; overload;
    function Projection(const AVector: TMultivector): TMultivector; overload;

    function Rejection(const AVector: TVector): TMultivector; overload;
    function Rejection(const AVector: TBivector): TMultivector; overload;
    function Rejection(const AVector: TTrivector): TMultivector; overload;
    function Rejection(const AVector: TQuadrivector): TMultivector; overload;
    function Rejection(const AVector: TMultivector): TMultivector; overload;

    function Reflection(const AVector: TVector): TMultivector; overload;
    function Reflection(const AVector: TBivector): TMultivector; overload;
    function Reflection(const AVector: TTrivector): TMultivector; overload;
    function Reflection(const AVector: TQuadrivector): TMultivector; overload;
    function Reflection(const AVector: TMultivector): TMultivector; overload;

    function Rotation(const AVector1, AVector2: TVector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TBivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TTrivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TQuadrivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector;overload;

    function SameValue(const AValue: TMultivector): boolean;
    function SameValue(const AValue: TQuadrivector): boolean;
    function SameValue(const AValue: TTrivector): boolean;
    function SameValue(const AValue: TBivector): boolean;
    function SameValue(const AValue: TVector): boolean;
    function SameValue(const AValue: double): boolean;

    function Extract(AComponents: TMultivectorComponents): TMultivector;
    function Extract(AComponents: TTrivectorComponents): TTrivector;
    function Extract(AComponents: TBivectorComponents): TBivector;
    function Extract(AComponents: TVectorComponents): TVector;

    function ExtractQuadrivector: TQuadrivector;
    function ExtractTrivector: TTrivector;
    function ExtractBivector: TBivector;
    function ExtractVector: TVector;
    function ExtractScalar: double;

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

  // TQuadrivectorHelper
  TQuadrivectorHelper = record helper for TQuadrivector
    function Dual: double;
    function Inverse: TQuadrivector;
    function Reverse: TQuadrivector;
    function Conjugate: TQuadrivector;
    function Reciprocal: TQuadrivector;
    function Normalized: TQuadrivector;
    function Norm: double;
    function SquaredNorm: double;

    function Dot(const AVector: TVector): TTrivector; overload;
    function Dot(const AVector: TBivector): TBivector; overload;
    function Dot(const AVector: TTrivector): TVector; overload;
    function Dot(const AVector: TQuadrivector): double; overload;
    function Dot(const AVector: TMultivector): TMultivector; overload;

    function Wedge(const AVector: TVector): double; overload;
    function Wedge(const AVector: TBivector): double; overload;
    function Wedge(const AVector: TTrivector): double; overload;
    function Wedge(const AVector: TQuadrivector): double; overload;
    function Wedge(const AVector: TMultivector): TQuadrivector; overload;

    function Projection(const AVector: TVector): TMultivector; overload;
    function Projection(const AVector: TBivector): TMultivector; overload;
    function Projection(const AVector: TTrivector): TMultivector; overload;
    function Projection(const AVector: TQuadrivector): TMultivector; overload;
    function Projection(const AVector: TMultivector): TMultivector; overload;

    function Rejection(const AVector: TVector): TMultivector; overload;
    function Rejection(const AVector: TBivector): TMultivector; overload;
    function Rejection(const AVector: TTrivector): TMultivector; overload;
    function Rejection(const AVector: TQuadrivector): TMultivector; overload;
    function Rejection(const AVector: TMultivector): TMultivector; overload;

    function Reflection(const AVector: TVector): TMultivector; overload;
    function Reflection(const AVector: TBivector): TMultivector; overload;
    function Reflection(const AVector: TTrivector): TMultivector; overload;
    function Reflection(const AVector: TQuadrivector): TMultivector; overload;
    function Reflection(const AVector: TMultivector): TMultivector; overload;

    function Rotation(const AVector1, AVector2: TVector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TBivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TTrivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TQuadrivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector; overload;

    function SameValue(const AValue: TMultivector): boolean;
    function SameValue(const AValue: TQuadrivector): boolean;

    function ToMultivector: TMultivector;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
  end;

  // TTrivectorHelper
  TTrivectorHelper = record helper for TTrivector
    function Dual: TVector;
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
    function Dot(const AVector: TQuadrivector): TVector; overload;
    function Dot(const AVector: TMultivector): TMultivector; overload;

    function Wedge(const AVector: TVector): TQuadrivector; overload;
    function Wedge(const AVector: TBivector): double; overload;
    function Wedge(const AVector: TTrivector): double; overload;
    function Wedge(const AVector: TQuadrivector): double; overload;
    function Wedge(const AVector: TMultivector): TMultivector; overload;

    function Projection(const AVector: TVector): TMultivector; overload;
    function Projection(const AVector: TBivector): TMultivector; overload;
    function Projection(const AVector: TTrivector): TMultivector; overload;
    function Projection(const AVector: TQuadrivector): TMultivector; overload;
    function Projection(const AVector: TMultivector): TMultivector; overload;

    function Rejection(const AVector: TVector): TMultivector; overload;
    function Rejection(const AVector: TBivector): TMultivector; overload;
    function Rejection(const AVector: TTrivector): TMultivector; overload;
    function Rejection(const AVector: TQuadrivector): TMultivector; overload;
    function Rejection(const AVector: TMultivector): TMultivector; overload;

    function Reflection(const AVector: TVector): TMultivector; overload;
    function Reflection(const AVector: TBivector): TMultivector; overload;
    function Reflection(const AVector: TTrivector): TMultivector; overload;
    function Reflection(const AVector: TQuadrivector): TMultivector; overload;
    function Reflection(const AVector: TMultivector): TMultivector; overload;

    function Rotation(const AVector1, AVector2: TVector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TBivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TTrivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TQuadrivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector; overload;

    function SameValue(const AValue: TMultivector): boolean;
    function SameValue(const AValue: TTrivector): boolean;

    function Extract(AComponents: TTrivectorComponents): TTrivector;

    function ToMultivector: TMultivector;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
  end;

  // TBivectorHelper
  TBivectorHelper = record helper for TBivector
    function Dual: TBivector;
    function Inverse: TBivector;
    function Reverse: TBivector;
    function Conjugate: TBivector;
    function Reciprocal: TMultivector;
    function Normalized: TBivector;
    function Norm: double;
    function SquaredNorm: double;

    function Dot(const AVector: TVector): TVector; overload;
    function Dot(const AVector: TBivector): double; overload;
    function Dot(const AVector: TTrivector): TVector; overload;
    function Dot(const AVector: TQuadrivector): TBivector; overload;
    function Dot(const AVector: TMultivector): TMultivector; overload;

    function Wedge(const AVector: TVector): TTrivector; overload;
    function Wedge(const AVector: TBivector): TQuadrivector; overload;
    function Wedge(const AVector: TTrivector): double; overload;
    function Wedge(const AVector: TQuadrivector): double; overload;
    function Wedge(const AVector: TMultivector): TMultivector; overload;

    function Projection(const AVector: TVector): TMultivector; overload;
    function Projection(const AVector: TBivector): TMultivector; overload;
    function Projection(const AVector: TTrivector): TMultivector; overload;
    function Projection(const AVector: TQuadrivector): TMultivector; overload;
    function Projection(const AVector: TMultivector): TMultivector; overload;

    function Rejection(const AVector: TVector): TMultivector; overload;
    function Rejection(const AVector: TBivector): TMultivector; overload;
    function Rejection(const AVector: TTrivector): TMultivector; overload;
    function Rejection(const AVector: TQuadrivector): TMultivector; overload;
    function Rejection(const AVector: TMultivector): TMultivector; overload;

    function Reflection(const AVector: TVector): TMultivector; overload;
    function Reflection(const AVector: TBivector): TMultivector; overload;
    function Reflection(const AVector: TTrivector): TMultivector; overload;
    function Reflection(const AVector: TQuadrivector): TMultivector; overload;
    function Reflection(const AVector: TMultivector): TMultivector; overload;

    function Rotation(const AVector1, AVector2: TVector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TBivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TTrivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TQuadrivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TMultivector): TMultivector; overload;

    function SameValue(const AValue: TMultivector): boolean;
    function SameValue(const AValue: TBivector): boolean;

    function Extract(AComponents: TBivectorComponents): TBivector;

    function ToMultivector: TMultivector;
    function ToVerboseString(APrecision, ADigits: longint): string;
    function ToString: string;
  end;

  // TVectorHelper
  TVectorHelper = record helper for TVector
    function Dual: TTrivector;
    function Inverse: TVector;
    function Reverse: TVector;
    function Conjugate: TVector;
    function Reciprocal: TVector;
    function Normalized: TVector;
    function Norm: double;
    function SquaredNorm: double;

    function Dot(const AVector: TVector): double; overload;
    function Dot(const AVector: TBivector): TVector; overload;
    function Dot(const AVector: TTrivector): TBivector; overload;
    function Dot(const AVector: TQuadrivector): TTrivector; overload;
    function Dot(const AVector: TMultivector): TMultivector; overload;

    function Wedge(const AVector: TVector): TBivector; overload;
    function Wedge(const AVector: TBivector): TTrivector; overload;
    function Wedge(const AVector: TTrivector): TQuadrivector; overload;
    function Wedge(const AVector: TQuadrivector): double; overload;
    function Wedge(const AVector: TMultivector): TMultivector; overload;

    function Projection(const AVector: TVector): TMultivector; overload;
    function Projection(const AVector: TBivector): TMultivector; overload;
    function Projection(const AVector: TTrivector): TMultivector; overload;
    function Projection(const AVector: TQuadrivector): TMultivector; overload;
    function Projection(const AVector: TMultivector): TMultivector; overload;

    function Rejection(const AVector: TVector): TMultivector; overload;
    function Rejection(const AVector: TBivector): TMultivector; overload;
    function Rejection(const AVector: TTrivector): TMultivector; overload;
    function Rejection(const AVector: TQuadrivector): TMultivector; overload;
    function Rejection(const AVector: TMultivector): TMultivector; overload;

    function Reflection(const AVector: TVector): TMultivector; overload;
    function Reflection(const AVector: TBivector): TMultivector; overload;
    function Reflection(const AVector: TTrivector): TMultivector; overload;
    function Reflection(const AVector: TQuadrivector): TMultivector; overload;
    function Reflection(const AVector: TMultivector): TMultivector; overload;

    function Rotation(const AVector1, AVector2: TVector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TBivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TTrivector): TMultivector; overload;
    function Rotation(const AVector1, AVector2: TQuadrivector): TMultivector; overload;
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
  TVersor3 = record class operator *(const AValue: double; const ASelf: TVersor3): TVector; end;
  TVersor4 = record class operator *(const AValue: double; const ASelf: TVersor4): TVector; end;

  // TBiversor
  TBiversor12 = record class operator *(const AValue: double; const ASelf: TBiversor12): TBivector; end;
  TBiversor13 = record class operator *(const AValue: double; const ASelf: TBiversor13): TBivector; end;
  TBiversor14 = record class operator *(const AValue: double; const ASelf: TBiversor14): TBivector; end;
  TBiversor23 = record class operator *(const AValue: double; const ASelf: TBiversor23): TBivector; end;
  TBiversor24 = record class operator *(const AValue: double; const ASelf: TBiversor24): TBivector; end;
  TBiversor34 = record class operator *(const AValue: double; const ASelf: TBiversor34): TBivector; end;

  // TTriversor
  TTriversor123 = record class operator *(const AValue: double; const ASelf: TTriversor123): TTrivector; end;
  TTriversor124 = record class operator *(const AValue: double; const ASelf: TTriversor124): TTrivector; end;
  TTriversor134 = record class operator *(const AValue: double; const ASelf: TTriversor134): TTrivector; end;
  TTriversor234 = record class operator *(const AValue: double; const ASelf: TTriversor234): TTrivector; end;

  // TQuadriversor
  TQuadriversor1234 = record class operator *(const AValue: double; const ASelf: TQuadriversor1234): TQuadrivector; end;

const
  e1 : TVersor1 = ();
  e2 : TVersor2 = ();
  e3 : TVersor3 = ();
  e4 : TVersor3 = ();

  e12 : TBiversor12 = ();
  e13 : TBiversor13 = ();
  e14 : TBiversor14 = ();
  e23 : TBiversor23 = ();
  e24 : TBiversor24 = ();
  e34 : TBiversor34 = ();

  e123 : TTriversor123 = ();
  e124 : TTriversor124 = ();
  e134 : TTriversor134 = ();
  e234 : TTriversor234 = ();

  e1234 : TQuadriversor1234 = ();

  u1 : TVector = (fm1:1.0; fm2:0.0; fm3:0.0; fm4:0.0);
  u2 : TVector = (fm1:0.0; fm2:1.0; fm3:0.0; fm4:0.0);
  u3 : TVector = (fm1:0.0; fm2:0.0; fm3:1.0; fm4:0.0);
  u4 : TVector = (fm1:0.0; fm2:0.0; fm3:0.0; fm4:1.0);

  u12 : TBivector = (fm12:1.0; fm13:0.0; fm14:0.0; fm23:0.0; fm24:0.0; fm34:0.0);
  u13 : TBivector = (fm12:0.0; fm13:1.0; fm14:0.0; fm23:0.0; fm24:0.0; fm34:0.0);
  u14 : TBivector = (fm12:0.0; fm13:0.0; fm14:1.0; fm23:0.0; fm24:0.0; fm34:0.0);
  u23 : TBivector = (fm12:0.0; fm13:0.0; fm14:1.0; fm23:0.0; fm24:0.0; fm34:0.0);
  u24 : TBivector = (fm12:0.0; fm13:0.0; fm14:1.0; fm23:0.0; fm24:0.0; fm34:0.0);
  u34 : TBivector = (fm12:0.0; fm13:0.0; fm14:1.0; fm23:0.0; fm24:0.0; fm34:0.0);

  u123 : TTrivector = (fm123:1.0; fm124:0.0; fm134:0.0; fm234:0.0);
  u124 : TTrivector = (fm123:0.0; fm124:1.0; fm134:0.0; fm234:0.0);
  u134 : TTrivector = (fm123:0.0; fm124:0.0; fm134:1.0; fm234:0.0);
  u234 : TTrivector = (fm123:0.0; fm124:0.0; fm134:0.0; fm234:1.0);

  u1234 : TQuadrivector = (fm1234:1.0);

  NullMultivector  : TMultivector  = (fm0:0.0; fm1:0.0; fm2:0.0; fm3:0.0; fm4:0.0; fm12:0.0; fm13:0.0; fm14:0.0; fm23:0.0; fm24:0.0; fm34:0.0; fm123:0.0; fm124:0.0; fm134:0.0; fm234:0.0; fm1234:0.0);
  NullQuadrivector : TQuadrivector = (fm1234:0.0);
  NullTrivector    : TTrivector    = (fm123:0.0; fm124:0.0; fm134:0.0; fm234:0.0);
  NullBivector     : TBivector     = (fm12:0.0; fm13:0.0; fm14:0.0; fm23:0.0; fm24:0.0; fm34:0.0);
  NullVector       : TVector       = (fm1:0.0; fm2:0.0; fm3:0.0; fm4:0.0);
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
  result.fm0    := AValue;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := 0.0;
end;

class operator TMultivector.:=(const AValue: TMultivector): double;
begin
  result := AValue.fm0;
end;

class operator TMultivector.<>(const ALeft, ARight: TMultivector): boolean;
begin
  result := (ALeft.fm0    <> ARight.fm0   ) or
            (ALeft.fm1    <> ARight.fm1   ) or
            (ALeft.fm2    <> ARight.fm2   ) or
            (ALeft.fm3    <> ARight.fm3   ) or
            (ALeft.fm4    <> ARight.fm4   ) or
            (ALeft.fm12   <> ARight.fm12  ) or
            (ALeft.fm13   <> ARight.fm13  ) or
            (ALeft.fm14   <> ARight.fm14  ) or
            (ALeft.fm23   <> ARight.fm23  ) or
            (ALeft.fm24   <> ARight.fm24  ) or
            (ALeft.fm34   <> ARight.fm34  ) or
            (ALeft.fm123  <> ARight.fm123 ) or
            (ALeft.fm124  <> ARight.fm124 ) or
            (ALeft.fm134  <> ARight.fm134 ) or
            (ALeft.fm234  <> ARight.fm234 ) or
            (ALeft.fm1234 <> ARight.fm1234);
end;

class operator TMultivector.<>(const ALeft: TMultivector; const ARight: double): boolean;
begin
  result := (ALeft.fm0    <> ARight) or
            (ALeft.fm1    <>    0.0) or
            (ALeft.fm2    <>    0.0) or
            (ALeft.fm3    <>    0.0) or
            (ALeft.fm4    <>    0.0) or
            (ALeft.fm12   <>    0.0) or
            (ALeft.fm13   <>    0.0) or
            (ALeft.fm14   <>    0.0) or
            (ALeft.fm23   <>    0.0) or
            (ALeft.fm24   <>    0.0) or
            (ALeft.fm34   <>    0.0) or
            (ALeft.fm123  <>    0.0) or
            (ALeft.fm124  <>    0.0) or
            (ALeft.fm134  <>    0.0) or
            (ALeft.fm234  <>    0.0) or
            (ALeft.fm1234 <>    0.0);
end;

class operator TMultivector.<>(const ALeft: double; const ARight: TMultivector): boolean;
begin
  result := (ALeft <> ARight.fm0   ) or
            (0.0   <> ARight.fm1   ) or
            (0.0   <> ARight.fm2   ) or
            (0.0   <> ARight.fm3   ) or
            (0.0   <> ARight.fm4   ) or
            (0.0   <> ARight.fm12  ) or
            (0.0   <> ARight.fm13  ) or
            (0.0   <> ARight.fm14  ) or
            (0.0   <> ARight.fm23  ) or
            (0.0   <> ARight.fm24  ) or
            (0.0   <> ARight.fm34  ) or
            (0.0   <> ARight.fm123 ) or
            (0.0   <> ARight.fm124 ) or
            (0.0   <> ARight.fm134 ) or
            (0.0   <> ARight.fm234 ) or
            (0.0   <> ARight.fm1234);
end;

class operator TMultivector.=(const ALeft: TMultivector; const ARight: double): boolean;
begin
  result := (ALeft.fm0    = ARight) and
            (ALeft.fm1    =    0.0) and
            (ALeft.fm2    =    0.0) and
            (ALeft.fm3    =    0.0) and
            (ALeft.fm4    =    0.0) and
            (ALeft.fm12   =    0.0) and
            (ALeft.fm13   =    0.0) and
            (ALeft.fm14   =    0.0) and
            (ALeft.fm23   =    0.0) and
            (ALeft.fm24   =    0.0) and
            (ALeft.fm34   =    0.0) and
            (ALeft.fm123  =    0.0) and
            (ALeft.fm124  =    0.0) and
            (ALeft.fm134  =    0.0) and
            (ALeft.fm234  =    0.0) and
            (ALeft.fm1234 =    0.0);
end;

class operator TMultivector.=(const ALeft: double; const ARight: TMultivector): boolean;
begin
  result := (ALeft = ARight.fm0   ) and
            (0.0   = ARight.fm1   ) and
            (0.0   = ARight.fm2   ) and
            (0.0   = ARight.fm3   ) and
            (0.0   = ARight.fm4   ) and
            (0.0   = ARight.fm12  ) and
            (0.0   = ARight.fm13  ) and
            (0.0   = ARight.fm14  ) and
            (0.0   = ARight.fm23  ) and
            (0.0   = ARight.fm24  ) and
            (0.0   = ARight.fm34  ) and
            (0.0   = ARight.fm123 ) and
            (0.0   = ARight.fm124 ) and
            (0.0   = ARight.fm134 ) and
            (0.0   = ARight.fm234 ) and
            (0.0   = ARight.fm1234);
end;

class operator TMultivector.=(const ALeft, ARight: TMultivector): boolean;
begin
  result := (ALeft.fm0    = ARight.fm0   ) and
            (ALeft.fm1    = ARight.fm1   ) and
            (ALeft.fm2    = ARight.fm2   ) and
            (ALeft.fm3    = ARight.fm3   ) and
            (ALeft.fm4    = ARight.fm4   ) and
            (ALeft.fm12   = ARight.fm12  ) and
            (ALeft.fm13   = ARight.fm13  ) and
            (ALeft.fm14   = ARight.fm14  ) and
            (ALeft.fm23   = ARight.fm23  ) and
            (ALeft.fm24   = ARight.fm24  ) and
            (ALeft.fm34   = ARight.fm34  ) and
            (ALeft.fm123  = ARight.fm123 ) and
            (ALeft.fm124  = ARight.fm124 ) and
            (ALeft.fm134  = ARight.fm134 ) and
            (ALeft.fm234  = ARight.fm234 ) and
            (ALeft.fm1234 = ARight.fm1234);
end;

class operator TMultivector.+(const ALeft: TMultivector; const ARight: double): TMultivector;
begin
  result.fm0    := ALeft.fm0 + ARight;
  result.fm1    := ALeft.fm1;
  result.fm2    := ALeft.fm2;
  result.fm3    := ALeft.fm3;
  result.fm4    := ALeft.fm4;
  result.fm12   := ALeft.fm12;
  result.fm13   := ALeft.fm13;
  result.fm14   := ALeft.fm14;
  result.fm23   := ALeft.fm23;
  result.fm24   := ALeft.fm24;
  result.fm34   := ALeft.fm34;
  result.fm123  := ALeft.fm123;
  result.fm124  := ALeft.fm124;
  result.fm134  := ALeft.fm134;
  result.fm234  := ALeft.fm234;
  result.fm1234 := ALeft.fm1234;
end;

class operator TMultivector.+(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result.fm0    := ARight.fm0 + ALeft;
  result.fm1    := ARight.fm1;
  result.fm2    := ARight.fm2;
  result.fm3    := ARight.fm3;
  result.fm4    := ARight.fm4;
  result.fm12   := ARight.fm12;
  result.fm13   := ARight.fm13;
  result.fm14   := ARight.fm14;
  result.fm23   := ARight.fm23;
  result.fm24   := ARight.fm24;
  result.fm34   := ARight.fm34;
  result.fm123  := ARight.fm123;
  result.fm124  := ARight.fm124;
  result.fm134  := ARight.fm134;
  result.fm234  := ARight.fm234;
  result.fm1234 := ARight.fm1234;
end;

class operator TMultivector.+(const ALeft, ARight: TMultivector): TMultivector;
begin
  result.fm0    := ALeft.fm0    + ARight.fm0;
  result.fm1    := ALeft.fm1    + ARight.fm1;
  result.fm2    := ALeft.fm2    + ARight.fm2;
  result.fm3    := ALeft.fm3    + ARight.fm3;
  result.fm4    := ALeft.fm4    + ARight.fm4;
  result.fm12   := ALeft.fm12   + ARight.fm12;
  result.fm13   := ALeft.fm13   + ARight.fm13;
  result.fm14   := ALeft.fm14   + ARight.fm14;
  result.fm23   := ALeft.fm23   + ARight.fm23;
  result.fm24   := ALeft.fm24   + ARight.fm24;
  result.fm34   := ALeft.fm34   + ARight.fm34;
  result.fm123  := ALeft.fm123  + ARight.fm123;
  result.fm124  := ALeft.fm124  + ARight.fm124;
  result.fm134  := ALeft.fm134  + ARight.fm134;
  result.fm234  := ALeft.fm234  + ARight.fm234;
  result.fm1234 := ALeft.fm1234 + ARight.fm1234;
end;

class operator TMultivector.-(const ASelf: TMultivector): TMultivector;
begin
  result.fm0    := -ASelf.fm0;
  result.fm1    := -ASelf.fm1;
  result.fm2    := -ASelf.fm2;
  result.fm3    := -ASelf.fm3;
  result.fm4    := -ASelf.fm4;
  result.fm12   := -ASelf.fm12;
  result.fm13   := -ASelf.fm13;
  result.fm14   := -ASelf.fm14;
  result.fm23   := -ASelf.fm23;
  result.fm24   := -ASelf.fm24;
  result.fm34   := -ASelf.fm34;
  result.fm123  := -ASelf.fm123;
  result.fm124  := -ASelf.fm124;
  result.fm134  := -ASelf.fm134;
  result.fm234  := -ASelf.fm234;
  result.fm1234 := -ASelf.fm1234;
end;

class operator TMultivector.-(const ALeft: TMultivector; const ARight: double): TMultivector;
begin
  result.fm0    := ALeft.fm0 - ARight;
  result.fm1    := ALeft.fm1;
  result.fm2    := ALeft.fm2;
  result.fm3    := ALeft.fm3;
  result.fm4    := ALeft.fm4;
  result.fm12   := ALeft.fm12;
  result.fm13   := ALeft.fm13;
  result.fm14   := ALeft.fm14;
  result.fm23   := ALeft.fm23;
  result.fm24   := ALeft.fm24;
  result.fm34   := ALeft.fm34;
  result.fm123  := ALeft.fm123;
  result.fm124  := ALeft.fm124;
  result.fm134  := ALeft.fm134;
  result.fm234  := ALeft.fm234;
  result.fm1234 := ALeft.fm1234;
end;

class operator TMultivector.-(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result.fm0    := ALeft - ARight.fm0;
  result.fm1    :=       - ARight.fm1;
  result.fm2    :=       - ARight.fm2;
  result.fm3    :=       - ARight.fm3;
  result.fm4    :=       - ARight.fm4;
  result.fm12   :=       - ARight.fm12;
  result.fm13   :=       - ARight.fm13;
  result.fm14   :=       - ARight.fm14;
  result.fm23   :=       - ARight.fm23;
  result.fm24   :=       - ARight.fm24;
  result.fm34   :=       - ARight.fm34;
  result.fm123  :=       - ARight.fm123;
  result.fm124  :=       - ARight.fm124;
  result.fm134  :=       - ARight.fm134;
  result.fm234  :=       - ARight.fm234;
  result.fm1234 :=       - ARight.fm1234;
end;

class operator TMultivector.-(const ALeft, ARight: TMultivector): TMultivector;
begin
  result.fm0    := ALeft.fm0    - ARight.fm0;
  result.fm1    := ALeft.fm1    - ARight.fm1;
  result.fm2    := ALeft.fm2    - ARight.fm2;
  result.fm3    := ALeft.fm3    - ARight.fm3;
  result.fm4    := ALeft.fm4    - ARight.fm4;
  result.fm12   := ALeft.fm12   - ARight.fm12;
  result.fm13   := ALeft.fm13   - ARight.fm13;
  result.fm14   := ALeft.fm14   - ARight.fm14;
  result.fm23   := ALeft.fm23   - ARight.fm23;
  result.fm24   := ALeft.fm24   - ARight.fm24;
  result.fm34   := ALeft.fm34   - ARight.fm34;
  result.fm123  := ALeft.fm123  - ARight.fm123;
  result.fm124  := ALeft.fm124  - ARight.fm124;
  result.fm134  := ALeft.fm134  - ARight.fm134;
  result.fm234  := ALeft.fm234  - ARight.fm234;
  result.fm1234 := ALeft.fm1234 - ARight.fm1234;
end;

class operator TMultivector.*(const ALeft: TMultivector; const ARight: double): TMultivector;
begin
  result.fm0    := ALeft.fm0    * ARight;
  result.fm1    := ALeft.fm1    * ARight;
  result.fm2    := ALeft.fm2    * ARight;
  result.fm3    := ALeft.fm3    * ARight;
  result.fm4    := ALeft.fm4    * ARight;
  result.fm12   := ALeft.fm12   * ARight;
  result.fm13   := ALeft.fm13   * ARight;
  result.fm14   := ALeft.fm14   * ARight;
  result.fm23   := ALeft.fm23   * ARight;
  result.fm24   := ALeft.fm24   * ARight;
  result.fm34   := ALeft.fm34   * ARight;
  result.fm123  := ALeft.fm123  * ARight;
  result.fm124  := ALeft.fm124  * ARight;
  result.fm134  := ALeft.fm134  * ARight;
  result.fm234  := ALeft.fm234  * ARight;
  result.fm1234 := ALeft.fm1234 * ARight;
end;

class operator TMultivector.*(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result.fm0    := ALeft * ARight.fm0;
  result.fm1    := ALeft * ARight.fm1;
  result.fm2    := ALeft * ARight.fm2;
  result.fm3    := ALeft * ARight.fm3;
  result.fm4    := ALeft * ARight.fm4;
  result.fm12   := ALeft * ARight.fm12;
  result.fm13   := ALeft * ARight.fm13;
  result.fm14   := ALeft * ARight.fm14;
  result.fm23   := ALeft * ARight.fm23;
  result.fm24   := ALeft * ARight.fm24;
  result.fm34   := ALeft * ARight.fm34;
  result.fm123  := ALeft * ARight.fm123;
  result.fm124  := ALeft * ARight.fm124;
  result.fm134  := ALeft * ARight.fm134;
  result.fm234  := ALeft * ARight.fm234;
  result.fm1234 := ALeft * ARight.fm1234;
end;

class operator TMultivector.*(const ALeft, ARight: TMultivector): TMultivector;
begin
  result.fm0 :=     ALeft.fm0    * ARight.fm0
                   +ALeft.fm1    * ARight.fm1
                   +ALeft.fm2    * ARight.fm2
                   +ALeft.fm3    * ARight.fm3
                   +ALeft.fm4    * ARight.fm4
                   -ALeft.fm12   * ARight.fm12
                   -ALeft.fm13   * ARight.fm13
                   -ALeft.fm14   * ARight.fm14
                   -ALeft.fm23   * ARight.fm23
                   -ALeft.fm24   * ARight.fm24
                   -ALeft.fm34   * ARight.fm34
                   -ALeft.fm123  * ARight.fm123
                   -ALeft.fm124  * ARight.fm124
                   -ALeft.fm134  * ARight.fm134
                   -ALeft.fm234  * ARight.fm234
                   +ALeft.fm1234 * ARight.fm1234;

  result.fm1 :=     ALeft.fm0    * ARight.fm1
                   +ALeft.fm1    * ARight.fm0
                   -ALeft.fm2    * ARight.fm12
                   -ALeft.fm3    * ARight.fm13
                   -ALeft.fm4    * ARight.fm14
                   +ALeft.fm12   * ARight.fm2
                   +ALeft.fm13   * ARight.fm3
                   +ALeft.fm14   * ARight.fm4
                   -ALeft.fm23   * ARight.fm123
                   -ALeft.fm24   * ARight.fm124
                   -ALeft.fm34   * ARight.fm134
                   -ALeft.fm123  * ARight.fm23
                   -ALeft.fm124  * ARight.fm24
                   -ALeft.fm134  * ARight.fm34
                   +ALeft.fm234  * ARight.fm1234
                   -ALeft.fm1234 * ARight.fm234;

  result.fm2 :=     ALeft.fm0    * ARight.fm2
                   +ALeft.fm1    * ARight.fm12
                   +ALeft.fm2    * ARight.fm0
                   -ALeft.fm3    * ARight.fm23
                   -ALeft.fm4    * ARight.fm24
                   -ALeft.fm12   * ARight.fm1
                   +ALeft.fm13   * ARight.fm123
                   +ALeft.fm14   * ARight.fm124
                   +ALeft.fm23   * ARight.fm3
                   +ALeft.fm24   * ARight.fm4
                   -ALeft.fm34   * ARight.fm234
                   +ALeft.fm123  * ARight.fm13
                   +ALeft.fm124  * ARight.fm14
                   -ALeft.fm134  * ARight.fm1234
                   -ALeft.fm234  * ARight.fm34
                   +ALeft.fm1234 * ARight.fm134;

  result.fm3 :=     ALeft.fm0    * ARight.fm3
                   +ALeft.fm1    * ARight.fm13
                   +ALeft.fm2    * ARight.fm23
                   +ALeft.fm3    * ARight.fm0
                   -ALeft.fm4    * ARight.fm34
                   -ALeft.fm12   * ARight.fm123
                   -ALeft.fm13   * ARight.fm1
                   +ALeft.fm14   * ARight.fm134
                   -ALeft.fm23   * ARight.fm2
                   +ALeft.fm24   * ARight.fm234
                   +ALeft.fm34   * ARight.fm4
                   -ALeft.fm123  * ARight.fm12
                   +ALeft.fm124  * ARight.fm1234
                   +ALeft.fm134  * ARight.fm14
                   +ALeft.fm234  * ARight.fm24
                   -ALeft.fm1234 * ARight.fm124;

  result.fm4 :=     ALeft.fm0    * ARight.fm4
                   +ALeft.fm1    * ARight.fm14
                   +ALeft.fm2    * ARight.fm24
                   +ALeft.fm3    * ARight.fm34
                   +ALeft.fm4    * ARight.fm0
                   -ALeft.fm12   * ARight.fm124
                   -ALeft.fm13   * ARight.fm134
                   -ALeft.fm14   * ARight.fm1
                   -ALeft.fm23   * ARight.fm234
                   -ALeft.fm24   * ARight.fm2
                   -ALeft.fm34   * ARight.fm3
                   -ALeft.fm123  * ARight.fm1234
                   -ALeft.fm124  * ARight.fm12
                   -ALeft.fm134  * ARight.fm13
                   -ALeft.fm234  * ARight.fm23
                   +ALeft.fm1234 * ARight.fm123;

  result.fm12 :=    ALeft.fm0    * ARight.fm12
                   +ALeft.fm1    * ARight.fm2
                   -ALeft.fm2    * ARight.fm1
                   +ALeft.fm3    * ARight.fm123
                   +ALeft.fm4    * ARight.fm124
                   +ALeft.fm12   * ARight.fm0
                   -ALeft.fm13   * ARight.fm23
                   -ALeft.fm14   * ARight.fm24
                   +ALeft.fm23   * ARight.fm13
                   +ALeft.fm24   * ARight.fm14
                   -ALeft.fm34   * ARight.fm1234
                   +ALeft.fm123  * ARight.fm3
                   +ALeft.fm124  * ARight.fm4
                   -ALeft.fm134  * ARight.fm234
                   +ALeft.fm234  * ARight.fm134
                   -ALeft.fm1234 * ARight.fm34;

  result.fm13 :=    ALeft.fm0    * ARight.fm13
                   +ALeft.fm1    * ARight.fm3
                   -ALeft.fm2    * ARight.fm123
                   -ALeft.fm3    * ARight.fm1
                   +ALeft.fm4    * ARight.fm134
                   +ALeft.fm12   * ARight.fm23
                   +ALeft.fm13   * ARight.fm0
                   -ALeft.fm14   * ARight.fm34
                   -ALeft.fm23   * ARight.fm12
                   +ALeft.fm24   * ARight.fm1234
                   +ALeft.fm34   * ARight.fm14
                   -ALeft.fm123  * ARight.fm2
                   +ALeft.fm124  * ARight.fm234
                   +ALeft.fm134  * ARight.fm4
                   -ALeft.fm234  * ARight.fm124
                   +ALeft.fm1234 * ARight.fm24;

  result.fm14 :=    ALeft.fm0    * ARight.fm14
                   +ALeft.fm1    * ARight.fm4
                   -ALeft.fm2    * ARight.fm124
                   -ALeft.fm3    * ARight.fm134
                   -ALeft.fm4    * ARight.fm1
                   +ALeft.fm12   * ARight.fm24
                   +ALeft.fm13   * ARight.fm34
                   +ALeft.fm14   * ARight.fm0
                   -ALeft.fm23   * ARight.fm1234
                   -ALeft.fm24   * ARight.fm12
                   -ALeft.fm34   * ARight.fm13
                   -ALeft.fm123  * ARight.fm234
                   -ALeft.fm124  * ARight.fm2
                   -ALeft.fm134  * ARight.fm3
                   +ALeft.fm234  * ARight.fm123
                   -ALeft.fm1234 * ARight.fm23;

  result.fm23 :=    ALeft.fm0    * ARight.fm23
                   +ALeft.fm1    * ARight.fm123
                   +ALeft.fm2    * ARight.fm3
                   -ALeft.fm3    * ARight.fm2
                   +ALeft.fm4    * ARight.fm234
                   -ALeft.fm12   * ARight.fm13
                   +ALeft.fm13   * ARight.fm12
                   -ALeft.fm14   * ARight.fm1234
                   +ALeft.fm23   * ARight.fm0
                   -ALeft.fm24   * ARight.fm34
                   +ALeft.fm34   * ARight.fm24
                   +ALeft.fm123  * ARight.fm1
                   -ALeft.fm124  * ARight.fm134
                   +ALeft.fm134  * ARight.fm124
                   +ALeft.fm234  * ARight.fm4
                   -ALeft.fm1234 * ARight.fm14;

  result.fm24 :=    ALeft.fm0    * ARight.fm24
                   +ALeft.fm1    * ARight.fm124
                   +ALeft.fm2    * ARight.fm4
                   -ALeft.fm3    * ARight.fm234
                   -ALeft.fm4    * ARight.fm2
                   -ALeft.fm12   * ARight.fm14
                   +ALeft.fm13   * ARight.fm1234
                   +ALeft.fm14   * ARight.fm12
                   +ALeft.fm23   * ARight.fm34
                   +ALeft.fm24   * ARight.fm0
                   -ALeft.fm34   * ARight.fm23
                   +ALeft.fm123  * ARight.fm134
                   +ALeft.fm124  * ARight.fm1
                   -ALeft.fm134  * ARight.fm123
                   -ALeft.fm234  * ARight.fm3
                   +ALeft.fm1234 * ARight.fm13;

  result.fm34 :=    ALeft.fm0    * ARight.fm34
                   +ALeft.fm1    * ARight.fm134
                   +ALeft.fm2    * ARight.fm234
                   +ALeft.fm3    * ARight.fm4
                   -ALeft.fm4    * ARight.fm3
                   -ALeft.fm12   * ARight.fm1234
                   -ALeft.fm13   * ARight.fm14
                   +ALeft.fm14   * ARight.fm13
                   -ALeft.fm23   * ARight.fm24
                   +ALeft.fm24   * ARight.fm23
                   +ALeft.fm34   * ARight.fm0
                   -ALeft.fm123  * ARight.fm124
                   +ALeft.fm124  * ARight.fm123
                   +ALeft.fm134  * ARight.fm1
                   +ALeft.fm234  * ARight.fm2
                   -ALeft.fm1234 * ARight.fm12;

  result.fm123 :=   ALeft.fm0    * ARight.fm123
                   +ALeft.fm1    * ARight.fm23
                   -ALeft.fm2    * ARight.fm13
                   +ALeft.fm3    * ARight.fm12
                   -ALeft.fm4    * ARight.fm1234
                   +ALeft.fm12   * ARight.fm3
                   -ALeft.fm13   * ARight.fm2
                   +ALeft.fm14   * ARight.fm234
                   +ALeft.fm23   * ARight.fm1
                   -ALeft.fm24   * ARight.fm134
                   +ALeft.fm34   * ARight.fm124
                   +ALeft.fm123  * ARight.fm0
                   -ALeft.fm124  * ARight.fm34
                   +ALeft.fm134  * ARight.fm24
                   -ALeft.fm234  * ARight.fm14
                   +ALeft.fm1234 * ARight.fm4;

  result.fm124 :=   ALeft.fm0    * ARight.fm124
                   +ALeft.fm1    * ARight.fm24
                   -ALeft.fm2    * ARight.fm14
                   +ALeft.fm3    * ARight.fm1234
                   +ALeft.fm4    * ARight.fm12
                   +ALeft.fm12   * ARight.fm4
                   -ALeft.fm13   * ARight.fm234
                   -ALeft.fm14   * ARight.fm2
                   +ALeft.fm23   * ARight.fm134
                   +ALeft.fm24   * ARight.fm1
                   -ALeft.fm34   * ARight.fm123
                   +ALeft.fm123  * ARight.fm34
                   +ALeft.fm124  * ARight.fm0
                   -ALeft.fm134  * ARight.fm23
                   +ALeft.fm234  * ARight.fm13
                   -ALeft.fm1234 * ARight.fm3;

  result.fm134 :=   ALeft.fm0    * ARight.fm134
                   +ALeft.fm1    * ARight.fm34
                   -ALeft.fm2    * ARight.fm1234
                   -ALeft.fm3    * ARight.fm14
                   +ALeft.fm4    * ARight.fm13
                   +ALeft.fm12   * ARight.fm234
                   +ALeft.fm13   * ARight.fm4
                   -ALeft.fm14   * ARight.fm3
                   -ALeft.fm23   * ARight.fm124
                   +ALeft.fm24   * ARight.fm123
                   +ALeft.fm34   * ARight.fm1
                   -ALeft.fm123  * ARight.fm24
                   +ALeft.fm124  * ARight.fm23
                   +ALeft.fm134  * ARight.fm0
                   -ALeft.fm234  * ARight.fm12
                   +ALeft.fm1234 * ARight.fm2;

  result.fm234 :=   ALeft.fm0    * ARight.fm234
                   +ALeft.fm1    * ARight.fm1234
                   +ALeft.fm2    * ARight.fm34
                   -ALeft.fm3    * ARight.fm24
                   +ALeft.fm4    * ARight.fm23
                   -ALeft.fm12   * ARight.fm134
                   +ALeft.fm13   * ARight.fm124
                   -ALeft.fm14   * ARight.fm123
                   +ALeft.fm23   * ARight.fm4
                   -ALeft.fm24   * ARight.fm3
                   +ALeft.fm34   * ARight.fm2
                   +ALeft.fm123  * ARight.fm14
                   -ALeft.fm124  * ARight.fm13
                   +ALeft.fm134  * ARight.fm12
                   +ALeft.fm234  * ARight.fm0
                   -ALeft.fm1234 * ARight.fm1;

  result.fm1234 :=  ALeft.fm0    * ARight.fm1234
                   +ALeft.fm1    * ARight.fm234
                   -ALeft.fm2    * ARight.fm134
                   +ALeft.fm3    * ARight.fm124
                   -ALeft.fm4    * ARight.fm123
                   +ALeft.fm12   * ARight.fm34
                   -ALeft.fm13   * ARight.fm24
                   +ALeft.fm14   * ARight.fm23
                   +ALeft.fm23   * ARight.fm14
                   -ALeft.fm24   * ARight.fm13
                   +ALeft.fm34   * ARight.fm12
                   +ALeft.fm123  * ARight.fm4
                   -ALeft.fm124  * ARight.fm3
                   +ALeft.fm134  * ARight.fm2
                   -ALeft.fm234  * ARight.fm1
                   +ALeft.fm1234 * ARight.fm0;
end;

class operator TMultivector./(const ALeft: TMultivector; const ARight: double): TMultivector;
begin
  result.fm0    := ALeft.fm0    / ARight;
  result.fm1    := ALeft.fm1    / ARight;
  result.fm2    := ALeft.fm2    / ARight;
  result.fm3    := ALeft.fm3    / ARight;
  result.fm4    := ALeft.fm4    / ARight;
  result.fm12   := ALeft.fm12   / ARight;
  result.fm13   := ALeft.fm13   / ARight;
  result.fm14   := ALeft.fm14   / ARight;
  result.fm23   := ALeft.fm23   / ARight;
  result.fm24   := ALeft.fm24   / ARight;
  result.fm34   := ALeft.fm34   / ARight;
  result.fm123  := ALeft.fm123  / ARight;
  result.fm124  := ALeft.fm124  / ARight;
  result.fm134  := ALeft.fm134  / ARight;
  result.fm234  := ALeft.fm234  / ARight;
  result.fm1234 := ALeft.fm1234 / ARight;
end;

class operator TMultivector./(const ALeft: double; const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TMultivector./(const ALeft, ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

// TQuadrivector

class operator TQuadrivector.:=(const AValue: TQuadrivector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := AValue.fm1234;
end;

class operator TQuadrivector.:=(const AValue: TMultivector): TQuadrivector;
begin
  result.fm1234 := AValue.fm1234;
end;

class operator TQuadrivector.<>(const ALeft, ARight: TQuadrivector): boolean;
begin
  result := ALeft.fm1234 <> ARight.fm1234;
end;

class operator TQuadrivector.<>(const ALeft: TMultivector; const ARight: TQuadrivector): boolean;
begin
  result := (ALeft.fm0    <>           0.0) or
            (ALeft.fm1    <>           0.0) or
            (ALeft.fm2    <>           0.0) or
            (ALeft.fm3    <>           0.0) or
            (ALeft.fm4    <>           0.0) or
            (ALeft.fm12   <>           0.0) or
            (ALeft.fm13   <>           0.0) or
            (ALeft.fm14   <>           0.0) or
            (ALeft.fm23   <>           0.0) or
            (ALeft.fm24   <>           0.0) or
            (ALeft.fm34   <>           0.0) or
            (ALeft.fm123  <>           0.0) or
            (ALeft.fm124  <>           0.0) or
            (ALeft.fm134  <>           0.0) or
            (ALeft.fm234  <>           0.0) or
            (ALeft.fm1234 <> ARight.fm1234);
end;

class operator TQuadrivector.<>(const ALeft: TQuadrivector; const ARight: TMultivector): boolean;
begin
  result := (0.0          <> ARight.fm0   ) or
            (0.0          <> ARight.fm1   ) or
            (0.0          <> ARight.fm2   ) or
            (0.0          <> ARight.fm3   ) or
            (0.0          <> ARight.fm4   ) or
            (0.0          <> ARight.fm12  ) or
            (0.0          <> ARight.fm13  ) or
            (0.0          <> ARight.fm14  ) or
            (0.0          <> ARight.fm23  ) or
            (0.0          <> ARight.fm24  ) or
            (0.0          <> ARight.fm34  ) or
            (0.0          <> ARight.fm123 ) or
            (0.0          <> ARight.fm124 ) or
            (0.0          <> ARight.fm134 ) or
            (0.0          <> ARight.fm234 ) or
            (ALeft.fm1234 <> ARight.fm1234);
end;

class operator TQuadrivector.=(const ALeft: TMultivector; const ARight: TQuadrivector): boolean;
begin
  result := (0.0           = ALeft.fm0   ) or
            (0.0           = ALeft.fm1   ) or
            (0.0           = ALeft.fm2   ) or
            (0.0           = ALeft.fm3   ) or
            (0.0           = ALeft.fm4   ) or
            (0.0           = ALeft.fm12  ) or
            (0.0           = ALeft.fm13  ) or
            (0.0           = ALeft.fm14  ) or
            (0.0           = ALeft.fm23  ) or
            (0.0           = ALeft.fm24  ) or
            (0.0           = ALeft.fm34  ) or
            (0.0           = ALeft.fm123 ) or
            (0.0           = ALeft.fm124 ) or
            (0.0           = ALeft.fm134 ) or
            (0.0           = ALeft.fm234 ) or
            (ARight.fm1234 = ALeft.fm1234);
end;

class operator TQuadrivector.=(const ALeft: TQuadrivector; const ARight: TMultivector): boolean;
begin
  result := (0.0          = ARight.fm0   ) or
            (0.0          = ARight.fm1   ) or
            (0.0          = ARight.fm2   ) or
            (0.0          = ARight.fm3   ) or
            (0.0          = ARight.fm4   ) or
            (0.0          = ARight.fm12  ) or
            (0.0          = ARight.fm13  ) or
            (0.0          = ARight.fm14  ) or
            (0.0          = ARight.fm23  ) or
            (0.0          = ARight.fm24  ) or
            (0.0          = ARight.fm34  ) or
            (0.0          = ARight.fm123 ) or
            (0.0          = ARight.fm124 ) or
            (0.0          = ARight.fm134 ) or
            (0.0          = ARight.fm234 ) or
            (ALeft.fm1234 = ARight.fm1234);
end;

class operator TQuadrivector.=(const ALeft, ARight: TQuadrivector): boolean;
begin
  result := ALeft.fm1234 = ARight.fm1234;
end;

class operator TQuadrivector.+(const ALeft, ARight: TQuadrivector): TQuadrivector;
begin
  result.fm1234 := ALeft.fm1234 + ARight.fm1234;
end;

class operator TQuadrivector.+(const ALeft: TQuadrivector; const ARight: double): TMultivector;
begin
  result.fm0    := ARight;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := ALeft.fm1234;
end;

class operator TQuadrivector.+(const ALeft: double; const ARight: TQuadrivector): TMultivector;
begin
  result.fm0    := ALeft;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := ARight.fm1234;
end;

class operator TQuadrivector.+(const ALeft: TMultivector; const ARight: TQuadrivector): TMultivector;
begin
  result.fm0    := ALeft.fm0;
  result.fm1    := ALeft.fm1;
  result.fm2    := ALeft.fm2;
  result.fm3    := ALeft.fm3;
  result.fm4    := ALeft.fm4;
  result.fm12   := ALeft.fm12;
  result.fm13   := ALeft.fm13;
  result.fm14   := ALeft.fm14;
  result.fm23   := ALeft.fm23;
  result.fm24   := ALeft.fm24;
  result.fm34   := ALeft.fm34;
  result.fm123  := ALeft.fm123;
  result.fm124  := ALeft.fm124;
  result.fm134  := ALeft.fm134;
  result.fm234  := ALeft.fm234;
  result.fm1234 := ALeft.fm1234 + ARight.fm1234;
end;

class operator TQuadrivector.+(const ALeft: TQuadrivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0    := ARight.fm0;
  result.fm1    := ARight.fm1;
  result.fm2    := ARight.fm2;
  result.fm3    := ARight.fm3;
  result.fm4    := ARight.fm4;
  result.fm12   := ARight.fm12;
  result.fm13   := ARight.fm13;
  result.fm14   := ARight.fm14;
  result.fm23   := ARight.fm23;
  result.fm24   := ARight.fm24;
  result.fm34   := ARight.fm34;
  result.fm123  := ARight.fm123;
  result.fm124  := ARight.fm124;
  result.fm134  := ARight.fm134;
  result.fm234  := ARight.fm234;
  result.fm1234 := ARight.fm1234 + ALeft.fm1234;
end;

class operator TQuadrivector.-(const ASelf: TQuadrivector): TQuadrivector;
begin
  result.fm1234 := -ASelf.fm1234;
end;

class operator TQuadrivector.-(const ALeft, ARight: TQuadrivector): TQuadrivector;
begin
  result.fm1234 := ALeft.fm1234 - ARight.fm1234;
end;

class operator TQuadrivector.-(const ALeft: TQuadrivector; const ARight: double): TMultivector;
begin
  result.fm0    := -ARight;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  ALeft.fm1234;
end;

class operator TQuadrivector.-(const ALeft: double; const ARight: TQuadrivector): TMultivector;
begin
  result.fm0    :=  ALeft;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 := -ARight.fm1234;
end;

class operator TQuadrivector.-(const ALeft: TMultivector; const ARight: TQuadrivector): TMultivector;
begin
  result.fm0    := ALeft.fm0;
  result.fm1    := ALeft.fm1;
  result.fm2    := ALeft.fm2;
  result.fm3    := ALeft.fm3;
  result.fm4    := ALeft.fm4;
  result.fm12   := ALeft.fm12;
  result.fm13   := ALeft.fm13;
  result.fm14   := ALeft.fm14;
  result.fm23   := ALeft.fm23;
  result.fm24   := ALeft.fm24;
  result.fm34   := ALeft.fm34;
  result.fm123  := ALeft.fm123;
  result.fm124  := ALeft.fm124;
  result.fm134  := ALeft.fm134;
  result.fm234  := ALeft.fm234;
  result.fm1234 := ALeft.fm1234 - ARight.fm1234;
end;

class operator TQuadrivector.-(const ALeft: TQuadrivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0    := -ARight.fm0;
  result.fm1    := -ARight.fm1;
  result.fm2    := -ARight.fm2;
  result.fm3    := -ARight.fm3;
  result.fm4    := -ARight.fm4;
  result.fm12   := -ARight.fm12;
  result.fm13   := -ARight.fm13;
  result.fm14   := -ARight.fm14;
  result.fm23   := -ARight.fm23;
  result.fm24   := -ARight.fm24;
  result.fm34   := -ARight.fm34;
  result.fm123  := -ARight.fm123;
  result.fm124  := -ARight.fm124;
  result.fm134  := -ARight.fm134;
  result.fm234  := -ARight.fm234;
  result.fm1234 :=  ALeft.fm1234 - ARight.fm1234;
end;

class operator TQuadrivector.*(const ALeft: double; const ARight: TQuadrivector): TQuadrivector;
begin
  result.fm1234 := ALeft * ARight.fm1234;
end;

class operator TQuadrivector.*(const ALeft: TQuadrivector; const ARight: double): TQuadrivector;
begin
  result.fm1234 := ALeft.fm1234 * ARight;
end;

class operator TQuadrivector.*(const ALeft, ARight: TQuadrivector): double;
begin
  result := ALeft.fm1234 * ARight.fm1234;
end;

class operator TQuadrivector.*(const ALeft: TMultivector; const ARight: TQuadrivector): TMultivector;
begin
  result.fm0    :=  ALeft.fm1234 * ARight.fm1234;
  result.fm1    :=  ALeft.fm234  * ARight.fm1234;
  result.fm2    := -ALeft.fm134  * ARight.fm1234;
  result.fm3    :=  ALeft.fm124  * ARight.fm1234;
  result.fm4    := -ALeft.fm123  * ARight.fm1234;
  result.fm12   := -ALeft.fm34   * ARight.fm1234;
  result.fm13   :=  ALeft.fm24   * ARight.fm1234;
  result.fm14   := -ALeft.fm23   * ARight.fm1234;
  result.fm23   := -ALeft.fm14   * ARight.fm1234;
  result.fm24   :=  ALeft.fm13   * ARight.fm1234;
  result.fm34   := -ALeft.fm12   * ARight.fm1234;
  result.fm123  := -ALeft.fm4    * ARight.fm1234;
  result.fm124  :=  ALeft.fm3    * ARight.fm1234;
  result.fm134  := -ALeft.fm2    * ARight.fm1234;
  result.fm234  :=  ALeft.fm1    * ARight.fm1234;
  result.fm1234 :=  ALeft.fm0    * ARight.fm1234;
end;

class operator TQuadrivector.*(const ALeft: TQuadrivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0    :=  ALeft.fm1234 * ARight.fm1234;
  result.fm1    := -ALeft.fm1234 * ARight.fm234;
  result.fm2    :=  ALeft.fm1234 * ARight.fm134;
  result.fm3    := -ALeft.fm1234 * ARight.fm124;
  result.fm4    :=  ALeft.fm1234 * ARight.fm123;
  result.fm12   := -ALeft.fm1234 * ARight.fm34;
  result.fm13   :=  ALeft.fm1234 * ARight.fm24;
  result.fm14   := -ALeft.fm1234 * ARight.fm23;
  result.fm23   := -ALeft.fm1234 * ARight.fm14;
  result.fm24   :=  ALeft.fm1234 * ARight.fm13;
  result.fm34   := -ALeft.fm1234 * ARight.fm12;
  result.fm123  :=  ALeft.fm1234 * ARight.fm4;
  result.fm124  := -ALeft.fm1234 * ARight.fm3;
  result.fm134  :=  ALeft.fm1234 * ARight.fm2;
  result.fm234  := -ALeft.fm1234 * ARight.fm1;
  result.fm1234 :=  ALeft.fm1234 * ARight.fm0;
end;

class operator TQuadrivector./(const ALeft, ARight: TQuadrivector): double;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TQuadrivector./(const ALeft: TQuadrivector; const ARight: double): TQuadrivector;
begin
  result.fm1234 := ALeft.fm1234 / ARight;
end;

class operator TQuadrivector./(const ALeft: double; const ARight: TQuadrivector): TQuadrivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TQuadrivector./(const ALeft: TMultivector; const ARight: TQuadrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TQuadrivector./(const ALeft: TQuadrivector; const ARight: TMultivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

// TTrivector

class operator TTrivector.:=(const AValue: TTrivector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := AValue.fm123;
  result.fm124  := AValue.fm124;
  result.fm134  := AValue.fm134;
  result.fm234  := AValue.fm234;
  result.fm1234 := 0.0;
end;

class operator TTrivector.:=(const AValue: TMultivector): TTrivector;
begin
  result.fm123 := AValue.fm123;
  result.fm124 := AValue.fm124;
  result.fm134 := AValue.fm134;
  result.fm234 := AValue.fm234;
end;

class operator TTrivector.<>(const ALeft, ARight: TTrivector): boolean;
begin
  result := ALeft.fm123 <> ARight.fm123;
  result := ALeft.fm124 <> ARight.fm124;
  result := ALeft.fm134 <> ARight.fm134;
  result := ALeft.fm234 <> ARight.fm234;
end;

class operator TTrivector.<>(const ALeft: TMultivector; const ARight: TTrivector): boolean;
begin
  result := (ALeft.fm0    <>          0.0) or
            (ALeft.fm1    <>          0.0) or
            (ALeft.fm2    <>          0.0) or
            (ALeft.fm3    <>          0.0) or
            (ALeft.fm4    <>          0.0) or
            (ALeft.fm12   <>          0.0) or
            (ALeft.fm13   <>          0.0) or
            (ALeft.fm14   <>          0.0) or
            (ALeft.fm23   <>          0.0) or
            (ALeft.fm24   <>          0.0) or
            (ALeft.fm34   <>          0.0) or
            (ALeft.fm123  <> ARight.fm123) or
            (ALeft.fm124  <> ARight.fm124) or
            (ALeft.fm134  <> ARight.fm134) or
            (ALeft.fm234  <> ARight.fm234) or
            (ALeft.fm1234 <>          0.0);
end;

class operator TTrivector.<>(const ALeft: TTrivector; const ARight: TMultivector): boolean;
begin
  result := (0.0         <> ARight.fm0   ) or
            (0.0         <> ARight.fm1   ) or
            (0.0         <> ARight.fm2   ) or
            (0.0         <> ARight.fm3   ) or
            (0.0         <> ARight.fm4   ) or
            (0.0         <> ARight.fm12  ) or
            (0.0         <> ARight.fm13  ) or
            (0.0         <> ARight.fm14  ) or
            (0.0         <> ARight.fm23  ) or
            (0.0         <> ARight.fm24  ) or
            (0.0         <> ARight.fm34  ) or
            (ALeft.fm123 <> ARight.fm123 ) or
            (ALeft.fm124 <> ARight.fm124 ) or
            (ALeft.fm134 <> ARight.fm134 ) or
            (ALeft.fm234 <> ARight.fm234 ) or
            (0.0         <> ARight.fm1234);
end;

class operator TTrivector.=(const ALeft: TMultivector; const ARight: TTrivector): boolean;
begin
  result := (ALeft.fm0    =          0.0) and
            (ALeft.fm1    =          0.0) and
            (ALeft.fm2    =          0.0) and
            (ALeft.fm3    =          0.0) and
            (ALeft.fm4    =          0.0) and
            (ALeft.fm12   =          0.0) and
            (ALeft.fm13   =          0.0) and
            (ALeft.fm14   =          0.0) and
            (ALeft.fm23   =          0.0) and
            (ALeft.fm24   =          0.0) and
            (ALeft.fm34   =          0.0) and
            (ALeft.fm123  = ARight.fm123) and
            (ALeft.fm124  = ARight.fm124) and
            (ALeft.fm134  = ARight.fm134) and
            (ALeft.fm234  = ARight.fm234) and
            (ALeft.fm1234 =          0.0);
end;

class operator TTrivector.=(const ALeft: TTrivector; const ARight: TMultivector): boolean;
begin
  result := (0.0         = ARight.fm0   ) and
            (0.0         = ARight.fm1   ) and
            (0.0         = ARight.fm2   ) and
            (0.0         = ARight.fm3   ) and
            (0.0         = ARight.fm4   ) and
            (0.0         = ARight.fm12  ) and
            (0.0         = ARight.fm13  ) and
            (0.0         = ARight.fm14  ) and
            (0.0         = ARight.fm23  ) and
            (0.0         = ARight.fm24  ) and
            (0.0         = ARight.fm34  ) and
            (ALeft.fm123 = ARight.fm123 ) and
            (ALeft.fm124 = ARight.fm124 ) and
            (ALeft.fm134 = ARight.fm134 ) and
            (ALeft.fm234 = ARight.fm234 ) and
            (0.0         = ARight.fm1234);
end;

class operator TTrivector.=(const ALeft, ARight: TTrivector): boolean;
begin
  result := (ALeft.fm123 = ARight.fm123) and
            (ALeft.fm124 = ARight.fm124) and
            (ALeft.fm134 = ARight.fm134) and
            (ALeft.fm234 = ARight.fm234);
end;

class operator TTrivector.+(const ALeft, ARight: TTrivector): TTrivector;
begin
  result.fm123 := ALeft.fm123 + ARight.fm123;
  result.fm124 := ALeft.fm124 + ARight.fm124;
  result.fm134 := ALeft.fm134 + ARight.fm134;
  result.fm234 := ALeft.fm234 + ARight.fm234;
end;

class operator TTrivector.+(const ALeft: TTrivector; const ARight: double): TMultivector;
begin
  result.fm0    := ARight;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := ALeft.fm123;
  result.fm124  := ALeft.fm124;
  result.fm134  := ALeft.fm134;
  result.fm234  := ALeft.fm234;
  result.fm1234 := 0.0;
end;

class operator TTrivector.+(const ALeft: double; const ARight: TTrivector): TMultivector;
begin
  result.fm0    := ALeft;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := ARight.fm123;
  result.fm124  := ARight.fm124;
  result.fm134  := ARight.fm134;
  result.fm234  := ARight.fm234;
  result.fm1234 := 0.0;
end;

class operator TTrivector.+(const ALeft: TTrivector; const ARight: TQuadrivector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := ALeft.fm123;
  result.fm124  := ALeft.fm124;
  result.fm134  := ALeft.fm134;
  result.fm234  := ALeft.fm234;
  result.fm1234 := ARight.fm1234;
end;

class operator TTrivector.+(const ALeft: TQuadrivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := ARight.fm123;
  result.fm124  := ARight.fm124;
  result.fm134  := ARight.fm134;
  result.fm234  := ARight.fm234;
  result.fm1234 := ALeft.fm1234;
end;

class operator TTrivector.+(const ALeft: TMultivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0    := ALeft.fm0;
  result.fm1    := ALeft.fm1;
  result.fm2    := ALeft.fm2;
  result.fm3    := ALeft.fm3;
  result.fm4    := ALeft.fm4;
  result.fm12   := ALeft.fm12;
  result.fm13   := ALeft.fm13;
  result.fm14   := ALeft.fm14;
  result.fm23   := ALeft.fm23;
  result.fm24   := ALeft.fm24;
  result.fm34   := ALeft.fm34;
  result.fm123  := ALeft.fm123 + ARight.fm123;
  result.fm124  := ALeft.fm124 + ARight.fm124;
  result.fm134  := ALeft.fm134 + ARight.fm134;
  result.fm234  := ALeft.fm234 + ARight.fm234;
  result.fm1234 := ALeft.fm1234;
end;

class operator TTrivector.+(const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0    := ARight.fm0;
  result.fm1    := ARight.fm1;
  result.fm2    := ARight.fm2;
  result.fm3    := ARight.fm3;
  result.fm4    := ARight.fm4;
  result.fm12   := ARight.fm12;
  result.fm13   := ARight.fm13;
  result.fm14   := ARight.fm14;
  result.fm23   := ARight.fm23;
  result.fm24   := ARight.fm24;
  result.fm34   := ARight.fm34;
  result.fm123  := ALeft.fm123 + ARight.fm123;
  result.fm124  := ALeft.fm124 + ARight.fm124;
  result.fm134  := ALeft.fm134 + ARight.fm134;
  result.fm234  := ALeft.fm234 + ARight.fm234;
  result.fm1234 := ARight.fm1234;
end;

class operator TTrivector.-(const ASelf: TTrivector): TTrivector;
begin
  result.fm123 := -ASelf.fm123;
  result.fm124 := -ASelf.fm124;
  result.fm134 := -ASelf.fm134;
  result.fm234 := -ASelf.fm234;
end;

class operator TTrivector.-(const ALeft, ARight: TTrivector): TTrivector;
begin
  result.fm123 := ALeft.fm123 - ARight.fm123;
  result.fm124 := ALeft.fm124 - ARight.fm124;
  result.fm134 := ALeft.fm134 - ARight.fm134;
  result.fm234 := ALeft.fm234 - ARight.fm234;
end;

class operator TTrivector.-(const ALeft: TTrivector; const ARight: double): TMultivector;
begin
  result.fm0    := -ARight;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  ALeft.fm123;
  result.fm124  :=  ALeft.fm124;
  result.fm134  :=  ALeft.fm134;
  result.fm234  :=  ALeft.fm234;
  result.fm1234 :=  0.0;
end;

class operator TTrivector.-(const ALeft: double; const ARight: TTrivector): TMultivector;
begin
  result.fm0    :=  ALeft;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  := -ARight.fm123;
  result.fm124  := -ARight.fm124;
  result.fm134  := -ARight.fm134;
  result.fm234  := -ARight.fm234;
  result.fm1234 :=  0.0;
end;

class operator TTrivector.-(const ALeft: TTrivector; const ARight: TQuadrivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  ALeft.fm123;
  result.fm124  :=  ALeft.fm124;
  result.fm134  :=  ALeft.fm134;
  result.fm234  :=  ALeft.fm234;
  result.fm1234 := -ARight.fm1234;
end;

class operator TTrivector.-(const ALeft: TQuadrivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  := -ARight.fm123;
  result.fm124  := -ARight.fm124;
  result.fm134  := -ARight.fm134;
  result.fm234  := -ARight.fm234;
  result.fm1234 :=  ALeft.fm1234;
end;

class operator TTrivector.-(const ALeft: TMultivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0    := ALeft.fm0;
  result.fm1    := ALeft.fm1;
  result.fm2    := ALeft.fm2;
  result.fm3    := ALeft.fm3;
  result.fm4    := ALeft.fm4;
  result.fm12   := ALeft.fm12;
  result.fm13   := ALeft.fm13;
  result.fm14   := ALeft.fm14;
  result.fm23   := ALeft.fm23;
  result.fm24   := ALeft.fm24;
  result.fm34   := ALeft.fm34;
  result.fm123  := ALeft.fm123 - ARight.fm123;
  result.fm124  := ALeft.fm124 - ARight.fm124;
  result.fm134  := ALeft.fm134 - ARight.fm134;
  result.fm234  := ALeft.fm234 - ARight.fm234;
  result.fm1234 := ALeft.fm1234;
end;

class operator TTrivector.-(const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0    := -ARight.fm0;
  result.fm1    := -ARight.fm1;
  result.fm2    := -ARight.fm2;
  result.fm3    := -ARight.fm3;
  result.fm4    := -ARight.fm4;
  result.fm12   := -ARight.fm12;
  result.fm13   := -ARight.fm13;
  result.fm14   := -ARight.fm14;
  result.fm23   := -ARight.fm23;
  result.fm24   := -ARight.fm24;
  result.fm34   := -ARight.fm34;
  result.fm123  :=  ALeft.fm123 - ARight.fm123;
  result.fm124  :=  ALeft.fm124 - ARight.fm124;
  result.fm134  :=  ALeft.fm134 - ARight.fm134;
  result.fm234  :=  ALeft.fm234 - ARight.fm234;
  result.fm1234 := -ARight.fm1234;
end;

class operator TTrivector.*(const ALeft: double; const ARight: TTrivector): TTrivector;
begin
  result.fm123 := ALeft * ARight.fm123;
  result.fm124 := ALeft * ARight.fm124;
  result.fm134 := ALeft * ARight.fm134;
  result.fm234 := ALeft * ARight.fm234;
end;

class operator TTrivector.*(const ALeft: TTrivector; const ARight: double): TTrivector;
begin
  result.fm123 := ALeft.fm123 * ARight;
  result.fm124 := ALeft.fm124 * ARight;
  result.fm134 := ALeft.fm134 * ARight;
  result.fm234 := ALeft.fm234 * ARight;
end;

class operator TTrivector.*(const ALeft, ARight: TTrivector): TMultivector;
begin
  result.fm0    := -ALeft.fm123 * ARight.fm123 -ALeft.fm124 * ARight.fm124
                   -ALeft.fm134 * ARight.fm134 -ALeft.fm234 * ARight.fm234;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   := -ALeft.fm134 * ARight.fm234 +ALeft.fm234 * ARight.fm134;
  result.fm13   :=  ALeft.fm124 * ARight.fm234 -ALeft.fm234 * ARight.fm124;
  result.fm14   := -ALeft.fm123 * ARight.fm234 +ALeft.fm234 * ARight.fm123;
  result.fm23   := -ALeft.fm124 * ARight.fm134 +ALeft.fm134 * ARight.fm124;
  result.fm24   := +ALeft.fm123 * ARight.fm134 -ALeft.fm134 * ARight.fm123;
  result.fm34   := -ALeft.fm123 * ARight.fm124 +ALeft.fm124 * ARight.fm123;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := 0.0;
end;

class operator TTrivector.*(const ALeft: TTrivector; const ARight: TQuadrivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  ALeft.fm234 * ARight.fm1234;
  result.fm2    := -ALeft.fm134 * ARight.fm1234;
  result.fm3    :=  ALeft.fm124 * ARight.fm1234;
  result.fm4    := -ALeft.fm123 * ARight.fm1234;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  0.0;
end;

class operator TTrivector.*(const ALeft: TQuadrivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    := -ALeft.fm1234 * ARight.fm234;
  result.fm2    :=  ALeft.fm1234 * ARight.fm134;
  result.fm3    := -ALeft.fm1234 * ARight.fm124;
  result.fm4    :=  ALeft.fm1234 * ARight.fm123;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  0.0;
end;

class operator TTrivector.*(const ALeft: TMultivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0 :=    -ALeft.fm123  * ARight.fm123
                   -ALeft.fm124  * ARight.fm124
                   -ALeft.fm134  * ARight.fm134
                   -ALeft.fm234  * ARight.fm234;

  result.fm1 :=    -ALeft.fm23   * ARight.fm123
                   -ALeft.fm24   * ARight.fm124
                   -ALeft.fm34   * ARight.fm134
                   -ALeft.fm1234 * ARight.fm234;

  result.fm2 :=     ALeft.fm13   * ARight.fm123
                   +ALeft.fm14   * ARight.fm124
                   -ALeft.fm34   * ARight.fm234
                   +ALeft.fm1234 * ARight.fm134;

  result.fm3 :=    -ALeft.fm12   * ARight.fm123
                   +ALeft.fm14   * ARight.fm134
                   +ALeft.fm24   * ARight.fm234
                   -ALeft.fm1234 * ARight.fm124;

  result.fm4 :=    -ALeft.fm12   * ARight.fm124
                   -ALeft.fm13   * ARight.fm134
                   -ALeft.fm23   * ARight.fm234
                   +ALeft.fm1234 * ARight.fm123;

  result.fm12 :=    ALeft.fm3    * ARight.fm123
                   +ALeft.fm4    * ARight.fm124
                   -ALeft.fm134  * ARight.fm234
                   +ALeft.fm234  * ARight.fm134;

  result.fm13 :=   -ALeft.fm2    * ARight.fm123
                   +ALeft.fm4    * ARight.fm134
                   +ALeft.fm124  * ARight.fm234
                   -ALeft.fm234  * ARight.fm124;

  result.fm14 :=   -ALeft.fm2    * ARight.fm124
                   -ALeft.fm3    * ARight.fm134
                   -ALeft.fm123  * ARight.fm234
                   +ALeft.fm234  * ARight.fm123;

  result.fm23 :=    ALeft.fm1    * ARight.fm123
                   +ALeft.fm4    * ARight.fm234
                   -ALeft.fm124  * ARight.fm134
                   +ALeft.fm134  * ARight.fm124;

  result.fm24 :=    ALeft.fm1    * ARight.fm124
                   -ALeft.fm3    * ARight.fm234
                   +ALeft.fm123  * ARight.fm134
                   -ALeft.fm134  * ARight.fm123;

  result.fm34 :=    ALeft.fm1    * ARight.fm134
                   +ALeft.fm2    * ARight.fm234
                   -ALeft.fm123  * ARight.fm124
                   +ALeft.fm124  * ARight.fm123;

  result.fm123 :=   ALeft.fm0    * ARight.fm123
                   +ALeft.fm14   * ARight.fm234
                   -ALeft.fm24   * ARight.fm134
                   +ALeft.fm34   * ARight.fm124;

  result.fm124 :=   ALeft.fm0    * ARight.fm124
                   -ALeft.fm13   * ARight.fm234
                   +ALeft.fm23   * ARight.fm134
                   -ALeft.fm34   * ARight.fm123;

  result.fm134 :=   ALeft.fm0    * ARight.fm134
                   +ALeft.fm12   * ARight.fm234
                   -ALeft.fm23   * ARight.fm124
                   +ALeft.fm24   * ARight.fm123;

  result.fm234 :=   ALeft.fm0    * ARight.fm234
                   -ALeft.fm12   * ARight.fm134
                   +ALeft.fm13   * ARight.fm124
                   -ALeft.fm14   * ARight.fm123;

  result.fm1234 :=  ALeft.fm1    * ARight.fm234
                   -ALeft.fm2    * ARight.fm134
                   +ALeft.fm3    * ARight.fm124
                   -ALeft.fm4    * ARight.fm123;
end;

class operator TTrivector.*(const ALeft: TTrivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0 :=    -ALeft.fm123  * ARight.fm123
                   -ALeft.fm124  * ARight.fm124
                   -ALeft.fm134  * ARight.fm134
                   -ALeft.fm234  * ARight.fm234;

  result.fm1 :=    -ALeft.fm123  * ARight.fm23
                   -ALeft.fm124  * ARight.fm24
                   -ALeft.fm134  * ARight.fm34
                   +ALeft.fm234  * ARight.fm1234;

  result.fm2 :=     ALeft.fm123  * ARight.fm13
                   +ALeft.fm124  * ARight.fm14
                   -ALeft.fm134  * ARight.fm1234
                   -ALeft.fm234  * ARight.fm34;

  result.fm3 :=    -ALeft.fm123  * ARight.fm12
                   +ALeft.fm124  * ARight.fm1234
                   +ALeft.fm134  * ARight.fm14
                   +ALeft.fm234  * ARight.fm24;

  result.fm4 :=    -ALeft.fm123  * ARight.fm1234
                   -ALeft.fm124  * ARight.fm12
                   -ALeft.fm134  * ARight.fm13
                   -ALeft.fm234  * ARight.fm23;

  result.fm12 :=   +ALeft.fm123  * ARight.fm3
                   +ALeft.fm124  * ARight.fm4
                   -ALeft.fm134  * ARight.fm234
                   +ALeft.fm234  * ARight.fm134;

  result.fm13 :=   -ALeft.fm123  * ARight.fm2
                   +ALeft.fm124  * ARight.fm234
                   +ALeft.fm134  * ARight.fm4
                   -ALeft.fm234  * ARight.fm124;

  result.fm14 :=   -ALeft.fm123  * ARight.fm234
                   -ALeft.fm124  * ARight.fm2
                   -ALeft.fm134  * ARight.fm3
                   +ALeft.fm234  * ARight.fm123;

  result.fm23 :=    ALeft.fm123  * ARight.fm1
                   -ALeft.fm124  * ARight.fm134
                   +ALeft.fm134  * ARight.fm124
                   +ALeft.fm234  * ARight.fm4;

  result.fm24 :=   +ALeft.fm123  * ARight.fm134
                   +ALeft.fm124  * ARight.fm1
                   -ALeft.fm134  * ARight.fm123
                   -ALeft.fm234  * ARight.fm3;

  result.fm34 :=   -ALeft.fm123  * ARight.fm124
                   +ALeft.fm124  * ARight.fm123
                   +ALeft.fm134  * ARight.fm1
                   +ALeft.fm234  * ARight.fm2;

  result.fm123 :=  +ALeft.fm123  * ARight.fm0
                   -ALeft.fm124  * ARight.fm34
                   +ALeft.fm134  * ARight.fm24
                   -ALeft.fm234  * ARight.fm14;

  result.fm124 :=  +ALeft.fm123  * ARight.fm34
                   +ALeft.fm124  * ARight.fm0
                   -ALeft.fm134  * ARight.fm23
                   +ALeft.fm234  * ARight.fm13;

  result.fm134 :=  -ALeft.fm123  * ARight.fm24
                   +ALeft.fm124  * ARight.fm23
                   +ALeft.fm134  * ARight.fm0
                   -ALeft.fm234  * ARight.fm12;

  result.fm234 :=   ALeft.fm123  * ARight.fm14
                   -ALeft.fm124  * ARight.fm13
                   +ALeft.fm134  * ARight.fm12
                   +ALeft.fm234  * ARight.fm0;

  result.fm1234 :=  ALeft.fm123  * ARight.fm4
                   -ALeft.fm124  * ARight.fm3
                   +ALeft.fm134  * ARight.fm2
                   -ALeft.fm234  * ARight.fm1;
end;

class operator TTrivector./(const ALeft, ARight: TTrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TTrivector./(const ALeft: TTrivector; const ARight: double): TTrivector;
begin
  result.fm123 := ALeft.fm123 / ARight;
  result.fm124 := ALeft.fm124 / ARight;
  result.fm134 := ALeft.fm134 / ARight;
  result.fm234 := ALeft.fm234 / ARight;
end;

class operator TTrivector./(const ALeft: double; const ARight: TTrivector): TTrivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TTrivector./(const ALeft: TTrivector; const ARight: TQuadrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TTrivector./(const ALeft: TQuadrivector; const ARight: TTrivector): TMultivector;
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

class operator TBivector.:=(const AValue: TBivector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := AValue.fm12;
  result.fm13   := AValue.fm13;
  result.fm14   := AValue.fm14;
  result.fm23   := AValue.fm23;
  result.fm24   := AValue.fm24;
  result.fm34   := AValue.fm34;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := 0.0;
end;

class operator TBivector.:=(const AValue: TMultivector): TBivector;
begin
  result.fm12 := AValue.fm12;
  result.fm13 := AValue.fm13;
  result.fm14 := AValue.fm14;
  result.fm23 := AValue.fm23;
  result.fm24 := AValue.fm24;
  result.fm34 := AValue.fm34;
end;

class operator TBivector.<>(const ALeft, ARight: TBivector): boolean;
begin
  result := (ALeft.fm12 <> ARight.fm12) or
            (ALeft.fm13 <> ARight.fm13) or
            (ALeft.fm14 <> ARight.fm14) or
            (ALeft.fm23 <> ARight.fm23) or
            (ALeft.fm24 <> ARight.fm24) or
            (ALeft.fm34 <> ARight.fm34);
end;

class operator TBivector.<>(const ALeft: TMultivector; const ARight: TBivector): boolean;
begin
  result := (ALeft.fm0    <>         0.0) or
            (ALeft.fm1    <>         0.0) or
            (ALeft.fm2    <>         0.0) or
            (ALeft.fm3    <>         0.0) or
            (ALeft.fm4    <>         0.0) or
            (ALeft.fm12   <> ARight.fm12) or
            (ALeft.fm13   <> ARight.fm13) or
            (ALeft.fm14   <> ARight.fm14) or
            (ALeft.fm23   <> ARight.fm23) or
            (ALeft.fm24   <> ARight.fm24) or
            (ALeft.fm34   <> ARight.fm34) or
            (ALeft.fm123  <>         0.0) or
            (ALeft.fm124  <>         0.0) or
            (ALeft.fm134  <>         0.0) or
            (ALeft.fm234  <>         0.0) or
            (ALeft.fm1234 <>         0.0);
end;

class operator TBivector.<>(const ALeft: TBivector; const ARight: TMultivector): boolean;
begin
  result := (ARight.fm0    <>        0.0) or
            (ARight.fm1    <>        0.0) or
            (ARight.fm2    <>        0.0) or
            (ARight.fm3    <>        0.0) or
            (ARight.fm4    <>        0.0) or
            (ARight.fm12   <> ALeft.fm12) or
            (ARight.fm13   <> ALeft.fm13) or
            (ARight.fm14   <> ALeft.fm14) or
            (ARight.fm23   <> ALeft.fm23) or
            (ARight.fm24   <> ALeft.fm24) or
            (ARight.fm34   <> ALeft.fm34) or
            (ARight.fm123  <>        0.0) or
            (ARight.fm124  <>        0.0) or
            (ARight.fm134  <>        0.0) or
            (ARight.fm234  <>        0.0) or
            (ARight.fm1234 <>        0.0);
end;

class operator TBivector.=(const ALeft, ARight: TBivector): boolean;
begin
  result := (ALeft.fm12 = ARight.fm12) or
            (ALeft.fm13 = ARight.fm13) or
            (ALeft.fm14 = ARight.fm14) or
            (ALeft.fm23 = ARight.fm23) or
            (ALeft.fm24 = ARight.fm24) or
            (ALeft.fm34 = ARight.fm34);
end;

class operator TBivector.=(const ALeft: TMultivector; const ARight: TBivector): boolean;
begin
  result := (ALeft.fm0    =         0.0) or
            (ALeft.fm1    =         0.0) or
            (ALeft.fm2    =         0.0) or
            (ALeft.fm3    =         0.0) or
            (ALeft.fm4    =         0.0) or
            (ALeft.fm12   = ARight.fm12) or
            (ALeft.fm13   = ARight.fm13) or
            (ALeft.fm14   = ARight.fm14) or
            (ALeft.fm23   = ARight.fm23) or
            (ALeft.fm24   = ARight.fm24) or
            (ALeft.fm34   = ARight.fm34) or
            (ALeft.fm123  =         0.0) or
            (ALeft.fm124  =         0.0) or
            (ALeft.fm134  =         0.0) or
            (ALeft.fm234  =         0.0) or
            (ALeft.fm1234 =         0.0);
end;

class operator TBivector.=(const ALeft: TBivector; const ARight: TMultivector): boolean;
begin
  result := (ARight.fm0    =        0.0) or
            (ARight.fm1    =        0.0) or
            (ARight.fm2    =        0.0) or
            (ARight.fm3    =        0.0) or
            (ARight.fm4    =        0.0) or
            (ARight.fm12   = ALeft.fm12) or
            (ARight.fm13   = ALeft.fm13) or
            (ARight.fm14   = ALeft.fm14) or
            (ARight.fm23   = ALeft.fm23) or
            (ARight.fm24   = ALeft.fm24) or
            (ARight.fm34   = ALeft.fm34) or
            (ARight.fm123  =        0.0) or
            (ARight.fm124  =        0.0) or
            (ARight.fm134  =        0.0) or
            (ARight.fm234  =        0.0) or
            (ARight.fm1234 =        0.0);
end;

class operator TBivector.+(const ALeft, ARight: TBivector): TBivector;
begin
  result.fm12 := ALeft.fm12 + ARight.fm12;
  result.fm13 := ALeft.fm13 + ARight.fm13;
  result.fm14 := ALeft.fm14 + ARight.fm14;
  result.fm23 := ALeft.fm23 + ARight.fm23;
  result.fm24 := ALeft.fm24 + ARight.fm24;
  result.fm34 := ALeft.fm34 + ARight.fm34;
end;

class operator TBivector.+(const ALeft: TBivector; const ARight: double): TMultivector;
begin
  result.fm0    := ARight;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := ALeft.fm12;
  result.fm13   := ALeft.fm13;
  result.fm14   := ALeft.fm14;
  result.fm23   := ALeft.fm23;
  result.fm24   := ALeft.fm24;
  result.fm34   := ALeft.fm34;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := 0.0;
end;

class operator TBivector.+(const ALeft: double; const ARight: TBivector): TMultivector;
begin
  result.fm0    := ALeft;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := ARight.fm12;
  result.fm13   := ARight.fm13;
  result.fm14   := ARight.fm14;
  result.fm23   := ARight.fm23;
  result.fm24   := ARight.fm24;
  result.fm34   := ARight.fm34;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := 0.0;
end;

class operator TBivector.+(const ALeft: TBivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := ALeft.fm12;
  result.fm13   := ALeft.fm13;
  result.fm14   := ALeft.fm14;
  result.fm23   := ALeft.fm23;
  result.fm24   := ALeft.fm24;
  result.fm34   := ALeft.fm34;
  result.fm123  := ARight.fm123;
  result.fm124  := ARight.fm124;
  result.fm134  := ARight.fm134;
  result.fm234  := ARight.fm234;
  result.fm1234 := 0.0;
end;

class operator TBivector.+(const ALeft: TTrivector; const ARight: TBivector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := ARight.fm12;
  result.fm13   := ARight.fm13;
  result.fm14   := ARight.fm14;
  result.fm23   := ARight.fm23;
  result.fm24   := ARight.fm24;
  result.fm34   := ARight.fm34;
  result.fm123  := ALeft.fm123;
  result.fm124  := ALeft.fm124;
  result.fm134  := ALeft.fm134;
  result.fm234  := ALeft.fm234;
  result.fm1234 := 0.0;
end;

class operator TBivector.+(const ALeft: TBivector; const ARight: TQuadrivector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := ALeft.fm12;
  result.fm13   := ALeft.fm13;
  result.fm14   := ALeft.fm14;
  result.fm23   := ALeft.fm23;
  result.fm24   := ALeft.fm24;
  result.fm34   := ALeft.fm34;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := ARight.fm1234;
end;

class operator TBivector.+(const ALeft: TQuadrivector; const ARight: TBivector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := ARight.fm12;
  result.fm13   := ARight.fm13;
  result.fm14   := ARight.fm14;
  result.fm23   := ARight.fm23;
  result.fm24   := ARight.fm24;
  result.fm34   := ARight.fm34;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := ALeft.fm1234;
end;

class operator TBivector.+(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0    := ARight.fm0;
  result.fm1    := ARight.fm1;
  result.fm2    := ARight.fm2;
  result.fm3    := ARight.fm3;
  result.fm4    := ARight.fm4;
  result.fm12   := ARight.fm12 + ALeft.fm12;
  result.fm13   := ARight.fm13 + ALeft.fm13;
  result.fm14   := ARight.fm14 + ALeft.fm14;
  result.fm23   := ARight.fm23 + ALeft.fm23;
  result.fm24   := ARight.fm24 + ALeft.fm24;
  result.fm34   := ARight.fm34 + ALeft.fm34;
  result.fm123  := ARight.fm123;
  result.fm124  := ARight.fm124;
  result.fm134  := ARight.fm134;
  result.fm234  := ARight.fm234;
  result.fm1234 := ARight.fm1234;
end;

class operator TBivector.+(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
begin
  result.fm0    := ALeft.fm0;
  result.fm1    := ALeft.fm1;
  result.fm2    := ALeft.fm2;
  result.fm3    := ALeft.fm3;
  result.fm4    := ALeft.fm4;
  result.fm12   := ALeft.fm12 + ARight.fm12;
  result.fm13   := ALeft.fm13 + ARight.fm13;
  result.fm14   := ALeft.fm14 + ARight.fm14;
  result.fm23   := ALeft.fm23 + ARight.fm23;
  result.fm24   := ALeft.fm24 + ARight.fm24;
  result.fm34   := ALeft.fm34 + ARight.fm34;
  result.fm123  := ALeft.fm123;
  result.fm124  := ALeft.fm124;
  result.fm134  := ALeft.fm134;
  result.fm234  := ALeft.fm234;
  result.fm1234 := ALeft.fm1234;
end;

class operator TBivector.-(const ASelf: TBivector): TBivector;
begin
  result.fm12 := -ASelf.fm12;
  result.fm13 := -ASelf.fm13;
  result.fm14 := -ASelf.fm14;
  result.fm23 := -ASelf.fm23;
  result.fm24 := -ASelf.fm24;
  result.fm34 := -ASelf.fm34;
end;

class operator TBivector.-(const ALeft, ARight: TBivector): TBivector;
begin
  result.fm12 := ALeft.fm12 - ARight.fm12;
  result.fm13 := ALeft.fm13 - ARight.fm13;
  result.fm14 := ALeft.fm14 - ARight.fm14;
  result.fm23 := ALeft.fm23 - ARight.fm23;
  result.fm24 := ALeft.fm24 - ARight.fm24;
  result.fm34 := ALeft.fm34 - ARight.fm34;
end;

class operator TBivector.-(const ALeft: TBivector; const ARight: double): TMultivector;
begin
  result.fm0    := -ARight;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   :=  ALeft.fm12;
  result.fm13   :=  ALeft.fm13;
  result.fm14   :=  ALeft.fm14;
  result.fm23   :=  ALeft.fm23;
  result.fm24   :=  ALeft.fm24;
  result.fm34   :=  ALeft.fm34;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  0.0;
end;

class operator TBivector.-(const ALeft: double; const ARight: TBivector): TMultivector;
begin
  result.fm0    :=  ALeft;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   := -ARight.fm12;
  result.fm13   := -ARight.fm13;
  result.fm14   := -ARight.fm14;
  result.fm23   := -ARight.fm23;
  result.fm24   := -ARight.fm24;
  result.fm34   := -ARight.fm34;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  0.0;
end;

class operator TBivector.-(const ALeft: TBivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0   :=  0.0;
  result.fm1   :=  0.0;
  result.fm2   :=  0.0;
  result.fm3   :=  0.0;
  result.fm4   :=  0.0;
  result.fm12  :=  ALeft.fm12;
  result.fm13  :=  ALeft.fm13;
  result.fm14  :=  ALeft.fm14;
  result.fm23  :=  ALeft.fm23;
  result.fm24  :=  ALeft.fm24;
  result.fm34  :=  ALeft.fm34;
  result.fm123 := -ARight.fm123;
  result.fm124 := -ARight.fm124;
  result.fm134 := -ARight.fm134;
  result.fm234 := -ARight.fm234;
  result.fm123 := -ARight.fm123;
end;

class operator TBivector.-(const ALeft: TTrivector; const ARight: TBivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   := -ARight.fm12;
  result.fm13   := -ARight.fm13;
  result.fm14   := -ARight.fm14;
  result.fm23   := -ARight.fm23;
  result.fm24   := -ARight.fm24;
  result.fm34   := -ARight.fm34;
  result.fm123  :=  ALeft.fm123;
  result.fm124  :=  ALeft.fm124;
  result.fm134  :=  ALeft.fm134;
  result.fm234  :=  ALeft.fm234;
  result.fm1234 :=  0.0;
end;


class operator TBivector.-(const ALeft: TBivector; const ARight: TQuadrivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   :=  ALeft.fm12;
  result.fm13   :=  ALeft.fm13;
  result.fm14   :=  ALeft.fm14;
  result.fm23   :=  ALeft.fm23;
  result.fm24   :=  ALeft.fm24;
  result.fm34   :=  ALeft.fm34;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 := -ARight.fm1234;
end;

class operator TBivector.-(const ALeft: TQuadrivector; const ARight: TBivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   := -ARight.fm12;
  result.fm13   := -ARight.fm13;
  result.fm14   := -ARight.fm14;
  result.fm23   := -ARight.fm23;
  result.fm24   := -ARight.fm24;
  result.fm34   := -ARight.fm34;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  ALeft.fm1234;
end;

class operator TBivector.-(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0    := -ARight.fm0;
  result.fm1    := -ARight.fm1;
  result.fm2    := -ARight.fm2;
  result.fm3    := -ARight.fm3;
  result.fm4    := -ARight.fm4;
  result.fm12   :=  ALeft.fm12 - ARight.fm12;
  result.fm13   :=  ALeft.fm13 - ARight.fm13;
  result.fm14   :=  ALeft.fm14 - ARight.fm14;
  result.fm23   :=  ALeft.fm23 - ARight.fm23;
  result.fm24   :=  ALeft.fm24 - ARight.fm24;
  result.fm34   :=  ALeft.fm34 - ARight.fm34;
  result.fm123  := -ARight.fm123;
  result.fm124  := -ARight.fm124;
  result.fm134  := -ARight.fm134;
  result.fm234  := -ARight.fm234;
  result.fm1234 := -ARight.fm1234;
end;

class operator TBivector.-(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
begin
  result.fm0    := ALeft.fm0;
  result.fm1    := ALeft.fm1;
  result.fm2    := ALeft.fm2;
  result.fm3    := ALeft.fm3;
  result.fm4    := ALeft.fm4;
  result.fm12   := ALeft.fm12 - ARight.fm12;
  result.fm13   := ALeft.fm13 - ARight.fm13;
  result.fm14   := ALeft.fm14 - ARight.fm14;
  result.fm23   := ALeft.fm23 - ARight.fm23;
  result.fm24   := ALeft.fm24 - ARight.fm24;
  result.fm34   := ALeft.fm34 - ARight.fm34;
  result.fm123  := ALeft.fm123;
  result.fm124  := ALeft.fm124;
  result.fm134  := ALeft.fm134;
  result.fm234  := ALeft.fm234;
  result.fm1234 := ALeft.fm1234;
end;

class operator TBivector.*(const ALeft, ARight: TBivector): TMultivector;
begin
  result.fm0 :=      -ALeft.fm12   * ARight.fm12
                     -ALeft.fm13   * ARight.fm13
                     -ALeft.fm14   * ARight.fm14
                     -ALeft.fm23   * ARight.fm23
                     -ALeft.fm24   * ARight.fm24
                     -ALeft.fm34   * ARight.fm34;
    result.fm1 :=     0.0;
    result.fm2 :=     0.0;
    result.fm3 :=     0.0;
    result.fm4 :=     0.0;
    result.fm12 :=   -ALeft.fm13   * ARight.fm23
                     -ALeft.fm14   * ARight.fm24
                     +ALeft.fm23   * ARight.fm13
                     +ALeft.fm24   * ARight.fm14;
    result.fm13 :=    ALeft.fm12   * ARight.fm23
                     -ALeft.fm14   * ARight.fm34
                     -ALeft.fm23   * ARight.fm12
                     +ALeft.fm34   * ARight.fm14;
    result.fm14 :=    ALeft.fm12   * ARight.fm24
                     +ALeft.fm13   * ARight.fm34
                     -ALeft.fm24   * ARight.fm12
                     -ALeft.fm34   * ARight.fm13;
    result.fm23 :=   -ALeft.fm12   * ARight.fm13
                     +ALeft.fm13   * ARight.fm12
                     -ALeft.fm24   * ARight.fm34
                     +ALeft.fm34   * ARight.fm24;
    result.fm24 :=   -ALeft.fm12   * ARight.fm14
                     +ALeft.fm14   * ARight.fm12
                     +ALeft.fm23   * ARight.fm34
                     -ALeft.fm34   * ARight.fm23;
    result.fm34 :=   -ALeft.fm13   * ARight.fm14
                     +ALeft.fm14   * ARight.fm13
                     -ALeft.fm23   * ARight.fm24
                     +ALeft.fm24   * ARight.fm23;
    result.fm123 :=   0.0;
    result.fm124 :=   0.0;
    result.fm134 :=   0.0;
    result.fm234 :=   0.0;
    result.fm1234 :=  ALeft.fm12   * ARight.fm34
                     -ALeft.fm13   * ARight.fm24
                     +ALeft.fm14   * ARight.fm23
                     +ALeft.fm23   * ARight.fm14
                     -ALeft.fm24   * ARight.fm13
                     +ALeft.fm34   * ARight.fm12;
end;

class operator TBivector.*(const ALeft: double; const ARight: TBivector): TBivector;
begin
  result.fm12 := ALeft * ARight.fm12;
  result.fm13 := ALeft * ARight.fm13;
  result.fm14 := ALeft * ARight.fm14;
  result.fm23 := ALeft * ARight.fm23;
  result.fm24 := ALeft * ARight.fm24;
  result.fm34 := ALeft * ARight.fm34;
end;

class operator TBivector.*(const ALeft: TBivector; const ARight: double): TBivector;
begin
  result.fm12 := ARight * ALeft.fm12;
  result.fm13 := ARight * ALeft.fm13;
  result.fm14 := ARight * ALeft.fm14;
  result.fm23 := ARight * ALeft.fm23;
  result.fm24 := ARight * ALeft.fm24;
  result.fm34 := ARight * ALeft.fm34;
end;

class operator TBivector.*(const ALeft: TBivector; const ARight: TTrivector): TMultivector;
begin
  result.fm0 :=     0.0;
  result.fm1 :=    -ALeft.fm23   * ARight.fm123
                   -ALeft.fm24   * ARight.fm124
                   -ALeft.fm34   * ARight.fm134;
  result.fm2 :=     ALeft.fm13   * ARight.fm123
                   +ALeft.fm14   * ARight.fm124
                   -ALeft.fm34   * ARight.fm234;
  result.fm3 :=    -ALeft.fm12   * ARight.fm123
                   +ALeft.fm14   * ARight.fm134
                   +ALeft.fm24   * ARight.fm234;
  result.fm4 :=    -ALeft.fm12   * ARight.fm124
                   -ALeft.fm13   * ARight.fm134
                   -ALeft.fm23   * ARight.fm234;
  result.fm12 :=    0.0;
  result.fm13 :=    0.0;
  result.fm14 :=    0.0;
  result.fm23 :=    0.0;
  result.fm24 :=    0.0;
  result.fm34 :=    0.0;
  result.fm123 :=   ALeft.fm14   * ARight.fm234
                   -ALeft.fm24   * ARight.fm134
                   +ALeft.fm34   * ARight.fm124;
  result.fm124 :=  -ALeft.fm13   * ARight.fm234
                   +ALeft.fm23   * ARight.fm134
                   -ALeft.fm34   * ARight.fm123;
  result.fm134 :=   ALeft.fm12   * ARight.fm234
                   -ALeft.fm23   * ARight.fm124
                   +ALeft.fm24   * ARight.fm123;
  result.fm234 :=  -ALeft.fm12   * ARight.fm134
                   +ALeft.fm13   * ARight.fm124
                   -ALeft.fm14   * ARight.fm123;
  result.fm1234 :=  0.0;
end;

class operator TBivector.*(const ALeft: TTrivector; const ARight: TBivector): TMultivector;
begin
  result.fm0 :=     0.0;
  result.fm1 :=    -ALeft.fm123  * ARight.fm23
                   -ALeft.fm124  * ARight.fm24
                   -ALeft.fm134  * ARight.fm34;
  result.fm2 :=     ALeft.fm123  * ARight.fm13
                   +ALeft.fm124  * ARight.fm14
                   -ALeft.fm234  * ARight.fm34;
  result.fm3 :=    -ALeft.fm123  * ARight.fm12
                   +ALeft.fm134  * ARight.fm14
                   +ALeft.fm234  * ARight.fm24;
  result.fm4 :=    -ALeft.fm124  * ARight.fm12
                   -ALeft.fm134  * ARight.fm13
                   -ALeft.fm234  * ARight.fm23;
  result.fm12 :=    0.0;
  result.fm13 :=    0.0;
  result.fm14 :=    0.0;
  result.fm23 :=    0.0;
  result.fm24 :=    0.0;
  result.fm34 :=    0.0;
  result.fm123 :=  -ALeft.fm124  * ARight.fm34
                   +ALeft.fm134  * ARight.fm24
                   -ALeft.fm234  * ARight.fm14;
  result.fm124 :=   ALeft.fm123  * ARight.fm34
                   -ALeft.fm134  * ARight.fm23
                   +ALeft.fm234  * ARight.fm13;
  result.fm134 :=  -ALeft.fm123  * ARight.fm24
                   +ALeft.fm124  * ARight.fm23
                   -ALeft.fm234  * ARight.fm12;
  result.fm234 :=   ALeft.fm123  * ARight.fm14
                   -ALeft.fm124  * ARight.fm13
                   +ALeft.fm134  * ARight.fm12;
  result.fm1234 :=  0.0;
end;


class operator TBivector.*(const ALeft: TBivector; const ARight: TQuadrivector): TBivector;
begin
  result.fm12 := -ALeft.fm34 * ARight.fm1234;
  result.fm13 :=  ALeft.fm24 * ARight.fm1234;
  result.fm14 := -ALeft.fm23 * ARight.fm1234;
  result.fm23 := -ALeft.fm14 * ARight.fm1234;
  result.fm24 :=  ALeft.fm13 * ARight.fm1234;
  result.fm34 := -ALeft.fm12 * ARight.fm1234;
end;

class operator TBivector.*(const ALeft: TQuadrivector; const ARight: TBivector): TBivector;
begin
  result.fm12 := -ALeft.fm1234 * ARight.fm34;
  result.fm13 :=  ALeft.fm1234 * ARight.fm24;
  result.fm14 := -ALeft.fm1234 * ARight.fm23;
  result.fm23 := -ALeft.fm1234 * ARight.fm14;
  result.fm24 :=  ALeft.fm1234 * ARight.fm13;
  result.fm34 := -ALeft.fm1234 * ARight.fm12;
end;

class operator TBivector.*(const ALeft: TBivector; const ARight: TMultivector): TMultivector;
begin
  result.fm0 :=    -ALeft.fm12   * ARight.fm12
                   -ALeft.fm13   * ARight.fm13
                   -ALeft.fm14   * ARight.fm14
                   -ALeft.fm23   * ARight.fm23
                   -ALeft.fm24   * ARight.fm24
                   -ALeft.fm34   * ARight.fm34;
  result.fm1 :=     ALeft.fm12   * ARight.fm2
                   +ALeft.fm13   * ARight.fm3
                   +ALeft.fm14   * ARight.fm4
                   -ALeft.fm23   * ARight.fm123
                   -ALeft.fm24   * ARight.fm124
                   -ALeft.fm34   * ARight.fm134;
  result.fm2 :=    -ALeft.fm12   * ARight.fm1
                   +ALeft.fm13   * ARight.fm123
                   +ALeft.fm14   * ARight.fm124
                   +ALeft.fm23   * ARight.fm3
                   +ALeft.fm24   * ARight.fm4
                   -ALeft.fm34   * ARight.fm234;
  result.fm3 :=    -ALeft.fm12   * ARight.fm123
                   -ALeft.fm13   * ARight.fm1
                   +ALeft.fm14   * ARight.fm134
                   -ALeft.fm23   * ARight.fm2
                   +ALeft.fm24   * ARight.fm234
                   +ALeft.fm34   * ARight.fm4;
  result.fm4 :=    -ALeft.fm12   * ARight.fm124
                   -ALeft.fm13   * ARight.fm134
                   -ALeft.fm14   * ARight.fm1
                   -ALeft.fm23   * ARight.fm234
                   -ALeft.fm24   * ARight.fm2
                   -ALeft.fm34   * ARight.fm3;
  result.fm12 :=    ALeft.fm12   * ARight.fm0
                   -ALeft.fm13   * ARight.fm23
                   -ALeft.fm14   * ARight.fm24
                   +ALeft.fm23   * ARight.fm13
                   +ALeft.fm24   * ARight.fm14
                   -ALeft.fm34   * ARight.fm1234;
  result.fm13 :=    ALeft.fm12   * ARight.fm23
                   +ALeft.fm13   * ARight.fm0
                   -ALeft.fm14   * ARight.fm34
                   -ALeft.fm23   * ARight.fm12
                   +ALeft.fm24   * ARight.fm1234
                   +ALeft.fm34   * ARight.fm14;
  result.fm14 :=    ALeft.fm12   * ARight.fm24
                   +ALeft.fm13   * ARight.fm34
                   +ALeft.fm14   * ARight.fm0
                   -ALeft.fm23   * ARight.fm1234
                   -ALeft.fm24   * ARight.fm12
                   -ALeft.fm34   * ARight.fm13;
  result.fm23 :=   -ALeft.fm12   * ARight.fm13
                   +ALeft.fm13   * ARight.fm12
                   -ALeft.fm14   * ARight.fm1234
                   +ALeft.fm23   * ARight.fm0
                   -ALeft.fm24   * ARight.fm34
                   +ALeft.fm34   * ARight.fm24;
  result.fm24 :=   -ALeft.fm12   * ARight.fm14
                   +ALeft.fm13   * ARight.fm1234
                   +ALeft.fm14   * ARight.fm12
                   +ALeft.fm23   * ARight.fm34
                   +ALeft.fm24   * ARight.fm0
                   -ALeft.fm34   * ARight.fm23;
  result.fm34 :=   -ALeft.fm12   * ARight.fm1234
                   -ALeft.fm13   * ARight.fm14
                   +ALeft.fm14   * ARight.fm13
                   -ALeft.fm23   * ARight.fm24
                   +ALeft.fm24   * ARight.fm23
                   +ALeft.fm34   * ARight.fm0;
  result.fm123 :=   ALeft.fm12   * ARight.fm3
                   -ALeft.fm13   * ARight.fm2
                   +ALeft.fm14   * ARight.fm234
                   +ALeft.fm23   * ARight.fm1
                   -ALeft.fm24   * ARight.fm134
                   +ALeft.fm34   * ARight.fm124;
  result.fm124 :=   ALeft.fm12   * ARight.fm4
                   -ALeft.fm13   * ARight.fm234
                   -ALeft.fm14   * ARight.fm2
                   +ALeft.fm23   * ARight.fm134
                   +ALeft.fm24   * ARight.fm1
                   -ALeft.fm34   * ARight.fm123;
  result.fm134 :=   ALeft.fm12   * ARight.fm234
                   +ALeft.fm13   * ARight.fm4
                   -ALeft.fm14   * ARight.fm3
                   -ALeft.fm23   * ARight.fm124
                   +ALeft.fm24   * ARight.fm123
                   +ALeft.fm34   * ARight.fm1;
  result.fm234 :=  -ALeft.fm12   * ARight.fm134
                   +ALeft.fm13   * ARight.fm124
                   -ALeft.fm14   * ARight.fm123
                   +ALeft.fm23   * ARight.fm4
                   -ALeft.fm24   * ARight.fm3
                   +ALeft.fm34   * ARight.fm2;
  result.fm1234 :=  ALeft.fm12   * ARight.fm34
                   -ALeft.fm13   * ARight.fm24
                   +ALeft.fm14   * ARight.fm23
                   +ALeft.fm23   * ARight.fm14
                   -ALeft.fm24   * ARight.fm13
                   +ALeft.fm34   * ARight.fm12;
end;

class operator TBivector.*(const ALeft: TMultivector; const ARight: TBivector): TMultivector;
begin
  result.fm0 :=    -ALeft.fm12   * ARight.fm12
                   -ALeft.fm13   * ARight.fm13
                   -ALeft.fm14   * ARight.fm14
                   -ALeft.fm23   * ARight.fm23
                   -ALeft.fm24   * ARight.fm24
                   -ALeft.fm34   * ARight.fm34;
  result.fm1 :=    -ALeft.fm2    * ARight.fm12
                   -ALeft.fm3    * ARight.fm13
                   -ALeft.fm4    * ARight.fm14
                   -ALeft.fm123  * ARight.fm23
                   -ALeft.fm124  * ARight.fm24
                   -ALeft.fm134  * ARight.fm34;
  result.fm2 :=     ALeft.fm1    * ARight.fm12
                   -ALeft.fm3    * ARight.fm23
                   -ALeft.fm4    * ARight.fm24
                   +ALeft.fm123  * ARight.fm13
                   +ALeft.fm124  * ARight.fm14
                   -ALeft.fm234  * ARight.fm34;
  result.fm3 :=     ALeft.fm1    * ARight.fm13
                   +ALeft.fm2    * ARight.fm23
                   -ALeft.fm4    * ARight.fm34
                   -ALeft.fm123  * ARight.fm12
                   +ALeft.fm134  * ARight.fm14
                   +ALeft.fm234  * ARight.fm24;
  result.fm4 :=     ALeft.fm1    * ARight.fm14
                   +ALeft.fm2    * ARight.fm24
                   +ALeft.fm3    * ARight.fm34
                   -ALeft.fm124  * ARight.fm12
                   -ALeft.fm134  * ARight.fm13
                   -ALeft.fm234  * ARight.fm23;
  result.fm12 :=    ALeft.fm0    * ARight.fm12
                   -ALeft.fm13   * ARight.fm23
                   -ALeft.fm14   * ARight.fm24
                   +ALeft.fm23   * ARight.fm13
                   +ALeft.fm24   * ARight.fm14
                   -ALeft.fm1234 * ARight.fm34;
  result.fm13 :=    ALeft.fm0    * ARight.fm13
                   +ALeft.fm12   * ARight.fm23
                   -ALeft.fm14   * ARight.fm34
                   -ALeft.fm23   * ARight.fm12
                   +ALeft.fm34   * ARight.fm14
                   +ALeft.fm1234 * ARight.fm24;
  result.fm14 :=    ALeft.fm0    * ARight.fm14
                   +ALeft.fm12   * ARight.fm24
                   +ALeft.fm13   * ARight.fm34
                   -ALeft.fm24   * ARight.fm12
                   -ALeft.fm34   * ARight.fm13
                   -ALeft.fm1234 * ARight.fm23;
  result.fm23 :=    ALeft.fm0    * ARight.fm23
                   -ALeft.fm12   * ARight.fm13
                   +ALeft.fm13   * ARight.fm12
                   -ALeft.fm24   * ARight.fm34
                   +ALeft.fm34   * ARight.fm24
                   -ALeft.fm1234 * ARight.fm14;
  result.fm24 :=    ALeft.fm0    * ARight.fm24
                   -ALeft.fm12   * ARight.fm14
                   +ALeft.fm14   * ARight.fm12
                   +ALeft.fm23   * ARight.fm34
                   -ALeft.fm34   * ARight.fm23
                   +ALeft.fm1234 * ARight.fm13;
  result.fm34 :=    ALeft.fm0    * ARight.fm34
                   -ALeft.fm13   * ARight.fm14
                   +ALeft.fm14   * ARight.fm13
                   -ALeft.fm23   * ARight.fm24
                   +ALeft.fm24   * ARight.fm23
                   -ALeft.fm1234 * ARight.fm12;
  result.fm123 :=   ALeft.fm1    * ARight.fm23
                   -ALeft.fm2    * ARight.fm13
                   +ALeft.fm3    * ARight.fm12
                   -ALeft.fm124  * ARight.fm34
                   +ALeft.fm134  * ARight.fm24
                   -ALeft.fm234  * ARight.fm14;
  result.fm124 :=   ALeft.fm1    * ARight.fm24
                   -ALeft.fm2    * ARight.fm14
                   +ALeft.fm4    * ARight.fm12
                   +ALeft.fm123  * ARight.fm34
                   -ALeft.fm134  * ARight.fm23
                   +ALeft.fm234  * ARight.fm13;
  result.fm134 :=   ALeft.fm1    * ARight.fm34
                   -ALeft.fm3    * ARight.fm14
                   +ALeft.fm4    * ARight.fm13
                   -ALeft.fm123  * ARight.fm24
                   +ALeft.fm124  * ARight.fm23
                   -ALeft.fm234  * ARight.fm12;
  result.fm234 :=   ALeft.fm2    * ARight.fm34
                   -ALeft.fm3    * ARight.fm24
                   +ALeft.fm4    * ARight.fm23
                   +ALeft.fm123  * ARight.fm14
                   -ALeft.fm124  * ARight.fm13
                   +ALeft.fm134  * ARight.fm12;
  result.fm1234 :=  ALeft.fm12   * ARight.fm34
                   -ALeft.fm13   * ARight.fm24
                   +ALeft.fm14   * ARight.fm23
                   +ALeft.fm23   * ARight.fm14
                   -ALeft.fm24   * ARight.fm13
                   +ALeft.fm34   * ARight.fm12;
end;

class operator TBivector./(const ALeft, ARight: TBivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TBivector; const ARight: double): TBivector;
begin
  result.fm12 := ALeft.fm12 / ARight;
  result.fm13 := ALeft.fm13 / ARight;
  result.fm14 := ALeft.fm14 / ARight;
  result.fm23 := ALeft.fm23 / ARight;
  result.fm24 := ALeft.fm24 / ARight;
  result.fm34 := ALeft.fm34 / ARight;
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

class operator TBivector./(const ALeft: TBivector; const ARight: TQuadrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TBivector./(const ALeft: TQuadrivector; const ARight: TBivector): TMultivector;
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
  result.fm0    := 0.0;
  result.fm1    := AValue.fm1;
  result.fm2    := AValue.fm2;
  result.fm3    := AValue.fm3;
  result.fm4    := AValue.fm4;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := 0.0;
end;

class operator TVector.:=(const AValue: TMultivector): TVector;
begin
  result.fm1 := AValue.fm1;
  result.fm2 := AValue.fm2;
  result.fm3 := AValue.fm3;
  result.fm4 := AValue.fm4;
end;

class operator TVector.<>(const ALeft, ARight: TVector): boolean;
begin
  result := (ALeft.fm1 <> ARight.fm1) or
            (ALeft.fm2 <> ARight.fm2) or
            (ALeft.fm3 <> ARight.fm3) or
            (ALeft.fm4 <> ARight.fm4);
end;

class operator TVector.<>(const ALeft: TMultivector; const ARight: TVector): boolean;
begin
  result := (ALeft.fm0    <>        0.0) or
            (ALeft.fm1    <> ARight.fm1) or
            (ALeft.fm2    <> ARight.fm2) or
            (ALeft.fm3    <> ARight.fm3) or
            (ALeft.fm4    <> ARight.fm4) or
            (ALeft.fm12   <>        0.0) or
            (ALeft.fm13   <>        0.0) or
            (ALeft.fm14   <>        0.0) or
            (ALeft.fm23   <>        0.0) or
            (ALeft.fm24   <>        0.0) or
            (ALeft.fm34   <>        0.0) or
            (ALeft.fm123  <>        0.0) or
            (ALeft.fm124  <>        0.0) or
            (ALeft.fm134  <>        0.0) or
            (ALeft.fm234  <>        0.0) or
            (ALeft.fm1234 <>        0.0);
end;

class operator TVector.<>(const ALeft: TVector; const ARight: TMultivector): boolean;
begin
  result := (ARight.fm0    <>       0.0) or
            (ARight.fm1    <> ALeft.fm1) or
            (ARight.fm2    <> ALeft.fm2) or
            (ARight.fm3    <> ALeft.fm3) or
            (ARight.fm4    <> ALeft.fm4) or
            (ARight.fm12   <>       0.0) or
            (ARight.fm13   <>       0.0) or
            (ARight.fm14   <>       0.0) or
            (ARight.fm23   <>       0.0) or
            (ARight.fm24   <>       0.0) or
            (ARight.fm34   <>       0.0) or
            (ARight.fm123  <>       0.0) or
            (ARight.fm124  <>       0.0) or
            (ARight.fm134  <>       0.0) or
            (ARight.fm234  <>       0.0) or
            (ARight.fm1234 <>       0.0);
end;

class operator TVector.=(const ALeft, ARight: TVector): boolean;
begin
  result := (ALeft.fm1 = ARight.fm1) and
            (ALeft.fm2 = ARight.fm2) and
            (ALeft.fm3 = ARight.fm3) and
            (ALeft.fm4 = ARight.fm4);
end;

class operator TVector.=(const ALeft: TVector; const ARight: TMultivector): boolean;
begin
  result := (ARight.fm0    =       0.0) and
            (ARight.fm1    = ALeft.fm1) and
            (ARight.fm2    = ALeft.fm2) and
            (ARight.fm3    = ALeft.fm3) and
            (ARight.fm4    = ALeft.fm4) and
            (ARight.fm12   =       0.0) and
            (ARight.fm13   =       0.0) and
            (ARight.fm14   =       0.0) and
            (ARight.fm23   =       0.0) and
            (ARight.fm24   =       0.0) and
            (ARight.fm34   =       0.0) and
            (ARight.fm123  =       0.0) and
            (ARight.fm124  =       0.0) and
            (ARight.fm134  =       0.0) and
            (ARight.fm234  =       0.0) and
            (ARight.fm1234 =       0.0);
end;

class operator TVector.=(const ALeft: TMultivector; const ARight: TVector): boolean;
begin
  result := (ALeft.fm0    =        0.0) and
            (ALeft.fm1    = ARight.fm1) and
            (ALeft.fm2    = ARight.fm2) and
            (ALeft.fm3    = ARight.fm3) and
            (ALeft.fm4    = ARight.fm4) and
            (ALeft.fm12   =        0.0) and
            (ALeft.fm13   =        0.0) and
            (ALeft.fm14   =        0.0) and
            (ALeft.fm23   =        0.0) and
            (ALeft.fm24   =        0.0) and
            (ALeft.fm34   =        0.0) and
            (ALeft.fm123  =        0.0) and
            (ALeft.fm124  =        0.0) and
            (ALeft.fm134  =        0.0) and
            (ALeft.fm234  =        0.0) and
            (ALeft.fm1234 =        0.0);
end;

class operator TVector.+(const ALeft, ARight: TVector): TVector;
begin
  result.fm1 := ALeft.fm1 + ARight.fm1;
  result.fm2 := ALeft.fm2 + ARight.fm2;
  result.fm3 := ALeft.fm3 + ARight.fm3;
  result.fm4 := ALeft.fm4 + ARight.fm4;
end;

class operator TVector.+(const ALeft: TVector; const ARight: double): TMultivector;
begin
  result.fm0    := ARight;
  result.fm1    := ALeft.fm1;
  result.fm2    := ALeft.fm2;
  result.fm3    := ALeft.fm3;
  result.fm4    := ALeft.fm4;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := 0.0;
end;

class operator TVector.+(const ALeft: double; const ARight: TVector): TMultivector;
begin
  result.fm0    := ALeft;
  result.fm1    := ARight.fm1;
  result.fm2    := ARight.fm2;
  result.fm3    := ARight.fm3;
  result.fm4    := ARight.fm4;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := 0.0;
end;

class operator TVector.+(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := ALeft.fm1;
  result.fm2    := ALeft.fm2;
  result.fm3    := ALeft.fm3;
  result.fm4    := ALeft.fm4;
  result.fm12   := ARight.fm12;
  result.fm13   := ARight.fm13;
  result.fm14   := ARight.fm14;
  result.fm23   := ARight.fm23;
  result.fm24   := ARight.fm24;
  result.fm34   := ARight.fm34;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := 0.0;
end;

class operator TVector.+(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := ARight.fm1;
  result.fm2    := ARight.fm2;
  result.fm3    := ARight.fm3;
  result.fm4    := ARight.fm4;
  result.fm12   := ALeft.fm12;
  result.fm13   := ALeft.fm13;
  result.fm14   := ALeft.fm14;
  result.fm23   := ALeft.fm23;
  result.fm24   := ALeft.fm24;
  result.fm34   := ALeft.fm34;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := 0.0;
end;

class operator TVector.+(const ALeft: TVector; const ARight: TTrivector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := ALeft.fm1;
  result.fm2    := ALeft.fm2;
  result.fm3    := ALeft.fm3;
  result.fm4    := ALeft.fm4;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := ARight.fm123;
  result.fm124  := ARight.fm124;
  result.fm134  := ARight.fm134;
  result.fm234  := ARight.fm234;
  result.fm1234 := 0.0;
end;

class operator TVector.+(const ALeft: TTrivector; const ARight: TVector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := ARight.fm1;
  result.fm2    := ARight.fm2;
  result.fm3    := ARight.fm3;
  result.fm4    := ARight.fm4;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := ALeft.fm123;
  result.fm124  := ALeft.fm124;
  result.fm134  := ALeft.fm134;
  result.fm234  := ALeft.fm234;
  result.fm1234 := 0.0;
end;

class operator TVector.+(const ALeft: TVector; const ARight: TQuadrivector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := ALeft.fm1;
  result.fm2    := ALeft.fm2;
  result.fm3    := ALeft.fm3;
  result.fm4    := ALeft.fm4;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := ARight.fm1234;
end;

class operator TVector.+(const ALeft: TQuadrivector; const ARight: TVector): TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := ARight.fm1;
  result.fm2    := ARight.fm2;
  result.fm3    := ARight.fm3;
  result.fm4    := ARight.fm4;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := ALeft.fm1234;
end;

class operator TVector.+(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result.fm0    := ARight.fm0;
  result.fm1    := ARight.fm1 + ALeft.fm1;
  result.fm2    := ARight.fm2 + ALeft.fm2;
  result.fm3    := ARight.fm3 + ALeft.fm3;
  result.fm4    := ARight.fm4 + ALeft.fm4;
  result.fm12   := ARight.fm12;
  result.fm13   := ARight.fm13;
  result.fm14   := ARight.fm14;
  result.fm23   := ARight.fm23;
  result.fm24   := ARight.fm24;
  result.fm34   := ARight.fm34;
  result.fm123  := ARight.fm123;
  result.fm124  := ARight.fm124;
  result.fm134  := ARight.fm134;
  result.fm234  := ARight.fm234;
  result.fm1234 := ARight.fm1234;
end;

class operator TVector.+(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result.fm0    := ALeft.fm0;
  result.fm1    := ALeft.fm1 + ARight.fm1;
  result.fm2    := ALeft.fm2 + ARight.fm2;
  result.fm3    := ALeft.fm3 + ARight.fm3;
  result.fm4    := ALeft.fm4 + ARight.fm4;
  result.fm12   := ALeft.fm12;
  result.fm13   := ALeft.fm13;
  result.fm14   := ALeft.fm14;
  result.fm23   := ALeft.fm23;
  result.fm24   := ALeft.fm24;
  result.fm34   := ALeft.fm34;
  result.fm123  := ALeft.fm123;
  result.fm124  := ALeft.fm124;
  result.fm134  := ALeft.fm134;
  result.fm234  := ALeft.fm234;
  result.fm1234 := ALeft.fm1234;
end;

class operator TVector.-(const ASelf: TVector): TVector;
begin
  result.fm1 := -ASelf.fm1;
  result.fm2 := -ASelf.fm2;
  result.fm3 := -ASelf.fm3;
  result.fm4 := -ASelf.fm4;
end;

class operator TVector.-(const ALeft, ARight: TVector): TVector;
begin
  result.fm1 := ALeft.fm1 - ARight.fm1;
  result.fm2 := ALeft.fm2 - ARight.fm2;
  result.fm3 := ALeft.fm3 - ARight.fm3;
  result.fm4 := ALeft.fm4 - ARight.fm4;
end;

class operator TVector.-(const ALeft: TVector; const ARight: double): TMultivector;
begin
  result.fm0    := -ARight;
  result.fm1    :=  ALeft.fm1;
  result.fm2    :=  ALeft.fm2;
  result.fm3    :=  ALeft.fm3;
  result.fm4    :=  ALeft.fm4;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  0.0;
end;

class operator TVector.-(const ALeft: double; const ARight: TVector): TMultivector;
begin
  result.fm0    :=  ALeft;
  result.fm1    := -ARight.fm1;
  result.fm2    := -ARight.fm2;
  result.fm3    := -ARight.fm3;
  result.fm4    := -ARight.fm4;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  0.0;
end;

class operator TVector.-(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  ALeft.fm1;
  result.fm2    :=  ALeft.fm2;
  result.fm3    :=  ALeft.fm3;
  result.fm4    :=  ALeft.fm4;
  result.fm12   := -ARight.fm12;
  result.fm13   := -ARight.fm13;
  result.fm14   := -ARight.fm14;
  result.fm23   := -ARight.fm23;
  result.fm24   := -ARight.fm24;
  result.fm34   := -ARight.fm34;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  0.0;
end;

class operator TVector.-(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    := -ARight.fm1;
  result.fm2    := -ARight.fm2;
  result.fm3    := -ARight.fm3;
  result.fm4    := -ARight.fm4;
  result.fm12   :=  ALeft.fm12;
  result.fm13   :=  ALeft.fm13;
  result.fm14   :=  ALeft.fm14;
  result.fm23   :=  ALeft.fm23;
  result.fm24   :=  ALeft.fm24;
  result.fm34   :=  ALeft.fm34;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  0.0;
end;

class operator TVector.-(const ALeft: TVector; const ARight: TTrivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  ALeft.fm1;
  result.fm2    :=  ALeft.fm2;
  result.fm3    :=  ALeft.fm3;
  result.fm4    :=  ALeft.fm4;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  := -ARight.fm123;
  result.fm124  := -ARight.fm124;
  result.fm134  := -ARight.fm134;
  result.fm234  := -ARight.fm234;
  result.fm1234 :=  0.0;
end;

class operator TVector.-(const ALeft: TTrivector; const ARight: TVector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    := -ARight.fm1;
  result.fm2    := -ARight.fm2;
  result.fm3    := -ARight.fm3;
  result.fm4    := -ARight.fm4;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  ALeft.fm123;
  result.fm124  :=  ALeft.fm124;
  result.fm134  :=  ALeft.fm134;
  result.fm234  :=  ALeft.fm234;
  result.fm1234 :=  0.0;
end;

class operator TVector.-(const ALeft: TVector; const ARight: TQuadrivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  ALeft.fm1;
  result.fm2    :=  ALeft.fm2;
  result.fm3    :=  ALeft.fm3;
  result.fm4    :=  ALeft.fm4;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 := -ARight.fm1234;
end;

class operator TVector.-(const ALeft: TQuadrivector; const ARight: TVector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    := -ARight.fm1;
  result.fm2    := -ARight.fm2;
  result.fm3    := -ARight.fm3;
  result.fm4    := -ARight.fm4;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  ALeft.fm1234;
end;

class operator TVector.-(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result.fm0    := -ARight.fm0;
  result.fm1    :=  ALeft.fm1 - ARight.fm1;
  result.fm2    :=  ALeft.fm2 - ARight.fm2;
  result.fm3    :=  ALeft.fm3 - ARight.fm3;
  result.fm4    :=  ALeft.fm4 - ARight.fm4;
  result.fm12   := -ARight.fm12;
  result.fm13   := -ARight.fm13;
  result.fm14   := -ARight.fm14;
  result.fm23   := -ARight.fm23;
  result.fm24   := -ARight.fm24;
  result.fm34   := -ARight.fm34;
  result.fm123  := -ARight.fm123;
  result.fm124  := -ARight.fm124;
  result.fm134  := -ARight.fm134;
  result.fm234  := -ARight.fm234;
  result.fm1234 := -ARight.fm1234;
end;

class operator TVector.-(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result.fm0    := ALeft.fm0;
  result.fm1    := ALeft.fm1 - ARight.fm1;
  result.fm2    := ALeft.fm2 - ARight.fm2;
  result.fm3    := ALeft.fm3 - ARight.fm3;
  result.fm4    := ALeft.fm4 - ARight.fm4;
  result.fm12   := ALeft.fm12;
  result.fm13   := ALeft.fm13;
  result.fm14   := ALeft.fm14;
  result.fm23   := ALeft.fm23;
  result.fm24   := ALeft.fm24;
  result.fm34   := ALeft.fm34;
  result.fm123  := ALeft.fm123;
  result.fm124  := ALeft.fm124;
  result.fm134  := ALeft.fm134;
  result.fm234  := ALeft.fm234;
  result.fm1234 := ALeft.fm1234;
end;

class operator TVector.*(const ALeft: double; const ARight: TVector): TVector;
begin
  result.fm1 := ALeft * ARight.fm1;
  result.fm2 := ALeft * ARight.fm2;
  result.fm3 := ALeft * ARight.fm3;
  result.fm4 := ALeft * ARight.fm4;
end;

class operator TVector.*(const ALeft: TVector; const ARight: double): TVector;
begin
  result.fm1 := ALeft.fm1 * ARight;
  result.fm2 := ALeft.fm2 * ARight;
  result.fm3 := ALeft.fm3 * ARight;
  result.fm4 := ALeft.fm4 * ARight;
end;

class operator TVector.*(const ALeft, ARight: TVector): TMultivector;
begin
  result.fm0 :=     ALeft.fm1    * ARight.fm1
                   +ALeft.fm2    * ARight.fm2
                   +ALeft.fm3    * ARight.fm3
                   +ALeft.fm4    * ARight.fm4;
  result.fm1 :=     0.0;
  result.fm2 :=     0.0;
  result.fm3 :=     0.0;
  result.fm4 :=     0.0;
  result.fm12 :=    ALeft.fm1    * ARight.fm2
                   -ALeft.fm2    * ARight.fm1;
  result.fm13 :=    ALeft.fm1    * ARight.fm3
                   -ALeft.fm3    * ARight.fm1;
  result.fm14 :=    ALeft.fm1    * ARight.fm4
                   -ALeft.fm4    * ARight.fm1;
  result.fm23 :=    ALeft.fm2    * ARight.fm3
                   -ALeft.fm3    * ARight.fm2;
  result.fm24 :=    ALeft.fm2    * ARight.fm4
                   -ALeft.fm4    * ARight.fm2;
  result.fm34 :=    ALeft.fm3    * ARight.fm4
                   -ALeft.fm4    * ARight.fm3;
  result.fm123 :=   0.0;
  result.fm124 :=   0.0;
  result.fm134 :=   0.0;
  result.fm234 :=   0.0;
  result.fm1234 :=  0.0;
end;

class operator TVector.*(const ALeft: TVector; const ARight: TBivector): TMultivector;
begin
  result.fm0 :=     0.0;
  result.fm1 :=    -ALeft.fm2    * ARight.fm12
                   -ALeft.fm3    * ARight.fm13
                   -ALeft.fm4    * ARight.fm14;
  result.fm2 :=     ALeft.fm1    * ARight.fm12
                   -ALeft.fm3    * ARight.fm23
                   -ALeft.fm4    * ARight.fm24;
  result.fm3 :=     ALeft.fm1    * ARight.fm13
                   +ALeft.fm2    * ARight.fm23
                   -ALeft.fm4    * ARight.fm34;
  result.fm4 :=     ALeft.fm1    * ARight.fm14
                   +ALeft.fm2    * ARight.fm24
                   +ALeft.fm3    * ARight.fm34;
  result.fm12 :=    0.0;
  result.fm13 :=    0.0;
  result.fm14 :=    0.0;
  result.fm23 :=    0.0;
  result.fm24 :=    0.0;
  result.fm34 :=    0.0;
  result.fm123 :=   ALeft.fm1    * ARight.fm23
                   -ALeft.fm2    * ARight.fm13
                   +ALeft.fm3    * ARight.fm12;
  result.fm124 :=   ALeft.fm1    * ARight.fm24
                   -ALeft.fm2    * ARight.fm14
                   +ALeft.fm4    * ARight.fm12;
  result.fm134 :=   ALeft.fm1    * ARight.fm34
                   -ALeft.fm3    * ARight.fm14
                   +ALeft.fm4    * ARight.fm13;
  result.fm234 :=   ALeft.fm2    * ARight.fm34
                   -ALeft.fm3    * ARight.fm24
                   +ALeft.fm4    * ARight.fm23;
  result.fm1234 :=  0.0;
end;

class operator TVector.*(const ALeft: TBivector; const ARight: TVector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  ALeft.fm12 * ARight.fm2 +ALeft.fm13 * ARight.fm3 +ALeft.fm14 * ARight.fm4;
  result.fm2    := -ALeft.fm12 * ARight.fm1 +ALeft.fm23 * ARight.fm3 +ALeft.fm24 * ARight.fm4;
  result.fm3    := -ALeft.fm13 * ARight.fm1 -ALeft.fm23 * ARight.fm2 +ALeft.fm34 * ARight.fm4;
  result.fm4    := -ALeft.fm14 * ARight.fm1 -ALeft.fm24 * ARight.fm2 -ALeft.fm34 * ARight.fm3;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  ALeft.fm12 * ARight.fm3 -ALeft.fm13 * ARight.fm2 +ALeft.fm23 * ARight.fm1;
  result.fm124  :=  ALeft.fm12 * ARight.fm4 -ALeft.fm14 * ARight.fm2 +ALeft.fm24 * ARight.fm1;
  result.fm134  :=  ALeft.fm13 * ARight.fm4 -ALeft.fm14 * ARight.fm3 +ALeft.fm34 * ARight.fm1;
  result.fm234  :=  ALeft.fm23 * ARight.fm4 -ALeft.fm24 * ARight.fm3 +ALeft.fm34 * ARight.fm2;
  result.fm1234 :=  0.0;
end;

class operator TVector.*(const ALeft: TVector; const ARight: TTrivector): TMultivector;
begin
  result.fm0 :=     0.0;
  result.fm1 :=     0.0;
  result.fm2 :=     0.0;
  result.fm3 :=     0.0;
  result.fm4 :=     0.0;
  result.fm12 :=    ALeft.fm3    * ARight.fm123
                   +ALeft.fm4    * ARight.fm124;
  result.fm13 :=   -ALeft.fm2    * ARight.fm123
                   +ALeft.fm4    * ARight.fm134;
  result.fm14 :=   -ALeft.fm2    * ARight.fm124
                   -ALeft.fm3    * ARight.fm134;
  result.fm23 :=    ALeft.fm1    * ARight.fm123
                   +ALeft.fm4    * ARight.fm234;
  result.fm24 :=    ALeft.fm1    * ARight.fm124
                   -ALeft.fm3    * ARight.fm234;
  result.fm34 :=    ALeft.fm1    * ARight.fm134
                   +ALeft.fm2    * ARight.fm234;
  result.fm123 :=   0.0;
  result.fm124 :=   0.0;
  result.fm134 :=   0.0;
  result.fm234 :=   0.0;
  result.fm1234 :=  ALeft.fm1    * ARight.fm234
                   -ALeft.fm2    * ARight.fm134
                   +ALeft.fm3    * ARight.fm124
                   -ALeft.fm4    * ARight.fm123;
end;

class operator TVector.*(const ALeft: TTrivector; const ARight: TVector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   :=  ALeft.fm123 * ARight.fm3 +ALeft.fm124 * ARight.fm4;
  result.fm13   := -ALeft.fm123 * ARight.fm2 +ALeft.fm134 * ARight.fm4;
  result.fm14   := -ALeft.fm124 * ARight.fm2 -ALeft.fm134 * ARight.fm3;
  result.fm23   :=  ALeft.fm123 * ARight.fm1 +ALeft.fm234 * ARight.fm4;
  result.fm24   :=  ALeft.fm124 * ARight.fm1 -ALeft.fm234 * ARight.fm3;
  result.fm34   :=  ALeft.fm134 * ARight.fm1 +ALeft.fm234 * ARight.fm2;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  ALeft.fm123 * ARight.fm4 -ALeft.fm124 * ARight.fm3
                   +ALeft.fm134 * ARight.fm2 -ALeft.fm234 * ARight.fm1;
end;

class operator TVector.*(const ALeft: TVector; const ARight: TQuadrivector): TTrivector;
begin
  result.fm123 := -ALeft.fm4 * ARight.fm1234;
  result.fm124 :=  ALeft.fm3 * ARight.fm1234;
  result.fm134 := -ALeft.fm2 * ARight.fm1234;
  result.fm234 :=  ALeft.fm1 * ARight.fm1234;
end;

class operator TVector.*(const ALeft: TQuadrivector; const ARight: TVector): TTrivector;
begin
  result.fm123 :=  ALeft.fm1234 * ARight.fm4;
  result.fm124 := -ALeft.fm1234 * ARight.fm3;
  result.fm134 :=  ALeft.fm1234 * ARight.fm2;
  result.fm234 := -ALeft.fm1234 * ARight.fm1;
end;

class operator TVector.*(const ALeft: TVector; const ARight: TMultivector): TMultivector;
begin
  result.fm0 :=     ALeft.fm1    * ARight.fm1
                   +ALeft.fm2    * ARight.fm2
                   +ALeft.fm3    * ARight.fm3
                   +ALeft.fm4    * ARight.fm4;
  result.fm1 :=     ALeft.fm1    * ARight.fm0
                   -ALeft.fm2    * ARight.fm12
                   -ALeft.fm3    * ARight.fm13
                   -ALeft.fm4    * ARight.fm14;
  result.fm2 :=     ALeft.fm1    * ARight.fm12
                   +ALeft.fm2    * ARight.fm0
                   -ALeft.fm3    * ARight.fm23
                   -ALeft.fm4    * ARight.fm24;
  result.fm3 :=     ALeft.fm1    * ARight.fm13
                   +ALeft.fm2    * ARight.fm23
                   +ALeft.fm3    * ARight.fm0
                   -ALeft.fm4    * ARight.fm34;
  result.fm4 :=     ALeft.fm1    * ARight.fm14
                   +ALeft.fm2    * ARight.fm24
                   +ALeft.fm3    * ARight.fm34
                   +ALeft.fm4    * ARight.fm0;
  result.fm12 :=    ALeft.fm1    * ARight.fm2
                   -ALeft.fm2    * ARight.fm1
                   +ALeft.fm3    * ARight.fm123
                   +ALeft.fm4    * ARight.fm124;
  result.fm13 :=    ALeft.fm1    * ARight.fm3
                   -ALeft.fm2    * ARight.fm123
                   -ALeft.fm3    * ARight.fm1
                   +ALeft.fm4    * ARight.fm134;
  result.fm14 :=    ALeft.fm1    * ARight.fm4
                   -ALeft.fm2    * ARight.fm124
                   -ALeft.fm3    * ARight.fm134
                   -ALeft.fm4    * ARight.fm1;
  result.fm23 :=    ALeft.fm1    * ARight.fm123
                   +ALeft.fm2    * ARight.fm3
                   -ALeft.fm3    * ARight.fm2
                   +ALeft.fm4    * ARight.fm234;
  result.fm24 :=    ALeft.fm1    * ARight.fm124
                   +ALeft.fm2    * ARight.fm4
                   -ALeft.fm3    * ARight.fm234
                   -ALeft.fm4    * ARight.fm2;
  result.fm34 :=    ALeft.fm1    * ARight.fm134
                   +ALeft.fm2    * ARight.fm234
                   +ALeft.fm3    * ARight.fm4
                   -ALeft.fm4    * ARight.fm3;
  result.fm123 :=   ALeft.fm1    * ARight.fm23
                   -ALeft.fm2    * ARight.fm13
                   +ALeft.fm3    * ARight.fm12
                   -ALeft.fm4    * ARight.fm1234;
  result.fm124 :=   ALeft.fm1    * ARight.fm24
                   -ALeft.fm2    * ARight.fm14
                   +ALeft.fm3    * ARight.fm1234
                   +ALeft.fm4    * ARight.fm12;
  result.fm134 :=   ALeft.fm1    * ARight.fm34
                   -ALeft.fm2    * ARight.fm1234
                   -ALeft.fm3    * ARight.fm14
                   +ALeft.fm4    * ARight.fm13;
  result.fm234 :=   ALeft.fm1    * ARight.fm1234
                   +ALeft.fm2    * ARight.fm34
                   -ALeft.fm3    * ARight.fm24
                   +ALeft.fm4    * ARight.fm23;
  result.fm1234 :=  ALeft.fm1    * ARight.fm234
                   -ALeft.fm2    * ARight.fm134
                   +ALeft.fm3    * ARight.fm124
                   -ALeft.fm4    * ARight.fm123;
end;

class operator TVector.*(const ALeft: TMultivector; const ARight: TVector): TMultivector;
begin
  result.fm0 :=
                   +ALeft.fm1    * ARight.fm1
                   +ALeft.fm2    * ARight.fm2
                   +ALeft.fm3    * ARight.fm3
                   +ALeft.fm4    * ARight.fm4;
  result.fm1 :=     ALeft.fm0    * ARight.fm1
                   +ALeft.fm12   * ARight.fm2
                   +ALeft.fm13   * ARight.fm3
                   +ALeft.fm14   * ARight.fm4;
  result.fm2 :=     ALeft.fm0    * ARight.fm2
                   -ALeft.fm12   * ARight.fm1
                   +ALeft.fm23   * ARight.fm3
                   +ALeft.fm24   * ARight.fm4;
  result.fm3 :=     ALeft.fm0    * ARight.fm3
                   -ALeft.fm13   * ARight.fm1
                   -ALeft.fm23   * ARight.fm2
                   +ALeft.fm34   * ARight.fm4;
  result.fm4 :=     ALeft.fm0    * ARight.fm4
                   -ALeft.fm14   * ARight.fm1
                   -ALeft.fm24   * ARight.fm2
                   -ALeft.fm34   * ARight.fm3;
  result.fm12 :=    ALeft.fm1    * ARight.fm2
                   -ALeft.fm2    * ARight.fm1
                   +ALeft.fm123  * ARight.fm3
                   +ALeft.fm124  * ARight.fm4;
  result.fm13 :=    ALeft.fm1    * ARight.fm3
                   -ALeft.fm3    * ARight.fm1
                   -ALeft.fm123  * ARight.fm2
                   +ALeft.fm134  * ARight.fm4;
  result.fm14 :=    ALeft.fm1    * ARight.fm4
                   -ALeft.fm4    * ARight.fm1
                   -ALeft.fm124  * ARight.fm2
                   -ALeft.fm134  * ARight.fm3;
  result.fm23 :=    ALeft.fm2    * ARight.fm3
                   -ALeft.fm3    * ARight.fm2
                   +ALeft.fm123  * ARight.fm1
                   +ALeft.fm234  * ARight.fm4;
  result.fm24 :=    ALeft.fm2    * ARight.fm4
                   -ALeft.fm4    * ARight.fm2
                   +ALeft.fm124  * ARight.fm1
                   -ALeft.fm234  * ARight.fm3;
  result.fm34 :=    ALeft.fm3    * ARight.fm4
                   -ALeft.fm4    * ARight.fm3
                   +ALeft.fm134  * ARight.fm1
                   +ALeft.fm234  * ARight.fm2;
  result.fm123 :=   ALeft.fm12   * ARight.fm3
                   -ALeft.fm13   * ARight.fm2
                   +ALeft.fm23   * ARight.fm1
                   +ALeft.fm1234 * ARight.fm4;
  result.fm124 :=   ALeft.fm12   * ARight.fm4
                   -ALeft.fm14   * ARight.fm2
                   +ALeft.fm24   * ARight.fm1
                   -ALeft.fm1234 * ARight.fm3;
  result.fm134 :=   ALeft.fm13   * ARight.fm4
                   -ALeft.fm14   * ARight.fm3
                   +ALeft.fm34   * ARight.fm1
                   +ALeft.fm1234 * ARight.fm2;
  result.fm234 :=   ALeft.fm23   * ARight.fm4
                   -ALeft.fm24   * ARight.fm3
                   +ALeft.fm34   * ARight.fm2
                   -ALeft.fm1234 * ARight.fm1;
  result.fm1234 :=  ALeft.fm123  * ARight.fm4
                   -ALeft.fm124  * ARight.fm3
                   +ALeft.fm134  * ARight.fm2
                   -ALeft.fm234  * ARight.fm1;
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
  result.fm4 := ALeft.fm4 / ARight;
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

class operator TVector./(const ALeft: TVector; const ARight: TTrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TTrivector; const ARight: TVector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TVector; const ARight: TQuadrivector): TMultivector;
begin
  result := ALeft * ARight.Reciprocal;
end;

class operator TVector./(const ALeft: TQuadrivector; const ARight: TVector): TMultivector;
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
  result.fm0    :=  fm1234;
  result.fm1    :=  fm234;
  result.fm2    := -fm134;
  result.fm3    :=  fm124;
  result.fm4    := -fm123;
  result.fm12   := -fm34;
  result.fm13   :=  fm24;
  result.fm14   := -fm23;
  result.fm23   := -fm14;
  result.fm24   :=  fm13;
  result.fm34   := -fm12;
  result.fm123  := -fm4;
  result.fm124  :=  fm3;
  result.fm134  := -fm2;
  result.fm234  :=  fm1;
  result.fm1234 :=  fm0;
end;

function TMultivectorHelper.Inverse: TMultivector;
begin
  result.fm0    :=  fm0;
  result.fm1    := -fm1;
  result.fm2    := -fm2;
  result.fm3    := -fm3;
  result.fm4    := -fm4;
  result.fm12   :=  fm12;
  result.fm13   :=  fm13;
  result.fm14   :=  fm14;
  result.fm23   :=  fm23;
  result.fm24   :=  fm24;
  result.fm34   :=  fm34;
  result.fm123  := -fm123;
  result.fm124  := -fm124;
  result.fm134  := -fm134;
  result.fm234  := -fm234;
  result.fm1234 :=  fm1234;
end;

function TMultivectorHelper.Reverse: TMultivector;
begin
  result.fm0    :=  fm0;
  result.fm1    :=  fm1;
  result.fm2    :=  fm2;
  result.fm3    :=  fm3;
  result.fm4    :=  fm4;
  result.fm12   := -fm12;
  result.fm13   := -fm13;
  result.fm14   := -fm14;
  result.fm23   := -fm23;
  result.fm24   := -fm24;
  result.fm34   := -fm34;
  result.fm123  := -fm123;
  result.fm124  := -fm124;
  result.fm134  := -fm134;
  result.fm234  := -fm234;
  result.fm1234 :=  fm1234;
end;

function TMultivectorHelper.Conjugate: TMultivector;
begin
  result.fm0    :=  fm0;
  result.fm1    := -fm1;
  result.fm2    := -fm2;
  result.fm3    := -fm3;
  result.fm4    := -fm4;
  result.fm12   := -fm12;
  result.fm13   := -fm13;
  result.fm14   := -fm14;
  result.fm23   := -fm23;
  result.fm24   := -fm24;
  result.fm34   := -fm34;
  result.fm123  :=  fm123;
  result.fm124  :=  fm124;
  result.fm134  :=  fm134;
  result.fm234  :=  fm234;
  result.fm1234 :=  fm1234;
end;

function TMultivectorHelper.Reciprocal: TMultivector;
var
  map34: TMultivector;
begin
  map34 := Self * Conjugate;
  map34.fm123  := -map34.fm123;
  map34.fm124  := -map34.fm124;
  map34.fm134  := -map34.fm134;
  map34.fm234  := -map34.fm234;
  map34.fm1234 := -map34.fm1234;

  map34  := Conjugate * map34;
  result := map34 / (Self * map34).fm0;
end;

function TMultivectorHelper.LeftReciprocal: TMultivector;
var
  map34: TMultivector;
begin
  map34 := Conjugate * Self;
  map34.fm123  := -map34.fm123;
  map34.fm124  := -map34.fm124;
  map34.fm134  := -map34.fm134;
  map34.fm234  := -map34.fm234;
  map34.fm1234 := -map34.fm1234;

  map34  := map34 * Conjugate;
  result := map34 / (map34 * Self).fm0;
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
  result :=  fm0   *fm0   +fm1  *fm1   +fm2  *fm2   +fm3  *fm3  +fm4 *fm4 +fm12*fm12
            +fm13  *fm13  +fm14 *fm14  +fm23 *fm23  +fm24 *fm24 +fm34*fm34
            +fm123 *fm123 +fm124*fm124 +fm134*fm134 +fm234*fm234
            +fm1234*fm1234;
end;

function TMultivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0 :=     fm0    * AVector.fm0
                   +fm1    * AVector.fm1
                   +fm2    * AVector.fm2
                   +fm3    * AVector.fm3
                   +fm4    * AVector.fm4
                   -fm12   * AVector.fm12
                   -fm13   * AVector.fm13
                   -fm14   * AVector.fm14
                   -fm23   * AVector.fm23
                   -fm24   * AVector.fm24
                   -fm34   * AVector.fm34
                   -fm123  * AVector.fm123
                   -fm124  * AVector.fm124
                   -fm134  * AVector.fm134
                   -fm234  * AVector.fm234
                   +fm1234 * AVector.fm1234;

  result.fm1 :=     fm0    * AVector.fm1
                   +fm1    * AVector.fm0
                   -fm2    * AVector.fm12
                   -fm3    * AVector.fm13
                   -fm4    * AVector.fm14
                   +fm12   * AVector.fm2
                   +fm13   * AVector.fm3
                   +fm14   * AVector.fm4
                   -fm23   * AVector.fm123
                   -fm24   * AVector.fm124
                   -fm34   * AVector.fm134
                   -fm123  * AVector.fm23
                   -fm124  * AVector.fm24
                   -fm134  * AVector.fm34
                   +fm234  * AVector.fm1234
                   -fm1234 * AVector.fm234;

  result.fm2 :=     fm0    * AVector.fm2
                   +fm1    * AVector.fm12
                   +fm2    * AVector.fm0
                   -fm3    * AVector.fm23
                   -fm4    * AVector.fm24
                   -fm12   * AVector.fm1
                   +fm13   * AVector.fm123
                   +fm14   * AVector.fm124
                   +fm23   * AVector.fm3
                   +fm24   * AVector.fm4
                   -fm34   * AVector.fm234
                   +fm123  * AVector.fm13
                   +fm124  * AVector.fm14
                   -fm134  * AVector.fm1234
                   -fm234  * AVector.fm34
                   +fm1234 * AVector.fm134;

  result.fm3 :=     fm0    * AVector.fm3
                   +fm1    * AVector.fm13
                   +fm2    * AVector.fm23
                   +fm3    * AVector.fm0
                   -fm4    * AVector.fm34
                   -fm12   * AVector.fm123
                   -fm13   * AVector.fm1
                   +fm14   * AVector.fm134
                   -fm23   * AVector.fm2
                   +fm24   * AVector.fm234
                   +fm34   * AVector.fm4
                   -fm123  * AVector.fm12
                   +fm124  * AVector.fm1234
                   +fm134  * AVector.fm14
                   +fm234  * AVector.fm24
                   -fm1234 * AVector.fm124;

  result.fm4 :=     fm0    * AVector.fm4
                   +fm1    * AVector.fm14
                   +fm2    * AVector.fm24
                   +fm3    * AVector.fm34
                   +fm4    * AVector.fm0
                   -fm12   * AVector.fm124
                   -fm13   * AVector.fm134
                   -fm14   * AVector.fm1
                   -fm23   * AVector.fm234
                   -fm24   * AVector.fm2
                   -fm34   * AVector.fm3
                   -fm123  * AVector.fm1234
                   -fm124  * AVector.fm12
                   -fm134  * AVector.fm13
                   -fm234  * AVector.fm23
                   +fm1234 * AVector.fm123;

  result.fm12 :=    fm0    * AVector.fm12
                   +fm3    * AVector.fm123
                   +fm4    * AVector.fm124
                   +fm12   * AVector.fm0
                   -fm34   * AVector.fm1234
                   +fm123  * AVector.fm3
                   +fm124  * AVector.fm4
                   -fm1234 * AVector.fm34;

  result.fm13 :=    fm0    * AVector.fm13
                   -fm2    * AVector.fm123
                   +fm4    * AVector.fm134
                   +fm13   * AVector.fm0
                   +fm24   * AVector.fm1234
                   -fm123  * AVector.fm2
                   +fm134  * AVector.fm4
                   +fm1234 * AVector.fm24;

  result.fm14 :=    fm0    * AVector.fm14
                   -fm2    * AVector.fm124
                   -fm3    * AVector.fm134
                   +fm14   * AVector.fm0
                   -fm23   * AVector.fm1234
                   -fm124  * AVector.fm2
                   -fm134  * AVector.fm3
                   -fm1234 * AVector.fm23;

  result.fm23 :=    fm0    * AVector.fm23
                   +fm1    * AVector.fm123
                   +fm4    * AVector.fm234
                   -fm14   * AVector.fm1234
                   +fm23   * AVector.fm0
                   +fm123  * AVector.fm1
                   +fm234  * AVector.fm4
                   -fm1234 * AVector.fm14;

  result.fm24 :=    fm0    * AVector.fm24
                   +fm1    * AVector.fm124
                   -fm3    * AVector.fm234
                   +fm13   * AVector.fm1234
                   +fm24   * AVector.fm0
                   +fm124  * AVector.fm1
                   -fm234  * AVector.fm3
                   +fm1234 * AVector.fm13;

  result.fm34 :=    fm0    * AVector.fm34
                   +fm1    * AVector.fm134
                   +fm2    * AVector.fm234
                   -fm12   * AVector.fm1234
                   +fm34   * AVector.fm0
                   +fm134  * AVector.fm1
                   +fm234  * AVector.fm2
                   -fm1234 * AVector.fm12;

  result.fm123 :=   fm0    * AVector.fm123
                   -fm4    * AVector.fm1234
                   +fm123  * AVector.fm0
                   +fm1234 * AVector.fm4;

  result.fm124 :=   fm0    * AVector.fm124
                   +fm3    * AVector.fm1234
                   +fm124  * AVector.fm0
                   -fm1234 * AVector.fm3;

  result.fm134 :=   fm0    * AVector.fm134
                   -fm2    * AVector.fm1234
                   +fm134  * AVector.fm0
                   +fm1234 * AVector.fm2;

  result.fm234 :=   fm0    * AVector.fm234
                   +fm1    * AVector.fm1234
                   +fm234  * AVector.fm0
                   -fm1234 * AVector.fm1;

  result.fm1234 :=  fm0    * AVector.fm1234
                   +fm1234 * AVector.fm0;
end;

function TMultivectorHelper.Dot(const AVector: TQuadrivector): TMultivector;
begin
  result.fm0    :=  fm1234 * AVector.fm1234;
  result.fm1    :=  fm234  * AVector.fm1234;
  result.fm2    := -fm134  * AVector.fm1234;
  result.fm3    :=  fm124  * AVector.fm1234;
  result.fm4    := -fm123  * AVector.fm1234;
  result.fm12   := -fm34   * AVector.fm1234;
  result.fm13   :=  fm24   * AVector.fm1234;
  result.fm14   := -fm23   * AVector.fm1234;
  result.fm23   := -fm14   * AVector.fm1234;
  result.fm24   :=  fm13   * AVector.fm1234;
  result.fm34   := -fm12   * AVector.fm1234;
  result.fm123  := -fm4    * AVector.fm1234;
  result.fm124  :=  fm3    * AVector.fm1234;
  result.fm134  := -fm2    * AVector.fm1234;
  result.fm234  :=  fm1    * AVector.fm1234;
  result.fm1234 :=  fm0    * AVector.fm1234;
end;

function TMultivectorHelper.Dot(const AVector: TTrivector): TMultivector;
begin
  result.fm0 :=    -fm123  * AVector.fm123
                   -fm124  * AVector.fm124
                   -fm134  * AVector.fm134
                   -fm234  * AVector.fm234;
  result.fm1 :=    -fm23   * AVector.fm123
                   -fm24   * AVector.fm124
                   -fm34   * AVector.fm134
                   -fm1234 * AVector.fm234;
  result.fm2 :=    +fm13   * AVector.fm123
                   +fm14   * AVector.fm124
                   -fm34   * AVector.fm234
                   +fm1234 * AVector.fm134;
  result.fm3 :=    -fm12   * AVector.fm123
                   +fm14   * AVector.fm134
                   +fm24   * AVector.fm234
                   -fm1234 * AVector.fm124;
  result.fm4 :=    -fm12   * AVector.fm124
                   -fm13   * AVector.fm134
                   -fm23   * AVector.fm234
                   +fm1234 * AVector.fm123;
  result.fm12 :=   +fm3    * AVector.fm123
                   +fm4    * AVector.fm124;
  result.fm13 :=   -fm2    * AVector.fm123
                   +fm4    * AVector.fm134;
  result.fm14 :=   -fm2    * AVector.fm124
                   -fm3    * AVector.fm134;
  result.fm23 :=   +fm1    * AVector.fm123
                   +fm4    * AVector.fm234;
  result.fm24 :=   +fm1    * AVector.fm124
                   -fm3    * AVector.fm234;
  result.fm34 :=   +fm1    * AVector.fm134
                   +fm2    * AVector.fm234;
  result.fm123 :=   fm0    * AVector.fm123;
  result.fm124 :=   fm0    * AVector.fm124;
  result.fm134 :=   fm0    * AVector.fm134;
  result.fm234 :=   fm0    * AVector.fm234;
  result.fm1234 :=  0.0;
end;

function TMultivectorHelper.Dot(const AVector: TBivector): TMultivector;
begin
  result.fm0 :=    -fm12   * AVector.fm12
                   -fm13   * AVector.fm13
                   -fm14   * AVector.fm14
                   -fm23   * AVector.fm23
                   -fm24   * AVector.fm24
                   -fm34   * AVector.fm34;
  result.fm1 :=    -fm2    * AVector.fm12
                   -fm3    * AVector.fm13
                   -fm4    * AVector.fm14
                   -fm123  * AVector.fm23
                   -fm124  * AVector.fm24
                   -fm134  * AVector.fm34;
  result.fm2 :=    +fm1    * AVector.fm12
                   -fm3    * AVector.fm23
                   -fm4    * AVector.fm24
                   +fm123  * AVector.fm13
                   +fm124  * AVector.fm14
                   -fm234  * AVector.fm34;
  result.fm3 :=    +fm1    * AVector.fm13
                   +fm2    * AVector.fm23
                   -fm4    * AVector.fm34
                   -fm123  * AVector.fm12
                   +fm134  * AVector.fm14
                   +fm234  * AVector.fm24;
  result.fm4 :=    +fm1    * AVector.fm14
                   +fm2    * AVector.fm24
                   +fm3    * AVector.fm34
                   -fm124  * AVector.fm12
                   -fm134  * AVector.fm13
                   -fm234  * AVector.fm23;
  result.fm12 :=    fm0    * AVector.fm12
                   -fm1234 * AVector.fm34;
  result.fm13 :=    fm0    * AVector.fm13
                   +fm1234 * AVector.fm24;
  result.fm14 :=    fm0    * AVector.fm14
                   -fm1234 * AVector.fm23;
  result.fm23 :=    fm0    * AVector.fm23
                   -fm1234 * AVector.fm14;
  result.fm24 :=    fm0    * AVector.fm24
                   +fm1234 * AVector.fm13;
  result.fm34 :=    fm0    * AVector.fm34
                   -fm1234 * AVector.fm12;
  result.fm123 :=   0.0;
  result.fm124 :=   0.0;
  result.fm134 :=   0.0;
  result.fm234 :=   0.0;
  result.fm1234 :=  0.0;
end;

function TMultivectorHelper.Dot(const AVector: TVector): TMultivector;
begin
  result.fm0 :=    +fm1    * AVector.fm1
                   +fm2    * AVector.fm2
                   +fm3    * AVector.fm3
                   +fm4    * AVector.fm4;
  result.fm1 :=     fm0    * AVector.fm1
                   +fm12   * AVector.fm2
                   +fm13   * AVector.fm3
                   +fm14   * AVector.fm4;
  result.fm2 :=     fm0    * AVector.fm2
                   -fm12   * AVector.fm1
                   +fm23   * AVector.fm3
                   +fm24   * AVector.fm4;
  result.fm3 :=     fm0    * AVector.fm3
                   -fm13   * AVector.fm1
                   -fm23   * AVector.fm2
                   +fm34   * AVector.fm4;
  result.fm4 :=     fm0    * AVector.fm4
                   -fm14   * AVector.fm1
                   -fm24   * AVector.fm2
                   -fm34   * AVector.fm3;
  result.fm12 :=   +fm123  * AVector.fm3
                   +fm124  * AVector.fm4;
  result.fm13 :=   -fm123  * AVector.fm2
                   +fm134  * AVector.fm4;
  result.fm14 :=   -fm124  * AVector.fm2
                   -fm134  * AVector.fm3;
  result.fm23 :=   +fm123  * AVector.fm1
                   +fm234  * AVector.fm4;
  result.fm24 :=   +fm124  * AVector.fm1
                   -fm234  * AVector.fm3;
  result.fm34 :=   +fm134  * AVector.fm1
                   +fm234  * AVector.fm2;
  result.fm123 :=  +fm1234 * AVector.fm4;
  result.fm124 :=  -fm1234 * AVector.fm3;
  result.fm134 :=  +fm1234 * AVector.fm2;
  result.fm234 :=  -fm1234 * AVector.fm1;
  result.fm1234 :=  0.0;
end;

function TMultivectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fm0 :=     fm0    * AVector.fm0;

  result.fm1 :=     fm0    * AVector.fm1
                   +fm1    * AVector.fm0;

  result.fm2 :=     fm0    * AVector.fm2
                   +fm2    * AVector.fm0;

  result.fm3 :=     fm0    * AVector.fm3
                   +fm3    * AVector.fm0;

  result.fm4 :=     fm0    * AVector.fm4
                   +fm4    * AVector.fm0;

  result.fm12 :=    fm0    * AVector.fm12
                   +fm1    * AVector.fm2
                   -fm2    * AVector.fm1
                   +fm12   * AVector.fm0;

  result.fm13 :=    fm0    * AVector.fm13
                   +fm1    * AVector.fm3
                   -fm3    * AVector.fm1
                   +fm13   * AVector.fm0;

  result.fm14 :=    fm0    * AVector.fm14
                   +fm1    * AVector.fm4
                   -fm4    * AVector.fm1
                   +fm14   * AVector.fm0;

  result.fm23 :=    fm0    * AVector.fm23
                   +fm2    * AVector.fm3
                   -fm3    * AVector.fm2
                   +fm23   * AVector.fm0;

  result.fm24 :=    fm0    * AVector.fm24
                   +fm2    * AVector.fm4
                   -fm4    * AVector.fm2
                   +fm24   * AVector.fm0;

  result.fm34 :=    fm0    * AVector.fm34
                   +fm3    * AVector.fm4
                   -fm4    * AVector.fm3
                   +fm34   * AVector.fm0;

  result.fm123 :=   fm0    * AVector.fm123
                   +fm1    * AVector.fm23
                   -fm2    * AVector.fm13
                   +fm3    * AVector.fm12
                   +fm12   * AVector.fm3
                   -fm13   * AVector.fm2
                   +fm23   * AVector.fm1
                   +fm123  * AVector.fm0;

  result.fm124 :=   fm0    * AVector.fm124
                   +fm1    * AVector.fm24
                   -fm2    * AVector.fm14
                   +fm4    * AVector.fm12
                   +fm12   * AVector.fm4
                   -fm14   * AVector.fm2
                   +fm24   * AVector.fm1
                   +fm124  * AVector.fm0;

  result.fm134 :=   fm0    * AVector.fm134
                   +fm1    * AVector.fm34
                   -fm3    * AVector.fm14
                   +fm4    * AVector.fm13
                   +fm13   * AVector.fm4
                   -fm14   * AVector.fm3
                   +fm34   * AVector.fm1
                   +fm134  * AVector.fm0;

  result.fm234 :=   fm0    * AVector.fm234
                   +fm2    * AVector.fm34
                   -fm3    * AVector.fm24
                   +fm4    * AVector.fm23
                   +fm23   * AVector.fm4
                   -fm24   * AVector.fm3
                   +fm34   * AVector.fm2
                   +fm234  * AVector.fm0;

  result.fm1234 :=  fm0    * AVector.fm1234
                   +fm1    * AVector.fm234
                   -fm2    * AVector.fm134
                   +fm3    * AVector.fm124
                   -fm4    * AVector.fm123
                   +fm12   * AVector.fm34
                   -fm13   * AVector.fm24
                   +fm14   * AVector.fm23
                   +fm23   * AVector.fm14
                   -fm24   * AVector.fm13
                   +fm34   * AVector.fm12
                   +fm123  * AVector.fm4
                   -fm124  * AVector.fm3
                   +fm134  * AVector.fm2
                   -fm234  * AVector.fm1
                   +fm1234 * AVector.fm0;
end;

function TMultivectorHelper.Wedge(const AVector: TQuadrivector): TQuadrivector;
begin
  result.fm1234 := fm0 * AVector.fm1234;
end;

function TMultivectorHelper.Wedge(const AVector: TTrivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  fm0 * AVector.fm123;
  result.fm124  :=  fm0 * AVector.fm124;
  result.fm134  :=  fm0 * AVector.fm134;
  result.fm234  :=  fm0 * AVector.fm234;
  result.fm1234 := +fm1 * AVector.fm234
                   -fm2 * AVector.fm134
                   +fm3 * AVector.fm124
                   -fm4 * AVector.fm123;
end;

function TMultivectorHelper.Wedge(const AVector: TBivector): TMultivector;
begin
  result.fm0   :=   0.0;
  result.fm1   :=   0.0;
  result.fm2   :=   0.0;
  result.fm3   :=   0.0;
  result.fm4   :=   0.0;
  result.fm12  :=   fm0    * AVector.fm12;
  result.fm13  :=   fm0    * AVector.fm13;
  result.fm14  :=   fm0    * AVector.fm14;
  result.fm23  :=   fm0    * AVector.fm23;
  result.fm24  :=   fm0    * AVector.fm24;
  result.fm34  :=   fm0    * AVector.fm34;
  result.fm123 :=  +fm1    * AVector.fm23
                   -fm2    * AVector.fm13
                   +fm3    * AVector.fm12;
  result.fm124 :=  +fm1    * AVector.fm24
                   -fm2    * AVector.fm14
                   +fm4    * AVector.fm12;
  result.fm134 :=  +fm1    * AVector.fm34
                   -fm3    * AVector.fm14
                   +fm4    * AVector.fm13;
  result.fm234 :=  +fm2    * AVector.fm34
                   -fm3    * AVector.fm24
                   +fm4    * AVector.fm23;
  result.fm1234 := +fm12   * AVector.fm34
                   -fm13   * AVector.fm24
                   +fm14   * AVector.fm23
                   +fm23   * AVector.fm14
                   -fm24   * AVector.fm13
                   +fm34   * AVector.fm12;
end;

function TMultivectorHelper.Wedge(const AVector: TVector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  fm0   * AVector.fm1;
  result.fm2    :=  fm0   * AVector.fm2;
  result.fm3    :=  fm0   * AVector.fm3;
  result.fm4    :=  fm0   * AVector.fm4;
  result.fm12   :=  fm1   * AVector.fm2
                   -fm2   * AVector.fm1;
  result.fm13   :=  fm1   * AVector.fm3
                   -fm3   * AVector.fm1;
  result.fm14   :=  fm1   * AVector.fm4
                   -fm4   * AVector.fm1;
  result.fm23   :=  fm2   * AVector.fm3
                   -fm3   * AVector.fm2;
  result.fm24   :=  fm2   * AVector.fm4
                   -fm4   * AVector.fm2;
  result.fm34   :=  fm3   * AVector.fm4
                   -fm4   * AVector.fm3;
  result.fm123  :=  fm12  * AVector.fm3
                   -fm13  * AVector.fm2
                   +fm23  * AVector.fm1;
  result.fm124  :=  fm12  * AVector.fm4
                   -fm14  * AVector.fm2
                   +fm24  * AVector.fm1;
  result.fm134  :=  fm13  * AVector.fm4
                   -fm14  * AVector.fm3
                   +fm34  * AVector.fm1;
  result.fm234  :=  fm23  * AVector.fm4
                   -fm24  * AVector.fm3
                   +fm34  * AVector.fm2;
  result.fm1234 :=  fm123 * AVector.fm4
                   -fm124 * AVector.fm3
                   +fm134 * AVector.fm2
                   -fm234 * AVector.fm1;
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

function TMultivectorHelper.Rejection(const AVector: TQuadrivector): TMultivector;
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

function TMultivectorHelper.Rotation(const AVector1, AVector2: TQuadrivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TMultivectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TMultivectorHelper.SameValue(const AValue: TMultivector): boolean;
begin
  result := Math.SameValue(fm0,    AValue.fm0   ) and
            Math.SameValue(fm1,    AValue.fm1   ) and
            Math.SameValue(fm2,    AValue.fm2   ) and
            Math.SameValue(fm3,    AValue.fm3   ) and
            Math.SameValue(fm4,    AValue.fm4   ) and
            Math.SameValue(fm12,   AValue.fm12  ) and
            Math.SameValue(fm13,   AValue.fm13  ) and
            Math.SameValue(fm14,   AValue.fm14  ) and
            Math.SameValue(fm23,   AValue.fm23  ) and
            Math.SameValue(fm24,   AValue.fm24  ) and
            Math.SameValue(fm34,   AValue.fm34  ) and
            Math.SameValue(fm123,  AValue.fm123 ) and
            Math.SameValue(fm124,  AValue.fm124 ) and
            Math.SameValue(fm134,  AValue.fm134 ) and
            Math.SameValue(fm234,  AValue.fm234 ) and
            Math.SameValue(fm1234, AValue.fm1234);
end;

function TMultivectorHelper.SameValue(const AValue: TQuadrivector): boolean;
begin
  result := Math.SameValue(fm0,              0.0) and
            Math.SameValue(fm1,              0.0) and
            Math.SameValue(fm2,              0.0) and
            Math.SameValue(fm3,              0.0) and
            Math.SameValue(fm4,              0.0) and
            Math.SameValue(fm12,             0.0) and
            Math.SameValue(fm13,             0.0) and
            Math.SameValue(fm14,             0.0) and
            Math.SameValue(fm23,             0.0) and
            Math.SameValue(fm24,             0.0) and
            Math.SameValue(fm34,             0.0) and
            Math.SameValue(fm123,            0.0) and
            Math.SameValue(fm124,            0.0) and
            Math.SameValue(fm134,            0.0) and
            Math.SameValue(fm234,            0.0) and
            Math.SameValue(fm1234, AValue.fm1234);
end;

function TMultivectorHelper.SameValue(const AValue: TTrivector): boolean;
begin
  result := Math.SameValue(fm0,             0.0) and
            Math.SameValue(fm1,             0.0) and
            Math.SameValue(fm2,             0.0) and
            Math.SameValue(fm3,             0.0) and
            Math.SameValue(fm4,             0.0) and
            Math.SameValue(fm12,            0.0) and
            Math.SameValue(fm13,            0.0) and
            Math.SameValue(fm14,            0.0) and
            Math.SameValue(fm23,            0.0) and
            Math.SameValue(fm24,            0.0) and
            Math.SameValue(fm34,            0.0) and
            Math.SameValue(fm123,  AValue.fm123) and
            Math.SameValue(fm124,  AValue.fm124) and
            Math.SameValue(fm134,  AValue.fm134) and
            Math.SameValue(fm234,  AValue.fm234) and
            Math.SameValue(fm1234,          0.0);
end;

function TMultivectorHelper.SameValue(const AValue: TBivector): boolean;
begin
  result := Math.SameValue(fm0,           0.0) and
            Math.SameValue(fm1,           0.0) and
            Math.SameValue(fm2,           0.0) and
            Math.SameValue(fm3,           0.0) and
            Math.SameValue(fm4,           0.0) and
            Math.SameValue(fm12,  AValue.fm12) and
            Math.SameValue(fm13,  AValue.fm13) and
            Math.SameValue(fm14,  AValue.fm14) and
            Math.SameValue(fm23,  AValue.fm23) and
            Math.SameValue(fm24,  AValue.fm24) and
            Math.SameValue(fm34,  AValue.fm34) and
            Math.SameValue(fm123,         0.0) and
            Math.SameValue(fm124,         0.0) and
            Math.SameValue(fm134,         0.0) and
            Math.SameValue(fm234,         0.0) and
            Math.SameValue(fm1234,        0.0);
end;

function TMultivectorHelper.SameValue(const AValue: TVector): boolean;
begin
  result := Math.SameValue(fm0,          0.0) and
            Math.SameValue(fm1,   AValue.fm1) and
            Math.SameValue(fm2,   AValue.fm2) and
            Math.SameValue(fm3,   AValue.fm3) and
            Math.SameValue(fm4,   AValue.fm4) and
            Math.SameValue(fm12,         0.0) and
            Math.SameValue(fm13,         0.0) and
            Math.SameValue(fm14,         0.0) and
            Math.SameValue(fm23,         0.0) and
            Math.SameValue(fm24,         0.0) and
            Math.SameValue(fm34,         0.0) and
            Math.SameValue(fm123,        0.0) and
            Math.SameValue(fm124,        0.0) and
            Math.SameValue(fm134,        0.0) and
            Math.SameValue(fm234,        0.0) and
            Math.SameValue(fm1234,       0.0);
end;

function TMultivectorHelper.SameValue(const AValue: double): boolean;
begin
  result := Math.SameValue(fm0,   AValue) and
            Math.SameValue(fm1,      0.0) and
            Math.SameValue(fm2,      0.0) and
            Math.SameValue(fm3,      0.0) and
            Math.SameValue(fm4,      0.0) and
            Math.SameValue(fm12,     0.0) and
            Math.SameValue(fm13,     0.0) and
            Math.SameValue(fm14,     0.0) and
            Math.SameValue(fm23,     0.0) and
            Math.SameValue(fm24,     0.0) and
            Math.SameValue(fm34,     0.0) and
            Math.SameValue(fm123,    0.0) and
            Math.SameValue(fm124,    0.0) and
            Math.SameValue(fm134,    0.0) and
            Math.SameValue(fm234,    0.0) and
            Math.SameValue(fm1234,   0.0);
end;

function TMultivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm0,    0.0) then result := result + Fmt(fm0,    APrecision, ADigits) + ' ';
  if not Math.SameValue(fm1,    0.0) then result := result + Fmt(fm1,    APrecision, ADigits) + 'e1 ';
  if not Math.SameValue(fm2,    0.0) then result := result + Fmt(fm2,    APrecision, ADigits) + 'e2 ';
  if not Math.SameValue(fm3,    0.0) then result := result + Fmt(fm3,    APrecision, ADigits) + 'e3 ';
  if not Math.SameValue(fm4,    0.0) then result := result + Fmt(fm4,    APrecision, ADigits) + 'e4 ';
  if not Math.SameValue(fm12,   0.0) then result := result + Fmt(fm12,   APrecision, ADigits) + 'e12 ';
  if not Math.SameValue(fm13,   0.0) then result := result + Fmt(fm13,   APrecision, ADigits) + 'e13 ';
  if not Math.SameValue(fm14,   0.0) then result := result + Fmt(fm14,   APrecision, ADigits) + 'e14 ';
  if not Math.SameValue(fm23,   0.0) then result := result + Fmt(fm23,   APrecision, ADigits) + 'e23 ';
  if not Math.SameValue(fm24,   0.0) then result := result + Fmt(fm24,   APrecision, ADigits) + 'e24 ';
  if not Math.SameValue(fm34,   0.0) then result := result + Fmt(fm34,   APrecision, ADigits) + 'e34 ';
  if not Math.SameValue(fm123,  0.0) then result := result + Fmt(fm123,  APrecision, ADigits) + 'e123 ';
  if not Math.SameValue(fm124,  0.0) then result := result + Fmt(fm124,  APrecision, ADigits) + 'e124 ';
  if not Math.SameValue(fm134,  0.0) then result := result + Fmt(fm134,  APrecision, ADigits) + 'e134 ';
  if not Math.SameValue(fm234,  0.0) then result := result + Fmt(fm234,  APrecision, ADigits) + 'e234 ';
  if not Math.SameValue(fm1234, 0.0) then result := result + Fmt(fm1234, APrecision, ADigits) + 'e1234 ';

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
  if not Math.SameValue(fm0,    0.0) then result := result + Fmt(fm0   ) + ' ';
  if not Math.SameValue(fm1,    0.0) then result := result + Fmt(fm1   ) + 'e1 ';
  if not Math.SameValue(fm2,    0.0) then result := result + Fmt(fm2   ) + 'e2 ';
  if not Math.SameValue(fm3,    0.0) then result := result + Fmt(fm3   ) + 'e3 ';
  if not Math.SameValue(fm4,    0.0) then result := result + Fmt(fm4   ) + 'e4 ';
  if not Math.SameValue(fm12,   0.0) then result := result + Fmt(fm12  ) + 'e12 ';
  if not Math.SameValue(fm13,   0.0) then result := result + Fmt(fm13  ) + 'e13 ';
  if not Math.SameValue(fm14,   0.0) then result := result + Fmt(fm14  ) + 'e14 ';
  if not Math.SameValue(fm23,   0.0) then result := result + Fmt(fm23  ) + 'e23 ';
  if not Math.SameValue(fm24,   0.0) then result := result + Fmt(fm24  ) + 'e24 ';
  if not Math.SameValue(fm34,   0.0) then result := result + Fmt(fm34  ) + 'e34 ';
  if not Math.SameValue(fm123,  0.0) then result := result + Fmt(fm123 ) + 'e123 ';
  if not Math.SameValue(fm124,  0.0) then result := result + Fmt(fm124 ) + 'e124 ';
  if not Math.SameValue(fm134,  0.0) then result := result + Fmt(fm134 ) + 'e134 ';
  if not Math.SameValue(fm234,  0.0) then result := result + Fmt(fm234 ) + 'e234 ';
  if not Math.SameValue(fm1234, 0.0) then result := result + Fmt(fm1234) + 'e1234 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0';
end;

function TMultivectorHelper.Extract(AComponents: TMultivectorComponents): TMultivector;
begin
  result := NullMultivector;
  if mc0    in AComponents then result.fm0    := fm0;
  if mc1    in AComponents then result.fm1    := fm1;
  if mc2    in AComponents then result.fm2    := fm2;
  if mc3    in AComponents then result.fm3    := fm3;
  if mc4    in AComponents then result.fm4    := fm4;
  if mc12   in AComponents then result.fm12   := fm12;
  if mc13   in AComponents then result.fm13   := fm13;
  if mc14   in AComponents then result.fm14   := fm14;
  if mc23   in AComponents then result.fm23   := fm23;
  if mc24   in AComponents then result.fm24   := fm24;
  if mc34   in AComponents then result.fm34   := fm34;
  if mc123  in AComponents then result.fm123  := fm123;
  if mc124  in AComponents then result.fm124  := fm124;
  if mc134  in AComponents then result.fm134  := fm134;
  if mc234  in AComponents then result.fm234  := fm234;
  if mc1234 in AComponents then result.fm1234 := fm1234;
end;

function TMultivectorHelper.Extract(AComponents: TTrivectorComponents): TTrivector;
begin
  result := NullTrivector;
  if tc123  in AComponents then result.fm123 := fm123;
  if tc124  in AComponents then result.fm124 := fm124;
  if tc134  in AComponents then result.fm134 := fm134;
  if tc234  in AComponents then result.fm234 := fm234;
end;

function TMultivectorHelper.Extract(AComponents: TBivectorComponents): TBivector;
begin
  result := NullBivector;
  if bc12 in AComponents then result.fm12 := fm12;
  if bc13 in AComponents then result.fm13 := fm13;
  if bc14 in AComponents then result.fm14 := fm14;
  if bc23 in AComponents then result.fm23 := fm23;
  if bc24 in AComponents then result.fm24 := fm24;
  if bc34 in AComponents then result.fm34 := fm34;
end;

function TMultivectorHelper.Extract(AComponents: TVectorComponents): TVector;
begin
  result := NullVector;
  if vc1 in AComponents then result.fm1 := fm1;
  if vc2 in AComponents then result.fm2 := fm2;
  if vc3 in AComponents then result.fm3 := fm3;
  if vc4 in AComponents then result.fm4 := fm4;
end;

function TMultivectorHelper.ExtractQuadrivector: TQuadrivector;
begin
  result.fm1234 := fm1234;
end;

function TMultivectorHelper.ExtractTrivector: TTrivector;
begin
  result.fm123 := fm123;
  result.fm124 := fm124;
  result.fm134 := fm134;
  result.fm234 := fm234;
end;

function TMultivectorHelper.ExtractBivector: TBivector;
begin
  result.fm12 := fm12;
  result.fm13 := fm13;
  result.fm14 := fm14;
  result.fm23 := fm23;
  result.fm24 := fm24;
  result.fm34 := fm34;
end;

function TMultivectorHelper.ExtractVector: TVector;
begin
  result.fm1 := fm1;
  result.fm2 := fm2;
  result.fm3 := fm3;
  result.fm4 := fm4;
end;

function TMultivectorHelper.ExtractScalar: double;
begin
  result := fm0;
end;

function TMultivectorHelper.IsQuadrivector: boolean;
begin
  result := (Math.SameValue(fm0,    0.0)) and
            (Math.SameValue(fm1,    0.0)) and
            (Math.SameValue(fm2,    0.0)) and
            (Math.SameValue(fm3,    0.0)) and
            (Math.SameValue(fm4,    0.0)) and
            (Math.SameValue(fm12,   0.0)) and
            (Math.SameValue(fm13,   0.0)) and
            (Math.SameValue(fm14,   0.0)) and
            (Math.SameValue(fm23,   0.0)) and
            (Math.SameValue(fm24,   0.0)) and
            (Math.SameValue(fm34,   0.0)) and
            (Math.SameValue(fm123,  0.0)) and
            (Math.SameValue(fm124,  0.0)) and
            (Math.SameValue(fm134,  0.0)) and
            (Math.SameValue(fm234,  0.0));

  if result then result := (not Math.SameValue(fm1234, 0.0));
end;

function TMultivectorHelper.IsTrivector: boolean;
begin
  result := (Math.SameValue(fm0,    0.0)) and
            (Math.SameValue(fm1,    0.0)) and
            (Math.SameValue(fm2,    0.0)) and
            (Math.SameValue(fm3,    0.0)) and
            (Math.SameValue(fm4,    0.0)) and
            (Math.SameValue(fm12,   0.0)) and
            (Math.SameValue(fm13,   0.0)) and
            (Math.SameValue(fm14,   0.0)) and
            (Math.SameValue(fm23,   0.0)) and
            (Math.SameValue(fm24,   0.0)) and
            (Math.SameValue(fm34,   0.0)) and
            (Math.SameValue(fm1234, 0.0));

  if result then result := (not Math.SameValue(fm123, 0.0)) or
                           (not Math.SameValue(fm124, 0.0)) or
                           (not Math.SameValue(fm134, 0.0)) or
                           (not Math.SameValue(fm234, 0.0));
end;

function TMultivectorHelper.IsBiVector: boolean;
begin
  result := (Math.SameValue(fm0,    0.0)) and
            (Math.SameValue(fm1,    0.0)) and
            (Math.SameValue(fm2,    0.0)) and
            (Math.SameValue(fm3,    0.0)) and
            (Math.SameValue(fm4,    0.0)) and
            (Math.SameValue(fm123,  0.0)) and
            (Math.SameValue(fm124,  0.0)) and
            (Math.SameValue(fm134,  0.0)) and
            (Math.SameValue(fm234,  0.0)) and
            (Math.SameValue(fm1234, 0.0));

  if result then result := (not Math.SameValue(fm12, 0.0)) or
                           (not Math.SameValue(fm13, 0.0)) or
                           (not Math.SameValue(fm14, 0.0)) or
                           (not Math.SameValue(fm23, 0.0)) or
                           (not Math.SameValue(fm24, 0.0)) or
                           (not Math.SameValue(fm34, 0.0));
end;

function TMultivectorHelper.IsVector: boolean;
begin
  result := (Math.SameValue(fm0,    0.0)) and
            (Math.SameValue(fm12,   0.0)) and
            (Math.SameValue(fm13,   0.0)) and
            (Math.SameValue(fm14,   0.0)) and
            (Math.SameValue(fm23,   0.0)) and
            (Math.SameValue(fm24,   0.0)) and
            (Math.SameValue(fm34,   0.0)) and
            (Math.SameValue(fm123,  0.0)) and
            (Math.SameValue(fm124,  0.0)) and
            (Math.SameValue(fm134,  0.0)) and
            (Math.SameValue(fm234,  0.0)) and
            (Math.SameValue(fm1234, 0.0));

  if result then result := (not Math.SameValue(fm1, 0.0)) or
                           (not Math.SameValue(fm2, 0.0)) or
                           (not Math.SameValue(fm3, 0.0)) or
                           (not Math.SameValue(fm4, 0.0));
end;

function TMultivectorHelper.IsScalar: boolean;
begin
  result := (Math.SameValue(fm1,    0.0)) and
            (Math.SameValue(fm2,    0.0)) and
            (Math.SameValue(fm3,    0.0)) and
            (Math.SameValue(fm4,    0.0)) and
            (Math.SameValue(fm12,   0.0)) and
            (Math.SameValue(fm13,   0.0)) and
            (Math.SameValue(fm14,   0.0)) and
            (Math.SameValue(fm23,   0.0)) and
            (Math.SameValue(fm24,   0.0)) and
            (Math.SameValue(fm34,   0.0)) and
            (Math.SameValue(fm123,  0.0)) and
            (Math.SameValue(fm124,  0.0)) and
            (Math.SameValue(fm134,  0.0)) and
            (Math.SameValue(fm234,  0.0)) and
            (Math.SameValue(fm1234, 0.0));

  if result then result := (not Math.SameValue(fm0, 0.0));
end;

function TMultivectorHelper.IsNull: boolean;
begin
  result := SameValue(NullMultivector);
end;

function TMultivectorHelper.IsA: string;
begin
  if IsNull         then Result := 'Null'          else
  if IsQuadrivector then Result := 'TQuadrivector' else
  if IsTrivector    then Result := 'TTrivector'    else
  if IsBivector     then Result := 'TBivector'     else
  if IsVector       then Result := 'TVector'       else
  if IsScalar       then Result := 'TScalar'       else Result := 'TMultivector';
end;

// TQuadrivectorHelper

function TQuadrivectorHelper.Dual: double;
begin
  result := fm1234;
end;

function TQuadrivectorHelper.Inverse: TQuadrivector;
begin
  result.fm1234 := fm1234;
end;

function TQuadrivectorHelper.Reverse: TQuadrivector;
begin
  result.fm1234 := fm1234;
end;

function TQuadrivectorHelper.Conjugate: TQuadrivector;
begin
  result.fm1234 := fm1234;
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
  result := abs(fm1234);
end;

function TQuadrivectorHelper.SquaredNorm: double;
begin
  result := fm1234*fm1234;
end;

function TQuadrivectorHelper.Dot(const AVector: TVector): TTrivector;
begin
  result.fm123 :=  fm1234 * AVector.fm4;
  result.fm124 := -fm1234 * AVector.fm3;
  result.fm134 :=  fm1234 * AVector.fm2;
  result.fm234 := -fm1234 * AVector.fm1;
end;

function TQuadrivectorHelper.Dot(const AVector: TBivector): TBivector;
begin
  result.fm12 := -fm1234 * AVector.fm34;
  result.fm13 :=  fm1234 * AVector.fm24;
  result.fm14 := -fm1234 * AVector.fm23;
  result.fm23 := -fm1234 * AVector.fm14;
  result.fm24 :=  fm1234 * AVector.fm13;
  result.fm34 := -fm1234 * AVector.fm12;
end;

function TQuadrivectorHelper.Dot(const AVector: TTrivector): TVector;
begin
  result.fm1 := -fm1234 * AVector.fm234;
  result.fm2 :=  fm1234 * AVector.fm134;
  result.fm3 := -fm1234 * AVector.fm124;
  result.fm4 :=  fm1234 * AVector.fm123;
end;

function TQuadrivectorHelper.Dot(const AVector: TQuadrivector): double;
begin
  result := fm1234 * AVector.fm1234;
end;

function TQuadrivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0    :=  fm1234 * AVector.fm1234;
  result.fm1    := -fm1234 * AVector.fm234;
  result.fm2    :=  fm1234 * AVector.fm134;
  result.fm3    := -fm1234 * AVector.fm124;
  result.fm4    :=  fm1234 * AVector.fm123;
  result.fm12   := -fm1234 * AVector.fm34;
  result.fm13   :=  fm1234 * AVector.fm24;
  result.fm14   := -fm1234 * AVector.fm23;
  result.fm23   := -fm1234 * AVector.fm14;
  result.fm24   :=  fm1234 * AVector.fm13;
  result.fm34   := -fm1234 * AVector.fm12;
  result.fm123  :=  fm1234 * AVector.fm4;
  result.fm124  := -fm1234 * AVector.fm3;
  result.fm134  :=  fm1234 * AVector.fm2;
  result.fm234  := -fm1234 * AVector.fm1;
  result.fm1234 :=  fm1234 * AVector.fm0;
end;

function TQuadrivectorHelper.Wedge(const AVector: TVector): double;
begin
  result := 0.0;
end;

function TQuadrivectorHelper.Wedge(const AVector: TBivector): double;
begin
  result := 0.0;
end;

function TQuadrivectorHelper.Wedge(const AVector: TTrivector): double;
begin
  result := 0.0;
end;

function TQuadrivectorHelper.Wedge(const AVector: TQuadrivector): double;
begin
  result := 0.0;
end;

function TQuadrivectorHelper.Wedge(const AVector: TMultivector): TQuadrivector;
begin
  result.fm1234 := fm1234 * AVector.fm0;
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

function TQuadrivectorHelper.Projection(const AVector: TQuadrivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Projection(const AVector: TMultivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Rejection(const AVector: TVector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Rejection(const AVector: TBivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Rejection(const AVector: TTrivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TQuadrivectorHelper.Rejection(const AVector: TQuadrivector): TMultivector;
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

function TQuadrivectorHelper.Reflection(const AVector: TQuadrivector): TMultivector;
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

function TQuadrivectorHelper.Rotation(const AVector1, AVector2: TTrivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TQuadrivectorHelper.Rotation(const AVector1, AVector2: TQuadrivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TQuadrivectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TQuadrivectorHelper.SameValue(const AValue: TMultivector): boolean;
begin
  result := Math.SameValue(0.0,    AValue.fm0   ) and
            Math.SameValue(0.0,    AValue.fm1   ) and
            Math.SameValue(0.0,    AValue.fm2   ) and
            Math.SameValue(0.0,    AValue.fm3   ) and
            Math.SameValue(0.0,    AValue.fm4   ) and
            Math.SameValue(0.0,    AValue.fm12  ) and
            Math.SameValue(0.0,    AValue.fm13  ) and
            Math.SameValue(0.0,    AValue.fm14  ) and
            Math.SameValue(0.0,    AValue.fm23  ) and
            Math.SameValue(0.0,    AValue.fm24  ) and
            Math.SameValue(0.0,    AValue.fm34  ) and
            Math.SameValue(0.0,    AValue.fm123 ) and
            Math.SameValue(0.0,    AValue.fm124 ) and
            Math.SameValue(0.0,    AValue.fm134 ) and
            Math.SameValue(0.0,    AValue.fm234 ) and
            Math.SameValue(fm1234, AValue.fm1234);
end;

function TQuadrivectorHelper.SameValue(const AValue: TQuadrivector): boolean;
begin
  result := Math.SameValue(fm1234, AValue.fm1234);
end;

function TQuadrivectorHelper.ToMultivector: TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := fm1234;
end;

function TQuadrivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
begin
  if not Math.SameValue(fm1234, 0.0) then
    result := Fmt(fm1234, APrecision, ADigits) + 'e1234'
  else
    result := '0e1234';
end;

function TQuadrivectorHelper.ToString: string;
begin
  if not Math.SameValue(fm1234, 0.0) then
    result := Fmt(fm1234) + 'e1234'
  else
    result := '0e1234';
end;

// TTrivectorHelper

function TTrivectorHelper.Dual: TVector;
begin
  // Self * e1234
  result.fm1 :=  fm234;
  result.fm2 := -fm134;
  result.fm3 :=  fm124;
  result.fm4 := -fm123;
end;

function TTrivectorHelper.Inverse: TTrivector;
begin
  result.fm123 := -fm123;
  result.fm124 := -fm124;
  result.fm134 := -fm134;
  result.fm234 := -fm234;
end;

function TTrivectorHelper.Reverse: TTrivector;
begin
  result.fm123 := -fm123;
  result.fm124 := -fm124;
  result.fm134 := -fm134;
  result.fm234 := -fm234;
end;

function TTrivectorHelper.Conjugate: TTrivector;
begin
  result.fm123 := fm123;
  result.fm124 := fm124;
  result.fm134 := fm134;
  result.fm234 := fm234;
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
  result := sqrt(SquaredNorm);
end;

function TTrivectorHelper.SquaredNorm: double;
begin
  result := fm123*fm123 + fm124*fm124 + fm134*fm134 + fm234*fm234;
end;

function TTrivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0    := -fm123 * AVector.fm123
                   -fm124 * AVector.fm124
                   -fm134 * AVector.fm134
                   -fm234 * AVector.fm234;
  result.fm1    := -fm123 * AVector.fm23
                   -fm124 * AVector.fm24
                   -fm134 * AVector.fm34
                   +fm234 * AVector.fm1234;
  result.fm2    :=  fm123 * AVector.fm13
                   +fm124 * AVector.fm14
                   -fm134 * AVector.fm1234
                   -fm234 * AVector.fm34;
  result.fm3    := -fm123 * AVector.fm12
                   +fm124 * AVector.fm1234
                   +fm134 * AVector.fm14
                   +fm234 * AVector.fm24;
  result.fm4    := -fm123 * AVector.fm1234
                   -fm124 * AVector.fm12
                   -fm134 * AVector.fm13
                   -fm234 * AVector.fm23;
  result.fm12   :=  fm123 * AVector.fm3
                   +fm124 * AVector.fm4;
  result.fm13   := -fm123 * AVector.fm2
                   +fm134 * AVector.fm4;
  result.fm14   := -fm124 * AVector.fm2
                   -fm134 * AVector.fm3;
  result.fm23   :=  fm123 * AVector.fm1
                   +fm234 * AVector.fm4;
  result.fm24   :=  fm124 * AVector.fm1
                   -fm234 * AVector.fm3;
  result.fm34   :=  fm134 * AVector.fm1
                   +fm234 * AVector.fm2;
  result.fm123  :=  fm123 * AVector.fm0;
  result.fm124  :=  fm124 * AVector.fm0;
  result.fm134  :=  fm134 * AVector.fm0;
  result.fm234  :=  fm234 * AVector.fm0;
  result.fm1234 :=  0.0;
end;

function TTrivectorHelper.Dot(const AVector: TQuadrivector): TVector;
begin
  result.fm1 :=  fm234 * AVector.fm1234;
  result.fm2 := -fm134 * AVector.fm1234;
  result.fm3 :=  fm124 * AVector.fm1234;
  result.fm4 := -fm123 * AVector.fm1234;
end;

function TTrivectorHelper.Dot(const AVector: TTrivector): double;
begin
  result := -fm123 * AVector.fm123 -fm124 * AVector.fm124
            -fm134 * AVector.fm134 -fm234 * AVector.fm234;
end;

function TTrivectorHelper.Dot(const AVector: TBivector): TVector;
begin
  result.fm1 := -fm123 * AVector.fm23 -fm124 * AVector.fm24 -fm134 * AVector.fm34;
  result.fm2 :=  fm123 * AVector.fm13 +fm124 * AVector.fm14 -fm234 * AVector.fm34;
  result.fm3 := -fm123 * AVector.fm12 +fm134 * AVector.fm14 +fm234 * AVector.fm24;
  result.fm4 := -fm124 * AVector.fm12 -fm134 * AVector.fm13 -fm234 * AVector.fm23;
end;

function TTrivectorHelper.Dot(const AVector: TVector): TBivector;
begin
  result.fm12 :=  fm123  * AVector.fm3
                 +fm124  * AVector.fm4;
  result.fm13 := -fm123  * AVector.fm2
                 +fm134  * AVector.fm4;
  result.fm14 := -fm124  * AVector.fm2
                 -fm134  * AVector.fm3;
  result.fm23 :=  fm123  * AVector.fm1
                 +fm234  * AVector.fm4;
  result.fm24 := +fm124  * AVector.fm1
                 -fm234  * AVector.fm3;
  result.fm34 :=  fm134  * AVector.fm1
                 +fm234  * AVector.fm2;
end;

function TTrivectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   :=  0.0;
  result.fm13   :=  0.0;
  result.fm14   :=  0.0;
  result.fm23   :=  0.0;
  result.fm24   :=  0.0;
  result.fm34   :=  0.0;
  result.fm123  :=  fm123 * AVector.fm0;
  result.fm124  :=  fm124 * AVector.fm0;
  result.fm134  :=  fm134 * AVector.fm0;
  result.fm234  :=  fm234 * AVector.fm0;
  result.fm1234 :=  fm123 * AVector.fm4 -fm124 * AVector.fm3
                   +fm134 * AVector.fm2 -fm234 * AVector.fm1;
end;

function TTrivectorHelper.Wedge(const AVector: TQuadrivector): double;
begin
  result := 0.0;
end;

function TTrivectorHelper.Wedge(const AVector: TTrivector): double;
begin
  result := 0.0;
end;

function TTrivectorHelper.Wedge(const AVector: TBivector): double;
begin
  result := 0.0;
end;

function TTrivectorHelper.Wedge(const AVector: TVector): TQuadrivector;
begin
  result.fm1234 :=  fm123 * AVector.fm4 -fm124 * AVector.fm3
                   +fm134 * AVector.fm2 -fm234 * AVector.fm1;
end;

function TTrivectorHelper.Projection(const AVector: TVector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Projection(const AVector: TBivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Projection(const AVector: TTrivector): TMultivector;
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

function TTrivectorHelper.Rejection(const AVector: TVector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Rejection(const AVector: TBivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Rejection(const AVector: TTrivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TTrivectorHelper.Rejection(const AVector: TQuadrivector): TMultivector;
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

function TTrivectorHelper.Rotation(const AVector1, AVector2: TQuadrivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TTrivectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TTrivectorHelper.SameValue(const AValue: TMultivector): boolean;
begin
  result := Math.SameValue(0.0,   AValue.fm0   ) and
            Math.SameValue(0.0,   AValue.fm1   ) and
            Math.SameValue(0.0,   AValue.fm2   ) and
            Math.SameValue(0.0,   AValue.fm3   ) and
            Math.SameValue(0.0,   AValue.fm4   ) and
            Math.SameValue(0.0,   AValue.fm12  ) and
            Math.SameValue(0.0,   AValue.fm13  ) and
            Math.SameValue(0.0,   AValue.fm14  ) and
            Math.SameValue(0.0,   AValue.fm23  ) and
            Math.SameValue(0.0,   AValue.fm24  ) and
            Math.SameValue(0.0,   AValue.fm34  ) and
            Math.SameValue(fm123, AValue.fm123 ) and
            Math.SameValue(fm124, AValue.fm124 ) and
            Math.SameValue(fm134, AValue.fm134 ) and
            Math.SameValue(fm234, AValue.fm234 ) and
            Math.SameValue(0.0,   AValue.fm1234);
end;

function TTrivectorHelper.SameValue(const AValue: TTrivector): boolean;
begin
  result := Math.SameValue(fm123, AValue.fm123);
  result := Math.SameValue(fm124, AValue.fm124);
  result := Math.SameValue(fm134, AValue.fm134);
  result := Math.SameValue(fm234, AValue.fm234);
end;

function TTrivectorHelper.Extract(AComponents: TTrivectorComponents): TTrivector;
begin
  result := NullTrivector;
  if tc123 in AComponents then result.fm123 := fm123;
  if tc124 in AComponents then result.fm124 := fm124;
  if tc134 in AComponents then result.fm134 := fm134;
  if tc234 in AComponents then result.fm234 := fm234;
end;

function TTrivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm123, 0.0) then result := result + Fmt(fm123, APrecision, ADigits) + 'e123 ';
  if not Math.SameValue(fm124, 0.0) then result := result + Fmt(fm124, APrecision, ADigits) + 'e124 ';
  if not Math.SameValue(fm134, 0.0) then result := result + Fmt(fm134, APrecision, ADigits) + 'e134 ';
  if not Math.SameValue(fm234, 0.0) then result := result + Fmt(fm234, APrecision, ADigits) + 'e234 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0e123';
end;

function TTrivectorHelper.ToString: string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm123, 0.0) then result := Fmt(fm123) + 'e123 ';
  if not Math.SameValue(fm124, 0.0) then result := Fmt(fm124) + 'e124 ';
  if not Math.SameValue(fm134, 0.0) then result := Fmt(fm134) + 'e134 ';
  if not Math.SameValue(fm234, 0.0) then result := Fmt(fm234) + 'e234 ';

  i := Length(result);
  if i > 0 then
    SetLength(result, i - 1)
  else
    result := '0e123';
end;

function TTrivectorHelper.ToMultivector: TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := fm123;
  result.fm124  := fm124;
  result.fm134  := fm134;
  result.fm234  := fm234;
  result.fm1234 := 0.0;
end;

// TBivectorHelper

function TBivectorHelper.Dual: TBivector;
begin
  // Self * e1234;
  result.fm12 := -fm34;
  result.fm13 :=  fm24;
  result.fm14 := -fm23;
  result.fm23 := -fm14;
  result.fm24 :=  fm13;
  result.fm34 := -fm12;
end;

function TBivectorHelper.Inverse: TBivector;
begin
  result.fm12 := fm12;
  result.fm13 := fm13;
  result.fm14 := fm14;
  result.fm23 := fm23;
  result.fm24 := fm24;
  result.fm34 := fm34;
end;

function TBivectorHelper.Conjugate: TBivector;
begin
  result.fm12 := -fm12;
  result.fm13 := -fm13;
  result.fm14 := -fm14;
  result.fm23 := -fm23;
  result.fm24 := -fm24;
  result.fm34 := -fm34;
end;

function TBivectorHelper.Reverse: TBivector;
begin
  result.fm12 := -fm12;
  result.fm13 := -fm13;
  result.fm14 := -fm14;
  result.fm23 := -fm23;
  result.fm24 := -fm24;
  result.fm34 := -fm34;
end;

function TBivectorHelper.Reciprocal: TMultivector;
begin
  result := Self.ToMultivector.Reciprocal;
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
  result := fm12*fm12 + fm13*fm13 + fm14*fm14 + fm23*fm23 + fm24*fm24 + fm34*fm34;
end;

function TBivectorHelper.Dot(const AVector: TVector): TVector;
begin
  result.fm1 := +fm12 * AVector.fm2
                +fm13 * AVector.fm3
                +fm14 * AVector.fm4;
  result.fm2 := -fm12 * AVector.fm1
                +fm23 * AVector.fm3
                +fm24 * AVector.fm4;
  result.fm3 := -fm13 * AVector.fm1
                -fm23 * AVector.fm2
                +fm34 * AVector.fm4;
  result.fm4 := -fm14 * AVector.fm1
                -fm24 * AVector.fm2
                -fm34 * AVector.fm3;
end;

function TBivectorHelper.Dot(const AVector: TBivector): double;
begin
  result := -fm12 * AVector.fm12
            -fm13 * AVector.fm13
            -fm14 * AVector.fm14
            -fm23 * AVector.fm23
            -fm24 * AVector.fm24
            -fm34 * AVector.fm34;
end;

function TBivectorHelper.Dot(const AVector: TTrivector): TVector;
begin
  result.fm1 := -fm23 * AVector.fm123
                -fm24 * AVector.fm124
                -fm34 * AVector.fm134;
  result.fm2 :=  fm13 * AVector.fm123
                +fm14 * AVector.fm124
                -fm34 * AVector.fm234;
  result.fm3 := -fm12 * AVector.fm123
                +fm14 * AVector.fm134
                +fm24 * AVector.fm234;
  result.fm4 := -fm12 * AVector.fm124
                -fm13 * AVector.fm134
                -fm23 * AVector.fm234;
end;

function TBivectorHelper.Dot(const AVector: TQuadrivector): TBivector;
begin
  result.fm12 := -fm34 * AVector.fm1234;
  result.fm13 :=  fm24 * AVector.fm1234;
  result.fm14 := -fm23 * AVector.fm1234;
  result.fm23 := -fm14 * AVector.fm1234;
  result.fm24 := +fm13 * AVector.fm1234;
  result.fm34 := -fm12 * AVector.fm1234;
end;

function TBivectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0    := -fm12 * AVector.fm12
                   -fm13 * AVector.fm13
                   -fm14 * AVector.fm14
                   -fm23 * AVector.fm23
                   -fm24 * AVector.fm24
                   -fm34 * AVector.fm34;
  result.fm1    := +fm12 * AVector.fm2
                   +fm13 * AVector.fm3
                   +fm14 * AVector.fm4
                   -fm23 * AVector.fm123
                   -fm24 * AVector.fm124
                   -fm34 * AVector.fm134;
  result.fm2    := -fm12 * AVector.fm1
                   +fm13 * AVector.fm123
                   +fm14 * AVector.fm124
                   +fm23 * AVector.fm3
                   +fm24 * AVector.fm4
                   -fm34 * AVector.fm234;
  result.fm3    := -fm12 * AVector.fm123
                   -fm13 * AVector.fm1
                   +fm14 * AVector.fm134
                   -fm23 * AVector.fm2
                   +fm24 * AVector.fm234
                   +fm34 * AVector.fm4;
  result.fm4    := -fm12 * AVector.fm124
                   -fm13 * AVector.fm134
                   -fm14 * AVector.fm1
                   -fm23 * AVector.fm234
                   -fm24 * AVector.fm2
                   -fm34 * AVector.fm3;
  result.fm12   := +fm12 * AVector.fm0
                   -fm34 * AVector.fm1234;
  result.fm13   := +fm13 * AVector.fm0
                   +fm24 * AVector.fm1234;
  result.fm14   := +fm14 * AVector.fm0
                   -fm23 * AVector.fm1234;
  result.fm23   := -fm14 * AVector.fm1234
                   +fm23 * AVector.fm0;
  result.fm24   := +fm13 * AVector.fm1234
                   +fm24 * AVector.fm0;
  result.fm34   := -fm12 * AVector.fm1234
                   +fm34 * AVector.fm0;
  result.fm123  :=  0.0;
  result.fm124  :=  0.0;
  result.fm134  :=  0.0;
  result.fm234  :=  0.0;
  result.fm1234 :=  0.0;
end;

function TBivectorHelper.Wedge(const AVector: TVector): TTrivector;
begin
  result.fm123 := fm12 * AVector.fm3 -fm13 * AVector.fm2 +fm23 * AVector.fm1;
  result.fm124 := fm12 * AVector.fm4 -fm14 * AVector.fm2 +fm24 * AVector.fm1;
  result.fm134 := fm13 * AVector.fm4 -fm14 * AVector.fm3 +fm34 * AVector.fm1;
  result.fm234 := fm23 * AVector.fm4 -fm24 * AVector.fm3 +fm34 * AVector.fm2;
end;

function TBivectorHelper.Wedge(const AVector: TBivector): TQuadrivector;
begin
  result.fm1234 :=  fm12 * AVector.fm34 -fm13 * AVector.fm24 +fm14 * AVector.fm23
                   +fm23 * AVector.fm14 -fm24 * AVector.fm13 +fm34 * AVector.fm12;
end;

function TBivectorHelper.Wedge(const AVector: TTrivector): double;
begin
  result := 0.0;
end;

function TBivectorHelper.Wedge(const AVector: TQuadrivector): double;
begin
  result := 0.0;
end;

function TBivectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  0.0;
  result.fm2    :=  0.0;
  result.fm3    :=  0.0;
  result.fm4    :=  0.0;
  result.fm12   :=  fm12 * AVector.fm0;
  result.fm13   :=  fm13 * AVector.fm0;
  result.fm14   :=  fm14 * AVector.fm0;
  result.fm23   :=  fm23 * AVector.fm0;
  result.fm24   :=  fm24 * AVector.fm0;
  result.fm34   :=  fm34 * AVector.fm0;
  result.fm123  :=  fm12 * AVector.fm3
                   -fm13 * AVector.fm2
                   +fm23 * AVector.fm1;
  result.fm124  :=  fm12 * AVector.fm4
                   -fm14 * AVector.fm2
                   +fm24 * AVector.fm1;
  result.fm134  :=  fm13 * AVector.fm4
                   -fm14 * AVector.fm3
                   +fm34 * AVector.fm1;
  result.fm234  :=  fm23 * AVector.fm4
                   -fm24 * AVector.fm3
                   +fm34 * AVector.fm2;
  result.fm1234 :=  fm12 * AVector.fm34
                   -fm13 * AVector.fm24
                   +fm14 * AVector.fm23
                   +fm23 * AVector.fm14
                   -fm24 * AVector.fm13
                   +fm34 * AVector.fm12;
end;

function TBivectorHelper.Projection(const AVector: TVector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Projection(const AVector: TBivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Projection(const AVector: TTrivector): TMultivector;
begin
  result := Dot(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Projection(const AVector: TQuadrivector): TMultivector;
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

function TBivectorHelper.Rejection (const AVector: TBivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Rejection (const AVector: TTrivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Rejection (const AVector: TQuadrivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TBivectorHelper.Rejection (const AVector: TMultivector): TMultivector;
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

function TBivectorHelper.Reflection(const AVector: TQuadrivector): TMultivector;
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

function TBivectorHelper.Rotation(const AVector1, AVector2: TQuadrivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TBivectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal * AVector2.Reciprocal;
end;

function TBivectorHelper.SameValue(const AValue: TMultivector): boolean;
begin
  result := Math.SameValue(0.0,  AValue.fm0   ) and
            Math.SameValue(0.0,  AValue.fm1   ) and
            Math.SameValue(0.0,  AValue.fm2   ) and
            Math.SameValue(0.0,  AValue.fm3   ) and
            Math.SameValue(0.0,  AValue.fm4   ) and
            Math.SameValue(fm12, AValue.fm12  ) and
            Math.SameValue(fm13, AValue.fm13  ) and
            Math.SameValue(fm14, AValue.fm14  ) and
            Math.SameValue(fm23, AValue.fm23  ) and
            Math.SameValue(fm24, AValue.fm24  ) and
            Math.SameValue(fm34, AValue.fm34  ) and
            Math.SameValue(0.0,  AValue.fm123 ) and
            Math.SameValue(0.0,  AValue.fm124 ) and
            Math.SameValue(0.0,  AValue.fm134 ) and
            Math.SameValue(0.0,  AValue.fm234 ) and
            Math.SameValue(0.0,  AValue.fm1234);
end;

function TBivectorHelper.SameValue(const AValue: TBivector): boolean;
begin
  result := Math.SameValue(fm12, AValue.fm12) and
            Math.SameValue(fm13, AValue.fm13) and
            Math.SameValue(fm14, AValue.fm14) and
            Math.SameValue(fm23, AValue.fm23) and
            Math.SameValue(fm24, AValue.fm24) and
            Math.SameValue(fm34, AValue.fm34);
end;

function TBivectorHelper.Extract(AComponents: TBivectorComponents): TBivector;
begin
  Result := NullBivector;
  if bc12 in AComponents then result.fm12 := fm12;
  if bc13 in AComponents then result.fm13 := fm13;
  if bc14 in AComponents then result.fm14 := fm14;
  if bc23 in AComponents then result.fm23 := fm23;
  if bc24 in AComponents then result.fm24 := fm24;
  if bc34 in AComponents then result.fm34 := fm34;
end;

function TBivectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  Result := '';
  if not Math.SameValue(fm12, 0.0) then Result := Result + Fmt(fm12,  APrecision, ADigits) + 'e12 ';
  if not Math.SameValue(fm13, 0.0) then Result := Result + Fmt(fm13,  APrecision, ADigits) + 'e13 ';
  if not Math.SameValue(fm14, 0.0) then Result := Result + Fmt(fm14,  APrecision, ADigits) + 'e14 ';
  if not Math.SameValue(fm23, 0.0) then Result := Result + Fmt(fm23,  APrecision, ADigits) + 'e23 ';
  if not Math.SameValue(fm24, 0.0) then Result := Result + Fmt(fm24,  APrecision, ADigits) + 'e24 ';
  if not Math.SameValue(fm34, 0.0) then Result := Result + Fmt(fm34,  APrecision, ADigits) + 'e34 ';

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
  if not Math.SameValue(fm13, 0.0) then Result := Result + Fmt(fm13) + 'e13 ';
  if not Math.SameValue(fm14, 0.0) then Result := Result + Fmt(fm14) + 'e14 ';
  if not Math.SameValue(fm23, 0.0) then Result := Result + Fmt(fm23) + 'e23 ';
  if not Math.SameValue(fm24, 0.0) then Result := Result + Fmt(fm24) + 'e24 ';
  if not Math.SameValue(fm34, 0.0) then Result := Result + Fmt(fm34) + 'e34 ';

  i := Length(Result);
  if i > 0 then
    SetLength(Result, i - 1)
  else
    Result := '0e12';
end;

function TBivectorHelper.ToMultivector: TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := 0.0;
  result.fm2    := 0.0;
  result.fm3    := 0.0;
  result.fm4    := 0.0;
  result.fm12   := fm12;
  result.fm13   := fm13;
  result.fm14   := fm14;
  result.fm23   := fm23;
  result.fm24   := fm24;
  result.fm34   := fm34;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := 0.0;
end;

// TVectorHelper

function TVectorHelper.Dual: TTrivector;
begin
  // Self * e1234;
  result.fm123 := -fm4;
  result.fm124 :=  fm3;
  result.fm134 := -fm2;
  result.fm234 :=  fm1;
end;

function TVectorHelper.Inverse: TVector;
begin
  result.fm1 := -fm1;
  result.fm2 := -fm2;
  result.fm3 := -fm3;
  result.fm4 := -fm4;
end;

function TVectorHelper.Reverse: TVector;
begin
  result.fm1 := fm1;
  result.fm2 := fm2;
  result.fm3 := fm3;
  result.fm4 := fm4;
end;

function TVectorHelper.Conjugate: TVector;
begin
  result.fm1 := -fm1;
  result.fm2 := -fm2;
  result.fm3 := -fm3;
  result.fm4 := -fm4;
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
  result := fm1*fm1 + fm2*fm2 + fm3*fm3 + fm4*fm4;
end;

function TVectorHelper.Dot(const AVector: TVector): double;
begin
  result :=  fm1 * AVector.fm1
            +fm2 * AVector.fm2
            +fm3 * AVector.fm3
            +fm4 * AVector.fm4;
end;

function TVectorHelper.Dot(const AVector: TBivector): TVector;
begin
  result.fm1 := -fm2 * AVector.fm12
                -fm3 * AVector.fm13
                -fm4 * AVector.fm14;
  result.fm2 :=  fm1 * AVector.fm12
                -fm3 * AVector.fm23
                -fm4 * AVector.fm24;
  result.fm3 :=  fm1 * AVector.fm13
                +fm2 * AVector.fm23
                -fm4 * AVector.fm34;
  result.fm4 :=  fm1 * AVector.fm14
                +fm2 * AVector.fm24
                +fm3 * AVector.fm34;
end;

function TVectorHelper.Dot(const AVector: TTrivector): TBivector;
begin
  result.fm12 :=  fm3 * AVector.fm123
                 +fm4 * AVector.fm124;
  result.fm13 := -fm2 * AVector.fm123
                 +fm4 * AVector.fm134;
  result.fm14 := -fm2 * AVector.fm124
                 -fm3 * AVector.fm134;
  result.fm23 := +fm1 * AVector.fm123
                 +fm4 * AVector.fm234;
  result.fm24 :=  fm1 * AVector.fm124
                 -fm3 * AVector.fm234;
  result.fm34 :=  fm1 * AVector.fm134
                 +fm2 * AVector.fm234;

end;

function TVectorHelper.Dot(const AVector: TQuadrivector): TTrivector;
begin
  result.fm123 := -fm4 * AVector.fm1234;
  result.fm124 :=  fm3 * AVector.fm1234;
  result.fm134 := -fm2 * AVector.fm1234;
  result.fm234 :=  fm1 * AVector.fm1234;
end;

function TVectorHelper.Dot(const AVector: TMultivector): TMultivector;
begin
  result.fm0    :=  fm1 * AVector.fm1
                   +fm2 * AVector.fm2
                   +fm3 * AVector.fm3
                   +fm4 * AVector.fm4;
  result.fm1    :=  fm1 * AVector.fm0
                   -fm2 * AVector.fm12
                   -fm3 * AVector.fm13
                   -fm4 * AVector.fm14;
  result.fm2    :=  fm1 * AVector.fm12
                   +fm2 * AVector.fm0
                   -fm3 * AVector.fm23
                   -fm4 * AVector.fm24;
  result.fm3    :=  fm1 * AVector.fm13
                   +fm2 * AVector.fm23
                   +fm3 * AVector.fm0
                   -fm4 * AVector.fm34;
  result.fm4    :=  fm1 * AVector.fm14
                   +fm2 * AVector.fm24
                   +fm3 * AVector.fm34
                   +fm4 * AVector.fm0;
  result.fm12   :=  fm3 * AVector.fm123
                   +fm4 * AVector.fm124;
  result.fm13   := -fm2 * AVector.fm123
                   +fm4 * AVector.fm134;
  result.fm14   := -fm2 * AVector.fm124
                   -fm3 * AVector.fm134;
  result.fm23   :=  fm1 * AVector.fm123
                   +fm4 * AVector.fm234;
  result.fm24   :=  fm1 * AVector.fm124
                   -fm3 * AVector.fm234;
  result.fm34   :=  fm1 * AVector.fm134
                   +fm2 * AVector.fm234;
  result.fm123  := -fm4 * AVector.fm1234;
  result.fm124  :=  fm3 * AVector.fm1234;
  result.fm134  := -fm2 * AVector.fm1234;
  result.fm234  :=  fm1 * AVector.fm1234;
  result.fm1234 :=  0.0;
end;

function TVectorHelper.Wedge(const AVector: TVector): TBivector;
begin
  result.fm12 := fm1 * AVector.fm2 -fm2 * AVector.fm1;
  result.fm13 := fm1 * AVector.fm3 -fm3 * AVector.fm1;
  result.fm14 := fm1 * AVector.fm4 -fm4 * AVector.fm1;
  result.fm23 := fm2 * AVector.fm3 -fm3 * AVector.fm2;
  result.fm24 := fm2 * AVector.fm4 -fm4 * AVector.fm2;
  result.fm34 := fm3 * AVector.fm4 -fm4 * AVector.fm3;
end;

function TVectorHelper.Wedge(const AVector: TBivector): TTrivector;
begin
  result.fm123 := fm1 * AVector.fm23 -fm2 * AVector.fm13 +fm3 * AVector.fm12;
  result.fm124 := fm1 * AVector.fm24 -fm2 * AVector.fm14 +fm4 * AVector.fm12;
  result.fm134 := fm1 * AVector.fm34 -fm3 * AVector.fm14 +fm4 * AVector.fm13;
  result.fm234 := fm2 * AVector.fm34 -fm3 * AVector.fm24 +fm4 * AVector.fm23;
end;

function TVectorHelper.Wedge(const AVector: TTrivector): TQuadrivector;
begin
  result.fm1234 :=  fm1 * AVector.fm234
                   -fm2 * AVector.fm134
                   +fm3 * AVector.fm124
                   -fm4 * AVector.fm123;
end;

function TVectorHelper.Wedge(const AVector: TQuadrivector): double;
begin
  result := 0.0;
end;

function TVectorHelper.Wedge(const AVector: TMultivector): TMultivector;
begin
  result.fm0    :=  0.0;
  result.fm1    :=  fm1 * AVector.fm0;
  result.fm2    :=  fm2 * AVector.fm0;
  result.fm3    :=  fm3 * AVector.fm0;
  result.fm4    :=  fm4 * AVector.fm0;
  result.fm12   :=  fm1 * AVector.fm2   -fm2 * AVector.fm1;
  result.fm13   :=  fm1 * AVector.fm3   -fm3 * AVector.fm1;
  result.fm14   :=  fm1 * AVector.fm4   -fm4 * AVector.fm1;
  result.fm23   :=  fm2 * AVector.fm3   -fm3 * AVector.fm2;
  result.fm24   :=  fm2 * AVector.fm4   -fm4 * AVector.fm2;
  result.fm34   :=  fm3 * AVector.fm4   -fm4 * AVector.fm3;
  result.fm123  :=  fm1 * AVector.fm23  -fm2 * AVector.fm13  +fm3 * AVector.fm12;
  result.fm124  :=  fm1 * AVector.fm24  -fm2 * AVector.fm14  +fm4 * AVector.fm12;
  result.fm134  :=  fm1 * AVector.fm34  -fm3 * AVector.fm14  +fm4 * AVector.fm13;
  result.fm234  :=  fm2 * AVector.fm34  -fm3 * AVector.fm24  +fm4 * AVector.fm23;
  result.fm1234 :=  fm1 * AVector.fm234 -fm2 * AVector.fm134 +fm3 * AVector.fm124 -fm4 * AVector.fm123;
end;

function TVectorHelper.Projection(const AVector: TVector): TMultivector;
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

function  TVectorHelper.Rejection(const AVector: TBivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Rejection(const AVector: TTrivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Rejection (const AVector: TQuadrivector): TMultivector;
begin
  result := Wedge(AVector) * AVector.Reciprocal;
end;

function TVectorHelper.Rejection (const AVector: TMultivector): TMultivector;
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
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal  * AVector2.Reciprocal;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TTrivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal  * AVector2.Reciprocal;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TQuadrivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal  * AVector2.Reciprocal;
end;

function TVectorHelper.Rotation(const AVector1, AVector2: TMultivector): TMultivector;
begin
  result := AVector2 * AVector1 * Self * AVector1.Reciprocal  * AVector2.Reciprocal;
end;

function TVectorHelper.SameValue(const AValue: TMultivector): boolean;
begin
  result := Math.SameValue(0.0, AValue.fm0   ) and
            Math.SameValue(fm1, AValue.fm1   ) and
            Math.SameValue(fm2, AValue.fm2   ) and
            Math.SameValue(fm3, AValue.fm3   ) and
            Math.SameValue(fm4, AValue.fm4   ) and
            Math.SameValue(0.0, AValue.fm12  ) and
            Math.SameValue(0.0, AValue.fm13  ) and
            Math.SameValue(0.0, AValue.fm14  ) and
            Math.SameValue(0.0, AValue.fm23  ) and
            Math.SameValue(0.0, AValue.fm24  ) and
            Math.SameValue(0.0, AValue.fm34  ) and
            Math.SameValue(0.0, AValue.fm123 ) and
            Math.SameValue(0.0, AValue.fm124 ) and
            Math.SameValue(0.0, AValue.fm134 ) and
            Math.SameValue(0.0, AValue.fm234 ) and
            Math.SameValue(0.0, AValue.fm1234);
end;

function TVectorHelper.SameValue(const AValue: TVector): boolean;
begin
  result := Math.SameValue(fm1, AValue.fm1) and
            Math.SameValue(fm2, AValue.fm2) and
            Math.SameValue(fm3, AValue.fm3) and
            Math.SameValue(fm4, AValue.fm4);
end;

function TVectorHelper.Extract(AComponents: TVectorComponents): TVector;
begin
  Result := NullVector;
  if vc1 in AComponents then result.fm1 := fm1;
  if vc2 in AComponents then result.fm2 := fm2;
  if vc3 in AComponents then result.fm3 := fm3;
  if vc4 in AComponents then result.fm4 := fm4;
end;


function TVectorHelper.ToVerboseString(APrecision, ADigits: longint): string;
var
  i: longint;
begin
  result := '';
  if not Math.SameValue(fm1, 0.0) then Result := Result + Fmt(fm1,  APrecision, ADigits) + 'e1 ';
  if not Math.SameValue(fm2, 0.0) then Result := Result + Fmt(fm2,  APrecision, ADigits) + 'e2 ';
  if not Math.SameValue(fm3, 0.0) then Result := Result + Fmt(fm3,  APrecision, ADigits) + 'e3 ';
  if not Math.SameValue(fm4, 0.0) then Result := Result + Fmt(fm4,  APrecision, ADigits) + 'e4 ';

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
  if not Math.SameValue(fm4, 0.0) then Result := Result + Fmt(fm4) + 'e4 ';

  i := Length(Result);
  if i > 0 then
    SetLength(Result, i - 1)
  else
    Result := '0e1';
end;

function TVectorHelper.ToMultivector: TMultivector;
begin
  result.fm0    := 0.0;
  result.fm1    := fm1;
  result.fm2    := fm2;
  result.fm3    := fm3;
  result.fm4    := fm4;
  result.fm12   := 0.0;
  result.fm13   := 0.0;
  result.fm14   := 0.0;
  result.fm23   := 0.0;
  result.fm24   := 0.0;
  result.fm34   := 0.0;
  result.fm123  := 0.0;
  result.fm124  := 0.0;
  result.fm134  := 0.0;
  result.fm234  := 0.0;
  result.fm1234 := 0.0;
end;

// TVersors

class operator TVersor1.*(const AValue: double; const ASelf: TVersor1): TVector;
begin
  result.fm1 := AValue;
  result.fm2 := 0.0;
  result.fm3 := 0.0;
  result.fm4 := 0.0;
end;

class operator TVersor2.*(const AValue: double; const ASelf: TVersor2): TVector;
begin
  result.fm1 := 0.0;
  result.fm2 := AValue;
  result.fm3 := 0.0;
  result.fm4 := 0.0;
end;

class operator TVersor3.*(const AValue: double; const ASelf: TVersor3): TVector;
begin
  result.fm1 := 0.0;
  result.fm2 := 0.0;
  result.fm3 := AValue;
  result.fm4 := 0.0;
end;

class operator TVersor4.*(const AValue: double; const ASelf: TVersor4): TVector;
begin
  result.fm1 := 0.0;
  result.fm2 := 0.0;
  result.fm3 := 0.0;
  result.fm4 := AValue;
end;

// TBiversors

class operator TBiversor12.*(const AValue: double; const ASelf: TBiversor12): TBivector;
begin
  result.fm12 := AValue;
  result.fm13 := 0.0;
  result.fm14 := 0.0;
  result.fm23 := 0.0;
  result.fm24 := 0.0;
  result.fm34 := 0.0;
end;

class operator TBiversor13.*(const AValue: double; const ASelf: TBiversor13): TBivector;
begin
  result.fm12 := 0.0;
  result.fm13 := AValue;
  result.fm14 := 0.0;
  result.fm23 := 0.0;
  result.fm24 := 0.0;
  result.fm34 := 0.0;
end;

class operator TBiversor14.*(const AValue: double; const ASelf: TBiversor14): TBivector;
begin
  result.fm12 := 0.0;
  result.fm13 := 0.0;
  result.fm14 := AValue;
  result.fm23 := 0.0;
  result.fm24 := 0.0;
  result.fm34 := 0.0;
end;

class operator TBiversor23.*(const AValue: double; const ASelf: TBiversor23): TBivector;
begin
  result.fm12 := 0.0;
  result.fm13 := 0.0;
  result.fm14 := 0.0;
  result.fm23 := AValue;
  result.fm24 := 0.0;
  result.fm34 := 0.0;
end;

class operator TBiversor24.*(const AValue: double; const ASelf: TBiversor24): TBivector;
begin
  result.fm12 := 0.0;
  result.fm13 := 0.0;
  result.fm14 := 0.0;
  result.fm23 := 0.0;
  result.fm24 := AValue;
  result.fm34 := 0.0;
end;

class operator TBiversor34.*(const AValue: double; const ASelf: TBiversor34): TBivector;
begin
  result.fm12 := 0.0;
  result.fm13 := 0.0;
  result.fm14 := 0.0;
  result.fm23 := 0.0;
  result.fm24 := 0.0;
  result.fm34 := AValue;
end;

// TTriversor

class operator TTriversor123.*(const AValue: double; const ASelf: TTriversor123): TTrivector;
begin
  result.fm123 := AValue;
  result.fm124 := 0.0;
  result.fm134 := 0.0;
  result.fm234 := 0.0;
end;

class operator TTriversor124.*(const AValue: double; const ASelf: TTriversor124): TTrivector;
begin
  result.fm123 := 0.0;
  result.fm124 := AValue;
  result.fm134 := 0.0;
  result.fm234 := 0.0;
end;

class operator TTriversor134.*(const AValue: double; const ASelf: TTriversor134): TTrivector;
begin
  result.fm123 := 0.0;
  result.fm124 := 0.0;
  result.fm134 := AValue;
  result.fm234 := 0.0;
end;

class operator TTriversor234.*(const AValue: double; const ASelf: TTriversor234): TTrivector;
begin
  result.fm123 := 0.0;
  result.fm124 := 0.0;
  result.fm134 := 0.0;
  result.fm234 := AValue;
end;

// TQuadriversor

class operator TQuadriversor1234.*(const AValue: double; const ASelf: TQuadriversor1234): TQuadrivector;
begin
  result.fm1234 := AValue;
end;

end.

