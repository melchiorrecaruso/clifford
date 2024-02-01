program cl3test;

{$mode objfpc}{$h+}

uses
  Cl3;

var
  a  : double;
  v  : TVector;
  v1 : TVector;
  v2 : TVector;
  v3 : TVector;
  B  : TBivector;
  I  : TTrivector;
  M  : TMultiVector;

begin
  Writeln('CL3 TEST: begin');

  a := 7;
  v := 5*e1  + 17*e2  + 23*e3;
  B := 3*e12 +  5*e23 +  7*e31;
  I := 2.5*e123;
  M := a + v + B + I;

  // TTrivector
  M  := I.ToMultivector;
  if I.DotProduct   (I) <> M.DotProduct   (M) then Writeln('TEST-101: NOT PASSED');
  if I.DotProduct   (M) <> M.DotProduct   (I) then Writeln('TEST-102: NOT PASSED');
  if I.WedgeProduct (I) <> M.WedgeProduct (M) then Writeln('TEST-103: NOT PASSED');
  if I.WedgeProduct (M) <> M.WedgeProduct (I) then Writeln('TEST-104: NOT PASSED');
  if (I*M)              <> (M*I)              then Writeln('TEST-105: NOT PASSED');
  if (I*I)              <> (M*M)              then Writeln('TEST-106: NOT PASSED');
  if I.Reciprocal       <> M.Reciprocal       then Writeln('TEST-107: NOT PASSED');
  if I.Conjugate        <> M.Conjugate        then Writeln('TEST-108: NOT PASSED');
  if I.Inverse          <> M.Inverse          then Writeln('TEST-109: NOT PASSED');
  if I.Dual             <> M.Dual             then Writeln('TEST-110: NOT PASSED');
  if I.Norm             <> M.Norm(gTrivector) then Writeln('TEST-111: NOT PASSED');
  if I.SquaredNorm      <> M.SquaredNorm      then Writeln('TEST-112: NOT PASSED');
  if I.Reverse          <> M.Reverse          then Writeln('TEST-113: NOT PASSED');

  if (I.Projection(u1  ) <> M.Projection(u1  .ToMultivector)) then Writeln('TEST-114: NOT PASSED');
  if (I.Projection(u2  ) <> M.Projection(u2  .ToMultivector)) then Writeln('TEST-115: NOT PASSED');
  if (I.Projection(u3  ) <> M.Projection(u3  .ToMultivector)) then Writeln('TEST-116: NOT PASSED');
  if (I.Projection(u12 ) <> M.Projection(u12 .ToMultivector)) then Writeln('TEST-117: NOT PASSED');
  if (I.Projection(u23 ) <> M.Projection(u23 .ToMultivector)) then Writeln('TEST-118: NOT PASSED');
  if (I.Projection(u31 ) <> M.Projection(u31 .ToMultivector)) then Writeln('TEST-119: NOT PASSED');
  if (I.Projection(u123) <> M.Projection(u123.ToMultivector)) then Writeln('TEST-120: NOT PASSED');

  if (I.Rejection (u1  ) <> M.Rejection (u1  .ToMultivector)) then Writeln('TEST-121: NOT PASSED');
  if (I.Rejection (u2  ) <> M.Rejection (u2  .ToMultivector)) then Writeln('TEST-122: NOT PASSED');
  if (I.Rejection (u3  ) <> M.Rejection (u3  .ToMultivector)) then Writeln('TEST-123: NOT PASSED');
  if (I.Rejection (u12 ) <> M.Rejection (u12 .ToMultivector)) then Writeln('TEST-124: NOT PASSED');
  if (I.Rejection (u23 ) <> M.Rejection (u23 .ToMultivector)) then Writeln('TEST-125: NOT PASSED');
  if (I.Rejection (u31 ) <> M.Rejection (u31 .ToMultivector)) then Writeln('TEST-126: NOT PASSED');
  if (I.Rejection (u123) <> M.Rejection (u123.ToMultivector)) then Writeln('TEST-127: NOT PASSED');

  if (I.Reflection(u1 ) <> M.Reflection(u1 .ToMultivector)) then Writeln('TEST-128: NOT PASSED');
  if (I.Reflection(u2 ) <> M.Reflection(u2 .ToMultivector)) then Writeln('TEST-129: NOT PASSED');
  if (I.Reflection(u3 ) <> M.Reflection(u3 .ToMultivector)) then Writeln('TEST-130: NOT PASSED');
  if (I.Reflection(u12) <> M.Reflection(u12.ToMultivector)) then Writeln('TEST-131: NOT PASSED');
  if (I.Reflection(u23) <> M.Reflection(u23.ToMultivector)) then Writeln('TEST-132: NOT PASSED');
  if (I.Reflection(u31) <> M.Reflection(u31.ToMultivector)) then Writeln('TEST-133: NOT PASSED');

  if (I.Rotation(u1 , u2 ) <> M.Rotation(u1 .ToMultivector, u2 .ToMultivector)) then Writeln('TEST-134: NOT PASSED');
  if (I.Rotation(u2 , u3 ) <> M.Rotation(u2 .ToMultivector, u3 .ToMultivector)) then Writeln('TEST-135: NOT PASSED');
  if (I.Rotation(u3 , u1 ) <> M.Rotation(u3 .ToMultivector, u1 .ToMultivector)) then Writeln('TEST-136: NOT PASSED');
  if (I.Rotation(u12, u23) <> M.Rotation(u12.ToMultivector, u23.ToMultivector)) then Writeln('TEST-137: NOT PASSED');
  if (I.Rotation(u23, u31) <> M.Rotation(u23.ToMultivector, u31.ToMultivector)) then Writeln('TEST-138: NOT PASSED');
  if (I.Rotation(u31, u12) <> M.Rotation(u31.ToMultivector, u12.ToMultivector)) then Writeln('TEST-139: NOT PASSED');

  // TBivector
  M  := B.ToMultivector;
  if B.DotProduct   (B) <> M.DotProduct   (M) then Writeln('TEST-201: NOT PASSED');
  if B.DotProduct   (M) <> M.DotProduct   (B) then Writeln('TEST-202: NOT PASSED');
  if B.WedgeProduct (B) <> M.WedgeProduct (M) then Writeln('TEST-203: NOT PASSED');
  if B.WedgeProduct (M) <> M.WedgeProduct (B) then Writeln('TEST-204: NOT PASSED');
  if (B*M)              <> (M*B)              then Writeln('TEST-205: NOT PASSED');
  if (B*B)              <> (M*M)              then Writeln('TEST-206: NOT PASSED');
  if B.Reciprocal       <> M.Reciprocal       then Writeln('TEST-208: NOT PASSED');
  if B.Conjugate        <> M.Conjugate        then Writeln('TEST-209: NOT PASSED');
  if B.Inverse          <> M.Inverse          then Writeln('TEST-210: NOT PASSED');
  if B.Dual             <> M.Dual             then Writeln('TEST-211: NOT PASSED');
  if B.Norm             <> M.Norm(gBivector)  then Writeln('TEST-212: NOT PASSED');
  if B.SquaredNorm      <> M.SquaredNorm      then Writeln('TEST-213: NOT PASSED');
  if B.Reverse          <> M.Reverse          then Writeln('TEST-214: NOT PASSED');

  if (B.Projection(u1  ) <> M.Projection(u1  .ToMultivector)) then Writeln('TEST-215: NOT PASSED');
  if (B.Projection(u2  ) <> M.Projection(u2  .ToMultivector)) then Writeln('TEST-216: NOT PASSED');
  if (B.Projection(u3  ) <> M.Projection(u3  .ToMultivector)) then Writeln('TEST-217: NOT PASSED');
  if (B.Projection(u12 ) <> M.Projection(u12 .ToMultivector)) then Writeln('TEST-218: NOT PASSED');
  if (B.Projection(u23 ) <> M.Projection(u23 .ToMultivector)) then Writeln('TEST-219: NOT PASSED');
  if (B.Projection(u31 ) <> M.Projection(u31 .ToMultivector)) then Writeln('TEST-220: NOT PASSED');
  if (B.Projection(u123) <> M.Projection(u123.ToMultivector)) then Writeln('TEST-221: NOT PASSED');

  if (B.Rejection (u1  ) <> M.Rejection (u1  .ToMultivector)) then Writeln('TEST-222: NOT PASSED');
  if (B.Rejection (u2  ) <> M.Rejection (u2  .ToMultivector)) then Writeln('TEST-223: NOT PASSED');
  if (B.Rejection (u3  ) <> M.Rejection (u3  .ToMultivector)) then Writeln('TEST-224: NOT PASSED');
  if (B.Rejection (u12 ) <> M.Rejection (u12 .ToMultivector)) then Writeln('TEST-225: NOT PASSED');
  if (B.Rejection (u23 ) <> M.Rejection (u23 .ToMultivector)) then Writeln('TEST-226: NOT PASSED');
  if (B.Rejection (u31 ) <> M.Rejection (u31 .ToMultivector)) then Writeln('TEST-227: NOT PASSED');
  if (B.Rejection (u123) <> M.Rejection (u123.ToMultivector)) then Writeln('TEST-228: NOT PASSED');

  if (B.Reflection(u1  ) <> M.Reflection(u1  .ToMultivector)) then Writeln('TEST-229: NOT PASSED');
  if (B.Reflection(u2  ) <> M.Reflection(u2  .ToMultivector)) then Writeln('TEST-230: NOT PASSED');
  if (B.Reflection(u3  ) <> M.Reflection(u3  .ToMultivector)) then Writeln('TEST-231: NOT PASSED');
  if (B.Reflection(u12 ) <> M.Reflection(u12 .ToMultivector)) then Writeln('TEST-232: NOT PASSED');
  if (B.Reflection(u23 ) <> M.Reflection(u23 .ToMultivector)) then Writeln('TEST-233: NOT PASSED');
  if (B.Reflection(u31 ) <> M.Reflection(u31 .ToMultivector)) then Writeln('TEST-234: NOT PASSED');
  if (B.Reflection(u123) <> M.Reflection(u123.ToMultivector)) then Writeln('TEST-235: NOT PASSED');

  if (B.Rotation(u1 , u2 ) <> M.Rotation(u1 .ToMultivector, u2 .ToMultivector)) then Writeln('TEST-236: NOT PASSED');
  if (B.Rotation(u2 , u3 ) <> M.Rotation(u2 .ToMultivector, u3 .ToMultivector)) then Writeln('TEST-237: NOT PASSED');
  if (B.Rotation(u3 , u1 ) <> M.Rotation(u3 .ToMultivector, u1 .ToMultivector)) then Writeln('TEST-238: NOT PASSED');
  if (B.Rotation(u12, u23) <> M.Rotation(u12.ToMultivector, u23.ToMultivector)) then Writeln('TEST-239: NOT PASSED');
  if (B.Rotation(u23, u31) <> M.Rotation(u23.ToMultivector, u31.ToMultivector)) then Writeln('TEST-240: NOT PASSED');
  if (B.Rotation(u31, u12) <> M.Rotation(u31.ToMultivector, u12.ToMultivector)) then Writeln('TEST-241: NOT PASSED');

  // TVector
  M  := v.ToMultivector;
  if v.DotProduct   (v) <> M.DotProduct   (M) then Writeln('TEST-301: PASSED');
  if v.DotProduct   (M) <> M.DotProduct   (v) then Writeln('TEST-302: PASSED');
  if v.WedgeProduct (v) <> M.WedgeProduct (M) then Writeln('TEST-303: PASSED');
  if v.WedgeProduct (M) <> M.WedgeProduct (v) then Writeln('TEST-304: PASSED');
  if (v*M)              <> (M*v)              then Writeln('TEST-305: PASSED');
  if (v*v)              <> (M*M)              then Writeln('TEST-306: PASSED');
  if v.Reciprocal       <> M.Reciprocal       then Writeln('TEST-307: PASSED');
  if v.Conjugate        <> M.Conjugate        then Writeln('TEST-308: PASSED');
  if v.Inverse          <> M.Inverse          then Writeln('TEST-309: PASSED');
  if v.Dual             <> M.Dual             then Writeln('TEST-310: PASSED');
  if v.Norm             <> M.Norm(gVector)    then Writeln('TEST-311: PASSED');
  if v.SquaredNorm      <> M.SquaredNorm      then Writeln('TEST-312: PASSED');
  if v.Reverse          <> M.Reverse          then Writeln('TEST-313: PASSED');

  if (v.Projection(u1  ) <> M.Projection(u1  .ToMultivector)) then Writeln('TEST-314: NOT PASSED');
  if (v.Projection(u2  ) <> M.Projection(u2  .ToMultivector)) then Writeln('TEST-315: NOT PASSED');
  if (v.Projection(u3  ) <> M.Projection(u3  .ToMultivector)) then Writeln('TEST-316: NOT PASSED');
  if (v.Projection(u12 ) <> M.Projection(u12 .ToMultivector)) then Writeln('TEST-317: NOT PASSED');
  if (v.Projection(u23 ) <> M.Projection(u23 .ToMultivector)) then Writeln('TEST-318: NOT PASSED');
  if (v.Projection(u31 ) <> M.Projection(u31 .ToMultivector)) then Writeln('TEST-319: NOT PASSED');
  if (v.Projection(u123) <> M.Projection(u123.ToMultivector)) then Writeln('TEST-320: NOT PASSED');

  if (v.Rejection (u1  ) <> M.Rejection (u1  .ToMultivector)) then Writeln('TEST-321: NOT PASSED');
  if (v.Rejection (u2  ) <> M.Rejection (u2  .ToMultivector)) then Writeln('TEST-322: NOT PASSED');
  if (v.Rejection (u3  ) <> M.Rejection (u3  .ToMultivector)) then Writeln('TEST-323: NOT PASSED');
  if (v.Rejection (u12 ) <> M.Rejection (u12 .ToMultivector)) then Writeln('TEST-324: NOT PASSED');
  if (v.Rejection (u23 ) <> M.Rejection (u23 .ToMultivector)) then Writeln('TEST-325: NOT PASSED');
  if (v.Rejection (u31 ) <> M.Rejection (u31 .ToMultivector)) then Writeln('TEST-326: NOT PASSED');
  if (v.Rejection (u123) <> M.Rejection (u123.ToMultivector)) then Writeln('TEST-327: NOT PASSED');

  if (v.Reflection(u1  ) <> M.Reflection(u1  .ToMultivector)) then Writeln('TEST-328: NOT PASSED');
  if (v.Reflection(u2  ) <> M.Reflection(u2  .ToMultivector)) then Writeln('TEST-329: NOT PASSED');
  if (v.Reflection(u3  ) <> M.Reflection(u3  .ToMultivector)) then Writeln('TEST-330: NOT PASSED');
  if (v.Reflection(u12 ) <> M.Reflection(u12 .ToMultivector)) then Writeln('TEST-331: NOT PASSED');
  if (v.Reflection(u23 ) <> M.Reflection(u23 .ToMultivector)) then Writeln('TEST-332: NOT PASSED');
  if (v.Reflection(u31 ) <> M.Reflection(u31 .ToMultivector)) then Writeln('TEST-333: NOT PASSED');
  if (v.Reflection(u123) <> M.Reflection(u123.ToMultivector)) then Writeln('TEST-334: NOT PASSED');

  if (v.Rotation(u1 , u2 ) <> M.Rotation(u1 .ToMultivector, u2 .ToMultivector)) then Writeln('TEST-335: NOT PASSED');
  if (v.Rotation(u2 , u3 ) <> M.Rotation(u2 .ToMultivector, u3 .ToMultivector)) then Writeln('TEST-336: NOT PASSED');
  if (v.Rotation(u3 , u1 ) <> M.Rotation(u3 .ToMultivector, u1 .ToMultivector)) then Writeln('TEST-337: NOT PASSED');
  if (v.Rotation(u12, u23) <> M.Rotation(u12.ToMultivector, u23.ToMultivector)) then Writeln('TEST-338: NOT PASSED');
  if (v.Rotation(u23, u31) <> M.Rotation(u23.ToMultivector, u31.ToMultivector)) then Writeln('TEST-339: NOT PASSED');
  if (v.Rotation(u31, u12) <> M.Rotation(u31.ToMultivector, u12.ToMultivector)) then Writeln('TEST-340: NOT PASSED');

  if ((4.5*e1).CrossProduct(2.0*e2)                     ) <> (9.0*e3) then Writeln('TEST-341: NOT PASSED');
  if ((9.0*e3).CrossProduct(4.5*e1)/(4.5*e1).SquaredNorm) <> (2.0*e2) then Writeln('TEST-342: NOT PASSED');
  if ((2.0*e2).CrossProduct(9.0*e3)/(2.0*e2).SquaredNorm) <> (4.5*e1) then Writeln('TEST-343: NOT PASSED');

  Writeln('CL3 TEST: end.');
end.
