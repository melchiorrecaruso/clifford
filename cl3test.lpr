program cl3test;

{$mode objfpc}{$h+}

uses
  Cl3, Math, SysUtils;

var
  k : longint;
  a : double;
  v, v1, v2, v3 : TVector;
  B, B1, B2, B3 : TBivector;
  I, I1, I2, I3 : TTrivector;
  M, M1, M2, M3 : TMultivector;

  function Rnd: double;
  var
    i: longint;
  begin
    i := RandomRange(-MaxSmallint, MaxSmallint);
    while (i = 0) do
    begin
      i := RandomRange(-MaxSmallint, MaxSmallint);
    end;
    result := i;
  end;


begin
  Randomize;

  Writeln('CL3 TEST: begin');
  for k := 0 to 1000000 do
  begin
    a := Rnd;
    v := Rnd*e1  + Rnd*e2  + Rnd*e3;
    B := Rnd*e12 + Rnd*e23 + Rnd*e31;
    I := Rnd*e123;
    M := a + v + B + I;

    // TTrivector
    M  := I.ToMultivector;
    if I.Dot          (I) <> M.Dot               (M) then Writeln('TEST-101: NOT PASSED');
    if I.Dot          (M) <> M.Dot               (I) then Writeln('TEST-102: NOT PASSED');
    if I.Wedge        (I) <> M.Wedge             (M) then Writeln('TEST-103: NOT PASSED');
    if I.Wedge        (M) <> M.Wedge             (I) then Writeln('TEST-104: NOT PASSED');
    if (I*M)              <> (M*I)                   then Writeln('TEST-105: NOT PASSED');
    if (I*I)              <> (M*M)                   then Writeln('TEST-106: NOT PASSED');
    if I.Reciprocal       <> M.Reciprocal            then Writeln('TEST-107: NOT PASSED');
    if I.Conjugate        <> M.Conjugate             then Writeln('TEST-108: NOT PASSED');
    if I.Inverse          <> M.Inverse               then Writeln('TEST-109: NOT PASSED');
    if I.Dual             <> M.Dual                  then Writeln('TEST-110: NOT PASSED');
    if I.Norm             <> M.ExtractTrivector.Norm then Writeln('TEST-111: NOT PASSED');
    if I.SquaredNorm      <> M.SquaredNorm           then Writeln('TEST-112: NOT PASSED');
    if I.Reverse          <> M.Reverse               then Writeln('TEST-113: NOT PASSED');
    if not SameValue(I/I, 1)                         then Writeln('TEST-114: NOT PASSED');

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
    if B.Dot          (B) <> M.Dot                (M) then Writeln('TEST-201: NOT PASSED');
    if B.Dot          (M) <> M.Dot                (B) then Writeln('TEST-202: NOT PASSED');
    if B.Wedge        (B) <> M.Wedge              (M) then Writeln('TEST-203: NOT PASSED');
    if B.Wedge        (M) <> M.Wedge              (B) then Writeln('TEST-204: NOT PASSED');
    if (B*M)              <> (M*B)                    then Writeln('TEST-205: NOT PASSED');
    if (B*B)              <> (M*M)                    then Writeln('TEST-206: NOT PASSED');
    if B.Reciprocal       <> M.Reciprocal             then Writeln('TEST-208: NOT PASSED');
    if B.Conjugate        <> M.Conjugate              then Writeln('TEST-209: NOT PASSED');
    if B.Inverse          <> M.Inverse                then Writeln('TEST-210: NOT PASSED');
    if B.Dual             <> M.Dual                   then Writeln('TEST-211: NOT PASSED');
    if B.Norm             <> M.ExtractBivector.Norm   then Writeln('TEST-212: NOT PASSED');
    if B.SquaredNorm      <> M.SquaredNorm            then Writeln('TEST-213: NOT PASSED');
    if B.Reverse          <> M.Reverse                then Writeln('TEST-214: NOT PASSED');
    if not (B/B).SameValue(1)                         then Writeln('TEST-215: NOT PASSED');

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
    if v.Dot           (v) <> M.Dot            (M) then Writeln('TEST-301: NOT PASSED');
    if v.Dot           (M) <> M.Dot            (v) then Writeln('TEST-302: NOT PASSED');
    if v.Wedge         (v) <> M.Wedge          (M) then Writeln('TEST-303: NOT PASSED');
    if v.Wedge         (M) <> M.Wedge          (v) then Writeln('TEST-304: NOT PASSED');
    if (v*M)               <> (M*v)                then Writeln('TEST-305: NOT PASSED');
    if (v*v)               <> (M*M)                then Writeln('TEST-306: NOT PASSED');
    if v.Reciprocal        <> M.Reciprocal         then Writeln('TEST-307: NOT PASSED');
    if v.Conjugate         <> M.Conjugate          then Writeln('TEST-308: NOT PASSED');
    if v.Inverse           <> M.Inverse            then Writeln('TEST-309: NOT PASSED');
    if v.Dual              <> M.Dual               then Writeln('TEST-310: NOT PASSED');
    if v.Norm              <> M.ExtractVector.Norm then Writeln('TEST-311: NOT PASSED');
    if v.SquaredNorm       <> M.SquaredNorm        then Writeln('TEST-312: NOT PASSED');
    if v.Reverse           <> M.Reverse            then Writeln('TEST-313: NOT PASSED');
    if not (v/v).SameValue(1)                      then Writeln('TEST-314: NOT PASSED');

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
  end;

  Writeln('CL3 TEST: end.');

  v1 := Rnd*e1 + Rnd*e2 + Rnd*e3;
  v2 := Rnd*e1 + Rnd*e2 + Rnd*e3;
  v3 := Rnd*e1 + Rnd*e2 + Rnd*e3;

  B1 := Rnd*e12 + Rnd*e23 + Rnd*e31;
  B2 := Rnd*e12 + Rnd*e23 + Rnd*e31;
  B3 := Rnd*e12 + Rnd*e23 + Rnd*e31;

  I1 := Rnd*e123;
  I2 := Rnd*e123;
  I3 := Rnd*e123;

  M1 := Rnd + Rnd*e1 + Rnd*e2 + Rnd*e3 + Rnd*e12 + Rnd*e23 + Rnd*e31 + Rnd*e123;
  M2 := Rnd + Rnd*e1 + Rnd*e2 + Rnd*e3 + Rnd*e12 + Rnd*e23 + Rnd*e31 + Rnd*e123;
  M3 := Rnd + Rnd*e1 + Rnd*e2 + Rnd*e3 + Rnd*e12 + Rnd*e23 + Rnd*e31 + Rnd*e123;

  writeln;
  Writeln(Format('v1 = %s (Norm = %s)', [v1.ToString, v1.Norm.ToString]));
  Writeln(Format('v2 = %s (Norm = %s)', [v2.ToString, v2.Norm.ToString]));
  Writeln(Format('v3 = %s (Norm = %s)', [v3.ToString, v3.Norm.ToString]));
  Writeln(Format('v1/v1 = %s', [(v1/v1).ToString]));
  Writeln(Format('v2/v2 = %s', [(v2/v2).ToString]));
  Writeln(Format('v3/v3 = %s', [(v3/v3).ToString]));

  Writeln(Format('B1 = %s (Norm = %s)', [B1.ToString, B1.Norm.ToString]));
  Writeln(Format('B2 = %s (Norm = %s)', [B2.ToString, B2.Norm.ToString]));
  Writeln(Format('B3 = %s (Norm = %s)', [B3.ToString, B3.Norm.ToString]));
  Writeln(Format('B1/B1 = %s', [(B1/B1).ToString]));
  Writeln(Format('B2/B2 = %s', [(B2/B2).ToString]));
  Writeln(Format('B3/B3 = %s', [(B3/B3).ToString]));

  Writeln(Format('I1 = %s (Norm = %s)', [I1.ToString, I1.Norm.ToString]));
  Writeln(Format('I2 = %s (Norm = %s)', [I2.ToString, I2.Norm.ToString]));
  Writeln(Format('I3 = %s (Norm = %s)', [I3.ToString, I2.Norm.ToString]));
  Writeln(Format('I1/I1 = %s', [(I1/I1).ToString]));
  Writeln(Format('I2/I2 = %s', [(I2/I2).ToString]));
  Writeln(Format('I3/I3 = %s', [(I3/I3).ToString]));

  Writeln(Format('M1 = %s (Norm = %s)', [M1.ToString, M1.Norm.ToString]));
  Writeln(Format('M2 = %s (Norm = %s)', [M2.ToString, M2.Norm.ToString]));
  Writeln(Format('M3 = %s (Norm = %s)', [M3.ToString, M3.Norm.ToString]));
  Writeln(Format('M1/M1 = %s', [(M1/M1).ToString]));
  Writeln(Format('M2/M2 = %s', [(M2/M2).ToString]));
  Writeln(Format('M3/M3 = %s', [(M3/M3).ToString]));

  Writeln(Format(' 1/M3 = (%s)', [(M3.Reciprocal            ).ToString]));
  Writeln(Format(' 1/M3 = (%s)', [(M3.Reverse/M3.SquaredNorm).ToString]));
  Writeln(Format('M.Normalized      = %s', [(M3.Normalized     ).ToString]));
  Writeln(Format('M.Normalized.Norm = %s', [(M3.Normalized.Norm).ToString]));

  writeln;
  writeln('TVECTOR');
  writeln('Dot       : vector &      vector  : ',  v1.ToMultivector.Dot(v2.ToMultivector).IsA);
  writeln('Dot       : vector &    bivector  : ',  v1.ToMultivector.Dot(B2.ToMultivector).IsA);
  writeln('Dot       : vector &   trivector  : ',  v1.ToMultivector.Dot(I2.ToMultivector).IsA);
  writeln('Dot       : vector & multivector  : ',  v1.ToMultivector.Dot(M2              ).IsA);
  writeln;
  writeln('Wedge     : vector &      vector  : ',  v1.ToMultivector.Wedge(v2.ToMultivector).IsA);
  writeln('Wedge     : vector &    bivector  : ',  v1.ToMultivector.Wedge(B2.ToMultivector).IsA);
  writeln('Wedge     : vector &   trivector  : ',  v1.ToMultivector.Wedge(I2.ToMultivector).IsA);
  writeln('Wedge     : vector & multivector  : ',  v1.ToMultivector.Wedge(M2              ).IsA);
  writeln;
  writeln('Geometric : vector &      vector  : ', (v1.ToMultivector*v2.ToMultivector).IsA);
  writeln('Geometric : vector &    bivector  : ', (v1.ToMultivector*B2.ToMultivector).IsA);
  writeln('Geometric : vector &   trivector  : ', (v1.ToMultivector*I2.ToMultivector).IsA);
  writeln('Geometric : vector & multivector  : ', (v1.ToMultivector*M2              ).IsA);
  writeln;
  writeln('Projection: vector &      vector  : ',  v1.ToMultivector.Projection(v2.ToMultivector).IsA);
  writeln('Projection: vector &    bivector  : ',  v1.ToMultivector.Projection(B2.ToMultivector).IsA);
  writeln('Projection: vector &   trivector  : ',  v1.ToMultivector.Projection(I2.ToMultivector).IsA);
  writeln('Projection: vector & multivector  : ',  v1.ToMultivector.Projection(M2              ).IsA);
  writeln;
  writeln('Rejection : vector &      vector  : ',  v1.ToMultivector.Rejection(v2.ToMultivector).IsA);
  writeln('Rejection : vector &    bivector  : ',  v1.ToMultivector.Rejection(B2.ToMultivector).IsA);
  writeln('Rejection : vector &   trivector  : ',  v1.ToMultivector.Rejection(I2.ToMultivector).IsA);
  writeln('Rejection : vector & multivector  : ',  v1.ToMultivector.Rejection(M2              ).IsA);
  writeln;
  writeln('Reflection: vector &      vector  : ',  v1.ToMultivector.Reflection(v2.ToMultivector).IsA);
  writeln('Reflection: vector &    bivector  : ',  v1.ToMultivector.Reflection(B2.ToMultivector).IsA);
  writeln('Reflection: vector &   trivector  : ',  v1.ToMultivector.Reflection(I2.ToMultivector).IsA);
  writeln('Reflection: vector & multivector  : ',  v1.ToMultivector.Reflection(M2              ).IsA);
  writeln;
  writeln('Rotation  : vector &      vectors : ',  v1.ToMultivector.Rotation(v2.ToMultivector, v3.ToMultivector).IsA);
  writeln('Rotation  : vector &    bivectors : ',  v1.ToMultivector.Rotation(B2.ToMultivector, B3.ToMultivector).IsA);
  writeln('Rotation  : vector &   trivectors : ',  v1.ToMultivector.Rotation(I2.ToMultivector, I3.ToMultivector).IsA);
  writeln('Rotation  : vector & multivectors : ',  v1.ToMultivector.Rotation(M2              , M3).IsA);

  writeln;
  writeln('TBIVECTOR');
  writeln('Dot       : bivector &      vector  : ',  B1.ToMultivector.Dot(v2.ToMultivector).IsA);
  writeln('Dot       : bivector &    bivector  : ',  B1.ToMultivector.Dot(B2.ToMultivector).IsA);
  writeln('Dot       : bivector &   trivector  : ',  B1.ToMultivector.Dot(I2.ToMultivector).IsA);
  writeln('Dot       : bivector & multivector  : ',  B1.ToMultivector.Dot(M2              ).IsA);
  writeln;
  writeln('Wedge     : bivector &      vector  : ',  B1.ToMultivector.Wedge(v2.ToMultivector).IsA);
  writeln('Wedge     : bivector &    bivector  : ',  B1.ToMultivector.Wedge(B2.ToMultivector).IsA);
  writeln('Wedge     : bivector &   trivector  : ',  B1.ToMultivector.Wedge(I2.ToMultivector).IsA);
  writeln('Wedge     : bivector & multivector  : ',  B1.ToMultivector.Wedge(M2              ).IsA);
  writeln;
  writeln('Geometric : bivector &      vector  : ', (B1.ToMultivector*v2.ToMultivector).IsA);
  writeln('Geometric : bivector &    bivector  : ', (B1.ToMultivector*B2.ToMultivector).IsA);
  writeln('Geometric : bivector &   trivector  : ', (B1.ToMultivector*I2.ToMultivector).IsA);
  writeln('Geometric : bivector & multivector  : ', (B1.ToMultivector*M2              ).IsA);
  writeln;
  writeln('Projection: bivector &      vector  : ',  B1.ToMultivector.Projection(v2.ToMultivector).IsA);
  writeln('Projection: bivector &    bivector  : ',  B1.ToMultivector.Projection(B2.ToMultivector).IsA);
  writeln('Projection: bivector &   trivector  : ',  B1.ToMultivector.Projection(I2.ToMultivector).IsA);
  writeln('Projection: bivector & multivector  : ',  B1.ToMultivector.Projection(M2              ).IsA);
  writeln;
  writeln('Rejection : bivector &      vector  : ',  B1.ToMultivector.Rejection(v2.ToMultivector).IsA);
  writeln('Rejection : bivector &    bivector  : ',  B1.ToMultivector.Rejection(B2.ToMultivector).IsA);
  writeln('Rejection : bivector &   trivector  : ',  B1.ToMultivector.Rejection(I2.ToMultivector).IsA);
  writeln('Rejection : bivector & multivector  : ',  B1.ToMultivector.Rejection(M2              ).IsA);
  writeln;
  writeln('Reflection: bivector &      vector  : ',  B1.ToMultivector.Reflection(v2.ToMultivector).IsA);
  writeln('Reflection: bivector &    bivector  : ',  B1.ToMultivector.Reflection(B2.ToMultivector).IsA);
  writeln('Reflection: bivector &   trivector  : ',  B1.ToMultivector.Reflection(I2.ToMultivector).IsA);
  writeln('Reflection: bivector & multivector  : ',  B1.ToMultivector.Reflection(M2              ).IsA);
  writeln;
  writeln('Rotation  : bivector &      vectors : ',  B1.ToMultivector.Rotation(v2.ToMultivector, v3.ToMultivector).IsA);
  writeln('Rotation  : bivector &    bivectors : ',  B1.ToMultivector.Rotation(B2.ToMultivector, B3.ToMultivector).IsA);
  writeln('Rotation  : bivector &   trivectors : ',  B1.ToMultivector.Rotation(I2.ToMultivector, I3.ToMultivector).IsA);
  writeln('Rotation  : bivector & multivectors : ',  B1.ToMultivector.Rotation(M2              , M3).IsA);

  writeln;
  writeln('TTRIVECTOR');
  writeln('Dot       : trivector &      vector  : ',  I1.ToMultivector.Dot(v2.ToMultivector).IsA);
  writeln('Dot       : trivector &    bivector  : ',  I1.ToMultivector.Dot(B2.ToMultivector).IsA);
  writeln('Dot       : trivector &   trivector  : ',  I1.ToMultivector.Dot(I2.ToMultivector).IsA);
  writeln('Dot       : trivector & multivector  : ',  I1.ToMultivector.Dot(M2              ).IsA);
  writeln;
  writeln('Wedge     : trivector &      vector  : ',  I1.ToMultivector.Wedge(v2.ToMultivector).IsA);
  writeln('Wedge     : trivector &    bivector  : ',  I1.ToMultivector.Wedge(B2.ToMultivector).IsA);
  writeln('Wedge     : trivector &   trivector  : ',  I1.ToMultivector.Wedge(I2.ToMultivector).IsA);
  writeln('Wedge     : trivector & multivector  : ',  I1.ToMultivector.Wedge(M2              ).IsA);
  writeln;
  writeln('Geometric : trivector &      vector  : ', (I1.ToMultivector*v2.ToMultivector).IsA);
  writeln('Geometric : trivector &    bivector  : ', (I1.ToMultivector*B2.ToMultivector).IsA);
  writeln('Geometric : trivector &   trivector  : ', (I1.ToMultivector*I2.ToMultivector).IsA);
  writeln('Geometric : trivector & multivector  : ', (I1.ToMultivector*M2              ).IsA);
  writeln;
  writeln('Projection: trivector &      vector  : ',  I1.ToMultivector.Projection(v2.ToMultivector).IsA);
  writeln('Projection: trivector &    bivector  : ',  I1.ToMultivector.Projection(B2.ToMultivector).IsA);
  writeln('Projection: trivector &   trivector  : ',  I1.ToMultivector.Projection(I2.ToMultivector).IsA);
  writeln('Projection: trivector & multivector  : ',  I1.ToMultivector.Projection(M2              ).IsA);
  writeln;
  writeln('Rejection : trivector &      vector  : ',  I1.ToMultivector.Rejection(v2.ToMultivector).IsA);
  writeln('Rejection : trivector &    bivector  : ',  I1.ToMultivector.Rejection(B2.ToMultivector).IsA);
  writeln('Rejection : trivector &   trivector  : ',  I1.ToMultivector.Rejection(I2.ToMultivector).IsA);
  writeln('Rejection : trivector & multivector  : ',  I1.ToMultivector.Rejection(M2              ).IsA);
  writeln;
  writeln('Reflection: trivector &      vector  : ',  I1.ToMultivector.Reflection(v2.ToMultivector).IsA);
  writeln('Reflection: trivector &    bivector  : ',  I1.ToMultivector.Reflection(B2.ToMultivector).IsA);
  writeln('Reflection: trivector &   trivector  : ',  I1.ToMultivector.Reflection(I2.ToMultivector).IsA);
  writeln('Reflection: trivector & multivector  : ',  I1.ToMultivector.Reflection(M2              ).IsA);
  writeln;
  writeln('Rotation  : trivector &      vectors : ',  I1.ToMultivector.Rotation(v2.ToMultivector, v3.ToMultivector).IsA);
  writeln('Rotation  : trivector &    bivectors : ',  I1.ToMultivector.Rotation(B2.ToMultivector, B3.ToMultivector).IsA);
  writeln('Rotation  : trivector &   trivectors : ',  I1.ToMultivector.Rotation(I2.ToMultivector, I3.ToMultivector).IsA);
  writeln('Rotation  : trivector & multivectors : ',  I1.ToMultivector.Rotation(M2              , M3).IsA);

  writeln;
  writeln('TMULTIVECTOR');
  writeln('Dot       : multivector &      vector  : ',  M1.Dot(v2.ToMultivector).IsA);
  writeln('Dot       : multivector &    bivector  : ',  M1.Dot(B2.ToMultivector).IsA);
  writeln('Dot       : multivector &   trivector  : ',  M1.Dot(I2.ToMultivector).IsA);
  writeln('Dot       : multivector & multivector  : ',  M1.Dot(M2              ).IsA);
  writeln;
  writeln('Wedge     : multivector &      vector  : ',  M1.Wedge(v2.ToMultivector).IsA);
  writeln('Wedge     : multivector &    bivector  : ',  M1.Wedge(B2.ToMultivector).IsA);
  writeln('Wedge     : multivector &   trivector  : ',  M1.Wedge(I2.ToMultivector).IsA);
  writeln('Wedge     : multivector & multivector  : ',  M1.Wedge(M2              ).IsA);
  writeln;
  writeln('Geometric : multivector &      vector  : ', (M1*v2.ToMultivector).IsA);
  writeln('Geometric : multivector &    bivector  : ', (M1*B2.ToMultivector).IsA);
  writeln('Geometric : multivector &   trivector  : ', (M1*I2.ToMultivector).IsA);
  writeln('Geometric : multivector & multivector  : ', (M1*M2              ).IsA);
  writeln;
  writeln('Projection: multivector &      vector  : ',  M1.Projection(v2.ToMultivector).IsA);
  writeln('Projection: multivector &    bivector  : ',  M1.Projection(B2.ToMultivector).IsA);
  writeln('Projection: multivector &   trivector  : ',  M1.Projection(I2.ToMultivector).IsA);
  writeln('Projection: multivector & multivector  : ',  M1.Projection(M2              ).IsA);
  writeln;
  writeln('Rejection : multivector &      vector  : ',  M1.Rejection(v2.ToMultivector).IsA);
  writeln('Rejection : multivector &    bivector  : ',  M1.Rejection(B2.ToMultivector).IsA);
  writeln('Rejection : multivector &   trivector  : ',  M1.Rejection(I2.ToMultivector).IsA);
  writeln('Rejection : multivector & multivector  : ',  M1.Rejection(M2              ).IsA);
  writeln;
  writeln('Reflection: multivector &      vector  : ',  M1.Reflection(v2.ToMultivector).IsA);
  writeln('Reflection: multivector &    bivector  : ',  M1.Reflection(B2.ToMultivector).IsA);
  writeln('Reflection: multivector &   trivector  : ',  M1.Reflection(I2.ToMultivector).IsA);
  writeln('Reflection: multivector & multivector  : ',  M1.Reflection(M2              ).IsA);
  writeln;
  writeln('Rotation  : multivector &      vectors : ',  M1.Rotation(v2.ToMultivector, v3.ToMultivector).IsA);
  writeln('Rotation  : multivector &    bivectors : ',  M1.Rotation(B2.ToMultivector, B3.ToMultivector).IsA);
  writeln('Rotation  : multivector &   trivectors : ',  M1.Rotation(I2.ToMultivector, I3.ToMultivector).IsA);
  writeln('Rotation  : multivector & multivectors : ',  M1.Rotation(M2              , M3).IsA);
  writeln;
end.
