program cl2test;

{$mode objfpc}{$h+}

uses
  Cl2, Math, SysUtils;

var
  k : longint;
  a : double;
  v, v1, v2, v3 : TVector;
  B, B1, B2, B3 : TBivector;
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

  Writeln('CL2 TEST: begin');
  for k := 0 to 1000000 do
  begin
    a := Rnd;
    v := Rnd*e1  + Rnd*e2;
    B := Rnd*e12;
    M := a + v + B;

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
    if (B.Projection(u12 ) <> M.Projection(u12 .ToMultivector)) then Writeln('TEST-218: NOT PASSED');

    if (B.Rejection (u1  ) <> M.Rejection (u1  .ToMultivector)) then Writeln('TEST-222: NOT PASSED');
    if (B.Rejection (u2  ) <> M.Rejection (u2  .ToMultivector)) then Writeln('TEST-223: NOT PASSED');
    if (B.Rejection (u12 ) <> M.Rejection (u12 .ToMultivector)) then Writeln('TEST-225: NOT PASSED');

    if (B.Reflection(u1  ) <> M.Reflection(u1  .ToMultivector)) then Writeln('TEST-229: NOT PASSED');
    if (B.Reflection(u2  ) <> M.Reflection(u2  .ToMultivector)) then Writeln('TEST-230: NOT PASSED');
    if (B.Reflection(u12 ) <> M.Reflection(u12 .ToMultivector)) then Writeln('TEST-232: NOT PASSED');

    if (B.Rotation(u1 , u2 ) <> M.Rotation(u1 .ToMultivector, u2 .ToMultivector)) then Writeln('TEST-236: NOT PASSED');

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
    if (v.Projection(u12 ) <> M.Projection(u12 .ToMultivector)) then Writeln('TEST-317: NOT PASSED');

    if (v.Rejection (u1  ) <> M.Rejection (u1  .ToMultivector)) then Writeln('TEST-321: NOT PASSED');
    if (v.Rejection (u2  ) <> M.Rejection (u2  .ToMultivector)) then Writeln('TEST-322: NOT PASSED');
    if (v.Rejection (u12 ) <> M.Rejection (u12 .ToMultivector)) then Writeln('TEST-324: NOT PASSED');

    if (v.Reflection(u1  ) <> M.Reflection(u1  .ToMultivector)) then Writeln('TEST-328: NOT PASSED');
    if (v.Reflection(u2  ) <> M.Reflection(u2  .ToMultivector)) then Writeln('TEST-329: NOT PASSED');
    if (v.Reflection(u12 ) <> M.Reflection(u12 .ToMultivector)) then Writeln('TEST-331: NOT PASSED');

    if (v.Rotation(u1 , u2 ) <> M.Rotation(u1 .ToMultivector, u2 .ToMultivector)) then Writeln('TEST-335: NOT PASSED');
  end;

  Writeln('CL2 TEST: end.');

  v1 := Rnd*e1 + Rnd*e2;
  v2 := Rnd*e1 + Rnd*e2;
  v3 := Rnd*e1 + Rnd*e2;

  B1 := Rnd*e12;
  B2 := Rnd*e12;
  B3 := Rnd*e12;

  M1 := Rnd + Rnd*e1 + Rnd*e2 + Rnd*e12;
  M2 := Rnd + Rnd*e1 + Rnd*e2 + Rnd*e12;
  M3 := Rnd + Rnd*e1 + Rnd*e2 + Rnd*e12;

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
  writeln('Dot       : vector & multivector  : ',  v1.ToMultivector.Dot(M2              ).IsA);
  writeln;
  writeln('Wedge     : vector &      vector  : ',  v1.ToMultivector.Wedge(v2.ToMultivector).IsA);
  writeln('Wedge     : vector &    bivector  : ',  v1.ToMultivector.Wedge(B2.ToMultivector).IsA);
  writeln('Wedge     : vector & multivector  : ',  v1.ToMultivector.Wedge(M2              ).IsA);
  writeln;
  writeln('Geometric : vector &      vector  : ', (v1.ToMultivector*v2.ToMultivector).IsA);
  writeln('Geometric : vector &    bivector  : ', (v1.ToMultivector*B2.ToMultivector).IsA);
  writeln('Geometric : vector & multivector  : ', (v1.ToMultivector*M2              ).IsA);
  writeln;
  writeln('Projection: vector &      vector  : ',  v1.ToMultivector.Projection(v2.ToMultivector).IsA);
  writeln('Projection: vector &    bivector  : ',  v1.ToMultivector.Projection(B2.ToMultivector).IsA);
  writeln('Projection: vector & multivector  : ',  v1.ToMultivector.Projection(M2              ).IsA);
  writeln;
  writeln('Rejection : vector &      vector  : ',  v1.ToMultivector.Rejection(v2.ToMultivector).IsA);
  writeln('Rejection : vector &    bivector  : ',  v1.ToMultivector.Rejection(B2.ToMultivector).IsA);
  writeln('Rejection : vector & multivector  : ',  v1.ToMultivector.Rejection(M2              ).IsA);
  writeln;
  writeln('Reflection: vector &      vector  : ',  v1.ToMultivector.Reflection(v2.ToMultivector).IsA);
  writeln('Reflection: vector &    bivector  : ',  v1.ToMultivector.Reflection(B2.ToMultivector).IsA);
  writeln('Reflection: vector & multivector  : ',  v1.ToMultivector.Reflection(M2              ).IsA);
  writeln;
  writeln('Rotation  : vector &      vectors : ',  v1.ToMultivector.Rotation(v2.ToMultivector, v3.ToMultivector).IsA);
  writeln('Rotation  : vector &    bivectors : ',  v1.ToMultivector.Rotation(B2.ToMultivector, B3.ToMultivector).IsA);
  writeln('Rotation  : vector & multivectors : ',  v1.ToMultivector.Rotation(M2              , M3).IsA);

  writeln;
  writeln('TBIVECTOR');
  writeln('Dot       : bivector &      vector  : ',  B1.ToMultivector.Dot(v2.ToMultivector).IsA);
  writeln('Dot       : bivector &    bivector  : ',  B1.ToMultivector.Dot(B2.ToMultivector).IsA);
  writeln('Dot       : bivector & multivector  : ',  B1.ToMultivector.Dot(M2              ).IsA);
  writeln;
  writeln('Wedge     : bivector &      vector  : ',  B1.ToMultivector.Wedge(v2.ToMultivector).IsA);
  writeln('Wedge     : bivector &    bivector  : ',  B1.ToMultivector.Wedge(B2.ToMultivector).IsA);
  writeln('Wedge     : bivector & multivector  : ',  B1.ToMultivector.Wedge(M2              ).IsA);
  writeln;
  writeln('Geometric : bivector &      vector  : ', (B1.ToMultivector*v2.ToMultivector).IsA);
  writeln('Geometric : bivector &    bivector  : ', (B1.ToMultivector*B2.ToMultivector).IsA);
  writeln('Geometric : bivector & multivector  : ', (B1.ToMultivector*M2              ).IsA);
  writeln;
  writeln('Projection: bivector &      vector  : ',  B1.ToMultivector.Projection(v2.ToMultivector).IsA);
  writeln('Projection: bivector &    bivector  : ',  B1.ToMultivector.Projection(B2.ToMultivector).IsA);
  writeln('Projection: bivector & multivector  : ',  B1.ToMultivector.Projection(M2              ).IsA);
  writeln;
  writeln('Rejection : bivector &      vector  : ',  B1.ToMultivector.Rejection(v2.ToMultivector).IsA);
  writeln('Rejection : bivector &    bivector  : ',  B1.ToMultivector.Rejection(B2.ToMultivector).IsA);
  writeln('Rejection : bivector & multivector  : ',  B1.ToMultivector.Rejection(M2              ).IsA);
  writeln;
  writeln('Reflection: bivector &      vector  : ',  B1.ToMultivector.Reflection(v2.ToMultivector).IsA);
  writeln('Reflection: bivector &    bivector  : ',  B1.ToMultivector.Reflection(B2.ToMultivector).IsA);
  writeln('Reflection: bivector & multivector  : ',  B1.ToMultivector.Reflection(M2              ).IsA);
  writeln;
  writeln('Rotation  : bivector &      vectors : ',  B1.ToMultivector.Rotation(v2.ToMultivector, v3.ToMultivector).IsA);
  writeln('Rotation  : bivector &    bivectors : ',  B1.ToMultivector.Rotation(B2.ToMultivector, B3.ToMultivector).IsA);
  writeln('Rotation  : bivector & multivectors : ',  B1.ToMultivector.Rotation(M2              , M3).IsA);

  writeln;
  writeln('TMULTIVECTOR');
  writeln('Dot       : multivector &      vector  : ',  M1.Dot(v2.ToMultivector).IsA);
  writeln('Dot       : multivector &    bivector  : ',  M1.Dot(B2.ToMultivector).IsA);
  writeln('Dot       : multivector & multivector  : ',  M1.Dot(M2              ).IsA);
  writeln;
  writeln('Wedge     : multivector &      vector  : ',  M1.Wedge(v2.ToMultivector).IsA);
  writeln('Wedge     : multivector &    bivector  : ',  M1.Wedge(B2.ToMultivector).IsA);
  writeln('Wedge     : multivector & multivector  : ',  M1.Wedge(M2              ).IsA);
  writeln;
  writeln('Geometric : multivector &      vector  : ', (M1*v2.ToMultivector).IsA);
  writeln('Geometric : multivector &    bivector  : ', (M1*B2.ToMultivector).IsA);
  writeln('Geometric : multivector & multivector  : ', (M1*M2              ).IsA);
  writeln;
  writeln('Projection: multivector &      vector  : ',  M1.Projection(v2.ToMultivector).IsA);
  writeln('Projection: multivector &    bivector  : ',  M1.Projection(B2.ToMultivector).IsA);
  writeln('Projection: multivector & multivector  : ',  M1.Projection(M2              ).IsA);
  writeln;
  writeln('Rejection : multivector &      vector  : ',  M1.Rejection(v2.ToMultivector).IsA);
  writeln('Rejection : multivector &    bivector  : ',  M1.Rejection(B2.ToMultivector).IsA);
  writeln('Rejection : multivector & multivector  : ',  M1.Rejection(M2              ).IsA);
  writeln;
  writeln('Reflection: multivector &      vector  : ',  M1.Reflection(v2.ToMultivector).IsA);
  writeln('Reflection: multivector &    bivector  : ',  M1.Reflection(B2.ToMultivector).IsA);
  writeln('Reflection: multivector & multivector  : ',  M1.Reflection(M2              ).IsA);
  writeln;
  writeln('Rotation  : multivector &      vectors : ',  M1.Rotation(v2.ToMultivector, v3.ToMultivector).IsA);
  writeln('Rotation  : multivector &    bivectors : ',  M1.Rotation(B2.ToMultivector, B3.ToMultivector).IsA);
  writeln('Rotation  : multivector & multivectors : ',  M1.Rotation(M2              , M3).IsA);
  writeln;
end.
