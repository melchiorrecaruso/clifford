unit cayleyform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    ProductBox: TComboBox;
    NegativeLabel: TLabel;
    PositiveBox: TComboBox;
    NegativeBox: TComboBox;
    PositiveLabel: TLabel;
    Table: TStringGrid;
    procedure BoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TablePrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure TableResize(Sender: TObject);
  private
    function InnerProduct    (const A, B: string): string;
    function WedgeProduct    (const A, B: string): string;
    function GeometricProduct(const A, B: string): string;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}
  
type
  TBasis = array of string;

const
  MaxDimensions = 6;

procedure GenerateCombinations(N, K: Integer; Prefix: string; Start: Integer; var Basis: TBasis);
var
  i: Integer;
begin
  if K = 0 then
  begin
    SetLength(Basis, Length(Basis) + 1);
    Basis[High(Basis)] := Prefix;
    Exit;
  end;

  for i := start to N do
  begin
    GenerateCombinations(N, K - 1, Prefix + 'e' + IntToStr(i), i + 1, Basis);
  end;
end;

function CheckInner(const A, B: string): boolean;
var
  i, Count: longint;
  Short, Long: string;
begin
  if Length(A) = Length(B) then
  begin
    result := A = B;
  end else
  begin
    if Length(A) < Length(B) then
    begin
      Short := StringReplace(A, 'e', '', [rfReplaceAll, rfIgnoreCase]);
      Long  := StringReplace(B, 'e', '', [rfReplaceAll, rfIgnoreCase]);
    end else
    begin
      Long  := StringReplace(A, 'e', '', [rfReplaceAll, rfIgnoreCase]);;
      Short := StringReplace(B, 'e', '', [rfReplaceAll, rfIgnoreCase]);;
    end;

    Count := 0;
    for i := Low(Short) to High(Short) do
      if Pos(Short[i], Long) > 0 then
        Inc(Count);

    result := Count = Length(Short);
  end;
end;

function CheckWedge(const A, B: string): boolean;
var
  i: longint;
  Short, Long: string;
begin
  Short := StringReplace(A, 'e', '', [rfReplaceAll, rfIgnoreCase]);
  Long  := StringReplace(B, 'e', '', [rfReplaceAll, rfIgnoreCase]);

  for i := Low(Short) to High(Short) do
    if Pos(Short[i], Long) > 0 then Exit(False);

  result := True;
end;

function TMainform.InnerProduct(const A, B: string): string;
begin
  if CheckInner(A, B) then
    result := GeometricProduct(A, B)
  else
    result := '0';
end;

function TMainform.WedgeProduct(const A, B: string): string;
begin
  if CheckWedge(A, B) then
    result := GeometricProduct(A, B)
  else
    result := '0';
end;

function TMainform.GeometricProduct(const A, B: string): string;
var
  i, k, dk: longint;
  T: string;
begin
  result := StringReplace(A + B, 'e', '', [rfReplaceAll, rfIgnoreCase]);
  // Ordinamento
  k := 0;
  repeat
    dk := 0;
    for i := Low(result) to High(result) -1 do
    begin
      if result[i] > result[i + 1] then
      begin
        T := result[i] + result[i + 1];
        result[i    ] := T[2];
        result[i + 1] := T[1];
        Inc(dk);
      end;
    end;
    Inc(k, dk);
  until dk = 0;
  // Calcola il segno del risultato
  dk := 0;
  for i := 1 to PositiveBox.ItemIndex do
    if Pos(i.ToString + i.ToString, result) > 0 then
    begin
      result := StringReplace(result, i.ToString + i.ToString, '', [rfReplaceAll, rfIgnoreCase]);
      Inc(dk);
    end;

  for i := (PositiveBox.ItemIndex + 1) to (PositiveBox.ItemIndex + NegativeBox.ItemIndex) do
    if Pos(i.ToString + i.ToString, result) > 0 then
    begin
      result := StringReplace(result, i.ToString + i.ToString, '', [rfReplaceAll, rfIgnoreCase]);
      Inc(dk);
      Inc(k);
    end;
  // Definisce il risultato
  if k mod 2 = 0 then
  begin
    if result = '' then
      result := '1'
    else
      result := 'e' + result;
  end else
  begin
    if result = '' then
      result := '-1'
    else
      result := '-e' + result
  end;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  BoxChange(Sender);
end;

procedure TMainForm.TablePrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
var
  S: string;
begin
  if Table.Cells[aCol, aRow] =  '0' then TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(238, 238, 238) else
  if Table.Cells[aCol, aRow] =  '1' then TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(0,   255,   0) else
  if Table.Cells[aCol, aRow] = '-1' then TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(255,   0,   0)else
  begin
    S := Table.Cells[aCol, aRow];
    S := StringReplace(S, '-', '', [rfReplaceAll, rfIgnoreCase]);

    if Pos('-', Table.Cells[aCol, aRow]) = 0 then
      case Length(S) of
        2: TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(140, 196, 255);
        3: TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(255, 187, 248);
        4: TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(252, 175,  62);
        5: TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(252, 236,  80);
      else TStringGrid(Sender).Canvas.Brush.Color := clWhite;
      end
    else
      case Length(S) of
        2: TStringGrid(Sender).Canvas.Brush.Color := RGBToColor( 81, 157, 255);
        3: TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(243, 166, 255);
        4: TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(255, 126,   0);
        5: TStringGrid(Sender).Canvas.Brush.Color := RGBToColor(255, 228,   0);
      else TStringGrid(Sender).Canvas.Brush.Color := clWhite;
      end;
  end;
end;

procedure TMainForm.TableResize(Sender: TObject);
begin
  Table.DefaultRowHeight := Trunc(Table.Height / Table.RowCount);
  Table.DefaultColWidth  := Trunc(Table.Width  / Table.ColCount);
end;

procedure TMainForm.BoxChange(Sender: TObject);
var
  Style: TTextStyle;
  N, K, i, j: Integer;
  Basis: TBasis = nil;
begin
  N := PositiveBox.ItemIndex + NegativeBox.ItemIndex;
  if N <= MaxDimensions then
  begin
    Table.ColCount := 1 shl N;
    Table.RowCount := 1 shl N;

    j := 1;
    for K := 1 to N do
    begin
      SetLength(Basis, 0);
      GenerateCombinations(N, K, '', 1, Basis);

      for i := 0 to High(Basis) do
      begin
       Table.Cells[j, 0] := 'e' + StringReplace(Basis[i], 'e', '', [rfReplaceAll, rfIgnoreCase]);
        Table.Cells[0, j] := Table.Cells[j, 0];
        Inc(j);
       end;
   end;
    Basis := nil;
    Table.Cells[0, 0] := '1';

    for i := 1 to Table.ColCount -1 do
      for j := 1 to Table.RowCount -1 do
        case ProductBox.ItemIndex of
          0: Table.Cells[i, j] := InnerProduct    (Table.Cells[0, j], Table.Cells[i, 0]);
          1: Table.Cells[i, j] := WedgeProduct    (Table.Cells[0, j], Table.Cells[i, 0]);
          2: Table.Cells[i, j] := GeometricProduct(Table.Cells[0, j], Table.Cells[i, 0]);
        end;

    Style.Alignment        := taCenter;
    style.Layout           := tlCenter;
    Table.DefaultTextStyle := Style;
    TableResize(Sender);
  end else
    MessageDlg('Warning', 'Maximun 5 dimensions!', mtWarning,  [mbOK], '');
end;

end.

