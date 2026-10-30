unit mainfrm;

{ Geometric Algebra ToolKit: main form.

  Copyright (c) 2024-2026 Melchiorre Caruso
  MIT License, see LICENSE.

  The form only shows the product tables and the generated source.
  The algebra and the code generator live in clgencore.pas, which is shared
  with the command-line generator (clgen.lpr). }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, SynHighlighterPas, SynEdit, clgencore;

type

  { TMainForm }

  TMainForm = class(TForm)
    ExportBtn: TBitBtn;
    ExportTestBtn: TBitBtn;
    IndexBaseBox: TComboBox;
    IndexBaseLabel: TLabel;
    PageControl: TPageControl;
    NegativeLabel: TLabel;
    PositiveBox: TComboBox;
    NegativeBox: TComboBox;
    PositiveLabel: TLabel;
    PrefixEdit: TEdit;
    PrefixLabel: TLabel;
    SaveDialog: TSaveDialog;
    SynPasSyn: TSynPasSyn;
    WedgeProduct: TStringGrid;
    GeometricProduct: TStringGrid;
    WedgeProductSheet: TTabSheet;
    GeometricProductSheet: TTabSheet;
    SourceCode: TSynEdit;
    DotProduct: TStringGrid;
    DotProductSheet: TTabSheet;
    SourceCodeSheet: TTabSheet;
    procedure BoxChange(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
    procedure ExportTestBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DoPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
  private
    FGenerator: TGenerator;
    function BasisPrefix: string;
    function CellText(ASign: longint; ABlade: TBlade): string;
    function CellGrade(const AText: string): longint;
    procedure FillTable(AGrid: TStringGrid; AProduct: TBladeProduct);
    procedure Rebuild;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  ZeroColor = $EEEEEE;
  GradeColors: array[0..MaxDimension] of TColor = (
    $00FF00,  // scalar (+1; -1 is drawn in red)
    $FF9D51,  // vectors
    $FFA6F3,  // bivectors
    $007EFF,  // trivectors
    $00E4FF,  // quadrivectors
    clWhite,  // pentavectors
    clWhite); // hexavectors
  NegativeOneColor = $0000FF;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PageControl.ActivePageIndex := 0;
  Rebuild;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGenerator);
end;

procedure TMainForm.BoxChange(Sender: TObject);
begin
  Rebuild;
end;

function TMainForm.BasisPrefix: string;
begin
  result := Trim(PrefixEdit.Text);
  // The prefix becomes part of identifiers such as e12 or g01
  if (result = '') or (not IsValidIdent(result + '1')) then
  begin
    result := 'e';
    PrefixEdit.Text := result;
  end;
end;

function TMainForm.CellText(ASign: longint; ABlade: TBlade): string;
begin
  if ASign = 0 then
    result := '0'
  else if ASign < 0 then
    result := '-' + FGenerator.Algebra.Name(ABlade)
  else
    result := FGenerator.Algebra.Name(ABlade);
end;

function TMainForm.CellGrade(const AText: string): longint;
var
  S: string;
begin
  // -1 for a vanishing product, otherwise the grade of the resulting blade
  S := StringReplace(AText, '-', '', []);
  if (S = '0') or (S = '') then
    result := -1
  else if S = '1' then
    result := 0
  else
    result := Length(S) - Length(FGenerator.Algebra.BasisPrefix);
end;

procedure TMainForm.FillTable(AGrid: TStringGrid; AProduct: TBladeProduct);
var
  Algebra: TAlgebra;
  Row, Col, Sign: longint;
  Blade: TBlade;
  Style: TTextStyle;
begin
  Algebra := FGenerator.Algebra;
  AGrid.BeginUpdate;
  try
    AGrid.Clear;
    AGrid.ColCount := Algebra.Count + 1;
    AGrid.RowCount := Algebra.Count + 1;
    AGrid.FixedCols := 1;
    AGrid.FixedRows := 1;

    for Col := 0 to Algebra.Count - 1 do
    begin
      AGrid.Cells[Col + 1, 0] := Algebra.Name(Algebra.Blade(Col));
      AGrid.Cells[0, Col + 1] := Algebra.Name(Algebra.Blade(Col));
    end;

    // Row operand on the left, column operand on the right
    for Row := 0 to Algebra.Count - 1 do
      for Col := 0 to Algebra.Count - 1 do
      begin
        Blade := AProduct(Algebra.Blade(Row), Algebra.Blade(Col), Sign);
        AGrid.Cells[Col + 1, Row + 1] := CellText(Sign, Blade);
      end;

    Style := AGrid.DefaultTextStyle;
    Style.Alignment := taCenter;
    Style.Layout := tlCenter;
    AGrid.DefaultTextStyle := Style;
  finally
    AGrid.EndUpdate;
  end;
end;

procedure TMainForm.Rebuild;
var
  Generator: TGenerator;
begin
  try
    Generator := TGenerator.Create(PositiveBox.ItemIndex, NegativeBox.ItemIndex, IndexBaseBox.ItemIndex);
  except
    on E: EAlgebraError do
    begin
      MessageDlg('Warning', E.Message, mtWarning, [mbOK], 0);
      Exit;
    end;
  end;

  FreeAndNil(FGenerator);
  FGenerator := Generator;
  FGenerator.Algebra.BasisPrefix := BasisPrefix;

  Screen.BeginWaitCursor;
  try
    FillTable(DotProduct, @FGenerator.Algebra.Inner);
    FillTable(WedgeProduct, @FGenerator.Algebra.Outer);
    FillTable(GeometricProduct, @FGenerator.Algebra.Geometric);
    FGenerator.Generate(SourceCode.Lines);
  finally
    Screen.EndWaitCursor;
  end;
end;

procedure TMainForm.DoPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
var
  Grid: TStringGrid;
  CellValue: string;
  Grade: longint;
begin
  if (aCol = 0) or (aRow = 0) or (FGenerator = nil) then Exit;

  Grid := TStringGrid(Sender);
  CellValue := Grid.Cells[aCol, aRow];
  Grade := CellGrade(CellValue);
  if Grade < 0 then
    Grid.Canvas.Brush.Color := ZeroColor
  else if CellValue = '-1' then
    Grid.Canvas.Brush.Color := NegativeOneColor
  else
    Grid.Canvas.Brush.Color := GradeColors[Grade];
end;

procedure TMainForm.ExportBtnClick(Sender: TObject);
var
  NewUnitName: string;
begin
  if FGenerator = nil then Exit;

  SaveDialog.Title := 'Export unit';
  SaveDialog.Filter := 'Pascal unit (*.pas)|*.pas|All files (*)|*';
  SaveDialog.DefaultExt := 'pas';
  SaveDialog.FileName := FGenerator.OutputUnitName + '.pas';
  if not SaveDialog.Execute then Exit;

  // Free Pascal requires the unit name to match the file name
  NewUnitName := ChangeFileExt(ExtractFileName(SaveDialog.FileName), '');
  if not IsValidIdent(NewUnitName) then
  begin
    MessageDlg('Warning', Format('"%s" is not a valid unit name.', [NewUnitName]), mtWarning, [mbOK], 0);
    Exit;
  end;

  if not SameText(NewUnitName, FGenerator.OutputUnitName) then
  begin
    FGenerator.OutputUnitName := NewUnitName;
    FGenerator.Generate(SourceCode.Lines);
  end;
  SourceCode.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TMainForm.ExportTestBtnClick(Sender: TObject);
var
  Lines: TStringList;
begin
  if FGenerator = nil then Exit;

  SaveDialog.Title := Format('Export self-check program for unit %s', [FGenerator.OutputUnitName]);
  SaveDialog.Filter := 'Pascal program (*.lpr;*.pas)|*.lpr;*.pas|All files (*)|*';
  SaveDialog.DefaultExt := 'lpr';
  SaveDialog.FileName := FGenerator.OutputUnitName + 'check.lpr';
  if not SaveDialog.Execute then Exit;

  Lines := TStringList.Create;
  try
    FGenerator.GenerateTest(Lines);
    Lines.SaveToFile(SaveDialog.FileName);
  finally
    Lines.Free;
  end;
end;

end.
