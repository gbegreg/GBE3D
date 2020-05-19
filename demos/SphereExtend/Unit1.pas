unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.Types3D, FMX.Ani, FMX.MaterialSources,
  FMX.Controls3D, FMX.Objects3D, FMX.Viewport3D,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  FMX.Edit, FMX.EditBox, FMX.SpinBox, GBESphereExtend;

type
  TForm6 = class(TForm)
    Viewport3D1: TViewport3D;
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    ArcDial1: TArcDial;
    LightMaterialSource2: TLightMaterialSource;
    ColorMaterialSource1: TColorMaterialSource;
    Layout1: TLayout;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    LightMaterialSource3: TLightMaterialSource;
    Dummy1: TDummy;
    LightMaterialSource4: TLightMaterialSource;
    SpinBox1: TSpinBox;
    Label1: TLabel;
    Label2: TLabel;
    SpinBox2: TSpinBox;
    GBESphereExtend1: TGBESphereExtend;
    FloatAnimation1: TFloatAnimation;
    procedure ArcDial1Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
    procedure SpinBox2Change(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

procedure TForm6.ArcDial1Change(Sender: TObject);
begin
  GBESphereExtend1.RotationAngle.X := ArcDial1.Value;
end;

procedure TForm6.CheckBox1Change(Sender: TObject);
begin
  GBESphereExtend1.ShowLines := CheckBox1.IsChecked;
end;

procedure TForm6.ComboBox1Change(Sender: TObject);
begin
  GBESphereExtend1.Depth := 9;
  GBESphereExtend1.Height := 8;
  GBESphereExtend1.Width := 9;
  GBESphereExtend1.RotationAngle.X := 0;
  GBESphereExtend1.SubdivisionsAxes := 24;
  GBESphereExtend1.SubdivisionsHeight := 16;
  SpinBox1.Value := 24;
  SpinBox2.Value := 16;

  case Combobox1.ItemIndex of
  0: begin
       GBESphereExtend1.Forme := TForme.pomme;
       GBESphereExtend1.MaterialSource := LightMaterialSource1;
     end;
  1: begin
       GBESphereExtend1.Forme := TForme.pot;
       GBESphereExtend1.MaterialSource := LightMaterialSource2;
     end;
  2: begin
       GBESphereExtend1.Forme := TForme.dome;
       GBESphereExtend1.Longueur := 0.5;
       GBESphereExtend1.RotationAngle.X := 180;
       GBESphereExtend1.MaterialSource := LightMaterialSource3;
     end;
  3: begin
       GBESphereExtend1.Forme := TForme.culbuto;
       GBESphereExtend1.Longueur := 0.5;
       GBESphereExtend1.MaterialSource := LightMaterialSource4;
     end;
  4: begin
       GBESphereExtend1.Forme := TForme.capsule;
       GBESphereExtend1.Longueur := 2;
       GBESphereExtend1.Depth := 3;
       GBESphereExtend1.Height := 8;
       GBESphereExtend1.Width := 3;
       GBESphereExtend1.MaterialSource := LightMaterialSource3;
     end;
  5: begin
       GBESphereExtend1.Forme := TForme.sphere;
       GBESphereExtend1.Depth := 9;
       GBESphereExtend1.Height := 9;
       GBESphereExtend1.Width := 9;
       GBESphereExtend1.MaterialSource := LightMaterialSource3;
     end;
  6: begin
       GBESphereExtend1.Forme := TForme.losange;
       GBESphereExtend1.MaterialSource := LightMaterialSource4;
     end;
  end;

end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  Combobox1.ItemIndex := 0;
end;

procedure TForm6.SpinBox1Change(Sender: TObject);
begin
  GBESphereExtend1.SubdivisionsAxes := round(SpinBox1.Value);
end;

procedure TForm6.SpinBox2Change(Sender: TObject);
begin
  GBESphereExtend1.SubdivisionsHeight := round(SpinBox2.Value);
end;

end.
