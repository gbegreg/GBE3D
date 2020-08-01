unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.Controls3D, FMX.Objects3D, GBECubeExtend,
  FMX.Viewport3D, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.Types3D, FMX.MaterialSources, FMX.Ani, FMX.Objects;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    Layout1: TLayout;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    LightMaterialSource2: TLightMaterialSource;
    LightMaterialSource3: TLightMaterialSource;
    LightMaterialSource4: TLightMaterialSource;
    LightMaterialSource5: TLightMaterialSource;
    LightMaterialSource6: TLightMaterialSource;
    Light2: TLight;
    Light3: TLight;
    GBECubeExtend1: TGBECubeExtend;
    FloatAnimation1: TFloatAnimation;
    Rectangle1: TRectangle;
    procedure FloatAnimation1Process(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  GBECubeExtend1.FaceFrontVisible := CheckBox1.IsChecked;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  GBECubeExtend1.FaceRightVisible := CheckBox2.IsChecked;
end;

procedure TForm1.CheckBox3Change(Sender: TObject);
begin
  GBECubeExtend1.FaceBackVisible := CheckBox3.IsChecked;
end;

procedure TForm1.CheckBox4Change(Sender: TObject);
begin
  GBECubeExtend1.FaceLeftVisible := CheckBox4.IsChecked;
end;

procedure TForm1.CheckBox5Change(Sender: TObject);
begin
  GBECubeExtend1.FaceTopVisible := CheckBox5.IsChecked;
end;

procedure TForm1.CheckBox6Change(Sender: TObject);
begin
  GBECubeExtend1.FaceBottomVisible := CheckBox6.IsChecked;
end;

procedure TForm1.FloatAnimation1Process(Sender: TObject);
begin
  GBECubeExtend1.RotationAngle.X := GBECubeExtend1.RotationAngle.X + 1;
  GBECubeExtend1.RotationAngle.Z := GBECubeExtend1.RotationAngle.Z + 2;
end;

end.
