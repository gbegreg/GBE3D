unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.MaterialSources, FMX.Controls3D, FMX.Objects3D,
  GBEClouds, FMX.Viewport3D, FMX.Ani, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Edit, FMX.EditBox, FMX.SpinBox, FMX.Layouts;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    GBEClouds1: TGBEClouds;
    TextureMaterialSource1: TTextureMaterialSource;
    TextureMaterialSource2: TTextureMaterialSource;
    TextureMaterialSource3: TTextureMaterialSource;
    FloatAnimation1: TFloatAnimation;
    Layout1: TLayout;
    ArcDial1: TArcDial;
    SpinBox1: TSpinBox;
    SpinBox2: TSpinBox;
    Camera1: TCamera;
    procedure FormCreate(Sender: TObject);
    procedure FloatAnimation1Process(Sender: TObject);
    procedure ArcDial1Change(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
    procedure SpinBox2Change(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.ArcDial1Change(Sender: TObject);
begin
  GBEClouds1.RotationAngle.Y := ArcDial1.Value;
end;

procedure TForm1.FloatAnimation1Process(Sender: TObject);
begin
  GBEClouds1.moveClouds;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  GBEClouds1.addTextureCloud(TextureMaterialSource1);
  GBEClouds1.addTextureCloud(TextureMaterialSource2);
  GBEClouds1.addTextureCloud(TextureMaterialSource3);

  GBEClouds1.NbClouds := 15;
  GBEClouds1.WindSpeed := 0.1;
  GBEClouds1.Limits := 100;

  GBEClouds1.ActiveWind := true;

  FloatAnimation1.Start;
end;

procedure TForm1.SpinBox1Change(Sender: TObject);
begin
  if SpinBox1.Value > 0 then
    GBEClouds1.NbClouds := Round(SpinBox1.Value);
end;

procedure TForm1.SpinBox2Change(Sender: TObject);
begin
  GBEClouds1.WindSpeed := 0.1 * SpinBox2.Value;
end;

end.
