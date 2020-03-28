unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.Types3D, FMX.Ani, FMX.Controls3D,
  FMX.MaterialSources, FMX.Objects3D, FMX.Viewport3D, GBEViewport3D,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.Layers3D;

type
  TForm1 = class(TForm)
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    Camera1: TCamera;
    GBEViewport3D1: TGBEViewport3D;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Camera2: TCamera;
    Layout4: TLayout;
    Image1: TImage;
    Image2: TImage;
    Image3D1: TImage3D;
    Camera3: TCamera;
    FloatAnimation2: TFloatAnimation;
    Plane1: TPlane;
    LightMaterialSource2: TLightMaterialSource;
    Dummy1: TDummy;
    Cube1: TCube;
    Cube2: TCube;
    LightMaterialSource3: TLightMaterialSource;
    Cube3: TCube;
    Cube4: TCube;
    Cube5: TCube;
    Cube6: TCube;
    Cube7: TCube;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Rectangle1: TRectangle;
    FloatAnimation1: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure FloatAnimation1Process(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FloatAnimation1Process(Sender: TObject);
begin
  image1.Bitmap := GBEViewport3D1.getBitmapFromView(camera1);
  image2.Bitmap := GBEViewport3D1.getBitmapFromView(camera2);
  Image3D1.Bitmap := GBEViewport3D1.getBitmapFromView(camera3);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  camera1.Position.Z := 0;
  camera2.Position.Z := 0;
  camera3.Position.Z := -10;
  GBEViewport3D1.DoAddView(camera1);    // On crée une 1ere vue avec la caméra 1
  GBEViewport3D1.DoAddView(camera2);    // On crée une 2eme vue avec la caméra 1
  GBEViewport3D1.DoAddView(camera3);    // On crée une 2eme vue avec la caméra 1
  GBEViewport3D1.BackgroundColor := GBEViewport3D1.color;  // La couleur du fond pour les images Bitmap
end;

end.
