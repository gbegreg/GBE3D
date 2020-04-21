unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.MaterialSources, FMX.Controls3D, FMX.Objects3D,
  GBEHeightmap, FMX.Viewport3D, FMX.Ani, FMX.Types3D,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Objects,
  FMX.Edit, FMX.EditBox, FMX.SpinBox;

type
  TForm6 = class(TForm)
    Viewport3D1: TViewport3D;
    GBEHeightmap1: TGBEHeightmap;
    ColorMaterialSource1: TColorMaterialSource;
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    Switch1: TSwitch;
    Switch2: TSwitch;
    LightMaterialSource3: TLightMaterialSource;
    Layout1: TLayout;
    Label1: TLabel;
    Label2: TLabel;
    Rectangle1: TRectangle;
    Image1: TImage;
    Label3: TLabel;
    SpinBox1: TSpinBox;
    Cylinder1: TCylinder;
    FloatAnimation1: TFloatAnimation;
    FloatAnimation2: TFloatAnimation;
    Cylinder2: TCylinder;
    FloatAnimation3: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure Switch1Switch(Sender: TObject);
    procedure Switch2Switch(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
    procedure FloatAnimation1Process(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

procedure TForm6.FloatAnimation1Process(Sender: TObject);
begin
  Cylinder1.Position.Y := GBEHeightmap1.GetHeight(Cylinder1.Position.Point);
end;

procedure TForm6.FormCreate(Sender: TObject);
var
  stream : TMemoryStream;
begin
  stream := TMemoryStream.Create;
  image1.Bitmap.SaveToStream(stream);
  GBEHeightmap1.loadHeightmapFromStream(stream);
  stream.Free;
end;

procedure TForm6.SpinBox1Change(Sender: TObject);
begin
  GBEHeightmap1.Flou := trunc(SpinBox1.Value);
end;

procedure TForm6.Switch1Switch(Sender: TObject);
begin
  GBEHeightmap1.ShowLines := Switch1.IsChecked;
end;

procedure TForm6.Switch2Switch(Sender: TObject);
begin
  if Switch2.IsChecked then GBEHeightmap1.MaterialSource := LightMaterialSource3
  else GBEHeightmap1.MaterialSource := LightMaterialSource1;
  GBEHeightmap1.UseRamp := Switch2.IsChecked;
end;

end.
