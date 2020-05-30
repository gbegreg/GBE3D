unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.Ani, FMX.MaterialSources, FMX.Controls3D,
  FMX.Objects3D, FMX.Viewport3D, GBEPlaneExtend, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, FMX.Edit,
  FMX.EditBox, FMX.SpinBox, FMX.Types3D;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    FloatAnimation1: TFloatAnimation;
    Layout1: TLayout;
    Rectangle1: TRectangle;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    TrackBar2: TTrackBar;
    Label3: TLabel;
    TrackBar3: TTrackBar;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    SpinBox1: TSpinBox;
    Label5: TLabel;
    SpinBox2: TSpinBox;
    Label6: TLabel;
    SpinBox3: TSpinBox;
    ColorMaterialSource1: TColorMaterialSource;
    Switch1: TSwitch;
    Label7: TLabel;
    Label8: TLabel;
    TrackBar4: TTrackBar;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    GBEPlaneExtend1: TGBEPlaneExtend;
    procedure FloatAnimation1Process(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Tracking(Sender: TObject);
    procedure TrackBar2Tracking(Sender: TObject);
    procedure TrackBar3Tracking(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
    procedure Switch1Switch(Sender: TObject);
    procedure TrackBar4Tracking(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    center : TPoint3D;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FloatAnimation1Process(Sender: TObject);
begin
  Viewport3D1.Repaint;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GBEPlaneExtend1.Origine := Point3D(-13,-13,0);
  TrackBar1.Value := GBEPlaneExtend1.Amplitude;
  TrackBar2.Value := GBEPlaneExtend1.Longueur;
  TrackBar3.Value := GBEPlaneExtend1.Vitesse;
  FloatAnimation1.Start;
end;

procedure TForm1.SpinBox1Change(Sender: TObject);
begin
  GBEPlaneExtend1.Origine := Point3D(SpinBox1.Value, SpinBox2.Value, SpinBox3.Value);
end;

procedure TForm1.Switch1Switch(Sender: TObject);
begin
  GBEPlaneExtend1.ShowLines := Switch1.IsChecked;
end;

procedure TForm1.TrackBar1Tracking(Sender: TObject);
begin
  GBEPlaneExtend1.Amplitude := TrackBar1.Value;
end;

procedure TForm1.TrackBar2Tracking(Sender: TObject);
begin
  GBEPlaneExtend1.Longueur := TrackBar2.Value;
end;

procedure TForm1.TrackBar3Tracking(Sender: TObject);
begin
  GBEPlaneExtend1.Vitesse := TrackBar3.Value;
end;

procedure TForm1.TrackBar4Tracking(Sender: TObject);
begin
  GBEPlaneExtend1.Opacity := TrackBar4.Value;
end;

end.
