{
  Ecrit par Grégory Bersegeay
  Le TGBEViewport3D hétite de TViewport3D. Il lui ajoute la possibilité de récupérer sous forme de TBitmap les images
  issues de chacune des caméras placées dans la scène 3D. Cela permet ensuite d'afficher ces images dans d'autres
  zones de l'interface sans avoir à dupliquer les TViewport3D ni les scènes 3D à calculer.
}
unit GBEViewport3D;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Viewport3D, FMX.Graphics, FMX.Types3D,
  FMX.Controls3D, System.generics.collections, System.Types, System.Math.Vectors, System.UITypes, FMX.Layouts;

type
  THelpOpenControl3D = class(TControl3D);

  TGBEViewport3D = class(TViewport3D)
  private
    { Déclarations privées }
    FDrawing : boolean;
    FMyBitmap : TBitmap;
    FMyTexture : TTexture;
    fMyContext: TContext3D;
    fBackgroundColor : cardinal;
  protected
    { Déclarations protégées }
    FMyRenderingList: TList<TControl3D>; // Liste des objets 3D a afficher
    FMyViewList: TDictionary<TCamera, TBitmap>; // Liste des vues (une vue par caméra)
    procedure RebuildRenderingList;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function getBitmapFromView(camera: TCamera):TBitmap;
    procedure DoAddView(camera: TCamera);
    procedure DoRemoveView(camera: TCamera);
    property MyContext: TContext3D read fMyContext write fMyContext;
    property BackgroundColor : cardinal read fBackgroundColor write fBackgroundColor;
  published
    { Déclarations publiées }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBEViewport3D]);
end;

{ TGBEViewport3D1 }
constructor TGBEViewport3D.Create(AOwner: TComponent);
begin
  inherited;
  FMyViewList := TDictionary<TCamera,TBitmap>.create;
  BackgroundColor := TAlphaColorRec.Null;
end;

destructor TGBEViewport3D.Destroy;
begin
  freeAndNil(fMyContext);
  freeAndNil(fMyBitmap);
  freeAndNil(fMyTexture);
  freeAndNil(FMyRenderingList);
  freeAndNil(FMyViewList);
  inherited;
end;

procedure TGBEViewport3D.DoAddView(camera: TCamera);
begin
  FMyViewList.Add(camera, TBitmap.create);
end;

procedure TGBEViewport3D.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject is TControl3D then RebuildRenderingList;
end;

procedure TGBEViewport3D.DoRemoveView(camera: TCamera);
begin
  FMyViewList.Remove(camera);
end;

function TGBEViewport3D.getBitmapFromView(camera: TCamera): TBitmap;
begin
  result := TBitmap.Create;
  if not(FDrawing) then FMyViewList.TryGetValue(camera, result);
end;

procedure TGBEViewport3D.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject is TControl3D then
  begin
    RebuildRenderingList;
    Repaint;
  end;
end;

procedure TGBEViewport3D.Paint;
var
  i : integer;
  Control : TControl3D;
  New : TMatrix3D;
  theCamera : TCamera;
begin
  inherited;
  if FDrawing then exit;

  FDrawing := true;

  try
    for theCamera in FMyViewList.Keys do
    begin

      if Assigned(fMyContext) then
      begin
        if fMyContext.BeginScene then
        begin
          try
            New := theCamera.CameraMatrix;

            fMyContext.Clear([TClearTarget.Color, TClearTarget.Depth],
                             BackgroundColor, 1.0, 0);
            fMyContext.SetCameraMatrix(theCamera.CameraMatrix);

            if Assigned(FMyRenderingList) and (FMyRenderingList.Count > 0) then
            begin
              for I := 0 to FMyRenderingList.Count -1 do
              begin
                if FMyRenderingList[i].Visible or
                  (not FMyRenderingList[i].Visible and (csDesigning in ComponentState)
                   and not FMyRenderingList[i].Locked) then
                  begin
                    Control := TControl3D(FMyRenderingList[i]);
                    Control.Context.SetCameraMatrix(New);
                    if (csDesigning in ComponentState) and (not Control.Visible) then continue;
                    THelpOpenControl3D(Control).RenderInternal;
                  end;
              end;
            end;

          finally
            fMyContext.EndScene;
          end;
        end;
      end;

      fMyContext.CopyToBitmap(FMyBitmap, Rect(0, 0, FMyBitmap.Width, FMyBitmap.Height));
      FMyViewList.Items[theCamera].Width := FMyBitmap.Width;
      FMyViewList.Items[theCamera].Height := FMyBitmap.Height;
      FMyViewList.Items[theCamera].CopyFromBitmap(FMyBitmap);
    end;

  finally
    FDrawing := false;
  end;
end;

procedure TGBEViewport3D.RebuildRenderingList;
var i: integer;
begin
  if Assigned(children) and (FUpdating = 0) then
  begin
    if not assigned(FMyRenderingList) then FMyRenderingList := TList<TControl3D>.create;
    FMyRenderingList.Clear;
    for i := 0 to Children.Count-1 do
    begin
      if children[i] is TControl3D then FMyRenderingList.Add((children[i] as TControl3d));
    end;
  end;
end;

procedure TGBEViewport3D.Resize;
begin
  inherited;
  FreeAndNil(FMyBitmap);
  FreeAndNil(FMyTexture);
  FreeAndNil(fMyContext);

  FMyTexture := TTexture.Create;
  FMyTexture.Style := [TTextureStyle.RenderTarget];
  FMyTexture.SetSize(Round(Width), Round(Height));
  fMyContext := TContextManager.CreateFromTexture(FMyTexture, TMultisample.FourSamples, True);
  FMyBitmap := TBitmap.Create(fMyContext.Width, fMyContext.Height);
end;

end.
