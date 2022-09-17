unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, System.Generics.Collections,
  FMX.Edit, uGBEPathFinder;

type
  TMainForm = class(TForm)
    layIHM: TLayout;
    btnTrouverChemin: TButton;
    layGrille: TLayout;
    rectangleModele: TRectangle;
    lblTotal: TLabel;
    tDistanceD: TText;
    tDistanceA: TText;
    lblInfos: TLabel;
    cbDiagonale: TCheckBox;
    layOptions: TLayout;
    ScrollBox: TScrollBox;
    eNbColonne: TEdit;
    SpinEditButton1: TSpinEditButton;
    lblNbColonne: TLabel;
    lblLigne: TLabel;
    eNbLigne: TEdit;
    SpinEditButton2: TSpinEditButton;
    btnCreerGrille: TButton;
    gbOptions: TGroupBox;
    cbPremiereEtape: TCheckBox;
    cbModeCout: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure rectangleModeleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTrouverCheminClick(Sender: TObject);
    procedure SpinEditButton1DownClick(Sender: TObject);
    procedure SpinEditButton1UpClick(Sender: TObject);
    procedure SpinEditButton2UpClick(Sender: TObject);
    procedure SpinEditButton2DownClick(Sender: TObject);
    procedure btnCreerGrilleClick(Sender: TObject);
  private
    procedure creerGrille;
    procedure mettreAJourCase(position: TPoint; couleur: TAlphaColor; dDistance, aDistance : integer);
    procedure celluleClick(Sender: TObject);
    procedure initialiserGrille;
    function getPoint(indice: integer): TPoint;
    function getIndice(point: TPoint): integer;
    procedure dessinerResultat;
    { D�clarations priv�es }
  public
    { D�clarations publiques }
    noeudDepart, noeudArrivee : TGBENoeud;
    lGrille, hGrille : integer;
    PathFinder : TGBEPathFinder;
  end;

var
  fMain: TMainForm;

implementation
{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  randomize;
  PathFinder := TGBEPathFinder.Create;
  PathFinder.CoutDeplacementCote := 10;
  PathFinder.CoutDeplacementDiagonal := 15;
  eNbColonne.Text := '12';
  eNbLigne.Text := '10';

  creerGrille;
  initialiserGrille;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(PathFinder);
end;

procedure TMainForm.creerGrille;
var x,y, indice : integer;
begin
  indice := 0;
  hGrille := strtointdef(eNbLigne.Text, 8);
  lGrille := strtointdef(eNbColonne.Text, 8);
  layGrille.Width := rectangleModele.Width * lGrille;
  layGrille.Height := rectangleModele.Height* hGrille;

  // La grille est constitu�e de TRectangle clon�s � partir du rectangleModele
  for y := 0 to hGrille -1 do begin
    for x := 0 to lGrille -1 do begin
      if indice >= 1 then begin
        with rectangleModele.Clone(nil) as TRectangle do begin
          parent := layGrille;
          name := 'rectangle'+indice.ToString;
          position.x := x * width;
          position.y := y * height;
          tag := indice;  // le tag va contenir l'indice de la case, ce qui permettra d'avoir ensuite les coordonn�es X et Y de la case
          onClick := celluleClick;
        end;
      end;
      inc(indice);
    end;
  end;
end;

procedure TMainForm.initialiserGrille;
var iRectangle: TFMxObject;
begin
  // On dessine toutes les cases de la grille en blanc et sans texte
  for iRectangle in layGrille.Children do begin
    (iRectangle as TRectangle).Fill.Gradient.Color := TAlphaColorRec.White;
    ((iRectangle as TRectangle).Children[0] as TLabel).Text := '';
    ((iRectangle as TRectangle).Children[1] as TText).Text := '';
    ((iRectangle as TRectangle).Children[2] as TText).Text := '';
  end;

  // On d�termine al�atoirement les cases de d�part et d'arriv�e
  NoeudDepart.position.X := random(lGrille);
  noeudDepart.position.Y := random(hGrille);
  noeudDepart.coutDeplacement := 0;

  noeudArrivee.position.X := random(lGrille);
  noeudArrivee.position.Y := random(hGrille);
  // Si la case d'arriv�e est adjacente � la case de d�part, on recherche une autre position pour la case d'arriv�e
  while (abs(noeudArrivee.position.X - noeudDepart.position.X) <=1) and
        (abs(noeudArrivee.position.Y - noeudDepart.position.Y) <=1) do begin
          noeudArrivee.position.X := random(lGrille);
          noeudArrivee.position.Y := random(hGrille);
        end;

  // On intialise les co�ts des cases de d�part et d'arriv�e
  noeudArrivee.coutDeplacement := PathFinder.calculerCoutArrivee(noeudDepart.position);
  noeudArrivee.heuristique := 0;
  noeudDepart.heuristique := noeudArrivee.coutDeplacement;

  // On dessine les cases de d�part et d'arriv�e
  mettreAJourCase(noeudDepart.position, TAlphaColorRec.Cyan, 0, noeudDepart.heuristique);
  mettreAJourCase(noeudArrivee.position, TAlphaColorRec.Cyan, noeudArrivee.coutDeplacement, 0);

  lblInfos.Text := 'Click on white squares to generate obstacles';
end;

// Dessine les diff�rentes cases de la grille
procedure TMainForm.mettreAJourCase(position : TPoint; couleur: TAlphaColor; dDistance, aDistance : integer);
var
  indice, valeur : integer;
  unNoeud: TGBENoeud;
begin
  indice := getIndice(position);

  if (position = noeudDepart.position) or (position = noeudArrivee.position) then begin
    if (position = noeudDepart.position) then ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel).Text := 'D'
    else ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel).Text := 'A';
    ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel).TextSettings.FontColor := TAlphaColorRec.Red;
    ((layGrille.Children[indice] as TRectangle).Children[1] as TText).Text := aDistance.ToString;
    ((layGrille.Children[indice] as TRectangle).Children[2] as TText).Text := dDistance.ToString;
    (layGrille.Children[indice] as TRectangle).Fill.Gradient.Color := TAlphaColorRec.Cyan;
  end else begin
    unNoeud.position := position;
    ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel).TextSettings.FontColor := TAlphaColorRec.Black;
    if PathFinder.listeNoeudsObstacles.TryGetValue(unNoeud.position, unNoeud) then begin
      ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel).Text := '';
      (layGrille.Children[indice] as TRectangle).Fill.Gradient.Color := couleur;
    end else begin
      valeur := dDistance + aDistance;
      if valeur = 0 then ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel).Text := ''
      else ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel).Text := valeur.ToString;
      if aDistance = 0 then ((layGrille.Children[indice] as TRectangle).Children[1] as TText).Text := ''
      else ((layGrille.Children[indice] as TRectangle).Children[1] as TText).Text := aDistance.ToString;
      if dDistance = 0 then ((layGrille.Children[indice] as TRectangle).Children[2] as TText).Text := ''
      else ((layGrille.Children[indice] as TRectangle).Children[2] as TText).Text := dDistance.ToString;
      (layGrille.Children[indice] as TRectangle).Fill.Gradient.Color := couleur;
    end;
  end;
end;

procedure TMainForm.rectangleModeleClick(Sender: TObject);
begin
  celluleClick(sender);
end;

procedure TMainForm.SpinEditButton1DownClick(Sender: TObject);
begin
  if strToIntDef(eNbColonne.Text, 0) > 0 then
    eNbColonne.Text := (strToIntDef(eNbColonne.Text, 0) - 1).toString;
end;

procedure TMainForm.SpinEditButton1UpClick(Sender: TObject);
begin
  if strToIntDef(eNbColonne.Text, 0) > 0 then
    eNbColonne.Text := (strToIntDef(eNbColonne.Text, 0) + 1).toString;
end;

procedure TMainForm.SpinEditButton2DownClick(Sender: TObject);
begin
  if strToIntDef(eNbLigne.Text, 0) > 0 then
    eNbLigne.Text := (strToIntDef(eNbLigne.Text, 0) - 1).toString;
end;

procedure TMainForm.SpinEditButton2UpClick(Sender: TObject);
begin
  if strToIntDef(eNbLigne.Text, 0) > 0 then
    eNbLigne.Text := (strToIntDef(eNbLigne.Text, 0) + 1).toString;
end;

procedure TMainForm.btnTrouverCheminClick(Sender: TObject);
var iRectangle: TFMxObject;
    unNoeud : TGBENoeud;
begin
  PathFinder.LargeurGrille := lGrille;
  PathFinder.HauteurGrille := hGrille;
  PathFinder.NoeudDepart := noeudDepart;
  PathFinder.NoeudArrivee := noeudArrivee;
  PathFinder.QuePremiereEtape := cbPremiereEtape.IsChecked;
  PathFinder.AutoriserDeplacementDiagonal := cbDiagonale.IsChecked;
  if cbModeCout.IsChecked then PathFinder.Mode := TGBEPathFinderMode.coutMinimum
  else PathFinder.Mode := TGBEPathFinderMode.deplacementsMinimum;

  // R�initialise les celleules de la grille qui ne sont ni les cases de d�part et d'arriv�e, ni les obstacles
  for iRectangle in layGrille.Children do begin
    unNoeud.position := getPoint(iRectangle.Tag);
    if (not(PathFinder.listeNoeudsObstacles.ContainsKey(unNoeud.position))) and
       (not(unNoeud.position = PathFinder.noeudArrivee.position)) and
       (not(unNoeud.position = PathFinder.noeudDepart.position)) then begin
      (iRectangle as TRectangle).Fill.Gradient.Color := TAlphaColorRec.White;
      ((iRectangle as TRectangle).Children[0] as TLabel).Text := '';
      ((iRectangle as TRectangle).Children[1] as TText).Text := '';
      ((iRectangle as TRectangle).Children[2] as TText).Text := '';
    end;
  end;

  if PathFinder.RechercherChemin then begin dessinerResultat; // Si on a trouv� un chemin, on le dessine
  end else lblInfos.Text := 'No path found';
end;

// Cr�ation d'une nouvelle grille
procedure TMainForm.btnCreerGrilleClick(Sender: TObject);
begin
  rectangleModele.Parent := fMain;
  layGrille.DeleteChildren;
  rectangleModele.Parent := layGrille;
  creerGrille;
  initialiserGrille;
end;

// Gestion du clic sur les cellules pour d�finir ou non la cellule comme obstacle
procedure TMainForm.celluleClick(Sender: TObject);
var
  noeudObstacle : TGBENoeud;
begin
  noeudObstacle.position := getPoint((sender as TRectangle).Tag);
  if PathFinder.listeNoeudsObstacles.ContainsKey(noeudObstacle.position) then begin
    PathFinder.listeNoeudsObstacles.Remove(noeudObstacle.position);
    mettreAJourCase(noeudObstacle.position, TAlphaColorRec.White, 0, 0);
  end else begin
    PathFinder.listeNoeudsObstacles.Add(noeudObstacle.position, noeudObstacle);
    mettreAJourCase(noeudObstacle.position, TAlphaColorRec.Darkslategrey, 0, 0);
  end;
end;

// Permet de r�cup�rer les coordonn�es en X et Y d'une cellule de la grille en fonction de son indice
function TMainForm.getPoint(indice: integer):TPoint;
begin
  result.X := indice mod lGrille;
  result.Y := indice div lGrille;
end;

// Permet de r�cup�rer l'indice d'une cellule depuis ses coordonn�es X et Y
function TMainForm.getIndice(point : TPoint):integer;
begin
  result := point.Y * lGrille + point.X;
end;

// Permet de dessiner le chemin trouv�
procedure TMainForm.dessinerResultat;
var
  coutDeplacement: integer;
  point : TPoint;
begin
  coutDeplacement := 0;

  for point in PathFinder.listeChemin.Keys do begin
    mettreAJourCase(PathFinder.listeChemin.Items[point].position, TAlphaColorRec.Cyan, PathFinder.listeChemin.Items[point].coutDeplacement, PathFinder.listeChemin.Items[point].heuristique);
    coutDeplacement := coutDeplacement + PathFinder.listeChemin.Items[point].coutDeplacement;
  end;
  lblInfos.Text := 'The shortest path is represented by the blue boxes' + sLineBreak + sLineBreak+
                   'Move : ' + (PathFinder.listeChemin.Count -1).ToString + slineBreak +
                   'Cost : ' + coutDeplacement.ToString;
end;

end.
