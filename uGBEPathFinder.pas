{ Auteur : Grégory BERSEGEAY (http://www.gbesoft.fr)
  Implémentation de l'algorithme A* (https://fr.wikipedia.org/wiki/Algorithme_A*)
}
unit uGBEPathFinder;

interface

uses System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections;

Type
  TGBENoeud = record
  public
    coutDeplacement, heuristique, estimationCout : integer;
    position, parent : TPoint;
  end;

  TGBEPathFinderMode = (deplacementsMinimum, coutMinimum);

  TGBEPathFinder = class
    fNoeudDepart, fNoeudArrivee : TGBENoeud;
    listeNoeudsPossibles : TDictionary<TPoint, TGBENoeud>;
    listeNoeudsVoisins : TDictionary<TPoint, TGBENoeud>;
    flGrille, fhGrille, fCoutDeplacementCote, fCoutDeplacementDiagonal : integer;
    fAutoriserDeplacementDiagonal, fQuePremiereEtape : boolean;
    fMode : TGBEPathFinderMode;

    function calculerCoutArrivee(point : TPoint):integer;
    procedure optimiserChemin;
    function rechercheCoutTotalMin(liste: TDictionary<TPoint, TGBENoeud>):TGBENoeud;
    procedure listerVoisins(unNoeud: TGBENoeud);

  public
    listeChemin : TDictionary<TPoint, TGBENoeud>;
    listeNoeudsObstacles : TDictionary<TPoint, TGBENoeud>;

    constructor Create; virtual;
    destructor Destroy; override;
    function RechercherChemin:boolean;

    property NoeudDepart : TGBENoeud read fNoeudDepart write fNoeudDepart;
    property NoeudArrivee: TGBENoeud read fNoeudArrivee write fNoeudArrivee;
    property LargeurGrille : integer read flGrille write flGrille;
    property HauteurGrille : integer read fhGrille write fhGrille;
    property CoutDeplacementCote : integer read fCoutDeplacementCote write fCoutDeplacementCote;
    property CoutDeplacementDiagonal : integer read fCoutDeplacementDiagonal write fCoutDeplacementDiagonal;
    property AutoriserDeplacementDiagonal : boolean read fAutoriserDeplacementDiagonal write fAutoriserDeplacementDiagonal;
    property QuePremiereEtape : boolean read fQuePremiereEtape write fQuePremiereEtape;
    property Mode : TGBEPathFinderMode read fMode write fMode;
  end;

implementation

{ TGBEPathFinder }

constructor TGBEPathFinder.Create;
begin
  largeurGrille := 12;
  hauteurGrille := 10;
  CoutDeplacementCote := 10;
  CoutDeplacementDiagonal := 15;
  AutoriserDeplacementDiagonal := true;
  QuePremiereEtape := false;
  Mode := TGBEPathFinderMode.deplacementsMinimum;

  listeNoeudsPossibles := TDictionary<TPoint, TGBENoeud>.create;
  listeChemin := TDictionary<TPoint, TGBENoeud>.create;
  listeNoeudsObstacles := TDictionary<TPoint, TGBENoeud>.create;
  listeNoeudsVoisins := TDictionary<TPoint, TGBENoeud>.create;
end;

destructor TGBEPathFinder.Destroy;
begin
  FreeAndNil(listeNoeudsPossibles);
  FreeAndNil(listeChemin);
  FreeAndNil(listeNoeudsObstacles);
  FreeAndNil(listeNoeudsVoisins);
  inherited;
end;

// Permet de calculer le cout d'un point donné jusqu'à l'arrivée
function TGBEPathFinder.calculerCoutArrivee(point : TPoint):integer;
var valeurDiagonale, valeurCote , absX, absY : integer;
begin
  absX := abs(point.X - noeudArrivee.position.X);
  absY := abs(point.Y - noeudArrivee.position.Y);
  if absX > absY then begin
    valeurDiagonale := absY * coutDeplacementDiagonal;
    valeurCote := (absX - absY) * coutDeplacementCote;
  end else begin
    valeurDiagonale := absX * coutDeplacementDiagonal;
    valeurCote := (absY - absX) * coutDeplacementCote;
  end;
  result := valeurDiagonale + valeurCote;
end;

// Algorithme A* : 1ère étape
// On explore toutes les pistes jusqu'à trouver le noeud d'arrivée
function TGBEPathFinder.RechercherChemin:boolean;
var unNoeud : TGBENoeud;
    unVoisin : TPoint;
begin
  result := false; // initialisation du retour à false (indiquant qu'aucun chemin n'a été trouvé)
  listeChemin.Clear;
  listeNoeudsVoisins.Clear;
  listeNoeudsPossibles.Clear;
  listeNoeudsPossibles.Add(NoeudDepart.position,NoeudDepart);  // au début, on se place sur le noeud de départ, c'est le seul noeud possible

  while listeNoeudsPossibles.Count > 0 do begin  // Tant que la liste des noeuds possibles n'est pas vide
    unNoeud := rechercheCoutTotalMin(listeNoeudsPossibles); // recherche du noeud possible ayant le cout minimum
    listeNoeudsPossibles.Remove(unNoeud.position);  // on enlève le noeud trouvé de la liste des noeuds possibles
    listeChemin.Add(unNoeud.position, unNoeud);  // on le rajoute à la liste des noeuds parcourus pour trouver le chemin

    if unNoeud.position = noeudArrivee.position then begin  // si le noeud trouvé est le noeud d'arrivée (test sur la position)
      noeudArrivee := unNoeud; // on reprend les informations du noeud trouvé pour compléter les informations du noeud d'arrivée (entre autre la position de son parent)
      listeNoeudsPossibles.Clear;
      result := true; // On a trouvé un chemin
      break;          // on sort du while
    end;

    listerVoisins(unNoeud);  // On renseigne la liste des noeuds voisins du noeud trouvé

    for unVoisin in listeNoeudsVoisins.Keys do begin  // Parcours des noeuds voisins
      if listeChemin.ContainsKey(unVoisin) then continue;   // Si le voisin est déjà dans la liste des noeuds parcourus on passe à l'itération suivante

      if not(listeNoeudsPossibles.ContainsKey(unVoisin)) then begin // Si le voisin n'est pas déjà dans la liste des noeuds possibles, on l'y rajoute
        listeNoeudsPossibles.Add(unVoisin, listeNoeudsVoisins.Items[unVoisin]);
      end;
    end;
  end;

  // 1ere étape terminée, si on a trouvé une solution et que l'on souhaite faire la 2nd étape,
  // alors on passe à "l'optimisation"
  if result and not(QuePremiereEtape) then optimiserChemin;
end;

// 2ème partie : permet de tracer uniquement le chemin à partir des pistes explorées à l'étape 1
// On va parcourir la liste des noeuds explorés à l'étape 1 en partant du noeud d'arrivée et en remontant
// jusqu'au noeud de départ afin de ne dresser la liste que des noeuds nécessaires à la constitution du chemin
procedure TGBEPathFinder.optimiserChemin;
var listeOptimisee : TList<TGBENoeud>;
    iNoeud : TGBENoeud;
begin
  listeOptimisee := TList<TGBENoeud>.create;  // On passe par une liste temporaire
  iNoeud := noeudArrivee; // on part du noeud d'arrivée

  while iNoeud.position <> noeudDepart.position do begin  // Tant qu'on n'est pas sur le noeud de départ
    listeOptimisee.Add(iNoeud); // on place le noued courant dans la liste temporaire
    listeChemin.TryGetValue(iNoeud.parent, iNoeud); // le nouveau noeud courant devient le noeud parent du noeud courant
  end;
  listeOptimisee.Add(noeudDepart); // On ajoute le noeud de départ à la fin de la liste
  listeOptimisee.Reverse; // On inverse la liste (pour avoir les noeuds dans l'ordre noeud de départ vers noeud d'arrivée)

  listeChemin.Clear;
  for iNoeud in listeOptimisee do // On replace dans listeChemin la liste optimisée trouvée
    listeChemin.Add(iNoeud.position, iNoeud);

  FreeAndNil(listeOptimisee);
end;

// Permet de récupérer le noeud le moins couteux d'une liste
function TGBEPathFinder.rechercheCoutTotalMin(liste: TDictionary<TPoint, TGBENoeud>):TGBENoeud;
var iNoeud : TPoint;
    tableau : TArray<TPair<TPoint, TGBENoeud>>;
begin
  if liste.Count > 0 then begin
    tableau := liste.ToArray;   // Astuce pour récupérer le premier élément d'un TDictionary (pas de méthode first sur le TDictionary)
    result := tableau[0].Value; // Astuce pour récupérer le premier élément d'un TDictionary
    for iNoeud in liste.Keys do begin  // Parcours de la liste
      if liste.Items[iNoeud].estimationCout < result.estimationCout then result := liste.Items[iNoeud]
      else begin
        if liste.Items[iNoeud].estimationCout = result.estimationCout then begin
          case mode of
            deplacementsMinimum: begin
                                   if liste.Items[iNoeud].heuristique < result.heuristique then result := liste.Items[iNoeud]
                                   else begin
                                     if liste.Items[iNoeud].heuristique = result.heuristique then begin
                                       if liste.Items[iNoeud].coutDeplacement < result.coutDeplacement then result := liste.Items[iNoeud];
                                     end;
                                   end;
                                 end;
            coutMinimum: begin
                           if liste.Items[iNoeud].coutDeplacement < result.coutDeplacement then result := liste.Items[iNoeud]
                           else begin
                             if liste.Items[iNoeud].coutDeplacement = result.coutDeplacement then begin
                               if liste.Items[iNoeud].heuristique < result.heuristique then result := liste.Items[iNoeud];
                             end;
                           end;
                         end;
          end;
        end;
      end;
    end;
  end;
end;

{ Permet de lister les voisins d'un noeud donné }
procedure TGBEPathFinder.listerVoisins(unNoeud: TGBENoeud);
var unVoisin : TGBENoeud;
    x, y : integer;
begin
  listeNoeudsVoisins.Clear;

  // Parcours des 8 positions autour du noeud donné
  for x := -1 to 1 do begin
    for y := -1 to 1 do begin
      if (x = 0) and (y = 0) then continue;
      if not(AutoriserDeplacementDiagonal) then begin // si les déplacements en diagonal sont autorisés
        if (x = -1) and (y = -1) then continue;
        if (x = 1) and (y = -1) then continue;
        if (x = 1) and (y = 1) then continue;
        if (x = -1) and (y = 1) then continue;
      end;

      unVoisin.position.x := unNoeud.position.X + x;
      unVoisin.position.y := unNoeud.position.Y + y;

      // Le voisin doit être dans la grille
      if (unVoisin.position.x >= 0) and (unVoisin.position.x < LargeurGrille) and
         (unVoisin.position.y >= 0) and (unVoisin.position.y < HauteurGrille) then begin
        if (unVoisin.position.x <> unNoeud.position.x) and (unVoisin.position.y <> unNoeud.position.y) then
           unVoisin.coutDeplacement := coutDeplacementDiagonal
        else unVoisin.coutDeplacement := coutDeplacementCote;
        unVoisin.parent := unNoeud.position;

        // Si le voisin n'est pas dans la liste des noeuds obstacles, on peut le rajouter à la liste des noeuds voisins
        if (not(listeNoeudsObstacles.ContainsKey(unVoisin.position))) then begin
          unVoisin.heuristique := calculerCoutArrivee(unVoisin.position); // On calcule ses couts
          unVoisin.estimationCout := unVoisin.coutDeplacement + unVoisin.heuristique;
          listeNoeudsVoisins.Add(unVoisin.position, unVoisin); // On ajoute le noeud à la liste des voisins
        end;
      end;
    end;
  end;
end;

end.
