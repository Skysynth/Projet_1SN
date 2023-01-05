with tools; use tools;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package cache_tree is

    type T_Cache_Arbre is limited private;
    type T_Arbre is limited private;

    -- nom : Initialiser
    -- sémantique : Initialiser le cache et sa taille
    -- paramètres :
    --      Cache : Mode Out T_Cache_Arbre; -- le cache
    --      Taille : Mode in Integer; -- la taille du cache
    -- post-condition : Est_Vide(Cache.Arbre) and Cache.Taille = Taille
    -- tests :
    --      entrées : . sortie : Cache = null.
    procedure Initialiser(Cache : out T_Cache_Arbre; Taille : in Integer) with
        Post => Est_Vide(Cache.Arbre) and Cache.Taille = Taille;


    -- nom : Est_Vide
    -- sémantique : Savoir si l'arbre du cache est vide ou non
    -- paramètres :
    --      Arbre : Mode in T_Arbre; -- le cache
    -- tests :
    --      entrées : . sortie : True.
    function Est_Vide(Arbre : in T_Arbre) return Boolean;


    -- nom : Vider
    -- sémantique : Vider entièrement le cache
    -- paramètres :
    --      Arbre : Mode In/Out T_Arbre; -- le cache à vider
    -- post-condition : Est_Vide(Cache)
    -- tests :
    --      entrées : Cache. sortie : Cache = null.
    procedure Vider(Arbre : in out T_Arbre) with
		Post => Est_Vide(Arbre);


    -- nom : Taille
    -- sémantique : Permet d'afficher la taille du cache
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache à afficher
    function Taille(Cache : in T_Cache_Arbre) return Integer with
        Post => Taille'Result >= 0;

    -- nom : Frequence
    -- sémantique : Permet d'afficher la fréquence d'une cellule de l'arbre du cache
    -- paramètres :
    --      Arbre : Mode In T_Arbre; -- le cache à afficher
    function Frequence(Arbre : in T_Arbre) return Integer with
        Post => Frequence'Result >= 0;


    -- nom : Enregistrer
    -- sémantique : Enregistrer une adresse, un masque et une sortie dans le cache
    -- paramètres :
    --      Arbre : Mode In/Out T_Arbre; -- l'arbre du cache
    --      Cache : Mode In/Out T_Cache_Arbre; -- le cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse IP à ajouter
    --      Masque : Mode In T_Adresse_IP; -- le masque à ajouter
    --      Sortie : Mode In Unbounded_String; -- la sortie à ajouter
    -- post-condition : Taille(Arbre) = Taille(Arbre)'Old + 1
    procedure Enregistrer(Arbre : in out T_Arbre; Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP; Masque : T_Adresse_IP; Sortie : Unbounded_String; Taille : Integer) with
        Post => Taille(Cache) = Taille(Cache)'Old + 1;


    -- nom : Ajouter_Frequence
    -- sémantique : Ajouter une fréquence lorsque l'on utilise une adresse dans le cache
    -- paramètres :
    --      Arbre : Mode In/Out T_Arbre; -- l'arbre du cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse IP utilisée
    -- pré-condition : not Est_Vide(Arbre)
    -- post-condition : Cache.All.Frequence = Cache.All.Frequence'Last + 1
    procedure Ajouter_Frequence(Arbre : in out T_Arbre; Adresse : in T_Adresse_IP) with
        Pre => not Est_Vide(Arbre),
        Post => Frequence(Arbre) = Frequence(Arbre)'Old + 1;


    -- nom : Supprimer
    -- sémantique : Supprimer un élément du cache selon la politique
    -- paramètres :
    --      Arbre : Mode In/Out T_Arbre; -- l'arbre du cache
    --      Cache : Mode In/Out T_Cache_Arbre -- le cache
    --      Politique : Mode In T_Politique; -- la politique choisie
    -- pré-condition : Est_Plein(Cache)
    -- post-condition : Taille(Arbre) = Taille(Arbre)'Old - 1
    procedure Supprimer(Arbre : in out T_Arbre; Cache : in out T_Cache_Arbre; Politique : in T_Politique; Taille : in Integer) with
        Pre => Est_Plein(Cache, Taille),
        Post => Taille(Cache) = Taille(Cache)'Old - 1;


    -- nom : Est_Plein
    -- sémantique : Permet de savoir si le cache est plein ou non
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache
    -- pré-condition : Cache.Taille > 0
    function Est_Plein(Cache : in T_Cache_Arbre; Taille : in Integer) return Boolean with
        Pre => Taille(Cache) > 0;

    -- nom : Afficher_Cache
    -- sémantique : Permet d'afficher le cache
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache à afficher
    procedure Afficher_Cache(Cache : in T_Cache_Arbre);

private

    type T_Cache_Cellule;

    type T_Arbre is access T_Cache_Cellule;
    type T_Cache_Arbre is record
        Arbre : T_Arbre;
        Taille : Integer;
    end record;

    type T_Cache_Cellule is record
        Adresse : T_Adresse_IP;
        Masque : T_Adresse_IP;
        Sortie : Unbounded_String;
        Gauche : T_Arbre;
        Droite : T_Arbre;
        Frequence : Integer; -- pour appliquer les politiques
        Active : Boolean; -- pour savoir si la cellule est active ou non
    end record;

end cache_tree;
