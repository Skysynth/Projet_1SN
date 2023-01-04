with tools; use tools;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package cache_tree is

    type T_Cache_Arbre is limited private;

    -- nom : Initialiser
    -- sémantique : Initialiser le cache à initialiser
    -- paramètres :
    --      Cache : Mode Out T_Cache_Arbre; -- le cache
    -- post-condition : Est_Vide(Cache)
    -- tests :
    --      entrées : . sortie : Cache = null.
    procedure Initialiser(Cache: out T_Cache_Arbre) with
        Post => Est_Vide(Cache);

    function Est_Vide(Cache : T_Cache_Arbre) return Boolean;


    -- nom : Vider
    -- sémantique : Vider entièrement le cache
    -- paramètres :
    --      Cache : Mode In/Out T_Cache_Arbre; -- le cache à vider
    -- post-condition : Est_Vide(Cache)
    -- tests :
    --      entrées : Cache. sortie : Cache = null.
    procedure Vider(Cache : in out T_Cache_Arbre) with
		Post => Est_Vide(Cache);


    -- nom : Enregistrer
    -- sémantique : Enregistrer une adresse, un masque et une sortie dans le cache
    -- paramètres :
    --      Cache : Mode In/Out T_Cache_Arbre; -- le cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse IP à ajouter
    --      Masque : Mode In T_Adresse_IP; -- le masque à ajouter
    --      Sortie : Mode In Unbounded_String; -- la sortie à ajouter
    -- post-condition : Cache.All.Taille = Cache.All.Taille'Last + 1
    procedure Enregistrer(Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP; Masque : T_Adresse_IP; Sortie : Unbounded_String; Taille : Integer) with
        Post => Cache.All.Taille = Cache.All.Taille'Last + 1;


    -- nom : Ajouter_Frequence
    -- sémantique : Ajouter une fréquence lorsque l'on utilise une adresse dans le cache
    -- paramètres :
    --      Cache : Mode In/Out T_Cache_Arbre; -- le cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse IP utilisée
    -- pré-condition : not Est_Vide(Cache)
    -- post-condition : Cache.All.Frequence = Cache.All.Frequence'Last + 1
    procedure Ajouter_Frequence(Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP) with
        Pre => not Est_Vide(Cache),
        Post => Cache.All.Frequence = Cache.All.Frequence'Last + 1;


    -- nom : Supprimer
    -- sémantique : Supprimer un élément du cache selon la politique
    -- paramètres :
    --      Cache : Mode In/Out T_Cache_Arbre; -- le cache
    --      Politique : Mode In T_Politique; -- la politique choisie
    -- pré-condition : Est_Plein(Cache)
    -- post-condition : Cache.All.Taille = Cache.All.Taille'Last - 1
    procedure Supprimer(Cache : in out T_Cache_Arbre; Politique : in T_Politique) with
        Pre => Est_Plein(Cache),
        Post => Cache.All.Taille = Cache.All.Taille'Last - 1;


    -- nom : Est_Plein
    -- sémantique : Permet de savoir si le cache est plein ou non
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache
    -- pré-condition : not Est_Vide(Cache)
    function Est_Plein(Cache : in T_Cache_Arbre; Taille : in Integer) return Boolean with
        Pre => not Est_Vide(Cache);

    -- nom : Afficher_Cache
    -- sémantique : Permet d'afficher le cache
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache à afficher
    procedure Afficher_Cache(Cache : in T_Cache_Arbre);

private

    type T_Cache_Cellule;

    type T_Cache_Arbre is access T_Cache_Cellule;

    type T_Cache_Cellule is record
        Taille : Integer;
        Adresse : T_Adresse_IP;
        Masque : T_Adresse_IP;
        Sortie : Unbounded_String;
        Gauche : T_Cache_Arbre;
        Droite : T_Cache_Arbre;
        Frequence : Integer; -- pour appliquer les politiques
        Active : Boolean; -- pour savoir si la cellule est active ou non
    end record;

end cache_tree;
