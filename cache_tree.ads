with tools; use tools;

package cache_tree is

    type T_Cache is limited private;

    -- nom : Initialiser
    -- sémantique : Initialiser le cache à initialiser
    -- paramètres :
    --      Cache : Mode Out T_Cache; -- le cache
    -- post-condition : Est_Vide(Cache)
    -- tests :
    --      entrées : . sortie : Cache = null.
    procedure Initialiser(Cache: out T_Cache) with
        Post => Est_Vide(Cache);

    function Est_Vide(Cache : T_Cache) return Boolean;


    -- nom : Vider
    -- sémantique : Vider entièrement le cache
    -- paramètres :
    --      Cache : Mode In/Out T_Cache; -- le cache à vider
    -- post-condition : Est_Vide(Cache)
    -- tests :
    --      entrées : Cache. sortie : Cache = null.
    procedure Vider(Cache : in out T_Cache) with
		Post => Est_Vide(Cache);


    -- nom : Enregistrer
    -- sémantique : Enregistrer une adresse, un masque et une sortie dans le cache
    -- paramètres :
    --      Cache : Mode In/Out T_Cache; -- le cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse IP à ajouter
    --      Masque : Mode In T_Adresse_IP; -- le masque à ajouter
    --      Sortie : Mode In Unbounded_String; -- la sortie à ajouter
    -- post-condition : Cache.All.Taille = Cache.All.Taille'Last + 1
    procedure Enregistrer(Cache : in out T_Cache; Adresse : in T_Adresse_IP; Masque : T_Adresse_IP; Sortie : Unbounded_String) with
        Post => Cache.All.Taille = Cache.All.Taille'Last + 1;


    -- nom : Ajouter_Frequence
    -- sémantique : Ajouter une fréquence lorsque l'on utilise une adresse dans le cache
    -- paramètres :
    --      Cache : Mode In/Out T_Cache; -- le cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse IP utilisée
    -- pré-condition : not Est_Vide(Cache)
    -- post-condition : Cache.All.Frequence = Cache.All.Frequence'Last + 1
    procedure Ajouter_Frequence(Cache : in out T_Cache; Adresse : in T_Adresse_IP) with
        Pre => not Est_Vide(Cache),
        Post => Cache.All.Frequence = Cache.All.Frequence'Last + 1;


    -- nom : Supprimer
    -- sémantique : Supprimer un élément du cache selon la politique
    -- paramètres :
    --      Cache : Mode In/Out T_Cache; -- le cache
    --      Politique : Mode In T_Politique; -- la politique choisie
    -- pré-condition : Est_Plein(Cache)
    -- post-condition : Cache.All.Taille = Cache.All.Taille'Last - 1
    procedure Supprimer(Cache : in out T_Cache; Politique : in T_Politique) with
        Pre => Est_Plein(Cache),
        Post => Cache.All.Taille = Cache.All.Taille'Last - 1;


    -- nom : Est_Plein
    -- sémantique : Permet de savoir si le cache est plein ou non
    -- paramètres :
    --      Cache : Mode In T_Cache; -- le cache
    -- pré-condition : not Est_Vide(Cache)
    function Est_Plein(Cache : in T_Cache) with
        Pre => not Est_Vide(Cache);

private

    type T_Cache_Cellule;

    type T_Cache_Arbre is access T_Cache_Cellule;

    type T_Cache_Cellule is record
        Taille : Integer; -- il s'agit de la hauteur de l'arbre
        Adresse : T_Adresse_IP;
        Masque : T_Adresse_IP;
        Sortie : Unbounded_String;
        Gauche : T_Cache;
        Droite : T_Cache;
        Frequence : Integer; -- pour appliquer les politiques
        Active : Boolean; -- pour savoir si la cellule est active ou non
    end record;

end cache_tree;
