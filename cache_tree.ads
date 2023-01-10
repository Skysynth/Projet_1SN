with tools; use tools;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
    Taille : Integer;
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
        Post => Est_Vide(Arbre_Cache(Cache)) and Taille_Cache(Cache) = Taille;


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


    -- nom : Taille_Cache
    -- sémantique : Permet de récupérer la taille du cache
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache
    function Taille_Cache(Cache : in T_Cache_Arbre) return Integer with
        Post => Taille_Cache'Result >= 0;


    -- nom : Arbre_Cache
    -- sémantique : Permet de récupérer l'arbre du cache
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache
    function Arbre_Cache(Cache : in T_Cache_Arbre) return T_Arbre;


    -- nom : Frequence_Arbre
    -- sémantique : Permet de récupérer la fréquence d'une cellule de l'arbre du cache
    -- paramètres :
    --      Arbre : Mode In T_Arbre; -- l'arbre
    function Frequence_Arbre(Arbre : in T_Arbre) return Integer with
        Post => Frequence_Arbre'Result >= 0;


    -- nom : Enregistrer
    -- sémantique : Enregistrer une adresse, un masque et une sortie dans le cache
    -- paramètres :
    --      Arbre : Mode In/Out T_Arbre; -- l'arbre du cache
    --      Cache : Mode In/Out T_Cache_Arbre; -- le cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse IP à ajouter
    --      Masque : Mode In T_Adresse_IP; -- le masque à ajouter
    --      Sortie : Mode In Unbounded_String; -- la sortie à ajouter
    --      Politique : Mode In T_Politique; -- la politique utilisée
    -- post-condition : Taille_Cache(Arbre) = Taille_Cache(Arbre)'Old + 1
    procedure Enregistrer(Arbre : in out T_Arbre; Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP; Masque : in T_Adresse_IP; Sortie : in Unbounded_String; Politique : in T_Politique) with
        Post => Taille_Cache(Cache) = Taille_Cache(Cache)'Old + 1;


    -- nom : Ajouter_Frequence
    -- sémantique : Ajouter une fréquence lorsque l'on utilise une adresse dans le cache
    -- paramètres :
    --      Arbre : Mode In/Out T_Arbre; -- l'arbre du cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse IP utilisée
    --      Masque : Mode In T_Adresse_IP; -- le masque
    -- pré-condition : not Est_Vide(Arbre)
    -- post-condition : Frequence_Arbre(Arbre) = Frequence_Arbre(Arbre)'Last + 1
    procedure Ajouter_Frequence(Arbre : in out T_Arbre; Adresse : in T_Adresse_IP; Masque : in T_Adresse_IP) with
        Pre => not Est_Vide(Arbre),
        Post => Frequence_Arbre(Arbre) = Frequence_Arbre(Arbre)'Old + 1;


    -- nom : Supprimer
    -- sémantique : Supprimer un élément du cache selon la politique
    -- paramètres :
    --      Arbre : Mode In/Out T_Arbre; -- l'arbre du cache
    --      Cache : Mode In/Out T_Cache_Arbre -- le cache
    --      Politique : Mode In T_Politique; -- la politique choisie
    --      Masque : Mode In T_Adresse_IP; -- le masque
    -- pré-condition : Est_Plein(Cache)
    -- post-condition : Taille_Cache(Arbre) = Taille_Cache(Arbre)'Old - 1
    procedure Supprimer(Arbre : in out T_Arbre; Cache : in out T_Cache_Arbre; Politique : in T_Politique; Masque : in T_Adresse_IP) with
        Pre => Est_Plein(Cache, Taille),
        Post => Taille_Cache(Cache) = Taille_Cache(Cache)'Old - 1;


    -- nom : Est_Plein
    -- sémantique : Permet de savoir si le cache est plein ou non
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache
    -- pré-condition : Taille_Cache(Cache) > 0
    function Est_Plein(Cache : in T_Cache_Arbre; Taille : in Integer) return Boolean with
        Pre => Taille_Cache(Cache) > 0;


    -- nom : Afficher_Arbre
    -- sémantique : Permet d'afficher le cache
    -- paramètres :
    --      Arbre : Mode In T_Arbre; -- le cache à afficher
    procedure Afficher_Arbre(Arbre : in T_Arbre);


    -- nom : Afficher_Statistiques_Cache
    -- sémantique : Permet d'afficher les statistiques relatives au cache et à sa politique
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache dont les statistiques doivent être affichées
    procedure Afficher_Statistiques_Cache(Cache : in T_Cache_Arbre);

private

    type T_Cache_Cellule;

    type T_Arbre is access T_Cache_Cellule;
    type T_Cache_Arbre is record
        Arbre : T_Arbre;
        Taille : Integer;
        Defauts : Integer;
        Demandes : Integer;
        Enregistrement : Integer; -- nombre d'enregistrement dans le cache (pas borné par la taille)
    end record;

    type T_Cache_Cellule is record
        Adresse : T_Adresse_IP;
        Masque : T_Adresse_IP;
        Sortie : Unbounded_String;
        Gauche : T_Arbre;
        Droite : T_Arbre;
        Frequence : Integer; -- pour appliquer la politique LFU
        Active : Boolean; -- pour savoir si la cellule est active ou non
        Identifiant : Integer; -- pour appliquer les politiques LRU et FIFO
    end record;

end cache_tree;
