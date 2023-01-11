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
    -- post-condition : Est_Vide(Cache.Arbre) and Cache.Taille = 0
    -- tests :
    --      entrées : . sortie : Cache = null.
    procedure Initialiser(Cache : out T_Cache_Arbre; Taille_Max : in Integer) with
        Post => Est_Vide(Arbre_Cache(Cache)) and Taille_Cache(Cache) = 0;


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
    -- post-condition : Taille_Cache'Result >= 0
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
    -- post-condition : Frequence_Arbre'Result >= 0
    function Frequence_Arbre(Arbre : in T_Arbre) return Integer with
        Post => Frequence_Arbre'Result >= 0;

    -- nom : Demandes_Cache
    -- sémantique : Permet de récupérer le nombre de demandes dans le cache
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache
    -- post-condition : Demandes_Cache'Result >= 0
	function Demandes_Cache(Cache : in T_Cache_Arbre) return Integer with
	    Post => Demandes_Cache'Result >= 0;


    -- nom : Defauts_Cache
    -- sémantique : Permet de récupérer le nombre de défauts dans le cache
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache
    -- post-condition : Defauts_Cache'Result >= 0
	function Defauts_Cache(Cache : in T_Cache_Arbre) return Integer with
        Post => Defauts_Cache'Result >= 0;


    -- nom : Enregistrement_Cache
    -- sémantique : Permet de récupérer le nombre d'enregistrements dans le cache
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache
    -- post-condition : Enregistrement_Cache'Result >= 0
	function Enregistrement_Cache(Cache : in T_Cache_Arbre) return Integer with
        Post => Enregistrement_Cache'Result >= 0;


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
        Pre => Est_Plein(Cache),
        Post => Taille_Cache(Cache) = Taille_Cache(Cache)'Old - 1;


    -- nom : Est_Plein
    -- sémantique : Permet de savoir si le cache est plein ou non
    -- paramètres :
    --      Cache : Mode In T_Cache_Arbre; -- le cache
    -- pré-condition : Taille_Cache(Cache) > 0
    function Est_Plein(Cache : in T_Cache_Arbre) return Boolean with
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


    -- nom : Recherche_Identifiant_Max
    -- sémantique : Permet de trouver l'identifiant max, cela est nécessaire pour la politique LRU. En effet, lorsque j'utilise le max, je regarde si l'identifiant de la route utilisée
    -- correspond au max des identifiants. Dans ce cas, c'est la route la plus récemment identifiée. Sinon, on affecte son identifiant à max + 1. Pour la suppression, il ne reste qu'à regarder
    -- l'identifiant minimum.
    -- paramètres :
    --      Arbre : Mode In T_Arbre; -- l'arbre du cache
    -- post-condition : Recherche_Identifiant_Max'Result >= 0
    function Recherche_Identifiant_Max(Arbre : in T_Arbre) return Integer with
        Post => Recherche_Identifiant_Max'Result >= 0;

    
    -- nom : Chercher_Cache
    -- sémantique : Permet de renvoyer la sortie correspondante à une adresse dans le cache
    -- paramètres :
    --      Arbre : Mode In/Out T_Arbre; -- l'arbre du cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse
    --      Politique : Mode In T_Politique; -- la politique
    --      Masque : Mode In T_Adresse_IP; -- le masque
    function Chercher_Cache(Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP; Politique : in T_Politique) return Unbounded_string;

private

    type T_Arbre_Cellule;

    type T_Arbre is access T_Arbre_Cellule;
    type T_Cache_Arbre is record
        Arbre : T_Arbre;
        Taille : Integer;
        Taille_Max : Integer;
        Defauts : Integer;
        Demandes : Integer;
        Enregistrement : Integer; -- nombre d'enregistrement dans le cache (pas borné par la taille, le mettre par défaut à 0)
    end record;

    type T_Arbre_Cellule is record
        Adresse : T_Adresse_IP;
        Masque : T_Adresse_IP;
        Sortie : Unbounded_String;
        Gauche : T_Arbre;
        Droite : T_Arbre;
        Frequence : Integer; -- pour appliquer la politique LFU
        Feuille : Boolean; -- pour savoir si la cellule est active ou non, c'est-à-dire si c'est une feuille
        Identifiant : Integer; -- pour appliquer les politiques LRU et FIFO
    end record;

end cache_tree;
