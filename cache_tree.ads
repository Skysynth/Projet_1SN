with tools; use tools;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package cache_tree is

    type T_Cache_Arbre is private;
    type T_Arbre is private;

    -- nom : Initialiser
    -- sémantique : Initialiser le cache et sa taille
    -- paramètres :
    --      Cache : Mode Out T_Cache_Arbre; -- le cache
    --      Taille : Mode In Integer; -- la taille du cache
    --      Politique : Mode In T_Politique; -- la politique du cache
    -- post-condition : Est_Vide(Cache.Arbre) and Cache.Taille = 0 and Cache.Defauts = 0 and Cache.Demandes = 0 and Cache.Enregistrement = 0
    -- tests :
    --      entrées : . sortie : Cache = null.
    procedure Initialiser(Cache : out T_Cache_Arbre; Taille_Max : in Integer; Politique : in T_Politique) with
        Post => Est_Vide(Cache) and Taille_Cache(Cache) = 0;


    -- nom : Est_Vide
    -- sémantique : Savoir si l'arbre du cache est vide ou non
    -- paramètres :
    --      Cache : Mode in T_Cache_Arbre; -- le cache
    -- tests :
    --      entrées : . sortie : True.
    function Est_Vide(Cache : in T_Cache_Arbre) return Boolean;


    -- nom : Vider
    -- sémantique : Vider entièrement le cache
    -- paramètres :
    --      Cache : Mode In/Out T_Cache_Arbre; -- le cache à vider
    -- post-condition : Est_Vide(Cache)
    -- tests :
    --      entrées : Cache. sortie : Cache = null.
    procedure Vider(Cache : in out T_Cache_Arbre) with
		Post => Est_Vide(Cache);


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
    --      Cache : Mode In T_Cache_Arbre; -- l'arbre
    -- post-condition : Frequence_Arbre'Result >= 0
    function Frequence_Arbre(Cache : in T_Cache_Arbre) return Integer with
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
    --      Cache : Mode In/Out T_Cache_Arbre; -- le cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse IP à ajouter
    --      Masque : Mode In T_Adresse_IP; -- le masque à ajouter
    --      Sortie : Mode In Unbounded_String; -- la sortie à ajouter
    --      Politique : Mode In T_Politique; -- la politique utilisée
    -- post-condition : Taille_Cache(Arbre) = Taille_Cache(Arbre)'Old + 1
    procedure Enregistrer(Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP; Masque : in T_Adresse_IP; Sortie : in Unbounded_String; Politique : in T_Politique) with
        Post => Taille_Cache(Cache) = Taille_Cache(Cache)'Old + 1;


    -- nom : Ajouter_Frequence
    -- sémantique : Ajouter une fréquence lorsque l'on utilise une adresse dans le cache
    -- paramètres :
    --      Cache : Mode In/Out T_Cache_Arbre; -- le cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse IP utilisée
    --      Masque : Mode In T_Adresse_IP; -- le masque
    -- pré-condition : not Est_Vide(Arbre)
    -- post-condition : Frequence_Arbre(Arbre_Cache(Arbre)) = Frequence_Arbre(Arbre_Cache(Arbre))'Last + 1
    procedure Ajouter_Frequence(Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP; Masque : in T_Adresse_IP) with
        Pre => not Est_Vide(Cache),
        Post => Frequence_Arbre(Cache) = Frequence_Arbre(Cache)'Old + 1;


    -- nom : Supprimer
    -- sémantique : Supprimer un élément du cache selon la politique
    -- paramètres :
    --      Cache : Mode In/Out T_Cache_Arbre -- le cache
    --      Masque : Mode In T_Adresse_IP; -- le masque
    -- pré-condition : Est_Plein(Cache)
    -- post-condition : Taille_Cache(Arbre) = Taille_Cache(Arbre)'Old - 1
    procedure Supprimer(Cache : in out T_Cache_Arbre; Masque : in T_Adresse_IP) with
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
    --      Cache : Mode In T_Cache_Arbre; -- le cache à afficher
    procedure Afficher_Arbre(Cache : in T_Cache_Arbre);


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
    --      Cache : Mode In T_Cache_Arbre; -- le cache
    -- post-condition : Recherche_Identifiant_Max'Result >= 0
    function Recherche_Identifiant_Max(Cache : in T_Cache_Arbre) return Integer with
        Post => Recherche_Identifiant_Max'Result >= 0;

    
    -- nom : Chercher_Arbre
    -- sémantique : Permet de renvoyer la sortie correspondante à une adresse dans le cache
    -- paramètres :
    --      Cache : Mode In/Out T_Cache_Arbre; -- le cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse
    --      Politique : Mode In T_Politique; -- la politique
    --      Masque : Mode In T_Adresse_IP; -- le masque
    function Chercher_Arbre(Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP) return Unbounded_string;

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
        Politique : T_Politique;
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
