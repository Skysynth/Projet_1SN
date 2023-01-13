with tools; use tools;
with Table_Routage; use Table_Routage;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package CACHE_LCA is

    TAILLE_MAX : Integer;
    POLITIQUE : T_Politique;

    type T_RECENT_LCA is limited private;

    type T_CACHE_LCA is limited private;

    -- Initialiser le cache.  Le cache est vide.
    procedure Initialiser(Cache_lca: out T_CACHE_LCA ; Taille : Integer ; Pol : T_Politique) with
        Post => Est_Vide(Cache_lca);

    -- Le cache est-il plein ?
    function Est_Plein(Cache_lca : T_CACHE_LCA) return Boolean with
        Post => (Taille(Cache_lca) = TAILLE_MAX) = Est_Plein'Result;

    -- Supprimer un element du cache, suivant la politique demandee au prealable par l'utilisateur.
    procedure Supprimer(Cache_lca : in out T_CACHE_LCA) with
        Post => Taille(Cache_lca) = Taille(Cache_lca)'Old - 1;

    -- Savoir si une adresse est presente dans le cache.
    function Adresse_Presente(Cache_lca : in T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return Boolean;

    -- Recuperer le masque et l'interface associes a l'adresse demandee, dans le cache.
    procedure Recuperer(Cache_lca : in out T_CACHE_LCA ; Adresse : T_ADRESSE_IP) with
        Pre => Adresse_Presente(Cache_lca, Adresse);

    -- Recherche de l'interface d'une adresse dans le cache : renvoie null si rien trouvé
    function Chercher_Dans_Cache(Cache : in T_CACHE_LCA ; Adresse : T_Adresse_IP) return Unbounded_String;

    -- Enregistrer une nouvelle route dans le cache.
    procedure Enregistrer(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP ; Eth : in Unbounded_String);

    -- Supprimer tous les elements du cache.
    procedure Vider(Cache_lca : in out T_CACHE_LCA) with
        Post => Est_Vide(Cache_lca);

    -- Le cache est-il vide ?
    function Est_Vide(Cache_lca : in T_CACHE_LCA) return Boolean;

    -- Renvoie la taille du cache.
    function Taille(Cache_lca : T_CACHE_LCA) return Integer with
        Post => Taille'Result >= 0
        and (Taille'Result = 0) = Est_Vide(Cache_lca);

    -- Supprime un element suivant la politique FIFO.
    -- procedure Supprimer_FIFO(Cache_lca : in out T_CACHE_LCA) with
    -- Post => Cache_lca := Cache_lca.all.Suivant'Old;

    -- Ajouter une adresse a la liste chainee Recente.
    -- procedure Ajouter_Recent(Rec_lca : in out T_RECENT_LCA ; Adresse : in T_ADRESSE_IP);

    -- Supprimer une adresse a la liste chainee Recente.
    -- procedure Supprimer_Recent(Rec_lca : in out T_RECENT_LCA ; Adresse : in T_ADRESSE_IP);

    -- Supprime un element suivant la politique LRU.
    -- procedure Supprimer_LRU(Cache_lca : in out T_CACHE_LCA);

    -- Recuperer la frequence minimale d'utilisation des adresses dans le cache.
    -- function Adresse_LFU(Cache_lca : in T_CACHE_LCA) return integer;

    -- Supprime un element suivant la politique LFU.
    -- procedure Supprimer_LFU(Cache_lca : in out T_CACHE_LCA);



private

    type T_Case;

    type T_RECENT_LCA is access T_Case;

    type T_Case is
        record
            Adresse : T_ADRESSE_IP;
            Suivant : T_RECENT_LCA;
        end record;

    type T_Cellule;

    type T_CACHE_LCA is access T_Cellule;

    type T_Cellule is
        record
            Adresse : T_ADRESSE_IP;
            Masque : T_ADRESSE_IP;
            Eth : Unbounded_String;
            Frequence : Integer;
            Suivant : T_CACHE_LCA;
        end record;

    RECENT_LCA : T_RECENT_LCA;

end CACHE_LCA;
