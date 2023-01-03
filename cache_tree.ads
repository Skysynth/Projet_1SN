with tools; use tools;

package cache_tree is

    type T_Cache is limited private;

    procedure Initialiser(Cache: out T_Cache) with
        Post => Est_Vide(Cache);

    function Est_Vide (Cache : T_Cache) return Boolean;

    procedure Taille_Cache (Cache : in out T_Cache) with
		Post => (Taille /= 0 and not Est_Vide(Cache)) and (Taille == 0 and Est_Vide(Cache));

    procedure Vider (Cache : in out T_Cache) with
		Post => Est_Vide (Cache);

    procedure Enregistrer (Cache : in out T_Cache, ) with; -- manque peut être des pré et des post conditions

private

    type T_Cache_Cellule;

    type T_Cache is access T_Cache_Cellule;

    type T_Cache_Cellule is record
        Taille : Integer; -- il s'agit de la hauteur de l'arbre
        Adresse : T_Adresse_IP;
        Masque : T_Adresse_IP;
        Sortie : Unbounded_String;
        Gauche : T_Cache;
        Droite : T_Cache;
        Frequence : Integer; -- pour appliquer les politiques
    end record;

end cache_tree;
