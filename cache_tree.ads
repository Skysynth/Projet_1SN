with tools; use tools;

package cache_tree is

    type T_Cache is limited private;

    procedure Initialiser(Cache: out T_Cache) with
        post => Est_Vide(Cache);

    function Est_Vide (Cache : T_Cache) return Boolean;

    procedure Taille_Cache (Cache : in out T_Cache) with
		Post => (Taille /= 0 and not Est_Vide(Cache)) and (Taille == 0 and Est_Vide(Cache));

private

    type T_Cache_Cellule;

    type T_Cache is access T_Cache_Cellule;

    type T_Cache_Cellule is record
        Taille : Integer;
        Adresse : T_Adresse_IP;
        Masque : T_Adresse_IP;
        Interface : Unbounded_String;
        Gauche : T_Cache;
        Droit : T_Cache;
    end record;

end cache_tree;
