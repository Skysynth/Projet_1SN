with tools; use tools;

package cache_tree is

    type T_Cache is limited private;

    procedure Initialiser(Cache: out T_Cache) with
        post Est_Vide(Cache);

    function Est_Vide (Sda : T_LCA) return Boolean;

    function Taille (Sda : in T_LCA) return Integer with
		Post => Taille'Result >= 0 and (Taille'Result = 0) = Est_Vide (Sda);

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
