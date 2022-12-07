with tools; use tools;

package cache_tree is

    type T_Cache is limited private;

private

    type T_Cache_Cellule;

    type T_Cache is access T_Cache_Cellule;

    type T_Cache_Cellule is record
        Taille: Integer;
        Adresse: T_Adresse_IP;
        Masque: T_Adresse_IP;
        Interface: Unbounded_String;
        Gauche: T_Cache;
        Droit: T_Cache;
    end record;

end cache_tree;
