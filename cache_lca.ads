generic
   type T_ADRESSE_IP is private;
	type T_IFACE is private;

package CACHE_LCA is

	type T_CACHE_LCA is limited private;

	-- Initialiser le cache.  Le cache est vide.
	procedure Initialiser(Cache_lca: out T_CACHE_LCA) with
		Post => Cache_lca == null;

	-- Est-ce que le cahce est plein ?
   function Est_Plein(Cache_lca : T_CACHE_LCA) return Boolean;

   -- Savoir si une adresse est présente dans le cache.
	function Adresse_Presente(Cache_lca : in T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return Boolean;

   -- Ajouter une nouvelle route dans le cache.
   procedure Ajouter(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP ; Eth : in T_IFACE) with
     Pre => 1==1;

   -- Supprimer tous les éléments du cache.
	procedure Vider (Cache_lca : in out T_CACHE_LCA) with
		Post => Cache_lca == null;

private

   type T_Cellule;

   type T_Politique is Character;

	type T_CACHE_LCA is access T_Cellule;

	type T_Cellule is
      record
			Adresse : T_ADRESSE_IP;
         Masque : T_ADRESSE_IP;
         Eth : T_IFACE;
			Suivant : T_CACHE_LCA;
		end record;

end CACHE_LCA;
