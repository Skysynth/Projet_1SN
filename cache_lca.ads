generic
	type T_ADRESSE_IP is private;
	type T_IFACE is private;

package CACHE_LCA is

	type T_CACHE_LCA is limited private;

	-- Initialiser une Cache_lca.  La Cache_lca est vide.
	procedure Initialiser(Cache_lca: out T_CACHE_LCA) with
		Post => Est_Vide (Cache_lca);


	-- Est-ce qu'une Cache_lca est vide ?
	function Est_Vide (Cache_lca : T_CACHE_LCA) return Boolean;


	-- Enregistrer une Masque associ�e � une Adresse dans une Cache_lca.
	-- Si la Adresse est d�j� pr�sente dans la Cache_lca, sa Masque est chang�e.
	procedure Enregistrer (Cache_lca : in out T_CACHE_LCA ; Adresse : in T_Adresse_IP ; Masque : in T_Adresse_IP) with
		Post => Adresse_Presente (Cache_lca, Adresse) and (Le_Masque (Cache_lca, Adresse) = Masque)   -- Masque ins�r�e
				and (not (Adresse_Presente (Cache_lca, Adresse)'Old) or Taille (Cache_lca) = Taille (Cache_lca)'Old)
				and (Adresse_Presente (Cache_lca, Adresse)'Old or Taille (Cache_lca) = Taille (Cache_lca)'Old + 1);

	-- Supprimer la Masque associ�e � une Adresse dans une Cache_lca.
	-- Exception : Adresse_Absente_Exception si Adresse n'est pas utilis�e dans la Cache_lca
   procedure Supprimer (Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) with
		Post =>  Taille (Cache_lca) = Taille (Cache_lca)'Old - 1 -- un �l�ment de moins
			and not Adresse_Presente (Cache_lca, Adresse);         -- la Adresse a �t� supprim�e


	-- Savoir si une Adresse est pr�sente dans une Cache_lca.
	function Adresse_Presente (Cache_lca : in T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return Boolean;


	-- Obtenir la Masque associ�e � une Adresse dans la Cache_lca.
	-- Exception : Adresse_Absente_Exception si Adresse n'est pas utilis�e dans l'Cache_lca
	function Le_Masque (Cache_lca : in T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return T_Adresse_IP;


	-- Supprimer tous les �l�ments d'une Cache_lca.
	procedure Vider (Cache_lca : in out T_CACHE_LCA) with
		Post => Est_Vide (Cache_lca);


	-- Appliquer un traitement (Traiter) pour chaque couple d'une Cache_lca.
	generic
		with procedure Traiter (Adresse : in T_ADRESSE_IP; Masque: in T_Adresse_IP);
	procedure Pour_Chaque (Cache_lca : in T_CACHE_LCA);


private

	type T_Cellule;

	type T_CACHE_LCA is access T_Cellule;

	type T_Cellule is
      record
			Adresse : T_ADRESSE_IP;
			Masque : T_Adresse_IP;
			Suivant : T_CACHE_LCA;
		end record;

end LCA;
