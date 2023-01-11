with tools; use tools;
with Table_Routage; use Table_Routage;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package CACHE_LCA is

   TAILLE_MAX : Integer;

	type T_CACHE_LCA is limited private;

	-- Initialiser le cache.  Le cache est vide.
	procedure Initialiser(Cache_lca: out T_CACHE_LCA ; Taille : Integer) with
		Post => Est_Vide(Cache_lca);

	-- Est-ce que le cache est plein ?
   function Est_Plein(Cache_lca : T_CACHE_LCA) return Boolean;

   -- Supprimer un élément du cache, suivant la politique demandée au préalable par l'utilisateur.
   procedure Supprimer(Cache_lca : in out T_CACHE_LCA ; Politique : T_Politique);

   -- Savoir si une adresse est présente dans le cache.
   function Adresse_Presente(Cache_lca : in T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return Boolean;

   -- Récupérer le masque et l'interface associés à l'adresse demandée, dans le cache.
   procedure Recuperer(Cache_lca : in T_CACHE_LCA ; Adresse : T_ADRESSE_IP);

   -- Trouver l'adresse à mettre dans le cache.
   procedure Trouver(Table_Routage : T_Table_Routage ; Adresse : T_ADRESSE_IP);

   -- Récupérer le masque le plus long dans la table de routage.
   procedure Recuperer_Masque_Long(Table_Routage : in T_Table_Routage ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP);

   -- Ajouter une nouvelle route dans le cache.
   procedure Ajouter(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP ; Eth : in T_IFACE) with
     Pre => 1=1;

   -- Supprimer tous les éléments du cache.
	procedure Vider(Cache_lca : in out T_CACHE_LCA) with
     Post => Est_Vide(Cache_lca);

   -- Fontion qui indique si le cache est vide ou non.
   function Est_Vide(Cache_lca : in T_CACHE_LCA) return Boolean;

   -- Taille du cache.
   function Taille(Cache_lca : T_CACHE_LCA) return Integer with
     Post => Taille'Result >= 0
       and (Taille'Result = 0) = Est_Vide(Cache_lca);

private

   type T_Cellule;

	type T_CACHE_LCA is access T_Cellule;

	type T_Cellule is
      record
			Adresse : T_ADRESSE_IP;
         Masque : T_ADRESSE_IP;
         Eth : Unbounded_String;
         Frequence : Integer;
         Recent : Integer;
			Suivant : T_CACHE_LCA;
		end record;

end CACHE_LCA;
