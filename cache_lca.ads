with tools; use tools;
with Table_Routage; use Table_Routage;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package CACHE_LCA is

   TAILLE_MAX : Integer;

   type T_RECENT_LCA is limited private;

	type T_CACHE_LCA is limited private;

	-- Initialiser le cache.  Le cache est vide.
	procedure Initialiser(Cache_lca: out T_CACHE_LCA ; Taille : Integer) with
		Post => Est_Vide(Cache_lca);

	-- Le cache est-il plein ?
   function Est_Plein(Cache_lca : T_CACHE_LCA) return Boolean;

   -- Supprimer un �l�ment du cache, suivant la politique demand�e au pr�alable par l'utilisateur.
   procedure Supprimer(Cache_lca : in out T_CACHE_LCA ; Politique : T_Politique);

   -- Supprime un �l�ment suivant la politique FIFO.
   procedure Supprimer_FIFO(Cache_lca : in out T_CACHE_LCA);

   -- Ajouter une adresse � la liste cha�n�e R�cente.
   procedure Ajouter_Recent(Rec_lca : in out T_RECENT_LCA ; Adresse : in T_ADRESSE_IP);

   -- Supprimer une adresse � la liste cha�n�e R�cente.
   procedure Supprimer_Recent(Rec_lca : in out T_RECENT_LCA ; Adresse : in T_ADRESSE_IP);

   -- Supprime un �l�ment suivant la politique LRU.
   procedure Supprimer_LRU(Cache_lca : in out T_CACHE_LCA);

   -- R�cup�rer la fr�quence minimale d'utilisation des adresses dans le cache.
   function Adresse_LFU(Cache_lca : in T_CACHE_LCA) return integer;

   -- Supprime un �l�ment suivant la politique LFU.
   procedure Supprimer_LFU(Cache_lca : in out T_CACHE_LCA);

   -- Savoir si une adresse est pr�sente dans le cache.
   function Adresse_Presente(Cache_lca : in T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return Boolean;

   -- R�cup�rer le masque et l'interface associ�s � l'adresse demand�e, dans le cache.
   procedure Recuperer(Cache_lca : in out T_CACHE_LCA ; Adresse : T_ADRESSE_IP);

   -- Ajouter une nouvelle route dans le cache.
   procedure Ajouter(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP ; Eth : in Unbounded_String);

   -- Supprimer tous les �l�ments du cache.
	procedure Vider(Cache_lca : in out T_CACHE_LCA) with
     Post => Est_Vide(Cache_lca);

   -- Le cache est-il vide ?
   function Est_Vide(Cache_lca : in T_CACHE_LCA) return Boolean;

   -- Renvoie la taille du cache.
   function Taille(Cache_lca : T_CACHE_LCA) return Integer with
     Post => Taille'Result >= 0
       and (Taille'Result = 0) = Est_Vide(Cache_lca);

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
