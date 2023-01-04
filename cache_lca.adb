with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with routeur_exceptions;    use routeur_exceptions;
with tools; use tools;
with Ada.Unchecked_Deallocation;

package body CACHE_LCA is

begin

   procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_CACHE_LCA);

	procedure Initialiser(Cache_lca: out T_CACHE_LCA) is
	begin
		Cache_lca := null;
   end Initialiser;

   function Est_Plein(Cache_lca : T_CACHE_LCA) return Boolean is
	begin
		return Cache_lca = null;
   end;

   function Adresse_Presente(Cache_lca : in T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return Boolean is
   begin
      return 1;
   end;

   procedure Ajouter(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP ; Eth : in T_IFACE) is
   begin
      return 2;
   end;

   procedure Supprimer(Cache_lca : in out T_CACHE_LCA, Politique : String) is
   begin
      case Politique is
         when 'FIFO' => Supprimer_FIFO(Cache_lca : in out T_CACHE_LCA);
         when 'LRU' => Supprimer_LRU(Cache_lca : in out T_CACHE_LCA);
         when others => Supprimer_LFU(Cache_lca : in out T_CACHE_LCA);
      end case;
   end Supprimer;

   function Taille (Cache_lca : in T_CACHE_LCA) return Integer is
      n : integer;
      Cache_lca0 : T_CACHE_LCA;
   begin
      n := 0;
      Cache_lca0 := Cache_lca;
      while Cache_lca0 /= null loop
         Cache_lca0 := Cache_lca0.all.Suivant;
         n := n + 1;
      end loop;
      return n;
   end Taille;

end CACHE_LCA;
