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

   function Est_Plein(Cache_lca : in T_CACHE_LCA) return Boolean is
      Cache_lca0 : T_CACHE_LCA;
      n : integer;
   begin
      n := 0;
      Cache_lca0 := Cache_lca;
      while Cache_lca0 /= null loop
         Cache_lca0 := Cache_lca0.all.Suivant;
         n += 1;
      end loop;
		return n = Taille_Max;
   end Est_Plein;

   function Adresse_Presente (Cache_lca : in T_CACHE_LCA ; Adresse : in T_Adresse_IP) return Boolean is
      Presence : Boolean;
   begin
      if Cache_lca = null then
         Presence := False;
      else
         if Cache_lca.all.Adresse = Adresse then
            Presence := True;
         else
            return Adresse_Presente(Cache_lca.all.Suivant, Adresse);
         end if;
      end if;
      return Presence;
   end Adresse_Presente;

   procedure Ajouter(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP ; Eth : in T_IFACE) is
   begin
      if Cache_lca = null then
         Cache_lca := new T_Cellule'(Adresse, Masque, Eth);
      else
         Ajouter(Cache_lca.all.Suivant);
         end if;
   end Ajouter;

   procedure Vider(Cache_lca : in out T_CACHE_LCA) is
   begin
      if Cache_lca = null then
         null;
      else
         Free(Cache_lca);
         Vider(Cache_lca.all.Suivant);
      end if;
   end Vider;

   procedure Supprimer(Cache_lca : in out T_CACHE_LCA, Politique : String) is
   begin
      case Politique is
         when "FIFO" => Supprimer_FIFO(Cache_lca : in out T_CACHE_LCA);
         when "LRU" => Supprimer_LRU(Cache_lca : in out T_CACHE_LCA);
         when "LFU" => Supprimer_LFU(Cache_lca : in out T_CACHE_LCA);
         when others => raise Error_Politique_Unknown;
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
