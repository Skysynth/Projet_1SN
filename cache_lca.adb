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

   function Est_Plein(Cache_lca : in T_CACHE_LCA ; Taille_Max : integer) return Boolean is
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

   procedure Supprimer(Cache_lca : in out T_CACHE_LCA, Politique : String) is
   begin
      case Politique is
         when "FIFO" => Supprimer_FIFO(Cache_lca : in out T_CACHE_LCA);
         when "LRU" => Supprimer_LRU(Cache_lca : in out T_CACHE_LCA);
         when "LFU" => Supprimer_LFU(Cache_lca : in out T_CACHE_LCA);
         when others => raise Error_Politique_Unknown;
      end case;
   end Supprimer;

   function Adresse_Presente (Cache_lca : in T_CACHE_LCA ; Adresse : in T_Adresse_IP) return Boolean is
      Presence : Boolean;
      Adresse_Masquee : T_ADRESSE_IP;
      Cache_lca0 : T_CACHE_LCA;
   begin
      Cache_lca0 := Cache_lca;
      if Cache_lca0 = null then
         Presence := False;
      else
         Adresse_Masquee := Adresse AND Cache_lca0.all.Masque;
         if Cache_lca0.all.Adresse = (Adresse_Masquee) then
            Presence := True;
         else
            return Adresse_Presente(Cache_lca0.all.Suivant, Adresse);
         end if;
      end if;
      return Presence;
   end Adresse_Presente;

   procedure Recuperer(Cache_lca : in T_CACHE_LCA ; Adresse : T_ADRESSE_IP) is
      Cache_lca0 : T_CACHE_LCA;
      Adresse_Masquee : T_ADRESSE_IP;
      Masque_Max : T_ADRESSE_IP;
   begin
      Cache_lca0 := Cache_lca;
      Masque_Max := 0.0.0.0;
      while Cache_lca0 /= null loop
         Adresse_Masquee := Adresse AND Cache_lca0.all.Masque;
         if Cache_lca0.all.Adresse = Adresse_Masquee and then Cache_lca0.all.Masque > Masque_Max then
            Masque_Max := Cache_lca0.all.Masque;
            Eth_Cache := Cache_lca0.all.Eth;
         else
            null;
         end if;
         Cache_lca0 := Cache_lca0.all.Suivant;
      end loop;
   end Recuperer;

   procedure Trouver(Table_Routage : T_Table_Routage ; Adresse : T_ADRESSE_IP) is
      Demande_Route_Masquee : T_ADRESSE_IP;
      Adresse_Cache : T_ADRESSE_IP;
      Masque_Cache : T_ADRESSE_IP;
      Eth_Cache : T_IFACE;
   begin
      while Table_Routage /= null loop
         Demande_Route_Masquee := Table_Routage.all.Masque AND Adresse;
         if Demande_Route_Masquee = Table_Routage.all.Adresse then
            Adresse_Cache := Table_Routage.all.Adresse;
            Masque_Cache := Table_Routage.all.Masque;
            Eth_Cache := Table_Routage.all.Eth;
         else
            null;
         end if;
         Table_Routage := Table_Routage.all.Suivant;
      end loop;
   end Trouver;

   procedure Recuperer_Masque_Long(Table_Routage : in T_Table_Routage ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP) is
      Table_Routage0 : T_Table_Routage;
      Masque_Long : T_ADRESSE_IP;
      Demande_Route_Masquee : T_ADRESSE_IP;
      Adresse_Masquee : T_ADRESSE_IP;
   begin
      Masque_Long := Masque;
      while Table_Routage0 /= null loop
         Demande_Route_Masquee := Adresse AND Masque;
         Adresse_Masquee := Table_Routage0.all.Adresse AND MAsque;
         if Adresse_Masquee = Demande_Route_Masquee and then Table_Routage0.all.Masque > Masque_Long then
            Masque_Long := Table_Routage0.all.Masque
         else
            null;
         end if;
         Table_Routage0 := Table_Routage0.all.Suivant;
      end loop;
   end Recuperer_Masque_Long;

   procedure Ajouter(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP ; Eth : in Unbounded_String) is
   begin
      if Cache_lca = null then
         Cache_lca := new T_Cellule'(Adresse, Masque, 1, 1, Eth);
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
