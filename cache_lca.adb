with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with routeur_exceptions;    use routeur_exceptions;
with tools; use tools;
with Ada.Unchecked_Deallocation;

package body CACHE_LCA is

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_CACHE_LCA);

   procedure Initialiser(Cache_lca: out T_CACHE_LCA ; Taille : Integer) is
	begin
      Cache_lca := null;
      TAILLE_MAX := Taille;
   end Initialiser;

   function Est_Plein(Cache_lca : in T_CACHE_LCA) return Boolean is
      Cache_lca0 : T_CACHE_LCA;
      n : integer;
   begin
      n := 0;
      Cache_lca0 := Cache_lca;
      while Cache_lca0 /= null loop
         Cache_lca0 := Cache_lca0.all.Suivant;
         n := n + 1;
      end loop;
      return n = TAILLE_MAX;
   end Est_Plein;

   procedure Supprimer(Cache_lca : in out T_CACHE_LCA ; Politique : T_Politique) is
   begin
      case Politique is
         when FIFO => Supprimer_FIFO(Cache_lca);
         when LRU => Supprimer_LRU(Cache_lca);
         when LFU => Supprimer_LFU(Cache_lca);
         when others => raise Politique_non_valide_exception;
      end case;
   end Supprimer;

   procedure Supprimer_FIFO(Cache_lca : in out T_CACHE_LCA) is
      Cache_lca0 : T_CACHE_LCA;
   begin
      Cache_lca0 := Cache_lca;
      Cache_lca := Cache_lca.all.Suivant;
      Free(Cache_lca0);
   end Supprimer_FIFO;

   procedure Supprimer_LRU(Cache_lca : in out T_CACHE_LCA) is
   begin
      Free(Cache_lca);
   end Supprimer_LRU;

   function Adresse_LFU(Cache_lca : in T_CACHE_LCA) return integer is
      Cache_lca0 : T_CACHE_LCA;
      Freq_min : integer;
   begin
      Cache_lca0 := Cache_lca;
      Freq_min := 1;
      while not(Est_Vide(Cache_lca0)) loop
         if Cache_lca0.all.Frequence < Freq_min then
            Freq_min := Cache_lca0.all.Frequence;
         else
            null;
         end if;
         Cache_lca0 := Cache_lca0.all.Suivant;
      end loop;
      return Freq_min;
   end Adresse_LFU;

   procedure Supprimer_LFU(Cache_lca : in out T_CACHE_LCA) is
      Freq_min : integer;
      Cache_lca0 : T_CACHE_LCA;
   begin
      Freq_min := Adresse_LFU(Cache_lca);
      while Cache_lca.all.Frequence /= Freq_min loop
         Cache_lca := Cache_lca.all.Suivant;
      end loop;
      Cache_lca0 := Cache_lca;
      Cache_lca := Cache_lca.all.Suivant;
      Free(Cache_lca0);
   end Supprimer_LFU;

   function Adresse_Presente(Cache_lca : in T_CACHE_LCA ; Adresse : in T_Adresse_IP) return Boolean is
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
      Eth_Cache : Unbounded_String;
   begin
      Cache_lca0 := Cache_lca;
      Masque_Max := 0;
      while Cache_lca0 /= null loop
         Adresse_Masquee := Adresse AND Cache_lca0.all.Masque;
         if Cache_lca0.all.Adresse = Adresse_Masquee and then Cache_lca0.all.Masque > Masque_Max then
            Masque_Max := Cache_lca0.all.Masque;
            Eth_Cache := Cache_lca0.all.Eth;
            Cache_lca0.all.Frequence := Cache_lca0.all.Frequence + 1;
         else
            null;
         end if;
         Cache_lca0 := Cache_lca0.all.Suivant;
      end loop;
   end Recuperer;

   procedure Ajouter(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP ; Eth : in Unbounded_String) is
   begin
      if Est_Vide(Cache_lca) then
         Cache_lca := new T_Cellule'(Adresse, Masque, Eth, 1, Null);
      else
         Ajouter(Cache_lca.all.Suivant, Adresse, Masque, Eth);
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

   function Est_Vide(Cache_lca : in T_CACHE_LCA) return Boolean is
   begin
      return Cache_lca = null;
   end Est_Vide;

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
