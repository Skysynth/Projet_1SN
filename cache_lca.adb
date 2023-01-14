with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with routeur_exceptions;    use routeur_exceptions;
with tools; use tools;
with Ada.Unchecked_Deallocation;

package body CACHE_LCA is

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_CACHE_LCA);

   procedure Free_Rec is
     new Ada.Unchecked_Deallocation (Object => T_Case, Name => T_RECENT_LCA);

   -- Initialiser le cache LCA

   procedure Initialiser(Cache_lca: out T_CACHE_LCA ; Taille : Integer ; pol : T_Politique) is
   begin
      Cache_lca := null;
      TAILLE_MAX := Taille;
      POLITIQUE := pol;
   end Initialiser;

   -- Savoir si le cache est plein ou non

   function Est_Plein(Cache_lca : in T_CACHE_LCA) return Boolean is
   begin
      return Taille(Cache_lca) = TAILLE_MAX;
   end Est_Plein;

   -- Savoir si le cache est vide ou non

   function Est_Vide(Cache_lca : in T_CACHE_LCA) return Boolean is
   begin
      return Cache_lca = null;
   end Est_Vide;


   -- Vider le cache

   procedure Vider(Cache_lca : in out T_CACHE_LCA) is
   begin
      if Cache_lca = null then
         null;
      else
         Free(Cache_lca);
         Vider(Cache_lca.all.Suivant);
      end if;
   end Vider;

   -- Connaitre la taille d'une liste chainee

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

   -- Politique FIFO

   procedure Supprimer_FIFO(Cache_lca : in out T_CACHE_LCA) is
      Cache_lca0 : T_CACHE_LCA;
   begin
      Cache_lca0 := Cache_lca;
      Cache_lca := Cache_lca.all.Suivant;
      Free(Cache_lca0);
   end Supprimer_FIFO;

   -- Politique LRU

   procedure Ajouter_Recent(Rec_lca : in out T_RECENT_LCA ; Adresse : in T_ADRESSE_IP) is
   begin
      if Rec_lca = null then
         Rec_lca := new T_Case'(Adresse, null);
      else
         Ajouter_Recent(Rec_lca.all.Suivant, Adresse);
      end if;
   end Ajouter_Recent;

   procedure Supprimer_Recent(Rec_lca : in out T_RECENT_LCA ; Adresse : in T_ADRESSE_IP) is
      Recent_lca0 : T_RECENT_LCA;
   begin
      if Rec_lca.all.Adresse = Adresse then
         Recent_lca0 := Rec_lca;
         Rec_lca := Rec_lca.all.Suivant;
         Free_Rec(Recent_lca0);
      else
         Supprimer_Recent(Rec_lca.all.Suivant, Adresse);
      end if;
   end Supprimer_Recent;

   procedure Supprimer_LRU(Cache_lca : in out T_CACHE_LCA ; Rec_lca : in out T_RECENT_LCA ; Adresse_Suppr : in T_Adresse_IP) is
      Cache_lca0 : T_CACHE_LCA;
   begin
      if Cache_lca.all.Adresse = Adresse_Suppr then
         Cache_lca0 := Cache_lca;
         Cache_lca := Cache_lca.all.Suivant;
         Free(Cache_lca0);
         Supprimer_Recent(Rec_lca, Adresse_Suppr);
      else
         Supprimer_LRU(Cache_lca.all.Suivant, Rec_lca, Adresse_Suppr);
      end if;
   end Supprimer_LRU;

   -- Politique LFU

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

      if Cache_lca.all.Frequence = Freq_min then
         Cache_lca0 := Cache_lca;
         Cache_lca := Cache_lca.all.Suivant;
         Free(Cache_lca0);
      else
         Supprimer_LFU(Cache_lca.all.Suivant);
      end if;

   end Supprimer_LFU;

   -- Supprimer un element du cache si ce dernier est plein, en suivant une politique particulière

   procedure Supprimer(Cache_lca : in out T_CACHE_LCA) is
   begin
      case POLITIQUE is
         when FIFO => Supprimer_FIFO(Cache_lca);
         when LRU => Supprimer_LRU(Cache_lca, RECENT_LCA, RECENT_LCA.all.Adresse);
         when LFU => Supprimer_LFU(Cache_lca);
         when others => raise Politique_non_valide_exception;
      end case;
   end Supprimer;

   -- Savoir si une adresse est presente ou non dans le cache

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
         if Cache_lca0.all.Adresse = Adresse_Masquee then
            Presence := True;
         else
            return Adresse_Presente(Cache_lca0.all.Suivant, Adresse);
         end if;
      end if;
      return Presence;
   end Adresse_Presente;

   -- Recuperer dans le cache le masque associe a l'adresse demandee.

   function Recuperer_Masque_Cache(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return T_Adresse_IP is
      Masque : T_ADRESSE_IP;
   begin
      Cache_lca := Cache_lca;
      if Cache_lca = null then
         raise Adresse_Absente_Exception;
      elsif Cache_lca.all.Adresse = Adresse then
         Masque := Cache_lca.all.Masque;
         Supprimer_Recent(RECENT_LCA, Adresse);
         Ajouter_Recent(RECENT_LCA, Adresse);
         Cache_lca.all.Frequence := Cache_lca.all.Frequence + 1;
      else
         Masque := Recuperer_Masque_Cache(Cache_lca.all.Suivant, Adresse);
      end if;
      return Masque;
   end Recuperer_Masque_Cache;

   -- Recuperer dans le cache l'interface associee a l'adresse demandee. Null est renvoyé dans le cas contraire.

   function Recuperer_Eth_Cache(Cache : in T_CACHE_LCA ; Adresse : T_Adresse_IP) return Unbounded_String is
      Cache_Temp : T_CACHE_LCA;
   begin
      Cache_Temp := Cache;
      while Cache_Temp /= null loop
         if Is_Equal_With_Mask(Adresse, Cache_Temp.all.Adresse, Cache_Temp.all.Masque) then
            return Cache_Temp.all.Eth;
         else
            Cache_Temp := Cache_Temp.all.Suivant;
         end if;
      end loop;
      raise Adresse_Absente_Exception;
   end Recuperer_Eth_Cache;

   -- Enregistrer une route (adresse, masque et interface) dans le cache

   procedure Enregistrer(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP ; Eth : in Unbounded_String) is
   begin
      if Est_Vide(Cache_lca) then
         Cache_lca := new T_Cellule'(Adresse, Masque, Eth, 1, Null);
         Ajouter_Recent(RECENT_LCA, Adresse);
      else
         Enregistrer(Cache_lca.all.Suivant, Adresse, Masque, Eth);
      end if;
   end Enregistrer;

end CACHE_LCA;
