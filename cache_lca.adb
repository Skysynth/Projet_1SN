with Ada.Text_IO;           use Ada.Text_IO;
-- with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
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

   function Est_Vide(Cache_lca : T_CACHE_LCA) return Boolean is
	begin
		return Cache_lca = null;
   end;

   procedure Ajouter(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Table_routage : in T_TABLE_ROUTAGE) is
   begin
      if Cache_Plein then
         Supprimer(Cache_lca);
      else
         null;
      end if;
      if not Adresse_Presente(Cache_lca, Adresse) then
         Ajouter_Adresse(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Table_routage : in T_TABLE_ROUTAGE);
      else
         null;
      end if;
   end Ajouter;

   procedure Adresse_Presente(Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) is
   begin
   end Adresse_Presente;

   procedure Cache_Plein(Cache_lca : in T_CACHE_LCA) is
      Plein : Boolean;
   begin
      Plein := False
      if Taille(Cache_lca) > Taille_Max_Cache then
         Plein := True;
      else
         null;
      end if;
   end Cache_Plein;

   procedure Supprimer(Cache_lca : T_CACHE_LCA) is
   begin
      case expression is
         when =>;
            when =>;
            when =>;
            when others =>;
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

   procedure Ajouter_Adresse(Cache_lca : T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Table_routage : in T_TABLE_ROUTAGE) is
   begin
      if Cache_lca = null then
         Cache_lca := new T_Cellule'(Adresse, Table_routage.Masque, Table_routage.Iface, null);
      else
         Enregistrer(Cache_lca.all.Suivant, Adresse, Table_routage);
      end if;
   end Ajouter_Adresse;
