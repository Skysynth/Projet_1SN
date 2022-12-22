with Ada.Text_IO;           use Ada.Text_IO;
-- with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with routeur_exceptions;    use routeur_exceptions;
with tools; use tools;
with Ada.Unchecked_Deallocation;

package body CACHE_LCA is

    TAILLE_MAX_CACHE : Integer := 0;
    POLITIQUE : T_Politique := FIFO;

    procedure Free is
        new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_CACHE_LCA);

    procedure Initialiser_Cache(taille_max : in Integer; politique_cache : in T_Politique; Cache_lca: out T_CACHE_LCA) is
    begin
        TAILLE_MAX_CACHE := taille_max;
        POLITIQUE := politique_cache;
        Cache_lca := Null;
    end Initialiser_Cache;

    function Est_Vide (Cache_lca : T_CACHE_LCA) return Boolean is
    begin
        return Cache_lca = null;
    end;

    function Taille (Cache_lca : in T_CACHE_LCA) return Integer is
        result : Integer;
        Cache_lca_temp : T_CACHE_LCA;
    begin
        result := 0;
        Cache_lca_temp := Cache_lca;
        while not(Est_Vide(Cache_lca_temp)) loop
            result := result +1;
            Cache_lca_temp := Cache_lca_temp.all.Suivant;
        end loop;

        return result;
    end Taille;


    procedure Enregistrer (Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP ; Iface : String) is
    begin
        if Cache_lca = null then
            Cache_lca := new T_Cellule'(Adresse, Masque, Iface, null);
        else
            if Taille(Cache_lca) >= TAILLE_MAX_CACHE then
                Supprimer_Un_Element(Cache_lca): -- Supprime un élément du cache en fonction de la politique
            else
                if Cache_lca.all.Adresse = Adresse then
                    Cache_lca.all.Masque := Masque;
                    Cache_lca.all.Iface := Iface;
                else
                    Enregistrer (Cache_lca.all.Suivant, Adresse, Masque, Iface);
                end if;
            end if;


        end if;
    end Enregistrer;


    procedure Supprimer (Cache_lca : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) is
        Cache_lca0 : T_CACHE_LCA;
    begin
        if Cache_lca = null then
            raise Adresse_Absente_Exception with "L'adresse n'est pas présente2";
        elsif Cache_lca.all.Adresse = Adresse then
            Cache_lca0 := Cache_lca;
            Cache_lca := Cache_lca.all.Suivant;
            Free(Cache_lca0);
        else
            Supprimer(Cache_lca.all.Suivant, Adresse);
        end if;
    end Supprimer;


    procedure Vider (Cache_lca : in out T_CACHE_LCA) is
    begin
        if Cache_lca /= Null then
            Vider(Cache_lca.all.Suivant);
            Free(Cache_lca);
        end if;
    end Vider;

    function Adresse_Presente (Cache_lca : in T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return Boolean is
        Cache_lca0 : T_LCA;
    begin
        Cache_lca0 := Cache_lca;
        while Cache_lca0 /= null loop
            if Cache_lca0.all.Adresse = Adresse then
                return True;
            else
                null;
            end if;
            Cache_lca0 := Cache_lca0.all.Suivant;
        end loop;
        return False;
    end;

    function Le_Masque (Cache_lca : in T_CACHE_LCA ; Masque : in T_Masque) return T_Masque is
    begin
        if Cache_lca = null then
            raise Adresse_Absente_Exception with "L'adresse n'est pas présente";
        elsif Cache_lca.all.Masque = Masque then
            return Cache_lca.all.Masque;
        else
            return Le_Masque(Cache_lca.all.Suivant, Masque);
        end if;
    end Le_Masque;


end CACHE_LCA;
