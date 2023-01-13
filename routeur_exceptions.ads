-- Définition d'une exception commune à toutes les SDA.

package Routeur_Exceptions is

    Option_non_valide_exception : Exception;	-- Une option n'est pas valide
    Argument_non_valide_exception : Exception;	-- Un argument n'est pas valide
    Adresse_Absente_Exception : Exception;      -- Une adresse invalide 
    Politique_non_valide_exception : Exception; -- La politique demandée est invalide
    Arbre_Vide_Exception : Exception; -- L'arbre est vide
    Suppression_Exception : Exception; -- L'élément à supprimer n'est pas trouvé
    -- Adresse_Redondante_Exception : Exception; -- L'adresse est déjà présente dans le cache

    COMMAND_FIN_CALLED : Exception;

end Routeur_Exceptions;
