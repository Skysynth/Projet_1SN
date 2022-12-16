-- Définition d'une exception commune à toutes les SDA.

package Routeur_Exceptions is

    Option_non_valide_exception : Exception;	-- Une option n'est pas valide
    Argument_non_valide_exception : Exception;	-- Un argument n'est pas valide
    Adresse_Absente_Exception : Exception;      -- une adresse invalide 

    COMMAND_FIN_CALLED : Exception;

end Routeur_Exceptions;
