main:
        $ra = array();
        $a0 = 0;
        goto _main_;

cuanto_sacare_en_mi_proyecto:
        print("Sacaras\n");
        $t0 = $a0+10;
        $s0 = $t0;
        goto cuanto_sacare_en_mi_proyecto_return_call;
        goto cuanto_sacare_en_mi_proyecto_return_call;

_main_:
        $a0 = 100;
        $ra[0] = 0;
        goto cuanto_sacare_en_mi_proyecto;

cuanto_sacare_en_mi_proyecto_call_0:
        $t1 = $s0;
        print("t");
        exit;

cuanto_sacare_en_mi_proyecto_return_call:
        if ($ra[0] == 0) goto cuanto_sacare_en_mi_proyecto_call_0;