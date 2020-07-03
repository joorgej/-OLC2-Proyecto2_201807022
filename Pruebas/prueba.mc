int cuanto_sacare_en_mi_proyecto(int nota){
    printf("Sacaras\n");
    return nota + 10;
}
int main(){
    int b = cuanto_sacare_en_mi_proyecto(100);
    printf(b);
}