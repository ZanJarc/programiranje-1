##############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end].
#
# Primer: za [start = 0] in [end = 8] tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
##############################################################################
def pivot(a, start, end):
    element = a[start]
    for i in range(start, end + 1):
        if a[i] > element:
            for j in range(i + 1, end + 1):
                if a[j] < element:
                    a[i], a[j] = a[j], a[i]
    #na tej tocki imamo ze nekako urejen seznam
    for i in range(start, end + 1):
        if element < a[i]:
            #prvic pridemo do elementa, ki je vecji od pivota
            a[start], a[i- 1] = a[i - 1], a[start]
            return i - 1
        
    a[start], a[end] =  a[end], a[start]            
    return end


##############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#   >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#   >>> quicksort(a)
#   [2, 3, 4, 5, 10, 11, 15, 17, 18]
##############################################################################
def swamp(a, i, j):
    a[i], a[j] = a[j], a[i]

def quicksort_part(a, start, end):
    #a je moja cela tabela, a[start] je pivot, a[end] pa zadnji element dela tabele, ki jo moramo urediti
    #vsak element je tukaj manjsi od pivota
    if start >= end:
        return a
    else:
        p = pivot(a, start, end)
        quicksort_part(a, start, p-1)
        quicksort_part(a, p+1, end)
        return a

def quicksort(a):
    return quicksort_part(a, 0, len(a) -1 )
     

##############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti
# element po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da
# jo rešite brez da v celoti uredite tabelo [a].
##############################################################################
def kth_element(a, k):
    element = pivot(a, 0, len(a) - 1)
    if element == k: #iskani element je ravno nas zacetni pivot
        return a[element]
    elif element > k: #iskani element je levo od pivota
        nov_seznam = a[:element]
        quicksort(nov_seznam)
        return nov_seznam[k]
    else:
        nov_seznam = a[(element + 1):]
        quicksort(nov_seznam)
        return nov_seznam[(k - element - 1)]