test_matrix = [[ 1 , 2 , 0 ],
     [ 2 , 4 , 5 ],
     [ 7 , 0 , 1 ]]
def down(matrix):
    matrix[1:]

def right(matrix):
    for i in range(len(matrix)):
        matrix[i] = matrix[i][1:]


def max_cheese(matrix):
    
    def vmesni(matrix, i, j):

        if i == len(matrix) and j == len(matrix[0]):
            return matrix[i][j]
        elif i + 1 > len(matrix):
            return 0
        elif j + 1 > len(matrix[0]):
            return 0
        else:
            return matrix[i][j] + max(vmesni(matrix, i + 1, j), vmesni(matrix, i, j + 1))
    return vmesni(matrix, 0, 0)

    
articles = [
	("yoghurt", 0.39, 0.18),
	("milk", 0.89, 1.03),
  ("coffee", 2.19, 0.2),
  ("butter", 1.49, 0.25),
  ("yeast", 0.22, 0.042),
  ("eggs", 2.39, 0.69),
  ("sausage", 3.76, 0.50),
  ("bread", 2.99, 1.0),
  ("Nutella", 4.99, 0.75),
  ("juice", 1.15, 2.0)
]

def best_value(articles, max_w):

    def best_val(w):
        options = []
        for item in articles:
            (name, price, weight) = item
            if w - weight < 0:
                pass
            else:
                option = best_val(w - weight) + price
                options.append(option)
        
        if options:
            return max(options)
        else:
            return 0

    return best_val(max_w)

   

         