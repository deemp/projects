main(A, B, C) :- 
    findall(X, (member(X,A), member(X,B)), CList),
    list_to_set(CList, CSet),
    findall(X, (member(X, CSet)), C)
    . 
