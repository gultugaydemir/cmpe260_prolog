% zeynep gultug aydemir
% 2019502276
% compiling: yes
% complete: yes

% include the knowledge base
:- ['load.pro'].


% 3.1 glanian_distance(Name1, Name2, Distance) 5 points

isNegativeThenZero(A,B):-
    (A<0 ,B = 0);
    (A>=0, B = A).

distance(A,B,C):-
    sumOfDiff(A,B,D),
    C is sqrt(D).


glanian_distance(Name1, Name2, Distance):-
    expects(Name1,_,Expected),
    glanian(Name2,_,Found),
    distance(Expected,Found,Result),
    Distance is Result.

sumOfDiff([],[],0).

sumOfDiff([-1|T1],[H2|T2],C):-
    sumOfDiff(T1,T2,C).

sumOfDiff([H1|T1],[H2|T2],C):-
    sumOfDiff(T1,T2,Crest),
    C is (H1-H2)*(H1-H2)+ Crest.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3.2 weighted_glanian_distance(Name1, Name2, Distance) 10 points

wdistance(A,B,W,C):-
    wsumOfDiff(A,B,W,D),
    C is sqrt(D).

wsumOfDiff([],[],[],0).

wsumOfDiff([H1|T1],[H2|T2],[W1|WR],C):-
    wsumOfDiff(T1,T2,WR,Crest),
    ((H1 >=0,W1>=0,    C is W1*(H1-H2)*(H1-H2)+ Crest);
    (H1 < 0 ,  C is  Crest);
    (W1 < 0,   C is  Crest)).

weighted_glanian_distance(Name1, Name2, Distance):-
    expects(Name1,_,Expected),
    glanian(Name2,_,Found),
    weight(Name1,W),
    wdistance(Expected,Found,W,Result),
    Distance is Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3.3 find_possible_cities(Name, CityList) 5 points

habitantof(Name,City):-
    city(C,A,_),
    member(Name,A),
    City = C.
find_possible_cities(Name,Cities):-
    habitantof(Name,City),
    likes(Name,_,D),
    union([City],D,L),
    Cities = L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3.4 merge_possible_cities(Name1, Name2, MergedCities) 5 points

merge_possible_cities(Name1,Name2,CityList):-
    find_possible_cities(Name1,Cities1),
    find_possible_cities(Name2,Cities2),
    union(Cities1,Cities2,L),
    CityList = L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3.5 find_mutual_activities(Name1, Name2, MutualActivities) 5 points

find_mutual_activities(Name1,Name2,ActivityList):-
    likes(Name1,A1,_),
    likes(Name2,A2,_),
    intersection(A1,A2,L),
    ActivityList = L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3.6 find_possible_targets(Name, Distances, TargetList) 10 points

/*
pair_key_values(Key-Value,Key,Value).
pair_keys_values([],[],[]).
pair_keys_values([H|T],L1,L2):-
    pair_key_values(H,K,V),
    pair_keys_values(T,L1rest,L2rest),
    L1 = [K|L1rest],
    L2 = [V|L2rest].
*/
check_gender_compatibility(Name1,Name2):-
    glanian(Name2,A,_),
    expects(Name1,B,_),
    member(A,B).
possible_target(Name1,Name2,Distance):-
    check_gender_compatibility(Name1,Name2),
    glanian_distance(Name1,Name2,Distance).
find_possible_targets(Name,Distances,TargetList):-
    setof(Distance-Y, possible_target(Name,Y,Distance), Set),
    pairs_keys_values(Set,L1,L2),
    TargetList = L2,
    Distances = L1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3.7 find_weighted_targets(Name, Distances, TargetList) 15 points

weighted_possible_target(Name1,Name2,Distance):-
    check_gender_compatibility(Name1,Name2),
    weighted_glanian_distance(Name1,Name2,Distance).
find_weighted_targets(Name,Distances,TargetList):-
    setof(Distance-Y, weighted_possible_target(Name,Y,Distance), Set),
    pairs_keys_values(Set,L1,L2),
    TargetList = L2,
    Distances = L1.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3.8 find_my_best_target(Name, Distances, Activities, Cities, Targets) 20 points

relation_check(Name1,Name2):-
    old_relation([Name1,Name2]).
relation_check(Name1,Name2):-
    old_relation([Name2,Name1]).

city_check(Name1,Name2,City):-
    find_possible_cities(Name1,Cities),
    member(City,Cities).
city_check(Name1,Name2,City):-
    find_possible_cities(Name2,CityList2),
    likes(Name1,Activities,_),
    member(A,Activities),
    member(B,CityList2),
    dislikes(Name1,_,DislikedCities,__),
    not(member(B,DislikedCities)),
    city(B,_,ActivitiesOfCity),
    member(A,ActivitiesOfCity),
    City = B.







give_activities(Name1,City,A):-
    find_possible_cities(Name1,Mycities),
    member(City,Mycities),
    city(City,_,ActsInCity),
    member(A,ActsInCity),
    dislikes(Name1,Disliked,_,__),
    not(member(A,Disliked)).
    


check_tolerance(Name1,Name2):-
    dislikes(Name1,_,_,FeatureTolerance),
    glanian(Name2,_,Features),
    check_tolerance_recurs(FeatureTolerance,Features).

check_tolerance_recurs([],[]).
check_tolerance_recurs([H1|T1],[H2|T2]):-
    inScope(H1,H2),
    check_tolerance_recurs(T1,T2).
check_tolerance_recurs([[]|T1],[H2|T2]):-
    
    check_tolerance_recurs(T1,T2).


inScope([Low,High],Val):-
    Val > Low,Val < High.




activity_check(Name1,Name2):-
    dislikes(Name1,Disliked,_,_),
    likes(Name2,Liked,_),
    intersection(Disliked,Liked,L),
    length(L,Len),
    Len < 3.



tuple_values(Distance-Target-Activity-City,Distance,Target,Activity,City).
tuples_values([],[],[],[],[]).
tuples_values([H|T],Distances,Targets,Activities,Cities):-
    tuple_values(H,Distance,Target,Activity,City),
    tuples_values(T,DistancesRest,TargetsRest,ActivitiesRest,CitiesRest),
    Distances = [Distance|DistancesRest],
    Targets = [Target|TargetsRest],
    Activities = [Activity|ActivitiesRest],
    Cities = [City|CitiesRest].



r_find_my_best_target(Name,Target,Distance,City,Act):-
    
    weighted_possible_target(Name,Name2,Distance),      %6
    check_tolerance(Name,Name2),                        %7
    not(relation_check(Name,Name2)),                    %1
    activity_check(Name,Name2),                         %8
    Target = Name2,
    city_check(Name,Target,City),
    give_activities(Name,City,Act).

find_my_best_target(Name,Distances,ActivityList,CityList,Targets):-
    setof(Distance-Target-Act-City, r_find_my_best_target(Name,Target,Distance,City,Act), Set),
    sort(1, @=<, Set, Sorted),
    tuples_values(Sorted,Distances,Targets,ActivityList,CityList).

    
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3.9 find_my_best_match(Name, Distances, Activities, Cities, Targets) 25 points


weighted_possible_match(Name1,Name2,Distance):-
    check_gender_compatibility(Name1,Name2),
    check_gender_compatibility(Name2,Name1),
    weighted_glanian_distance(Name1,Name2,Distance1),
    weighted_glanian_distance(Name2,Name1,Distance2),
    Distance is Distance1/2 + Distance2/2.

give_activities2(Name1,Name2,City,A):-
    find_possible_cities(Name1,Mycities),
    member(City,Mycities),
    city(City,_,ActsInCity),
    member(A,ActsInCity),
    dislikes(Name1,Disliked,_,_),
    dislikes(Name2,Disliked2,_,_),
    not(member(A,Disliked)),
    not(member(A,Disliked2)).
give_activities3(Name,Name2,City,A):-
    give_activities(Name,City,A).
give_activities3(Name,Name2,City,A):-
    give_activities(Name2,City,A).
city_check2(Name,Target,City):-
    city_check(Name,Target,City).
city_check2(Name,Target,City):-
    city_check(Target,Name,City).
r_find_my_best_match(Name,Target,Distance,City,Act):-
    
    weighted_possible_match(Name,Name2,Distance),
    
    check_tolerance(Name,Name2),                        %7
    check_tolerance(Name2,Name),                        %7
    not(relation_check(Name,Name2)),                    %1
    activity_check(Name,Name2),                         %8
    activity_check2(Name2,Name),                         %8
    Target = Name2,


    city_check(Name,Target,City),
    city_check(Target,Name,City),
    
    give_activities3(Name,Name2,City,Act).
    

activity_check2(Name1,Name2):-
    dislikes(Name1,Disliked,_,_),
    likes(Name2,Liked,_),
    intersection(Disliked,Liked,L),
    length(L,Len),
    Len < 2.

find_my_best_match(Name,Distances,ActivityList,CityList,Targets):-
    setof(Distance-Target-Act-City, r_find_my_best_match(Name,Target,Distance,City,Act), Set),
    sort(1, @=<, Set, Sorted),
    tuples_values(Sorted,Distances,Targets,ActivityList,CityList).
