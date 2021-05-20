{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

{-
    PP Project 2021
    This is where you will write the implementation for the given tasks.
    You can add other modules aswell.
-}

{-
    PP Project 2021
    This is where you will write the implementation for the given tasks.
    You can add other modules aswell.
-}

module Tasks where

import Dataset
import Data.List
import Text.Printf

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
    TASK SET 1
-}

-- Task 1
mark_interview :: Row -> Float
mark_interview row =
            (if(row !! 1) == "" then 0 else (read (row !! 1) :: Float)) +
            (if(row !! 2) == "" then 0 else (read (row !! 2) :: Float)) +
            (if(row !! 3) == "" then 0 else (read (row !! 3) :: Float)) +
            (if(row !! 4) == "" then 0 else (read (row !! 4) :: Float)) +
            (if(row !! 5) == "" then 0 else (read (row !! 5) :: Float)) +
            (if(row !! 6) == "" then 0 else (read (row !! 6) :: Float))

mark :: Row -> Float
mark row =
            
                mark_interview row / 4 + (read (row !! 7) :: Float)

student_and_mark :: Row -> Row
student_and_mark row = (row !! 0) : [printf "%.2f" $ (mark row)]

for_student_exam :: (Int, Int, Table, Table) -> Table
for_student_exam (length, it, table, result)
    | it < length = for_student_exam (length, it + 1, table, result ++ [student_and_mark (table !! it)])
    | otherwise = result

compute_exam_grades :: Table -> Table
compute_exam_grades table = ["Nume", "Punctaj Exam"] : for_student_exam ((length table), 1, table, [])

-- Task 2
-- Number of students who have passed the exam:

get_passed_students_num :: Table -> Int
get_passed_students_num = passed_no.compute_exam_grades
            where
                passed_no :: Table -> Int
                passed_no [] = 0
                passed_no (x:xs)
                    | head x == "Nume" = passed_no xs
                    | otherwise = if (read (head(tail x)) :: Float) >= 2.5 then 1 + passed_no xs else passed_no xs

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage table = passed / total
            where
                passed = fromIntegral (get_passed_students_num table) :: Float
                total = fromIntegral (length table - 1) :: Float

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg table =  make_sum (tail (compute_exam_grades table)) / total
            where
                total = fromIntegral (length table - 1) :: Float
                make_sum :: Table -> Float
                make_sum [] = 0
                make_sum (x:xs) = (read (head (tail x)) :: Float) + make_sum xs

-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
get_passed_hw_num = hw_grades.tail
            where
                hw_grades :: Table -> Int
                hw_grades [] = 0
                hw_grades (x:xs) =
                    if
                            (if((x !! 2) == "") then 0 else (read (x !! 2) :: Float)) +
                            (if((x !! 3) == "") then 0 else (read (x !! 3) :: Float)) +
                            (if((x !! 4) == "") then 0 else (read (x !! 4) :: Float)) >= 1.5
                    then 1 + hw_grades xs else hw_grades xs


-- Task 3

tr :: [[a]] -> [[a]]
tr ([]:_)  = []
tr m = map head m : tr (map tail m)

avg_line :: Row -> Float
avg_line line = sum (list_strings_to_floats line) / frac
            where
                frac = fromIntegral (length (tail line)) + 1 :: Float
                list_strings_to_floats :: Row -> [Float]
                list_strings_to_floats [] = []
                list_strings_to_floats (x:xs) = (if x == "" then 0 else (read x :: Float)) : list_strings_to_floats xs

avg_table :: Table -> Row
avg_table table =
                [printf "%.2f" (avg_line (tail (table !! 0)))] ++
                [printf "%.2f" (avg_line (tail (table !! 1)))] ++
                [printf "%.2f" (avg_line (tail (table !! 2)))] ++
                [printf "%.2f" (avg_line (tail (table !! 3)))] ++
                [printf "%.2f" (avg_line (tail (table !! 4)))] ++
                [printf "%.2f" (avg_line (tail (table !! 5)))]

get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs table = ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6"] : [(avg_table (tail(init (tr table))))]



-- Task 4
q_points :: Row -> Row
q_points row = map show (foldr check_value [0, 0, 0] row)
            where
                check_value :: Value -> [Integer] -> [Integer]
                check_value value acc
                    | value == "0" =    [(acc !! 0) + 1, acc !! 1, acc !! 2]
                    | value == "1" =    [acc !! 0, (acc !! 1) + 1, acc !! 2]
                    | value == "2" =    [acc !! 0, acc !! 1, (acc !! 2) + 1]
                    | value == "" =     [(acc !! 0) + 1, acc !! 1, acc !! 2]
                    | otherwise = [acc !! 0, acc !! 1 , acc !! 2]

avg_question :: Table -> Table
avg_question table =
                [head(table !! 0) : q_points (tail (table !! 0))] ++
                [head(table !! 1) : q_points (tail (table !! 1))] ++
                [head(table !! 2) : q_points (tail (table !! 2))] ++
                [head(table !! 3) : q_points (tail (table !! 3))] ++
                [head(table !! 4) : q_points (tail (table !! 4))] ++
                [head(table !! 5) : q_points (tail (table !! 5))]


get_exam_summary :: Table -> Table
get_exam_summary table = ["Q","0","1","2"] : (avg_question (tail (init (tr table))))


-- Task 5
compare_final_mark :: Ord a => [a] -> [a] -> Ordering
compare_final_mark [a1, b1] [a2, b2]
    | b1 < b2 = LT
    | b1 > b2 = GT
    | b1 == b2 = compare a1 a2

get_ranking :: Table -> Table
get_ranking table = ["Nume","Punctaj Exam"] : sortBy compare_final_mark (tail (compute_exam_grades table))

-- Task 6

compute_table :: Table -> Table
compute_table [] = []
compute_table (x:xs) = (create_new_form x) : (compute_table xs)
            where
                create_new_form :: Row -> Row
                create_new_form row = [row !! 0, printf "%.2f" ((mark_interview row) / 4), printf "%.2f" (read (row !! 7) :: Float), printf "%.2f" (abs ((read (row !! 7) :: Float) - (mark_interview row) / 4))]


compare_dif :: Row -> Row -> Ordering
compare_dif [nume1, punctaj_interview1, punctaj_scri1, diferenta1] [nume2, punctaj_interview2, punctaj_scri2, diferenta2]
    | (read diferenta1 :: Float) > (read diferenta2 :: Float) = GT
    | (read diferenta1 :: Float) < (read diferenta2 :: Float) = LT
    | (read diferenta1 :: Float) == (read diferenta2 :: Float) = if nume1 > nume2 then GT else LT

get_exam_diff_table :: Table -> Table
get_exam_diff_table table = ["Nume","Punctaj interviu","Punctaj scris","Diferenta"] : (sortBy compare_dif (compute_table (tail table)))


{-
    TASK SET 2
-}

-- Task Set 2
-- Implement a function which reads a string in CSV-format to a Table and a function which writes a string from a Table.

-- Prerequisite

split_by :: Char -> String -> [String]
split_by del = foldr op [""]
    where
        op c [] = if c == del then [""] else [[c]]
        op ch acc@(y:ys)
            | ch == del = []:acc
            | otherwise = (ch:y):ys

read_csv :: CSV -> Table
read_csv grades = map (split_by ',') (split_by '\n' grades)

write_csv :: Table -> CSV
write_csv table = init (foldr concat_line [] table)
                where
                    concat_line [] acc = "\n" ++ acc
                    concat_line (x:xs) acc
                        | null xs = x ++ concat_line xs acc
                        | otherwise = x ++ "," ++ concat_line xs acc

-- Task 1 - 0.1p
-- Write a function which takes a column name and a Table and returns the values from that column as a list.

get_column_index :: String -> Table -> Int
get_column_index column_name table = head (elemIndices column_name (table !! 0))

as_list :: String -> Table -> [String]
as_list column_name table = tail $ foldr data_by_column [] table
                        where
                            data_by_column el acc = (el !! get_column_index column_name table) : acc


-- Task 2 - 0.1p
-- Write a function which takes a column name and a Table and returns the Table sorted by that column (if multiple entries have the same values, then it is sorted by the first column). Sorting is ascending for columns with numeric values and lexicographical for strings.

compare_lines :: Ord a => Int -> [a] -> [a] -> Ordering
compare_lines index elem1 elem2
    | elem1 !! index > elem2 !! index = GT
    | elem1 !! index < elem2 !! index = LT
    | otherwise = if elem1 !! 0 > elem2 !! 0 then GT else LT


tsort :: String -> Table -> Table
tsort column_name table = (table !! 0) : sortBy (compare_lines (get_column_index column_name table)) (tail table)

-- Task 3 - 0.1p
-- Implement a function which applies a function to all values in a table.

vmap :: (Value -> Value) -> Table -> Table
vmap _ [] = []
vmap op (x:xs) = map op x:vmap op xs

{-
Task 4 - 0.1p
Implement a function which applies a function to all entries (rows) in a table. The new column names are given. Addionally, you have to implement a function which takes a Row from hw_grades Table and returns a Row with 2 values: name, total points (sum of columns 2 - 8).
-}

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap function head_of_table table = head_of_table : map (function) (tail table)

get_hw_grade_total :: Row -> Row
get_hw_grade_total row = (row !! 0) : [(compute_sum row)]
        where
            compute_sum :: Row -> Value
            compute_sum row = printf "%.2f" (
                    (if row !! 2 == "" then 0 else read (row !! 2) :: Float) +
                    (if row !! 3 == "" then 0 else read (row !! 3) :: Float) +
                    (if row !! 4 == "" then 0 else read (row !! 4) :: Float) +
                    (if row !! 5 == "" then 0 else read (row !! 5) :: Float) +
                    (if row !! 6 == "" then 0 else read (row !! 6) :: Float) +
                    (if row !! 7 == "" then 0 else read (row !! 7) :: Float) +
                    (if row !! 8 == "" then 0 else read (row !! 8) :: Float)
                )

{-
Task 5 - 0.1p
Implement a function which takes Tables t1 and t2 and adds all rows from t2 at the end of t1, if column names coincide. If columns names are not the same, t1 remains unchanged.
-}

vunion :: Table -> Table -> Table
vunion t1 t2 = if check_names (head t1) (head t2) then concat t1 t2 else t1
        where
            check_names :: Row -> Row -> Bool
            check_names head1 head2 = null (head1 \\ head2) && null (head2 \\ head1)
            concat :: Table -> Table -> Table
            concat t1 t2 = t1 ++ tail t2

{-
Task 6 - 0.1p
Implement a function which adds new columns to a table (simple horizontal union of 2 tables).
-}

for_each_row :: (Int, Int, Table, Table, Table) -> Table
for_each_row (lungime, it, t1, t2, result)
    | it < lungime = for_each_row (lungime, it + 1, t1, t2,
        result ++ [
            (if it < (length t1) then t1 !! it else []) ++
            (if it < (length t2) then t2 !! it else [])
        ])
    | otherwise = result

fill_spaces :: Int -> Table -> Table
fill_spaces max_length = map (fill max_length)
fill max_length el
    | length el < max_length = el ++ replicate (max_length - length el) ""
    | otherwise = el


hunion :: Table -> Table -> Table
hunion t1 t2 = fill_spaces (length (head t1) + length (head t2)) (for_each_row (max (length t1) (length t2), 0, t1, t2, []))

{-
Task 7 - 0.2p
Implement table join with respect to a key (a column name). If an entry from table 1 has the same value for the key as an entry from table 2, their contents must be merged. If there are other columns with the same name in both tables, then the value from t2 overrides the value from t1, unless it is null (“”).
-}

for_row :: (Int, Int, Row, Row) -> Row
for_row (lgth, it, row, result)
    | it < lgth = for_row (lgth, it + 1, row,
        result ++ [row !! it])
    | otherwise = result


elim_col :: String -> Table -> Table
elim_col key table = foldr (elim_col_row (get_column_index key table)) [] table

elim_col_row :: Int -> Row -> Table -> Table
elim_col_row index el [] = [
    for_row(index, 0, el, []) ++
    for_row(length el, index + 1, el, [])]
elim_col_row index el acc = (for_row(index, 0, el, []) ++
    for_row(length el, index + 1, el, [])) : acc


tjoin :: String -> Table -> Table -> Table
tjoin key t1 t2 = fill_spaces (length (head t1) + length (head t2) - 1) (foldr (joinrow (elim_col key t2) t2
                        (get_column_index key t1)
                        (get_column_index key t2)) [] t1)
        where
            joinrow table_clean table_full index_t1 index_t2 el acc
                | match_key el index_t1 index_t2 table_full = foldr (joinR el index_t1 index_t2) acc table_full
                | otherwise = el : acc
            joinR el1 index_t1 index_t2 el2 acc
                | el1 !! index_t1 == el2 !! index_t2 = (el1 ++ head (elim_col_row index_t2 el2 [])) : acc
                | otherwise = acc
            match_key  _ _ _ [] = False
            match_key el index_t1 index_t2 (x:xs) = ((el !! index_t1) == (x !! index_t2)) || match_key el index_t1 index_t2 xs


{-
Task 8 - 0.1p
Write the implementation for the cartesian product of 2 tables. The new column names are given. 
To generate the entries in the resulting table you have to apply the given operation on each entry in t1 with each entry in t2
-}

apply_cart_row_table :: (Row -> Row -> Row) -> Table -> Row -> Table -> Table
apply_cart_row_table function table row acc = foldr (concat_row function row) acc table
        where
            concat_row function row1 row2 acc = function row1 row2 : acc

cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian fct head_of_table table1 table2 = head_of_table : foldr (apply_cart_row_table fct (tail table2)) [] (tail table1)

{-
Task 9 - 0.1p
Extract from the table only the specified columns.
-}
projection :: [String] -> Table -> Table
projection column_names table = tr (foldr (proj column_names) [] (tr table))
    where
        proj column_names el acc = if (el !! 0) `elem` column_names then el : acc else acc


{-
    TASK SET 3
-}

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query

type EdgeOp = Row -> Row -> Maybe Value

data QResult = CSV CSV | Table Table | List [String]

-- Task 3.1.: Enroll QResult in class Show. For Table, useg write_csv (see task set X). For List and String, use default.

instance Show QResult where
    show (CSV s) = show s
    show (Table t) = write_csv t
    show (List l) = show l

-- Task 3.2.: In order to ensure separation between queries and their evaluation we define class Eval, which offers function eval. Your job is to enroll Query in this class.

calc_acc :: (Eq a1, Num a2) => Int -> [a1] -> [a1] -> a2 -> a2
calc_acc it row1 row2 acc
  | it == 0 = acc
  | ((row1 !! it) == (row2 !! it)) = acc + 1
  | otherwise = acc

for_ai_equals_bi :: Int -> Int -> Row -> Row -> Int -> Int
for_ai_equals_bi it len row1 row2 acc
        | it < len = for_ai_equals_bi (it + 1) len row1 row2 (calc_acc it row1 row2 acc)
        | otherwise = acc

edge_op_3 :: Row -> Row -> Maybe String
edge_op_3 row1 row2
    | head row1 == "" || head row2 == "" = Nothing
    | for_ai_equals_bi 0 (length row1) row1 row2 0 >= 5 = Just $ show (for_ai_equals_bi 0 (length row1) row1 row2 0)
    | otherwise = Nothing

remove_duplicates :: Table -> Table
remove_duplicates = remove_d []
    where remove_d exist [] = exist
          remove_d exist (x:xs)
              | elem x exist = remove_d exist xs
              | otherwise = remove_d (exist ++ [x]) xs

class Eval a where
    eval :: a -> QResult

qResultTable_to_Table :: QResult -> Table
qResultTable_to_Table (Table result) = result

compare_value :: Int -> Row -> Row -> Ordering
compare_value index elem1 elem2
    | ((elem1 !! index) == "" && (elem2 !! index) /= "") = compare 0 (read (elem2 !! index) :: Float)
    | ((elem1 !! index) /= "" && (elem2 !! index) == "") = compare (read (elem1 !! index) :: Float) 0
    | ((elem1 !! index) == "" && (elem2 !! index) == "") = compare (head elem1) (head elem2)
    | (read (elem1 !! index) :: Float) < (read (elem2 !! index) :: Float) = LT
    | (read (elem1 !! index) :: Float) > (read (elem2 !! index) :: Float) = GT
    | otherwise = compare (head elem1) (head elem2)

tsort_new :: String -> Table -> Table
tsort_new column_name table = (table !! 0) : sortBy (compare_value ((maybe (-1) id (elemIndex column_name (table !! 0))))) (tail table)

instance Eval Query where
    eval (FromCSV csv_string) = Table (read_csv csv_string)
    eval (ToCSV query_table) = CSV $ write_csv $ qResultTable_to_Table $ eval query_table
    eval (AsList column_name query_table) = List $ as_list column_name $ qResultTable_to_Table $ eval query_table
    eval (Sort column_name query_table) = Table (tsort_new column_name $ qResultTable_to_Table $ eval query_table)
    eval (ValueMap function query_table) = Table $ vmap function $ qResultTable_to_Table $ eval query_table
    eval (RowMap function list query_table) = Table $ rmap function list $ qResultTable_to_Table $ eval query_table
    eval (VUnion query_table1 query_table2) = Table $ vunion (qResultTable_to_Table $ eval query_table1) (qResultTable_to_Table $ eval query_table2)
    eval (HUnion query_table1 query_table2) = Table $ hunion (qResultTable_to_Table $ eval query_table1) (qResultTable_to_Table $ eval query_table2)
    eval (TableJoin string_list query_table1 query_table2) = Table $ tjoin string_list (qResultTable_to_Table $ eval query_table1) (qResultTable_to_Table $ eval query_table2)
    eval (Cartesian function string_list query_table1 query_table2) = Table $ cartesian function string_list (qResultTable_to_Table $ eval query_table1) (qResultTable_to_Table $ eval query_table2)
    eval (Projection string_list query_table) = Table $ projection string_list $ qResultTable_to_Table $ eval query_table
    eval (Filter fcond query_table) = Table $ (head $ qResultTable_to_Table $ eval query_table) :
            filter (feval (head $ qResultTable_to_Table $ eval query_table) fcond) (tail $ qResultTable_to_Table $ eval query_table)
    eval (Graph edgeop query_table) = Table $ ["From","To","Value"] :
        (remove_duplicates $ foldr (second_foldr edgeop (tail $ qResultTable_to_Table $ eval query_table)) [] $ tail $ qResultTable_to_Table $ eval query_table)
            where
                second_foldr edgeop table row_table acc = foldr (check_values edgeop row_table) acc table
                check_values edgeop row_table1 row_table2 acc
                  | maybe ("nothing") (\x->x) (edgeop row_table1 row_table2) == "nothing" || ((row_table1 !! 0) == (row_table2 !! 0)) = acc
                  | ((row_table1 !! 0) < (row_table2 !! 0)) = ([(row_table1 !! 0)] ++ [(row_table2 !! 0)] ++ [(maybe ("nothing") (\x->x) (edgeop row_table1 row_table2))]) : acc
                  | otherwise = ([(row_table2 !! 0)] ++ [(row_table1 !! 0)] ++ [(maybe ("nothing") (\x->x) (edgeop row_table1 row_table2))]) : acc

similarities_query :: Query
similarities_query = Sort "Value" (Graph edge_op_3 $ FromCSV $ lecture_grades_csv)

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool

class FEval a where
    feval :: [String] -> FilterCondition a -> FilterOp


instance FEval Float where
    feval list_of_strings (Eq str no) = \row -> (read (row !! get_column_index str [list_of_strings]) :: Float) == no
    feval list_of_strings (Lt str no) = \row -> (read (row !! get_column_index str [list_of_strings]) :: Float) < no
    feval list_of_strings (Gt str no) = \row -> (read (row !! get_column_index str [list_of_strings]) :: Float) > no
    feval list_of_strings (In str list_of_values) = \row -> foldr (eval_check_row (row !! get_column_index str [list_of_strings])) True list_of_values
                    where
                        eval_check_row value value_of_list acc
                            | value == "" = acc && False
                            | otherwise = if (read value :: Float) == value_of_list then acc && True else acc && False

instance FEval String where
    feval list_of_strings (Eq str string) = \row -> row !! get_column_index str [list_of_strings] == string
    feval list_of_strings (Lt str string) = \row -> row !! get_column_index str [list_of_strings] < string
    feval list_of_strings (Gt str string) = \row -> row !! get_column_index str [list_of_strings] > string
    feval list_of_strings (FNot (Eq str string)) = \row -> row !! get_column_index str [list_of_strings] /= string
    feval list_of_strings (FieldEq field1 field2) = \row ->
        (row !! get_column_index field1 [list_of_strings] == "" && row !! get_column_index field2 [list_of_strings] == "") || (if (row !! (get_column_index field1 [list_of_strings]) /= "" && row !! (get_column_index field2 [list_of_strings]) == "") then False
                                                                                                                              else if (row !! (get_column_index field1 [list_of_strings]) == "" && row !! (get_column_index field2 [list_of_strings]) /= "") then False
                                                                                                                              else (read (row !! (get_column_index field1 [list_of_strings])) :: Float) == (read (row !! (get_column_index field2 [list_of_strings])) :: Float))


dist :: (Num a1, Ord a1, Eq a2) => [a2] -> [a2] -> a1
dist str1 str2 = last (
            if (length str1 - length str2) == 0 then p_diag 
            else 
                if (length str1 - length str2) > 0 then l_diags !! ((length str1 - length str2) - 1) 
                else u_diags !! (-1 - (length str1 - length str2))
        )
    where
        for_each_diag a [] diags = []
        for_each_diag a (y:ys) (last_diag:diags) = (calculate_diag a ys (head (tail diags)) last_diag) : for_each_diag a ys diags
        calculate_diag a b diag_top diag_low = (1 + head diag_low) : (complete_diag a b (1 + head diag_low) diag_top (tail diag_low))
        complete_diag [] b nw n w = []
        complete_diag (x:xs) (y:ys) nw n w = (
                    if x == y then nw 
                    else 1 + (if (head w) < nw then (head w) else min nw (head n))
                ) : (
                    complete_diag xs ys (
                            if x == y then nw 
                            else 1 + (if (head w) < nw then (head w) else min nw (head n))
                        ) (tail n) (tail w)
                )
        u_diags = for_each_diag str1 str2 (p_diag : u_diags)
        l_diags = for_each_diag str2 str1 (p_diag : l_diags)
        p_diag  = calculate_diag str1 str2 (head u_diags) (-1 : head l_diags)
        

best_match :: (Foldable t, Eq a) => t [a] -> [a] -> [Int]
best_match col el = elemIndices (minimum $ foldr (check el) [] col) (reverse $ foldr (check el) [] col)
    where check el1 el2 acc2 = acc2 ++ [(dist el1 el2)]
                
get_best_element :: Eq a => [[a]] -> [a] -> [[a]] -> [[a]]
get_best_element column_ref element acc = acc ++ [(column_ref !! ((best_match column_ref element) !! 0))]


correct_table :: String -> CSV -> CSV -> CSV
correct_table column_name typos_table ref_table = write_csv $ [["Nume", "Email"]] ++
    (zipWith (\x y -> [x ,y]) (reverse $ foldr (get_best_element (as_list column_name $ read_csv ref_table)) [] (as_list column_name $ read_csv typos_table)) (as_list "Email" $ read_csv typos_table))


hw_points :: Row -> Row
hw_points row = (row !! 0) : [(compute_sum row)]
        where
            compute_sum :: Row -> Value
            compute_sum row = printf "%.2f" (
                    (if row !! 1 == "" then 0 else read (row !! 1) :: Float) +
                    (if row !! 2 == "" then 0 else read (row !! 2) :: Float) +
                    (if row !! 3 == "" then 0 else read (row !! 3) :: Float) +
                    (if row !! 4 == "" then 0 else read (row !! 4) :: Float) +
                    (if row !! 5 == "" then 0 else read (row !! 5) :: Float) +
                    (if row !! 6 == "" then 0 else read (row !! 6) :: Float) +
                    (if row !! 7 == "" then 0 else read (row !! 7) :: Float) +
                    (if row !! 8 == "" then 0 else read (row !! 8) :: Float)
                )

lecture_points :: Row -> Row
lecture_points row = [(compute_sum $ tail row)]
        where
            compute_sum :: Row -> Value
            compute_sum rw = printf "%.2f" (2 * (foldr (check_float_value) 0 rw) / ((fromIntegral $ length rw :: Float)))
            check_float_value value acc = acc + (if value == "" then 0 else read value :: Float)

exam_points :: Row -> Row
exam_points row = [compute_sum row]
        where
            compute_sum :: Row -> Value
            compute_sum row = printf "%.2f" (
                ((if row !! 1 == "" then 0 else read (row !! 1) :: Float) +
                (if row !! 2 == "" then 0 else read (row !! 2) :: Float) +
                (if row !! 3 == "" then 0 else read (row !! 3) :: Float) +
                (if row !! 4 == "" then 0 else read (row !! 4) :: Float) +
                (if row !! 5 == "" then 0 else read (row !! 5) :: Float) +
                (if row !! 6 == "" then 0 else read (row !! 6) :: Float)) / 4 + (if row !! 7 == "" then 0 else read (row !! 7) :: Float))

compare_name :: Row -> Row -> Ordering 
compare_name row1 row2 = compare (row1 !! 0) (row2 !! 0)


grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades email_map_csv hw_grades_csv exam_grades_csv lecture_grades_csv = write_csv $ [["Nume", "Punctaj Teme", "Punctaj Curs", "Punctaj Exam", "Punctaj Total"]] ++ sortBy compare_name (complete_table (read_csv $ correct_table "Nume" email_map_csv hw_grades_csv) (read_csv hw_grades_csv) (read_csv exam_grades_csv) (read_csv lecture_grades_csv))
    where
        complete_table :: Table -> Table -> Table -> Table -> Table
        complete_table t_correct hw_grades_rd exam_grades_rd lecture_grades_rd = foldr (complete_line hw_grades_rd exam_grades_rd lecture_grades_rd) [] $ tail t_correct
        
        complete_line hw_grades_rwd exam_grades_rwd lecture_grades_rwd row acc = acc ++ [
            (punctaj_teme hw_grades_rwd row) ++ 
            (punctaj_curs lecture_grades_rwd row) ++ 
            (punctaj_examen exam_grades_rwd row) 
            ++ (punctaj_total ((punctaj_teme hw_grades_rwd row)) ((punctaj_curs lecture_grades_rwd row)) ((punctaj_examen exam_grades_rwd row)))
            ]
        punctaj_teme hw row = hw_points $ (tail hw) !! ((elemIndices (row !! 0) (as_list "Nume" hw)) !! 0)
        punctaj_curs lecture row = if (elemIndices (row !! 1) (as_list "Email" lecture)) == [] then [""] else (lecture_points $ (tail lecture) !! ((elemIndices (row !! 1) (as_list "Email" lecture)) !! 0))
        punctaj_examen exam row  = if (elemIndices (row !! 0) (as_list "Nume" exam)) == [] then [""] else (exam_points $ (tail exam) !! ((elemIndices (row !! 0) (as_list "Nume" exam)) !! 0))
        punctaj_total hw_mark lecture_mark exam_mark = 
            if ((if hw_mark == [""] then 0 else read (hw_mark !! 1) :: Float) + (if lecture_mark == [""] then 0 else (read (lecture_mark !! 0) :: Float))) < 2.5 || (if exam_mark == [""] then 0 else read (exam_mark !! 0) :: Float) < 2.5 then ["4.00"] 
            else [printf "%.2f" (min ((if hw_mark == [""] then 0 else read (hw_mark !! 1) :: Float) + (if lecture_mark == [""] then 0 else read (lecture_mark !! 0) :: Float)) 5 + (if exam_mark == [""] then 0 else read (exam_mark !! 0) :: Float))]