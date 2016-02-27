CLS 0 13 toplevel:main
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 0 13 toplevel:main
      FUN 2 1 .
        FUN 0 23 execution:init_builtins
        FUN 2 1 .
          FUN 3 5 catch
            FUN 0 4 conv
            VAR 0
            FUN 1 16 handle_exception
              VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 1 16 handle_exception
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 16 handle_exception
        VAR 0
      FUN 2 1 .
        FUN 1 15 io:error_stream
          VAR 1
        FUN 2 1 .
          FUN 1 2 nl
            VAR 1
          FUN 2 1 .
            FUN 2 25 execution:write_exception
              VAR 1
              VAR 0
            FUN 2 1 .
              FUN 1 2 nl
                VAR 1
              FUN 2 1 .
                FUN 1 4 halt
                  INT 1
                FUN 0 2 []
    FUN 0 2 []
CLS 1 4 warn
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 4 warn
        VAR 0
      FUN 2 1 .
        FUN 1 15 io:error_stream
          VAR 1
        FUN 2 1 .
          FUN 2 5 write
            VAR 1
            VAR 0
          FUN 2 1 .
            FUN 1 2 nl
              VAR 1
            FUN 0 2 []
    FUN 0 2 []
CLS 2 4 warn
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 4 warn
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 15 io:error_stream
          VAR 2
        FUN 2 1 .
          FUN 3 11 misc:format
            VAR 2
            VAR 0
            VAR 1
          FUN 0 2 []
    FUN 0 2 []
CLS 0 4 conv
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 4 conv
      FUN 2 1 .
        FUN 0 9 read_file
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 0 10 write_file
            FUN 0 2 []
    FUN 0 2 []
CLS 3 21 read_file$0_0_1$2_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 21 read_file$0_0_1$2_0_0
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 2 \=
          VAR 2
          FUN 0 2 []
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 4 warn
              FUN 0 40 {Warning: Singleton variables ~w in ~q}

              FUN 2 1 .
                VAR 2
                FUN 2 1 .
                  FUN 2 1 /
                    VAR 1
                    VAR 0
                  FUN 0 2 []
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 21 read_file$0_0_1$2_0_0
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 2 15 read_file$0_0_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 2 15 read_file$0_0_1
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 2 \=
          VAR 1
          FUN 0 2 []
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 11 clause_head
              VAR 0
              VAR 2
            FUN 2 1 .
              FUN 3 7 functor
                VAR 2
                VAR 3
                VAR 4
              FUN 2 1 .
                FUN 2 15 singleton_names
                  VAR 1
                  VAR 5
                FUN 2 1 .
                  FUN 3 21 read_file$0_0_1$2_0_0
                    VAR 4
                    VAR 3
                    VAR 5
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 15 read_file$0_0_1
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 0 9 read_file
  FUN 2 1 .
    FUN 3 6 clause
      INT 10
      FUN 0 9 read_file
      FUN 2 1 .
        FUN 1 13 dynamic_clean
          FUN 3 14 conv predicate
            VAR 0
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 1 13 dynamic_clean
            FUN 1 12 conv private
              VAR 3
          FUN 2 1 .
            FUN 1 13 dynamic_clean
              FUN 2 11 conv module
                VAR 4
                VAR 5
            FUN 2 1 .
              FUN 1 13 dynamic_clean
                FUN 2 11 conv import
                  VAR 6
                  VAR 7
              FUN 2 1 .
                FUN 0 6 repeat
                FUN 2 1 .
                  FUN 2 9 read_term
                    VAR 8
                    FUN 2 1 .
                      FUN 1 10 singletons
                        VAR 9
                      FUN 0 2 []
                  FUN 2 1 .
                    FUN 2 15 read_file$0_0_1
                      VAR 8
                      VAR 9
                    FUN 2 1 .
                      FUN 1 14 process_clause
                        VAR 8
                      FUN 0 2 []
    FUN 0 2 []
CLS 4 21 singleton_names$2_1_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 21 singleton_names$2_1_0
        VAR 0
        VAR 1
        VAR 2
        FUN 2 1 .
          INT 95
          VAR 3
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 15 singleton_names
            VAR 2
            VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 21 singleton_names$2_1_0
          VAR 0
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 15 singleton_names
            VAR 2
            VAR 1
          FUN 0 2 []
      FUN 0 2 []
CLS 2 15 singleton_names
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 15 singleton_names
        FUN 0 2 []
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 2 15 singleton_names
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              VAR 1
            VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 10 atom_codes
            VAR 0
            VAR 4
          FUN 2 1 .
            FUN 4 21 singleton_names$2_1_0
              VAR 0
              VAR 3
              VAR 2
              VAR 4
            FUN 0 2 []
      FUN 0 2 []
CLS 1 13 dynamic_clean
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 13 dynamic_clean
        VAR 0
      FUN 2 1 .
        FUN 1 7 asserta
          VAR 0
        FUN 2 1 .
          FUN 1 19 database:retractall
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 1 14 process_clause
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 14 process_clause
        FUN 0 11 end_of_file
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 14 process_clause
          FUN 1 2 :-
            FUN 2 6 module
              VAR 0
              VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 7 asserta
              FUN 2 11 conv module
                VAR 0
                VAR 1
            FUN 2 1 .
              FUN 1 7 asserta
                FUN 1 12 conv private
                  FUN 0 2 []
              FUN 2 1 .
                FUN 0 4 fail
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 1 14 process_clause
            FUN 1 2 :-
              FUN 2 10 use_module
                VAR 0
                VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 7 asserta
                FUN 2 11 conv import
                  VAR 0
                  VAR 1
              FUN 2 1 .
                FUN 0 4 fail
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 1 14 process_clause
              FUN 1 2 :-
                VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 0 4 fail
                FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 2
              FUN 1 14 process_clause
                FUN 2 1 ,
                  VAR 0
                  VAR 1
              FUN 2 1 .
                FUN 1 4 warn
                  FUN 2 1 -
                    FUN 0 22 Warning: defining ',' 
                    FUN 2 1 ,
                      VAR 0
                      VAR 1
                FUN 2 1 .
                  FUN 0 4 fail
                  FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 2
                FUN 1 14 process_clause
                  FUN 2 1 ;
                    VAR 0
                    VAR 1
                FUN 2 1 .
                  FUN 1 4 warn
                    FUN 2 1 -
                      FUN 0 22 Warning: defining ';' 
                      FUN 2 1 ;
                        VAR 0
                        VAR 1
                  FUN 2 1 .
                    FUN 0 4 fail
                    FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 2
                  FUN 1 14 process_clause
                    FUN 2 2 ->
                      VAR 0
                      VAR 1
                  FUN 2 1 .
                    FUN 1 4 warn
                      FUN 2 1 -
                        FUN 0 23 Warning: defining '->' 
                        FUN 2 2 ->
                          VAR 0
                          VAR 1
                    FUN 2 1 .
                      FUN 0 4 fail
                      FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 2
                    FUN 1 14 process_clause
                      VAR 0
                    FUN 2 1 .
                      FUN 2 16 term:expand_term
                        VAR 0
                        VAR 1
                      FUN 2 1 .
                        FUN 1 20 add_module_converted
                          VAR 1
                        FUN 2 1 .
                          FUN 0 1 !
                          FUN 2 1 .
                            FUN 0 4 fail
                            FUN 0 2 []
                  FUN 0 2 []
CLS 1 20 add_module_converted
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 20 add_module_converted
        VAR 0
      FUN 2 1 .
        FUN 2 11 module_head
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 13 add_converted
            VAR 1
          FUN 0 2 []
    FUN 0 2 []
CLS 1 13 add_converted
  FUN 2 1 .
    FUN 3 6 clause
      INT 12
      FUN 1 13 add_converted
        VAR 0
      FUN 2 1 .
        FUN 2 11 clause_head
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 3 7 functor
            VAR 1
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 3 13 get_predicate
              VAR 2
              VAR 3
              VAR 4
            FUN 2 1 .
              FUN 2 12 lists:length
                VAR 4
                VAR 5
              FUN 2 1 .
                FUN 4 11 clause_conv
                  FUN 2 1 -
                    FUN 2 1 -
                      VAR 2
                      VAR 3
                    VAR 5
                  VAR 0
                  VAR 6
                  VAR 7
                FUN 2 1 .
                  FUN 2 11 escape_vars
                    FUN 2 1 -
                      VAR 6
                      VAR 7
                    FUN 2 1 -
                      VAR 8
                      VAR 9
                  FUN 2 1 .
                    FUN 3 15 term:numbervars
                      FUN 2 1 -
                        VAR 8
                        VAR 9
                      INT 0
                      VAR 10
                    FUN 2 1 .
                      FUN 3 12 lists:append
                        VAR 4
                        FUN 2 1 .
                          FUN 3 6 clause
                            VAR 10
                            VAR 8
                            VAR 9
                          FUN 0 2 []
                        VAR 11
                      FUN 2 1 .
                        FUN 3 13 put_predicate
                          VAR 2
                          VAR 3
                          VAR 11
                        FUN 0 2 []
    FUN 0 2 []
CLS 2 11 clause_head
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 11 clause_head
        FUN 2 2 :-
          VAR 0
          VAR 1
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 11 clause_head
          VAR 0
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 4 19 clause_head_replace
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 19 clause_head_replace
        FUN 2 2 :-
          VAR 0
          VAR 1
        VAR 0
        FUN 2 2 :-
          VAR 2
          VAR 1
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 4 19 clause_head_replace
          VAR 0
          VAR 0
          VAR 1
          VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 3 13 get_predicate
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 13 get_predicate
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 7 retract
          FUN 3 14 conv predicate
            VAR 0
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 13 get_predicate
          VAR 0
          VAR 1
          FUN 0 2 []
        FUN 0 2 []
      FUN 0 2 []
CLS 3 13 put_predicate
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 13 put_predicate
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 7 assertz
          FUN 3 14 conv predicate
            VAR 0
            VAR 1
            VAR 2
        FUN 0 2 []
    FUN 0 2 []
CLS 4 11 clause_conv
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 11 clause_conv
        VAR 0
        FUN 2 2 :-
          VAR 1
          VAR 2
        VAR 1
        VAR 3
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 5 9 body_list
            VAR 2
            VAR 0
            FUN 2 1 .
              VAR 1
              FUN 0 2 []
            FUN 0 2 []
            VAR 3
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 4 11 clause_conv
          VAR 0
          VAR 1
          VAR 1
          FUN 0 2 []
        FUN 0 2 []
      FUN 0 2 []
CLS 5 9 body_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 5 9 body_list
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        FUN 2 1 .
          VAR 0
          VAR 3
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 5 9 body_list
          FUN 2 1 ;
            VAR 0
            VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 5
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 5 19 convert_disjunction
              FUN 2 1 ;
                VAR 0
                VAR 1
              VAR 2
              VAR 3
              VAR 4
              VAR 5
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 6
          FUN 5 9 body_list
            FUN 2 2 ->
              VAR 0
              VAR 1
            VAR 2
            VAR 3
            VAR 4
            VAR 5
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 5 19 convert_disjunction
                FUN 2 2 ->
                  VAR 0
                  VAR 1
                VAR 2
                VAR 3
                VAR 4
                VAR 5
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 7
            FUN 5 9 body_list
              FUN 2 1 ,
                VAR 0
                VAR 1
              VAR 2
              VAR 3
              VAR 4
              VAR 5
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 5 9 body_list
                  VAR 1
                  VAR 2
                  FUN 2 1 .
                    VAR 0
                    VAR 3
                  VAR 4
                  VAR 6
                FUN 2 1 .
                  FUN 5 9 body_list
                    VAR 0
                    VAR 2
                    FUN 2 1 .
                      VAR 1
                      VAR 3
                    VAR 6
                    VAR 5
                  FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 4
              FUN 5 9 body_list
                VAR 0
                VAR 1
                VAR 2
                VAR 3
                FUN 2 1 .
                  VAR 0
                  VAR 3
              FUN 0 2 []
            FUN 0 2 []
CLS 2 11 escape_vars
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 11 escape_vars
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 1
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 11 escape_vars
          FUN 1 4 $VAR
            VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 1
              FUN 1 4 $VAR
                FUN 1 4 $VAR
                  VAR 2
            FUN 2 1 .
              FUN 2 11 escape_vars
                VAR 0
                VAR 2
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 5
          FUN 2 11 escape_vars
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 1 8 compound
              VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 3 =..
                  VAR 0
                  FUN 2 1 .
                    VAR 2
                    VAR 3
                FUN 2 1 .
                  FUN 2 13 escape_vars_l
                    VAR 3
                    VAR 4
                  FUN 2 1 .
                    FUN 2 3 =..
                      VAR 1
                      FUN 2 1 .
                        VAR 2
                        VAR 4
                    FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 2 11 escape_vars
              VAR 0
              VAR 0
            FUN 0 2 []
          FUN 0 2 []
CLS 2 13 escape_vars_l
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 13 escape_vars_l
        FUN 0 2 []
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 2 13 escape_vars_l
          FUN 2 1 .
            VAR 0
            VAR 1
          FUN 2 1 .
            VAR 2
            VAR 3
        FUN 2 1 .
          FUN 2 11 escape_vars
            VAR 0
            VAR 2
          FUN 2 1 .
            FUN 2 13 escape_vars_l
              VAR 1
              VAR 3
            FUN 0 2 []
      FUN 0 2 []
CLS 5 19 convert_disjunction
  FUN 2 1 .
    FUN 3 6 clause
      INT 8
      FUN 5 19 convert_disjunction
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 1 7 no_cuts
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 12 lists:length
              VAR 3
              VAR 5
            FUN 2 1 .
              FUN 3 16 disjunction_name
                VAR 1
                VAR 5
                VAR 6
              FUN 2 1 .
                FUN 4 18 expand_disjunction
                  VAR 0
                  VAR 2
                  VAR 6
                  VAR 7
                FUN 2 1 .
                  FUN 2 1 =
                    VAR 4
                    FUN 2 1 .
                      VAR 7
                      VAR 3
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 5 19 convert_disjunction
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 2 1 =
            VAR 4
            FUN 2 1 .
              FUN 1 7 $choice
                VAR 5
              FUN 2 1 .
                FUN 2 19 execution:interpret
                  VAR 0
                  VAR 5
                VAR 3
          FUN 0 2 []
      FUN 0 2 []
CLS 4 18 expand_disjunction
  FUN 2 1 .
    FUN 3 6 clause
      INT 8
      FUN 4 18 expand_disjunction
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 3 12 collect_vars
          VAR 0
          VAR 4
          FUN 0 2 []
        FUN 2 1 .
          FUN 3 12 collect_vars
            VAR 1
            VAR 5
            FUN 0 2 []
          FUN 2 1 .
            FUN 2 14 different_vars
              VAR 4
              VAR 6
            FUN 2 1 .
              FUN 3 9 intersect
                VAR 6
                VAR 5
                VAR 7
              FUN 2 1 .
                FUN 2 3 =..
                  VAR 3
                  FUN 2 1 .
                    VAR 2
                    VAR 7
                FUN 2 1 .
                  FUN 2 11 new_clauses
                    VAR 0
                    VAR 3
                  FUN 0 2 []
    FUN 0 2 []
CLS 2 11 new_clauses
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 2 11 new_clauses
        FUN 2 1 ;
          FUN 2 2 ->
            VAR 0
            VAR 1
          VAR 2
        VAR 3
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 19 inline_unifications
            FUN 2 2 :-
              VAR 3
              FUN 2 1 ,
                VAR 0
                FUN 2 1 ,
                  FUN 0 1 !
                  VAR 1
            VAR 4
          FUN 2 1 .
            FUN 1 13 add_converted
              VAR 4
            FUN 2 1 .
              FUN 2 11 new_clauses
                VAR 2
                VAR 3
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 2 11 new_clauses
          FUN 2 1 ;
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 19 inline_unifications
              FUN 2 2 :-
                VAR 2
                VAR 0
              VAR 3
            FUN 2 1 .
              FUN 1 13 add_converted
                VAR 3
              FUN 2 1 .
                FUN 2 11 new_clauses
                  VAR 1
                  VAR 2
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 2 11 new_clauses
            FUN 2 2 ->
              VAR 0
              VAR 1
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 19 inline_unifications
                FUN 2 2 :-
                  VAR 2
                  FUN 2 1 ,
                    VAR 0
                    FUN 2 1 ,
                      FUN 0 1 !
                      VAR 1
                VAR 3
              FUN 2 1 .
                FUN 1 13 add_converted
                  VAR 3
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 3
            FUN 2 11 new_clauses
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 2 19 inline_unifications
                FUN 2 2 :-
                  VAR 1
                  VAR 0
                VAR 2
              FUN 2 1 .
                FUN 1 13 add_converted
                  VAR 2
                FUN 0 2 []
          FUN 0 2 []
CLS 2 19 inline_unifications
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 19 inline_unifications
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 9 copy_term
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 2 22 inline_unifications_in
            VAR 2
            VAR 1
          FUN 0 2 []
    FUN 0 2 []
CLS 2 28 inline_unifications_in$2_0_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 28 inline_unifications_in$2_0_2
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 1
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 28 inline_unifications_in$2_0_2
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 3 var
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 2 28 inline_unifications_in$2_1_3
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 28 inline_unifications_in$2_1_3
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 1
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 28 inline_unifications_in$2_1_3
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 3 var
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 2 22 inline_unifications_in
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 22 inline_unifications_in
        FUN 2 2 :-
          VAR 0
          FUN 2 1 =
            VAR 1
            VAR 2
        VAR 0
      FUN 2 1 .
        FUN 2 28 inline_unifications_in$2_0_2
          VAR 2
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 1
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 2 22 inline_unifications_in
          FUN 2 2 :-
            VAR 0
            FUN 2 1 ,
              FUN 2 1 =
                VAR 1
                VAR 2
              VAR 3
          VAR 4
        FUN 2 1 .
          FUN 2 28 inline_unifications_in$2_1_3
            VAR 2
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 1
                VAR 2
              FUN 2 1 .
                FUN 2 22 inline_unifications_in
                  FUN 2 2 :-
                    VAR 0
                    VAR 3
                  VAR 4
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 2 22 inline_unifications_in
            VAR 0
            VAR 0
          FUN 0 2 []
        FUN 0 2 []
CLS 1 7 no_cuts
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 7 no_cuts
        FUN 2 1 ,
          VAR 0
          VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 7 no_cuts
            VAR 0
          FUN 2 1 .
            FUN 1 7 no_cuts
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 7 no_cuts
          FUN 2 1 ;
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 7 no_cuts
              VAR 0
            FUN 2 1 .
              FUN 1 7 no_cuts
                VAR 1
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 1 7 no_cuts
            FUN 2 2 ->
              VAR 0
              VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 7 no_cuts
                VAR 0
              FUN 2 1 .
                FUN 1 7 no_cuts
                  VAR 1
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 0
            FUN 1 7 no_cuts
              FUN 0 1 !
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 0 4 fail
                FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 1
              FUN 1 7 no_cuts
                VAR 0
              FUN 0 2 []
            FUN 0 2 []
CLS 3 16 disjunction_name
  FUN 2 1 .
    FUN 3 6 clause
      INT 12
      FUN 3 16 disjunction_name
        FUN 2 1 -
          FUN 2 1 -
            VAR 0
            VAR 1
          VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 2 10 atom_codes
          VAR 0
          VAR 5
        FUN 2 1 .
          FUN 2 12 number_codes
            VAR 1
            VAR 6
          FUN 2 1 .
            FUN 2 12 number_codes
              VAR 2
              VAR 7
            FUN 2 1 .
              FUN 2 12 number_codes
                VAR 3
                VAR 8
              FUN 2 1 .
                FUN 3 12 lists:append
                  VAR 7
                  FUN 2 1 .
                    INT 95
                    VAR 8
                  VAR 9
                FUN 2 1 .
                  FUN 3 12 lists:append
                    VAR 6
                    FUN 2 1 .
                      INT 95
                      VAR 9
                    VAR 10
                  FUN 2 1 .
                    FUN 3 12 lists:append
                      VAR 5
                      FUN 2 1 .
                        INT 36
                        VAR 10
                      VAR 11
                    FUN 2 1 .
                      FUN 2 10 atom_codes
                        VAR 4
                        VAR 11
                      FUN 0 2 []
    FUN 0 2 []
CLS 3 12 collect_vars
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 12 collect_vars
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 1
              FUN 2 1 .
                VAR 0
                VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 12 collect_vars
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 8 compound
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 3 =..
                VAR 0
                FUN 2 1 .
                  VAR 3
                  VAR 4
              FUN 2 1 .
                FUN 3 13 collect_vars0
                  VAR 4
                  VAR 1
                  VAR 2
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 12 collect_vars
            VAR 0
            VAR 1
            VAR 1
          FUN 0 2 []
        FUN 0 2 []
CLS 3 13 collect_vars0
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 13 collect_vars0
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 13 collect_vars0
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 3 12 collect_vars
            VAR 0
            VAR 4
            VAR 3
          FUN 2 1 .
            FUN 3 13 collect_vars0
              VAR 1
              VAR 2
              VAR 4
            FUN 0 2 []
      FUN 0 2 []
CLS 4 15 intersect$3_1_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 15 intersect$3_1_1
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 13 var_memberchk
          VAR 3
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 1
              FUN 2 1 .
                VAR 3
                VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 15 intersect$3_1_1
          VAR 0
          VAR 0
          VAR 1
          VAR 2
        FUN 0 2 []
      FUN 0 2 []
CLS 3 9 intersect
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 9 intersect
        FUN 0 2 []
        VAR 0
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 9 intersect
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 4 15 intersect$3_1_1
            VAR 4
            VAR 3
            VAR 2
            VAR 0
          FUN 2 1 .
            FUN 3 9 intersect
              VAR 1
              VAR 2
              VAR 4
            FUN 0 2 []
      FUN 0 2 []
CLS 4 20 different_vars$2_1_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 20 different_vars$2_1_1
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 2 \+
          FUN 2 13 var_memberchk
            VAR 3
            VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 1
              FUN 2 1 .
                VAR 3
                VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 20 different_vars$2_1_1
          VAR 0
          VAR 0
          VAR 1
          VAR 2
        FUN 0 2 []
      FUN 0 2 []
CLS 2 14 different_vars
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 14 different_vars
        FUN 0 2 []
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 2 14 different_vars
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 4 20 different_vars$2_1_1
            VAR 3
            VAR 2
            VAR 1
            VAR 0
          FUN 2 1 .
            FUN 2 14 different_vars
              VAR 1
              VAR 3
            FUN 0 2 []
      FUN 0 2 []
CLS 2 13 var_memberchk
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 13 var_memberchk
        VAR 0
        FUN 2 1 .
          VAR 1
          VAR 2
      FUN 2 1 .
        FUN 2 2 ==
          VAR 1
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 13 var_memberchk
          VAR 0
          FUN 2 1 .
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 2 13 var_memberchk
            VAR 0
            VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 3 11 module_name
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 3 11 module_name
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 10 atom_codes
          VAR 1
          VAR 3
        FUN 2 1 .
          FUN 2 10 atom_codes
            VAR 0
            VAR 4
          FUN 2 1 .
            FUN 3 12 lists:append
              VAR 3
              FUN 2 1 .
                INT 58
                VAR 4
              VAR 5
            FUN 2 1 .
              FUN 2 10 atom_codes
                VAR 2
                VAR 5
              FUN 0 2 []
    FUN 0 2 []
CLS 4 17 module_head$2_0_4
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 17 module_head$2_0_4
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 12 lists:member
          FUN 2 1 /
            VAR 3
            VAR 2
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 17 module_head$2_0_4
          FUN 2 1 .
            FUN 2 1 /
              VAR 0
              VAR 1
            VAR 2
          VAR 2
          VAR 1
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 2 11 module_head
  FUN 2 1 .
    FUN 3 6 clause
      INT 12
      FUN 2 11 module_head
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 11 conv module
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 4 19 clause_head_replace
            VAR 0
            VAR 4
            VAR 1
            VAR 5
          FUN 2 1 .
            FUN 3 7 functor
              VAR 4
              VAR 6
              VAR 7
            FUN 2 1 .
              FUN 1 2 \+
                FUN 2 12 lists:member
                  FUN 2 1 /
                    VAR 6
                    VAR 7
                  VAR 3
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 1 7 retract
                    FUN 1 12 conv private
                      VAR 8
                  FUN 2 1 .
                    FUN 4 17 module_head$2_0_4
                      VAR 9
                      VAR 8
                      VAR 7
                      VAR 6
                    FUN 2 1 .
                      FUN 1 7 asserta
                        FUN 1 12 conv private
                          VAR 9
                      FUN 2 1 .
                        FUN 2 3 =..
                          VAR 4
                          FUN 2 1 .
                            VAR 6
                            VAR 10
                        FUN 2 1 .
                          FUN 3 11 module_name
                            VAR 6
                            VAR 2
                            VAR 11
                          FUN 2 1 .
                            FUN 2 3 =..
                              VAR 5
                              FUN 2 1 .
                                VAR 11
                                VAR 10
                            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 11 module_head
          VAR 0
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 3 11 module_term
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 11 module_term
        VAR 0
        FUN 0 2 []
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 11 module_term
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 3 var
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 5 throw
                FUN 1 3 bad
                  FUN 3 11 module_term
                    VAR 0
                    VAR 1
                    VAR 2
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 3 11 module_term
            FUN 0 2 []
            VAR 0
            FUN 0 2 []
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 4
            FUN 3 11 module_term
              FUN 2 1 .
                VAR 0
                VAR 1
              VAR 2
              VAR 3
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 4 23 module_term_clause_list
                  VAR 0
                  VAR 1
                  VAR 2
                  VAR 3
                FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 3
              FUN 3 11 module_term
                VAR 0
                VAR 1
                VAR 2
              FUN 2 1 .
                FUN 1 5 throw
                  FUN 1 3 bad
                    FUN 3 11 module_term
                      VAR 0
                      VAR 1
                      VAR 2
                FUN 0 2 []
            FUN 0 2 []
CLS 3 29 module_term_clause_list$4_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 3 29 module_term_clause_list$4_0_0
        VAR 0
        VAR 1
        FUN 2 1 .
          VAR 2
          VAR 3
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 4 23 module_term_clause_list
            VAR 2
            VAR 3
            VAR 1
            VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 29 module_term_clause_list$4_0_0
          VAR 0
          VAR 1
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 3 29 module_term_clause_list$4_0_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 3 29 module_term_clause_list$4_0_1
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 2
        FUN 2 1 .
          FUN 2 1 =
            VAR 2
            FUN 3 6 clause
              VAR 3
              VAR 4
              VAR 5
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 1
                FUN 3 6 clause
                  VAR 3
                  VAR 4
                  VAR 6
              FUN 2 1 .
                FUN 3 14 modularize_all
                  VAR 5
                  VAR 0
                  VAR 6
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 29 module_term_clause_list$4_0_1
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 5 throw
            FUN 1 3 bad
              FUN 3 12 modularize__
                VAR 2
                VAR 0
                VAR 1
          FUN 2 1 .
            FUN 3 10 modularize
              VAR 2
              VAR 0
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 4 23 module_term_clause_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 23 module_term_clause_list
        VAR 0
        VAR 1
        VAR 2
        FUN 2 1 .
          VAR 3
          VAR 4
      FUN 2 1 .
        FUN 3 29 module_term_clause_list$4_0_1
          VAR 2
          VAR 3
          VAR 0
        FUN 2 1 .
          FUN 3 29 module_term_clause_list$4_0_0
            VAR 4
            VAR 2
            VAR 1
          FUN 0 2 []
    FUN 0 2 []
CLS 1 16 mymeta_predicate
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 16 mymeta_predicate
        FUN 2 1 ,
          FUN 0 1 :
          FUN 0 1 :
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 16 mymeta_predicate
          FUN 2 2 ->
            FUN 0 1 :
            FUN 0 1 :
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 0
          FUN 1 16 mymeta_predicate
            FUN 2 1 .
              FUN 0 1 :
              FUN 0 1 :
          FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 0
            FUN 1 16 mymeta_predicate
              FUN 2 1 ;
                FUN 0 1 :
                FUN 0 1 :
            FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 0
              FUN 1 16 mymeta_predicate
                FUN 1 2 \+
                  FUN 0 1 :
              FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 0
                FUN 1 16 mymeta_predicate
                  FUN 2 1 ^
                    FUN 0 1 ?
                    FUN 0 1 :
                FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 0
                  FUN 1 16 mymeta_predicate
                    FUN 2 16 call_cleanup_det
                      FUN 0 1 :
                      FUN 0 1 :
                  FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 0
                    FUN 1 16 mymeta_predicate
                      FUN 3 8 $asserts
                        FUN 0 1 :
                        FUN 0 1 :
                        FUN 0 1 ?
                    FUN 0 2 []
                  FUN 2 1 .
                    FUN 3 6 clause
                      INT 0
                      FUN 1 16 mymeta_predicate
                        FUN 1 8 $abolish
                          FUN 0 1 :
                      FUN 0 2 []
                    FUN 2 1 .
                      FUN 3 6 clause
                        INT 0
                        FUN 1 16 mymeta_predicate
                          FUN 4 10 with_input
                            FUN 0 1 ?
                            FUN 0 1 ?
                            FUN 0 1 :
                            FUN 0 1 :
                        FUN 0 2 []
                      FUN 2 1 .
                        FUN 3 6 clause
                          INT 0
                          FUN 1 16 mymeta_predicate
                            FUN 4 11 with_output
                              FUN 0 1 ?
                              FUN 0 1 ?
                              FUN 0 1 :
                              FUN 0 1 :
                          FUN 0 2 []
                        FUN 2 1 .
                          FUN 3 6 clause
                            INT 0
                            FUN 1 16 mymeta_predicate
                              FUN 1 7 nodebug
                                FUN 0 1 :
                            FUN 0 2 []
                          FUN 2 1 .
                            FUN 3 6 clause
                              INT 0
                              FUN 1 16 mymeta_predicate
                                FUN 2 9 err_check
                                  FUN 0 1 :
                                  FUN 0 6 dolist
                              FUN 0 2 []
                            FUN 2 1 .
                              FUN 3 6 clause
                                INT 0
                                FUN 1 16 mymeta_predicate
                                  FUN 4 10 catch_some
                                    FUN 0 1 :
                                    FUN 0 1 ?
                                    FUN 0 1 :
                                    FUN 0 1 ?
                                FUN 0 2 []
                              FUN 2 1 .
                                FUN 3 6 clause
                                  INT 0
                                  FUN 1 16 mymeta_predicate
                                    FUN 4 20 execution:catch_some
                                      FUN 0 1 :
                                      FUN 0 1 ?
                                      FUN 0 1 :
                                      FUN 0 1 ?
                                  FUN 0 2 []
                                FUN 2 1 .
                                  FUN 3 6 clause
                                    INT 0
                                    FUN 1 16 mymeta_predicate
                                      FUN 1 18 io:default_streams
                                        FUN 0 1 :
                                    FUN 0 2 []
                                  FUN 2 1 .
                                    FUN 3 6 clause
                                      INT 0
                                      FUN 1 16 mymeta_predicate
                                        FUN 1 3 not
                                          FUN 0 1 :
                                      FUN 0 2 []
                                    FUN 2 1 .
                                      FUN 3 6 clause
                                        INT 0
                                        FUN 1 16 mymeta_predicate
                                          FUN 1 7 abolish
                                            FUN 0 1 :
                                        FUN 0 2 []
                                      FUN 2 1 .
                                        FUN 3 6 clause
                                          INT 0
                                          FUN 1 16 mymeta_predicate
                                            FUN 2 7 abolish
                                              FUN 0 1 :
                                              FUN 0 1 ?
                                          FUN 0 2 []
                                        FUN 2 1 .
                                          FUN 3 6 clause
                                            INT 0
                                            FUN 1 16 mymeta_predicate
                                              FUN 2 14 add_breakpoint
                                                FUN 0 1 :
                                                FUN 0 1 ?
                                            FUN 0 2 []
                                          FUN 2 1 .
                                            FUN 3 6 clause
                                              INT 0
                                              FUN 1 16 mymeta_predicate
                                                FUN 1 6 assert
                                                  FUN 0 1 :
                                              FUN 0 2 []
                                            FUN 2 1 .
                                              FUN 3 6 clause
                                                INT 0
                                                FUN 1 16 mymeta_predicate
                                                  FUN 2 6 assert
                                                    FUN 0 1 :
                                                    FUN 0 1 ?
                                                FUN 0 2 []
                                              FUN 2 1 .
                                                FUN 3 6 clause
                                                  INT 0
                                                  FUN 1 16 mymeta_predicate
                                                    FUN 1 7 asserta
                                                      FUN 0 1 :
                                                  FUN 0 2 []
                                                FUN 2 1 .
                                                  FUN 3 6 clause
                                                    INT 0
                                                    FUN 1 16 mymeta_predicate
                                                      FUN 2 7 asserta
                                                        FUN 0 1 :
                                                        FUN 0 1 ?
                                                    FUN 0 2 []
                                                  FUN 2 1 .
                                                    FUN 3 6 clause
                                                      INT 0
                                                      FUN 1 16 mymeta_predicate
                                                        FUN 1 7 assertz
                                                          FUN 0 1 :
                                                      FUN 0 2 []
                                                    FUN 2 1 .
                                                      FUN 3 6 clause
                                                        INT 0
                                                        FUN 1 16 mymeta_predicate
                                                          FUN 2 7 assertz
                                                            FUN 0 1 :
                                                            FUN 0 1 ?
                                                        FUN 0 2 []
                                                      FUN 2 1 .
                                                        FUN 3 6 clause
                                                          INT 0
                                                          FUN 1 16 mymeta_predicate
                                                            FUN 3 5 bagof
                                                              FUN 0 1 ?
                                                              FUN 0 1 :
                                                              FUN 0 1 ?
                                                          FUN 0 2 []
                                                        FUN 2 1 .
                                                          FUN 3 6 clause
                                                            INT 0
                                                            FUN 1 16 mymeta_predicate
                                                              FUN 1 4 call
                                                                FUN 0 1 :
                                                            FUN 0 2 []
                                                          FUN 2 1 .
                                                            FUN 3 6 clause
                                                              INT 0
                                                              FUN 1 16 mymeta_predicate
                                                                FUN 2 12 call_cleanup
                                                                  FUN 0 1 :
                                                                  FUN 0 1 :
                                                              FUN 0 2 []
                                                            FUN 2 1 .
                                                              FUN 3 6 clause
                                                                INT 0
                                                                FUN 1 16 mymeta_predicate
                                                                  FUN 2 12 call_residue
                                                                    FUN 0 1 :
                                                                    FUN 0 1 ?
                                                                FUN 0 2 []
                                                              FUN 2 1 .
                                                                FUN 3 6 clause
                                                                  INT 0
                                                                  FUN 1 16 mymeta_predicate
                                                                    FUN 3 5 catch
                                                                      FUN 0 1 :
                                                                      FUN 0 1 ?
                                                                      FUN 0 1 :
                                                                  FUN 0 2 []
                                                                FUN 2 1 .
                                                                  FUN 3 6 clause
                                                                    INT 0
                                                                    FUN 1 16 mymeta_predicate
                                                                      FUN 2 6 clause
                                                                        FUN 0 1 :
                                                                        FUN 0 1 ?
                                                                    FUN 0 2 []
                                                                  FUN 2 1 .
                                                                    FUN 3 6 clause
                                                                      INT 0
                                                                      FUN 1 16 mymeta_predicate
                                                                        FUN 3 6 clause
                                                                          FUN 0 1 :
                                                                          FUN 0 1 ?
                                                                          FUN 0 1 ?
                                                                      FUN 0 2 []
                                                                    FUN 2 1 .
                                                                      FUN 3 6 clause
                                                                        INT 0
                                                                        FUN 1 16 mymeta_predicate
                                                                          FUN 1 7 compile
                                                                            FUN 0 1 :
                                                                        FUN 0 2 []
                                                                      FUN 2 1 .
                                                                        FUN 3 6 clause
                                                                          INT 0
                                                                          FUN 1 16 mymeta_predicate
                                                                            FUN 1 7 consult
                                                                              FUN 0 1 :
                                                                          FUN 0 2 []
                                                                        FUN 2 1 .
                                                                          FUN 3 6 clause
                                                                            INT 0
                                                                            FUN 1 16 mymeta_predicate
                                                                              FUN 5 18 current_breakpoint
                                                                                FUN 0 1 :
                                                                                FUN 0 1 ?
                                                                                FUN 0 1 ?
                                                                                FUN 0 1 ?
                                                                                FUN 0 1 ?
                                                                            FUN 0 2 []
                                                                          FUN 2 1 .
                                                                            FUN 3 6 clause
                                                                              INT 0
                                                                              FUN 1 16 mymeta_predicate
                                                                                FUN 1 17 current_predicate
                                                                                  FUN 0 1 :
                                                                              FUN 0 2 []
                                                                            FUN 2 1 .
                                                                              FUN 3 6 clause
                                                                                INT 0
                                                                                FUN 1 16 mymeta_predicate
                                                                                  FUN 2 17 current_predicate
                                                                                    FUN 0 1 ?
                                                                                    FUN 0 1 :
                                                                                FUN 0 2 []
                                                                              FUN 2 1 .
                                                                                FUN 3 6 clause
                                                                                  INT 0
                                                                                  FUN 1 16 mymeta_predicate
                                                                                    FUN 1 13 ensure_loaded
                                                                                      FUN 0 1 :
                                                                                  FUN 0 2 []
                                                                                FUN 2 1 .
                                                                                  FUN 3 6 clause
                                                                                    INT 0
                                                                                    FUN 1 16 mymeta_predicate
                                                                                      FUN 1 15 execution_state
                                                                                        FUN 0 1 :
                                                                                    FUN 0 2 []
                                                                                  FUN 2 1 .
                                                                                    FUN 3 6 clause
                                                                                      INT 0
                                                                                      FUN 1 16 mymeta_predicate
                                                                                        FUN 2 15 execution_state
                                                                                          FUN 0 1 ?
                                                                                          FUN 0 1 :
                                                                                      FUN 0 2 []
                                                                                    FUN 2 1 .
                                                                                      FUN 3 6 clause
                                                                                        INT 0
                                                                                        FUN 1 16 mymeta_predicate
                                                                                          FUN 3 7 findall
                                                                                            FUN 0 1 ?
                                                                                            FUN 0 1 :
                                                                                            FUN 0 1 ?
                                                                                        FUN 0 2 []
                                                                                      FUN 2 1 .
                                                                                        FUN 3 6 clause
                                                                                          INT 0
                                                                                          FUN 1 16 mymeta_predicate
                                                                                            FUN 4 7 findall
                                                                                              FUN 0 1 ?
                                                                                              FUN 0 1 :
                                                                                              FUN 0 1 ?
                                                                                              FUN 0 1 ?
                                                                                          FUN 0 2 []
                                                                                        FUN 2 1 .
                                                                                          FUN 3 6 clause
                                                                                            INT 0
                                                                                            FUN 1 16 mymeta_predicate
                                                                                              FUN 2 6 format
                                                                                                FUN 0 1 ?
                                                                                                FUN 0 1 :
                                                                                            FUN 0 2 []
                                                                                          FUN 2 1 .
                                                                                            FUN 3 6 clause
                                                                                              INT 0
                                                                                              FUN 1 16 mymeta_predicate
                                                                                                FUN 3 6 format
                                                                                                  FUN 0 1 ?
                                                                                                  FUN 0 1 ?
                                                                                                  FUN 0 1 :
                                                                                              FUN 0 2 []
                                                                                            FUN 2 1 .
                                                                                              FUN 3 6 clause
                                                                                                INT 0
                                                                                                FUN 1 16 mymeta_predicate
                                                                                                  FUN 2 6 freeze
                                                                                                    FUN 0 1 ?
                                                                                                    FUN 0 1 :
                                                                                                FUN 0 2 []
                                                                                              FUN 2 1 .
                                                                                                FUN 3 6 clause
                                                                                                  INT 0
                                                                                                  FUN 1 16 mymeta_predicate
                                                                                                    FUN 3 2 if
                                                                                                      FUN 0 1 :
                                                                                                      FUN 0 1 :
                                                                                                      FUN 0 1 :
                                                                                                  FUN 0 2 []
                                                                                                FUN 2 1 .
                                                                                                  FUN 3 6 clause
                                                                                                    INT 0
                                                                                                    FUN 1 16 mymeta_predicate
                                                                                                      FUN 1 6 incore
                                                                                                        FUN 0 1 :
                                                                                                    FUN 0 2 []
                                                                                                  FUN 2 1 .
                                                                                                    FUN 3 6 clause
                                                                                                      INT 0
                                                                                                      FUN 1 16 mymeta_predicate
                                                                                                        FUN 1 7 listing
                                                                                                          FUN 0 1 :
                                                                                                      FUN 0 2 []
                                                                                                    FUN 2 1 .
                                                                                                      FUN 3 6 clause
                                                                                                        INT 0
                                                                                                        FUN 1 16 mymeta_predicate
                                                                                                          FUN 1 4 load
                                                                                                            FUN 0 1 :
                                                                                                        FUN 0 2 []
                                                                                                      FUN 2 1 .
                                                                                                        FUN 3 6 clause
                                                                                                          INT 0
                                                                                                          FUN 1 16 mymeta_predicate
                                                                                                            FUN 1 10 load_files
                                                                                                              FUN 0 1 :
                                                                                                          FUN 0 2 []
                                                                                                        FUN 2 1 .
                                                                                                          FUN 3 6 clause
                                                                                                            INT 0
                                                                                                            FUN 1 16 mymeta_predicate
                                                                                                              FUN 2 10 load_files
                                                                                                                FUN 0 1 :
                                                                                                                FUN 0 1 ?
                                                                                                            FUN 0 2 []
                                                                                                          FUN 2 1 .
                                                                                                            FUN 3 6 clause
                                                                                                              INT 0
                                                                                                              FUN 1 16 mymeta_predicate
                                                                                                                FUN 2 18 load_foreign_files
                                                                                                                  FUN 0 1 :
                                                                                                                  FUN 0 1 ?
                                                                                                              FUN 0 2 []
                                                                                                            FUN 2 1 .
                                                                                                              FUN 3 6 clause
                                                                                                                INT 0
                                                                                                                FUN 1 16 mymeta_predicate
                                                                                                                  FUN 1 21 load_foreign_resource
                                                                                                                    FUN 0 1 :
                                                                                                                FUN 0 2 []
                                                                                                              FUN 2 1 .
                                                                                                                FUN 3 6 clause
                                                                                                                  INT 0
                                                                                                                  FUN 1 16 mymeta_predicate
                                                                                                                    FUN 1 5 nospy
                                                                                                                      FUN 0 1 :
                                                                                                                  FUN 0 2 []
                                                                                                                FUN 2 1 .
                                                                                                                  FUN 3 6 clause
                                                                                                                    INT 0
                                                                                                                    FUN 1 16 mymeta_predicate
                                                                                                                      FUN 3 12 on_exception
                                                                                                                        FUN 0 1 ?
                                                                                                                        FUN 0 1 :
                                                                                                                        FUN 0 1 :
                                                                                                                    FUN 0 2 []
                                                                                                                  FUN 2 1 .
                                                                                                                    FUN 3 6 clause
                                                                                                                      INT 0
                                                                                                                      FUN 1 16 mymeta_predicate
                                                                                                                        FUN 1 4 once
                                                                                                                          FUN 0 1 :
                                                                                                                      FUN 0 2 []
                                                                                                                    FUN 2 1 .
                                                                                                                      FUN 3 6 clause
                                                                                                                        INT 0
                                                                                                                        FUN 1 16 mymeta_predicate
                                                                                                                          FUN 2 6 phrase
                                                                                                                            FUN 0 1 :
                                                                                                                            FUN 0 1 ?
                                                                                                                        FUN 0 2 []
                                                                                                                      FUN 2 1 .
                                                                                                                        FUN 3 6 clause
                                                                                                                          INT 0
                                                                                                                          FUN 1 16 mymeta_predicate
                                                                                                                            FUN 3 6 phrase
                                                                                                                              FUN 0 1 :
                                                                                                                              FUN 0 1 ?
                                                                                                                              FUN 0 1 ?
                                                                                                                          FUN 0 2 []
                                                                                                                        FUN 2 1 .
                                                                                                                          FUN 3 6 clause
                                                                                                                            INT 0
                                                                                                                            FUN 1 16 mymeta_predicate
                                                                                                                              FUN 2 18 predicate_property
                                                                                                                                FUN 0 1 :
                                                                                                                                FUN 0 1 ?
                                                                                                                            FUN 0 2 []
                                                                                                                          FUN 2 1 .
                                                                                                                            FUN 3 6 clause
                                                                                                                              INT 0
                                                                                                                              FUN 1 16 mymeta_predicate
                                                                                                                                FUN 4 12 profile_data
                                                                                                                                  FUN 0 1 :
                                                                                                                                  FUN 0 1 ?
                                                                                                                                  FUN 0 1 ?
                                                                                                                                  FUN 0 1 ?
                                                                                                                              FUN 0 2 []
                                                                                                                            FUN 2 1 .
                                                                                                                              FUN 3 6 clause
                                                                                                                                INT 0
                                                                                                                                FUN 1 16 mymeta_predicate
                                                                                                                                  FUN 1 13 profile_reset
                                                                                                                                    FUN 0 1 :
                                                                                                                                FUN 0 2 []
                                                                                                                              FUN 2 1 .
                                                                                                                                FUN 3 6 clause
                                                                                                                                  INT 0
                                                                                                                                  FUN 1 16 mymeta_predicate
                                                                                                                                    FUN 1 9 reconsult
                                                                                                                                      FUN 0 1 :
                                                                                                                                  FUN 0 2 []
                                                                                                                                FUN 2 1 .
                                                                                                                                  FUN 3 6 clause
                                                                                                                                    INT 0
                                                                                                                                    FUN 1 16 mymeta_predicate
                                                                                                                                      FUN 1 7 require
                                                                                                                                        FUN 0 1 :
                                                                                                                                    FUN 0 2 []
                                                                                                                                  FUN 2 1 .
                                                                                                                                    FUN 3 6 clause
                                                                                                                                      INT 0
                                                                                                                                      FUN 1 16 mymeta_predicate
                                                                                                                                        FUN 1 7 retract
                                                                                                                                          FUN 0 1 :
                                                                                                                                      FUN 0 2 []
                                                                                                                                    FUN 2 1 .
                                                                                                                                      FUN 3 6 clause
                                                                                                                                        INT 0
                                                                                                                                        FUN 1 16 mymeta_predicate
                                                                                                                                          FUN 1 10 retractall
                                                                                                                                            FUN 0 1 :
                                                                                                                                        FUN 0 2 []
                                                                                                                                      FUN 2 1 .
                                                                                                                                        FUN 3 6 clause
                                                                                                                                          INT 0
                                                                                                                                          FUN 1 16 mymeta_predicate
                                                                                                                                            FUN 2 15 save_predicates
                                                                                                                                              FUN 0 1 :
                                                                                                                                              FUN 0 1 ?
                                                                                                                                          FUN 0 2 []
                                                                                                                                        FUN 2 1 .
                                                                                                                                          FUN 3 6 clause
                                                                                                                                            INT 0
                                                                                                                                            FUN 1 16 mymeta_predicate
                                                                                                                                              FUN 2 12 save_program
                                                                                                                                                FUN 0 1 ?
                                                                                                                                                FUN 0 1 :
                                                                                                                                            FUN 0 2 []
                                                                                                                                          FUN 2 1 .
                                                                                                                                            FUN 3 6 clause
                                                                                                                                              INT 0
                                                                                                                                              FUN 1 16 mymeta_predicate
                                                                                                                                                FUN 3 5 setof
                                                                                                                                                  FUN 0 1 ?
                                                                                                                                                  FUN 0 1 :
                                                                                                                                                  FUN 0 1 ?
                                                                                                                                              FUN 0 2 []
                                                                                                                                            FUN 2 1 .
                                                                                                                                              FUN 3 6 clause
                                                                                                                                                INT 0
                                                                                                                                                FUN 1 16 mymeta_predicate
                                                                                                                                                  FUN 1 4 undo
                                                                                                                                                    FUN 0 1 :
                                                                                                                                                FUN 0 2 []
                                                                                                                                              FUN 2 1 .
                                                                                                                                                FUN 3 6 clause
                                                                                                                                                  INT 0
                                                                                                                                                  FUN 1 16 mymeta_predicate
                                                                                                                                                    FUN 1 23 unload_foreign_resource
                                                                                                                                                      FUN 0 1 :
                                                                                                                                                  FUN 0 2 []
                                                                                                                                                FUN 2 1 .
                                                                                                                                                  FUN 3 6 clause
                                                                                                                                                    INT 0
                                                                                                                                                    FUN 1 16 mymeta_predicate
                                                                                                                                                      FUN 1 10 use_module
                                                                                                                                                        FUN 0 1 :
                                                                                                                                                    FUN 0 2 []
                                                                                                                                                  FUN 2 1 .
                                                                                                                                                    FUN 3 6 clause
                                                                                                                                                      INT 0
                                                                                                                                                      FUN 1 16 mymeta_predicate
                                                                                                                                                        FUN 2 10 use_module
                                                                                                                                                          FUN 0 1 :
                                                                                                                                                          FUN 0 1 ?
                                                                                                                                                      FUN 0 2 []
                                                                                                                                                    FUN 2 1 .
                                                                                                                                                      FUN 3 6 clause
                                                                                                                                                        INT 0
                                                                                                                                                        FUN 1 16 mymeta_predicate
                                                                                                                                                          FUN 3 10 use_module
                                                                                                                                                            FUN 0 1 ?
                                                                                                                                                            FUN 0 1 :
                                                                                                                                                            FUN 0 1 ?
                                                                                                                                                        FUN 0 2 []
                                                                                                                                                      FUN 2 1 .
                                                                                                                                                        FUN 3 6 clause
                                                                                                                                                          INT 0
                                                                                                                                                          FUN 1 16 mymeta_predicate
                                                                                                                                                            FUN 2 4 when
                                                                                                                                                              FUN 0 1 ?
                                                                                                                                                              FUN 0 1 :
                                                                                                                                                          FUN 0 2 []
                                                                                                                                                        FUN 0 2 []
CLS 3 14 modularize_all
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 14 modularize_all
        FUN 0 2 []
        VAR 0
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 14 modularize_all
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          FUN 2 1 .
            VAR 3
            VAR 4
        FUN 2 1 .
          FUN 3 10 modularize
            VAR 0
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 3 14 modularize_all
              VAR 1
              VAR 2
              VAR 4
            FUN 0 2 []
      FUN 0 2 []
CLS 5 16 modularize$3_0_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 8
      FUN 5 16 modularize$3_0_1
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 3 7 functor
          VAR 5
          VAR 4
          VAR 3
        FUN 2 1 .
          FUN 1 16 mymeta_predicate
            VAR 5
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 3 =..
                VAR 5
                FUN 2 1 .
                  VAR 6
                  VAR 7
              FUN 2 1 .
                FUN 4 15 modularize_args
                  VAR 2
                  VAR 7
                  VAR 1
                  VAR 0
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 5 16 modularize$3_0_1
          VAR 0
          VAR 1
          VAR 0
          VAR 2
          VAR 3
        FUN 0 2 []
      FUN 0 2 []
CLS 3 10 modularize
  FUN 2 1 .
    FUN 3 6 clause
      INT 8
      FUN 3 10 modularize
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 8 callable
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 7 functor
              VAR 0
              VAR 3
              VAR 4
            FUN 2 1 .
              FUN 2 3 =..
                VAR 0
                FUN 2 1 .
                  VAR 3
                  VAR 5
              FUN 2 1 .
                FUN 4 15 modularize_name
                  VAR 3
                  VAR 4
                  VAR 1
                  VAR 6
                FUN 2 1 .
                  FUN 5 16 modularize$3_0_1
                    VAR 7
                    VAR 1
                    VAR 5
                    VAR 4
                    VAR 3
                  FUN 2 1 .
                    FUN 2 3 =..
                      VAR 2
                      FUN 2 1 .
                        VAR 6
                        VAR 7
                    FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 10 modularize
          VAR 0
          VAR 1
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 4 15 modularize_name
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 15 modularize_name
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 12 lists:member
          FUN 2 1 -
            FUN 2 1 /
              VAR 0
              VAR 1
            VAR 4
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 11 module_name
              VAR 0
              VAR 4
              VAR 3
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 15 modularize_name
          VAR 0
          VAR 1
          VAR 2
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 4 21 modularize_args$4_1_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 21 modularize_args$4_1_1
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 2 ==
          VAR 3
          FUN 0 1 :
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 10 modularize
              VAR 2
              VAR 1
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 21 modularize_args$4_1_1
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 2 ==
            VAR 3
            FUN 0 6 dolist
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 17 modularize_dolist
                VAR 2
                VAR 1
                VAR 0
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 4 21 modularize_args$4_1_1
            VAR 0
            VAR 1
            VAR 0
            VAR 2
          FUN 0 2 []
        FUN 0 2 []
CLS 4 15 modularize_args
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 4 15 modularize_args
        FUN 0 2 []
        VAR 0
        VAR 1
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 7
        FUN 4 15 modularize_args
          FUN 2 1 .
            VAR 0
            VAR 1
          FUN 2 1 .
            VAR 2
            VAR 3
          VAR 4
          FUN 2 1 .
            VAR 5
            VAR 6
        FUN 2 1 .
          FUN 4 21 modularize_args$4_1_1
            VAR 5
            VAR 4
            VAR 0
            VAR 2
          FUN 2 1 .
            FUN 4 15 modularize_args
              VAR 1
              VAR 3
              VAR 4
              VAR 6
            FUN 0 2 []
      FUN 0 2 []
CLS 3 29 modularize_dolist$3_0_0$3_0_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 3 29 modularize_dolist$3_0_0$3_0_2
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 2
        FUN 2 1 .
          FUN 2 1 =
            VAR 2
            FUN 1 2 do
              VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 10 modularize
                VAR 3
                VAR 1
                VAR 4
              FUN 2 1 .
                FUN 2 1 =
                  VAR 0
                  FUN 1 2 do
                    VAR 4
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 29 modularize_dolist$3_0_0$3_0_2
          VAR 0
          VAR 1
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 3 23 modularize_dolist$3_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 3 23 modularize_dolist$3_0_0
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 2
        FUN 2 1 .
          FUN 2 1 =
            VAR 2
            FUN 2 1 .
              VAR 3
              VAR 4
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 29 modularize_dolist$3_0_0$3_0_2
                VAR 5
                VAR 1
                VAR 3
              FUN 2 1 .
                FUN 2 1 =
                  VAR 0
                  FUN 2 1 .
                    VAR 5
                    VAR 6
                FUN 2 1 .
                  FUN 3 17 modularize_dolist
                    VAR 4
                    VAR 1
                    VAR 6
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 23 modularize_dolist$3_0_0
          VAR 0
          VAR 1
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 3 17 modularize_dolist
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 17 modularize_dolist
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 23 modularize_dolist$3_0_0
          VAR 2
          VAR 1
          VAR 0
        FUN 0 2 []
    FUN 0 2 []
CLS 1 16 make_prefix_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 1 16 make_prefix_list
        VAR 0
      FUN 2 1 .
        FUN 1 14 module_private
          VAR 1
        FUN 2 1 .
          FUN 3 7 findall
            VAR 2
            FUN 2 11 conv import
              VAR 2
              VAR 3
            VAR 4
          FUN 2 1 .
            FUN 3 16 imported_modules
              VAR 4
              VAR 1
              VAR 0
            FUN 0 2 []
    FUN 0 2 []
CLS 1 14 module_private
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 1 14 module_private
        VAR 0
      FUN 2 1 .
        FUN 2 11 conv module
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 12 conv private
              VAR 3
            FUN 2 1 .
              FUN 4 18 module_prefix_list
                VAR 3
                VAR 1
                FUN 0 2 []
                VAR 0
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 14 module_private
          FUN 0 2 []
        FUN 0 2 []
      FUN 0 2 []
CLS 3 16 imported_modules
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 16 imported_modules
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 3 16 imported_modules
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 11 conv import
            VAR 0
            VAR 4
          FUN 2 1 .
            FUN 4 18 module_prefix_list
              VAR 4
              VAR 0
              VAR 2
              VAR 5
            FUN 2 1 .
              FUN 3 16 imported_modules
                VAR 1
                VAR 5
                VAR 3
              FUN 0 2 []
      FUN 0 2 []
CLS 4 18 module_prefix_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 4 18 module_prefix_list
        FUN 0 2 []
        VAR 0
        VAR 1
        VAR 1
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 4 18 module_prefix_list
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 4 18 module_prefix_list
            VAR 1
            VAR 2
            FUN 2 1 .
              FUN 2 1 -
                VAR 0
                VAR 2
              VAR 3
            VAR 4
          FUN 0 2 []
      FUN 0 2 []
CLS 0 10 write_file
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 0 10 write_file
      FUN 2 1 .
        FUN 1 16 make_prefix_list
          VAR 0
        FUN 2 1 .
          FUN 1 7 retract
            FUN 3 14 conv predicate
              VAR 1
              VAR 2
              VAR 3
          FUN 2 1 .
            FUN 2 10 write_pred
              FUN 2 1 -
                FUN 2 1 /
                  VAR 1
                  VAR 2
                VAR 3
              VAR 0
            FUN 2 1 .
              FUN 0 4 fail
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 0 10 write_file
        FUN 0 2 []
      FUN 0 2 []
CLS 2 10 write_pred
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 2 10 write_pred
        FUN 2 1 -
          FUN 2 1 /
            VAR 0
            VAR 1
          VAR 2
        VAR 3
      FUN 2 1 .
        FUN 3 11 module_term
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 4 10 write_func
            VAR 0
            VAR 1
            FUN 0 3 CLS
            INT 0
          FUN 2 1 .
            FUN 2 10 write_expr
              VAR 4
              INT 2
            FUN 0 2 []
    FUN 0 2 []
CLS 2 10 write_expr
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 10 write_expr
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 5 float
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 9 write_num
              VAR 0
              FUN 0 3 FLT
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 10 write_expr
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 7 integer
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 9 write_num
                VAR 0
                FUN 0 3 INT
                VAR 1
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 10 write_expr
            FUN 1 4 $VAR
              FUN 1 4 $VAR
                VAR 0
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 4 12 write_struct
                FUN 1 4 $VAR
                  VAR 0
                FUN 0 4 $VAR
                INT 1
                VAR 1
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 2 10 write_expr
              FUN 1 4 $VAR
                VAR 0
              VAR 1
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 3 9 write_num
                  VAR 0
                  FUN 0 3 VAR
                  VAR 1
                FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 4
              FUN 2 10 write_expr
                VAR 0
                VAR 1
              FUN 2 1 .
                FUN 3 7 functor
                  VAR 0
                  VAR 2
                  VAR 3
                FUN 2 1 .
                  FUN 1 4 atom
                    VAR 2
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 4 12 write_struct
                        VAR 0
                        VAR 2
                        VAR 3
                        VAR 1
                      FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 2
                FUN 2 10 write_expr
                  VAR 0
                  VAR 1
                FUN 2 1 .
                  FUN 1 5 write
                    FUN 0 26 WARNING: could not output 
                  FUN 2 1 .
                    FUN 1 5 write
                      VAR 0
                    FUN 2 1 .
                      FUN 0 2 nl
                      FUN 0 2 []
              FUN 0 2 []
CLS 4 12 write_struct
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 12 write_struct
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 4 10 write_func
          VAR 1
          VAR 2
          FUN 0 3 FUN
          VAR 3
        FUN 2 1 .
          FUN 2 2 is
            VAR 4
            FUN 2 1 +
              VAR 3
              INT 2
          FUN 2 1 .
            FUN 3 10 write_args
              VAR 0
              VAR 2
              VAR 4
            FUN 0 2 []
    FUN 0 2 []
CLS 3 10 write_args
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 3 10 write_args
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 9 mybetween
          INT 1
          VAR 1
          VAR 3
        FUN 2 1 .
          FUN 3 3 arg
            VAR 3
            VAR 0
            VAR 4
          FUN 2 1 .
            FUN 2 10 write_expr
              VAR 4
              VAR 2
            FUN 2 1 .
              FUN 0 4 fail
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 10 write_args
          VAR 0
          VAR 1
          VAR 2
        FUN 0 2 []
      FUN 0 2 []
CLS 3 9 write_num
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 9 write_num
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 9 write_tag
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 5 write
            VAR 0
          FUN 2 1 .
            FUN 0 2 nl
            FUN 0 2 []
    FUN 0 2 []
CLS 4 10 write_func
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 10 write_func
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 9 write_tag
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 1 5 write
            VAR 1
          FUN 2 1 .
            FUN 1 10 write_atom
              VAR 0
            FUN 2 1 .
              FUN 0 2 nl
              FUN 0 2 []
    FUN 0 2 []
CLS 2 9 write_tag
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 9 write_tag
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 5 mytab
          VAR 1
        FUN 2 1 .
          FUN 1 5 write
            VAR 0
          FUN 2 1 .
            FUN 1 5 write
              FUN 0 1  
            FUN 0 2 []
    FUN 0 2 []
CLS 1 10 write_atom
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 10 write_atom
        VAR 0
      FUN 2 1 .
        FUN 1 5 mytab
          INT 1
        FUN 2 1 .
          FUN 2 11 atom_length
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 1 5 write
              VAR 1
            FUN 2 1 .
              FUN 1 5 mytab
                INT 1
              FUN 2 1 .
                FUN 1 5 write
                  VAR 0
                FUN 0 2 []
    FUN 0 2 []
CLS 3 9 mybetween
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 9 mybetween
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 2 =<
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 1 =
            VAR 2
            VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 3 9 mybetween
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 1 <
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 2 is
              VAR 3
              FUN 2 1 +
                VAR 0
                INT 1
            FUN 2 1 .
              FUN 3 9 mybetween
                VAR 3
                VAR 1
                VAR 2
              FUN 0 2 []
      FUN 0 2 []
CLS 1 5 mytab
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 5 mytab
        VAR 0
      FUN 2 1 .
        FUN 2 1 >
          VAR 0
          INT 0
        FUN 2 1 .
          FUN 1 8 put_code
            INT 32
          FUN 2 1 .
            FUN 2 2 is
              VAR 1
              FUN 2 1 -
                VAR 0
                INT 1
            FUN 2 1 .
              FUN 1 5 mytab
                VAR 1
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 5 mytab
          INT 0
        FUN 0 2 []
      FUN 0 2 []
