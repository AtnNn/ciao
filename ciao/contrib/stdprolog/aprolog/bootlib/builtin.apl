CLS 2 2 is
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 2 is
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 5 $expr
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 2
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 2 is
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 17 arith:arith_error
            FUN 2 2 is
              VAR 0
              VAR 1
            FUN 2 1 .
              VAR 1
              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 3 =:=
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 3 =:=
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 5 $expr
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 2 5 $expr
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 3 $eq
                VAR 2
                VAR 3
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 3 =:=
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 17 arith:arith_error
            FUN 2 3 =:=
              VAR 0
              VAR 1
            FUN 2 1 .
              VAR 0
              FUN 2 1 .
                VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 3 =\=
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 3 =\=
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 5 $expr
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 2 5 $expr
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 4 $neq
                VAR 2
                VAR 3
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 3 =\=
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 17 arith:arith_error
            FUN 2 3 =\=
              VAR 0
              VAR 1
            FUN 2 1 .
              VAR 0
              FUN 2 1 .
                VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 1 <
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 1 <
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 5 $expr
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 2 5 $expr
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 5 $less
                VAR 2
                VAR 3
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 1 <
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 17 arith:arith_error
            FUN 2 1 <
              VAR 0
              VAR 1
            FUN 2 1 .
              VAR 0
              FUN 2 1 .
                VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 2 =<
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 2 =<
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 5 $expr
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 2 5 $expr
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 4 $leq
                VAR 2
                VAR 3
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 2 =<
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 17 arith:arith_error
            FUN 2 2 =<
              VAR 0
              VAR 1
            FUN 2 1 .
              VAR 0
              FUN 2 1 .
                VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 1 >
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 1 >
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 5 $expr
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 2 5 $expr
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 5 $less
                VAR 3
                VAR 2
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 1 >
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 17 arith:arith_error
            FUN 2 1 >
              VAR 0
              VAR 1
            FUN 2 1 .
              VAR 0
              FUN 2 1 .
                VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 2 >=
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 2 >=
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 5 $expr
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 2 5 $expr
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 4 $leq
                VAR 3
                VAR 2
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 2 >=
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 17 arith:arith_error
            FUN 2 2 >=
              VAR 0
              VAR 1
            FUN 2 1 .
              VAR 0
              FUN 2 1 .
                VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 17 arith:arith_error
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 17 arith:arith_error
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 6 $error
          VAR 2
        FUN 2 1 .
          FUN 3 16 arith:find_error
            VAR 2
            VAR 1
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 3 16 arith:find_error
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 16 arith:find_error
        FUN 0 12 zero_divisor
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 5 throw
          FUN 2 5 error
            FUN 1 16 evaluation_error
              FUN 0 12 zero_divisor
            VAR 1
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 16 arith:find_error
          FUN 0 14 float_overflow
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 5 throw
            FUN 2 5 error
              FUN 1 16 evaluation_error
                FUN 0 14 float_overflow
              VAR 1
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 16 arith:find_error
            FUN 0 9 undefined
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 1 5 throw
              FUN 2 5 error
                FUN 1 16 evaluation_error
                  FUN 0 9 undefined
                VAR 1
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 3 16 arith:find_error
              FUN 0 12 int_overflow
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 5 throw
                FUN 2 5 error
                  FUN 1 16 evaluation_error
                    FUN 0 12 int_overflow
                  VAR 1
              FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 2
              FUN 3 16 arith:find_error
                FUN 0 10 type_error
                VAR 0
                VAR 1
              FUN 2 1 .
                FUN 2 16 arith:check_list
                  VAR 0
                  VAR 1
                FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 3
                FUN 3 16 arith:find_error
                  VAR 0
                  VAR 1
                  VAR 2
                FUN 2 1 .
                  FUN 1 5 throw
                    FUN 2 5 error
                      FUN 0 13 unknown_error
                      VAR 2
                  FUN 0 2 []
              FUN 0 2 []
CLS 2 16 arith:check_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 16 arith:check_list
        FUN 2 1 .
          VAR 0
          VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 16 arith:check_expr
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 2 16 arith:check_list
            VAR 1
            VAR 2
          FUN 0 2 []
    FUN 0 2 []
CLS 3 22 arith:check_expr$2_2_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 22 arith:check_expr$2_2_0
        VAR 0
        VAR 1
        FUN 0 2 []
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 16 arith:eval_error
            FUN 2 1 /
              VAR 1
              INT 0
            VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 22 arith:check_expr$2_2_0
          VAR 0
          VAR 1
          FUN 2 1 .
            VAR 2
            FUN 0 2 []
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 13 arith:check_1
              VAR 1
              VAR 2
              VAR 0
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 3 22 arith:check_expr$2_2_0
            VAR 0
            VAR 1
            FUN 2 1 .
              VAR 2
              FUN 2 1 .
                VAR 3
                FUN 0 2 []
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 4 13 arith:check_2
                VAR 1
                VAR 2
                VAR 3
                VAR 0
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 4
            FUN 3 22 arith:check_expr$2_2_0
              VAR 0
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 2 12 lists:length
                VAR 2
                VAR 3
              FUN 2 1 .
                FUN 2 16 arith:eval_error
                  FUN 2 1 /
                    VAR 1
                    VAR 3
                  VAR 0
                FUN 0 2 []
          FUN 0 2 []
CLS 2 16 arith:check_expr
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 16 arith:check_expr
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 5 throw
              FUN 2 5 error
                FUN 0 19 instantiation_error
                VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 16 arith:check_expr
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 6 number
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 2 16 arith:check_expr
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 3 =..
              VAR 0
              FUN 2 1 .
                VAR 2
                VAR 3
            FUN 2 1 .
              FUN 3 22 arith:check_expr$2_2_0
                VAR 1
                VAR 2
                VAR 3
              FUN 0 2 []
        FUN 0 2 []
CLS 3 13 arith:check_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 3 13 arith:check_1
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 18 arith:check_name_1
          VAR 0
          VAR 3
          VAR 2
        FUN 2 1 .
          FUN 2 16 arith:check_expr
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 2 2 is
              VAR 4
              VAR 1
            FUN 2 1 .
              FUN 3 16 arith:check_type
                VAR 3
                VAR 4
                VAR 2
              FUN 0 2 []
    FUN 0 2 []
CLS 4 13 arith:check_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 4 13 arith:check_2
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 3 18 arith:check_name_2
          VAR 0
          VAR 4
          VAR 3
        FUN 2 1 .
          FUN 2 16 arith:check_expr
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 2 16 arith:check_expr
              VAR 2
              VAR 3
            FUN 2 1 .
              FUN 2 2 is
                VAR 5
                VAR 1
              FUN 2 1 .
                FUN 2 2 is
                  VAR 6
                  VAR 2
                FUN 2 1 .
                  FUN 3 16 arith:check_type
                    VAR 4
                    VAR 5
                    VAR 3
                  FUN 2 1 .
                    FUN 3 16 arith:check_type
                      VAR 4
                      VAR 6
                      VAR 3
                    FUN 0 2 []
    FUN 0 2 []
CLS 3 16 arith:check_type
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 16 arith:check_type
        FUN 0 2 if
        VAR 0
        VAR 1
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 16 arith:check_type
          FUN 0 1 i
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 7 integer
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 16 arith:check_type
            FUN 0 1 i
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 1 5 throw
              FUN 2 5 error
                FUN 2 10 type_error
                  FUN 0 7 integer
                  VAR 0
                VAR 1
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 3 16 arith:check_type
              FUN 0 1 f
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 5 float
                VAR 0
              FUN 2 1 .
                FUN 0 1 !
                FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 2
              FUN 3 16 arith:check_type
                FUN 0 1 f
                VAR 0
                VAR 1
              FUN 2 1 .
                FUN 1 5 throw
                  FUN 2 5 error
                    FUN 2 10 type_error
                      FUN 0 5 float
                      VAR 0
                    VAR 1
                FUN 0 2 []
            FUN 0 2 []
CLS 3 18 arith:check_name_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 18 arith:check_name_1
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 12 lists:member
          FUN 2 1 t
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 1 t
              FUN 0 1 \
              FUN 0 1 i
            FUN 2 1 .
              FUN 2 1 t
                FUN 0 18 float_integer_part
                FUN 0 1 f
              FUN 2 1 .
                FUN 2 1 t
                  FUN 0 21 float_fractional_part
                  FUN 0 1 f
                FUN 2 1 .
                  FUN 2 1 t
                    FUN 0 5 round
                    FUN 0 1 f
                  FUN 2 1 .
                    FUN 2 1 t
                      FUN 0 8 truncate
                      FUN 0 1 f
                    FUN 2 1 .
                      FUN 2 1 t
                        FUN 0 5 floor
                        FUN 0 1 f
                      FUN 2 1 .
                        FUN 2 1 t
                          FUN 0 7 ceiling
                          FUN 0 1 f
                        FUN 2 1 .
                          FUN 2 1 t
                            FUN 0 1 -
                            FUN 0 2 if
                          FUN 2 1 .
                            FUN 2 1 t
                              FUN 0 3 abs
                              FUN 0 2 if
                            FUN 2 1 .
                              FUN 2 1 t
                                FUN 0 4 sign
                                FUN 0 2 if
                              FUN 2 1 .
                                FUN 2 1 t
                                  FUN 0 3 sin
                                  FUN 0 2 if
                                FUN 2 1 .
                                  FUN 2 1 t
                                    FUN 0 3 cos
                                    FUN 0 2 if
                                  FUN 2 1 .
                                    FUN 2 1 t
                                      FUN 0 4 atan
                                      FUN 0 2 if
                                    FUN 2 1 .
                                      FUN 2 1 t
                                        FUN 0 4 sqrt
                                        FUN 0 2 if
                                      FUN 2 1 .
                                        FUN 2 1 t
                                          FUN 0 3 log
                                          FUN 0 2 if
                                        FUN 2 1 .
                                          FUN 2 1 t
                                            FUN 0 3 exp
                                            FUN 0 2 if
                                          FUN 2 1 .
                                            FUN 2 1 t
                                              FUN 0 5 float
                                              FUN 0 2 if
                                            FUN 0 2 []
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 18 arith:check_name_1
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 16 arith:eval_error
            FUN 2 1 /
              VAR 0
              INT 1
            VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 3 18 arith:check_name_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 18 arith:check_name_2
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 12 lists:member
          FUN 2 1 t
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 1 t
              FUN 0 2 //
              FUN 0 1 i
            FUN 2 1 .
              FUN 2 1 t
                FUN 0 3 rem
                FUN 0 1 i
              FUN 2 1 .
                FUN 2 1 t
                  FUN 0 3 mod
                  FUN 0 1 i
                FUN 2 1 .
                  FUN 2 1 t
                    FUN 0 2 /\
                    FUN 0 1 i
                  FUN 2 1 .
                    FUN 2 1 t
                      FUN 0 2 \/
                      FUN 0 1 i
                    FUN 2 1 .
                      FUN 2 1 t
                        FUN 0 2 <<
                        FUN 0 1 i
                      FUN 2 1 .
                        FUN 2 1 t
                          FUN 0 2 >>
                          FUN 0 1 i
                        FUN 2 1 .
                          FUN 2 1 t
                            FUN 0 1 +
                            FUN 0 2 if
                          FUN 2 1 .
                            FUN 2 1 t
                              FUN 0 1 -
                              FUN 0 2 if
                            FUN 2 1 .
                              FUN 2 1 t
                                FUN 0 1 *
                                FUN 0 2 if
                              FUN 2 1 .
                                FUN 2 1 t
                                  FUN 0 1 /
                                  FUN 0 2 if
                                FUN 2 1 .
                                  FUN 2 1 t
                                    FUN 0 2 **
                                    FUN 0 2 if
                                  FUN 0 2 []
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 18 arith:check_name_2
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 16 arith:eval_error
            FUN 2 1 /
              VAR 0
              INT 2
            VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 2 16 arith:eval_error
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 16 arith:eval_error
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 5 throw
          FUN 2 5 error
            FUN 2 10 type_error
              FUN 0 9 evaluable
              VAR 0
            VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 1 7 asserta
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 7 asserta
        FUN 2 2 :-
          VAR 0
          VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 21 database:asserta_conv
            VAR 0
            VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 7 asserta
          VAR 0
        FUN 2 1 .
          FUN 2 17 database:asserta2
            VAR 0
            FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 2 21 database:asserta_conv
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 21 database:asserta_conv
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 25 execution:convert_to_goal
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 17 database:asserta2
              VAR 0
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 21 database:asserta_conv
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 7 asserta
              FUN 2 2 :-
                VAR 0
                VAR 1
            FUN 2 1 .
              FUN 1 12 conv_to_goal
                VAR 1
              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 17 database:asserta2
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 17 database:asserta2
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 3 8 $asserta
          VAR 0
          FUN 2 1 .
            VAR 1
            FUN 0 2 []
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 17 database:asserta2
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 7 asserta
              VAR 2
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 8 callable
                  VAR 0
                FUN 2 1 .
                  FUN 1 16 static_procedure
                    VAR 0
                  FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 7 assertz
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 7 assertz
        FUN 2 2 :-
          VAR 0
          VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 21 database:assertz_conv
            VAR 0
            VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 7 assertz
          VAR 0
        FUN 2 1 .
          FUN 2 17 database:assertz2
            VAR 0
            FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 2 21 database:assertz_conv
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 21 database:assertz_conv
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 25 execution:convert_to_goal
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 17 database:assertz2
              VAR 0
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 21 database:assertz_conv
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 7 assertz
              FUN 2 2 :-
                VAR 0
                VAR 1
            FUN 2 1 .
              FUN 1 12 conv_to_goal
                VAR 1
              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 17 database:assertz2
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 17 database:assertz2
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 3 8 $assertz
          VAR 0
          FUN 2 1 .
            VAR 1
            FUN 0 2 []
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 17 database:assertz2
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 7 assertz
              VAR 2
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 8 callable
                  VAR 0
                FUN 2 1 .
                  FUN 1 16 static_procedure
                    VAR 0
                  FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 4 23 database:nth_clause_try
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 4 23 database:nth_clause_try
        VAR 0
        VAR 1
        VAR 1
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 4 23 database:nth_clause_try
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 2 is
            VAR 4
            FUN 2 1 +
              VAR 0
              INT 1
          FUN 2 1 .
            FUN 3 19 database:nth_clause
              VAR 4
              VAR 1
              VAR 3
            FUN 0 2 []
      FUN 0 2 []
CLS 3 19 database:nth_clause
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 3 19 database:nth_clause
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 7 $clause
          VAR 0
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 4 23 database:nth_clause_try
            VAR 0
            VAR 1
            FUN 2 1 -
              VAR 3
              VAR 4
            VAR 2
          FUN 0 2 []
    FUN 0 2 []
CLS 1 12 clause$2_0_3
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 12 clause$2_0_3
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 12 clause$2_0_3
          VAR 0
        FUN 2 1 .
          FUN 1 8 callable
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 2 6 clause
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 6 clause
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 12 clause$2_0_3
          VAR 1
        FUN 2 1 .
          FUN 2 11 $clauselist
            VAR 0
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 19 database:nth_clause
                VAR 2
                FUN 2 1 -
                  VAR 0
                  VAR 1
                VAR 3
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 6 clause
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 6 clause
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 8 callable
                  VAR 0
                FUN 2 1 .
                  FUN 1 8 callable
                    VAR 1
                  FUN 2 1 .
                    FUN 1 17 private_procedure
                      VAR 0
                    FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 7 retract
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 7 retract
        FUN 2 2 :-
          VAR 0
          VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 17 database:retract2
            VAR 0
            VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 7 retract
          VAR 0
        FUN 2 1 .
          FUN 2 17 database:retract2
            VAR 0
            FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 2 17 database:retract2
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 17 database:retract2
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 11 $clauselist
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 19 database:nth_clause
              VAR 2
              FUN 2 1 -
                VAR 0
                VAR 1
              VAR 3
            FUN 2 1 .
              FUN 1 8 $retract
                VAR 3
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 17 database:retract2
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 7 retract
              VAR 2
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 8 callable
                  VAR 0
                FUN 2 1 .
                  FUN 1 16 static_procedure
                    VAR 0
                  FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 19 database:retractall
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 19 database:retractall
        VAR 0
      FUN 2 1 .
        FUN 1 7 retract
          VAR 0
        FUN 2 1 .
          FUN 0 4 fail
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 19 database:retractall
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 1 13 abolish$1_0_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 13 abolish$1_0_2
        FUN 0 12 pred_dynamic
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 13 abolish$1_0_2
          FUN 0 2 []
        FUN 0 2 []
      FUN 0 2 []
CLS 1 7 abolish
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 1 7 abolish
        FUN 2 1 /
          VAR 0
          VAR 1
      FUN 2 1 .
        FUN 3 7 functor
          VAR 2
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 3 10 $pred_info
            VAR 2
            VAR 3
            VAR 4
          FUN 2 1 .
            FUN 1 13 abolish$1_0_2
              VAR 3
            FUN 2 1 .
              FUN 1 8 $abolish
                VAR 2
              FUN 2 1 .
                FUN 0 1 !
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 1 7 abolish
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 7 abolish
              VAR 0
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 8 pred_ind
                  VAR 0
                FUN 2 1 .
                  FUN 1 2 do
                    FUN 2 1 =
                      VAR 0
                      FUN 2 1 /
                        VAR 1
                        VAR 2
                  FUN 2 1 .
                    FUN 1 4 inst
                      VAR 1
                    FUN 2 1 .
                      FUN 1 4 inst
                        VAR 2
                      FUN 2 1 .
                        FUN 1 7 integer
                          VAR 2
                        FUN 2 1 .
                          FUN 1 6 nonneg
                            VAR 2
                          FUN 2 1 .
                            FUN 1 9 max_arity
                              VAR 2
                            FUN 2 1 .
                              FUN 1 2 do
                                FUN 3 7 functor
                                  VAR 3
                                  VAR 1
                                  VAR 2
                              FUN 2 1 .
                                FUN 1 16 static_procedure
                                  VAR 3
                                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 17 current_predicate
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 17 current_predicate
        VAR 0
      FUN 2 1 .
        FUN 1 23 database:check_pred_ind
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 24 database:list_predicates
              INT 0
              VAR 1
            FUN 2 1 .
              FUN 2 12 lists:member
                VAR 0
                VAR 1
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 17 current_predicate
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 17 current_predicate
              VAR 0
            FUN 2 1 .
              FUN 1 15 pred_ind_strict
                VAR 0
              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 24 database:list_predicates
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 2 24 database:list_predicates
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 12 $nth_functor
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 10 $pred_info
              VAR 2
              VAR 3
              VAR 4
            FUN 2 1 .
              FUN 5 21 database:is_predicate
                VAR 4
                VAR 3
                VAR 2
                VAR 1
                VAR 5
              FUN 2 1 .
                FUN 2 2 is
                  VAR 6
                  FUN 2 1 +
                    VAR 0
                    INT 1
                FUN 2 1 .
                  FUN 2 24 database:list_predicates
                    VAR 6
                    VAR 5
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 24 database:list_predicates
          VAR 0
          FUN 0 2 []
        FUN 0 2 []
      FUN 0 2 []
CLS 1 27 database:is_predicate$5_0_3
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 27 database:is_predicate$5_0_3
        FUN 0 11 pred_static
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 27 database:is_predicate$5_0_3
          FUN 0 12 pred_dynamic
        FUN 0 2 []
      FUN 0 2 []
CLS 5 21 database:is_predicate
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 5 21 database:is_predicate
        FUN 0 11 access_user
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 27 database:is_predicate$5_0_3
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 7 functor
              VAR 1
              VAR 4
              VAR 5
            FUN 2 1 .
              FUN 2 1 =
                VAR 2
                FUN 2 1 .
                  FUN 2 1 /
                    VAR 4
                    VAR 5
                  VAR 3
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 5 21 database:is_predicate
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 3
        FUN 0 2 []
      FUN 0 2 []
CLS 1 29 database:check_pred_ind$1_1_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 29 database:check_pred_ind$1_1_0
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 29 database:check_pred_ind$1_1_0
          VAR 0
        FUN 2 1 .
          FUN 1 7 integer
            VAR 0
          FUN 2 1 .
            FUN 2 2 >=
              VAR 0
              INT 0
            FUN 2 1 .
              FUN 2 1 <
                VAR 0
                INT 256
              FUN 0 2 []
      FUN 0 2 []
CLS 1 29 database:check_pred_ind$1_1_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 29 database:check_pred_ind$1_1_1
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 29 database:check_pred_ind$1_1_1
          VAR 0
        FUN 2 1 .
          FUN 1 4 atom
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 1 23 database:check_pred_ind
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 23 database:check_pred_ind
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 23 database:check_pred_ind
          FUN 2 1 /
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 29 database:check_pred_ind$1_1_1
            VAR 0
          FUN 2 1 .
            FUN 1 29 database:check_pred_ind$1_1_0
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 2 9 copy_term
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 9 copy_term
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 7 asserta
          FUN 1 7 $copied
            VAR 0
        FUN 2 1 .
          FUN 1 7 retract
            FUN 1 7 $copied
              VAR 2
          FUN 2 1 .
            FUN 2 1 =
              VAR 2
              VAR 1
            FUN 0 2 []
    FUN 0 2 []
CLS 2 12 dcg:dcg rule
  FUN 2 1 .
    FUN 3 6 clause
      INT 9
      FUN 2 12 dcg:dcg rule
        FUN 2 3 -->
          VAR 0
          VAR 1
        VAR 2
      FUN 2 1 .
        FUN 5 12 dcg:dcg head
          VAR 0
          VAR 3
          VAR 4
          VAR 5
          VAR 6
        FUN 2 1 .
          FUN 4 12 dcg:dcg body
            VAR 1
            VAR 7
            VAR 5
            VAR 6
          FUN 2 1 .
            FUN 3 12 dcg:dcg conj
              VAR 7
              VAR 4
              VAR 8
            FUN 2 1 .
              FUN 2 1 =
                VAR 2
                FUN 2 2 :-
                  VAR 3
                  VAR 8
              FUN 0 2 []
    FUN 0 2 []
CLS 5 12 dcg:dcg head
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 5 12 dcg:dcg head
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
          FUN 4 12 dcg:dcg goal
            VAR 0
            VAR 2
            VAR 4
            VAR 6
          FUN 2 1 .
            FUN 4 12 dcg:dcg body
              VAR 1
              VAR 3
              VAR 6
              VAR 5
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 5 12 dcg:dcg head
          VAR 0
          VAR 1
          FUN 0 4 true
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 4 12 dcg:dcg goal
            VAR 0
            VAR 1
            VAR 2
            VAR 3
          FUN 0 2 []
      FUN 0 2 []
CLS 4 12 dcg:dcg goal
  FUN 2 1 .
    FUN 3 6 clause
      INT 8
      FUN 4 12 dcg:dcg goal
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 3 7 functor
          VAR 0
          VAR 4
          VAR 5
        FUN 2 1 .
          FUN 2 2 is
            VAR 6
            FUN 2 1 +
              VAR 5
              INT 1
          FUN 2 1 .
            FUN 2 2 is
              VAR 7
              FUN 2 1 +
                VAR 5
                INT 2
            FUN 2 1 .
              FUN 3 7 functor
                VAR 1
                VAR 4
                VAR 7
              FUN 2 1 .
                FUN 3 3 arg
                  VAR 7
                  VAR 1
                  VAR 3
                FUN 2 1 .
                  FUN 3 3 arg
                    VAR 6
                    VAR 1
                    VAR 2
                  FUN 2 1 .
                    FUN 3 12 dcg:dcg args
                      VAR 5
                      VAR 0
                      VAR 1
                    FUN 0 2 []
    FUN 0 2 []
CLS 3 18 dcg:dcg args$3_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 18 dcg:dcg args$3_0_0
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 3 =:=
          VAR 2
          INT 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 0 4 true
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 18 dcg:dcg args$3_0_0
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 3 arg
            VAR 2
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 3 3 arg
              VAR 2
              VAR 0
              VAR 3
            FUN 2 1 .
              FUN 2 2 is
                VAR 4
                FUN 2 1 -
                  VAR 2
                  INT 1
              FUN 2 1 .
                FUN 3 12 dcg:dcg args
                  VAR 4
                  VAR 1
                  VAR 0
                FUN 0 2 []
      FUN 0 2 []
CLS 3 12 dcg:dcg args
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 12 dcg:dcg args
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 18 dcg:dcg args$3_0_0
          VAR 2
          VAR 1
          VAR 0
        FUN 0 2 []
    FUN 0 2 []
CLS 4 12 dcg:dcg body
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 12 dcg:dcg body
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 1
              FUN 3 6 phrase
                VAR 0
                VAR 2
                VAR 3
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 8
        FUN 4 12 dcg:dcg body
          FUN 2 1 ,
            VAR 0
            VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 12 dcg:dcg body
              VAR 0
              VAR 5
              VAR 3
              VAR 6
            FUN 2 1 .
              FUN 4 12 dcg:dcg body
                VAR 1
                VAR 7
                VAR 6
                VAR 4
              FUN 2 1 .
                FUN 3 12 dcg:dcg conj
                  VAR 5
                  VAR 7
                  VAR 2
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 7
          FUN 4 12 dcg:dcg body
            FUN 2 2 ->
              VAR 0
              VAR 1
            FUN 2 2 ->
              VAR 2
              VAR 3
            VAR 4
            VAR 5
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 4 12 dcg:dcg body
                VAR 0
                VAR 2
                VAR 4
                VAR 6
              FUN 2 1 .
                FUN 4 12 dcg:dcg body
                  VAR 1
                  VAR 3
                  VAR 6
                  VAR 5
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 6
            FUN 4 12 dcg:dcg body
              FUN 2 1 ;
                VAR 0
                VAR 1
              FUN 2 1 ;
                VAR 2
                VAR 3
              VAR 4
              VAR 5
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 4 12 dcg:dcg disj
                  VAR 0
                  VAR 2
                  VAR 4
                  VAR 5
                FUN 2 1 .
                  FUN 4 12 dcg:dcg disj
                    VAR 1
                    VAR 3
                    VAR 4
                    VAR 5
                  FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 2
              FUN 4 12 dcg:dcg body
                FUN 1 2 {}
                  VAR 0
                VAR 0
                VAR 1
                VAR 1
              FUN 2 1 .
                FUN 0 1 !
                FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 1
                FUN 4 12 dcg:dcg body
                  FUN 0 1 !
                  FUN 0 1 !
                  VAR 0
                  VAR 0
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 1
                  FUN 4 12 dcg:dcg body
                    FUN 0 2 []
                    FUN 0 4 true
                    VAR 0
                    VAR 0
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 8
                    FUN 4 12 dcg:dcg body
                      FUN 2 1 .
                        VAR 0
                        VAR 1
                      VAR 2
                      VAR 3
                      VAR 4
                    FUN 2 1 .
                      FUN 0 1 !
                      FUN 2 1 .
                        FUN 4 12 dcg:dcg term
                          VAR 0
                          VAR 5
                          VAR 3
                          VAR 6
                        FUN 2 1 .
                          FUN 4 12 dcg:dcg body
                            VAR 1
                            VAR 7
                            VAR 6
                            VAR 4
                          FUN 2 1 .
                            FUN 3 12 dcg:dcg conj
                              VAR 5
                              VAR 7
                              VAR 2
                            FUN 0 2 []
                  FUN 2 1 .
                    FUN 3 6 clause
                      INT 4
                      FUN 4 12 dcg:dcg body
                        VAR 0
                        VAR 1
                        VAR 2
                        VAR 3
                      FUN 2 1 .
                        FUN 4 12 dcg:dcg goal
                          VAR 0
                          VAR 1
                          VAR 2
                          VAR 3
                        FUN 0 2 []
                    FUN 0 2 []
CLS 4 12 dcg:dcg term
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 12 dcg:dcg term
        VAR 0
        FUN 3 5 dcg:C
          VAR 1
          VAR 0
          VAR 2
        VAR 1
        VAR 2
      FUN 0 2 []
    FUN 0 2 []
CLS 5 18 dcg:dcg disj$4_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 5 18 dcg:dcg disj$4_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 2 2 ==
          VAR 4
          VAR 3
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 12 dcg:dcg conj
              FUN 2 1 =
                VAR 4
                VAR 2
              VAR 1
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 5 18 dcg:dcg disj$4_0_0
          VAR 0
          VAR 0
          VAR 1
          VAR 2
          VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 4 12 dcg:dcg disj
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 4 12 dcg:dcg disj
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 4 12 dcg:dcg body
          VAR 0
          VAR 4
          VAR 5
          VAR 3
        FUN 2 1 .
          FUN 5 18 dcg:dcg disj$4_0_0
            VAR 1
            VAR 4
            VAR 2
            VAR 3
            VAR 5
          FUN 0 2 []
    FUN 0 2 []
CLS 3 12 dcg:dcg conj
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 12 dcg:dcg conj
        VAR 0
        FUN 0 4 true
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 12 dcg:dcg conj
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 12 dcg:dcg CONJ
            VAR 0
            VAR 1
            VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 3 12 dcg:dcg CONJ
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 12 dcg:dcg CONJ
        FUN 0 4 true
        VAR 0
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 3 12 dcg:dcg CONJ
          FUN 2 1 ,
            VAR 0
            VAR 1
          VAR 2
          FUN 2 1 ,
            VAR 0
            VAR 3
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 12 dcg:dcg CONJ
              VAR 1
              VAR 2
              VAR 3
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 12 dcg:dcg CONJ
            VAR 0
            VAR 1
            FUN 2 1 ,
              VAR 0
              VAR 1
          FUN 0 2 []
        FUN 0 2 []
CLS 3 5 dcg:C
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 5 dcg:C
        FUN 2 1 .
          VAR 0
          VAR 1
        VAR 0
        VAR 1
      FUN 0 2 []
    FUN 0 2 []
CLS 2 6 phrase
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 6 phrase
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 3 6 phrase
          VAR 0
          VAR 1
          FUN 0 2 []
        FUN 0 2 []
    FUN 0 2 []
CLS 3 6 phrase
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 3 6 phrase
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 4 12 dcg:dcg body
          VAR 0
          VAR 3
          VAR 4
          VAR 5
        FUN 2 1 .
          FUN 2 1 =
            VAR 4
            VAR 1
          FUN 2 1 .
            FUN 2 1 =
              VAR 5
              VAR 2
            FUN 2 1 .
              FUN 1 4 call
                VAR 3
              FUN 0 2 []
    FUN 0 2 []
CLS 0 5 trace
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 0 5 trace
      FUN 2 1 .
        FUN 1 19 database:retractall
          FUN 1 7 $trmode
            VAR 0
        FUN 2 1 .
          FUN 1 7 asserta
            FUN 1 7 $trmode
              FUN 0 5 creep
          FUN 2 1 .
            FUN 1 6 $trace
              INT 1
            FUN 0 2 []
    FUN 0 2 []
CLS 0 7 notrace
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 0 7 notrace
      FUN 2 1 .
        FUN 1 6 $trace
          INT -1
        FUN 2 1 .
          FUN 1 19 database:retractall
            FUN 1 7 $trmode
              VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 1 11 debug:debug
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 11 debug:debug
        VAR 0
      FUN 2 1 .
        FUN 2 11 $get_global
          FUN 0 10 $can_debug
          FUN 0 4 true
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 11 $set_global
              FUN 0 10 $can_debug
              FUN 0 5 false
            FUN 2 1 .
              FUN 1 14 debug:debug_do
                VAR 0
              FUN 2 1 .
                FUN 2 11 $set_global
                  FUN 0 10 $can_debug
                  FUN 0 4 true
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 11 debug:debug
          VAR 0
        FUN 2 1 .
          FUN 3 10 $pred_info
            VAR 0
            VAR 1
            FUN 0 11 access_user
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 14 debug:debug_do
                VAR 0
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 1 11 debug:debug
            VAR 0
          FUN 2 1 .
            FUN 1 6 $trace
              INT 0
            FUN 2 1 .
              VAR 0
              FUN 0 2 []
        FUN 0 2 []
CLS 1 14 debug:debug_do
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 14 debug:debug_do
        VAR 0
      FUN 2 1 .
        FUN 1 20 debug:new_invocation
          VAR 1
        FUN 2 1 .
          FUN 2 15 debug:debug_try
            VAR 1
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 1 20 debug:new_invocation
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 20 debug:new_invocation
        FUN 2 1 -
          VAR 0
          VAR 1
      FUN 2 1 .
        FUN 2 11 $get_global
          FUN 0 12 $debug_level
          VAR 1
        FUN 2 1 .
          FUN 2 11 $get_global
            FUN 0 15 $invocation_num
            VAR 2
          FUN 2 1 .
            FUN 2 2 is
              VAR 0
              FUN 2 1 +
                VAR 2
                INT 1
            FUN 2 1 .
              FUN 2 11 $set_global
                FUN 0 15 $invocation_num
                VAR 0
              FUN 0 2 []
    FUN 0 2 []
CLS 2 15 debug:debug_try
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 15 debug:debug_try
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 4 20 execution:catch_some
          FUN 2 17 debug:debug_enter
            VAR 0
            VAR 1
          FUN 0 6 $retry
          FUN 2 15 debug:debug_try
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 0 6 $abort
            FUN 0 2 []
        FUN 0 2 []
    FUN 0 2 []
CLS 2 17 debug:debug_enter
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 2 17 debug:debug_enter
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 11 debug:enter
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 1 =
            VAR 0
            FUN 2 1 -
              VAR 2
              VAR 3
          FUN 2 1 .
            FUN 2 2 is
              VAR 4
              FUN 2 1 +
                VAR 3
                INT 1
            FUN 2 1 .
              FUN 2 11 $set_global
                FUN 0 12 $debug_level
                VAR 4
              FUN 2 1 .
                FUN 3 5 catch
                  FUN 1 16 debug:debug_call
                    VAR 1
                  VAR 5
                  FUN 3 15 debug:exception
                    VAR 0
                    VAR 1
                    VAR 5
                FUN 2 1 .
                  FUN 2 11 $set_global
                    FUN 0 12 $debug_level
                    VAR 3
                  FUN 2 1 .
                    FUN 2 11 debug:leave
                      VAR 0
                      VAR 1
                    FUN 0 2 []
    FUN 0 2 []
CLS 2 11 debug:enter
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 11 debug:enter
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 3 10 debug:port
          FUN 0 4 call
          VAR 0
          VAR 1
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 11 debug:enter
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 18 debug:restore_skip
            VAR 0
          FUN 2 1 .
            FUN 3 10 debug:port
              FUN 0 4 fail
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 0 4 fail
              FUN 0 2 []
      FUN 0 2 []
CLS 2 11 debug:leave
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 11 debug:leave
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 18 debug:restore_skip
          VAR 0
        FUN 2 1 .
          FUN 3 10 debug:port
            FUN 0 4 exit
            VAR 0
            VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 11 debug:leave
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 3 10 debug:port
            FUN 0 4 redo
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 0 4 fail
            FUN 0 2 []
      FUN 0 2 []
CLS 3 15 debug:exception
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 15 debug:exception
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 18 debug:restore_skip
          VAR 0
        FUN 2 1 .
          FUN 3 10 debug:port
            FUN 0 9 exception
            VAR 0
            FUN 2 1 -
              VAR 1
              VAR 2
          FUN 2 1 .
            FUN 1 5 throw
              VAR 2
            FUN 0 2 []
    FUN 0 2 []
CLS 1 16 debug:debug_call
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 16 debug:debug_call
        VAR 0
      FUN 2 1 .
        FUN 3 10 $pred_info
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 16 debug:debug_type
            VAR 2
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 2 16 debug:debug_type
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 16 debug:debug_type
        FUN 0 13 access_prolog
        VAR 0
      FUN 2 1 .
        FUN 1 6 $trace
          INT 0
        FUN 2 1 .
          VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 16 debug:debug_type
          FUN 0 11 access_user
          VAR 0
        FUN 2 1 .
          FUN 2 11 $set_global
            FUN 0 10 $can_debug
            FUN 0 4 true
          FUN 2 1 .
            FUN 1 6 $trace
              INT 0
            FUN 2 1 .
              VAR 0
              FUN 2 1 .
                FUN 2 11 $set_global
                  FUN 0 10 $can_debug
                  FUN 0 5 false
                FUN 0 2 []
      FUN 0 2 []
CLS 3 10 debug:port
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 10 debug:port
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 14 debug:get_mode
          FUN 0 5 creep
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 18 io:default_streams
              FUN 3 17 debug:port_action
                VAR 0
                VAR 1
                VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 10 debug:port
          VAR 0
          VAR 1
          VAR 2
        FUN 0 2 []
      FUN 0 2 []
CLS 3 17 debug:port_action
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 3 17 debug:port_action
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 17 debug:write_debug
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 12 debug:prompt
            VAR 3
          FUN 2 1 .
            FUN 3 16 debug:deb_action
              VAR 3
              VAR 0
              VAR 1
            FUN 0 2 []
    FUN 0 2 []
CLS 3 16 debug:deb_action
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 16 debug:deb_action
        INT 97
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 0 15 execution:abort
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 16 debug:deb_action
          INT 115
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 13 debug:in_port
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 15 debug:skip_this
                VAR 1
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 16 debug:deb_action
            INT 114
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 5 throw
                FUN 0 6 $retry
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 3 16 debug:deb_action
              INT 108
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 1 14 debug:set_mode
                  FUN 0 4 leap
                FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 3
              FUN 3 16 debug:deb_action
                VAR 0
                VAR 1
                VAR 2
              FUN 0 2 []
            FUN 0 2 []
CLS 1 13 debug:in_port
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 13 debug:in_port
        FUN 0 4 call
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 13 debug:in_port
          FUN 0 4 redo
        FUN 0 2 []
      FUN 0 2 []
CLS 1 15 debug:skip_this
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 15 debug:skip_this
        VAR 0
      FUN 2 1 .
        FUN 2 1 =
          VAR 0
          FUN 2 1 -
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 1 7 asserta
            FUN 1 16 $skip_invocation
              VAR 1
          FUN 2 1 .
            FUN 1 14 debug:set_mode
              FUN 0 4 skip
            FUN 0 2 []
    FUN 0 2 []
CLS 1 18 debug:restore_skip
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 18 debug:restore_skip
        VAR 0
      FUN 2 1 .
        FUN 2 1 =
          VAR 0
          FUN 2 1 -
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 1 7 retract
            FUN 1 16 $skip_invocation
              VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 14 debug:set_mode
                FUN 0 5 creep
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 18 debug:restore_skip
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 3 17 debug:write_debug
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 3 17 debug:write_debug
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 1 =
          VAR 1
          FUN 2 1 -
            VAR 3
            VAR 4
        FUN 2 1 .
          FUN 1 5 write
            VAR 3
          FUN 2 1 .
            FUN 1 8 put_code
              INT 9
            FUN 2 1 .
              FUN 1 5 write
                VAR 4
              FUN 2 1 .
                FUN 1 8 put_code
                  INT 9
                FUN 2 1 .
                  FUN 2 15 debug:port_name
                    VAR 0
                    VAR 5
                  FUN 2 1 .
                    FUN 1 5 write
                      VAR 5
                    FUN 2 1 .
                      FUN 1 5 write
                        FUN 0 2 : 
                      FUN 2 1 .
                        FUN 2 16 debug:write_goal
                          VAR 0
                          VAR 2
                        FUN 2 1 .
                          FUN 1 5 write
                            FUN 0 3  ? 
                          FUN 0 2 []
    FUN 0 2 []
CLS 2 16 debug:write_goal
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 16 debug:write_goal
        FUN 0 9 exception
        FUN 2 1 -
          VAR 0
          VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 5 write
            VAR 1
          FUN 2 1 .
            FUN 1 5 write
              FUN 0 3  - 
            FUN 2 1 .
              FUN 1 5 write
                VAR 0
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 16 debug:write_goal
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 5 write
            VAR 1
          FUN 0 2 []
      FUN 0 2 []
CLS 2 15 debug:port_name
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 15 debug:port_name
        FUN 0 4 call
        FUN 0 4 Call
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 2 15 debug:port_name
          FUN 0 4 exit
          FUN 0 4 Exit
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 0
          FUN 2 15 debug:port_name
            FUN 0 4 fail
            FUN 0 4 Fail
          FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 0
            FUN 2 15 debug:port_name
              FUN 0 4 redo
              FUN 0 4 Redo
            FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 0
              FUN 2 15 debug:port_name
                FUN 0 9 exception
                FUN 0 9 Exception
              FUN 0 2 []
            FUN 0 2 []
CLS 1 12 debug:prompt
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 12 debug:prompt
        VAR 0
      FUN 2 1 .
        FUN 1 12 io:read_line
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 1 14 debug:set_mode
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 14 debug:set_mode
        VAR 0
      FUN 2 1 .
        FUN 1 7 retract
          FUN 1 7 $trmode
            VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 7 asserta
              FUN 1 7 $trmode
                VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 14 debug:set_mode
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 1 14 debug:get_mode
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 14 debug:get_mode
        VAR 0
      FUN 2 1 .
        FUN 2 6 clause
          FUN 1 7 $trmode
            VAR 0
          VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 0 11 debug:reset
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 11 debug:reset
      FUN 2 1 .
        FUN 2 11 $set_global
          FUN 0 15 $invocation_num
          INT 0
        FUN 2 1 .
          FUN 2 11 $set_global
            FUN 0 12 $debug_level
            INT 1
          FUN 0 2 []
    FUN 0 2 []
CLS 2 13 err:err_check
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 13 err:err_check
        VAR 0
        FUN 0 2 []
      FUN 2 1 .
        FUN 1 5 throw
          FUN 2 5 error
            FUN 0 13 unknown_error
            VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 13 err:err_check
          VAR 0
          FUN 2 1 .
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 2 13 err:check_one
            VAR 1
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 13 err:err_check
                VAR 0
                VAR 2
              FUN 0 2 []
      FUN 0 2 []
CLS 1 19 err:check_one$2_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 19 err:check_one$2_0_0
        VAR 0
      FUN 2 1 .
        FUN 1 4 call
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 19 err:check_one$2_0_0
          VAR 0
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 1 20 err:check_one$2_21_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 20 err:check_one$2_21_1
        VAR 0
      FUN 2 1 .
        FUN 2 1 <
          VAR 0
          INT 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 20 err:check_one$2_21_1
          VAR 0
        FUN 2 1 .
          FUN 2 1 >
            VAR 0
            INT 1200
          FUN 0 2 []
      FUN 0 2 []
CLS 2 20 err:check_one$2_30_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 20 err:check_one$2_30_1
        VAR 0
        FUN 0 4 text
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 0
            FUN 0 13 binary_stream
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 20 err:check_one$2_30_1
          FUN 0 11 text_stream
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 2 13 err:check_one
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 13 err:check_one
        FUN 1 2 do
          VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 19 err:check_one$2_0_0
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 13 err:check_one
          FUN 2 2 if
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 4 call
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 13 err:check_one
                VAR 1
                VAR 2
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 13 err:check_one
            FUN 1 4 inst
              VAR 0
            VAR 1
          FUN 2 1 .
            FUN 1 3 var
              VAR 0
            FUN 2 1 .
              FUN 2 14 err:throwerror
                FUN 0 19 instantiation_error
                VAR 1
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 2 13 err:check_one
              FUN 1 3 var
                VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 6 nonvar
                VAR 0
              FUN 2 1 .
                FUN 2 14 err:throwerror
                  FUN 2 10 type_error
                    FUN 0 8 variable
                    VAR 0
                  VAR 1
                FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 2
              FUN 2 13 err:check_one
                FUN 1 7 integer
                  VAR 0
                VAR 1
              FUN 2 1 .
                FUN 1 6 nonvar
                  VAR 0
                FUN 2 1 .
                  FUN 1 2 \+
                    FUN 1 7 integer
                      VAR 0
                  FUN 2 1 .
                    FUN 2 14 err:throwerror
                      FUN 2 10 type_error
                        FUN 0 7 integer
                        VAR 0
                      VAR 1
                    FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 2
                FUN 2 13 err:check_one
                  FUN 1 6 number
                    VAR 0
                  VAR 1
                FUN 2 1 .
                  FUN 1 6 nonvar
                    VAR 0
                  FUN 2 1 .
                    FUN 1 2 \+
                      FUN 1 6 number
                        VAR 0
                    FUN 2 1 .
                      FUN 2 14 err:throwerror
                        FUN 2 10 type_error
                          FUN 0 6 number
                          VAR 0
                        VAR 1
                      FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 2
                  FUN 2 13 err:check_one
                    FUN 1 4 atom
                      VAR 0
                    VAR 1
                  FUN 2 1 .
                    FUN 1 6 nonvar
                      VAR 0
                    FUN 2 1 .
                      FUN 1 2 \+
                        FUN 1 4 atom
                          VAR 0
                      FUN 2 1 .
                        FUN 2 14 err:throwerror
                          FUN 2 10 type_error
                            FUN 0 4 atom
                            VAR 0
                          VAR 1
                        FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 2
                    FUN 2 13 err:check_one
                      FUN 1 6 atomic
                        VAR 0
                      VAR 1
                    FUN 2 1 .
                      FUN 1 6 nonvar
                        VAR 0
                      FUN 2 1 .
                        FUN 1 2 \+
                          FUN 1 6 atomic
                            VAR 0
                        FUN 2 1 .
                          FUN 2 14 err:throwerror
                            FUN 2 10 type_error
                              FUN 0 6 atomic
                              VAR 0
                            VAR 1
                          FUN 0 2 []
                  FUN 2 1 .
                    FUN 3 6 clause
                      INT 2
                      FUN 2 13 err:check_one
                        FUN 1 8 compound
                          VAR 0
                        VAR 1
                      FUN 2 1 .
                        FUN 1 6 nonvar
                          VAR 0
                        FUN 2 1 .
                          FUN 1 2 \+
                            FUN 1 8 compound
                              VAR 0
                          FUN 2 1 .
                            FUN 2 14 err:throwerror
                              FUN 2 10 type_error
                                FUN 0 8 compound
                                VAR 0
                              VAR 1
                            FUN 0 2 []
                    FUN 2 1 .
                      FUN 3 6 clause
                        INT 2
                        FUN 2 13 err:check_one
                          FUN 1 8 callable
                            VAR 0
                          VAR 1
                        FUN 2 1 .
                          FUN 1 6 nonvar
                            VAR 0
                          FUN 2 1 .
                            FUN 1 2 \+
                              FUN 1 8 callable
                                VAR 0
                            FUN 2 1 .
                              FUN 2 14 err:throwerror
                                FUN 2 10 type_error
                                  FUN 0 8 callable
                                  VAR 0
                                VAR 1
                              FUN 0 2 []
                      FUN 2 1 .
                        FUN 3 6 clause
                          INT 3
                          FUN 2 13 err:check_one
                            FUN 1 12 conv_to_goal
                              VAR 0
                            VAR 1
                          FUN 2 1 .
                            FUN 1 6 nonvar
                              VAR 0
                            FUN 2 1 .
                              FUN 1 2 \+
                                FUN 2 25 execution:convert_to_goal
                                  VAR 0
                                  VAR 2
                              FUN 2 1 .
                                FUN 2 14 err:throwerror
                                  FUN 2 10 type_error
                                    FUN 0 8 callable
                                    VAR 0
                                  VAR 1
                                FUN 0 2 []
                        FUN 2 1 .
                          FUN 3 6 clause
                            INT 2
                            FUN 2 13 err:check_one
                              FUN 1 9 character
                                VAR 0
                              VAR 1
                            FUN 2 1 .
                              FUN 1 6 nonvar
                                VAR 0
                              FUN 2 1 .
                                FUN 1 2 \+
                                  FUN 1 11 err:is_char
                                    VAR 0
                                FUN 2 1 .
                                  FUN 2 14 err:throwerror
                                    FUN 2 10 type_error
                                      FUN 0 9 character
                                      VAR 0
                                    VAR 1
                                  FUN 0 2 []
                          FUN 2 1 .
                            FUN 3 6 clause
                              INT 2
                              FUN 2 13 err:check_one
                                FUN 1 4 byte
                                  VAR 0
                                VAR 1
                              FUN 2 1 .
                                FUN 1 6 nonvar
                                  VAR 0
                                FUN 2 1 .
                                  FUN 1 2 \+
                                    FUN 2 1 ,
                                      FUN 1 7 integer
                                        VAR 0
                                      FUN 2 1 ,
                                        FUN 2 2 >=
                                          VAR 0
                                          INT 0
                                        FUN 2 1 <
                                          VAR 0
                                          INT 256
                                  FUN 2 1 .
                                    FUN 2 14 err:throwerror
                                      FUN 2 10 type_error
                                        FUN 0 4 byte
                                        VAR 0
                                      VAR 1
                                    FUN 0 2 []
                            FUN 2 1 .
                              FUN 3 6 clause
                                INT 2
                                FUN 2 13 err:check_one
                                  FUN 1 4 list
                                    VAR 0
                                  VAR 1
                                FUN 2 1 .
                                  FUN 1 2 \+
                                    FUN 1 26 lists:list_or_partial_list
                                      VAR 0
                                  FUN 2 1 .
                                    FUN 2 14 err:throwerror
                                      FUN 2 10 type_error
                                        FUN 0 4 list
                                        VAR 0
                                      VAR 1
                                    FUN 0 2 []
                              FUN 2 1 .
                                FUN 3 6 clause
                                  INT 2
                                  FUN 2 13 err:check_one
                                    FUN 1 8 instlist
                                      VAR 0
                                    VAR 1
                                  FUN 2 1 .
                                    FUN 1 2 \+
                                      FUN 1 11 err:is_list
                                        VAR 0
                                    FUN 2 1 .
                                      FUN 2 14 err:throwerror
                                        FUN 0 19 instantiation_error
                                        VAR 1
                                      FUN 0 2 []
                                FUN 2 1 .
                                  FUN 3 6 clause
                                    INT 2
                                    FUN 2 13 err:check_one
                                      FUN 1 16 instlistelements
                                        VAR 0
                                      VAR 1
                                    FUN 2 1 .
                                      FUN 1 2 \+
                                        FUN 1 22 err:inst_list_elements
                                          VAR 0
                                      FUN 2 1 .
                                        FUN 2 14 err:throwerror
                                          FUN 0 19 instantiation_error
                                          VAR 1
                                        FUN 0 2 []
                                  FUN 2 1 .
                                    FUN 3 6 clause
                                      INT 2
                                      FUN 2 13 err:check_one
                                        FUN 1 12 nonemptylist
                                          VAR 0
                                        VAR 1
                                      FUN 2 1 .
                                        FUN 1 6 nonvar
                                          VAR 0
                                        FUN 2 1 .
                                          FUN 2 1 =
                                            VAR 0
                                            FUN 0 2 []
                                          FUN 2 1 .
                                            FUN 2 14 err:throwerror
                                              FUN 2 12 domain_error
                                                FUN 0 14 non_empty_list
                                                VAR 0
                                              VAR 1
                                            FUN 0 2 []
                                    FUN 2 1 .
                                      FUN 3 6 clause
                                        INT 4
                                        FUN 2 13 err:check_one
                                          FUN 1 8 pred_ind
                                            VAR 0
                                          VAR 1
                                        FUN 2 1 .
                                          FUN 1 6 nonvar
                                            VAR 0
                                          FUN 2 1 .
                                            FUN 2 2 \=
                                              VAR 0
                                              FUN 2 1 /
                                                VAR 2
                                                VAR 3
                                            FUN 2 1 .
                                              FUN 2 14 err:throwerror
                                                FUN 2 10 type_error
                                                  FUN 0 19 predicate_indicator
                                                  VAR 0
                                                VAR 1
                                              FUN 0 2 []
                                      FUN 2 1 .
                                        FUN 3 6 clause
                                          INT 2
                                          FUN 2 13 err:check_one
                                            FUN 1 15 pred_ind_strict
                                              VAR 0
                                            VAR 1
                                          FUN 2 1 .
                                            FUN 1 2 \+
                                              FUN 1 23 database:check_pred_ind
                                                VAR 0
                                            FUN 2 1 .
                                              FUN 2 14 err:throwerror
                                                FUN 2 10 type_error
                                                  FUN 0 19 predicate_indicator
                                                  VAR 0
                                                VAR 1
                                              FUN 0 2 []
                                        FUN 2 1 .
                                          FUN 3 6 clause
                                            INT 2
                                            FUN 2 13 err:check_one
                                              FUN 1 6 nonneg
                                                VAR 0
                                              VAR 1
                                            FUN 2 1 .
                                              FUN 1 7 integer
                                                VAR 0
                                              FUN 2 1 .
                                                FUN 2 1 <
                                                  VAR 0
                                                  INT 0
                                                FUN 2 1 .
                                                  FUN 2 14 err:throwerror
                                                    FUN 2 12 domain_error
                                                      FUN 0 18 not_less_than_zero
                                                      VAR 0
                                                    VAR 1
                                                  FUN 0 2 []
                                          FUN 2 1 .
                                            FUN 3 6 clause
                                              INT 3
                                              FUN 2 13 err:check_one
                                                FUN 1 9 max_arity
                                                  VAR 0
                                                VAR 1
                                              FUN 2 1 .
                                                FUN 1 7 integer
                                                  VAR 0
                                                FUN 2 1 .
                                                  FUN 2 19 current_prolog_flag
                                                    FUN 0 9 max_arity
                                                    VAR 2
                                                  FUN 2 1 .
                                                    FUN 2 1 >
                                                      VAR 0
                                                      VAR 2
                                                    FUN 2 1 .
                                                      FUN 2 14 err:throwerror
                                                        FUN 1 20 representation_error
                                                          FUN 0 9 max_arity
                                                        VAR 1
                                                      FUN 0 2 []
                                            FUN 2 1 .
                                              FUN 3 6 clause
                                                INT 2
                                                FUN 2 13 err:check_one
                                                  FUN 1 7 op_prec
                                                    VAR 0
                                                  VAR 1
                                                FUN 2 1 .
                                                  FUN 1 7 integer
                                                    VAR 0
                                                  FUN 2 1 .
                                                    FUN 1 20 err:check_one$2_21_1
                                                      VAR 0
                                                    FUN 2 1 .
                                                      FUN 2 14 err:throwerror
                                                        FUN 2 12 domain_error
                                                          FUN 0 17 operator_priority
                                                          VAR 0
                                                        VAR 1
                                                      FUN 0 2 []
                                              FUN 2 1 .
                                                FUN 3 6 clause
                                                  INT 3
                                                  FUN 2 13 err:check_one
                                                    FUN 1 7 op_type
                                                      VAR 0
                                                    VAR 1
                                                  FUN 2 1 .
                                                    FUN 1 2 \+
                                                      FUN 2 16 term_io:op_class
                                                        VAR 0
                                                        VAR 2
                                                    FUN 2 1 .
                                                      FUN 2 14 err:throwerror
                                                        FUN 2 12 domain_error
                                                          FUN 0 18 operator_specifier
                                                          VAR 0
                                                        VAR 1
                                                      FUN 0 2 []
                                                FUN 2 1 .
                                                  FUN 3 6 clause
                                                    INT 2
                                                    FUN 2 13 err:check_one
                                                      FUN 1 9 op_modify
                                                        VAR 0
                                                      VAR 1
                                                    FUN 2 1 .
                                                      FUN 1 6 nonvar
                                                        VAR 0
                                                      FUN 2 1 .
                                                        FUN 2 1 =
                                                          VAR 0
                                                          FUN 0 1 ,
                                                        FUN 2 1 .
                                                          FUN 2 14 err:throwerror
                                                            FUN 3 16 permission_error
                                                              FUN 0 6 modify
                                                              FUN 0 8 operator
                                                              VAR 0
                                                            VAR 1
                                                          FUN 0 2 []
                                                  FUN 2 1 .
                                                    FUN 3 6 clause
                                                      INT 3
                                                      FUN 2 13 err:check_one
                                                        FUN 2 9 op_create
                                                          VAR 0
                                                          VAR 1
                                                        VAR 2
                                                      FUN 2 1 .
                                                        FUN 2 19 term_io:op_conflict
                                                          VAR 0
                                                          VAR 1
                                                        FUN 2 1 .
                                                          FUN 2 14 err:throwerror
                                                            FUN 3 16 permission_error
                                                              FUN 0 6 create
                                                              FUN 0 8 operator
                                                              VAR 0
                                                            VAR 2
                                                          FUN 0 2 []
                                                    FUN 2 1 .
                                                      FUN 3 6 clause
                                                        INT 2
                                                        FUN 2 13 err:check_one
                                                          FUN 1 11 source_sink
                                                            VAR 0
                                                          VAR 1
                                                        FUN 2 1 .
                                                          FUN 1 6 nonvar
                                                            VAR 0
                                                          FUN 2 1 .
                                                            FUN 1 2 \+
                                                              FUN 1 4 atom
                                                                VAR 0
                                                            FUN 2 1 .
                                                              FUN 2 14 err:throwerror
                                                                FUN 2 12 domain_error
                                                                  FUN 0 11 source_sink
                                                                  VAR 0
                                                                VAR 1
                                                              FUN 0 2 []
                                                      FUN 2 1 .
                                                        FUN 3 6 clause
                                                          INT 2
                                                          FUN 2 13 err:check_one
                                                            FUN 1 6 stream
                                                              VAR 0
                                                            VAR 1
                                                          FUN 2 1 .
                                                            FUN 1 2 \+
                                                              FUN 1 15 io:check_stream
                                                                VAR 0
                                                            FUN 2 1 .
                                                              FUN 2 14 err:throwerror
                                                                FUN 2 12 domain_error
                                                                  FUN 0 6 stream
                                                                  VAR 0
                                                                VAR 1
                                                              FUN 0 2 []
                                                        FUN 2 1 .
                                                          FUN 3 6 clause
                                                            INT 3
                                                            FUN 2 13 err:check_one
                                                              FUN 1 6 s_or_a
                                                                VAR 0
                                                              VAR 1
                                                            FUN 2 1 .
                                                              FUN 1 2 \+
                                                                FUN 2 16 io:stream_to_num
                                                                  VAR 0
                                                                  VAR 2
                                                              FUN 2 1 .
                                                                FUN 2 14 err:throwerror
                                                                  FUN 2 12 domain_error
                                                                    FUN 0 15 stream_or_alias
                                                                    VAR 0
                                                                  VAR 1
                                                                FUN 0 2 []
                                                          FUN 2 1 .
                                                            FUN 3 6 clause
                                                              INT 5
                                                              FUN 2 13 err:check_one
                                                                FUN 1 7 s_exist
                                                                  VAR 0
                                                                VAR 1
                                                              FUN 2 1 .
                                                                FUN 2 16 io:stream_to_num
                                                                  VAR 0
                                                                  VAR 2
                                                                FUN 2 1 .
                                                                  FUN 1 2 \+
                                                                    FUN 3 14 io:stream_type
                                                                      VAR 2
                                                                      VAR 3
                                                                      VAR 4
                                                                  FUN 2 1 .
                                                                    FUN 2 14 err:throwerror
                                                                      FUN 2 15 existence_error
                                                                        FUN 0 6 stream
                                                                        VAR 0
                                                                      VAR 1
                                                                    FUN 0 2 []
                                                            FUN 2 1 .
                                                              FUN 3 6 clause
                                                                INT 6
                                                                FUN 2 13 err:check_one
                                                                  FUN 2 8 s_in_out
                                                                    VAR 0
                                                                    VAR 1
                                                                  VAR 2
                                                                FUN 2 1 .
                                                                  FUN 2 16 io:stream_to_num
                                                                    VAR 1
                                                                    VAR 3
                                                                  FUN 2 1 .
                                                                    FUN 3 14 io:stream_type
                                                                      VAR 3
                                                                      VAR 4
                                                                      VAR 5
                                                                    FUN 2 1 .
                                                                      FUN 2 2 \=
                                                                        VAR 4
                                                                        VAR 0
                                                                      FUN 2 1 .
                                                                        FUN 2 14 err:throwerror
                                                                          FUN 3 16 permission_error
                                                                            VAR 0
                                                                            FUN 0 6 stream
                                                                            VAR 1
                                                                          VAR 2
                                                                        FUN 0 2 []
                                                              FUN 2 1 .
                                                                FUN 3 6 clause
                                                                  INT 8
                                                                  FUN 2 13 err:check_one
                                                                    FUN 3 13 s_text_binary
                                                                      VAR 0
                                                                      VAR 1
                                                                      VAR 2
                                                                    VAR 3
                                                                  FUN 2 1 .
                                                                    FUN 2 16 io:stream_to_num
                                                                      VAR 2
                                                                      VAR 4
                                                                    FUN 2 1 .
                                                                      FUN 3 14 io:stream_type
                                                                        VAR 4
                                                                        VAR 5
                                                                        VAR 6
                                                                      FUN 2 1 .
                                                                        FUN 2 2 \=
                                                                          VAR 1
                                                                          VAR 6
                                                                        FUN 2 1 .
                                                                          FUN 2 20 err:check_one$2_30_1
                                                                            VAR 7
                                                                            VAR 1
                                                                          FUN 2 1 .
                                                                            FUN 2 14 err:throwerror
                                                                              FUN 3 16 permission_error
                                                                                VAR 0
                                                                                VAR 7
                                                                                VAR 2
                                                                              VAR 3
                                                                            FUN 0 2 []
                                                                FUN 2 1 .
                                                                  FUN 3 6 clause
                                                                    INT 2
                                                                    FUN 2 13 err:check_one
                                                                      FUN 1 15 stream_property
                                                                        VAR 0
                                                                      VAR 1
                                                                    FUN 2 1 .
                                                                      FUN 1 2 \+
                                                                        FUN 1 17 io:check_property
                                                                          VAR 0
                                                                      FUN 2 1 .
                                                                        FUN 2 14 err:throwerror
                                                                          FUN 2 12 domain_error
                                                                            FUN 0 15 stream_property
                                                                            VAR 0
                                                                          VAR 1
                                                                        FUN 0 2 []
                                                                  FUN 2 1 .
                                                                    FUN 3 6 clause
                                                                      INT 2
                                                                      FUN 2 13 err:check_one
                                                                        FUN 1 12 s_reposition
                                                                          VAR 0
                                                                        VAR 1
                                                                      FUN 2 1 .
                                                                        FUN 1 2 \+
                                                                          FUN 2 15 stream_property
                                                                            VAR 0
                                                                            FUN 1 10 reposition
                                                                              FUN 0 4 true
                                                                        FUN 2 1 .
                                                                          FUN 2 14 err:throwerror
                                                                            FUN 3 16 permission_error
                                                                              FUN 0 10 reposition
                                                                              FUN 0 6 stream
                                                                              VAR 0
                                                                            VAR 1
                                                                          FUN 0 2 []
                                                                    FUN 2 1 .
                                                                      FUN 3 6 clause
                                                                        INT 3
                                                                        FUN 2 13 err:check_one
                                                                          FUN 1 8 position
                                                                            VAR 0
                                                                          VAR 1
                                                                        FUN 2 1 .
                                                                          FUN 1 2 \+
                                                                            FUN 2 24 io:check_stream_position
                                                                              VAR 0
                                                                              VAR 2
                                                                          FUN 2 1 .
                                                                            FUN 2 14 err:throwerror
                                                                              FUN 2 12 domain_error
                                                                                FUN 0 15 stream_position
                                                                                VAR 0
                                                                              VAR 1
                                                                            FUN 0 2 []
                                                                      FUN 2 1 .
                                                                        FUN 3 6 clause
                                                                          INT 2
                                                                          FUN 2 13 err:check_one
                                                                            FUN 1 12 in_char_code
                                                                              VAR 0
                                                                            VAR 1
                                                                          FUN 2 1 .
                                                                            FUN 1 2 \+
                                                                              FUN 1 21 char_io:check_in_code
                                                                                VAR 0
                                                                            FUN 2 1 .
                                                                              FUN 2 14 err:throwerror
                                                                                FUN 1 20 representation_error
                                                                                  FUN 0 17 in_character_code
                                                                                VAR 1
                                                                              FUN 0 2 []
                                                                        FUN 2 1 .
                                                                          FUN 3 6 clause
                                                                            INT 2
                                                                            FUN 2 13 err:check_one
                                                                              FUN 1 9 char_code
                                                                                VAR 0
                                                                              VAR 1
                                                                            FUN 2 1 .
                                                                              FUN 1 2 \+
                                                                                FUN 1 16 err:is_char_code
                                                                                  VAR 0
                                                                              FUN 2 1 .
                                                                                FUN 2 14 err:throwerror
                                                                                  FUN 1 20 representation_error
                                                                                    FUN 0 14 character_code
                                                                                  VAR 1
                                                                                FUN 0 2 []
                                                                          FUN 2 1 .
                                                                            FUN 3 6 clause
                                                                              INT 2
                                                                              FUN 2 13 err:check_one
                                                                                FUN 1 9 char_list
                                                                                  VAR 0
                                                                                VAR 1
                                                                              FUN 2 1 .
                                                                                FUN 1 6 nonvar
                                                                                  VAR 0
                                                                                FUN 2 1 .
                                                                                  FUN 1 2 \+
                                                                                    FUN 2 19 err:check_char_list
                                                                                      VAR 0
                                                                                      VAR 1
                                                                                  FUN 0 2 []
                                                                            FUN 2 1 .
                                                                              FUN 3 6 clause
                                                                                INT 2
                                                                                FUN 2 13 err:check_one
                                                                                  FUN 1 14 char_code_list
                                                                                    VAR 0
                                                                                  VAR 1
                                                                                FUN 2 1 .
                                                                                  FUN 1 6 nonvar
                                                                                    VAR 0
                                                                                  FUN 2 1 .
                                                                                    FUN 1 2 \+
                                                                                      FUN 1 21 err:is_char_code_list
                                                                                        VAR 0
                                                                                    FUN 2 1 .
                                                                                      FUN 2 14 err:throwerror
                                                                                        FUN 1 20 representation_error
                                                                                          FUN 0 14 character_code
                                                                                        VAR 1
                                                                                      FUN 0 2 []
                                                                              FUN 2 1 .
                                                                                FUN 3 6 clause
                                                                                  INT 2
                                                                                  FUN 2 13 err:check_one
                                                                                    FUN 1 7 in_char
                                                                                      VAR 0
                                                                                    VAR 1
                                                                                  FUN 2 1 .
                                                                                    FUN 1 2 \+
                                                                                      FUN 1 21 char_io:check_in_char
                                                                                        VAR 0
                                                                                    FUN 2 1 .
                                                                                      FUN 2 14 err:throwerror
                                                                                        FUN 2 10 type_error
                                                                                          FUN 0 12 in_character
                                                                                          VAR 0
                                                                                        VAR 1
                                                                                      FUN 0 2 []
                                                                                FUN 2 1 .
                                                                                  FUN 3 6 clause
                                                                                    INT 2
                                                                                    FUN 2 13 err:check_one
                                                                                      FUN 1 7 in_byte
                                                                                        VAR 0
                                                                                      VAR 1
                                                                                    FUN 2 1 .
                                                                                      FUN 1 2 \+
                                                                                        FUN 1 21 char_io:check_in_byte
                                                                                          VAR 0
                                                                                      FUN 2 1 .
                                                                                        FUN 2 14 err:throwerror
                                                                                          FUN 2 10 type_error
                                                                                            FUN 0 7 in_byte
                                                                                            VAR 0
                                                                                          VAR 1
                                                                                        FUN 0 2 []
                                                                                  FUN 2 1 .
                                                                                    FUN 3 6 clause
                                                                                      INT 1
                                                                                      FUN 2 13 err:check_one
                                                                                        FUN 0 15 input_character
                                                                                        VAR 0
                                                                                      FUN 2 1 .
                                                                                        FUN 2 14 err:throwerror
                                                                                          FUN 1 20 representation_error
                                                                                            FUN 0 9 character
                                                                                          VAR 0
                                                                                        FUN 0 2 []
                                                                                    FUN 2 1 .
                                                                                      FUN 3 6 clause
                                                                                        INT 5
                                                                                        FUN 2 13 err:check_one
                                                                                          FUN 1 17 private_procedure
                                                                                            VAR 0
                                                                                          VAR 1
                                                                                        FUN 2 1 .
                                                                                          FUN 1 2 \+
                                                                                            FUN 3 10 $pred_info
                                                                                              VAR 0
                                                                                              FUN 0 12 pred_dynamic
                                                                                              VAR 2
                                                                                          FUN 2 1 .
                                                                                            FUN 3 7 functor
                                                                                              VAR 0
                                                                                              VAR 3
                                                                                              VAR 4
                                                                                            FUN 2 1 .
                                                                                              FUN 2 14 err:throwerror
                                                                                                FUN 3 16 permission_error
                                                                                                  FUN 0 6 access
                                                                                                  FUN 0 17 private_procedure
                                                                                                  FUN 2 1 /
                                                                                                    VAR 3
                                                                                                    VAR 4
                                                                                                VAR 1
                                                                                              FUN 0 2 []
                                                                                      FUN 2 1 .
                                                                                        FUN 3 6 clause
                                                                                          INT 5
                                                                                          FUN 2 13 err:check_one
                                                                                            FUN 1 16 static_procedure
                                                                                              VAR 0
                                                                                            VAR 1
                                                                                          FUN 2 1 .
                                                                                            FUN 1 2 \+
                                                                                              FUN 3 10 $pred_info
                                                                                                VAR 0
                                                                                                FUN 0 12 pred_dynamic
                                                                                                VAR 2
                                                                                            FUN 2 1 .
                                                                                              FUN 3 7 functor
                                                                                                VAR 0
                                                                                                VAR 3
                                                                                                VAR 4
                                                                                              FUN 2 1 .
                                                                                                FUN 2 14 err:throwerror
                                                                                                  FUN 3 16 permission_error
                                                                                                    FUN 0 6 modify
                                                                                                    FUN 0 16 static_procedure
                                                                                                    FUN 2 1 /
                                                                                                      VAR 3
                                                                                                      VAR 4
                                                                                                  VAR 1
                                                                                                FUN 0 2 []
                                                                                        FUN 2 1 .
                                                                                          FUN 3 6 clause
                                                                                            INT 2
                                                                                            FUN 2 13 err:check_one
                                                                                              FUN 1 11 prolog_flag
                                                                                                VAR 0
                                                                                              VAR 1
                                                                                            FUN 2 1 .
                                                                                              FUN 1 2 \+
                                                                                                FUN 1 17 execution:flag_ok
                                                                                                  VAR 0
                                                                                              FUN 2 1 .
                                                                                                FUN 2 14 err:throwerror
                                                                                                  FUN 2 12 domain_error
                                                                                                    FUN 0 11 prolog_flag
                                                                                                    VAR 0
                                                                                                  VAR 1
                                                                                                FUN 0 2 []
                                                                                          FUN 2 1 .
                                                                                            FUN 3 6 clause
                                                                                              INT 2
                                                                                              FUN 2 13 err:check_one
                                                                                                FUN 1 18 prolog_flag_modify
                                                                                                  VAR 0
                                                                                                VAR 1
                                                                                              FUN 2 1 .
                                                                                                FUN 1 2 \+
                                                                                                  FUN 1 25 execution:flag_modifiable
                                                                                                    VAR 0
                                                                                                FUN 2 1 .
                                                                                                  FUN 2 14 err:throwerror
                                                                                                    FUN 3 16 permission_error
                                                                                                      FUN 0 6 modify
                                                                                                      FUN 0 4 flag
                                                                                                      VAR 0
                                                                                                    VAR 1
                                                                                                  FUN 0 2 []
                                                                                            FUN 2 1 .
                                                                                              FUN 3 6 clause
                                                                                                INT 3
                                                                                                FUN 2 13 err:check_one
                                                                                                  FUN 2 17 prolog_flag_value
                                                                                                    VAR 0
                                                                                                    VAR 1
                                                                                                  VAR 2
                                                                                                FUN 2 1 .
                                                                                                  FUN 1 2 \+
                                                                                                    FUN 2 23 execution:flag_value_ok
                                                                                                      VAR 0
                                                                                                      VAR 1
                                                                                                  FUN 2 1 .
                                                                                                    FUN 2 14 err:throwerror
                                                                                                      FUN 2 12 domain_error
                                                                                                        FUN 0 10 flag_value
                                                                                                        FUN 2 1 +
                                                                                                          VAR 0
                                                                                                          VAR 1
                                                                                                      VAR 2
                                                                                                    FUN 0 2 []
                                                                                              FUN 2 1 .
                                                                                                FUN 3 6 clause
                                                                                                  INT 2
                                                                                                  FUN 2 13 err:check_one
                                                                                                    VAR 0
                                                                                                    VAR 1
                                                                                                  FUN 0 2 []
                                                                                                FUN 0 2 []
CLS 2 14 err:throwerror
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 14 err:throwerror
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 5 throw
          FUN 2 5 error
            VAR 0
            VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 1 11 err:is_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 11 err:is_list
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 11 err:is_list
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 1
          FUN 2 1 .
            FUN 1 11 err:is_list
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 1 22 err:inst_list_elements
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 22 err:inst_list_elements
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 22 err:inst_list_elements
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 0
          FUN 2 1 .
            FUN 1 22 err:inst_list_elements
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 1 16 err:is_char_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 16 err:is_char_code
        VAR 0
      FUN 2 1 .
        FUN 1 7 integer
          VAR 0
        FUN 2 1 .
          FUN 2 1 >
            VAR 0
            INT 0
          FUN 2 1 .
            FUN 2 1 <
              VAR 0
              INT 256
            FUN 0 2 []
    FUN 0 2 []
CLS 1 11 err:is_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 11 err:is_char
        VAR 0
      FUN 2 1 .
        FUN 1 4 atom
          VAR 0
        FUN 2 1 .
          FUN 2 10 atom_codes
            VAR 0
            FUN 2 1 .
              VAR 1
              FUN 0 2 []
          FUN 0 2 []
    FUN 0 2 []
CLS 1 21 err:is_char_code_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 21 err:is_char_code_list
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 21 err:is_char_code_list
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 1
          FUN 2 1 .
            FUN 1 16 err:is_char_code
              VAR 0
            FUN 2 1 .
              FUN 1 21 err:is_char_code_list
                VAR 1
              FUN 0 2 []
      FUN 0 2 []
CLS 2 19 err:check_char_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 19 err:check_char_list
        FUN 0 2 []
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 19 err:check_char_list
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 1
          FUN 2 1 .
            FUN 2 14 err:check_char
              VAR 0
              VAR 2
            FUN 2 1 .
              FUN 2 19 err:check_char_list
                VAR 1
                VAR 2
              FUN 0 2 []
      FUN 0 2 []
CLS 2 14 err:check_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 14 err:check_char
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 11 err:is_char
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 14 err:check_char
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 14 err:throwerror
            FUN 2 10 type_error
              FUN 0 9 character
              VAR 0
            VAR 1
          FUN 0 2 []
      FUN 0 2 []
CLS 0 6 repeat
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 6 repeat
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 0 6 repeat
        FUN 2 1 .
          FUN 0 6 repeat
          FUN 0 2 []
      FUN 0 2 []
CLS 0 4 halt
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 4 halt
      FUN 2 1 .
        FUN 1 5 $halt
          INT 0
        FUN 0 2 []
    FUN 0 2 []
CLS 1 4 halt
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 4 halt
        VAR 0
      FUN 2 1 .
        FUN 1 7 integer
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 5 $halt
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 4 halt
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 4 halt
              VAR 0
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 7 integer
                  VAR 0
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 3 5 catch
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 5 catch
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 4 20 execution:catch_some
          VAR 0
          VAR 1
          VAR 2
          FUN 2 1 .
            FUN 0 6 $abort
            FUN 2 1 .
              FUN 0 6 $retry
              FUN 0 2 []
        FUN 0 2 []
    FUN 0 2 []
CLS 4 20 execution:catch_some
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 20 execution:catch_some
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 19 execution:catch_try
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 4 20 execution:catch_some
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 1 7 retract
            FUN 1 15 $exception_term
              VAR 4
          FUN 2 1 .
            FUN 4 22 execution:do_exception
              VAR 1
              VAR 4
              VAR 2
              VAR 3
            FUN 0 2 []
      FUN 0 2 []
CLS 1 19 execution:catch_try
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 19 execution:catch_try
        VAR 0
      FUN 2 1 .
        FUN 1 7 $choice
          VAR 1
        FUN 2 1 .
          FUN 2 11 $get_global
            FUN 0 17 $exception_choice
            VAR 2
          FUN 2 1 .
            FUN 2 11 $set_global
              FUN 0 17 $exception_choice
              VAR 1
            FUN 2 1 .
              FUN 2 19 execution:interpret
                VAR 0
                VAR 1
              FUN 2 1 .
                FUN 2 11 $set_global
                  FUN 0 17 $exception_choice
                  VAR 2
                FUN 0 2 []
    FUN 0 2 []
CLS 4 22 execution:do_exception
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 22 execution:do_exception
        VAR 0
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 2 \+
          FUN 2 12 lists:member
            VAR 0
            VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 4 call
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 22 execution:do_exception
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 1 5 throw
            VAR 1
          FUN 0 2 []
      FUN 0 2 []
CLS 1 5 throw
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 5 throw
        VAR 0
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 7 asserta
              FUN 1 15 $exception_term
                VAR 0
            FUN 2 1 .
              FUN 2 11 $get_global
                FUN 0 17 $exception_choice
                VAR 1
              FUN 2 1 .
                FUN 1 4 $cut
                  VAR 1
                FUN 2 1 .
                  FUN 0 4 fail
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 5 throw
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 5 throw
              VAR 0
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 4 20 execution:ifthenelse
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 20 execution:ifthenelse
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 16 execution:call_i
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 19 execution:interpret
              VAR 1
              VAR 3
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 20 execution:ifthenelse
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 19 execution:interpret
            VAR 2
            VAR 3
          FUN 0 2 []
      FUN 0 2 []
CLS 2 19 execution:interpret
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 19 execution:interpret
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 10 $var_index
          VAR 0
          VAR 2
        FUN 2 1 .
          VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 19 execution:interpret
          FUN 2 2 ->
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 20 execution:ifthenelse
              VAR 0
              VAR 1
              FUN 0 4 fail
              VAR 2
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 2 19 execution:interpret
            FUN 2 1 ;
              FUN 2 2 ->
                VAR 0
                VAR 1
              VAR 2
            VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 4 20 execution:ifthenelse
                VAR 0
                VAR 1
                VAR 2
                VAR 3
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 3
            FUN 2 19 execution:interpret
              FUN 2 1 ;
                VAR 0
                VAR 1
              VAR 2
            FUN 2 1 .
              FUN 2 19 execution:interpret
                VAR 0
                VAR 2
              FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 3
              FUN 2 19 execution:interpret
                FUN 2 1 ;
                  VAR 0
                  VAR 1
                VAR 2
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 19 execution:interpret
                    VAR 1
                    VAR 2
                  FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 3
                FUN 2 19 execution:interpret
                  FUN 2 1 ,
                    VAR 0
                    VAR 1
                  VAR 2
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 2 19 execution:interpret
                      VAR 0
                      VAR 2
                    FUN 2 1 .
                      FUN 2 19 execution:interpret
                        VAR 1
                        VAR 2
                      FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 1
                  FUN 2 19 execution:interpret
                    FUN 0 1 !
                    VAR 0
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 1 4 $cut
                        VAR 0
                      FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 2
                    FUN 2 19 execution:interpret
                      VAR 0
                      VAR 1
                    FUN 2 1 .
                      VAR 0
                      FUN 0 2 []
                  FUN 0 2 []
CLS 1 16 execution:call_i
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 16 execution:call_i
        VAR 0
      FUN 2 1 .
        FUN 1 7 $choice
          VAR 1
        FUN 2 1 .
          FUN 2 19 execution:interpret
            VAR 0
            VAR 1
          FUN 0 2 []
    FUN 0 2 []
CLS 1 23 execution:toplevel_call
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 23 execution:toplevel_call
        VAR 0
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 2 25 execution:convert_to_goal
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 0 11 debug:reset
              FUN 2 1 .
                FUN 2 11 $set_global
                  FUN 0 10 $can_debug
                  FUN 0 4 true
                FUN 2 1 .
                  FUN 1 7 $choice
                    VAR 2
                  FUN 2 1 .
                    FUN 2 19 execution:interpret
                      VAR 1
                      VAR 2
                    FUN 2 1 .
                      FUN 2 11 $set_global
                        FUN 0 10 $can_debug
                        FUN 0 5 false
                      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 23 execution:toplevel_call
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 4 call
              VAR 0
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 12 conv_to_goal
                  VAR 0
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 4 call
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 4 call
        VAR 0
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 2 25 execution:convert_to_goal
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 7 $choice
                VAR 2
              FUN 2 1 .
                FUN 2 19 execution:interpret
                  VAR 1
                  VAR 2
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 4 call
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 4 call
              VAR 0
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 12 conv_to_goal
                  VAR 0
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 25 execution:convert_to_goal
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 25 execution:convert_to_goal
        VAR 0
        FUN 1 4 call
          VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 2 25 execution:convert_to_goal
          FUN 2 1 ,
            VAR 0
            VAR 1
          FUN 2 1 ,
            VAR 2
            VAR 3
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 25 execution:convert_to_goal
              VAR 0
              VAR 2
            FUN 2 1 .
              FUN 2 25 execution:convert_to_goal
                VAR 1
                VAR 3
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 2 25 execution:convert_to_goal
            FUN 2 1 ;
              VAR 0
              VAR 1
            FUN 2 1 ;
              VAR 2
              VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 25 execution:convert_to_goal
                VAR 0
                VAR 2
              FUN 2 1 .
                FUN 2 25 execution:convert_to_goal
                  VAR 1
                  VAR 3
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 4
            FUN 2 25 execution:convert_to_goal
              FUN 2 2 ->
                VAR 0
                VAR 1
              FUN 2 2 ->
                VAR 2
                VAR 3
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 25 execution:convert_to_goal
                  VAR 0
                  VAR 2
                FUN 2 1 .
                  FUN 2 25 execution:convert_to_goal
                    VAR 1
                    VAR 3
                  FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 1
              FUN 2 25 execution:convert_to_goal
                VAR 0
                VAR 0
              FUN 2 1 .
                FUN 1 8 callable
                  VAR 0
                FUN 0 2 []
            FUN 0 2 []
CLS 1 4 once
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 4 once
        VAR 0
      FUN 2 1 .
        FUN 1 4 call
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 0 2 []
CLS 1 2 \+
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 2 \+
        VAR 0
      FUN 2 1 .
        FUN 1 4 call
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 0 4 fail
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 2 \+
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 1 13 execution:not
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 13 execution:not
        VAR 0
      FUN 2 1 .
        VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 0 4 fail
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 13 execution:not
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 2 26 execution:call_cleanup_det
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 26 execution:call_cleanup_det
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 4 20 execution:catch_some
          VAR 0
          VAR 2
          FUN 2 1 ,
            FUN 1 17 execution:cleanup
              VAR 1
            FUN 1 5 throw
              VAR 2
          FUN 2 1 .
            FUN 0 6 $retry
            FUN 0 2 []
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 17 execution:cleanup
              VAR 1
            FUN 2 1 .
              FUN 0 1 !
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 26 execution:call_cleanup_det
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 17 execution:cleanup
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 0 4 fail
              FUN 0 2 []
      FUN 0 2 []
CLS 1 17 execution:cleanup
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 17 execution:cleanup
        VAR 0
      FUN 2 1 .
        FUN 1 4 call
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 17 execution:cleanup
          VAR 0
        FUN 2 1 .
          FUN 1 5 throw
            FUN 2 5 error
              FUN 1 14 internal_error
                VAR 0
              FUN 2 1 /
                FUN 0 16 call_cleanup_det
                INT 2
          FUN 0 2 []
      FUN 0 2 []
CLS 1 22 execution:call_dynamic
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 22 execution:call_dynamic
        VAR 0
      FUN 2 1 .
        FUN 1 17 execution:nodebug
          FUN 2 6 clause
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 7 $choice
            VAR 2
          FUN 2 1 .
            FUN 2 19 execution:interpret
              VAR 1
              VAR 2
            FUN 0 2 []
    FUN 0 2 []
CLS 1 20 execution:call_error
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 20 execution:call_error
        VAR 0
      FUN 2 1 .
        FUN 1 17 execution:nodebug
          FUN 1 23 execution:call_error_do
            VAR 0
        FUN 0 2 []
    FUN 0 2 []
CLS 1 23 execution:call_error_do
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 23 execution:call_error_do
        VAR 0
      FUN 2 1 .
        FUN 1 8 callable
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 19 current_prolog_flag
              FUN 0 7 unknown
              VAR 1
            FUN 2 1 .
              FUN 2 25 execution:call_error_type
                VAR 1
                VAR 0
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 23 execution:call_error_do
          VAR 0
        FUN 2 1 .
          FUN 1 5 throw
            FUN 2 5 error
              FUN 2 14 internal_error
                FUN 0 8 callable
                VAR 0
              FUN 0 9 execution
          FUN 0 2 []
      FUN 0 2 []
CLS 2 25 execution:call_error_type
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 25 execution:call_error_type
        FUN 0 4 fail
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 0 4 fail
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 25 execution:call_error_type
          FUN 0 7 warning
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 7 functor
              VAR 0
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 3 11 misc:format
                FUN 0 11 user_output
                FUN 0 41 
{Warning: the procedure ~q is undefined}
                FUN 2 1 .
                  FUN 2 1 /
                    VAR 1
                    VAR 2
                  FUN 0 2 []
              FUN 2 1 .
                FUN 0 4 fail
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 2 25 execution:call_error_type
            FUN 0 5 error
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 7 functor
                VAR 0
                VAR 1
                VAR 2
              FUN 2 1 .
                FUN 1 5 throw
                  FUN 2 5 error
                    FUN 2 15 existence_error
                      FUN 0 9 procedure
                      FUN 2 1 /
                        VAR 1
                        VAR 2
                    FUN 0 7 execute
                FUN 0 2 []
        FUN 0 2 []
CLS 1 17 execution:nodebug
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 17 execution:nodebug
        VAR 0
      FUN 2 1 .
        FUN 2 11 $get_global
          FUN 0 10 $can_debug
          FUN 0 5 false
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 17 execution:nodebug
          VAR 0
        FUN 2 1 .
          FUN 2 11 $set_global
            FUN 0 10 $can_debug
            FUN 0 5 false
          FUN 2 1 .
            VAR 0
            FUN 2 1 .
              FUN 2 11 $set_global
                FUN 0 10 $can_debug
                FUN 0 4 true
              FUN 0 2 []
      FUN 0 2 []
CLS 0 15 execution:abort
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 15 execution:abort
      FUN 2 1 .
        FUN 1 5 throw
          FUN 0 6 $abort
        FUN 0 2 []
    FUN 0 2 []
CLS 2 25 execution:write_exception
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 25 execution:write_exception
        VAR 0
        FUN 2 5 error
          VAR 1
          VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 3 11 misc:format
            VAR 0
            FUN 0 17 {ERROR: ~q in ~q}
            FUN 2 1 .
              VAR 1
              FUN 2 1 .
                VAR 2
                FUN 0 2 []
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 25 execution:write_exception
          VAR 0
          FUN 0 6 $abort
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 5 write
              VAR 0
              FUN 0 19 {Execution aborted}
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 25 execution:write_exception
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 3 11 misc:format
              VAR 0
              FUN 0 31 {ERROR: uncaught exception: ~q}
              FUN 2 1 .
                VAR 1
                FUN 0 2 []
            FUN 0 2 []
        FUN 0 2 []
CLS 2 19 current_prolog_flag
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 19 current_prolog_flag
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 6 clause
              FUN 3 12 $prolog_flag
                VAR 0
                VAR 1
                VAR 2
              VAR 3
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 2 19 current_prolog_flag
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 4 atom
            VAR 0
          FUN 2 1 .
            FUN 2 6 clause
              FUN 3 12 $prolog_flag
                VAR 0
                VAR 2
                VAR 3
              VAR 4
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 1 =
                  VAR 1
                  VAR 2
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 19 current_prolog_flag
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 13 err:err_check
              FUN 2 19 current_prolog_flag
                VAR 0
                VAR 1
              FUN 2 1 .
                FUN 1 4 atom
                  VAR 0
                FUN 2 1 .
                  FUN 1 11 prolog_flag
                    VAR 0
                  FUN 0 2 []
            FUN 0 2 []
        FUN 0 2 []
CLS 2 15 set_prolog_flag
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 2 15 set_prolog_flag
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 1
          FUN 2 1 .
            FUN 1 4 atom
              VAR 0
            FUN 2 1 .
              FUN 2 6 clause
                FUN 3 12 $prolog_flag
                  VAR 0
                  VAR 2
                  FUN 0 4 true
                VAR 3
              FUN 2 1 .
                FUN 2 23 execution:flag_value_ok
                  VAR 0
                  VAR 1
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 1 7 retract
                      FUN 3 12 $prolog_flag
                        VAR 0
                        VAR 4
                        VAR 5
                    FUN 2 1 .
                      FUN 1 7 asserta
                        FUN 3 12 $prolog_flag
                          VAR 0
                          VAR 1
                          FUN 0 4 true
                      FUN 2 1 .
                        FUN 2 18 execution:flag_set
                          VAR 0
                          VAR 1
                        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 15 set_prolog_flag
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 15 set_prolog_flag
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 4 inst
                  VAR 1
                FUN 2 1 .
                  FUN 1 4 atom
                    VAR 0
                  FUN 2 1 .
                    FUN 1 11 prolog_flag
                      VAR 0
                    FUN 2 1 .
                      FUN 1 18 prolog_flag_modify
                        VAR 0
                      FUN 2 1 .
                        FUN 2 17 prolog_flag_value
                          VAR 0
                          VAR 1
                        FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 17 execution:flag_ok
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 1 17 execution:flag_ok
        VAR 0
      FUN 2 1 .
        FUN 2 6 clause
          FUN 3 12 $prolog_flag
            VAR 0
            VAR 1
            VAR 2
          VAR 3
        FUN 0 2 []
    FUN 0 2 []
CLS 1 25 execution:flag_modifiable
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 25 execution:flag_modifiable
        VAR 0
      FUN 2 1 .
        FUN 2 6 clause
          FUN 3 12 $prolog_flag
            VAR 0
            VAR 1
            FUN 0 4 true
          VAR 2
        FUN 0 2 []
    FUN 0 2 []
CLS 2 23 execution:flag_value_ok
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 23 execution:flag_value_ok
        FUN 0 15 char_conversion
        VAR 0
      FUN 2 1 .
        FUN 2 15 lists:memberchk
          VAR 0
          FUN 2 1 .
            FUN 0 3 off
            FUN 2 1 .
              FUN 0 2 on
              FUN 0 2 []
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 23 execution:flag_value_ok
          FUN 0 5 debug
          VAR 0
        FUN 2 1 .
          FUN 2 15 lists:memberchk
            VAR 0
            FUN 2 1 .
              FUN 0 3 off
              FUN 2 1 .
                FUN 0 2 on
                FUN 0 2 []
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 2 23 execution:flag_value_ok
            FUN 0 7 unknown
            VAR 0
          FUN 2 1 .
            FUN 2 15 lists:memberchk
              VAR 0
              FUN 2 1 .
                FUN 0 5 error
                FUN 2 1 .
                  FUN 0 4 fail
                  FUN 2 1 .
                    FUN 0 7 warning
                    FUN 0 2 []
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 2 23 execution:flag_value_ok
              FUN 0 13 double_quotes
              VAR 0
            FUN 2 1 .
              FUN 2 15 lists:memberchk
                VAR 0
                FUN 2 1 .
                  FUN 0 5 codes
                  FUN 2 1 .
                    FUN 0 5 chars
                    FUN 2 1 .
                      FUN 0 4 atom
                      FUN 0 2 []
              FUN 0 2 []
          FUN 0 2 []
CLS 2 18 execution:flag_set
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 18 execution:flag_set
        FUN 0 15 char_conversion
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 0 29 term_io:check_char_conv_state
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 18 execution:flag_set
          VAR 0
          VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 0 33 execution:init_prolog_flags$0_0_4
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 0 33 execution:init_prolog_flags$0_0_4
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 8 $version
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 7 assertz
              FUN 3 12 $prolog_flag
                FUN 0 7 version
                VAR 0
                FUN 0 5 false
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 0 33 execution:init_prolog_flags$0_0_4
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 0 27 execution:init_prolog_flags
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 0 27 execution:init_prolog_flags
      FUN 2 1 .
        FUN 1 7 assertz
          FUN 3 12 $prolog_flag
            FUN 0 7 bounded
            FUN 0 4 true
            FUN 0 5 false
        FUN 2 1 .
          FUN 2 2 is
            VAR 0
            FUN 2 2 <<
              INT 1
              INT 30
          FUN 2 1 .
            FUN 2 2 is
              VAR 1
              FUN 1 1 \
                VAR 0
            FUN 2 1 .
              FUN 1 7 assertz
                FUN 3 12 $prolog_flag
                  FUN 0 11 max_integer
                  VAR 1
                  FUN 0 5 false
              FUN 2 1 .
                FUN 1 7 assertz
                  FUN 3 12 $prolog_flag
                    FUN 0 11 min_integer
                    VAR 0
                    FUN 0 5 false
                FUN 2 1 .
                  FUN 1 7 assertz
                    FUN 3 12 $prolog_flag
                      FUN 0 25 integer_rounding_function
                      FUN 0 11 toward_zero
                      FUN 0 5 false
                  FUN 2 1 .
                    FUN 1 7 assertz
                      FUN 3 12 $prolog_flag
                        FUN 0 9 max_arity
                        INT 255
                        FUN 0 5 false
                    FUN 2 1 .
                      FUN 0 33 execution:init_prolog_flags$0_0_4
                      FUN 2 1 .
                        FUN 1 7 assertz
                          FUN 3 12 $prolog_flag
                            FUN 0 15 char_conversion
                            FUN 0 2 on
                            FUN 0 4 true
                        FUN 2 1 .
                          FUN 1 7 assertz
                            FUN 3 12 $prolog_flag
                              FUN 0 5 debug
                              FUN 0 3 off
                              FUN 0 4 true
                          FUN 2 1 .
                            FUN 1 7 assertz
                              FUN 3 12 $prolog_flag
                                FUN 0 7 unknown
                                FUN 0 5 error
                                FUN 0 4 true
                            FUN 2 1 .
                              FUN 1 7 assertz
                                FUN 3 12 $prolog_flag
                                  FUN 0 13 double_quotes
                                  FUN 0 5 codes
                                  FUN 0 4 true
                              FUN 0 2 []
    FUN 0 2 []
CLS 0 23 execution:init_builtins
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 23 execution:init_builtins
      FUN 2 1 .
        FUN 2 11 $set_global
          FUN 0 17 $exception_choice
          INT 0
        FUN 2 1 .
          FUN 2 11 $set_global
            FUN 0 10 $can_debug
            FUN 0 5 false
          FUN 2 1 .
            FUN 0 27 execution:init_prolog_flags
            FUN 2 1 .
              FUN 0 16 term_io:init_ops
              FUN 2 1 .
                FUN 0 21 term_io:init_charconv
                FUN 2 1 .
                  FUN 0 15 io:init_streams
                  FUN 0 2 []
    FUN 0 2 []
CLS 2 16 io:stream_to_num
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 16 io:stream_to_num
        FUN 1 7 $stream
          VAR 0
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 16 io:stream_to_num
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 6 clause
            FUN 2 13 $stream_alias
              VAR 0
              VAR 1
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
      FUN 0 2 []
CLS 3 14 io:stream_type
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 3 14 io:stream_type
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 6 clause
          FUN 3 18 $stream_properties
            VAR 0
            VAR 3
            FUN 2 1 .
              VAR 1
              FUN 2 1 .
                VAR 2
                VAR 4
          VAR 5
        FUN 0 2 []
    FUN 0 2 []
CLS 1 13 current_input
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 13 current_input
        FUN 1 7 $stream
          VAR 0
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 14 $current_input
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 1 13 current_input
          FUN 1 7 $stream
            VAR 0
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 0
          FUN 2 1 .
            FUN 2 6 clause
              FUN 3 18 $stream_properties
                VAR 0
                VAR 1
                VAR 2
              VAR 3
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 0 4 fail
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 1 13 current_input
            VAR 0
          FUN 2 1 .
            FUN 2 13 err:err_check
              FUN 1 13 current_input
                VAR 0
              FUN 2 1 .
                FUN 1 6 stream
                  VAR 0
                FUN 0 2 []
            FUN 0 2 []
        FUN 0 2 []
CLS 1 20 io:current_input_num
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 20 io:current_input_num
        VAR 0
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 14 $current_input
          VAR 0
        FUN 0 2 []
    FUN 0 2 []
CLS 1 14 current_output
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 14 current_output
        FUN 1 7 $stream
          VAR 0
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 15 $current_output
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 1 14 current_output
          FUN 1 7 $stream
            VAR 0
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 0
          FUN 2 1 .
            FUN 2 6 clause
              FUN 3 18 $stream_properties
                VAR 0
                VAR 1
                VAR 2
              VAR 3
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 0 4 fail
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 1 14 current_output
            VAR 0
          FUN 2 1 .
            FUN 2 13 err:err_check
              FUN 1 14 current_output
                VAR 0
              FUN 2 1 .
                FUN 1 6 stream
                  VAR 0
                FUN 0 2 []
            FUN 0 2 []
        FUN 0 2 []
CLS 1 21 io:current_output_num
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 21 io:current_output_num
        VAR 0
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 15 $current_output
          VAR 0
        FUN 0 2 []
    FUN 0 2 []
CLS 0 19 io:check_input_text
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 19 io:check_input_text
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 19 $current_input_type
          FUN 0 4 text
        FUN 0 2 []
    FUN 0 2 []
CLS 0 21 io:check_input_binary
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 21 io:check_input_binary
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 19 $current_input_type
          FUN 0 6 binary
        FUN 0 2 []
    FUN 0 2 []
CLS 0 20 io:check_output_text
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 20 io:check_output_text
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 20 $current_output_type
          FUN 0 4 text
        FUN 0 2 []
    FUN 0 2 []
CLS 0 22 io:check_output_binary
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 22 io:check_output_binary
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 20 $current_output_type
          FUN 0 6 binary
        FUN 0 2 []
    FUN 0 2 []
CLS 0 19 io:check_error_text
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 19 io:check_error_text
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 19 $current_error_type
          FUN 0 4 text
        FUN 0 2 []
    FUN 0 2 []
CLS 0 21 io:check_error_binary
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 21 io:check_error_binary
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 19 $current_error_type
          FUN 0 6 binary
        FUN 0 2 []
    FUN 0 2 []
CLS 1 9 set_input
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 9 set_input
        VAR 0
      FUN 2 1 .
        FUN 3 17 io:set_input_type
          VAR 0
          VAR 1
          FUN 1 9 set_input
            VAR 0
        FUN 2 1 .
          FUN 2 7 $set_bb
            FUN 0 19 $current_input_type
            VAR 1
          FUN 0 2 []
    FUN 0 2 []
CLS 3 17 io:set_input_type
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 3 17 io:set_input_type
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 2 16 io:stream_to_num
            VAR 0
            VAR 3
          FUN 2 1 .
            FUN 3 14 io:stream_type
              VAR 3
              FUN 0 5 input
              VAR 1
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 7 $set_bb
                  FUN 0 14 $current_input
                  VAR 3
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 17 io:set_input_type
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 13 err:err_check
            VAR 2
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 6 s_or_a
                  VAR 0
                FUN 2 1 .
                  FUN 1 7 s_exist
                    VAR 0
                  FUN 2 1 .
                    FUN 2 8 s_in_out
                      FUN 0 5 input
                      VAR 0
                    FUN 2 1 .
                      FUN 3 13 s_text_binary
                        FUN 0 5 input
                        VAR 1
                        VAR 0
                      FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 10 set_output
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 10 set_output
        VAR 0
      FUN 2 1 .
        FUN 4 18 io:set_output_type
          FUN 0 15 $current_output
          VAR 0
          VAR 1
          FUN 1 10 set_output
            VAR 0
        FUN 2 1 .
          FUN 2 7 $set_bb
            FUN 0 20 $current_output_type
            VAR 1
          FUN 0 2 []
    FUN 0 2 []
CLS 4 18 io:set_output_type
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 18 io:set_output_type
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 1
        FUN 2 1 .
          FUN 2 16 io:stream_to_num
            VAR 1
            VAR 4
          FUN 2 1 .
            FUN 3 14 io:stream_type
              VAR 4
              FUN 0 6 output
              VAR 2
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 7 $set_bb
                  VAR 0
                  VAR 4
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 18 io:set_output_type
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 13 err:err_check
            VAR 3
            FUN 2 1 .
              FUN 1 4 inst
                VAR 1
              FUN 2 1 .
                FUN 1 6 s_or_a
                  VAR 1
                FUN 2 1 .
                  FUN 1 7 s_exist
                    VAR 1
                  FUN 2 1 .
                    FUN 2 8 s_in_out
                      FUN 0 6 output
                      VAR 1
                    FUN 2 1 .
                      FUN 3 13 s_text_binary
                        FUN 0 6 output
                        VAR 2
                        VAR 1
                      FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 9 set_error
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 9 set_error
        VAR 0
      FUN 2 1 .
        FUN 4 18 io:set_output_type
          FUN 0 14 $current_error
          VAR 0
          VAR 1
          FUN 1 9 set_error
            VAR 0
        FUN 2 1 .
          FUN 2 7 $set_bb
            FUN 0 19 $current_error_type
            VAR 1
          FUN 0 2 []
    FUN 0 2 []
CLS 3 4 open
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 4 open
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 4 4 open
          VAR 0
          VAR 1
          VAR 2
          FUN 0 2 []
        FUN 0 2 []
    FUN 0 2 []
CLS 4 4 open
  FUN 2 1 .
    FUN 3 6 clause
      INT 13
      FUN 4 4 open
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 3 var
          VAR 2
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 3
          FUN 2 1 .
            FUN 4 11 io:get_type
              VAR 1
              VAR 3
              VAR 4
              FUN 3 4 prop
                VAR 5
                VAR 6
                VAR 7
            FUN 2 1 .
              FUN 3 5 $open
                VAR 0
                VAR 1
                VAR 8
              FUN 2 1 .
                FUN 2 17 io:check_open_err
                  VAR 8
                  VAR 0
                FUN 2 1 .
                  FUN 3 12 $stream_type
                    VAR 8
                    VAR 9
                    VAR 10
                  FUN 2 1 .
                    FUN 4 14 io:check_repos
                      VAR 9
                      VAR 6
                      VAR 8
                      FUN 4 4 open
                        VAR 0
                        VAR 1
                        VAR 2
                        VAR 3
                    FUN 2 1 .
                      FUN 0 1 !
                      FUN 2 1 .
                        FUN 3 19 io:check_eof_action
                          VAR 7
                          VAR 10
                          VAR 11
                        FUN 2 1 .
                          FUN 2 1 =
                            VAR 12
                            FUN 2 1 .
                              VAR 4
                              FUN 2 1 .
                                VAR 5
                                FUN 2 1 .
                                  VAR 11
                                  FUN 2 1 .
                                    VAR 9
                                    FUN 2 1 .
                                      VAR 10
                                      FUN 2 1 .
                                        FUN 0 6 public
                                        FUN 2 1 .
                                          FUN 1 9 file_name
                                            VAR 0
                                          FUN 2 1 .
                                            FUN 1 4 mode
                                              VAR 1
                                            FUN 0 2 []
                          FUN 2 1 .
                            FUN 1 7 asserta
                              FUN 3 18 $stream_properties
                                VAR 8
                                FUN 0 5 false
                                VAR 12
                            FUN 2 1 .
                              FUN 2 14 io:add_aliases
                                VAR 3
                                VAR 8
                              FUN 2 1 .
                                FUN 2 1 =
                                  VAR 2
                                  FUN 1 7 $stream
                                    VAR 8
                                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 4 open
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 4 4 open
              VAR 0
              VAR 1
              VAR 2
              VAR 3
            FUN 2 1 .
              FUN 1 3 var
                VAR 2
              FUN 2 1 .
                FUN 1 4 inst
                  VAR 0
                FUN 2 1 .
                  FUN 1 11 source_sink
                    VAR 0
                  FUN 2 1 .
                    FUN 1 4 inst
                      VAR 1
                    FUN 2 1 .
                      FUN 1 4 atom
                        VAR 1
                      FUN 2 1 .
                        FUN 1 4 inst
                          VAR 3
                        FUN 2 1 .
                          FUN 1 4 list
                            VAR 3
                          FUN 2 1 .
                            FUN 1 8 instlist
                              VAR 3
                            FUN 2 1 .
                              FUN 1 16 instlistelements
                                VAR 3
                              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 17 io:check_open_err
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 17 io:check_open_err
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 7 integer
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 17 io:check_open_err
          FUN 0 6 eexist
          VAR 0
        FUN 2 1 .
          FUN 1 5 throw
            FUN 2 5 error
              FUN 2 15 existence_error
                FUN 0 11 source_sink
                VAR 0
              FUN 2 1 /
                FUN 0 4 open
                INT 4
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 2 17 io:check_open_err
            FUN 0 5 eperm
            VAR 0
          FUN 2 1 .
            FUN 1 5 throw
              FUN 2 5 error
                FUN 3 16 permission_error
                  FUN 0 4 open
                  FUN 0 11 source_sink
                  VAR 0
                FUN 2 1 /
                  FUN 0 4 open
                  INT 4
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 2 17 io:check_open_err
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 5 throw
                FUN 2 5 error
                  FUN 0 12 system_error
                  FUN 2 4 open
                    VAR 1
                    FUN 0 3 ...
              FUN 0 2 []
          FUN 0 2 []
CLS 4 14 io:check_repos
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 4 14 io:check_repos
        FUN 0 5 false
        FUN 0 4 true
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 6 $close
            VAR 0
          FUN 2 1 .
            FUN 1 5 throw
              FUN 2 5 error
                FUN 3 16 permission_error
                  FUN 0 4 open
                  FUN 0 11 source_sink
                  FUN 1 10 reposition
                    FUN 0 4 true
                VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 14 io:check_repos
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 0 2 []
      FUN 0 2 []
CLS 3 19 io:check_eof_action
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 19 io:check_eof_action
        FUN 0 9 undefined
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 21 io:default_eof_action
            VAR 0
            VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 19 io:check_eof_action
          VAR 0
          VAR 1
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 2 21 io:default_eof_action
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 21 io:default_eof_action
        FUN 0 4 true
        FUN 0 5 reset
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 2 21 io:default_eof_action
          FUN 0 5 false
          FUN 0 8 eof_code
        FUN 0 2 []
      FUN 0 2 []
CLS 4 11 io:get_type
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 11 io:get_type
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 4 atom
          VAR 0
        FUN 2 1 .
          FUN 2 14 io:get_io_type
            VAR 0
            VAR 2
          FUN 2 1 .
            FUN 3 17 io:get_properties
              VAR 1
              FUN 3 4 prop
                FUN 0 4 text
                FUN 0 5 false
                FUN 0 9 undefined
              VAR 3
            FUN 0 2 []
    FUN 0 2 []
CLS 2 14 io:get_io_type
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 14 io:get_io_type
        FUN 0 4 read
        FUN 0 5 input
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 2 14 io:get_io_type
          FUN 0 5 write
          FUN 0 6 output
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 0
          FUN 2 14 io:get_io_type
            FUN 0 6 append
            FUN 0 6 output
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 2 14 io:get_io_type
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 5 throw
                FUN 2 5 error
                  FUN 2 12 domain_error
                    FUN 0 7 io_mode
                    VAR 0
                  FUN 0 4 open
              FUN 0 2 []
          FUN 0 2 []
CLS 3 17 io:get_properties
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 17 io:get_properties
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 17 io:get_properties
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 0
          FUN 2 1 .
            FUN 1 6 nonvar
              VAR 1
            FUN 2 1 .
              FUN 3 15 io:opt_property
                VAR 0
                VAR 2
                VAR 4
              FUN 2 1 .
                FUN 3 17 io:get_properties
                  VAR 1
                  VAR 4
                  VAR 3
                FUN 0 2 []
      FUN 0 2 []
CLS 1 21 io:opt_property$3_0_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 21 io:opt_property$3_0_1
        FUN 0 4 text
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 21 io:opt_property$3_0_1
          FUN 0 6 binary
        FUN 0 2 []
      FUN 0 2 []
CLS 1 21 io:opt_property$3_1_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 21 io:opt_property$3_1_1
        FUN 0 4 true
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 21 io:opt_property$3_1_1
          FUN 0 5 false
        FUN 0 2 []
      FUN 0 2 []
CLS 1 21 io:opt_property$3_2_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 21 io:opt_property$3_2_1
        FUN 0 5 error
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 21 io:opt_property$3_2_1
          FUN 0 8 eof_code
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 0
          FUN 1 21 io:opt_property$3_2_1
            FUN 0 5 reset
          FUN 0 2 []
        FUN 0 2 []
CLS 3 15 io:opt_property
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 3 15 io:opt_property
        FUN 1 4 type
          VAR 0
        FUN 3 4 prop
          VAR 1
          VAR 2
          VAR 3
        FUN 3 4 prop
          VAR 0
          VAR 2
          VAR 3
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 1 21 io:opt_property$3_0_1
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 3 15 io:opt_property
          FUN 1 10 reposition
            VAR 0
          FUN 3 4 prop
            VAR 1
            VAR 2
            VAR 3
          FUN 3 4 prop
            VAR 1
            VAR 0
            VAR 3
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 0
          FUN 2 1 .
            FUN 1 21 io:opt_property$3_1_1
              VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 3 15 io:opt_property
            FUN 1 10 eof_action
              VAR 0
            FUN 3 4 prop
              VAR 1
              VAR 2
              VAR 3
            FUN 3 4 prop
              VAR 1
              VAR 2
              VAR 0
          FUN 2 1 .
            FUN 1 6 nonvar
              VAR 0
            FUN 2 1 .
              FUN 1 21 io:opt_property$3_2_1
                VAR 0
              FUN 2 1 .
                FUN 0 1 !
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 3 15 io:opt_property
              FUN 1 5 alias
                VAR 0
              VAR 1
              VAR 1
            FUN 2 1 .
              FUN 1 6 nonvar
                VAR 0
              FUN 2 1 .
                FUN 1 20 io:nonexistent_alias
                  VAR 0
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 3
              FUN 3 15 io:opt_property
                VAR 0
                VAR 1
                VAR 2
              FUN 2 1 .
                FUN 1 5 throw
                  FUN 2 5 error
                    FUN 2 12 domain_error
                      FUN 0 13 stream_option
                      VAR 0
                    FUN 2 1 /
                      FUN 0 4 open
                      INT 4
                FUN 0 2 []
            FUN 0 2 []
CLS 1 20 io:nonexistent_alias
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 20 io:nonexistent_alias
        VAR 0
      FUN 2 1 .
        FUN 2 6 clause
          FUN 2 13 $stream_alias
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 5 throw
            FUN 2 5 error
              FUN 3 16 permission_error
                FUN 0 4 open
                FUN 0 11 source_sink
                FUN 1 5 alias
                  VAR 0
              FUN 2 1 /
                FUN 0 4 open
                INT 4
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 20 io:nonexistent_alias
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 2 14 io:add_aliases
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 14 io:add_aliases
        FUN 0 2 []
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 14 io:add_aliases
          FUN 2 1 .
            FUN 1 5 alias
              VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 7 asserta
              FUN 2 13 $stream_alias
                VAR 0
                VAR 2
            FUN 2 1 .
              FUN 2 14 io:add_aliases
                VAR 1
                VAR 2
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 2 14 io:add_aliases
            FUN 2 1 .
              VAR 0
              VAR 1
            VAR 2
          FUN 2 1 .
            FUN 2 14 io:add_aliases
              VAR 1
              VAR 2
            FUN 0 2 []
        FUN 0 2 []
CLS 1 5 close
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 5 close
        VAR 0
      FUN 2 1 .
        FUN 2 5 close
          VAR 0
          FUN 0 2 []
        FUN 0 2 []
    FUN 0 2 []
CLS 2 5 close
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 2 5 close
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 1
          FUN 2 1 .
            FUN 2 16 io:stream_to_num
              VAR 0
              VAR 2
            FUN 2 1 .
              FUN 2 6 clause
                FUN 3 18 $stream_properties
                  VAR 2
                  VAR 3
                  FUN 2 1 .
                    VAR 4
                    VAR 5
                VAR 6
              FUN 2 1 .
                FUN 1 16 io:close_options
                  VAR 1
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 3 10 io:close_i
                      VAR 0
                      VAR 2
                      VAR 4
                    FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 5 close
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 5 close
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 6 s_or_a
                  VAR 0
                FUN 2 1 .
                  FUN 1 7 s_exist
                    VAR 0
                  FUN 2 1 .
                    FUN 1 4 inst
                      VAR 1
                    FUN 2 1 .
                      FUN 1 4 list
                        VAR 1
                      FUN 2 1 .
                        FUN 1 8 instlist
                          VAR 1
                        FUN 2 1 .
                          FUN 1 16 instlistelements
                            VAR 1
                          FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 16 io:close_options
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 16 io:close_options
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 16 io:close_options
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 0
          FUN 2 1 .
            FUN 1 6 nonvar
              VAR 1
            FUN 2 1 .
              FUN 1 15 io:close_option
                VAR 0
              FUN 2 1 .
                FUN 1 16 io:close_options
                  VAR 1
                FUN 0 2 []
      FUN 0 2 []
CLS 1 21 io:close_option$1_0_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 21 io:close_option$1_0_1
        FUN 0 4 true
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 21 io:close_option$1_0_1
          FUN 0 5 false
        FUN 0 2 []
      FUN 0 2 []
CLS 1 15 io:close_option
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 15 io:close_option
        FUN 1 5 force
          VAR 0
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 1 21 io:close_option$1_0_1
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 15 io:close_option
          VAR 0
        FUN 2 1 .
          FUN 1 5 throw
            FUN 2 5 error
              FUN 2 12 domain_error
                FUN 0 12 close_option
                VAR 0
              FUN 2 1 /
                FUN 0 5 close
                INT 2
          FUN 0 2 []
      FUN 0 2 []
CLS 1 16 io:close_i$3_0_3
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 16 io:close_i$3_0_3
        VAR 0
      FUN 2 1 .
        FUN 1 20 io:current_input_num
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 9 set_input
              FUN 0 10 user_input
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 16 io:close_i$3_0_3
          VAR 0
        FUN 2 1 .
          FUN 1 21 io:current_output_num
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 10 set_output
                FUN 0 11 user_output
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 1 16 io:close_i$3_0_3
            VAR 0
          FUN 2 1 .
            FUN 1 20 io:current_error_num
              VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 1 9 set_error
                  FUN 0 10 user_error
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 1 16 io:close_i$3_0_3
              VAR 0
            FUN 2 1 .
              FUN 0 4 true
              FUN 0 2 []
          FUN 0 2 []
CLS 2 16 io:close_i$3_0_8
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 16 io:close_i$3_0_8
        VAR 0
        FUN 0 6 output
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 6 $flush
            VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 16 io:close_i$3_0_8
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 3 10 io:close_i
  FUN 2 1 .
    FUN 3 6 clause
      INT 9
      FUN 3 10 io:close_i
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 16 io:close_i$3_0_8
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 2 \+
            FUN 2 6 clause
              FUN 2 13 $stream_alias
                FUN 0 10 user_input
                VAR 1
              VAR 3
          FUN 2 1 .
            FUN 1 2 \+
              FUN 2 6 clause
                FUN 2 13 $stream_alias
                  FUN 0 10 user_error
                  VAR 1
                VAR 4
            FUN 2 1 .
              FUN 1 2 \+
                FUN 2 6 clause
                  FUN 2 13 $stream_alias
                    FUN 0 11 user_output
                    VAR 1
                  VAR 5
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 1 16 io:close_i$3_0_3
                    VAR 1
                  FUN 2 1 .
                    FUN 1 6 $close
                      VAR 1
                    FUN 2 1 .
                      FUN 1 19 database:retractall
                        FUN 2 13 $stream_alias
                          VAR 6
                          VAR 1
                      FUN 2 1 .
                        FUN 1 19 database:retractall
                          FUN 3 18 $stream_properties
                            VAR 1
                            VAR 7
                            VAR 8
                        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 10 io:close_i
          VAR 0
          VAR 1
          VAR 2
        FUN 0 2 []
      FUN 0 2 []
CLS 2 21 stream_property$2_0_4
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 21 stream_property$2_0_4
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 1
        FUN 2 1 .
          FUN 2 16 io:stream_to_num
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 0
                FUN 1 7 $stream
                  VAR 2
              FUN 2 1 .
                FUN 1 15 io:check_stream
                  VAR 0
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 2 21 stream_property$2_0_4
          VAR 0
          VAR 0
        FUN 2 1 .
          FUN 2 6 clause
            FUN 3 18 $stream_properties
              VAR 1
              VAR 2
              VAR 3
            VAR 4
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              FUN 1 7 $stream
                VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 2 15 stream_property
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 15 stream_property
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 21 stream_property$2_0_4
          VAR 2
          VAR 0
        FUN 2 1 .
          FUN 1 17 io:check_property
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 7 findall
                FUN 2 1 -
                  VAR 2
                  VAR 1
                FUN 2 22 io:get_stream_property
                  VAR 2
                  VAR 1
                VAR 3
              FUN 2 1 .
                FUN 2 12 lists:member
                  FUN 2 1 -
                    VAR 2
                    VAR 1
                  VAR 3
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 15 stream_property
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 15 stream_property
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 6 stream
                VAR 0
              FUN 2 1 .
                FUN 1 15 stream_property
                  VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 15 io:check_stream
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 15 io:check_stream
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 1 15 io:check_stream
          FUN 1 7 $stream
            VAR 0
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 0
          FUN 2 1 .
            FUN 2 6 clause
              FUN 3 18 $stream_properties
                VAR 0
                VAR 1
                VAR 2
              VAR 3
            FUN 0 2 []
      FUN 0 2 []
CLS 1 17 io:check_property
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 17 io:check_property
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 17 io:check_property
          FUN 1 4 type
            VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 1 17 io:check_property
            FUN 1 4 mode
              VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 1 17 io:check_property
              FUN 1 5 alias
                VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 0
              FUN 1 17 io:check_property
                FUN 0 5 input
              FUN 2 1 .
                FUN 0 1 !
                FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 0
                FUN 1 17 io:check_property
                  FUN 0 6 output
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 1
                  FUN 1 17 io:check_property
                    FUN 1 8 position
                      VAR 0
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 1
                    FUN 1 17 io:check_property
                      FUN 1 9 file_name
                        VAR 0
                    FUN 2 1 .
                      FUN 0 1 !
                      FUN 0 2 []
                  FUN 2 1 .
                    FUN 3 6 clause
                      INT 1
                      FUN 1 17 io:check_property
                        FUN 1 13 end_of_stream
                          VAR 0
                      FUN 2 1 .
                        FUN 0 1 !
                        FUN 0 2 []
                    FUN 2 1 .
                      FUN 3 6 clause
                        INT 1
                        FUN 1 17 io:check_property
                          FUN 1 10 eof_action
                            VAR 0
                        FUN 2 1 .
                          FUN 0 1 !
                          FUN 0 2 []
                      FUN 2 1 .
                        FUN 3 6 clause
                          INT 1
                          FUN 1 17 io:check_property
                            FUN 1 10 reposition
                              VAR 0
                          FUN 2 1 .
                            FUN 0 1 !
                            FUN 0 2 []
                        FUN 0 2 []
CLS 9 28 io:get_stream_property$2_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 8
      FUN 9 28 io:get_stream_property$2_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
        VAR 5
        VAR 6
        VAR 7
        VAR 7
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 8
        FUN 9 28 io:get_stream_property$2_0_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 5
          VAR 6
          VAR 7
          FUN 1 4 type
            VAR 6
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 8
          FUN 9 28 io:get_stream_property$2_0_0
            VAR 0
            VAR 1
            VAR 2
            VAR 3
            VAR 4
            VAR 5
            VAR 6
            VAR 7
            FUN 1 10 reposition
              VAR 5
          FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 8
            FUN 9 28 io:get_stream_property$2_0_0
              VAR 0
              VAR 1
              VAR 2
              VAR 3
              VAR 4
              VAR 5
              VAR 6
              VAR 7
              FUN 1 10 eof_action
                VAR 4
            FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 9
              FUN 9 28 io:get_stream_property$2_0_0
                VAR 0
                VAR 1
                VAR 2
                VAR 3
                VAR 4
                VAR 5
                VAR 6
                VAR 7
                VAR 8
              FUN 2 1 .
                FUN 2 12 lists:member
                  VAR 8
                  VAR 3
                FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 10
                FUN 9 28 io:get_stream_property$2_0_0
                  VAR 0
                  VAR 1
                  VAR 2
                  VAR 3
                  VAR 4
                  VAR 5
                  VAR 6
                  VAR 7
                  FUN 1 5 alias
                    VAR 8
                FUN 2 1 .
                  FUN 2 6 clause
                    FUN 2 13 $stream_alias
                      VAR 8
                      VAR 2
                    VAR 9
                  FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 9
                  FUN 9 28 io:get_stream_property$2_0_0
                    VAR 0
                    VAR 1
                    VAR 2
                    VAR 3
                    VAR 4
                    VAR 5
                    VAR 6
                    VAR 7
                    FUN 1 13 end_of_stream
                      VAR 8
                  FUN 2 1 .
                    FUN 5 20 io:get_end_of_stream
                      VAR 7
                      VAR 2
                      VAR 1
                      VAR 0
                      VAR 8
                    FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 9
                    FUN 9 28 io:get_stream_property$2_0_0
                      VAR 0
                      VAR 1
                      VAR 2
                      VAR 3
                      VAR 4
                      FUN 0 4 true
                      VAR 5
                      VAR 6
                      VAR 7
                    FUN 2 1 .
                      FUN 0 1 !
                      FUN 2 1 .
                        FUN 2 1 =
                          VAR 7
                          FUN 1 8 position
                            FUN 1 16 $stream_position
                              VAR 8
                        FUN 2 1 .
                          FUN 2 8 $get_pos
                            VAR 2
                            VAR 8
                          FUN 0 2 []
                  FUN 0 2 []
CLS 2 28 io:get_stream_property$2_0_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 28 io:get_stream_property$2_0_2
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              FUN 0 6 public
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 28 io:get_stream_property$2_0_2
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 2 22 io:get_stream_property
  FUN 2 1 .
    FUN 3 6 clause
      INT 11
      FUN 2 22 io:get_stream_property
        FUN 1 7 $stream
          VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 28 io:get_stream_property$2_0_2
          VAR 2
          VAR 0
        FUN 2 1 .
          FUN 2 6 clause
            FUN 3 18 $stream_properties
              VAR 0
              VAR 3
              FUN 2 1 .
                VAR 4
                FUN 2 1 .
                  VAR 5
                  FUN 2 1 .
                    VAR 6
                    FUN 2 1 .
                      VAR 7
                      FUN 2 1 .
                        VAR 8
                        FUN 2 1 .
                          VAR 2
                          VAR 9
            VAR 10
          FUN 2 1 .
            FUN 9 28 io:get_stream_property$2_0_0
              VAR 8
              VAR 3
              VAR 0
              VAR 9
              VAR 6
              VAR 7
              VAR 5
              VAR 4
              VAR 1
            FUN 0 2 []
    FUN 0 2 []
CLS 5 20 io:get_end_of_stream
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 5 20 io:get_end_of_stream
        FUN 0 6 output
        VAR 0
        VAR 1
        VAR 2
        FUN 0 3 not
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 5 20 io:get_end_of_stream
          FUN 0 5 input
          VAR 0
          VAR 1
          FUN 0 4 true
          FUN 0 3 not
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 5 20 io:get_end_of_stream
            FUN 0 5 input
            VAR 0
            FUN 0 4 true
            VAR 1
            FUN 0 4 past
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 5
            FUN 5 20 io:get_end_of_stream
              FUN 0 5 input
              VAR 0
              VAR 1
              VAR 2
              FUN 0 2 at
            FUN 2 1 .
              FUN 4 13 io:with_input
                FUN 1 7 $stream
                  VAR 0
                VAR 3
                FUN 1 13 char_io:peekc
                  VAR 4
                FUN 0 17 get_end_of_stream
              FUN 2 1 .
                FUN 2 1 =
                  VAR 4
                  INT -1
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 3
              FUN 5 20 io:get_end_of_stream
                FUN 0 5 input
                VAR 0
                VAR 1
                VAR 2
                FUN 0 3 not
              FUN 0 2 []
            FUN 0 2 []
CLS 0 16 at_end_of_stream
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 0 16 at_end_of_stream
      FUN 2 1 .
        FUN 1 13 current_input
          VAR 0
        FUN 2 1 .
          FUN 1 16 at_end_of_stream
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 1 16 at_end_of_stream
  FUN 2 1 .
    FUN 3 6 clause
      INT 10
      FUN 1 16 at_end_of_stream
        VAR 0
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 2 16 io:stream_to_num
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 6 clause
              FUN 3 18 $stream_properties
                VAR 1
                VAR 2
                FUN 2 1 .
                  FUN 0 5 input
                  FUN 2 1 .
                    VAR 3
                    FUN 2 1 .
                      VAR 4
                      FUN 2 1 .
                        VAR 5
                        FUN 2 1 .
                          VAR 6
                          VAR 7
              VAR 8
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 5 20 io:get_end_of_stream
                  FUN 0 5 input
                  VAR 1
                  VAR 2
                  VAR 6
                  VAR 9
                FUN 2 1 .
                  FUN 2 2 \=
                    VAR 9
                    FUN 0 3 not
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 16 at_end_of_stream
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 16 at_end_of_stream
              VAR 0
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 6 s_or_a
                  VAR 0
                FUN 2 1 .
                  FUN 1 7 s_exist
                    VAR 0
                  FUN 2 1 .
                    FUN 2 8 s_in_out
                      FUN 0 5 input
                      VAR 0
                    FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 19 set_stream_position
  FUN 2 1 .
    FUN 3 6 clause
      INT 10
      FUN 2 19 set_stream_position
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 1
          FUN 2 1 .
            FUN 2 16 io:stream_to_num
              VAR 0
              VAR 2
            FUN 2 1 .
              FUN 2 24 io:check_stream_position
                VAR 1
                VAR 3
              FUN 2 1 .
                FUN 2 1 =
                  VAR 4
                  FUN 2 1 .
                    VAR 5
                    FUN 2 1 .
                      VAR 6
                      FUN 2 1 .
                        VAR 7
                        FUN 2 1 .
                          FUN 0 4 true
                          VAR 8
                FUN 2 1 .
                  FUN 1 7 retract
                    FUN 3 18 $stream_properties
                      VAR 2
                      VAR 9
                      VAR 4
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 1 7 asserta
                        FUN 3 18 $stream_properties
                          VAR 2
                          FUN 0 5 false
                          VAR 4
                      FUN 2 1 .
                        FUN 2 8 $set_pos
                          VAR 2
                          VAR 3
                        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 19 set_stream_position
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 19 set_stream_position
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 4 inst
                  VAR 1
                FUN 2 1 .
                  FUN 1 6 s_or_a
                    VAR 0
                  FUN 2 1 .
                    FUN 1 8 position
                      VAR 1
                    FUN 2 1 .
                      FUN 1 7 s_exist
                        VAR 0
                      FUN 2 1 .
                        FUN 1 12 s_reposition
                          VAR 0
                        FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 24 io:check_stream_position
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 24 io:check_stream_position
        FUN 1 16 $stream_position
          VAR 0
        VAR 0
      FUN 0 2 []
    FUN 0 2 []
CLS 0 15 io:init_streams
  FUN 2 1 .
    FUN 3 6 clause
      INT 12
      FUN 0 15 io:init_streams
      FUN 2 1 .
        FUN 1 20 io:current_input_num
          VAR 0
        FUN 2 1 .
          FUN 1 21 io:current_output_num
            VAR 1
          FUN 2 1 .
            FUN 1 15 io:error_stream
              FUN 1 7 $stream
                VAR 2
            FUN 2 1 .
              FUN 3 12 $stream_type
                VAR 0
                VAR 3
                VAR 4
              FUN 2 1 .
                FUN 3 12 $stream_type
                  VAR 1
                  VAR 5
                  VAR 6
                FUN 2 1 .
                  FUN 3 12 $stream_type
                    VAR 2
                    VAR 7
                    VAR 8
                  FUN 2 1 .
                    FUN 2 21 io:default_eof_action
                      VAR 4
                      VAR 9
                    FUN 2 1 .
                      FUN 2 21 io:default_eof_action
                        VAR 6
                        VAR 10
                      FUN 2 1 .
                        FUN 2 21 io:default_eof_action
                          VAR 8
                          VAR 11
                        FUN 2 1 .
                          FUN 1 7 asserta
                            FUN 2 13 $stream_alias
                              FUN 0 10 user_input
                              VAR 0
                          FUN 2 1 .
                            FUN 1 7 asserta
                              FUN 2 13 $stream_alias
                                FUN 0 11 user_output
                                VAR 1
                            FUN 2 1 .
                              FUN 1 7 asserta
                                FUN 2 13 $stream_alias
                                  FUN 0 10 user_error
                                  VAR 2
                              FUN 2 1 .
                                FUN 1 7 asserta
                                  FUN 3 18 $stream_properties
                                    VAR 0
                                    FUN 0 5 false
                                    FUN 2 1 .
                                      FUN 0 5 input
                                      FUN 2 1 .
                                        FUN 0 4 text
                                        FUN 2 1 .
                                          VAR 9
                                          FUN 2 1 .
                                            VAR 3
                                            FUN 2 1 .
                                              VAR 4
                                              FUN 2 1 .
                                                FUN 0 6 public
                                                FUN 2 1 .
                                                  FUN 1 4 mode
                                                    FUN 0 4 read
                                                  FUN 0 2 []
                                FUN 2 1 .
                                  FUN 1 7 asserta
                                    FUN 3 18 $stream_properties
                                      VAR 1
                                      FUN 0 5 false
                                      FUN 2 1 .
                                        FUN 0 6 output
                                        FUN 2 1 .
                                          FUN 0 4 text
                                          FUN 2 1 .
                                            VAR 10
                                            FUN 2 1 .
                                              VAR 5
                                              FUN 2 1 .
                                                VAR 6
                                                FUN 2 1 .
                                                  FUN 0 6 public
                                                  FUN 2 1 .
                                                    FUN 1 4 mode
                                                      FUN 0 6 append
                                                    FUN 0 2 []
                                  FUN 2 1 .
                                    FUN 1 7 asserta
                                      FUN 3 18 $stream_properties
                                        VAR 2
                                        FUN 0 5 false
                                        FUN 2 1 .
                                          FUN 0 6 output
                                          FUN 2 1 .
                                            FUN 0 4 text
                                            FUN 2 1 .
                                              VAR 11
                                              FUN 2 1 .
                                                VAR 7
                                                FUN 2 1 .
                                                  VAR 8
                                                  FUN 2 1 .
                                                    FUN 0 7 private
                                                    FUN 2 1 .
                                                      FUN 1 4 mode
                                                        FUN 0 6 append
                                                      FUN 0 2 []
                                    FUN 2 1 .
                                      FUN 2 7 $set_bb
                                        FUN 0 19 $current_input_type
                                        FUN 0 4 text
                                      FUN 2 1 .
                                        FUN 2 7 $set_bb
                                          FUN 0 20 $current_output_type
                                          FUN 0 4 text
                                        FUN 2 1 .
                                          FUN 2 7 $set_bb
                                            FUN 0 19 $current_error_type
                                            FUN 0 4 text
                                          FUN 0 2 []
    FUN 0 2 []
CLS 4 13 io:with_input
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 13 io:with_input
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 13 current_input
          VAR 4
        FUN 2 1 .
          FUN 3 17 io:set_input_type
            VAR 0
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 2 26 execution:call_cleanup_det
              VAR 2
              FUN 1 9 set_input
                VAR 4
            FUN 0 2 []
    FUN 0 2 []
CLS 4 14 io:with_output
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 14 io:with_output
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 14 current_output
          VAR 4
        FUN 2 1 .
          FUN 4 18 io:set_output_type
            FUN 0 15 $current_output
            VAR 0
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 2 26 execution:call_cleanup_det
              VAR 2
              FUN 1 10 set_output
                VAR 4
            FUN 0 2 []
    FUN 0 2 []
CLS 4 13 io:with_error
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 13 io:with_error
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 13 current_error
          VAR 4
        FUN 2 1 .
          FUN 4 18 io:set_output_type
            FUN 0 14 $current_error
            VAR 0
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 2 26 execution:call_cleanup_det
              VAR 2
              FUN 1 9 set_error
                VAR 4
            FUN 0 2 []
    FUN 0 2 []
CLS 3 18 io:restore_streams
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 18 io:restore_streams
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 9 set_input
          VAR 0
        FUN 2 1 .
          FUN 1 10 set_output
            VAR 1
          FUN 2 1 .
            FUN 1 9 set_error
              VAR 2
            FUN 0 2 []
    FUN 0 2 []
CLS 1 18 io:default_streams
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 1 18 io:default_streams
        VAR 0
      FUN 2 1 .
        FUN 1 13 current_input
          VAR 1
        FUN 2 1 .
          FUN 1 14 current_output
            VAR 2
          FUN 2 1 .
            FUN 1 13 current_error
              VAR 3
            FUN 2 1 .
              FUN 1 9 set_input
                FUN 0 10 user_input
              FUN 2 1 .
                FUN 1 10 set_output
                  FUN 0 11 user_output
                FUN 2 1 .
                  FUN 1 9 set_error
                    FUN 0 10 user_error
                  FUN 2 1 .
                    FUN 2 26 execution:call_cleanup_det
                      VAR 0
                      FUN 3 18 io:restore_streams
                        VAR 1
                        VAR 2
                        VAR 3
                    FUN 0 2 []
    FUN 0 2 []
CLS 1 15 io:error_stream
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 15 io:error_stream
        FUN 1 7 $stream
          VAR 0
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 14 $current_error
          VAR 0
        FUN 0 2 []
    FUN 0 2 []
CLS 1 13 current_error
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 13 current_error
        FUN 1 7 $stream
          VAR 0
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 14 $current_error
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 1 13 current_error
          FUN 1 7 $stream
            VAR 0
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 0
          FUN 2 1 .
            FUN 2 6 clause
              FUN 3 18 $stream_properties
                VAR 0
                VAR 1
                VAR 2
              VAR 3
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 0 4 fail
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 1 13 current_error
            VAR 0
          FUN 2 1 .
            FUN 2 13 err:err_check
              FUN 1 13 current_error
                VAR 0
              FUN 2 1 .
                FUN 1 6 stream
                  VAR 0
                FUN 0 2 []
            FUN 0 2 []
        FUN 0 2 []
CLS 1 20 io:current_error_num
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 20 io:current_error_num
        VAR 0
      FUN 2 1 .
        FUN 2 7 $get_bb
          FUN 0 14 $current_error
          VAR 0
        FUN 0 2 []
    FUN 0 2 []
CLS 0 12 flush_output
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 0 12 flush_output
      FUN 2 1 .
        FUN 1 21 io:current_output_num
          VAR 0
        FUN 2 1 .
          FUN 1 6 $flush
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 1 12 flush_output
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 12 flush_output
        VAR 0
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 2 16 io:stream_to_num
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 3 14 io:stream_type
              VAR 1
              FUN 0 6 output
              VAR 2
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 1 6 $flush
                  VAR 1
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 12 flush_output
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 12 flush_output
              VAR 0
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 6 s_or_a
                  VAR 0
                FUN 2 1 .
                  FUN 1 7 s_exist
                    VAR 0
                  FUN 2 1 .
                    FUN 2 8 s_in_out
                      FUN 0 6 output
                      VAR 0
                    FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 13 io:handle_eof
  FUN 2 1 .
    FUN 3 6 clause
      INT 8
      FUN 1 13 io:handle_eof
        VAR 0
      FUN 2 1 .
        FUN 1 20 io:current_input_num
          VAR 1
        FUN 2 1 .
          FUN 2 6 clause
            FUN 3 18 $stream_properties
              VAR 1
              VAR 2
              FUN 2 1 .
                VAR 3
                FUN 2 1 .
                  VAR 4
                  FUN 2 1 .
                    VAR 5
                    VAR 6
            VAR 7
          FUN 2 1 .
            FUN 4 13 io:handle_eof
              VAR 5
              VAR 2
              VAR 0
              VAR 1
            FUN 0 2 []
    FUN 0 2 []
CLS 4 13 io:handle_eof
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 4 13 io:handle_eof
        FUN 0 8 eof_code
        FUN 0 5 false
        FUN 0 3 get
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 15 io:set_past_eof
            VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 4 13 io:handle_eof
          FUN 0 5 error
          FUN 0 5 false
          FUN 0 3 get
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 15 io:set_past_eof
              VAR 0
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 4 13 io:handle_eof
            FUN 0 5 error
            FUN 0 4 true
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 5 throw
                FUN 2 5 error
                  FUN 3 16 permission_error
                    FUN 0 5 input
                    FUN 0 18 past_end_of_stream
                    FUN 1 7 $stream
                      VAR 1
                  FUN 0 4 getc
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 4
            FUN 4 13 io:handle_eof
              VAR 0
              VAR 1
              VAR 2
              VAR 3
            FUN 0 2 []
          FUN 0 2 []
CLS 1 15 io:set_past_eof
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 15 io:set_past_eof
        VAR 0
      FUN 2 1 .
        FUN 1 7 retract
          FUN 3 18 $stream_properties
            VAR 0
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 1 7 asserta
            FUN 3 18 $stream_properties
              VAR 0
              FUN 0 4 true
              VAR 2
          FUN 0 2 []
    FUN 0 2 []
CLS 2 12 io:read_line
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 12 io:read_line
        FUN 0 2 []
        INT 10
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 2 12 io:read_line
          FUN 0 2 []
          INT -1
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 2 12 io:read_line
            FUN 2 1 .
              VAR 0
              VAR 1
            VAR 2
          FUN 2 1 .
            FUN 1 8 get_code
              VAR 0
            FUN 2 1 .
              FUN 2 12 io:read_line
                VAR 1
                VAR 0
              FUN 0 2 []
        FUN 0 2 []
CLS 1 12 io:read_line
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 12 io:read_line
        VAR 0
      FUN 2 1 .
        FUN 2 12 io:read_line
          VAR 0
          INT 0
        FUN 0 2 []
    FUN 0 2 []
CLS 1 12 char_io:getc
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 12 char_io:getc
        VAR 0
      FUN 2 1 .
        FUN 1 9 $get_code
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 12 char_io:getc
          VAR 0
        FUN 2 1 .
          FUN 1 13 io:handle_eof
            FUN 0 3 get
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              INT -1
            FUN 0 2 []
      FUN 0 2 []
CLS 1 13 char_io:peekc
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 13 char_io:peekc
        VAR 0
      FUN 2 1 .
        FUN 1 10 $peek_code
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 13 char_io:peekc
          VAR 0
        FUN 2 1 .
          FUN 1 13 io:handle_eof
            FUN 0 4 peek
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              INT -1
            FUN 0 2 []
      FUN 0 2 []
CLS 1 27 char_io:check_in_code$1_1_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 27 char_io:check_in_code$1_1_0
        INT -1
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 27 char_io:check_in_code$1_1_0
          VAR 0
        FUN 2 1 .
          FUN 2 1 >
            VAR 0
            INT 0
          FUN 2 1 .
            FUN 2 1 <
              VAR 0
              INT 256
            FUN 0 2 []
      FUN 0 2 []
CLS 1 21 char_io:check_in_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 21 char_io:check_in_code
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 21 char_io:check_in_code
          VAR 0
        FUN 2 1 .
          FUN 1 7 integer
            VAR 0
          FUN 2 1 .
            FUN 1 27 char_io:check_in_code$1_1_0
              VAR 0
            FUN 0 2 []
      FUN 0 2 []
CLS 1 27 char_io:check_in_char$1_1_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 27 char_io:check_in_char$1_1_0
        FUN 0 11 end_of_file
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 27 char_io:check_in_char$1_1_0
          VAR 0
        FUN 2 1 .
          FUN 2 11 $atom_codes
            VAR 0
            FUN 2 1 .
              VAR 1
              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 21 char_io:check_in_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 21 char_io:check_in_char
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 21 char_io:check_in_char
          VAR 0
        FUN 2 1 .
          FUN 1 4 atom
            VAR 0
          FUN 2 1 .
            FUN 1 27 char_io:check_in_char$1_1_0
              VAR 0
            FUN 0 2 []
      FUN 0 2 []
CLS 1 21 char_io:check_in_byte
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 21 char_io:check_in_byte
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 21 char_io:check_in_byte
          VAR 0
        FUN 2 1 .
          FUN 1 7 integer
            VAR 0
          FUN 2 1 .
            FUN 2 2 >=
              VAR 0
              INT -1
            FUN 2 1 .
              FUN 2 1 <
                VAR 0
                INT 256
              FUN 0 2 []
      FUN 0 2 []
CLS 1 22 char_io:check_ret_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 22 char_io:check_ret_code
        INT 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 0 4 fail
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 22 char_io:check_ret_code
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 2 21 char_io:get_code_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 21 char_io:get_code_char
        INT -1
        FUN 0 11 end_of_file
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 21 char_io:get_code_char
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 10 atom_codes
            VAR 1
            FUN 2 1 .
              VAR 0
              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 8 get_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 8 get_code
        VAR 0
      FUN 2 1 .
        FUN 1 21 char_io:check_in_code
          VAR 0
        FUN 2 1 .
          FUN 0 19 io:check_input_text
          FUN 2 1 .
            FUN 1 12 char_io:getc
              VAR 1
            FUN 2 1 .
              FUN 1 22 char_io:check_ret_code
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
        INT 2
        FUN 1 8 get_code
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 8 get_code
              VAR 0
            FUN 2 1 .
              FUN 1 7 integer
                VAR 0
              FUN 2 1 .
                FUN 1 12 in_char_code
                  VAR 0
                FUN 2 1 .
                  FUN 1 2 do
                    FUN 1 13 current_input
                      VAR 1
                  FUN 2 1 .
                    FUN 3 13 s_text_binary
                      FUN 0 5 input
                      FUN 0 4 text
                      VAR 1
                    FUN 2 1 .
                      FUN 0 15 input_character
                      FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 8 get_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 8 get_char
        VAR 0
      FUN 2 1 .
        FUN 1 21 char_io:check_in_char
          VAR 0
        FUN 2 1 .
          FUN 0 19 io:check_input_text
          FUN 2 1 .
            FUN 1 12 char_io:getc
              VAR 1
            FUN 2 1 .
              FUN 1 22 char_io:check_ret_code
                VAR 1
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 21 char_io:get_code_char
                    VAR 1
                    VAR 0
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 8 get_char
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 8 get_char
              VAR 0
            FUN 2 1 .
              FUN 1 7 in_char
                VAR 0
              FUN 2 1 .
                FUN 1 2 do
                  FUN 1 13 current_input
                    VAR 1
                FUN 2 1 .
                  FUN 3 13 s_text_binary
                    FUN 0 5 input
                    FUN 0 4 text
                    VAR 1
                  FUN 2 1 .
                    FUN 0 15 input_character
                    FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 8 get_byte
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 8 get_byte
        VAR 0
      FUN 2 1 .
        FUN 1 21 char_io:check_in_byte
          VAR 0
        FUN 2 1 .
          FUN 0 21 io:check_input_binary
          FUN 2 1 .
            FUN 1 12 char_io:getc
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
        INT 2
        FUN 1 8 get_byte
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 8 get_byte
              VAR 0
            FUN 2 1 .
              FUN 1 7 in_byte
                VAR 0
              FUN 2 1 .
                FUN 1 2 do
                  FUN 1 13 current_input
                    VAR 1
                FUN 2 1 .
                  FUN 3 13 s_text_binary
                    FUN 0 5 input
                    FUN 0 6 binary
                    VAR 1
                  FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 9 peek_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 9 peek_code
        VAR 0
      FUN 2 1 .
        FUN 1 21 char_io:check_in_code
          VAR 0
        FUN 2 1 .
          FUN 0 19 io:check_input_text
          FUN 2 1 .
            FUN 1 13 char_io:peekc
              VAR 1
            FUN 2 1 .
              FUN 1 22 char_io:check_ret_code
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
        INT 2
        FUN 1 9 peek_code
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 9 peek_code
              VAR 0
            FUN 2 1 .
              FUN 1 7 integer
                VAR 0
              FUN 2 1 .
                FUN 1 12 in_char_code
                  VAR 0
                FUN 2 1 .
                  FUN 1 2 do
                    FUN 1 13 current_input
                      VAR 1
                  FUN 2 1 .
                    FUN 3 13 s_text_binary
                      FUN 0 5 input
                      FUN 0 4 text
                      VAR 1
                    FUN 2 1 .
                      FUN 0 15 input_character
                      FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 9 peek_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 9 peek_char
        VAR 0
      FUN 2 1 .
        FUN 1 21 char_io:check_in_char
          VAR 0
        FUN 2 1 .
          FUN 0 19 io:check_input_text
          FUN 2 1 .
            FUN 1 13 char_io:peekc
              VAR 1
            FUN 2 1 .
              FUN 1 22 char_io:check_ret_code
                VAR 1
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 21 char_io:get_code_char
                    VAR 1
                    VAR 0
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 9 peek_char
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 9 peek_char
              VAR 0
            FUN 2 1 .
              FUN 1 7 in_char
                VAR 0
              FUN 2 1 .
                FUN 1 2 do
                  FUN 1 13 current_input
                    VAR 1
                FUN 2 1 .
                  FUN 3 13 s_text_binary
                    FUN 0 5 input
                    FUN 0 4 text
                    VAR 1
                  FUN 2 1 .
                    FUN 0 15 input_character
                    FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 9 peek_byte
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 9 peek_byte
        VAR 0
      FUN 2 1 .
        FUN 1 21 char_io:check_in_byte
          VAR 0
        FUN 2 1 .
          FUN 0 21 io:check_input_binary
          FUN 2 1 .
            FUN 1 13 char_io:peekc
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
        INT 2
        FUN 1 9 peek_byte
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 9 peek_byte
              VAR 0
            FUN 2 1 .
              FUN 1 7 in_byte
                VAR 0
              FUN 2 1 .
                FUN 1 2 do
                  FUN 1 13 current_input
                    VAR 1
                FUN 2 1 .
                  FUN 3 13 s_text_binary
                    FUN 0 5 input
                    FUN 0 6 binary
                    VAR 1
                  FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 8 get_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 8 get_code
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 21 char_io:check_in_code
          VAR 1
        FUN 2 1 .
          FUN 4 13 io:with_input
            VAR 0
            FUN 0 4 text
            FUN 1 12 char_io:getc
              VAR 2
            FUN 2 8 get_code
              VAR 0
              VAR 1
          FUN 2 1 .
            FUN 1 22 char_io:check_ret_code
              VAR 2
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 1 =
                  VAR 1
                  VAR 2
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 8 get_code
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 8 get_code
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 7 integer
                VAR 1
              FUN 2 1 .
                FUN 1 12 in_char_code
                  VAR 1
                FUN 2 1 .
                  FUN 0 15 input_character
                  FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 8 get_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 8 get_char
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 21 char_io:check_in_char
          VAR 1
        FUN 2 1 .
          FUN 4 13 io:with_input
            VAR 0
            FUN 0 4 text
            FUN 1 12 char_io:getc
              VAR 2
            FUN 2 8 get_char
              VAR 0
              VAR 1
          FUN 2 1 .
            FUN 1 22 char_io:check_ret_code
              VAR 2
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 21 char_io:get_code_char
                  VAR 2
                  VAR 1
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 8 get_char
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 8 get_char
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 7 in_char
                VAR 1
              FUN 2 1 .
                FUN 0 15 input_character
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 8 get_byte
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 8 get_byte
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 21 char_io:check_in_byte
          VAR 1
        FUN 2 1 .
          FUN 4 13 io:with_input
            VAR 0
            FUN 0 6 binary
            FUN 1 12 char_io:getc
              VAR 2
            FUN 2 8 get_byte
              VAR 0
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
        INT 2
        FUN 2 8 get_byte
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 8 get_byte
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 7 in_byte
                VAR 1
              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 9 peek_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 9 peek_code
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 21 char_io:check_in_code
          VAR 1
        FUN 2 1 .
          FUN 4 13 io:with_input
            VAR 0
            FUN 0 4 text
            FUN 1 13 char_io:peekc
              VAR 2
            FUN 2 9 peek_code
              VAR 0
              VAR 1
          FUN 2 1 .
            FUN 1 22 char_io:check_ret_code
              VAR 2
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 1 =
                  VAR 1
                  VAR 2
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 9 peek_code
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 9 peek_code
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 7 integer
                VAR 1
              FUN 2 1 .
                FUN 1 12 in_char_code
                  VAR 1
                FUN 2 1 .
                  FUN 0 15 input_character
                  FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 9 peek_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 9 peek_char
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 21 char_io:check_in_char
          VAR 1
        FUN 2 1 .
          FUN 4 13 io:with_input
            VAR 0
            FUN 0 4 text
            FUN 1 13 char_io:peekc
              VAR 2
            FUN 2 9 peek_char
              VAR 0
              VAR 1
          FUN 2 1 .
            FUN 1 22 char_io:check_ret_code
              VAR 2
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 21 char_io:get_code_char
                  VAR 2
                  VAR 1
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 9 peek_char
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 9 peek_char
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 7 in_char
                VAR 1
              FUN 2 1 .
                FUN 0 15 input_character
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 9 peek_byte
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 9 peek_byte
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 21 char_io:check_in_byte
          VAR 1
        FUN 2 1 .
          FUN 4 13 io:with_input
            VAR 0
            FUN 0 6 binary
            FUN 1 13 char_io:peekc
              VAR 2
            FUN 2 9 peek_byte
              VAR 0
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
        INT 2
        FUN 2 9 peek_byte
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 9 peek_byte
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 7 in_byte
                VAR 1
              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 8 put_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 8 put_code
        VAR 0
      FUN 2 1 .
        FUN 1 7 integer
          VAR 0
        FUN 2 1 .
          FUN 2 1 >
            VAR 0
            INT 0
          FUN 2 1 .
            FUN 2 1 <
              VAR 0
              INT 256
            FUN 2 1 .
              FUN 0 20 io:check_output_text
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 1 9 $put_code
                    VAR 0
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 8 put_code
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 8 put_code
              VAR 0
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 7 integer
                  VAR 0
                FUN 2 1 .
                  FUN 1 9 char_code
                    VAR 0
                  FUN 2 1 .
                    FUN 1 2 do
                      FUN 1 14 current_output
                        VAR 1
                    FUN 2 1 .
                      FUN 3 13 s_text_binary
                        FUN 0 6 output
                        FUN 0 4 text
                        VAR 1
                      FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 8 put_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 8 put_char
        VAR 0
      FUN 2 1 .
        FUN 2 11 $atom_codes
          VAR 0
          FUN 2 1 .
            VAR 1
            FUN 0 2 []
        FUN 2 1 .
          FUN 0 20 io:check_output_text
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 9 $put_code
                VAR 1
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 8 put_char
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 8 put_char
              VAR 0
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 9 character
                  VAR 0
                FUN 2 1 .
                  FUN 1 2 do
                    FUN 1 14 current_output
                      VAR 1
                  FUN 2 1 .
                    FUN 3 13 s_text_binary
                      FUN 0 6 output
                      FUN 0 4 text
                      VAR 1
                    FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 8 put_byte
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 8 put_byte
        VAR 0
      FUN 2 1 .
        FUN 1 7 integer
          VAR 0
        FUN 2 1 .
          FUN 2 2 >=
            VAR 0
            INT 0
          FUN 2 1 .
            FUN 2 1 <
              VAR 0
              INT 256
            FUN 2 1 .
              FUN 0 22 io:check_output_binary
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 1 9 $put_code
                    VAR 0
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 8 put_byte
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 8 put_byte
              VAR 0
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 4 byte
                  VAR 0
                FUN 2 1 .
                  FUN 1 2 do
                    FUN 1 14 current_output
                      VAR 1
                  FUN 2 1 .
                    FUN 3 13 s_text_binary
                      FUN 0 6 output
                      FUN 0 6 binary
                      VAR 1
                    FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 8 put_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 8 put_code
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 7 integer
          VAR 1
        FUN 2 1 .
          FUN 2 1 >
            VAR 1
            INT 0
          FUN 2 1 .
            FUN 2 1 <
              VAR 1
              INT 256
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 4 14 io:with_output
                  VAR 0
                  FUN 0 4 text
                  FUN 1 9 $put_code
                    VAR 1
                  FUN 2 8 put_code
                    VAR 0
                    VAR 1
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 8 put_code
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 8 put_code
              VAR 1
            FUN 2 1 .
              FUN 1 4 inst
                VAR 1
              FUN 2 1 .
                FUN 1 7 integer
                  VAR 1
                FUN 2 1 .
                  FUN 1 9 char_code
                    VAR 1
                  FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 8 put_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 8 put_char
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 11 $atom_codes
          VAR 1
          FUN 2 1 .
            VAR 2
            FUN 0 2 []
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 14 io:with_output
              VAR 0
              FUN 0 4 text
              FUN 1 9 $put_code
                VAR 2
              FUN 2 8 put_char
                VAR 0
                VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 8 put_char
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 8 put_char
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 4 inst
                VAR 1
              FUN 2 1 .
                FUN 1 9 character
                  VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 8 put_byte
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 8 put_byte
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 7 integer
          VAR 1
        FUN 2 1 .
          FUN 2 2 >=
            VAR 1
            INT 0
          FUN 2 1 .
            FUN 2 1 <
              VAR 1
              INT 256
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 4 14 io:with_output
                  VAR 0
                  FUN 0 6 binary
                  FUN 1 9 $put_code
                    VAR 1
                  FUN 2 8 put_byte
                    VAR 0
                    VAR 1
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 8 put_byte
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 8 put_byte
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 4 inst
                VAR 1
              FUN 2 1 .
                FUN 1 4 byte
                  VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 0 2 nl
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 2 nl
      FUN 2 1 .
        FUN 0 20 io:check_output_text
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 9 $put_code
              INT 10
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 0 2 nl
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 0 2 nl
            FUN 2 1 .
              FUN 1 2 do
                FUN 1 14 current_output
                  VAR 0
              FUN 2 1 .
                FUN 3 13 s_text_binary
                  FUN 0 6 output
                  FUN 0 4 text
                  VAR 0
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 2 nl
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 2 nl
        VAR 0
      FUN 2 1 .
        FUN 4 14 io:with_output
          VAR 0
          FUN 0 4 text
          FUN 1 9 $put_code
            INT 10
          FUN 1 2 nl
            VAR 0
        FUN 0 2 []
    FUN 0 2 []
CLS 1 4 read
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 4 read
        VAR 0
      FUN 2 1 .
        FUN 0 19 io:check_input_text
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 19 read:read_with_vars
              VAR 0
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 4 read
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 4 read
              VAR 0
            FUN 2 1 .
              FUN 1 2 do
                FUN 1 13 current_input
                  VAR 1
              FUN 2 1 .
                FUN 3 13 s_text_binary
                  FUN 0 5 input
                  FUN 0 4 text
                  VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 4 read
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 4 read
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 4 13 io:with_input
          VAR 0
          FUN 0 4 text
          FUN 2 19 read:read_with_vars
            VAR 1
            VAR 2
          FUN 2 4 read
            VAR 0
            VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 2 9 read_term
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 9 read_term
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 19 io:check_input_text
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 19 term_io:read_term_i
              VAR 0
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 9 read_term
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 9 read_term
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 2 do
                FUN 1 13 current_input
                  VAR 2
              FUN 2 1 .
                FUN 3 13 s_text_binary
                  FUN 0 5 input
                  FUN 0 4 text
                  VAR 2
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 3 9 read_term
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 9 read_term
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 4 13 io:with_input
          VAR 0
          FUN 0 4 text
          FUN 2 19 term_io:read_term_i
            VAR 1
            VAR 2
          FUN 3 9 read_term
            VAR 0
            VAR 1
            VAR 2
        FUN 0 2 []
    FUN 0 2 []
CLS 2 19 term_io:read_term_i
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 19 term_io:read_term_i
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 1
        FUN 2 1 .
          FUN 1 23 term_io:check_read_opts
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 19 read:read_with_vars
                VAR 0
                VAR 2
              FUN 2 1 .
                FUN 2 22 term_io:read_term_opts
                  VAR 1
                  VAR 2
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 19 term_io:read_term_i
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 9 read_term
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 4 inst
                VAR 1
              FUN 2 1 .
                FUN 1 4 list
                  VAR 1
                FUN 2 1 .
                  FUN 1 8 instlist
                    VAR 1
                  FUN 2 1 .
                    FUN 1 16 instlistelements
                      VAR 1
                    FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 23 term_io:check_read_opts
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 23 term_io:check_read_opts
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 23 term_io:check_read_opts
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 0
          FUN 2 1 .
            FUN 1 6 nonvar
              VAR 1
            FUN 2 1 .
              FUN 1 22 term_io:check_read_opt
                VAR 0
              FUN 2 1 .
                FUN 1 23 term_io:check_read_opts
                  VAR 1
                FUN 0 2 []
      FUN 0 2 []
CLS 1 22 term_io:check_read_opt
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 22 term_io:check_read_opt
        FUN 1 14 variable_names
          VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 22 term_io:check_read_opt
          FUN 1 9 variables
            VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 1 22 term_io:check_read_opt
            FUN 1 10 singletons
              VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 1 22 term_io:check_read_opt
              VAR 0
            FUN 2 1 .
              FUN 1 5 throw
                FUN 2 5 error
                  FUN 2 12 domain_error
                    FUN 0 11 read_option
                    VAR 0
                  FUN 0 9 read_term
              FUN 0 2 []
          FUN 0 2 []
CLS 2 22 term_io:read_term_opts
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 22 term_io:read_term_opts
        FUN 0 2 []
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 22 term_io:read_term_opts
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 21 term_io:read_term_opt
            VAR 0
            VAR 2
          FUN 2 1 .
            FUN 2 22 term_io:read_term_opts
              VAR 1
              VAR 2
            FUN 0 2 []
      FUN 0 2 []
CLS 2 21 term_io:read_term_opt
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 21 term_io:read_term_opt
        FUN 1 14 variable_names
          VAR 0
        VAR 1
      FUN 2 1 .
        FUN 3 21 term_io:get_var_names
          VAR 1
          FUN 0 2 []
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 21 term_io:read_term_opt
          FUN 1 9 variables
            VAR 0
          VAR 1
        FUN 2 1 .
          FUN 3 20 term_io:get_var_list
            VAR 1
            FUN 0 2 []
            VAR 0
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 21 term_io:read_term_opt
            FUN 1 10 singletons
              VAR 0
            VAR 1
          FUN 2 1 .
            FUN 3 26 term_io:get_var_singeltons
              VAR 1
              FUN 0 2 []
              VAR 0
            FUN 0 2 []
        FUN 0 2 []
CLS 3 27 term_io:get_var_names$3_1_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 27 term_io:get_var_names$3_1_1
        VAR 0
        VAR 1
        FUN 2 1 =
          FUN 0 1 _
          VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 1
            VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 27 term_io:get_var_names$3_1_1
          VAR 0
          FUN 2 1 .
            VAR 1
            VAR 0
          VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 3 21 term_io:get_var_names
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 21 term_io:get_var_names
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 3 21 term_io:get_var_names
          FUN 2 1 .
            FUN 2 1 -
              VAR 0
              VAR 1
            VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 3 27 term_io:get_var_names$3_1_1
            VAR 3
            VAR 5
            VAR 0
          FUN 2 1 .
            FUN 3 21 term_io:get_var_names
              VAR 2
              VAR 5
              VAR 4
            FUN 0 2 []
      FUN 0 2 []
CLS 3 20 term_io:get_var_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 20 term_io:get_var_list
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 3 20 term_io:get_var_list
          FUN 2 1 .
            FUN 2 1 -
              FUN 2 1 =
                VAR 0
                VAR 1
              VAR 2
            VAR 3
          VAR 4
          VAR 5
        FUN 2 1 .
          FUN 3 20 term_io:get_var_list
            VAR 3
            FUN 2 1 .
              VAR 1
              VAR 4
            VAR 5
          FUN 0 2 []
      FUN 0 2 []
CLS 4 32 term_io:get_var_singeltons$3_1_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 32 term_io:get_var_singeltons$3_1_1
        VAR 0
        VAR 1
        VAR 2
        FUN 2 1 =
          FUN 0 1 _
          VAR 3
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 2
            VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 32 term_io:get_var_singeltons$3_1_1
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 2
                VAR 1
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 4 32 term_io:get_var_singeltons$3_1_1
            VAR 0
            VAR 1
            FUN 2 1 .
              VAR 2
              VAR 1
            VAR 2
          FUN 0 2 []
        FUN 0 2 []
CLS 3 26 term_io:get_var_singeltons
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 26 term_io:get_var_singeltons
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 3 26 term_io:get_var_singeltons
          FUN 2 1 .
            FUN 2 1 -
              VAR 0
              VAR 1
            VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 4 32 term_io:get_var_singeltons$3_1_1
            VAR 1
            VAR 3
            VAR 5
            VAR 0
          FUN 2 1 .
            FUN 3 26 term_io:get_var_singeltons
              VAR 2
              VAR 5
              VAR 4
            FUN 0 2 []
      FUN 0 2 []
CLS 1 15 term_io:write_i
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 15 term_io:write_i
        VAR 0
      FUN 2 1 .
        FUN 2 17 write:write_style
          VAR 0
          FUN 3 5 style
            FUN 0 5 false
            FUN 0 5 false
            FUN 0 4 true
        FUN 0 2 []
    FUN 0 2 []
CLS 1 5 write
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 5 write
        VAR 0
      FUN 2 1 .
        FUN 0 20 io:check_output_text
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 15 term_io:write_i
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 5 write
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 5 write
              VAR 0
            FUN 2 1 .
              FUN 1 2 do
                FUN 1 14 current_output
                  VAR 1
              FUN 2 1 .
                FUN 3 13 s_text_binary
                  FUN 0 6 output
                  FUN 0 4 text
                  VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 5 write
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 5 write
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 4 14 io:with_output
          VAR 0
          FUN 0 4 text
          FUN 1 15 term_io:write_i
            VAR 1
          FUN 2 5 write
            VAR 0
            VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 1 16 term_io:writeq_i
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 16 term_io:writeq_i
        VAR 0
      FUN 2 1 .
        FUN 2 17 write:write_style
          VAR 0
          FUN 3 5 style
            FUN 0 4 true
            FUN 0 5 false
            FUN 0 4 true
        FUN 0 2 []
    FUN 0 2 []
CLS 1 6 writeq
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 6 writeq
        VAR 0
      FUN 2 1 .
        FUN 0 20 io:check_output_text
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 16 term_io:writeq_i
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 6 writeq
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 6 writeq
              VAR 0
            FUN 2 1 .
              FUN 1 2 do
                FUN 1 14 current_output
                  VAR 1
              FUN 2 1 .
                FUN 3 13 s_text_binary
                  FUN 0 6 output
                  FUN 0 4 text
                  VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 6 writeq
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 6 writeq
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 4 14 io:with_output
          VAR 0
          FUN 0 4 text
          FUN 1 16 term_io:writeq_i
            VAR 1
          FUN 2 6 writeq
            VAR 0
            VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 1 25 term_io:write_canonical_i
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 25 term_io:write_canonical_i
        VAR 0
      FUN 2 1 .
        FUN 2 17 write:write_style
          VAR 0
          FUN 3 5 style
            FUN 0 4 true
            FUN 0 4 true
            FUN 0 5 false
        FUN 0 2 []
    FUN 0 2 []
CLS 1 15 write_canonical
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 15 write_canonical
        VAR 0
      FUN 2 1 .
        FUN 0 20 io:check_output_text
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 25 term_io:write_canonical_i
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 15 write_canonical
          VAR 0
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 1 15 write_canonical
              VAR 0
            FUN 2 1 .
              FUN 1 2 do
                FUN 1 14 current_output
                  VAR 1
              FUN 2 1 .
                FUN 3 13 s_text_binary
                  FUN 0 6 output
                  FUN 0 4 text
                  VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 15 write_canonical
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 15 write_canonical
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 4 14 io:with_output
          VAR 0
          FUN 0 4 text
          FUN 1 25 term_io:write_canonical_i
            VAR 1
          FUN 2 15 write_canonical
            VAR 0
            VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 2 10 write_term
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 10 write_term
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 20 io:check_output_text
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 20 term_io:write_term_i
              VAR 0
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 10 write_term
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 10 write_term
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 2 do
                FUN 1 14 current_output
                  VAR 2
              FUN 2 1 .
                FUN 3 13 s_text_binary
                  FUN 0 6 output
                  FUN 0 4 text
                  VAR 2
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 3 10 write_term
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 10 write_term
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 4 14 io:with_output
          VAR 0
          FUN 0 4 text
          FUN 2 20 term_io:write_term_i
            VAR 1
            VAR 2
          FUN 3 10 write_term
            VAR 0
            VAR 1
            VAR 2
        FUN 0 2 []
    FUN 0 2 []
CLS 2 20 term_io:write_term_i
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 20 term_io:write_term_i
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 1
        FUN 2 1 .
          FUN 3 18 term_io:write_opts
            VAR 1
            FUN 3 5 style
              FUN 0 5 false
              FUN 0 5 false
              FUN 0 5 false
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 17 write:write_style
                VAR 0
                VAR 2
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 20 term_io:write_term_i
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 10 write_term
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 4 inst
                VAR 1
              FUN 2 1 .
                FUN 1 4 list
                  VAR 1
                FUN 2 1 .
                  FUN 1 8 instlist
                    VAR 1
                  FUN 2 1 .
                    FUN 1 16 instlistelements
                      VAR 1
                    FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 3 18 term_io:write_opts
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 18 term_io:write_opts
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 18 term_io:write_opts
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 0
          FUN 2 1 .
            FUN 1 6 nonvar
              VAR 1
            FUN 2 1 .
              FUN 3 17 term_io:write_opt
                VAR 0
                VAR 2
                VAR 4
              FUN 2 1 .
                FUN 3 18 term_io:write_opts
                  VAR 1
                  VAR 4
                  VAR 3
                FUN 0 2 []
      FUN 0 2 []
CLS 3 17 term_io:write_opt
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 3 17 term_io:write_opt
        FUN 1 6 quoted
          VAR 0
        FUN 3 5 style
          VAR 1
          VAR 2
          VAR 3
        FUN 3 5 style
          VAR 0
          VAR 2
          VAR 3
      FUN 2 1 .
        FUN 1 18 term_io:check_bool
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 3 17 term_io:write_opt
          FUN 1 10 ignore_ops
            VAR 0
          FUN 3 5 style
            VAR 1
            VAR 2
            VAR 3
          FUN 3 5 style
            VAR 1
            VAR 0
            VAR 3
        FUN 2 1 .
          FUN 1 18 term_io:check_bool
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 3 17 term_io:write_opt
            FUN 1 10 numbervars
              VAR 0
            FUN 3 5 style
              VAR 1
              VAR 2
              VAR 3
            FUN 3 5 style
              VAR 1
              VAR 2
              VAR 0
          FUN 2 1 .
            FUN 1 18 term_io:check_bool
              VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 3
            FUN 3 17 term_io:write_opt
              VAR 0
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 1 5 throw
                FUN 2 5 error
                  FUN 2 12 domain_error
                    FUN 0 12 write_option
                    VAR 0
                  FUN 0 10 write_term
              FUN 0 2 []
          FUN 0 2 []
CLS 1 24 term_io:check_bool$1_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 24 term_io:check_bool$1_0_0
        FUN 0 4 true
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 24 term_io:check_bool$1_0_0
          FUN 0 5 false
        FUN 0 2 []
      FUN 0 2 []
CLS 1 18 term_io:check_bool
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 18 term_io:check_bool
        VAR 0
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 1 24 term_io:check_bool$1_0_0
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 1 16 current_op$3_0_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 16 current_op$3_0_2
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 16 current_op$3_0_2
          VAR 0
        FUN 2 1 .
          FUN 1 4 atom
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 1 16 current_op$3_0_3
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 16 current_op$3_0_3
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 16 current_op$3_0_3
          VAR 0
        FUN 2 1 .
          FUN 2 16 term_io:op_class
            VAR 0
            VAR 1
          FUN 0 2 []
      FUN 0 2 []
CLS 1 16 current_op$3_0_4
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 16 current_op$3_0_4
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 16 current_op$3_0_4
          VAR 0
        FUN 2 1 .
          FUN 1 7 integer
            VAR 0
          FUN 2 1 .
            FUN 2 2 >=
              VAR 0
              INT 0
            FUN 2 1 .
              FUN 2 2 =<
                VAR 0
                INT 1200
              FUN 0 2 []
      FUN 0 2 []
CLS 3 10 current_op
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 3 10 current_op
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 16 current_op$3_0_4
          VAR 0
        FUN 2 1 .
          FUN 1 16 current_op$3_0_3
            VAR 1
          FUN 2 1 .
            FUN 1 16 current_op$3_0_2
              VAR 2
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 6 clause
                  FUN 3 9 $operator
                    VAR 2
                    VAR 1
                    VAR 0
                  VAR 3
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 10 current_op
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 3 10 current_op
              VAR 0
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 1 7 integer
                VAR 0
              FUN 2 1 .
                FUN 1 7 op_prec
                  VAR 0
                FUN 2 1 .
                  FUN 1 4 atom
                    VAR 1
                  FUN 2 1 .
                    FUN 1 7 op_type
                      VAR 1
                    FUN 2 1 .
                      FUN 1 4 atom
                        VAR 2
                      FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 3 2 op
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 3 2 op
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 7 integer
          VAR 0
        FUN 2 1 .
          FUN 2 2 >=
            VAR 0
            INT 0
          FUN 2 1 .
            FUN 2 2 =<
              VAR 0
              INT 1200
            FUN 2 1 .
              FUN 1 6 nonvar
                VAR 1
              FUN 2 1 .
                FUN 2 16 term_io:op_class
                  VAR 1
                  VAR 3
                FUN 2 1 .
                  FUN 1 6 nonvar
                    VAR 2
                  FUN 2 1 .
                    FUN 3 17 term_io:check_ops
                      VAR 2
                      VAR 1
                      VAR 4
                    FUN 2 1 .
                      FUN 0 1 !
                      FUN 2 1 .
                        FUN 3 21 term_io:modify_oplist
                          VAR 4
                          VAR 1
                          VAR 0
                        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 2 op
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 3 2 op
              VAR 0
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 4 inst
                  VAR 1
                FUN 2 1 .
                  FUN 1 4 inst
                    VAR 2
                  FUN 2 1 .
                    FUN 1 7 integer
                      VAR 0
                    FUN 2 1 .
                      FUN 1 4 atom
                        VAR 1
                      FUN 2 1 .
                        FUN 1 7 op_prec
                          VAR 0
                        FUN 2 1 .
                          FUN 1 7 op_type
                            VAR 1
                          FUN 2 1 .
                            FUN 1 4 list
                              VAR 2
                            FUN 2 1 .
                              FUN 1 8 instlist
                                VAR 2
                              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 3 12 term_io:op_i
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 12 term_io:op_i
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 21 term_io:modify_oplist
          VAR 2
          VAR 1
          VAR 0
        FUN 0 2 []
    FUN 0 2 []
CLS 3 21 term_io:modify_oplist
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 21 term_io:modify_oplist
        FUN 0 2 []
        VAR 0
        VAR 1
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 3 21 term_io:modify_oplist
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 3 17 term_io:modify_op
            VAR 0
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 3 21 term_io:modify_oplist
              VAR 1
              VAR 2
              VAR 3
            FUN 0 2 []
      FUN 0 2 []
CLS 3 17 term_io:modify_op
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 17 term_io:modify_op
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 17 term_io:remove_op
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 3 14 term_io:add_op
            VAR 2
            VAR 1
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 3 14 term_io:add_op
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 14 term_io:add_op
        INT 0
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 14 term_io:add_op
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 7 assertz
            FUN 3 9 $operator
              VAR 2
              VAR 1
              VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 2 17 term_io:remove_op
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 2 17 term_io:remove_op
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 16 term_io:op_class
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 16 term_io:op_class
            VAR 3
            VAR 2
          FUN 2 1 .
            FUN 1 7 retract
              FUN 3 9 $operator
                VAR 0
                VAR 3
                VAR 4
            FUN 2 1 .
              FUN 0 1 !
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 17 term_io:remove_op
          VAR 0
          VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 3 17 term_io:check_ops
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 17 term_io:check_ops
        FUN 0 2 []
        VAR 0
        FUN 0 2 []
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 17 term_io:check_ops
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 20 term_io:check_oplist
              FUN 2 1 .
                VAR 0
                VAR 1
              VAR 2
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 17 term_io:check_ops
            VAR 0
            VAR 1
            FUN 2 1 .
              VAR 0
              FUN 0 2 []
          FUN 2 1 .
            FUN 1 4 atom
              VAR 0
            FUN 2 1 .
              FUN 2 16 term_io:check_op
                VAR 0
                VAR 1
              FUN 0 2 []
        FUN 0 2 []
CLS 2 20 term_io:check_oplist
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 20 term_io:check_oplist
        FUN 0 2 []
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 20 term_io:check_oplist
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 1
          FUN 2 1 .
            FUN 2 16 term_io:check_op
              VAR 0
              VAR 2
            FUN 2 1 .
              FUN 2 20 term_io:check_oplist
                VAR 1
                VAR 2
              FUN 0 2 []
      FUN 0 2 []
CLS 2 16 term_io:check_op
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 16 term_io:check_op
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 4 atom
          VAR 0
        FUN 2 1 .
          FUN 2 2 \=
            VAR 0
            FUN 0 1 ,
          FUN 2 1 .
            FUN 1 2 \+
              FUN 2 19 term_io:op_conflict
                VAR 0
                VAR 1
            FUN 2 1 .
              FUN 0 1 !
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 16 term_io:check_op
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 3 2 op
              VAR 2
              VAR 1
              VAR 0
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 4 atom
                  VAR 0
                FUN 2 1 .
                  FUN 1 9 op_modify
                    VAR 0
                  FUN 2 1 .
                    FUN 2 9 op_create
                      VAR 0
                      VAR 1
                    FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 19 term_io:op_conflict
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 2 19 term_io:op_conflict
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 16 term_io:op_class
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 6 clause
            FUN 3 9 $operator
              VAR 0
              VAR 3
              VAR 4
            VAR 5
          FUN 2 1 .
            FUN 2 16 term_io:op_class
              VAR 3
              VAR 6
            FUN 2 1 .
              FUN 2 22 term_io:class_conflict
                VAR 2
                VAR 6
              FUN 2 1 .
                FUN 0 1 !
                FUN 0 2 []
    FUN 0 2 []
CLS 2 22 term_io:class_conflict
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 22 term_io:class_conflict
        FUN 0 5 infix
        FUN 0 7 postfix
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 2 22 term_io:class_conflict
          FUN 0 7 postfix
          FUN 0 5 infix
        FUN 0 2 []
      FUN 0 2 []
CLS 2 16 term_io:op_class
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 16 term_io:op_class
        FUN 0 2 fx
        FUN 0 6 prefix
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 2 16 term_io:op_class
          FUN 0 2 fy
          FUN 0 6 prefix
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 0
          FUN 2 16 term_io:op_class
            FUN 0 2 xf
            FUN 0 7 postfix
          FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 0
            FUN 2 16 term_io:op_class
              FUN 0 2 yf
              FUN 0 7 postfix
            FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 0
              FUN 2 16 term_io:op_class
                FUN 0 3 xfx
                FUN 0 5 infix
              FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 0
                FUN 2 16 term_io:op_class
                  FUN 0 3 yfx
                  FUN 0 5 infix
                FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 0
                  FUN 2 16 term_io:op_class
                    FUN 0 3 xfy
                    FUN 0 5 infix
                  FUN 0 2 []
                FUN 0 2 []
CLS 0 16 term_io:init_ops
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 0 16 term_io:init_ops
      FUN 2 1 .
        FUN 3 12 term_io:op_i
          INT 1200
          FUN 0 3 xfx
          FUN 2 1 .
            FUN 0 2 :-
            FUN 2 1 .
              FUN 0 3 -->
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 12 term_io:op_i
            INT 1200
            FUN 0 2 fx
            FUN 2 1 .
              FUN 0 2 :-
              FUN 2 1 .
                FUN 0 2 ?-
                FUN 0 2 []
          FUN 2 1 .
            FUN 3 12 term_io:op_i
              INT 1100
              FUN 0 3 xfy
              FUN 2 1 .
                FUN 0 1 ;
                FUN 0 2 []
            FUN 2 1 .
              FUN 3 12 term_io:op_i
                INT 1050
                FUN 0 3 xfy
                FUN 2 1 .
                  FUN 0 2 ->
                  FUN 0 2 []
              FUN 2 1 .
                FUN 3 12 term_io:op_i
                  INT 1000
                  FUN 0 3 xfy
                  FUN 2 1 .
                    FUN 0 1 ,
                    FUN 0 2 []
                FUN 2 1 .
                  FUN 3 12 term_io:op_i
                    INT 900
                    FUN 0 2 fy
                    FUN 2 1 .
                      FUN 0 2 \+
                      FUN 0 2 []
                  FUN 2 1 .
                    FUN 3 12 term_io:op_i
                      INT 700
                      FUN 0 3 xfx
                      FUN 2 1 .
                        FUN 0 1 =
                        FUN 2 1 .
                          FUN 0 2 \=
                          FUN 2 1 .
                            FUN 0 2 is
                            FUN 2 1 .
                              FUN 0 3 =..
                              FUN 2 1 .
                                FUN 0 2 ==
                                FUN 2 1 .
                                  FUN 0 3 \==
                                  FUN 2 1 .
                                    FUN 0 2 @<
                                    FUN 2 1 .
                                      FUN 0 2 @>
                                      FUN 2 1 .
                                        FUN 0 3 @=<
                                        FUN 2 1 .
                                          FUN 0 3 @>=
                                          FUN 2 1 .
                                            FUN 0 3 =:=
                                            FUN 2 1 .
                                              FUN 0 3 =\=
                                              FUN 2 1 .
                                                FUN 0 1 <
                                                FUN 2 1 .
                                                  FUN 0 1 >
                                                  FUN 2 1 .
                                                    FUN 0 2 =<
                                                    FUN 2 1 .
                                                      FUN 0 2 >=
                                                      FUN 0 2 []
                    FUN 2 1 .
                      FUN 3 12 term_io:op_i
                        INT 500
                        FUN 0 3 yfx
                        FUN 2 1 .
                          FUN 0 1 +
                          FUN 2 1 .
                            FUN 0 1 -
                            FUN 2 1 .
                              FUN 0 2 /\
                              FUN 2 1 .
                                FUN 0 2 \/
                                FUN 0 2 []
                      FUN 2 1 .
                        FUN 3 12 term_io:op_i
                          INT 400
                          FUN 0 3 yfx
                          FUN 2 1 .
                            FUN 0 1 *
                            FUN 2 1 .
                              FUN 0 1 /
                              FUN 2 1 .
                                FUN 0 2 //
                                FUN 2 1 .
                                  FUN 0 3 mod
                                  FUN 2 1 .
                                    FUN 0 3 rem
                                    FUN 2 1 .
                                      FUN 0 2 <<
                                      FUN 2 1 .
                                        FUN 0 2 >>
                                        FUN 0 2 []
                        FUN 2 1 .
                          FUN 3 12 term_io:op_i
                            INT 200
                            FUN 0 3 xfx
                            FUN 2 1 .
                              FUN 0 2 **
                              FUN 0 2 []
                          FUN 2 1 .
                            FUN 3 12 term_io:op_i
                              INT 200
                              FUN 0 3 xfy
                              FUN 2 1 .
                                FUN 0 1 ^
                                FUN 0 2 []
                            FUN 2 1 .
                              FUN 3 12 term_io:op_i
                                INT 200
                                FUN 0 2 fy
                                FUN 2 1 .
                                  FUN 0 1 -
                                  FUN 2 1 .
                                    FUN 0 1 \
                                    FUN 0 2 []
                              FUN 0 2 []
    FUN 0 2 []
CLS 2 15 char_conversion
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 15 char_conversion
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 15 term_io:is_char
          VAR 0
        FUN 2 1 .
          FUN 1 15 term_io:is_char
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 9 char_code
                VAR 0
                VAR 2
              FUN 2 1 .
                FUN 2 9 char_code
                  VAR 1
                  VAR 3
                FUN 2 1 .
                  FUN 1 24 term_io:remove_char_conv
                    VAR 2
                  FUN 2 1 .
                    FUN 2 21 term_io:add_char_conv
                      VAR 2
                      VAR 3
                    FUN 2 1 .
                      FUN 0 29 term_io:check_char_conv_state
                      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 15 char_conversion
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 15 char_conversion
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 9 character
                  VAR 0
                FUN 2 1 .
                  FUN 1 4 inst
                    VAR 1
                  FUN 2 1 .
                    FUN 1 9 character
                      VAR 1
                    FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 24 term_io:remove_char_conv
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 24 term_io:remove_char_conv
        VAR 0
      FUN 2 1 .
        FUN 1 7 retract
          FUN 2 10 $char_conv
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 24 term_io:remove_char_conv
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 2 21 term_io:add_char_conv
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 21 term_io:add_char_conv
        VAR 0
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 21 term_io:add_char_conv
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 7 asserta
            FUN 2 10 $char_conv
              VAR 0
              VAR 1
          FUN 0 2 []
      FUN 0 2 []
CLS 1 29 current_char_conversion$2_0_4
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 29 current_char_conversion$2_0_4
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 29 current_char_conversion$2_0_4
          VAR 0
        FUN 2 1 .
          FUN 1 15 term_io:is_char
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 1 29 current_char_conversion$2_0_5
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 29 current_char_conversion$2_0_5
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 29 current_char_conversion$2_0_5
          VAR 0
        FUN 2 1 .
          FUN 1 15 term_io:is_char
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 2 23 current_char_conversion
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 23 current_char_conversion
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 29 current_char_conversion$2_0_5
          VAR 0
        FUN 2 1 .
          FUN 1 29 current_char_conversion$2_0_4
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 10 $char_conv
                VAR 2
                VAR 3
              FUN 2 1 .
                FUN 2 9 char_code
                  VAR 0
                  VAR 2
                FUN 2 1 .
                  FUN 2 9 char_code
                    VAR 1
                    VAR 3
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 23 current_char_conversion
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 23 current_char_conversion
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 9 character
                VAR 0
              FUN 2 1 .
                FUN 1 9 character
                  VAR 1
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 0 29 term_io:check_char_conv_state
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 0 29 term_io:check_char_conv_state
      FUN 2 1 .
        FUN 2 19 current_prolog_flag
          FUN 0 15 char_conversion
          FUN 0 2 on
        FUN 2 1 .
          FUN 2 10 $char_conv
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 8 $abolish
                FUN 2 10 $conv_char
                  VAR 2
                  VAR 3
              FUN 2 1 .
                FUN 3 8 $asserts
                  FUN 2 10 $conv_char
                    VAR 4
                    VAR 5
                  FUN 2 1 .
                    FUN 2 20 term_io:do_conv_char
                      VAR 4
                      VAR 5
                    FUN 0 2 []
                  VAR 6
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 0 29 term_io:check_char_conv_state
        FUN 2 1 .
          FUN 1 8 $abolish
            FUN 2 10 $conv_char
              VAR 0
              VAR 1
          FUN 2 1 .
            FUN 3 8 $asserts
              FUN 2 10 $conv_char
                VAR 2
                VAR 2
              FUN 0 2 []
              VAR 3
            FUN 0 2 []
      FUN 0 2 []
CLS 2 20 term_io:do_conv_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 20 term_io:do_conv_char
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 10 $char_conv
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 20 term_io:do_conv_char
          VAR 0
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 0 21 term_io:init_charconv
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 0 21 term_io:init_charconv
      FUN 2 1 .
        FUN 1 7 asserta
          FUN 2 10 $char_conv
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 7 retract
            FUN 2 10 $char_conv
              VAR 2
              VAR 3
          FUN 2 1 .
            FUN 0 29 term_io:check_char_conv_state
            FUN 0 2 []
    FUN 0 2 []
CLS 1 15 term_io:is_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 15 term_io:is_char
        VAR 0
      FUN 2 1 .
        FUN 1 4 atom
          VAR 0
        FUN 2 1 .
          FUN 2 10 atom_codes
            VAR 0
            FUN 2 1 .
              VAR 1
              FUN 0 2 []
          FUN 0 2 []
    FUN 0 2 []
CLS 3 12 lists:append
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 12 lists:append
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 3 12 lists:append
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          FUN 2 1 .
            VAR 0
            VAR 3
        FUN 2 1 .
          FUN 3 12 lists:append
            VAR 1
            VAR 2
            VAR 3
          FUN 0 2 []
      FUN 0 2 []
CLS 2 12 lists:member
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 12 lists:member
        VAR 0
        FUN 2 1 .
          VAR 0
          VAR 1
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 12 lists:member
          VAR 0
          FUN 2 1 .
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 2 12 lists:member
            VAR 0
            VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 2 15 lists:memberchk
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 15 lists:memberchk
        VAR 0
        FUN 2 1 .
          VAR 0
          VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 15 lists:memberchk
          VAR 0
          FUN 2 1 .
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 2 15 lists:memberchk
            VAR 0
            VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 3 12 lists:select
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 12 lists:select
        VAR 0
        FUN 2 1 .
          VAR 0
          VAR 1
        VAR 1
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 3 12 lists:select
          VAR 0
          FUN 2 1 .
            VAR 1
            VAR 2
          FUN 2 1 .
            VAR 1
            VAR 3
        FUN 2 1 .
          FUN 3 12 lists:select
            VAR 0
            VAR 2
            VAR 3
          FUN 0 2 []
      FUN 0 2 []
CLS 2 13 lists:reverse
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 13 lists:reverse
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 3 13 lists:reverse
          VAR 0
          FUN 0 2 []
          VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 3 13 lists:reverse
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 13 lists:reverse
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 3 13 lists:reverse
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 3 13 lists:reverse
            VAR 1
            FUN 2 1 .
              VAR 0
              VAR 2
            VAR 3
          FUN 0 2 []
      FUN 0 2 []
CLS 2 12 lists:prefix
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 12 lists:prefix
        FUN 0 2 []
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 12 lists:prefix
          FUN 2 1 .
            VAR 0
            VAR 1
          FUN 2 1 .
            VAR 0
            VAR 2
        FUN 2 1 .
          FUN 2 12 lists:prefix
            VAR 1
            VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 4 15 lists:count_nth
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 15 lists:count_nth
        VAR 0
        VAR 0
        FUN 2 1 .
          VAR 1
          VAR 2
        VAR 1
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 4 15 lists:count_nth
          VAR 0
          VAR 1
          FUN 2 1 .
            VAR 2
            VAR 3
          VAR 4
        FUN 2 1 .
          FUN 2 2 is
            VAR 5
            FUN 2 1 +
              VAR 0
              INT 1
          FUN 2 1 .
            FUN 4 15 lists:count_nth
              VAR 5
              VAR 1
              VAR 3
              VAR 4
            FUN 0 2 []
      FUN 0 2 []
CLS 3 9 lists:nth
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 9 lists:nth
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 7 integer
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 2 >=
              VAR 0
              INT 1
            FUN 2 1 .
              FUN 4 15 lists:count_nth
                INT 1
                VAR 0
                VAR 1
                VAR 2
              FUN 2 1 .
                FUN 0 1 !
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 9 lists:nth
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 3 var
            VAR 0
          FUN 2 1 .
            FUN 4 15 lists:count_nth
              INT 1
              VAR 0
              VAR 1
              VAR 2
            FUN 0 2 []
      FUN 0 2 []
CLS 3 18 lists:count_length
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 18 lists:count_length
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 18 lists:count_length
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 2 is
            VAR 4
            FUN 2 1 +
              VAR 2
              INT 1
          FUN 2 1 .
            FUN 3 18 lists:count_length
              VAR 1
              VAR 4
              VAR 3
            FUN 0 2 []
      FUN 0 2 []
CLS 2 12 lists:length
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 12 lists:length
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 7 integer
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 2 >=
              VAR 1
              INT 0
            FUN 2 1 .
              FUN 3 18 lists:count_length
                VAR 0
                INT 0
                VAR 1
              FUN 2 1 .
                FUN 0 1 !
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 12 lists:length
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 3 var
            VAR 1
          FUN 2 1 .
            FUN 3 18 lists:count_length
              VAR 0
              INT 0
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 3 20 lists:count_max_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 20 lists:count_max_list
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 3 20 lists:count_max_list
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 1 >
            VAR 0
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 20 lists:count_max_list
                VAR 1
                VAR 0
                VAR 3
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 3 20 lists:count_max_list
            FUN 2 1 .
              VAR 0
              VAR 1
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 3 20 lists:count_max_list
              VAR 1
              VAR 2
              VAR 3
            FUN 0 2 []
        FUN 0 2 []
CLS 2 14 lists:max_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 14 lists:max_list
        FUN 2 1 .
          VAR 0
          VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 20 lists:count_max_list
          VAR 1
          VAR 0
          VAR 2
        FUN 0 2 []
    FUN 0 2 []
CLS 3 20 lists:count_min_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 20 lists:count_min_list
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 3 20 lists:count_min_list
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 1 <
            VAR 0
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 20 lists:count_min_list
                VAR 1
                VAR 0
                VAR 3
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 3 20 lists:count_min_list
            FUN 2 1 .
              VAR 0
              VAR 1
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 3 20 lists:count_min_list
              VAR 1
              VAR 2
              VAR 3
            FUN 0 2 []
        FUN 0 2 []
CLS 2 14 lists:min_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 14 lists:min_list
        FUN 2 1 .
          VAR 0
          VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 20 lists:count_min_list
          VAR 1
          VAR 0
          VAR 2
        FUN 0 2 []
    FUN 0 2 []
CLS 3 20 lists:count_sum_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 20 lists:count_sum_list
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 20 lists:count_sum_list
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 2 is
            VAR 4
            FUN 2 1 +
              VAR 2
              VAR 0
          FUN 2 1 .
            FUN 3 20 lists:count_sum_list
              VAR 1
              VAR 4
              VAR 3
            FUN 0 2 []
      FUN 0 2 []
CLS 2 14 lists:sum_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 14 lists:sum_list
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 3 20 lists:count_sum_list
          VAR 0
          INT 0
          VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 2 10 lists:last
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 10 lists:last
        FUN 2 1 .
          VAR 0
          FUN 0 2 []
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 10 lists:last
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 10 lists:last
            VAR 1
            VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 1 26 lists:list_or_partial_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 26 lists:list_or_partial_list
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 26 lists:list_or_partial_list
          FUN 0 2 []
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 1 26 lists:list_or_partial_list
            FUN 2 1 .
              VAR 0
              VAR 1
          FUN 2 1 .
            FUN 1 26 lists:list_or_partial_list
              VAR 1
            FUN 0 2 []
        FUN 0 2 []
CLS 2 15 misc:statistics
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 15 misc:statistics
        FUN 0 7 runtime
        FUN 2 1 .
          VAR 0
          FUN 2 1 .
            VAR 1
            FUN 0 2 []
      FUN 2 1 .
        FUN 2 8 $runtime
          VAR 0
          VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 3 11 misc:format
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 3 11 misc:format
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 14 current_output
          VAR 3
        FUN 2 1 .
          FUN 1 10 set_output
            VAR 0
          FUN 2 1 .
            FUN 2 26 execution:call_cleanup_det
              FUN 2 11 misc:format
                VAR 1
                VAR 2
              FUN 1 10 set_output
                VAR 3
            FUN 0 2 []
    FUN 0 2 []
CLS 2 11 misc:format
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 11 misc:format
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 4 atom
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 10 atom_codes
              VAR 0
              VAR 2
            FUN 2 1 .
              FUN 2 18 misc:format_string
                VAR 2
                VAR 1
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 11 misc:format
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 18 misc:format_string
            VAR 0
            VAR 1
          FUN 0 2 []
      FUN 0 2 []
CLS 2 18 misc:format_string
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 18 misc:format_string
        FUN 0 2 []
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 18 misc:format_string
          FUN 2 1 .
            INT 126
            VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 18 misc:format_escape
              VAR 0
              VAR 1
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 2 18 misc:format_string
            FUN 2 1 .
              VAR 0
              VAR 1
            VAR 2
          FUN 2 1 .
            FUN 1 8 put_code
              VAR 0
            FUN 2 1 .
              FUN 2 18 misc:format_string
                VAR 1
                VAR 2
              FUN 0 2 []
        FUN 0 2 []
CLS 2 18 misc:format_escape
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 2 18 misc:format_escape
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 5 12 misc:get_num
          VAR 0
          VAR 1
          VAR 2
          FUN 2 1 .
            VAR 3
            VAR 4
          VAR 5
        FUN 2 1 .
          FUN 4 14 misc:print_arg
            VAR 3
            VAR 2
            VAR 5
            VAR 6
          FUN 2 1 .
            FUN 2 18 misc:format_string
              VAR 4
              VAR 6
            FUN 0 2 []
    FUN 0 2 []
CLS 2 20 misc:print_arg$4_1_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 20 misc:print_arg$4_1_1
        VAR 0
        FUN 0 2 []
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 0
            INT 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 20 misc:print_arg$4_1_1
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 2 >=
            VAR 1
            INT 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 0
                VAR 1
              FUN 0 2 []
      FUN 0 2 []
CLS 1 26 misc:print_arg$4_2_0$2_0_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 26 misc:print_arg$4_2_0$2_0_2
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 26 misc:print_arg$4_2_0$2_0_2
          INT 0
        FUN 0 2 []
      FUN 0 2 []
CLS 2 20 misc:print_arg$4_2_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 20 misc:print_arg$4_2_0
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 26 misc:print_arg$4_2_0$2_0_2
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 5 write
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 20 misc:print_arg$4_2_0
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 1 >
            VAR 1
            INT 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 18 misc:n_put_decimal
                VAR 1
                VAR 0
              FUN 0 2 []
      FUN 0 2 []
CLS 2 20 misc:print_arg$4_7_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 20 misc:print_arg$4_7_1
        VAR 0
        FUN 0 2 []
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 0
            INT 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 20 misc:print_arg$4_7_1
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 2 >=
            VAR 1
            INT 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 0
                VAR 1
              FUN 0 2 []
      FUN 0 2 []
CLS 1 21 misc:print_arg$4_10_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 21 misc:print_arg$4_10_1
        VAR 0
      FUN 2 1 .
        FUN 1 6 number
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 5 write
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 21 misc:print_arg$4_10_1
          VAR 0
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 4 14 misc:print_arg
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 14 misc:print_arg
        INT 97
        VAR 0
        FUN 2 1 .
          VAR 1
          VAR 2
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 4 atom
            VAR 1
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              FUN 0 2 []
            FUN 2 1 .
              FUN 1 5 write
                VAR 1
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 14 misc:print_arg
          INT 99
          VAR 0
          FUN 2 1 .
            VAR 1
            VAR 2
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 7 integer
              VAR 1
            FUN 2 1 .
              FUN 2 20 misc:print_arg$4_1_1
                VAR 3
                VAR 0
              FUN 2 1 .
                FUN 2 15 misc:n_put_code
                  VAR 3
                  VAR 1
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 4 14 misc:print_arg
            INT 100
            VAR 0
            FUN 2 1 .
              VAR 1
              VAR 2
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 7 integer
                VAR 1
              FUN 2 1 .
                FUN 2 20 misc:print_arg$4_2_0
                  VAR 1
                  VAR 0
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 3
            FUN 4 14 misc:print_arg
              INT 112
              VAR 0
              FUN 2 1 .
                VAR 1
                VAR 2
              VAR 2
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 1 =
                  VAR 0
                  FUN 0 2 []
                FUN 2 1 .
                  FUN 1 5 write
                    VAR 1
                  FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 3
              FUN 4 14 misc:print_arg
                INT 113
                VAR 0
                FUN 2 1 .
                  VAR 1
                  VAR 2
                VAR 2
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 1 =
                    VAR 0
                    FUN 0 2 []
                  FUN 2 1 .
                    FUN 1 6 writeq
                      VAR 1
                    FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 3
                FUN 4 14 misc:print_arg
                  INT 119
                  VAR 0
                  FUN 2 1 .
                    VAR 1
                    VAR 2
                  VAR 2
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 2 1 =
                      VAR 0
                      FUN 0 2 []
                    FUN 2 1 .
                      FUN 1 5 write
                        VAR 1
                      FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 2
                  FUN 4 14 misc:print_arg
                    INT 126
                    VAR 0
                    VAR 1
                    VAR 1
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 2 1 =
                        VAR 0
                        FUN 0 2 []
                      FUN 2 1 .
                        FUN 1 8 put_code
                          INT 126
                        FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 3
                    FUN 4 14 misc:print_arg
                      INT 110
                      VAR 0
                      VAR 1
                      VAR 1
                    FUN 2 1 .
                      FUN 0 1 !
                      FUN 2 1 .
                        FUN 2 20 misc:print_arg$4_7_1
                          VAR 2
                          VAR 0
                        FUN 2 1 .
                          FUN 2 15 misc:n_put_code
                            VAR 2
                            INT 10
                          FUN 0 2 []
                  FUN 2 1 .
                    FUN 3 6 clause
                      INT 2
                      FUN 4 14 misc:print_arg
                        INT 78
                        VAR 0
                        VAR 1
                        VAR 1
                      FUN 2 1 .
                        FUN 0 1 !
                        FUN 2 1 .
                          FUN 1 8 put_code
                            INT 10
                          FUN 0 2 []
                    FUN 2 1 .
                      FUN 3 6 clause
                        INT 3
                        FUN 4 14 misc:print_arg
                          INT 64
                          VAR 0
                          FUN 2 1 .
                            VAR 1
                            VAR 2
                          VAR 2
                        FUN 2 1 .
                          FUN 0 1 !
                          FUN 2 1 .
                            FUN 2 1 =
                              VAR 0
                              FUN 0 2 []
                            FUN 2 1 .
                              FUN 1 4 call
                                VAR 1
                              FUN 0 2 []
                      FUN 2 1 .
                        FUN 3 6 clause
                          INT 3
                          FUN 4 14 misc:print_arg
                            VAR 0
                            VAR 1
                            VAR 2
                            VAR 2
                          FUN 2 1 .
                            FUN 1 8 put_code
                              INT 126
                            FUN 2 1 .
                              FUN 1 21 misc:print_arg$4_10_1
                                VAR 1
                              FUN 2 1 .
                                FUN 1 8 put_code
                                  VAR 0
                                FUN 0 2 []
                        FUN 0 2 []
CLS 3 24 misc:n_put_decimal$2_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 3 24 misc:n_put_decimal$2_0_0
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 2 =<
          VAR 2
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 2 is
              VAR 3
              FUN 2 1 -
                VAR 1
                VAR 2
            FUN 2 1 .
              FUN 1 8 put_code
                INT 48
              FUN 2 1 .
                FUN 1 8 put_code
                  INT 46
                FUN 2 1 .
                  FUN 2 15 misc:n_put_code
                    VAR 3
                    INT 48
                  FUN 2 1 .
                    FUN 1 15 misc:put_string
                      VAR 0
                    FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 24 misc:n_put_decimal$2_0_0
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 12 lists:length
            VAR 3
            VAR 1
          FUN 2 1 .
            FUN 3 12 lists:append
              VAR 4
              VAR 3
              VAR 0
            FUN 2 1 .
              FUN 1 15 misc:put_string
                VAR 4
              FUN 2 1 .
                FUN 1 8 put_code
                  INT 46
                FUN 2 1 .
                  FUN 1 15 misc:put_string
                    VAR 3
                  FUN 0 2 []
      FUN 0 2 []
CLS 2 18 misc:n_put_decimal
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 18 misc:n_put_decimal
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 12 number_codes
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 12 lists:length
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 3 24 misc:n_put_decimal$2_0_0
              VAR 2
              VAR 0
              VAR 3
            FUN 0 2 []
    FUN 0 2 []
CLS 2 15 misc:n_put_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 15 misc:n_put_code
        INT 0
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 15 misc:n_put_code
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 8 put_code
            VAR 1
          FUN 2 1 .
            FUN 2 2 is
              VAR 2
              FUN 2 1 -
                VAR 0
                INT 1
            FUN 2 1 .
              FUN 2 15 misc:n_put_code
                VAR 2
                VAR 1
              FUN 0 2 []
      FUN 0 2 []
CLS 1 15 misc:put_string
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 15 misc:put_string
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 15 misc:put_string
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 8 put_code
            VAR 0
          FUN 2 1 .
            FUN 1 15 misc:put_string
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 5 12 misc:get_num
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 5 12 misc:get_num
        FUN 2 1 .
          FUN 0 1 *
          VAR 0
        FUN 2 1 .
          VAR 1
          VAR 2
        VAR 1
        VAR 0
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 7 integer
            VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 5 12 misc:get_num
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 2
        FUN 2 1 .
          FUN 1 13 misc:is_digit
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 5 19 misc:get_num_digits
                VAR 0
                VAR 1
                INT 0
                VAR 3
                VAR 4
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 5 12 misc:get_num
            VAR 0
            VAR 1
            FUN 0 2 []
            VAR 0
            VAR 1
          FUN 0 2 []
        FUN 0 2 []
CLS 5 19 misc:get_num_digits
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 5 19 misc:get_num_digits
        VAR 0
        FUN 2 1 .
          VAR 1
          VAR 2
        VAR 3
        VAR 4
        VAR 5
      FUN 2 1 .
        FUN 1 13 misc:is_digit
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 2 is
              VAR 6
              FUN 2 1 +
                FUN 2 1 *
                  VAR 3
                  INT 10
                FUN 2 1 -
                  VAR 0
                  INT 48
            FUN 2 1 .
              FUN 5 19 misc:get_num_digits
                VAR 1
                VAR 2
                VAR 6
                VAR 4
                VAR 5
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 5 19 misc:get_num_digits
          VAR 0
          VAR 1
          VAR 2
          VAR 2
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 1 13 misc:is_digit
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 13 misc:is_digit
        VAR 0
      FUN 2 1 .
        FUN 2 2 >=
          VAR 0
          INT 48
        FUN 2 1 .
          FUN 2 2 =<
            VAR 0
            INT 57
          FUN 0 2 []
    FUN 0 2 []
CLS 1 14 rdtok:get_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 14 rdtok:get_code
        VAR 0
      FUN 2 1 .
        FUN 1 9 $get_code
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 14 rdtok:get_code
          INT -1
        FUN 0 2 []
      FUN 0 2 []
CLS 1 15 rdtok:peek_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 15 rdtok:peek_code
        VAR 0
      FUN 2 1 .
        FUN 1 10 $peek_code
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 15 rdtok:peek_code
          INT -1
        FUN 0 2 []
      FUN 0 2 []
CLS 2 15 rdtok:conv_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 15 rdtok:conv_char
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 10 $conv_char
          VAR 0
          VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 1 19 rdtok:get_code_conv
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 19 rdtok:get_code_conv
        VAR 0
      FUN 2 1 .
        FUN 1 14 rdtok:get_code
          VAR 1
        FUN 2 1 .
          FUN 2 15 rdtok:conv_char
            VAR 1
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 1 20 rdtok:peek_code_conv
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 20 rdtok:peek_code_conv
        VAR 0
      FUN 2 1 .
        FUN 1 15 rdtok:peek_code
          VAR 1
        FUN 2 1 .
          FUN 2 15 rdtok:conv_char
            VAR 1
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 2 18 rdtok:number_codes
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 18 rdtok:number_codes
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 13 $number_codes
          VAR 0
          VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 5 23 rdtok:read_tokens$2_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 5 23 rdtok:read_tokens$2_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        FUN 0 11 end_of_file
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 13 io:handle_eof
            FUN 0 3 get
          FUN 2 1 .
            FUN 2 1 =
              VAR 3
              FUN 2 1 .
                FUN 1 4 atom
                  FUN 0 11 end_of_file
                FUN 0 2 []
            FUN 2 1 .
              FUN 2 1 =
                VAR 2
                FUN 0 2 []
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 5 23 rdtok:read_tokens$2_0_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 5 17 rdtok:read_tokens
            VAR 4
            VAR 1
            VAR 0
            VAR 2
            VAR 3
          FUN 0 2 []
      FUN 0 2 []
CLS 2 17 rdtok:read_tokens
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 2 17 rdtok:read_tokens
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 14 rdtok:get_code
          VAR 2
        FUN 2 1 .
          FUN 5 16 rdtok:read_token
            VAR 2
            VAR 3
            FUN 0 2 []
            VAR 4
            VAR 5
          FUN 2 1 .
            FUN 5 23 rdtok:read_tokens$2_0_0
              VAR 4
              VAR 5
              VAR 1
              VAR 0
              VAR 3
            FUN 0 2 []
    FUN 0 2 []
CLS 5 17 rdtok:read_tokens
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 5 17 rdtok:read_tokens
        FUN 0 11 end_of_file
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 5 throw
          FUN 2 5 error
            FUN 1 12 syntax_error
              FUN 0 25 End of file in expression
            FUN 2 1 /
              FUN 0 4 read
              INT 1
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 5 17 rdtok:read_tokens
          FUN 0 11 end_of_term
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 3
              FUN 0 2 []
            FUN 2 1 .
              FUN 2 1 =
                VAR 2
                VAR 1
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 8
          FUN 5 17 rdtok:read_tokens
            VAR 0
            VAR 1
            VAR 2
            VAR 3
            FUN 2 1 .
              VAR 0
              VAR 4
          FUN 2 1 .
            FUN 5 16 rdtok:read_token
              VAR 1
              VAR 5
              VAR 2
              VAR 6
              VAR 7
            FUN 2 1 .
              FUN 5 17 rdtok:read_tokens
                VAR 5
                VAR 7
                VAR 6
                VAR 3
                VAR 4
              FUN 0 2 []
        FUN 0 2 []
CLS 6 22 rdtok:read_token$5_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 6 22 rdtok:read_token$5_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
        VAR 5
      FUN 2 1 .
        FUN 2 18 rdtok:is_var_token
          VAR 5
          VAR 4
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 21 rdtok:build_var_token
              VAR 4
              VAR 3
              VAR 2
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 6 22 rdtok:read_token$5_0_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 5
        FUN 2 1 .
          FUN 4 17 rdtok:build_token
            VAR 5
            VAR 4
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 1 =
              VAR 2
              VAR 3
            FUN 0 2 []
      FUN 0 2 []
CLS 5 16 rdtok:read_token
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 5 16 rdtok:read_token
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 4 16 rdtok:read_token
          VAR 0
          VAR 5
          VAR 6
          VAR 4
        FUN 2 1 .
          FUN 6 22 rdtok:read_token$5_0_0
            VAR 0
            VAR 1
            VAR 3
            VAR 2
            VAR 5
            VAR 6
          FUN 0 2 []
    FUN 0 2 []
CLS 1 24 rdtok:is_var_token$2_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 24 rdtok:is_var_token$2_0_0
        VAR 0
      FUN 2 1 .
        FUN 2 2 =<
          INT 65
          VAR 0
        FUN 2 1 .
          FUN 2 2 =<
            VAR 0
            INT 90
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 0 4 true
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 24 rdtok:is_var_token$2_0_0
          INT 95
        FUN 0 2 []
      FUN 0 2 []
CLS 2 18 rdtok:is_var_token
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 18 rdtok:is_var_token
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 1 =
          VAR 0
          FUN 0 6 letter
        FUN 2 1 .
          FUN 2 1 =
            VAR 1
            FUN 2 1 .
              VAR 2
              VAR 3
          FUN 2 1 .
            FUN 1 24 rdtok:is_var_token$2_0_0
              VAR 2
            FUN 0 2 []
    FUN 0 2 []
CLS 4 21 rdtok:build_var_token
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 21 rdtok:build_var_token
        VAR 0
        VAR 1
        VAR 2
        FUN 2 3 var
          VAR 3
          VAR 4
      FUN 2 1 .
        FUN 2 10 atom_codes
          VAR 4
          VAR 0
        FUN 2 1 .
          FUN 4 13 rdtok:add_var
            VAR 4
            VAR 3
            VAR 1
            VAR 2
          FUN 0 2 []
    FUN 0 2 []
CLS 4 13 rdtok:add_var
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 13 rdtok:add_var
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 2 \=
          VAR 0
          FUN 0 1 _
        FUN 2 1 .
          FUN 2 12 lists:member
            FUN 2 1 -
              FUN 2 1 =
                VAR 0
                VAR 1
              FUN 0 12 notsingleton
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 3
                VAR 2
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 13 rdtok:add_var
          VAR 0
          VAR 1
          VAR 2
          FUN 2 1 .
            FUN 2 1 -
              FUN 2 1 =
                VAR 0
                VAR 1
              VAR 3
            VAR 2
        FUN 0 2 []
      FUN 0 2 []
CLS 4 17 rdtok:build_token
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 17 rdtok:build_token
        FUN 1 5 quote
          INT 34
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 2
            FUN 1 6 string
              VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 17 rdtok:build_token
          FUN 0 5 digit
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 18 rdtok:number_codes
              VAR 3
              VAR 0
            FUN 2 1 .
              FUN 2 1 =
                VAR 2
                FUN 1 6 number
                  VAR 3
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 4 17 rdtok:build_token
            FUN 0 6 number
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 2
                FUN 1 6 number
                  VAR 0
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 4
            FUN 4 17 rdtok:build_token
              FUN 1 4 verb
                VAR 0
              VAR 1
              VAR 2
              VAR 3
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 1 =
                  VAR 3
                  VAR 0
                FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 2
              FUN 4 17 rdtok:build_token
                FUN 0 5 punct
                FUN 2 1 .
                  INT 40
                  FUN 0 2 []
                VAR 0
                VAR 1
              FUN 2 1 .
                FUN 2 15 rdtok:char_type
                  VAR 0
                  FUN 0 6 layout
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 2 1 =
                      VAR 1
                      FUN 0 2  (
                    FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 3
                FUN 4 17 rdtok:build_token
                  FUN 0 5 punct
                  VAR 0
                  VAR 1
                  VAR 2
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 2 10 atom_codes
                      VAR 2
                      VAR 0
                    FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 4
                  FUN 4 17 rdtok:build_token
                    FUN 1 5 error
                      VAR 0
                    VAR 1
                    VAR 2
                    FUN 2 5 error
                      VAR 0
                      VAR 3
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 2 10 atom_codes
                        VAR 3
                        VAR 1
                      FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 4
                    FUN 4 17 rdtok:build_token
                      VAR 0
                      VAR 1
                      VAR 2
                      FUN 1 4 atom
                        VAR 3
                    FUN 2 1 .
                      FUN 2 10 atom_codes
                        VAR 3
                        VAR 1
                      FUN 0 2 []
                  FUN 0 2 []
CLS 6 22 rdtok:read_token$4_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 6 22 rdtok:read_token$4_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
        FUN 0 3 eof
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 4
            FUN 1 4 verb
              FUN 0 11 end_of_file
          FUN 2 1 .
            FUN 2 1 =
              VAR 3
              FUN 0 2 []
            FUN 2 1 .
              FUN 2 1 =
                VAR 2
                INT -1
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 8
        FUN 6 22 rdtok:read_token$4_0_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 5
        FUN 2 1 .
          FUN 2 1 =
            VAR 1
            INT 46
          FUN 2 1 .
            FUN 1 20 rdtok:peek_code_conv
              VAR 6
            FUN 2 1 .
              FUN 2 15 rdtok:char_type
                VAR 6
                VAR 7
              FUN 2 1 .
                FUN 1 19 rdtok:eot_succ_type
                  VAR 7
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 2 1 =
                      VAR 4
                      FUN 1 4 verb
                        FUN 0 11 end_of_term
                    FUN 2 1 .
                      FUN 2 1 =
                        VAR 3
                        FUN 0 2 []
                      FUN 2 1 .
                        FUN 2 1 =
                          VAR 2
                          INT -1
                        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 7
          FUN 6 22 rdtok:read_token$4_0_0
            VAR 0
            VAR 1
            VAR 2
            VAR 3
            VAR 4
            VAR 5
          FUN 2 1 .
            FUN 1 14 rdtok:get_code
              VAR 6
            FUN 2 1 .
              FUN 6 16 rdtok:read_token
                VAR 5
                VAR 0
                VAR 6
                VAR 3
                VAR 4
                VAR 2
              FUN 0 2 []
        FUN 0 2 []
CLS 4 16 rdtok:read_token
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 4 16 rdtok:read_token
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 15 rdtok:conv_char
          VAR 0
          VAR 4
        FUN 2 1 .
          FUN 2 15 rdtok:char_type
            VAR 4
            VAR 5
          FUN 2 1 .
            FUN 6 22 rdtok:read_token$4_0_0
              VAR 0
              VAR 4
              VAR 3
              VAR 1
              VAR 2
              VAR 5
            FUN 0 2 []
    FUN 0 2 []
CLS 3 22 rdtok:read_token$6_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 22 rdtok:read_token$6_0_0
        VAR 0
        VAR 1
        FUN 0 0 
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 1
            FUN 1 5 quote
              VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 22 rdtok:read_token$6_0_0
          VAR 0
          FUN 1 5 error
            VAR 1
          VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 6 16 rdtok:read_token
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 6 16 rdtok:read_token
        FUN 0 5 quote
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 5 17 rdtok:read_quoted
            VAR 1
            VAR 0
            VAR 2
            VAR 4
            VAR 5
          FUN 2 1 .
            FUN 3 22 rdtok:read_token$6_0_0
              VAR 0
              VAR 3
              VAR 5
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 6 16 rdtok:read_token
          FUN 0 6 layout
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 16 rdtok:read_token
              VAR 1
              VAR 2
              VAR 3
              VAR 4
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 6
          FUN 6 16 rdtok:read_token
            FUN 0 5 digit
            VAR 0
            VAR 1
            VAR 2
            VAR 3
            VAR 4
          FUN 2 1 .
            FUN 2 15 rdtok:conv_char
              VAR 0
              INT 48
            FUN 2 1 .
              FUN 2 15 rdtok:conv_char
                VAR 1
                VAR 5
              FUN 2 1 .
                FUN 4 22 rdtok:read_number_tail
                  VAR 5
                  VAR 1
                  VAR 2
                  VAR 4
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 2 1 =
                      VAR 3
                      FUN 0 6 number
                    FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 9
            FUN 6 16 rdtok:read_token
              VAR 0
              VAR 1
              VAR 2
              VAR 3
              VAR 4
              VAR 5
            FUN 2 1 .
              FUN 2 15 rdtok:conv_char
                VAR 1
                VAR 6
              FUN 2 1 .
                FUN 2 15 rdtok:conv_char
                  VAR 2
                  VAR 7
                FUN 2 1 .
                  FUN 2 18 rdtok:read_comment
                    VAR 6
                    VAR 7
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 1 14 rdtok:get_code
                        VAR 8
                      FUN 2 1 .
                        FUN 4 16 rdtok:read_token
                          VAR 8
                          VAR 3
                          VAR 4
                          VAR 5
                        FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 6
              FUN 6 16 rdtok:read_token
                VAR 0
                VAR 1
                VAR 2
                FUN 2 1 .
                  VAR 3
                  VAR 4
                VAR 0
                VAR 5
              FUN 2 1 .
                FUN 2 15 rdtok:conv_char
                  VAR 1
                  VAR 3
                FUN 2 1 .
                  FUN 4 21 rdtok:read_token_tail
                    VAR 0
                    VAR 2
                    VAR 4
                    VAR 5
                  FUN 0 2 []
            FUN 0 2 []
CLS 4 22 rdtok:read_number_tail
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 22 rdtok:read_number_tail
        INT 39
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 14 rdtok:get_code
            VAR 3
          FUN 2 1 .
            FUN 4 22 rdtok:read_quoted_char
              VAR 3
              VAR 0
              VAR 1
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 22 rdtok:read_number_tail
          INT 98
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 23 rdtok:read_based_number
              INT 2
              VAR 1
              VAR 2
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 4 22 rdtok:read_number_tail
            INT 111
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 23 rdtok:read_based_number
                INT 8
                VAR 1
                VAR 2
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 3
            FUN 4 22 rdtok:read_number_tail
              INT 120
              VAR 0
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 3 23 rdtok:read_based_number
                  INT 16
                  VAR 1
                  VAR 2
                FUN 0 2 []
          FUN 0 2 []
CLS 3 23 rdtok:read_based_number
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 3 23 rdtok:read_based_number
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 20 rdtok:peek_code_conv
          VAR 3
        FUN 2 1 .
          FUN 3 17 rdtok:digit_based
            VAR 0
            VAR 3
            VAR 4
          FUN 2 1 .
            FUN 1 14 rdtok:get_code
              VAR 5
            FUN 2 1 .
              FUN 4 16 rdtok:read_based
                VAR 0
                VAR 4
                VAR 1
                VAR 2
              FUN 0 2 []
    FUN 0 2 []
CLS 2 18 rdtok:read_comment
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 18 rdtok:read_comment
        INT 37
        VAR 0
      FUN 2 1 .
        FUN 1 15 rdtok:skip_line
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 18 rdtok:read_comment
          INT 47
          INT 42
        FUN 2 1 .
          FUN 1 19 rdtok:get_code_conv
            VAR 0
          FUN 2 1 .
            FUN 1 19 rdtok:get_code_conv
              VAR 1
            FUN 2 1 .
              FUN 2 23 rdtok:skip_star_comment
                VAR 0
                VAR 1
              FUN 0 2 []
      FUN 0 2 []
CLS 1 15 rdtok:skip_line
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 15 rdtok:skip_line
        INT 10
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 15 rdtok:skip_line
          INT -1
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 1 15 rdtok:skip_line
            VAR 0
          FUN 2 1 .
            FUN 1 19 rdtok:get_code_conv
              VAR 1
            FUN 2 1 .
              FUN 1 15 rdtok:skip_line
                VAR 1
              FUN 0 2 []
        FUN 0 2 []
CLS 2 23 rdtok:skip_star_comment
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 23 rdtok:skip_star_comment
        INT 42
        INT 47
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 23 rdtok:skip_star_comment
          VAR 0
          INT -1
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 2 23 rdtok:skip_star_comment
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 1 19 rdtok:get_code_conv
              VAR 2
            FUN 2 1 .
              FUN 2 23 rdtok:skip_star_comment
                VAR 1
                VAR 2
              FUN 0 2 []
        FUN 0 2 []
CLS 4 21 rdtok:read_token_tail
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 4 21 rdtok:read_token_tail
        VAR 0
        VAR 1
        FUN 2 1 .
          VAR 2
          VAR 3
        VAR 4
      FUN 2 1 .
        FUN 2 15 rdtok:conv_char
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 15 rdtok:char_type
            VAR 2
            VAR 5
          FUN 2 1 .
            FUN 2 15 rdtok:continued
              VAR 0
              VAR 5
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 1 14 rdtok:get_code
                  VAR 6
                FUN 2 1 .
                  FUN 4 21 rdtok:read_token_tail
                    VAR 0
                    VAR 6
                    VAR 3
                    VAR 4
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 21 rdtok:read_token_tail
          FUN 0 5 digit
          VAR 0
          FUN 2 1 .
            INT 46
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 15 rdtok:conv_char
            VAR 0
            INT 46
          FUN 2 1 .
            FUN 1 20 rdtok:peek_code_conv
              VAR 3
            FUN 2 1 .
              FUN 2 15 rdtok:char_type
                VAR 3
                FUN 0 5 digit
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 21 rdtok:read_float_tail
                    VAR 1
                    VAR 2
                  FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 4 21 rdtok:read_token_tail
            VAR 0
            VAR 1
            FUN 0 2 []
            VAR 1
          FUN 0 2 []
        FUN 0 2 []
CLS 5 29 rdtok:read_quoted$5_0_0$6_1_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 5 29 rdtok:read_quoted$5_0_0$6_1_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        FUN 0 17 newline in quoted
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 3
            FUN 0 2 []
          FUN 2 1 .
            FUN 2 1 =
              VAR 2
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 5 29 rdtok:read_quoted$5_0_0$6_1_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 5 17 rdtok:read_quoted
            VAR 1
            VAR 0
            VAR 3
            VAR 2
            VAR 5
          FUN 0 2 []
      FUN 0 2 []
CLS 6 23 rdtok:read_quoted$5_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 6 23 rdtok:read_quoted$5_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
        INT -1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 4
            FUN 0 2 []
          FUN 2 1 .
            FUN 2 1 =
              VAR 3
              VAR 2
            FUN 2 1 .
              FUN 2 1 =
                VAR 1
                FUN 0 0 
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 6 23 rdtok:read_quoted$5_0_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 5
        FUN 2 1 .
          FUN 1 4 atom
            VAR 5
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 1
                VAR 5
              FUN 2 1 .
                FUN 5 29 rdtok:read_quoted$5_0_0$6_1_0
                  VAR 0
                  VAR 2
                  VAR 3
                  VAR 4
                  VAR 1
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 6
          FUN 6 23 rdtok:read_quoted$5_0_0
            VAR 0
            VAR 1
            VAR 2
            VAR 3
            FUN 2 1 .
              VAR 4
              VAR 5
            VAR 4
          FUN 2 1 .
            FUN 5 17 rdtok:read_quoted
              VAR 2
              VAR 0
              VAR 5
              VAR 3
              VAR 1
            FUN 0 2 []
        FUN 0 2 []
CLS 5 17 rdtok:read_quoted
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 5 17 rdtok:read_quoted
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 4 22 rdtok:read_quoted_char
          VAR 0
          VAR 1
          VAR 5
          VAR 6
        FUN 2 1 .
          FUN 6 23 rdtok:read_quoted$5_0_0
            VAR 1
            VAR 4
            VAR 6
            VAR 3
            VAR 2
            VAR 5
          FUN 0 2 []
    FUN 0 2 []
CLS 4 28 rdtok:read_quoted_char$4_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 28 rdtok:read_quoted_char$4_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 1 14 rdtok:get_code
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 4 28 rdtok:read_quoted_char$4_0_0
          VAR 0
          INT -1
          VAR 1
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 4 28 rdtok:read_quoted_char$4_2_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 28 rdtok:read_quoted_char$4_2_0
        VAR 0
        VAR 1
        VAR 2
        INT 10
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 14 rdtok:get_code
            VAR 3
          FUN 2 1 .
            FUN 4 22 rdtok:read_quoted_char
              VAR 3
              VAR 2
              VAR 1
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 28 rdtok:read_quoted_char$4_2_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 3 17 rdtok:read_escape
            VAR 3
            VAR 1
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 4 22 rdtok:read_quoted_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 22 rdtok:read_quoted_char
        VAR 0
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 14 rdtok:get_code
            VAR 3
          FUN 2 1 .
            FUN 4 28 rdtok:read_quoted_char$4_0_0
              VAR 2
              VAR 1
              VAR 0
              VAR 3
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 22 rdtok:read_quoted_char
          INT 10
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 14 rdtok:get_code
              VAR 2
            FUN 2 1 .
              FUN 2 1 =
                VAR 1
                FUN 0 17 newline in quoted
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 4 22 rdtok:read_quoted_char
            INT 92
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 14 rdtok:get_code
                VAR 3
              FUN 2 1 .
                FUN 4 28 rdtok:read_quoted_char$4_2_0
                  VAR 2
                  VAR 1
                  VAR 0
                  VAR 3
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 3
            FUN 4 22 rdtok:read_quoted_char
              VAR 0
              VAR 1
              VAR 0
              VAR 2
            FUN 2 1 .
              FUN 1 14 rdtok:get_code
                VAR 2
              FUN 0 2 []
          FUN 0 2 []
CLS 3 17 rdtok:read_escape
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 3 17 rdtok:read_escape
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 17 rdtok:digit_based
          INT 8
          VAR 0
          VAR 3
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 16 rdtok:read_based
              INT 8
              VAR 3
              VAR 4
              VAR 5
            FUN 2 1 .
              FUN 4 21 rdtok:complete_escape
                VAR 5
                VAR 4
                VAR 1
                VAR 2
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 3 17 rdtok:read_escape
          INT 120
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 14 rdtok:get_code
            VAR 2
          FUN 2 1 .
            FUN 3 17 rdtok:digit_based
              INT 16
              VAR 2
              VAR 3
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 4 16 rdtok:read_based
                  INT 16
                  VAR 3
                  VAR 4
                  VAR 5
                FUN 2 1 .
                  FUN 4 21 rdtok:complete_escape
                    VAR 5
                    VAR 4
                    VAR 0
                    VAR 1
                  FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 3 17 rdtok:read_escape
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 2 18 rdtok:escape_equiv
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 1 14 rdtok:get_code
                  VAR 2
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 3 17 rdtok:read_escape
              VAR 0
              FUN 0 14 unknown escape
              VAR 0
            FUN 0 2 []
          FUN 0 2 []
CLS 4 21 rdtok:complete_escape
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 21 rdtok:complete_escape
        INT 92
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 1 <
          VAR 0
          INT 256
        FUN 2 1 .
          FUN 2 2 >=
            VAR 0
            INT 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 1
                VAR 0
              FUN 2 1 .
                FUN 1 14 rdtok:get_code
                  VAR 2
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 21 rdtok:complete_escape
          INT 92
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 1
              FUN 0 12 code too big
            FUN 2 1 .
              FUN 1 14 rdtok:get_code
                VAR 2
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 4 21 rdtok:complete_escape
            VAR 0
            VAR 1
            FUN 0 17 missing backslash
            VAR 0
          FUN 0 2 []
        FUN 0 2 []
CLS 4 28 rdtok:read_based$4_0_0$5_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 28 rdtok:read_based$4_0_0$5_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 3
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 16 rdtok:read_based
              VAR 2
              VAR 3
              VAR 1
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 28 rdtok:read_based$4_0_0$5_0_0
          VAR 0
          INT -1
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 4 16 rdtok:read_based
            VAR 1
            INT 0
            VAR 3
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 5 22 rdtok:read_based$4_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 8
      FUN 5 22 rdtok:read_based$4_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 3 17 rdtok:digit_based
          VAR 4
          VAR 3
          VAR 5
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 5 catch
              FUN 2 2 is
                VAR 6
                FUN 2 1 +
                  FUN 2 1 *
                    VAR 2
                    VAR 4
                  VAR 5
              FUN 2 5 error
                FUN 1 16 evaluation_error
                  FUN 0 12 int_overflow
                VAR 7
              FUN 0 4 true
            FUN 2 1 .
              FUN 4 28 rdtok:read_based$4_0_0$5_0_0
                VAR 0
                VAR 1
                VAR 4
                VAR 6
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 5 22 rdtok:read_based$4_0_0
          VAR 0
          VAR 1
          VAR 1
          VAR 0
          VAR 2
        FUN 0 2 []
      FUN 0 2 []
CLS 4 16 rdtok:read_based
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 16 rdtok:read_based
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 14 rdtok:get_code
          VAR 4
        FUN 2 1 .
          FUN 5 22 rdtok:read_based$4_0_0
            VAR 3
            VAR 2
            VAR 1
            VAR 4
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 3 17 rdtok:digit_based
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 17 rdtok:digit_based
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 2 >=
          VAR 1
          INT 48
        FUN 2 1 .
          FUN 2 1 <
            VAR 1
            FUN 2 1 +
              INT 48
              VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 2 is
                VAR 2
                FUN 2 1 -
                  VAR 1
                  INT 48
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 17 rdtok:digit_based
          INT 16
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 2 >=
            VAR 0
            INT 97
          FUN 2 1 .
            FUN 2 2 =<
              VAR 0
              INT 102
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 2 is
                  VAR 1
                  FUN 2 1 +
                    FUN 2 1 -
                      VAR 0
                      INT 97
                    INT 10
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 17 rdtok:digit_based
            INT 16
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 2 >=
              VAR 0
              INT 65
            FUN 2 1 .
              FUN 2 2 =<
                VAR 0
                INT 70
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 2 is
                    VAR 1
                    FUN 2 1 +
                      FUN 2 1 -
                        VAR 0
                        INT 65
                      INT 10
                  FUN 0 2 []
        FUN 0 2 []
CLS 1 33 rdtok:read_float_tail$2_0_0$4_1_6
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 33 rdtok:read_float_tail$2_0_0$4_1_6
        INT 101
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 33 rdtok:read_float_tail$2_0_0$4_1_6
          INT 69
        FUN 0 2 []
      FUN 0 2 []
CLS 4 27 rdtok:read_float_tail$2_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 27 rdtok:read_float_tail$2_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 15 rdtok:char_type
          VAR 3
          FUN 0 5 digit
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 2
              FUN 2 1 .
                VAR 3
                VAR 4
            FUN 2 1 .
              FUN 2 21 rdtok:read_float_tail
                VAR 4
                VAR 1
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 7
        FUN 4 27 rdtok:read_float_tail$2_0_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 1 33 rdtok:read_float_tail$2_0_0$4_1_6
            VAR 3
          FUN 2 1 .
            FUN 1 20 rdtok:peek_code_conv
              VAR 4
            FUN 2 1 .
              FUN 1 19 rdtok:exponent_head
                VAR 4
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 1 =
                    VAR 2
                    FUN 2 1 .
                      VAR 3
                      FUN 2 1 .
                        VAR 4
                        VAR 5
                  FUN 2 1 .
                    FUN 1 14 rdtok:get_code
                      VAR 6
                    FUN 2 1 .
                      FUN 2 19 rdtok:read_exponent
                        VAR 5
                        VAR 1
                      FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 4 27 rdtok:read_float_tail$2_0_0
            VAR 0
            VAR 0
            FUN 0 2 []
            VAR 1
          FUN 0 2 []
        FUN 0 2 []
CLS 2 21 rdtok:read_float_tail
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 21 rdtok:read_float_tail
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 14 rdtok:get_code
          VAR 2
        FUN 2 1 .
          FUN 2 15 rdtok:conv_char
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 4 27 rdtok:read_float_tail$2_0_0
              VAR 2
              VAR 1
              VAR 0
              VAR 3
            FUN 0 2 []
    FUN 0 2 []
CLS 1 19 rdtok:exponent_head
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 19 rdtok:exponent_head
        INT 43
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 19 rdtok:exponent_head
          INT 45
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 1 19 rdtok:exponent_head
            VAR 0
          FUN 2 1 .
            FUN 2 15 rdtok:char_type
              VAR 0
              FUN 0 5 digit
            FUN 0 2 []
        FUN 0 2 []
CLS 4 25 rdtok:read_exponent$2_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 25 rdtok:read_exponent$2_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 15 rdtok:char_type
          VAR 3
          FUN 0 5 digit
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 2
              FUN 2 1 .
                VAR 3
                VAR 4
            FUN 2 1 .
              FUN 2 19 rdtok:read_exponent
                VAR 4
                VAR 1
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 4 25 rdtok:read_exponent$2_0_0
          VAR 0
          VAR 0
          FUN 0 2 []
          VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 2 19 rdtok:read_exponent
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 19 rdtok:read_exponent
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 14 rdtok:get_code
          VAR 2
        FUN 2 1 .
          FUN 2 15 rdtok:conv_char
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 4 25 rdtok:read_exponent$2_0_0
              VAR 2
              VAR 1
              VAR 0
              VAR 3
            FUN 0 2 []
    FUN 0 2 []
CLS 2 18 rdtok:escape_equiv
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 18 rdtok:escape_equiv
        INT 97
        INT 7
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 2 18 rdtok:escape_equiv
          INT 98
          INT 8
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 0
          FUN 2 18 rdtok:escape_equiv
            INT 114
            INT 13
          FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 0
            FUN 2 18 rdtok:escape_equiv
              INT 102
              INT 12
            FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 0
              FUN 2 18 rdtok:escape_equiv
                INT 116
                INT 9
              FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 0
                FUN 2 18 rdtok:escape_equiv
                  INT 110
                  INT 10
                FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 0
                  FUN 2 18 rdtok:escape_equiv
                    INT 118
                    INT 11
                  FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 0
                    FUN 2 18 rdtok:escape_equiv
                      INT 92
                      INT 92
                    FUN 0 2 []
                  FUN 2 1 .
                    FUN 3 6 clause
                      INT 0
                      FUN 2 18 rdtok:escape_equiv
                        INT 39
                        INT 39
                      FUN 0 2 []
                    FUN 2 1 .
                      FUN 3 6 clause
                        INT 0
                        FUN 2 18 rdtok:escape_equiv
                          INT 34
                          INT 34
                        FUN 0 2 []
                      FUN 2 1 .
                        FUN 3 6 clause
                          INT 0
                          FUN 2 18 rdtok:escape_equiv
                            INT 96
                            INT 96
                          FUN 0 2 []
                        FUN 0 2 []
CLS 2 15 rdtok:char_type
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 15 rdtok:char_type
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 16 rdtok:char_type0
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 1
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 15 rdtok:char_type
          VAR 0
          FUN 0 6 symbol
        FUN 0 2 []
      FUN 0 2 []
CLS 1 22 rdtok:char_type0$2_1_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 22 rdtok:char_type0$2_1_0
        VAR 0
      FUN 2 1 .
        FUN 2 2 >=
          VAR 0
          INT 97
        FUN 2 1 .
          FUN 2 2 =<
            VAR 0
            INT 122
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 22 rdtok:char_type0$2_1_0
          VAR 0
        FUN 2 1 .
          FUN 2 2 >=
            VAR 0
            INT 65
          FUN 2 1 .
            FUN 2 2 =<
              VAR 0
              INT 90
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 0
          FUN 1 22 rdtok:char_type0$2_1_0
            INT 95
          FUN 0 2 []
        FUN 0 2 []
CLS 2 16 rdtok:char_type0
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 16 rdtok:char_type0
        VAR 0
        FUN 0 5 digit
      FUN 2 1 .
        FUN 2 2 >=
          VAR 0
          INT 48
        FUN 2 1 .
          FUN 2 2 =<
            VAR 0
            INT 57
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 16 rdtok:char_type0
          VAR 0
          FUN 0 6 letter
        FUN 2 1 .
          FUN 1 22 rdtok:char_type0$2_1_0
            VAR 0
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 0
          FUN 2 16 rdtok:char_type0
            INT -1
            FUN 0 3 eof
          FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 2 16 rdtok:char_type0
              VAR 0
              FUN 0 6 layout
            FUN 2 1 .
              FUN 2 2 =<
                VAR 0
                INT 32
              FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 0
              FUN 2 16 rdtok:char_type0
                INT 37
                FUN 0 7 comment
              FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 1
                FUN 2 16 rdtok:char_type0
                  VAR 0
                  FUN 0 5 punct
                FUN 2 1 .
                  FUN 2 12 lists:member
                    VAR 0
                    FUN 2 1 .
                      INT 40
                      FUN 2 1 .
                        INT 41
                        FUN 2 1 .
                          INT 44
                          FUN 2 1 .
                            INT 124
                            FUN 2 1 .
                              INT 91
                              FUN 2 1 .
                                INT 93
                                FUN 2 1 .
                                  INT 123
                                  FUN 2 1 .
                                    INT 125
                                    FUN 0 2 []
                  FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 1
                  FUN 2 16 rdtok:char_type0
                    VAR 0
                    FUN 0 4 solo
                  FUN 2 1 .
                    FUN 2 12 lists:member
                      VAR 0
                      FUN 2 1 .
                        INT 59
                        FUN 2 1 .
                          INT 33
                          FUN 0 2 []
                    FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 1
                    FUN 2 16 rdtok:char_type0
                      VAR 0
                      FUN 0 5 quote
                    FUN 2 1 .
                      FUN 2 12 lists:member
                        VAR 0
                        FUN 2 1 .
                          INT 34
                          FUN 2 1 .
                            INT 39
                            FUN 0 2 []
                      FUN 0 2 []
                  FUN 0 2 []
CLS 2 15 rdtok:continued
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 15 rdtok:continued
        FUN 0 6 letter
        FUN 0 6 letter
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 2 15 rdtok:continued
          FUN 0 6 letter
          FUN 0 5 digit
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 0
          FUN 2 15 rdtok:continued
            FUN 0 5 digit
            FUN 0 5 digit
          FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 0
            FUN 2 15 rdtok:continued
              FUN 0 6 symbol
              FUN 0 6 symbol
            FUN 0 2 []
          FUN 0 2 []
CLS 1 19 rdtok:eot_succ_type
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 19 rdtok:eot_succ_type
        FUN 0 6 layout
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 1 19 rdtok:eot_succ_type
          FUN 0 7 comment
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 0
          FUN 1 19 rdtok:eot_succ_type
            FUN 0 3 eof
          FUN 0 2 []
        FUN 0 2 []
CLS 2 25 read:read_with_vars$2_0_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 25 read:read_with_vars$2_0_2
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 4 9 read:read
          VAR 1
          INT 1200
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 1 13 read:all_read
            VAR 2
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 25 read:read_with_vars$2_0_2
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 11 read:syntax
            VAR 1
          FUN 0 2 []
      FUN 0 2 []
CLS 2 19 read:read_with_vars
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 19 read:read_with_vars
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 17 rdtok:read_tokens
          VAR 2
          VAR 1
        FUN 2 1 .
          FUN 2 25 read:read_with_vars$2_0_2
            VAR 3
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 0
                VAR 3
              FUN 0 2 []
    FUN 0 2 []
CLS 1 13 read:all_read
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 13 read:all_read
        FUN 0 2 []
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 13 read:all_read
          VAR 0
        FUN 2 1 .
          FUN 2 11 read:syntax
            FUN 2 1 .
              FUN 0 8 operator
              FUN 2 1 .
                FUN 0 8 expected
                FUN 2 1 .
                  FUN 0 5 after
                  FUN 2 1 .
                    FUN 0 10 expression
                    FUN 0 2 []
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 3 11 read:expect
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 11 read:expect
        VAR 0
        FUN 2 1 .
          VAR 0
          VAR 1
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 11 read:expect
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 11 read:syntax
            FUN 2 1 .
              VAR 0
              FUN 2 1 .
                FUN 0 2 or
                FUN 2 1 .
                  FUN 0 8 operator
                  FUN 2 1 .
                    FUN 0 8 expected
                    FUN 0 2 []
            VAR 1
          FUN 0 2 []
      FUN 0 2 []
CLS 3 13 read:prefixop
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 13 read:prefixop
        VAR 0
        VAR 1
        VAR 1
      FUN 2 1 .
        FUN 3 10 current_op
          VAR 1
          FUN 0 2 fy
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 13 read:prefixop
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 10 current_op
            VAR 1
            FUN 0 2 fx
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 2 is
                VAR 2
                FUN 2 1 -
                  VAR 1
                  INT 1
              FUN 0 2 []
      FUN 0 2 []
CLS 3 14 read:postfixop
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 14 read:postfixop
        VAR 0
        VAR 1
        VAR 1
      FUN 2 1 .
        FUN 3 10 current_op
          VAR 1
          FUN 0 2 yf
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 14 read:postfixop
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 10 current_op
            VAR 2
            FUN 0 2 xf
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 2 is
                VAR 1
                FUN 2 1 -
                  VAR 2
                  INT 1
              FUN 0 2 []
      FUN 0 2 []
CLS 4 12 read:infixop
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 12 read:infixop
        VAR 0
        VAR 1
        VAR 2
        VAR 1
      FUN 2 1 .
        FUN 3 10 current_op
          VAR 2
          FUN 0 3 xfx
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 2 is
              VAR 1
              FUN 2 1 -
                VAR 2
                INT 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 12 read:infixop
          VAR 0
          VAR 1
          VAR 2
          VAR 2
        FUN 2 1 .
          FUN 3 10 current_op
            VAR 2
            FUN 0 3 xfy
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 2 is
                VAR 1
                FUN 2 1 -
                  VAR 2
                  INT 1
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 4 12 read:infixop
            VAR 0
            VAR 1
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 3 10 current_op
              VAR 1
              FUN 0 3 yfx
              VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 2 is
                  VAR 2
                  FUN 2 1 -
                    VAR 1
                    INT 1
                FUN 0 2 []
        FUN 0 2 []
CLS 6 12 read:ambigop
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 6 12 read:ambigop
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
        VAR 5
      FUN 2 1 .
        FUN 3 14 read:postfixop
          VAR 0
          VAR 4
          VAR 5
        FUN 2 1 .
          FUN 4 12 read:infixop
            VAR 0
            VAR 1
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
    FUN 0 2 []
CLS 4 9 read:read
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 9 read:read
        FUN 2 1 .
          VAR 0
          VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 5 9 read:read
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 9 read:read
          FUN 0 2 []
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 11 read:syntax
            FUN 2 1 .
              FUN 0 10 expression
              FUN 2 1 .
                FUN 0 8 expected
                FUN 0 2 []
            FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 3 16 read:read$5_12_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 16 read:read$5_12_1
        VAR 0
        VAR 1
        FUN 0 5 codes
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
        FUN 3 16 read:read$5_12_1
          VAR 0
          VAR 1
          FUN 0 5 chars
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 10 atom_codes
              VAR 2
              VAR 0
            FUN 2 1 .
              FUN 2 10 atom_chars
                VAR 2
                VAR 1
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 3 16 read:read$5_12_1
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 2 10 atom_codes
              VAR 1
              VAR 0
            FUN 0 2 []
        FUN 0 2 []
CLS 5 9 read:read
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 5 9 read:read
        FUN 2 3 var
          VAR 0
          VAR 1
        VAR 2
        VAR 3
        VAR 4
        VAR 5
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 5 12 read:exprtl0
            VAR 2
            VAR 0
            VAR 3
            VAR 4
            VAR 5
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 5 9 read:read
          FUN 1 4 atom
            FUN 0 1 -
          FUN 2 1 .
            FUN 1 6 number
              VAR 0
            VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 19 read:check_negative
              VAR 0
              VAR 5
              VAR 1
            FUN 2 1 .
              FUN 5 12 read:exprtl0
                VAR 1
                VAR 5
                VAR 2
                VAR 3
                VAR 4
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 10
          FUN 5 9 read:read
            FUN 1 4 atom
              VAR 0
            FUN 2 1 .
              FUN 0 1 (
              VAR 1
            VAR 2
            VAR 3
            VAR 4
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 4 9 read:read
                VAR 1
                INT 999
                VAR 5
                VAR 6
              FUN 2 1 .
                FUN 3 14 read:read_args
                  VAR 6
                  VAR 7
                  VAR 8
                FUN 2 1 .
                  FUN 2 3 =..
                    VAR 9
                    FUN 2 1 .
                      VAR 0
                      FUN 2 1 .
                        VAR 5
                        VAR 7
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 5 12 read:exprtl0
                        VAR 8
                        VAR 9
                        VAR 2
                        VAR 3
                        VAR 4
                      FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 7
            FUN 5 9 read:read
              FUN 1 4 atom
                VAR 0
              VAR 1
              VAR 2
              VAR 3
              VAR 4
            FUN 2 1 .
              FUN 3 13 read:prefixop
                VAR 0
                VAR 5
                VAR 6
              FUN 2 1 .
                FUN 7 20 read:after_prefix_op
                  VAR 0
                  VAR 5
                  VAR 6
                  VAR 1
                  VAR 2
                  VAR 3
                  VAR 4
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 5
              FUN 5 9 read:read
                FUN 1 4 atom
                  VAR 0
                VAR 1
                VAR 2
                VAR 3
                VAR 4
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 5 12 read:exprtl0
                    VAR 1
                    VAR 0
                    VAR 2
                    VAR 3
                    VAR 4
                  FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 5
                FUN 5 9 read:read
                  FUN 1 6 number
                    VAR 0
                  VAR 1
                  VAR 2
                  VAR 3
                  VAR 4
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 2 19 read:check_positive
                      VAR 0
                      VAR 1
                    FUN 2 1 .
                      FUN 5 12 read:exprtl0
                        VAR 1
                        VAR 0
                        VAR 2
                        VAR 3
                        VAR 4
                      FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 4
                  FUN 5 9 read:read
                    FUN 0 1 [
                    FUN 2 1 .
                      FUN 0 1 ]
                      VAR 0
                    VAR 1
                    VAR 2
                    VAR 3
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 5 12 read:exprtl0
                        VAR 0
                        FUN 0 2 []
                        VAR 1
                        VAR 2
                        VAR 3
                      FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 8
                    FUN 5 9 read:read
                      FUN 0 1 [
                      VAR 0
                      VAR 1
                      VAR 2
                      VAR 3
                    FUN 2 1 .
                      FUN 0 1 !
                      FUN 2 1 .
                        FUN 4 9 read:read
                          VAR 0
                          INT 999
                          VAR 4
                          VAR 5
                        FUN 2 1 .
                          FUN 3 14 read:read_list
                            VAR 5
                            VAR 6
                            VAR 7
                          FUN 2 1 .
                            FUN 0 1 !
                            FUN 2 1 .
                              FUN 5 12 read:exprtl0
                                VAR 7
                                FUN 2 1 .
                                  VAR 4
                                  VAR 6
                                VAR 1
                                VAR 2
                                VAR 3
                              FUN 0 2 []
                  FUN 2 1 .
                    FUN 3 6 clause
                      INT 7
                      FUN 5 9 read:read
                        FUN 0 1 (
                        VAR 0
                        VAR 1
                        VAR 2
                        VAR 3
                      FUN 2 1 .
                        FUN 0 1 !
                        FUN 2 1 .
                          FUN 4 9 read:read
                            VAR 0
                            INT 1200
                            VAR 4
                            VAR 5
                          FUN 2 1 .
                            FUN 3 11 read:expect
                              FUN 0 1 )
                              VAR 5
                              VAR 6
                            FUN 2 1 .
                              FUN 0 1 !
                              FUN 2 1 .
                                FUN 5 12 read:exprtl0
                                  VAR 6
                                  VAR 4
                                  VAR 1
                                  VAR 2
                                  VAR 3
                                FUN 0 2 []
                    FUN 2 1 .
                      FUN 3 6 clause
                        INT 7
                        FUN 5 9 read:read
                          FUN 0 2  (
                          VAR 0
                          VAR 1
                          VAR 2
                          VAR 3
                        FUN 2 1 .
                          FUN 0 1 !
                          FUN 2 1 .
                            FUN 4 9 read:read
                              VAR 0
                              INT 1200
                              VAR 4
                              VAR 5
                            FUN 2 1 .
                              FUN 3 11 read:expect
                                FUN 0 1 )
                                VAR 5
                                VAR 6
                              FUN 2 1 .
                                FUN 0 1 !
                                FUN 2 1 .
                                  FUN 5 12 read:exprtl0
                                    VAR 6
                                    VAR 4
                                    VAR 1
                                    VAR 2
                                    VAR 3
                                  FUN 0 2 []
                      FUN 2 1 .
                        FUN 3 6 clause
                          INT 4
                          FUN 5 9 read:read
                            FUN 0 1 {
                            FUN 2 1 .
                              FUN 0 1 }
                              VAR 0
                            VAR 1
                            VAR 2
                            VAR 3
                          FUN 2 1 .
                            FUN 0 1 !
                            FUN 2 1 .
                              FUN 5 12 read:exprtl0
                                VAR 0
                                FUN 0 2 {}
                                VAR 1
                                VAR 2
                                VAR 3
                              FUN 0 2 []
                        FUN 2 1 .
                          FUN 3 6 clause
                            INT 7
                            FUN 5 9 read:read
                              FUN 0 1 {
                              VAR 0
                              VAR 1
                              VAR 2
                              VAR 3
                            FUN 2 1 .
                              FUN 0 1 !
                              FUN 2 1 .
                                FUN 4 9 read:read
                                  VAR 0
                                  INT 1200
                                  VAR 4
                                  VAR 5
                                FUN 2 1 .
                                  FUN 3 11 read:expect
                                    FUN 0 1 }
                                    VAR 5
                                    VAR 6
                                  FUN 2 1 .
                                    FUN 0 1 !
                                    FUN 2 1 .
                                      FUN 5 12 read:exprtl0
                                        VAR 6
                                        FUN 1 2 {}
                                          VAR 4
                                        VAR 1
                                        VAR 2
                                        VAR 3
                                      FUN 0 2 []
                          FUN 2 1 .
                            FUN 3 6 clause
                              INT 7
                              FUN 5 9 read:read
                                FUN 1 6 string
                                  VAR 0
                                VAR 1
                                VAR 2
                                VAR 3
                                VAR 4
                              FUN 2 1 .
                                FUN 0 1 !
                                FUN 2 1 .
                                  FUN 2 19 current_prolog_flag
                                    FUN 0 13 double_quotes
                                    VAR 5
                                  FUN 2 1 .
                                    FUN 3 16 read:read$5_12_1
                                      VAR 0
                                      VAR 6
                                      VAR 5
                                    FUN 2 1 .
                                      FUN 5 12 read:exprtl0
                                        VAR 1
                                        VAR 6
                                        VAR 2
                                        VAR 3
                                        VAR 4
                                      FUN 0 2 []
                            FUN 2 1 .
                              FUN 3 6 clause
                                INT 6
                                FUN 5 9 read:read
                                  FUN 2 5 error
                                    VAR 0
                                    VAR 1
                                  VAR 2
                                  VAR 3
                                  VAR 4
                                  VAR 5
                                FUN 2 1 .
                                  FUN 0 1 !
                                  FUN 2 1 .
                                    FUN 2 11 read:syntax
                                      FUN 2 1 .
                                        VAR 0
                                        FUN 2 1 .
                                          FUN 0 2 in
                                          FUN 2 1 .
                                            VAR 1
                                            FUN 0 2 []
                                      VAR 2
                                    FUN 0 2 []
                              FUN 2 1 .
                                FUN 3 6 clause
                                  INT 5
                                  FUN 5 9 read:read
                                    VAR 0
                                    VAR 1
                                    VAR 2
                                    VAR 3
                                    VAR 4
                                  FUN 2 1 .
                                    FUN 2 11 read:syntax
                                      FUN 2 1 .
                                        VAR 0
                                        FUN 2 1 .
                                          FUN 0 6 cannot
                                          FUN 2 1 .
                                            FUN 0 5 start
                                            FUN 2 1 .
                                              FUN 0 2 an
                                              FUN 2 1 .
                                                FUN 0 10 expression
                                                FUN 0 2 []
                                      VAR 1
                                    FUN 0 2 []
                                FUN 0 2 []
CLS 3 14 read:read_args
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 3 14 read:read_args
        FUN 2 1 .
          FUN 0 1 ,
          VAR 0
        FUN 2 1 .
          VAR 1
          VAR 2
        VAR 3
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 4 9 read:read
            VAR 0
            INT 999
            VAR 1
            VAR 4
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 14 read:read_args
                VAR 4
                VAR 2
                VAR 3
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 3 14 read:read_args
          FUN 2 1 .
            FUN 0 1 )
            VAR 0
          FUN 0 2 []
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 3 14 read:read_args
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 2 11 read:syntax
              FUN 2 1 .
                FUN 0 6 , or )
                FUN 2 1 .
                  FUN 0 8 expected
                  FUN 2 1 .
                    FUN 0 2 in
                    FUN 2 1 .
                      FUN 0 9 arguments
                      FUN 0 2 []
              VAR 0
            FUN 0 2 []
        FUN 0 2 []
CLS 3 14 read:read_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 3 14 read:read_list
        FUN 2 1 .
          FUN 0 1 ,
          VAR 0
        FUN 2 1 .
          VAR 1
          VAR 2
        VAR 3
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 4 9 read:read
            VAR 0
            INT 999
            VAR 1
            VAR 4
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 14 read:read_list
                VAR 4
                VAR 2
                VAR 3
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 3 14 read:read_list
          FUN 2 1 .
            FUN 0 1 |
            VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 9 read:read
              VAR 0
              INT 999
              VAR 1
              VAR 3
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 3 11 read:expect
                  FUN 0 1 ]
                  VAR 3
                  VAR 2
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 3 14 read:read_list
            FUN 2 1 .
              FUN 0 1 ]
              VAR 0
            FUN 0 2 []
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 3
            FUN 3 14 read:read_list
              VAR 0
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 2 11 read:syntax
                FUN 2 1 .
                  FUN 0 8 , | or ]
                  FUN 2 1 .
                    FUN 0 8 expected
                    FUN 2 1 .
                      FUN 0 2 in
                      FUN 2 1 .
                        FUN 0 4 list
                        FUN 0 2 []
                VAR 0
              FUN 0 2 []
          FUN 0 2 []
CLS 7 20 read:after_prefix_op
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 7 20 read:after_prefix_op
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
        VAR 5
        VAR 6
      FUN 2 1 .
        FUN 2 1 <
          VAR 4
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 11 read:syntax
              FUN 2 1 .
                FUN 0 6 prefix
                FUN 2 1 .
                  FUN 0 8 operator
                  FUN 2 1 .
                    VAR 0
                    FUN 2 1 .
                      FUN 0 2 in
                      FUN 2 1 .
                        FUN 0 7 context
                        FUN 2 1 .
                          FUN 0 4 with
                          FUN 2 1 .
                            FUN 0 10 precedence
                            FUN 2 1 .
                              VAR 4
                              FUN 0 2 []
              VAR 3
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 8
        FUN 7 20 read:after_prefix_op
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 5
          VAR 6
        FUN 2 1 .
          FUN 2 11 read:peepop
            VAR 3
            VAR 7
          FUN 2 1 .
            FUN 2 19 read:prefix_is_atom
              VAR 7
              VAR 1
            FUN 2 1 .
              FUN 6 11 read:exprtl
                VAR 7
                VAR 1
                VAR 0
                VAR 4
                VAR 5
                VAR 6
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 10
          FUN 7 20 read:after_prefix_op
            VAR 0
            VAR 1
            VAR 2
            VAR 3
            VAR 4
            VAR 5
            VAR 6
          FUN 2 1 .
            FUN 4 9 read:read
              VAR 3
              VAR 2
              VAR 7
              VAR 8
            FUN 2 1 .
              FUN 2 3 =..
                VAR 9
                FUN 2 1 .
                  VAR 0
                  FUN 2 1 .
                    VAR 7
                    FUN 0 2 []
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 6 11 read:exprtl
                    VAR 8
                    VAR 1
                    VAR 9
                    VAR 4
                    VAR 5
                    VAR 6
                  FUN 0 2 []
        FUN 0 2 []
CLS 2 11 read:peepop
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 11 read:peepop
        FUN 2 1 .
          FUN 1 4 atom
            VAR 0
          FUN 2 1 .
            FUN 0 1 (
            VAR 1
        FUN 2 1 .
          FUN 1 4 atom
            VAR 0
          FUN 2 1 .
            FUN 0 1 (
            VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 2 11 read:peepop
          FUN 2 1 .
            FUN 1 4 atom
              VAR 0
            VAR 1
          FUN 2 1 .
            FUN 4 7 infixop
              VAR 0
              VAR 2
              VAR 3
              VAR 4
            VAR 1
        FUN 2 1 .
          FUN 4 12 read:infixop
            VAR 0
            VAR 2
            VAR 3
            VAR 4
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 2 11 read:peepop
            FUN 2 1 .
              FUN 1 4 atom
                VAR 0
              VAR 1
            FUN 2 1 .
              FUN 3 9 postfixop
                VAR 0
                VAR 2
                VAR 3
              VAR 1
          FUN 2 1 .
            FUN 3 14 read:postfixop
              VAR 0
              VAR 2
              VAR 3
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 2 11 read:peepop
              VAR 0
              VAR 0
            FUN 0 2 []
          FUN 0 2 []
CLS 2 19 read:prefix_is_atom
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 19 read:prefix_is_atom
        FUN 2 1 .
          VAR 0
          VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 19 read:prefix_is_atom
          VAR 0
          VAR 2
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 2 19 read:prefix_is_atom
          FUN 4 7 infixop
            VAR 0
            VAR 1
            VAR 2
            VAR 3
          VAR 4
        FUN 2 1 .
          FUN 2 2 >=
            VAR 1
            VAR 4
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 2 19 read:prefix_is_atom
            FUN 3 9 postfixop
              VAR 0
              VAR 1
              VAR 2
            VAR 3
          FUN 2 1 .
            FUN 2 2 >=
              VAR 1
              VAR 3
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 2 19 read:prefix_is_atom
              FUN 0 1 )
              VAR 0
            FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 1
              FUN 2 19 read:prefix_is_atom
                FUN 0 1 ]
                VAR 0
              FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 1
                FUN 2 19 read:prefix_is_atom
                  FUN 0 1 }
                  VAR 0
                FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 1
                  FUN 2 19 read:prefix_is_atom
                    FUN 0 1 |
                    VAR 0
                  FUN 2 1 .
                    FUN 2 2 >=
                      INT 1100
                      VAR 0
                    FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 1
                    FUN 2 19 read:prefix_is_atom
                      FUN 0 1 ,
                      VAR 0
                    FUN 2 1 .
                      FUN 2 2 >=
                        INT 1000
                        VAR 0
                      FUN 0 2 []
                  FUN 2 1 .
                    FUN 3 6 clause
                      INT 1
                      FUN 2 19 read:prefix_is_atom
                        FUN 0 2 []
                        VAR 0
                      FUN 0 2 []
                    FUN 0 2 []
CLS 11 18 read:exprtl0$5_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 11
      FUN 11 18 read:exprtl0$5_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
        VAR 5
        VAR 6
        VAR 7
        VAR 8
        VAR 9
        VAR 10
      FUN 2 1 .
        FUN 6 11 read:exprtl
          FUN 2 1 .
            FUN 4 7 infixop
              VAR 10
              VAR 9
              VAR 8
              VAR 7
            VAR 6
          INT 0
          VAR 5
          VAR 4
          VAR 3
          VAR 2
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 11
        FUN 11 18 read:exprtl0$5_0_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 5
          VAR 6
          VAR 7
          VAR 8
          VAR 9
          VAR 10
        FUN 2 1 .
          FUN 6 11 read:exprtl
            FUN 2 1 .
              FUN 3 9 postfixop
                VAR 10
                VAR 1
                VAR 0
              VAR 6
            INT 0
            VAR 5
            VAR 4
            VAR 3
            VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 5 12 read:exprtl0
  FUN 2 1 .
    FUN 3 6 clause
      INT 11
      FUN 5 12 read:exprtl0
        FUN 2 1 .
          FUN 1 4 atom
            VAR 0
          VAR 1
        VAR 2
        VAR 3
        VAR 4
        VAR 5
      FUN 2 1 .
        FUN 6 12 read:ambigop
          VAR 0
          VAR 6
          VAR 7
          VAR 8
          VAR 9
          VAR 10
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 11 18 read:exprtl0$5_0_0
              VAR 10
              VAR 9
              VAR 5
              VAR 4
              VAR 3
              VAR 2
              VAR 1
              VAR 8
              VAR 7
              VAR 6
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 9
        FUN 5 12 read:exprtl0
          FUN 2 1 .
            FUN 1 4 atom
              VAR 0
            VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 5
        FUN 2 1 .
          FUN 4 12 read:infixop
            VAR 0
            VAR 6
            VAR 7
            VAR 8
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 6 11 read:exprtl
                FUN 2 1 .
                  FUN 4 7 infixop
                    VAR 0
                    VAR 6
                    VAR 7
                    VAR 8
                  VAR 1
                INT 0
                VAR 2
                VAR 3
                VAR 4
                VAR 5
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 8
          FUN 5 12 read:exprtl0
            FUN 2 1 .
              FUN 1 4 atom
                VAR 0
              VAR 1
            VAR 2
            VAR 3
            VAR 4
            VAR 5
          FUN 2 1 .
            FUN 3 14 read:postfixop
              VAR 0
              VAR 6
              VAR 7
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 6 11 read:exprtl
                  FUN 2 1 .
                    FUN 3 9 postfixop
                      VAR 0
                      VAR 6
                      VAR 7
                    VAR 1
                  INT 0
                  VAR 2
                  VAR 3
                  VAR 4
                  VAR 5
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 7
            FUN 5 12 read:exprtl0
              FUN 2 1 .
                FUN 0 1 ,
                VAR 0
              VAR 1
              VAR 2
              VAR 3
              VAR 4
            FUN 2 1 .
              FUN 2 2 >=
                VAR 2
                INT 1000
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 4 9 read:read
                    VAR 0
                    INT 1000
                    VAR 5
                    VAR 6
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 6 11 read:exprtl
                        VAR 6
                        INT 1000
                        FUN 2 1 ,
                          VAR 1
                          VAR 5
                        VAR 2
                        VAR 3
                        VAR 4
                      FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 7
              FUN 5 12 read:exprtl0
                FUN 2 1 .
                  FUN 0 1 |
                  VAR 0
                VAR 1
                VAR 2
                VAR 3
                VAR 4
              FUN 2 1 .
                FUN 2 2 >=
                  VAR 2
                  INT 1100
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 4 9 read:read
                      VAR 0
                      INT 1100
                      VAR 5
                      VAR 6
                    FUN 2 1 .
                      FUN 0 1 !
                      FUN 2 1 .
                        FUN 6 11 read:exprtl
                          VAR 6
                          INT 1100
                          FUN 2 1 ;
                            VAR 1
                            VAR 5
                          VAR 2
                          VAR 3
                          VAR 4
                        FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 7
                FUN 5 12 read:exprtl0
                  FUN 2 1 .
                    VAR 0
                    VAR 1
                  VAR 2
                  VAR 3
                  VAR 4
                  VAR 5
                FUN 2 1 .
                  FUN 2 21 read:cant_follow_expr
                    VAR 0
                    VAR 6
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 2 11 read:syntax
                        FUN 2 1 .
                          VAR 6
                          FUN 2 1 .
                            FUN 0 7 follows
                            FUN 2 1 .
                              FUN 0 10 expression
                              FUN 0 2 []
                        FUN 2 1 .
                          VAR 0
                          VAR 1
                      FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 3
                  FUN 5 12 read:exprtl0
                    VAR 0
                    VAR 1
                    VAR 2
                    VAR 1
                    VAR 0
                  FUN 0 2 []
                FUN 0 2 []
CLS 2 21 read:cant_follow_expr
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 21 read:cant_follow_expr
        FUN 1 4 atom
          VAR 0
        FUN 0 4 atom
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 21 read:cant_follow_expr
          FUN 2 3 var
            VAR 0
            VAR 1
          FUN 0 8 variable
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 2 21 read:cant_follow_expr
            FUN 1 6 number
              VAR 0
            FUN 0 6 number
          FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 2 21 read:cant_follow_expr
              FUN 1 6 string
                VAR 0
              FUN 0 6 string
            FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 0
              FUN 2 21 read:cant_follow_expr
                FUN 0 2  (
                FUN 0 7 bracket
              FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 0
                FUN 2 21 read:cant_follow_expr
                  FUN 0 1 (
                  FUN 0 7 bracket
                FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 0
                  FUN 2 21 read:cant_follow_expr
                    FUN 0 1 [
                    FUN 0 7 bracket
                  FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 0
                    FUN 2 21 read:cant_follow_expr
                      FUN 0 1 {
                      FUN 0 7 bracket
                    FUN 0 2 []
                  FUN 0 2 []
CLS 6 11 read:exprtl
  FUN 2 1 .
    FUN 3 6 clause
      INT 13
      FUN 6 11 read:exprtl
        FUN 2 1 .
          FUN 4 7 infixop
            VAR 0
            VAR 1
            VAR 2
            VAR 3
          VAR 4
        VAR 5
        VAR 6
        VAR 7
        VAR 8
        VAR 9
      FUN 2 1 .
        FUN 2 2 >=
          VAR 7
          VAR 2
        FUN 2 1 .
          FUN 2 2 =<
            VAR 5
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 4 9 read:read
                VAR 4
                VAR 3
                VAR 10
                VAR 11
              FUN 2 1 .
                FUN 2 3 =..
                  VAR 12
                  FUN 2 1 .
                    VAR 0
                    FUN 2 1 .
                      VAR 6
                      FUN 2 1 .
                        VAR 10
                        FUN 0 2 []
                FUN 2 1 .
                  FUN 6 11 read:exprtl
                    VAR 11
                    VAR 2
                    VAR 12
                    VAR 7
                    VAR 8
                    VAR 9
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 11
        FUN 6 11 read:exprtl
          FUN 2 1 .
            FUN 3 9 postfixop
              VAR 0
              VAR 1
              VAR 2
            VAR 3
          VAR 4
          VAR 5
          VAR 6
          VAR 7
          VAR 8
        FUN 2 1 .
          FUN 2 2 >=
            VAR 6
            VAR 2
          FUN 2 1 .
            FUN 2 2 =<
              VAR 4
              VAR 1
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 3 =..
                  VAR 9
                  FUN 2 1 .
                    VAR 0
                    FUN 2 1 .
                      VAR 5
                      FUN 0 2 []
                FUN 2 1 .
                  FUN 2 11 read:peepop
                    VAR 3
                    VAR 10
                  FUN 2 1 .
                    FUN 6 11 read:exprtl
                      VAR 10
                      VAR 2
                      VAR 9
                      VAR 6
                      VAR 7
                      VAR 8
                    FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 8
          FUN 6 11 read:exprtl
            FUN 2 1 .
              FUN 0 1 ,
              VAR 0
            VAR 1
            VAR 2
            VAR 3
            VAR 4
            VAR 5
          FUN 2 1 .
            FUN 2 2 >=
              VAR 3
              INT 1000
            FUN 2 1 .
              FUN 2 1 <
                VAR 1
                INT 1000
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 4 9 read:read
                    VAR 0
                    INT 1000
                    VAR 6
                    VAR 7
                  FUN 2 1 .
                    FUN 6 11 read:exprtl
                      VAR 7
                      INT 1000
                      FUN 2 1 ,
                        VAR 2
                        VAR 6
                      VAR 3
                      VAR 4
                      VAR 5
                    FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 8
            FUN 6 11 read:exprtl
              FUN 2 1 .
                FUN 0 1 |
                VAR 0
              VAR 1
              VAR 2
              VAR 3
              VAR 4
              VAR 5
            FUN 2 1 .
              FUN 2 2 >=
                VAR 3
                INT 1100
              FUN 2 1 .
                FUN 2 1 <
                  VAR 1
                  INT 1100
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 4 9 read:read
                      VAR 0
                      INT 1100
                      VAR 6
                      VAR 7
                    FUN 2 1 .
                      FUN 6 11 read:exprtl
                        VAR 7
                        INT 1100
                        FUN 2 1 ;
                          VAR 2
                          VAR 6
                        VAR 3
                        VAR 4
                        VAR 5
                      FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 4
              FUN 6 11 read:exprtl
                VAR 0
                VAR 1
                VAR 2
                VAR 3
                VAR 2
                VAR 0
              FUN 0 2 []
            FUN 0 2 []
CLS 3 19 read:check_negative
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 19 read:check_negative
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 5 float
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 2 is
              VAR 1
              FUN 1 1 -
                VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 19 read:check_negative
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 2 >=
            VAR 0
            INT 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 2 is
                VAR 1
                FUN 1 1 -
                  VAR 0
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 19 read:check_negative
            VAR 0
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 1 16 read:min_integer
              VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 3
            FUN 3 19 read:check_negative
              VAR 0
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 2 11 read:syntax
                FUN 2 1 .
                  FUN 0 6 number
                  FUN 2 1 .
                    FUN 0 3 too
                    FUN 2 1 .
                      FUN 0 3 big
                      FUN 0 2 []
                FUN 2 1 .
                  FUN 1 6 number
                    VAR 0
                  VAR 2
              FUN 0 2 []
          FUN 0 2 []
CLS 2 19 read:check_positive
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 19 read:check_positive
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 5 float
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 19 read:check_positive
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 2 >=
            VAR 0
            INT 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 19 read:check_positive
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 11 read:syntax
              FUN 2 1 .
                FUN 0 6 number
                FUN 2 1 .
                  FUN 0 3 too
                  FUN 2 1 .
                    FUN 0 3 big
                    FUN 0 2 []
              FUN 2 1 .
                FUN 1 6 number
                  VAR 0
                VAR 1
            FUN 0 2 []
        FUN 0 2 []
CLS 1 16 read:min_integer
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 16 read:min_integer
        INT -1073741824
      FUN 0 2 []
    FUN 0 2 []
CLS 2 11 read:syntax
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 11 read:syntax
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 12 lists:length
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 7 asserta
            FUN 2 7 $syntax
              VAR 0
              VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 0 4 fail
              FUN 0 2 []
    FUN 0 2 []
CLS 1 11 read:syntax
  FUN 2 1 .
    FUN 3 6 clause
      INT 14
      FUN 1 11 read:syntax
        VAR 0
      FUN 2 1 .
        FUN 1 7 retract
          FUN 2 7 $syntax
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 1 19 database:retractall
            FUN 2 7 $syntax
              VAR 3
              VAR 4
          FUN 2 1 .
            FUN 2 12 lists:length
              VAR 0
              VAR 5
            FUN 2 1 .
              FUN 2 2 is
                VAR 6
                FUN 2 1 -
                  VAR 5
                  VAR 2
              FUN 2 1 .
                FUN 2 12 lists:length
                  VAR 7
                  VAR 6
                FUN 2 1 .
                  FUN 3 12 lists:append
                    VAR 7
                    VAR 8
                    VAR 0
                  FUN 2 1 .
                    FUN 3 18 read:list_to_codes
                      VAR 1
                      FUN 2 1 .
                        VAR 9
                        VAR 10
                      FUN 2 1 .
                        INT 58
                        VAR 11
                    FUN 2 1 .
                      FUN 3 18 read:list_to_codes
                        VAR 7
                        VAR 11
                        VAR 12
                      FUN 2 1 .
                        FUN 3 18 read:list_to_codes
                          FUN 2 1 .
                            FUN 0 8 <<here>>
                            VAR 8
                          VAR 12
                          FUN 0 2 []
                        FUN 2 1 .
                          FUN 2 10 atom_codes
                            VAR 13
                            VAR 10
                          FUN 2 1 .
                            FUN 1 5 throw
                              FUN 2 5 error
                                FUN 1 12 syntax_error
                                  VAR 13
                                FUN 2 1 /
                                  FUN 0 4 read
                                  INT 1
                            FUN 0 2 []
    FUN 0 2 []
CLS 3 18 read:list_to_codes
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 18 read:list_to_codes
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 3 18 read:list_to_codes
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 16 read:token_value
            VAR 0
            VAR 4
          FUN 2 1 .
            FUN 3 12 lists:append
              FUN 2 1 .
                INT 32
                VAR 4
              VAR 5
              VAR 2
            FUN 2 1 .
              FUN 3 18 read:list_to_codes
                VAR 1
                VAR 5
                VAR 3
              FUN 0 2 []
      FUN 0 2 []
CLS 2 16 read:token_value
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 16 read:token_value
        FUN 1 4 atom
          VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 10 atom_codes
            VAR 0
            VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 16 read:token_value
          FUN 2 3 var
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 10 atom_codes
              VAR 1
              VAR 2
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 16 read:token_value
            FUN 1 6 number
              VAR 0
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 12 number_codes
                VAR 0
                VAR 1
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 2 16 read:token_value
              FUN 1 6 string
                VAR 0
              VAR 1
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
              FUN 2 16 read:token_value
                FUN 2 5 error
                  VAR 0
                  VAR 1
                VAR 2
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 10 atom_codes
                    VAR 1
                    VAR 2
                  FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 2
                FUN 2 16 read:token_value
                  VAR 0
                  VAR 1
                FUN 2 1 .
                  FUN 2 10 atom_codes
                    VAR 0
                    VAR 1
                  FUN 0 2 []
              FUN 0 2 []
CLS 2 1 .
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 1 .
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 22 readprog:consult_files
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 1 7 consult
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 7 consult
        FUN 2 1 .
          VAR 0
          VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 22 readprog:consult_files
            FUN 2 1 .
              VAR 0
              VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 7 consult
          VAR 0
        FUN 2 1 .
          FUN 1 22 readprog:consult_files
            FUN 2 1 .
              VAR 0
              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 22 readprog:consult_files
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 22 readprog:consult_files
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 22 readprog:consult_files
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 21 readprog:consult_file
            VAR 0
          FUN 2 1 .
            FUN 1 22 readprog:consult_files
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 1 21 readprog:consult_file
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 21 readprog:consult_file
        VAR 0
      FUN 2 1 .
        FUN 3 17 readprog:get_file
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 7 asserta
            FUN 1 13 $consult_file
              VAR 1
          FUN 2 1 .
            FUN 1 7 asserta
              FUN 1 12 $prev_clause
                FUN 0 2 []
            FUN 2 1 .
              FUN 1 7 asserta
                FUN 1 6 $inits
                  FUN 0 2 []
              FUN 2 1 .
                FUN 1 18 readprog:put_preds
                  FUN 0 2 []
                FUN 2 1 .
                  FUN 2 26 execution:call_cleanup_det
                    FUN 2 1 ,
                      FUN 1 18 readprog:read_file
                        VAR 2
                      FUN 0 21 readprog:post_actions
                    FUN 1 20 readprog:end_consult
                      VAR 2
                  FUN 0 2 []
    FUN 0 2 []
CLS 1 20 readprog:end_consult
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 1 20 readprog:end_consult
        VAR 0
      FUN 2 1 .
        FUN 1 18 readprog:get_preds
          VAR 1
        FUN 2 1 .
          FUN 1 7 retract
            FUN 1 6 $inits
              VAR 2
          FUN 2 1 .
            FUN 1 7 retract
              FUN 1 12 $prev_clause
                VAR 3
            FUN 2 1 .
              FUN 1 7 retract
                FUN 1 13 $consult_file
                  VAR 4
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 1 5 close
                    VAR 0
                  FUN 0 2 []
    FUN 0 2 []
CLS 0 21 readprog:post_actions
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 0 21 readprog:post_actions
      FUN 2 1 .
        FUN 1 14 readprog:preds
          VAR 0
        FUN 2 1 .
          FUN 1 28 readprog:add_uncreated_preds
            VAR 0
          FUN 2 1 .
            FUN 0 17 readprog:do_inits
            FUN 0 2 []
    FUN 0 2 []
CLS 0 17 readprog:do_inits
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 0 17 readprog:do_inits
      FUN 2 1 .
        FUN 1 6 $inits
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 17 readprog:do_inits
              VAR 0
            FUN 0 2 []
    FUN 0 2 []
CLS 1 17 readprog:do_inits
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 17 readprog:do_inits
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 17 readprog:do_inits
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 4 call
            VAR 0
          FUN 2 1 .
            FUN 1 17 readprog:do_inits
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 3 17 readprog:get_file
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 3 17 readprog:get_file
        FUN 0 4 user
        FUN 0 4 user
        FUN 0 10 user_input
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 17 readprog:get_file
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 24 readprog:check_extension
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 3 4 open
              VAR 1
              FUN 0 4 read
              VAR 2
            FUN 0 2 []
      FUN 0 2 []
CLS 2 24 readprog:check_extension
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 24 readprog:check_extension
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 5 8 sub_atom
          VAR 0
          VAR 2
          INT 3
          INT 0
          FUN 0 3 .pl
        FUN 2 1 .
          FUN 2 1 >
            VAR 2
            INT 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 1
                VAR 0
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 24 readprog:check_extension
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 3 11 atom_concat
            VAR 0
            FUN 0 3 .pl
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 24 readprog:check_extension
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 13 err:err_check
              FUN 1 7 consult
                VAR 0
              FUN 2 1 .
                FUN 1 4 inst
                  VAR 0
                FUN 2 1 .
                  FUN 1 4 atom
                    VAR 0
                  FUN 0 2 []
            FUN 0 2 []
        FUN 0 2 []
CLS 1 21 readprog:include_file
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 21 readprog:include_file
        VAR 0
      FUN 2 1 .
        FUN 3 17 readprog:get_file
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 26 execution:call_cleanup_det
            FUN 1 18 readprog:read_file
              VAR 2
            FUN 1 5 close
              VAR 2
          FUN 0 2 []
    FUN 0 2 []
CLS 1 18 readprog:read_file
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 18 readprog:read_file
        VAR 0
      FUN 2 1 .
        FUN 1 13 current_input
          VAR 1
        FUN 2 1 .
          FUN 1 9 set_input
            VAR 0
          FUN 2 1 .
            FUN 2 26 execution:call_cleanup_det
              FUN 0 21 readprog:read_clauses
              FUN 1 17 readprog:end_file
                VAR 1
            FUN 0 2 []
    FUN 0 2 []
CLS 1 17 readprog:end_file
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 17 readprog:end_file
        VAR 0
      FUN 2 1 .
        FUN 1 9 set_input
          VAR 0
        FUN 0 2 []
    FUN 0 2 []
CLS 2 27 readprog:read_clauses$0_0_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 27 readprog:read_clauses$0_0_1
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 22 readprog:consult_error
              FUN 0 19 instantiation_error
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 27 readprog:read_clauses$0_0_1
          VAR 0
          FUN 0 11 end_of_file
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 0 4 true
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 27 readprog:read_clauses$0_0_1
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 1 8 callable
              VAR 1
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 23 readprog:add_to_program
                  VAR 1
                  VAR 0
                FUN 2 1 .
                  FUN 0 4 fail
                  FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 2 27 readprog:read_clauses$0_0_1
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 22 readprog:consult_error
                FUN 2 10 type_error
                  FUN 0 8 callable
                  VAR 1
              FUN 0 2 []
          FUN 0 2 []
CLS 0 21 readprog:read_clauses
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 0 21 readprog:read_clauses
      FUN 2 1 .
        FUN 0 6 repeat
        FUN 2 1 .
          FUN 2 9 read_term
            VAR 0
            FUN 2 1 .
              FUN 1 10 singletons
                VAR 1
              FUN 0 2 []
          FUN 2 1 .
            FUN 2 27 readprog:read_clauses$0_0_1
              VAR 1
              VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 0 2 []
    FUN 0 2 []
CLS 2 23 readprog:add_to_program
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 23 readprog:add_to_program
        FUN 1 2 :-
          VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 21 readprog:do_directive
            VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 11
        FUN 2 23 readprog:add_to_program
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 16 term:expand_term
            VAR 0
            VAR 2
          FUN 2 1 .
            FUN 3 21 readprog:split_clause
              VAR 2
              VAR 3
              VAR 4
            FUN 2 1 .
              FUN 3 7 functor
                VAR 3
                VAR 5
                VAR 6
              FUN 2 1 .
                FUN 2 1 =
                  VAR 7
                  FUN 2 1 /
                    VAR 5
                    VAR 6
                FUN 2 1 .
                  FUN 2 25 readprog:check_singletons
                    VAR 1
                    VAR 7
                  FUN 2 1 .
                    FUN 3 20 readprog:check_props
                      VAR 7
                      VAR 8
                      VAR 9
                    FUN 2 1 .
                      FUN 5 19 readprog:add_clause
                        VAR 8
                        VAR 9
                        VAR 7
                        VAR 3
                        VAR 4
                      FUN 2 1 .
                        FUN 1 7 retract
                          FUN 1 12 $prev_clause
                            VAR 10
                        FUN 2 1 .
                          FUN 1 7 asserta
                            FUN 1 12 $prev_clause
                              VAR 7
                          FUN 2 1 .
                            FUN 0 1 !
                            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 23 readprog:add_to_program
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 1 22 readprog:consult_error
              FUN 1 14 internal_error
                FUN 1 6 adding
                  VAR 0
            FUN 0 2 []
        FUN 0 2 []
CLS 2 31 readprog:check_singletons$2_1_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 31 readprog:check_singletons$2_1_0
        VAR 0
        FUN 0 2 []
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 31 readprog:check_singletons$2_1_0
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 readprog:warn
            FUN 0 28 Singleton variables ~w in ~q
            FUN 2 1 .
              VAR 1
              FUN 2 1 .
                VAR 0
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 25 readprog:check_singletons
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 25 readprog:check_singletons
        FUN 0 2 []
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 25 readprog:check_singletons
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 24 readprog:singleton_names
            VAR 0
            VAR 2
          FUN 2 1 .
            FUN 2 31 readprog:check_singletons$2_1_0
              VAR 1
              VAR 2
            FUN 0 2 []
      FUN 0 2 []
CLS 2 24 readprog:singleton_names
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 24 readprog:singleton_names
        FUN 0 2 []
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 2 24 readprog:singleton_names
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              VAR 1
            VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 10 atom_codes
            VAR 0
            FUN 2 1 .
              INT 95
              VAR 4
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 24 readprog:singleton_names
                VAR 2
                VAR 3
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 2 24 readprog:singleton_names
            FUN 2 1 .
              FUN 2 1 =
                VAR 0
                VAR 1
              VAR 2
            FUN 2 1 .
              VAR 0
              VAR 3
          FUN 2 1 .
            FUN 2 24 readprog:singleton_names
              VAR 2
              VAR 3
            FUN 0 2 []
        FUN 0 2 []
CLS 1 21 readprog:do_directive
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 21 readprog:do_directive
        FUN 3 2 op
          VAR 0
          VAR 1
          VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 3 2 op
            VAR 0
            VAR 1
            VAR 2
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 21 readprog:do_directive
          FUN 2 15 char_conversion
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 15 char_conversion
              VAR 0
              VAR 1
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 1 21 readprog:do_directive
            FUN 2 15 set_prolog_flag
              VAR 0
              VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 15 set_prolog_flag
                VAR 0
                VAR 1
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 1 21 readprog:do_directive
              FUN 1 14 initialization
                VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 1 27 readprog:add_initialization
                  VAR 0
                FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 1
              FUN 1 21 readprog:do_directive
                FUN 1 7 dynamic
                  VAR 0
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 21 readprog:add_property
                    FUN 0 7 dynamic
                    VAR 0
                  FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 1
                FUN 1 21 readprog:do_directive
                  FUN 1 9 multifile
                    VAR 0
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 2 21 readprog:add_property
                      FUN 0 9 multifile
                      VAR 0
                    FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 1
                  FUN 1 21 readprog:do_directive
                    FUN 1 13 discontiguous
                      VAR 0
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 2 21 readprog:add_property
                        FUN 0 13 discontiguous
                        VAR 0
                      FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 1
                    FUN 1 21 readprog:do_directive
                      FUN 1 7 include
                        VAR 0
                    FUN 2 1 .
                      FUN 0 1 !
                      FUN 2 1 .
                        FUN 1 21 readprog:include_file
                          VAR 0
                        FUN 0 2 []
                  FUN 2 1 .
                    FUN 3 6 clause
                      INT 1
                      FUN 1 21 readprog:do_directive
                        FUN 1 13 ensure_loaded
                          VAR 0
                      FUN 2 1 .
                        FUN 0 1 !
                        FUN 2 1 .
                          FUN 1 21 readprog:consult_file
                            VAR 0
                          FUN 0 2 []
                    FUN 2 1 .
                      FUN 3 6 clause
                        INT 3
                        FUN 1 21 readprog:do_directive
                          VAR 0
                        FUN 2 1 .
                          FUN 0 1 !
                          FUN 2 1 .
                            FUN 3 7 functor
                              VAR 0
                              VAR 1
                              VAR 2
                            FUN 2 1 .
                              FUN 2 13 readprog:warn
                                FUN 0 27 ~q is not a valid directive
                                FUN 2 1 .
                                  FUN 2 1 /
                                    VAR 1
                                    VAR 2
                                  FUN 0 2 []
                              FUN 0 2 []
                      FUN 0 2 []
CLS 1 27 readprog:add_initialization
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 27 readprog:add_initialization
        VAR 0
      FUN 2 1 .
        FUN 1 7 retract
          FUN 1 6 $inits
            VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 12 lists:append
              VAR 1
              FUN 2 1 .
                VAR 0
                FUN 0 2 []
              VAR 2
            FUN 2 1 .
              FUN 1 7 asserta
                FUN 1 6 $inits
                  VAR 2
              FUN 0 2 []
    FUN 0 2 []
CLS 5 19 readprog:add_clause
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 5 19 readprog:add_clause
        FUN 0 7 dynamic
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 4 20 readprog:add_dynamic
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 5 19 readprog:add_clause
          FUN 0 6 static
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 4 19 readprog:add_static
            VAR 0
            VAR 1
            VAR 2
            VAR 3
          FUN 0 2 []
      FUN 0 2 []
CLS 4 20 readprog:add_dynamic
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 4 20 readprog:add_dynamic
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 25 execution:convert_to_goal
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 3 8 $assertz
            VAR 2
            FUN 2 1 .
              VAR 4
              FUN 0 2 []
            VAR 5
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 18 readprog:add_multi
                VAR 0
                VAR 1
                VAR 5
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 4 20 readprog:add_dynamic
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 1 18 readprog:file_name
            VAR 4
          FUN 2 1 .
            FUN 2 13 err:err_check
              VAR 4
              FUN 2 1 .
                FUN 1 12 conv_to_goal
                  VAR 3
                FUN 0 2 []
            FUN 0 2 []
      FUN 0 2 []
CLS 4 19 readprog:add_static
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 4 19 readprog:add_static
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 18 readprog:body_list
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 3 8 $asserts
            VAR 2
            VAR 4
            VAR 5
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 18 readprog:add_multi
                VAR 0
                VAR 1
                VAR 5
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 4 19 readprog:add_static
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 1 18 readprog:file_name
            VAR 4
          FUN 2 1 .
            FUN 2 13 err:err_check
              VAR 4
              FUN 0 2 []
            FUN 0 2 []
      FUN 0 2 []
CLS 3 18 readprog:add_multi
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 18 readprog:add_multi
        FUN 0 10 singlefile
        VAR 0
        VAR 1
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 18 readprog:add_multi
          FUN 0 9 multifile
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 10 $refclause
            VAR 1
          FUN 2 1 .
            FUN 1 18 readprog:file_name
              VAR 2
            FUN 2 1 .
              FUN 1 7 retract
                FUN 3 12 $pred_origin
                  VAR 0
                  VAR 3
                  VAR 4
              FUN 2 1 .
                FUN 1 7 asserta
                  FUN 3 12 $pred_origin
                    VAR 0
                    VAR 2
                    FUN 2 1 .
                      VAR 1
                      VAR 4
                FUN 0 2 []
      FUN 0 2 []
CLS 2 21 readprog:add_property
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 2 21 readprog:add_property
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 14 readprog:preds
          VAR 2
        FUN 2 1 .
          FUN 2 26 readprog:directive_to_list
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 4 26 readprog:add_property_list
              VAR 3
              VAR 0
              VAR 2
              VAR 4
            FUN 2 1 .
              FUN 1 18 readprog:get_preds
                VAR 5
              FUN 2 1 .
                FUN 1 18 readprog:put_preds
                  VAR 4
                FUN 0 2 []
    FUN 0 2 []
CLS 4 26 readprog:add_property_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 4 26 readprog:add_property_list
        FUN 0 2 []
        VAR 0
        VAR 1
        VAR 1
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 4 26 readprog:add_property_list
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 2 23 readprog:check_pred_ind
            VAR 0
            VAR 2
          FUN 2 1 .
            FUN 4 25 readprog:add_property_one
              VAR 0
              VAR 2
              VAR 3
              VAR 5
            FUN 2 1 .
              FUN 4 26 readprog:add_property_list
                VAR 1
                VAR 2
                VAR 5
                VAR 4
              FUN 0 2 []
      FUN 0 2 []
CLS 7 31 readprog:add_property_one$4_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 7 31 readprog:add_property_one$4_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
        VAR 5
        FUN 0 3 yes
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 13 readprog:warn
            FUN 0 55 directive ~w used after first occurance of procedure ~q
            FUN 2 1 .
              VAR 5
              FUN 2 1 .
                VAR 4
                FUN 0 2 []
          FUN 2 1 .
            FUN 2 1 =
              VAR 3
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 8
        FUN 7 31 readprog:add_property_one$4_0_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 5
          VAR 6
        FUN 2 1 .
          FUN 3 24 readprog:add_property_to
            VAR 5
            VAR 1
            VAR 7
          FUN 2 1 .
            FUN 2 1 =
              VAR 3
              FUN 2 1 .
                FUN 3 5 props
                  VAR 4
                  VAR 6
                  VAR 7
                VAR 0
            FUN 0 2 []
      FUN 0 2 []
CLS 4 25 readprog:add_property_one
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 4 25 readprog:add_property_one
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 3 12 lists:select
          FUN 3 5 props
            VAR 0
            VAR 4
            VAR 5
          VAR 2
          VAR 6
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 7 31 readprog:add_property_one$4_0_0
              VAR 6
              VAR 5
              VAR 2
              VAR 3
              VAR 0
              VAR 1
              VAR 4
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 25 readprog:add_property_one
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 1 =
            VAR 3
            FUN 2 1 .
              FUN 3 5 props
                VAR 0
                FUN 0 2 no
                FUN 2 1 .
                  VAR 1
                  FUN 0 2 []
              VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 3 24 readprog:add_property_to
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 24 readprog:add_property_to
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 12 lists:member
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 2
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 24 readprog:add_property_to
          VAR 0
          VAR 1
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 2 23 readprog:check_pred_ind
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 23 readprog:check_pred_ind
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 2 1 =
            VAR 0
            FUN 2 1 /
              VAR 2
              VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 23 readprog:check_pred_ind
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            VAR 1
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 8 pred_ind
                  VAR 0
                FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 4 26 readprog:check_props$3_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 26 readprog:check_props$3_0_0
        VAR 0
        VAR 1
        VAR 2
        FUN 0 3 yes
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 25 readprog:check_contiguity
            VAR 2
            VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 4 26 readprog:check_props$3_0_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 4 20 readprog:create_pred
            VAR 2
            VAR 1
            VAR 0
            VAR 4
          FUN 2 1 .
            FUN 1 18 readprog:get_preds
              VAR 5
            FUN 2 1 .
              FUN 1 18 readprog:put_preds
                VAR 4
              FUN 0 2 []
      FUN 0 2 []
CLS 3 20 readprog:check_props
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 3 20 readprog:check_props
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 14 readprog:preds
          VAR 3
        FUN 2 1 .
          FUN 4 17 readprog:get_prop
            VAR 0
            FUN 3 5 props
              VAR 0
              VAR 4
              VAR 5
            VAR 3
            VAR 6
          FUN 2 1 .
            FUN 4 16 readprog:is_prop
              VAR 5
              FUN 0 7 dynamic
              FUN 0 6 static
              VAR 1
            FUN 2 1 .
              FUN 4 16 readprog:is_prop
                VAR 5
                FUN 0 9 multifile
                FUN 0 10 singlefile
                VAR 2
              FUN 2 1 .
                FUN 4 26 readprog:check_props$3_0_0
                  VAR 6
                  VAR 5
                  VAR 0
                  VAR 4
                FUN 0 2 []
    FUN 0 2 []
CLS 3 31 readprog:check_contiguity$2_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 31 readprog:check_contiguity$2_0_0
        VAR 0
        VAR 1
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 31 readprog:check_contiguity$2_0_0
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 12 lists:member
            FUN 0 13 discontiguous
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 0 4 true
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 3 31 readprog:check_contiguity$2_0_0
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 2 13 readprog:warn
              FUN 0 30 predicate ~q is not contiguous
              FUN 2 1 .
                VAR 2
                FUN 0 2 []
            FUN 0 2 []
        FUN 0 2 []
CLS 2 25 readprog:check_contiguity
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 25 readprog:check_contiguity
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 12 $prev_clause
          VAR 2
        FUN 2 1 .
          FUN 3 31 readprog:check_contiguity$2_0_0
            VAR 1
            VAR 2
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 4 17 readprog:get_prop
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 4 17 readprog:get_prop
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 1 =
          VAR 1
          FUN 3 5 props
            VAR 0
            VAR 4
            VAR 5
        FUN 2 1 .
          FUN 3 12 lists:select
            VAR 1
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 4 17 readprog:get_prop
          VAR 0
          FUN 3 5 props
            VAR 0
            FUN 0 2 no
            FUN 0 2 []
          VAR 1
          VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 3 34 readprog:add_uncreated_preds$1_2_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 34 readprog:add_uncreated_preds$1_2_1
        VAR 0
        VAR 1
        FUN 0 2 []
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 4 16 readprog:is_prop
            VAR 1
            FUN 0 7 dynamic
            FUN 0 6 static
            VAR 2
          FUN 2 1 .
            FUN 2 18 readprog:make_pred
              VAR 2
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 34 readprog:add_uncreated_preds$1_2_1
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 1 28 readprog:add_uncreated_preds
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 28 readprog:add_uncreated_preds
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 1 28 readprog:add_uncreated_preds
          FUN 2 1 .
            FUN 3 5 props
              VAR 0
              FUN 0 3 yes
              VAR 1
            VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 28 readprog:add_uncreated_preds
              VAR 2
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 10
          FUN 1 28 readprog:add_uncreated_preds
            FUN 2 1 .
              FUN 3 5 props
                VAR 0
                VAR 1
                VAR 2
              VAR 3
          FUN 2 1 .
            FUN 4 20 readprog:create_pred
              VAR 0
              VAR 2
              FUN 0 2 []
              VAR 4
            FUN 2 1 .
              FUN 2 1 =
                VAR 0
                FUN 2 1 /
                  VAR 5
                  VAR 6
              FUN 2 1 .
                FUN 3 7 functor
                  VAR 7
                  VAR 5
                  VAR 6
                FUN 2 1 .
                  FUN 3 10 $pred_info
                    VAR 7
                    VAR 8
                    VAR 9
                  FUN 2 1 .
                    FUN 3 34 readprog:add_uncreated_preds$1_2_1
                      VAR 7
                      VAR 2
                      VAR 8
                    FUN 2 1 .
                      FUN 1 28 readprog:add_uncreated_preds
                        VAR 3
                      FUN 0 2 []
        FUN 0 2 []
CLS 2 18 readprog:make_pred
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 18 readprog:make_pred
        FUN 0 7 dynamic
        VAR 0
      FUN 2 1 .
        FUN 1 7 asserta
          VAR 0
        FUN 2 1 .
          FUN 1 7 retract
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 18 readprog:make_pred
          FUN 0 6 static
          VAR 0
        FUN 2 1 .
          FUN 3 8 $asserts
            VAR 0
            FUN 0 2 []
            VAR 1
          FUN 2 1 .
            FUN 1 10 $refclause
              VAR 1
            FUN 2 1 .
              FUN 1 10 $delclause
                VAR 1
              FUN 0 2 []
      FUN 0 2 []
CLS 4 26 readprog:create_pred$4_0_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 26 readprog:create_pred$4_0_1
        VAR 0
        VAR 1
        VAR 2
        FUN 0 10 singlefile
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 3 25 readprog:check_singlefile
            VAR 2
            VAR 1
            VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 26 readprog:create_pred$4_0_1
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 1 24 readprog:check_multifile
            VAR 1
          FUN 2 1 .
            FUN 2 22 readprog:add_multifile
              VAR 2
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 4 20 readprog:create_pred
  FUN 2 1 .
    FUN 3 6 clause
      INT 12
      FUN 4 20 readprog:create_pred
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 4 16 readprog:is_prop
          VAR 1
          FUN 0 7 dynamic
          FUN 0 6 static
          VAR 4
        FUN 2 1 .
          FUN 4 16 readprog:is_prop
            VAR 1
            FUN 0 9 multifile
            FUN 0 10 singlefile
            VAR 5
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              FUN 2 1 /
                VAR 6
                VAR 7
            FUN 2 1 .
              FUN 3 7 functor
                VAR 8
                VAR 6
                VAR 7
              FUN 2 1 .
                FUN 3 10 $pred_info
                  VAR 8
                  VAR 9
                  VAR 10
                FUN 2 1 .
                  FUN 1 18 readprog:file_name
                    VAR 11
                  FUN 2 1 .
                    FUN 3 21 readprog:check_access
                      VAR 9
                      VAR 10
                      VAR 0
                    FUN 2 1 .
                      FUN 3 19 readprog:check_type
                        VAR 9
                        VAR 4
                        VAR 0
                      FUN 2 1 .
                        FUN 4 26 readprog:create_pred$4_0_1
                          VAR 8
                          VAR 0
                          VAR 11
                          VAR 5
                        FUN 2 1 .
                          FUN 2 1 =
                            VAR 3
                            FUN 2 1 .
                              FUN 3 5 props
                                VAR 0
                                FUN 0 3 yes
                                VAR 1
                              VAR 2
                          FUN 0 2 []
    FUN 0 2 []
CLS 3 21 readprog:check_access
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 21 readprog:check_access
        FUN 0 2 []
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 21 readprog:check_access
          VAR 0
          FUN 0 11 access_user
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 3 21 readprog:check_access
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 1 22 readprog:consult_error
              FUN 3 16 permission_error
                FUN 0 8 redefine
                FUN 0 17 builtin_predicate
                VAR 2
            FUN 0 2 []
        FUN 0 2 []
CLS 3 19 readprog:check_type
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 19 readprog:check_type
        FUN 0 2 []
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 3 19 readprog:check_type
          FUN 0 11 pred_static
          FUN 0 6 static
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 3 19 readprog:check_type
            FUN 0 12 pred_dynamic
            FUN 0 7 dynamic
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 3 19 readprog:check_type
              VAR 0
              FUN 0 6 static
              VAR 1
            FUN 2 1 .
              FUN 1 22 readprog:consult_error
                FUN 3 16 permission_error
                  FUN 0 6 modify
                  FUN 0 20 not_static_procedure
                  VAR 1
              FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 2
              FUN 3 19 readprog:check_type
                VAR 0
                FUN 0 7 dynamic
                VAR 1
              FUN 2 1 .
                FUN 1 22 readprog:consult_error
                  FUN 3 16 permission_error
                    FUN 0 6 modify
                    FUN 0 16 static_procedure
                    VAR 1
                FUN 0 2 []
            FUN 0 2 []
CLS 4 31 readprog:check_singlefile$3_0_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 31 readprog:check_singlefile$3_0_1
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 2 \=
          VAR 3
          FUN 0 10 singlefile
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 22 readprog:consult_error
              FUN 3 16 permission_error
                FUN 0 8 redefine
                FUN 0 9 multifile
                VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 31 readprog:check_singlefile$3_0_1
          VAR 0
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 0 4 true
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 4 31 readprog:check_singlefile$3_0_1
            VAR 0
            VAR 1
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 1 22 readprog:consult_error
              FUN 3 16 permission_error
                FUN 0 8 redefine
                FUN 1 13 original_file
                  VAR 1
                VAR 2
            FUN 0 2 []
        FUN 0 2 []
CLS 3 25 readprog:check_singlefile
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 3 25 readprog:check_singlefile
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 6 clause
          FUN 3 12 $pred_origin
            VAR 1
            VAR 3
            VAR 4
          VAR 5
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 31 readprog:check_singlefile$3_0_1
              VAR 0
              VAR 3
              VAR 1
              VAR 4
            FUN 2 1 .
              FUN 1 8 $abolish
                VAR 2
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 25 readprog:check_singlefile
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 7 asserta
            FUN 3 12 $pred_origin
              VAR 1
              VAR 0
              FUN 0 10 singlefile
          FUN 2 1 .
            FUN 1 8 $abolish
              VAR 2
            FUN 0 2 []
      FUN 0 2 []
CLS 2 30 readprog:check_multifile$1_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 30 readprog:check_multifile$1_0_0
        VAR 0
        FUN 0 10 singlefile
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 22 readprog:consult_error
            FUN 3 16 permission_error
              FUN 0 8 redefine
              FUN 0 10 singlefile
              VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 30 readprog:check_multifile$1_0_0
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 1 24 readprog:check_multifile
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 1 24 readprog:check_multifile
        VAR 0
      FUN 2 1 .
        FUN 2 6 clause
          FUN 3 12 $pred_origin
            VAR 0
            VAR 1
            VAR 2
          VAR 3
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 30 readprog:check_multifile$1_0_0
              VAR 0
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 24 readprog:check_multifile
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 2 28 readprog:add_multifile$2_0_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 28 readprog:add_multifile$2_0_1
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 7 retract
          FUN 3 12 $pred_origin
            VAR 1
            VAR 0
            VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 20 readprog:del_clauses
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 28 readprog:add_multifile$2_0_1
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
      FUN 0 2 []
CLS 2 22 readprog:add_multifile
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 22 readprog:add_multifile
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 28 readprog:add_multifile$2_0_1
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 7 asserta
            FUN 3 12 $pred_origin
              VAR 1
              VAR 0
              FUN 0 2 []
          FUN 0 2 []
    FUN 0 2 []
CLS 1 20 readprog:del_clauses
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 20 readprog:del_clauses
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 20 readprog:del_clauses
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 10 $delclause
            VAR 0
          FUN 2 1 .
            FUN 1 20 readprog:del_clauses
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 4 16 readprog:is_prop
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 16 readprog:is_prop
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 12 lists:member
          VAR 1
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 3
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 16 readprog:is_prop
          VAR 0
          VAR 1
          VAR 2
          VAR 2
        FUN 0 2 []
      FUN 0 2 []
CLS 1 14 readprog:preds
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 14 readprog:preds
        VAR 0
      FUN 2 1 .
        FUN 1 6 $preds
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 0 2 []
CLS 1 18 readprog:get_preds
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 18 readprog:get_preds
        VAR 0
      FUN 2 1 .
        FUN 1 7 retract
          FUN 1 6 $preds
            VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 0 2 []
CLS 1 18 readprog:put_preds
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 18 readprog:put_preds
        VAR 0
      FUN 2 1 .
        FUN 1 7 asserta
          FUN 1 6 $preds
            VAR 0
        FUN 0 2 []
    FUN 0 2 []
CLS 2 13 readprog:warn
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 13 readprog:warn
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 18 readprog:file_name
          VAR 2
        FUN 2 1 .
          FUN 1 15 io:error_stream
            VAR 3
          FUN 2 1 .
            FUN 3 11 misc:format
              VAR 3
              FUN 0 14 {Warning: ~w: 
              FUN 2 1 .
                VAR 2
                FUN 0 2 []
            FUN 2 1 .
              FUN 3 11 misc:format
                VAR 3
                VAR 0
                VAR 1
              FUN 2 1 .
                FUN 2 5 write
                  VAR 3
                  FUN 0 2 }

                FUN 0 2 []
    FUN 0 2 []
CLS 1 22 readprog:consult_error
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 1 22 readprog:consult_error
        VAR 0
      FUN 2 1 .
        FUN 1 18 readprog:file_name
          VAR 1
        FUN 2 1 .
          FUN 1 5 throw
            FUN 2 5 error
              VAR 0
              VAR 1
          FUN 0 2 []
    FUN 0 2 []
CLS 1 18 readprog:file_name
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 18 readprog:file_name
        VAR 0
      FUN 2 1 .
        FUN 1 13 $consult_file
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 0 2 []
CLS 3 21 readprog:split_clause
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 21 readprog:split_clause
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 2 1 =
            VAR 0
            FUN 2 2 :-
              VAR 1
              VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 3 21 readprog:split_clause
          VAR 0
          VAR 0
          FUN 0 4 true
        FUN 0 2 []
      FUN 0 2 []
CLS 2 24 readprog:body_list$2_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 24 readprog:body_list$2_0_0
        VAR 0
        FUN 2 1 .
          FUN 0 4 true
          FUN 0 2 []
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 0
            FUN 0 2 []
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 24 readprog:body_list$2_0_0
          VAR 0
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 2 18 readprog:body_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 18 readprog:body_list
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 3 18 readprog:body_list
          VAR 0
          FUN 0 2 []
          VAR 2
        FUN 2 1 .
          FUN 2 24 readprog:body_list$2_0_0
            VAR 1
            VAR 2
          FUN 0 2 []
    FUN 0 2 []
CLS 3 18 readprog:body_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 18 readprog:body_list
        VAR 0
        VAR 1
        FUN 2 1 .
          FUN 1 4 call
            VAR 0
          VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 18 readprog:body_list
          FUN 2 1 ,
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 18 readprog:body_list
              VAR 1
              VAR 2
              VAR 4
            FUN 2 1 .
              FUN 3 18 readprog:body_list
                VAR 0
                VAR 4
                VAR 3
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 6
          FUN 3 18 readprog:body_list
            FUN 2 1 ;
              VAR 0
              VAR 1
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 21 readprog:convert_goal
                FUN 2 1 ;
                  VAR 0
                  VAR 1
                VAR 4
              FUN 2 1 .
                FUN 2 1 =
                  VAR 3
                  FUN 2 1 .
                    FUN 1 7 $choice
                      VAR 5
                    FUN 2 1 .
                      FUN 2 19 execution:interpret
                        VAR 4
                        VAR 5
                      VAR 2
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 6
            FUN 3 18 readprog:body_list
              FUN 2 2 ->
                VAR 0
                VAR 1
              VAR 2
              VAR 3
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 21 readprog:convert_goal
                  FUN 2 2 ->
                    VAR 0
                    VAR 1
                  VAR 4
                FUN 2 1 .
                  FUN 2 1 =
                    VAR 3
                    FUN 2 1 .
                      FUN 1 7 $choice
                        VAR 5
                      FUN 2 1 .
                        FUN 2 19 execution:interpret
                          VAR 4
                          VAR 5
                        VAR 2
                  FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 3
              FUN 3 18 readprog:body_list
                VAR 0
                VAR 1
                VAR 2
              FUN 2 1 .
                FUN 1 8 callable
                  VAR 0
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 2 1 =
                      VAR 2
                      FUN 2 1 .
                        VAR 0
                        VAR 1
                    FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 4
                FUN 3 18 readprog:body_list
                  VAR 0
                  VAR 1
                  VAR 2
                FUN 2 1 .
                  FUN 1 18 readprog:file_name
                    VAR 3
                  FUN 2 1 .
                    FUN 2 13 err:err_check
                      VAR 3
                      FUN 2 1 .
                        FUN 1 8 callable
                          VAR 0
                        FUN 0 2 []
                    FUN 0 2 []
              FUN 0 2 []
CLS 2 21 readprog:convert_goal
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 21 readprog:convert_goal
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 25 execution:convert_to_goal
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 21 readprog:convert_goal
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 18 readprog:file_name
            VAR 2
          FUN 2 1 .
            FUN 2 13 err:err_check
              VAR 2
              FUN 2 1 .
                FUN 1 12 conv_to_goal
                  VAR 0
                FUN 0 2 []
            FUN 0 2 []
      FUN 0 2 []
CLS 2 26 readprog:directive_to_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 26 readprog:directive_to_list
        FUN 0 2 []
        FUN 0 2 []
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 26 readprog:directive_to_list
          FUN 2 1 .
            VAR 0
            VAR 1
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 2 26 readprog:directive_to_list
            FUN 2 1 ,
              VAR 0
              VAR 1
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 20 readprog:seq_to_list
                FUN 2 1 ,
                  VAR 0
                  VAR 1
                FUN 0 2 []
                VAR 2
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 2 26 readprog:directive_to_list
              VAR 0
              FUN 2 1 .
                VAR 0
                FUN 0 2 []
            FUN 0 2 []
          FUN 0 2 []
CLS 3 20 readprog:seq_to_list
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 3 20 readprog:seq_to_list
        FUN 2 1 ,
          VAR 0
          VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 3 20 readprog:seq_to_list
            VAR 1
            VAR 2
            VAR 4
          FUN 2 1 .
            FUN 3 20 readprog:seq_to_list
              VAR 0
              VAR 4
              VAR 3
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 20 readprog:seq_to_list
          VAR 0
          VAR 1
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 3 7 findall
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 7 findall
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 8 callable
          VAR 1
        FUN 2 1 .
          FUN 1 26 lists:list_or_partial_list
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 20 setof:save_instances
                FUN 1 1 -
                  VAR 0
                VAR 1
              FUN 2 1 .
                FUN 2 20 setof:list_instances
                  FUN 0 2 []
                  VAR 2
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 7 findall
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 3 7 findall
              VAR 0
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 1 4 inst
                VAR 1
              FUN 2 1 .
                FUN 1 8 callable
                  VAR 1
                FUN 2 1 .
                  FUN 1 4 list
                    VAR 2
                  FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 3 5 setof
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 3 5 setof
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 4 20 setof:free_variables
          VAR 1
          VAR 3
          VAR 0
          VAR 4
        FUN 2 1 .
          FUN 1 8 callable
            VAR 3
          FUN 2 1 .
            FUN 1 26 lists:list_or_partial_list
              VAR 2
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 4 16 setof:bagof_vars
                  VAR 4
                  VAR 0
                  VAR 3
                  VAR 5
                FUN 2 1 .
                  FUN 2 10 setof:sort
                    VAR 5
                    VAR 2
                  FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 5 setof
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 3 5 setof
              VAR 0
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 1 4 inst
                VAR 1
              FUN 2 1 .
                FUN 1 2 do
                  FUN 4 20 setof:free_variables
                    VAR 1
                    VAR 3
                    VAR 0
                    VAR 4
                FUN 2 1 .
                  FUN 1 4 inst
                    VAR 3
                  FUN 2 1 .
                    FUN 1 8 callable
                      VAR 3
                    FUN 2 1 .
                      FUN 1 4 list
                        VAR 2
                      FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 3 5 bagof
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 3 5 bagof
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 4 20 setof:free_variables
          VAR 1
          VAR 3
          VAR 0
          VAR 4
        FUN 2 1 .
          FUN 1 8 callable
            VAR 3
          FUN 2 1 .
            FUN 1 26 lists:list_or_partial_list
              VAR 2
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 4 16 setof:bagof_vars
                  VAR 4
                  VAR 0
                  VAR 3
                  VAR 2
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 5 bagof
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 3 5 bagof
              VAR 0
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 1 4 inst
                VAR 1
              FUN 2 1 .
                FUN 1 2 do
                  FUN 4 20 setof:free_variables
                    VAR 1
                    VAR 3
                    VAR 0
                    VAR 4
                FUN 2 1 .
                  FUN 1 4 inst
                    VAR 3
                  FUN 2 1 .
                    FUN 1 8 callable
                      VAR 3
                    FUN 2 1 .
                      FUN 1 4 list
                        VAR 2
                      FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 4 16 setof:bagof_vars
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 16 setof:bagof_vars
        FUN 0 2 []
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 20 setof:save_instances
            FUN 1 1 -
              VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 20 setof:list_instances
              FUN 0 2 []
              VAR 2
            FUN 2 1 .
              FUN 2 2 \=
                VAR 2
                FUN 0 2 []
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 10
        FUN 4 16 setof:bagof_vars
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 3 =..
            VAR 4
            FUN 2 1 .
              FUN 0 1 .
              VAR 0
          FUN 2 1 .
            FUN 3 7 functor
              VAR 4
              FUN 0 1 .
              VAR 5
            FUN 2 1 .
              FUN 2 20 setof:save_instances
                FUN 2 1 -
                  VAR 4
                  VAR 1
                VAR 2
              FUN 2 1 .
                FUN 5 20 setof:list_instances
                  VAR 4
                  VAR 5
                  VAR 6
                  FUN 0 2 []
                  VAR 7
                FUN 2 1 .
                  FUN 2 13 setof:keysort
                    VAR 7
                    VAR 8
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 3 23 setof:concordant_subset
                        VAR 8
                        VAR 4
                        VAR 9
                      FUN 2 1 .
                        FUN 2 1 =
                          VAR 3
                          VAR 9
                        FUN 0 2 []
      FUN 0 2 []
CLS 2 20 setof:save_instances
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 20 setof:save_instances
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 7 asserta
          FUN 1 13 $bag_instance
            FUN 0 1 -
        FUN 2 1 .
          FUN 1 4 call
            VAR 1
          FUN 2 1 .
            FUN 1 7 asserta
              FUN 1 13 $bag_instance
                VAR 0
            FUN 2 1 .
              FUN 0 4 fail
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 20 setof:save_instances
          VAR 0
          VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 2 20 setof:list_instances
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 20 setof:list_instances
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 7 retract
          FUN 1 13 $bag_instance
            VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 20 setof:list_instances
              VAR 2
              VAR 0
              VAR 1
            FUN 0 2 []
    FUN 0 2 []
CLS 3 20 setof:list_instances
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 20 setof:list_instances
        FUN 0 1 -
        VAR 0
        VAR 1
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
        FUN 3 20 setof:list_instances
          FUN 1 1 -
            VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 20 setof:list_instances
            FUN 2 1 .
              VAR 0
              VAR 1
            VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 5 20 setof:list_instances
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 5 20 setof:list_instances
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 1 7 retract
          FUN 1 13 $bag_instance
            VAR 5
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 6 20 setof:list_instances
              VAR 5
              VAR 0
              VAR 1
              VAR 2
              VAR 3
              VAR 4
            FUN 0 2 []
    FUN 0 2 []
CLS 6 20 setof:list_instances
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 6 20 setof:list_instances
        FUN 0 1 -
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 3
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 7
        FUN 6 20 setof:list_instances
          FUN 2 1 -
            VAR 0
            VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 5
          VAR 6
        FUN 2 1 .
          FUN 4 27 setof:replace_key_variables
            VAR 3
            VAR 2
            VAR 4
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 5 20 setof:list_instances
                VAR 2
                VAR 3
                VAR 4
                FUN 2 1 .
                  FUN 2 1 -
                    VAR 0
                    VAR 1
                  VAR 5
                VAR 6
              FUN 0 2 []
      FUN 0 2 []
CLS 4 27 setof:replace_key_variables
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 27 setof:replace_key_variables
        INT 0
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 7
        FUN 4 27 setof:replace_key_variables
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 3 3 arg
            VAR 0
            VAR 3
            VAR 4
          FUN 2 1 .
            FUN 1 6 nonvar
              VAR 4
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 3 23 setof:replace_variables
                  VAR 4
                  VAR 2
                  VAR 5
                FUN 2 1 .
                  FUN 2 2 is
                    VAR 6
                    FUN 2 1 -
                      VAR 0
                      INT 1
                  FUN 2 1 .
                    FUN 4 27 setof:replace_key_variables
                      VAR 6
                      VAR 1
                      VAR 5
                      VAR 3
                    FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 6
          FUN 4 27 setof:replace_key_variables
            VAR 0
            VAR 1
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 3 3 arg
              VAR 0
              VAR 1
              VAR 4
            FUN 2 1 .
              FUN 3 3 arg
                VAR 0
                VAR 3
                VAR 4
              FUN 2 1 .
                FUN 2 2 is
                  VAR 5
                  FUN 2 1 -
                    VAR 0
                    INT 1
                FUN 2 1 .
                  FUN 4 27 setof:replace_key_variables
                    VAR 5
                    VAR 1
                    VAR 2
                    VAR 3
                  FUN 0 2 []
        FUN 0 2 []
CLS 3 23 setof:replace_variables
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 23 setof:replace_variables
        VAR 0
        FUN 2 1 .
          VAR 1
          VAR 2
        VAR 2
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 23 setof:replace_variables
          VAR 0
          VAR 1
          VAR 1
        FUN 2 1 .
          FUN 1 6 atomic
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 5
          FUN 3 23 setof:replace_variables
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 3 7 functor
              VAR 0
              VAR 3
              VAR 4
            FUN 2 1 .
              FUN 4 28 setof:replace_variables_term
                VAR 4
                VAR 0
                VAR 1
                VAR 2
              FUN 0 2 []
        FUN 0 2 []
CLS 4 28 setof:replace_variables_term
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 4 28 setof:replace_variables_term
        INT 0
        VAR 0
        VAR 1
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 7
        FUN 4 28 setof:replace_variables_term
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 3 3 arg
            VAR 0
            VAR 1
            VAR 4
          FUN 2 1 .
            FUN 3 23 setof:replace_variables
              VAR 4
              VAR 2
              VAR 5
            FUN 2 1 .
              FUN 2 2 is
                VAR 6
                FUN 2 1 -
                  VAR 0
                  INT 1
              FUN 2 1 .
                FUN 4 28 setof:replace_variables_term
                  VAR 6
                  VAR 1
                  VAR 5
                  VAR 3
                FUN 0 2 []
      FUN 0 2 []
CLS 3 23 setof:concordant_subset
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 3 23 setof:concordant_subset
        FUN 2 1 .
          FUN 2 1 -
            VAR 0
            VAR 1
          VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 4 23 setof:concordant_subset
          VAR 2
          VAR 0
          VAR 5
          VAR 6
        FUN 2 1 .
          FUN 5 23 setof:concordant_subset
            VAR 6
            VAR 0
            FUN 2 1 .
              VAR 1
              VAR 5
            VAR 3
            VAR 4
          FUN 0 2 []
    FUN 0 2 []
CLS 4 23 setof:concordant_subset
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 4 23 setof:concordant_subset
        FUN 2 1 .
          FUN 2 1 -
            VAR 0
            VAR 1
          VAR 2
        VAR 3
        FUN 2 1 .
          VAR 1
          VAR 4
        VAR 5
      FUN 2 1 .
        FUN 2 2 ==
          VAR 0
          VAR 3
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 23 setof:concordant_subset
              VAR 2
              VAR 3
              VAR 4
              VAR 5
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 4 23 setof:concordant_subset
          VAR 0
          VAR 1
          FUN 0 2 []
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 5 23 setof:concordant_subset
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 5 23 setof:concordant_subset
        FUN 0 2 []
        VAR 0
        VAR 1
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 5 23 setof:concordant_subset
          VAR 0
          VAR 1
          VAR 2
          VAR 1
          VAR 2
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 5
          FUN 5 23 setof:concordant_subset
            VAR 0
            VAR 1
            VAR 2
            VAR 3
            VAR 4
          FUN 2 1 .
            FUN 3 23 setof:concordant_subset
              VAR 0
              VAR 3
              VAR 4
            FUN 0 2 []
        FUN 0 2 []
CLS 4 20 setof:free_variables
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 4 20 setof:free_variables
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 2 19 term:term_variables
            VAR 2
            VAR 4
          FUN 2 1 .
            FUN 4 15 setof:get_bound
              VAR 0
              VAR 1
              VAR 4
              VAR 5
            FUN 2 1 .
              FUN 2 19 term:term_variables
                VAR 1
                VAR 6
              FUN 2 1 .
                FUN 3 23 setof:free_variables_in
                  VAR 6
                  VAR 5
                  VAR 3
                FUN 0 2 []
    FUN 0 2 []
CLS 4 15 setof:get_bound
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 4 15 setof:get_bound
        FUN 2 1 ^
          VAR 0
          VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 1
          FUN 2 1 .
            FUN 3 17 term:collect_vars
              VAR 0
              VAR 5
              VAR 3
            FUN 2 1 .
              FUN 4 15 setof:get_bound
                VAR 1
                VAR 2
                VAR 5
                VAR 4
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 4 15 setof:get_bound
          VAR 0
          VAR 0
          VAR 1
          VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 4 29 setof:free_variables_in$3_1_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 29 setof:free_variables_in$3_1_1
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 1 2 \+
          FUN 2 19 setof:var_memberchk
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
        FUN 4 29 setof:free_variables_in$3_1_1
          VAR 0
          VAR 0
          VAR 1
          VAR 2
        FUN 0 2 []
      FUN 0 2 []
CLS 3 23 setof:free_variables_in
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 23 setof:free_variables_in
        FUN 0 2 []
        VAR 0
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 23 setof:free_variables_in
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 4 29 setof:free_variables_in$3_1_1
            VAR 4
            VAR 3
            VAR 2
            VAR 0
          FUN 2 1 .
            FUN 3 23 setof:free_variables_in
              VAR 1
              VAR 2
              VAR 4
            FUN 0 2 []
      FUN 0 2 []
CLS 2 19 setof:var_memberchk
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 19 setof:var_memberchk
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
        FUN 2 19 setof:var_memberchk
          VAR 0
          FUN 2 1 .
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 2 19 setof:var_memberchk
            VAR 0
            VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 4 10 setof:sort
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 4 10 setof:sort
        VAR 0
        VAR 1
        FUN 0 2 []
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 10 setof:sort
          VAR 0
          VAR 1
          FUN 2 1 .
            VAR 2
            FUN 0 2 []
          FUN 2 1 .
            VAR 2
            FUN 0 2 []
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 10
          FUN 4 10 setof:sort
            VAR 0
            VAR 1
            FUN 2 1 .
              VAR 2
              FUN 2 1 .
                VAR 3
                VAR 4
            VAR 5
          FUN 2 1 .
            FUN 4 11 setof:halve
              VAR 4
              FUN 2 1 .
                VAR 3
                VAR 4
              VAR 6
              VAR 7
            FUN 2 1 .
              FUN 4 10 setof:sort
                VAR 0
                VAR 1
                FUN 2 1 .
                  VAR 2
                  VAR 6
                VAR 8
              FUN 2 1 .
                FUN 4 10 setof:sort
                  VAR 0
                  VAR 1
                  VAR 7
                  VAR 9
                FUN 2 1 .
                  FUN 5 11 setof:merge
                    VAR 0
                    VAR 1
                    VAR 8
                    VAR 9
                    VAR 5
                  FUN 0 2 []
        FUN 0 2 []
CLS 4 11 setof:halve
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 4 11 setof:halve
        FUN 2 1 .
          VAR 0
          FUN 2 1 .
            VAR 1
            VAR 2
        FUN 2 1 .
          VAR 3
          VAR 4
        FUN 2 1 .
          VAR 3
          VAR 5
        VAR 6
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 4 11 setof:halve
            VAR 2
            VAR 4
            VAR 5
            VAR 6
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 4 11 setof:halve
          VAR 0
          VAR 1
          FUN 0 2 []
          VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 9 17 setof:merge$5_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 8
      FUN 9 17 setof:merge$5_0_0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
        VAR 5
        VAR 6
        VAR 7
        FUN 0 1 <
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 7
            VAR 6
          FUN 2 1 .
            FUN 5 11 setof:merge
              VAR 5
              VAR 4
              VAR 3
              FUN 2 1 .
                VAR 2
                VAR 1
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 8
        FUN 9 17 setof:merge$5_0_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 5
          VAR 6
          VAR 7
          FUN 0 1 >
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 7
              VAR 2
            FUN 2 1 .
              FUN 5 11 setof:merge
                VAR 5
                VAR 4
                FUN 2 1 .
                  VAR 6
                  VAR 3
                VAR 1
                VAR 0
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 8
          FUN 9 17 setof:merge$5_0_0
            VAR 0
            VAR 1
            VAR 2
            VAR 3
            VAR 4
            VAR 5
            VAR 6
            VAR 7
            FUN 0 1 =
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 7
                VAR 6
              FUN 2 1 .
                FUN 5 11 setof:merge
                  VAR 5
                  VAR 4
                  VAR 3
                  VAR 1
                  VAR 0
                FUN 0 2 []
        FUN 0 2 []
CLS 5 11 setof:merge
  FUN 2 1 .
    FUN 3 6 clause
      INT 9
      FUN 5 11 setof:merge
        VAR 0
        VAR 1
        FUN 2 1 .
          VAR 2
          VAR 3
        FUN 2 1 .
          VAR 4
          VAR 5
        FUN 2 1 .
          VAR 6
          VAR 7
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 5 13 setof:compare
            VAR 0
            VAR 1
            VAR 2
            VAR 4
            VAR 8
          FUN 2 1 .
            FUN 9 17 setof:merge$5_0_0
              VAR 7
              VAR 5
              VAR 4
              VAR 3
              VAR 1
              VAR 0
              VAR 2
              VAR 6
              VAR 8
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 5 11 setof:merge
          VAR 0
          VAR 1
          FUN 0 2 []
          VAR 2
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 5 11 setof:merge
            VAR 0
            VAR 1
            VAR 2
            FUN 0 2 []
            VAR 2
          FUN 0 2 []
        FUN 0 2 []
CLS 5 13 setof:compare
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 5 13 setof:compare
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 4 13 setof:compare
          VAR 0
          VAR 2
          VAR 3
          VAR 5
        FUN 2 1 .
          FUN 3 13 setof:combine
            VAR 1
            VAR 5
            VAR 4
          FUN 0 2 []
    FUN 0 2 []
CLS 4 13 setof:compare
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 13 setof:compare
        INT 0
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 3 12 term:compare
            VAR 2
            VAR 0
            VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 4 13 setof:compare
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 3 3 arg
            VAR 0
            VAR 1
            VAR 4
          FUN 2 1 .
            FUN 3 3 arg
              VAR 0
              VAR 2
              VAR 5
            FUN 2 1 .
              FUN 3 12 term:compare
                VAR 3
                VAR 4
                VAR 5
              FUN 0 2 []
      FUN 0 2 []
CLS 3 13 setof:combine
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 13 setof:combine
        FUN 0 1 <
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 3 13 setof:combine
          FUN 0 2 =<
          FUN 0 1 >
          FUN 0 1 >
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 3 13 setof:combine
            FUN 0 2 =<
            VAR 0
            FUN 0 1 <
          FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 0
            FUN 3 13 setof:combine
              FUN 0 2 >=
              FUN 0 1 <
              FUN 0 1 >
            FUN 2 1 .
              FUN 0 1 !
              FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 1
              FUN 3 13 setof:combine
                FUN 0 2 >=
                VAR 0
                FUN 0 1 <
              FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 0
                FUN 3 13 setof:combine
                  FUN 0 1 >
                  FUN 0 1 <
                  FUN 0 1 >
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 0
                  FUN 3 13 setof:combine
                    FUN 0 1 >
                    FUN 0 1 >
                    FUN 0 1 <
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 0
                    FUN 3 13 setof:combine
                      FUN 0 1 >
                      FUN 0 1 =
                      FUN 0 1 =
                    FUN 0 2 []
                  FUN 0 2 []
CLS 2 13 setof:keysort
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 13 setof:keysort
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 4 10 setof:sort
          INT 1
          FUN 0 2 =<
          VAR 0
          VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 2 10 setof:sort
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 10 setof:sort
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 4 10 setof:sort
          INT 0
          FUN 0 1 <
          VAR 0
          VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 2 1 =
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 1 =
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 0 2 []
CLS 2 2 \=
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 2 \=
        VAR 0
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 0 4 fail
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 2 \=
          VAR 0
          VAR 1
        FUN 0 2 []
      FUN 0 2 []
CLS 3 12 term:compare
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 3 12 term:compare
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 11 term:typeof
          VAR 1
          VAR 3
        FUN 2 1 .
          FUN 2 11 term:typeof
            VAR 2
            VAR 4
          FUN 2 1 .
            FUN 5 18 term:compare_typed
              VAR 3
              VAR 4
              VAR 1
              VAR 2
              VAR 0
            FUN 0 2 []
    FUN 0 2 []
CLS 2 11 term:typeof
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 11 term:typeof
        VAR 0
        INT 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 11 term:typeof
          VAR 0
          INT 1
        FUN 2 1 .
          FUN 1 5 float
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 2 11 term:typeof
            VAR 0
            INT 2
          FUN 2 1 .
            FUN 1 7 integer
              VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 2 11 term:typeof
              VAR 0
              INT 3
            FUN 2 1 .
              FUN 1 4 atom
                VAR 0
              FUN 2 1 .
                FUN 0 1 !
                FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 1
              FUN 2 11 term:typeof
                VAR 0
                INT 4
              FUN 0 2 []
            FUN 0 2 []
CLS 5 18 term:compare_typed
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 5 18 term:compare_typed
        VAR 0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 4 19 term:compare_values
            VAR 0
            VAR 1
            VAR 2
            VAR 3
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 5 18 term:compare_typed
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 3 16 term:compare_num
            VAR 0
            VAR 1
            VAR 4
          FUN 0 2 []
      FUN 0 2 []
CLS 4 19 term:compare_values
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 19 term:compare_values
        INT 0
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 10 $var_index
          VAR 0
          VAR 3
        FUN 2 1 .
          FUN 2 10 $var_index
            VAR 1
            VAR 4
          FUN 2 1 .
            FUN 3 16 term:compare_num
              VAR 3
              VAR 4
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 19 term:compare_values
          INT 1
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 16 term:compare_num
            VAR 0
            VAR 1
            VAR 2
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 4 19 term:compare_values
            INT 2
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 3 16 term:compare_num
              VAR 0
              VAR 1
              VAR 2
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 5
            FUN 4 19 term:compare_values
              INT 3
              VAR 0
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 2 10 atom_codes
                VAR 0
                VAR 3
              FUN 2 1 .
                FUN 2 10 atom_codes
                  VAR 1
                  VAR 4
                FUN 2 1 .
                  FUN 3 20 term:compare_intlist
                    VAR 3
                    VAR 4
                    VAR 2
                  FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 8
              FUN 4 19 term:compare_values
                INT 4
                VAR 0
                VAR 1
                VAR 2
              FUN 2 1 .
                FUN 3 16 term:get_functor
                  VAR 0
                  VAR 3
                  VAR 4
                FUN 2 1 .
                  FUN 3 16 term:get_functor
                    VAR 1
                    VAR 5
                    VAR 6
                  FUN 2 1 .
                    FUN 3 16 term:compare_num
                      VAR 4
                      VAR 6
                      VAR 7
                    FUN 2 1 .
                      FUN 6 18 term:compare_arity
                        VAR 7
                        VAR 3
                        VAR 5
                        VAR 0
                        VAR 1
                        VAR 2
                      FUN 0 2 []
            FUN 0 2 []
CLS 6 18 term:compare_arity
  FUN 2 1 .
    FUN 3 6 clause
      INT 8
      FUN 6 18 term:compare_arity
        FUN 0 1 =
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 10 atom_codes
            VAR 0
            VAR 5
          FUN 2 1 .
            FUN 2 10 atom_codes
              VAR 1
              VAR 6
            FUN 2 1 .
              FUN 3 20 term:compare_intlist
                VAR 5
                VAR 6
                VAR 7
              FUN 2 1 .
                FUN 4 20 term:compare_functor
                  VAR 7
                  VAR 2
                  VAR 3
                  VAR 4
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 6 18 term:compare_arity
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 4 20 term:compare_functor
  FUN 2 1 .
    FUN 3 6 clause
      INT 7
      FUN 4 20 term:compare_functor
        FUN 0 1 =
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 3 =..
            VAR 0
            FUN 2 1 .
              VAR 3
              VAR 4
          FUN 2 1 .
            FUN 2 3 =..
              VAR 1
              FUN 2 1 .
                VAR 5
                VAR 6
            FUN 2 1 .
              FUN 4 17 term:compare_args
                FUN 0 1 =
                VAR 4
                VAR 6
                VAR 2
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 20 term:compare_functor
          VAR 0
          VAR 1
          VAR 2
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 4 17 term:compare_args
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 4 17 term:compare_args
        FUN 0 1 =
        FUN 2 1 .
          VAR 0
          VAR 1
        FUN 2 1 .
          VAR 2
          VAR 3
        VAR 4
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 3 12 term:compare
            VAR 5
            VAR 0
            VAR 2
          FUN 2 1 .
            FUN 4 17 term:compare_args
              VAR 5
              VAR 1
              VAR 3
              VAR 4
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 4 17 term:compare_args
          VAR 0
          VAR 1
          VAR 2
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 3 20 term:compare_intlist
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 20 term:compare_intlist
        FUN 0 2 []
        FUN 0 2 []
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 0
            FUN 0 1 =
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 20 term:compare_intlist
          FUN 0 2 []
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 1
              FUN 0 1 <
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 20 term:compare_intlist
            VAR 0
            FUN 0 2 []
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 1
                FUN 0 1 >
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 4
            FUN 3 20 term:compare_intlist
              FUN 2 1 .
                VAR 0
                VAR 1
              FUN 2 1 .
                VAR 0
                VAR 2
              VAR 3
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 3 20 term:compare_intlist
                  VAR 1
                  VAR 2
                  VAR 3
                FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 5
              FUN 3 20 term:compare_intlist
                FUN 2 1 .
                  VAR 0
                  VAR 1
                FUN 2 1 .
                  VAR 2
                  VAR 3
                VAR 4
              FUN 2 1 .
                FUN 3 16 term:compare_num
                  VAR 0
                  VAR 2
                  VAR 4
                FUN 0 2 []
            FUN 0 2 []
CLS 3 16 term:compare_num
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 16 term:compare_num
        VAR 0
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 1
            FUN 0 1 =
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 16 term:compare_num
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 1 <
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 2
                FUN 0 1 <
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 16 term:compare_num
            VAR 0
            VAR 1
            FUN 0 1 >
          FUN 0 2 []
        FUN 0 2 []
CLS 2 2 ==
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 2 ==
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 3 var
              VAR 1
            FUN 2 1 .
              FUN 2 10 $var_index
                VAR 0
                VAR 2
              FUN 2 1 .
                FUN 2 10 $var_index
                  VAR 1
                  VAR 2
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 2 ==
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 6 atomic
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 6 nonvar
                VAR 1
              FUN 2 1 .
                FUN 2 1 =
                  VAR 0
                  VAR 1
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 2 2 ==
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 1 6 nonvar
              VAR 1
            FUN 2 1 .
              FUN 3 16 term:get_functor
                VAR 0
                VAR 2
                VAR 3
              FUN 2 1 .
                FUN 3 16 term:get_functor
                  VAR 1
                  VAR 2
                  VAR 3
                FUN 2 1 .
                  FUN 3 12 term:args_eq
                    VAR 3
                    VAR 0
                    VAR 1
                  FUN 0 2 []
        FUN 0 2 []
CLS 3 12 term:args_eq
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 12 term:args_eq
        INT 0
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 3 12 term:args_eq
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 4 $arg
            VAR 0
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 3 4 $arg
              VAR 0
              VAR 2
              VAR 4
            FUN 2 1 .
              FUN 2 2 ==
                VAR 3
                VAR 4
              FUN 2 1 .
                FUN 2 2 is
                  VAR 5
                  FUN 2 1 -
                    VAR 0
                    INT 1
                FUN 2 1 .
                  FUN 3 12 term:args_eq
                    VAR 5
                    VAR 1
                    VAR 2
                  FUN 0 2 []
      FUN 0 2 []
CLS 2 3 \==
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 3 \==
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 13 execution:not
          FUN 2 2 ==
            VAR 0
            VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 2 2 @<
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 2 @<
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 3 12 term:compare
          FUN 0 1 <
          VAR 0
          VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 2 3 @=<
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 3 @=<
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 13 execution:not
          FUN 3 12 term:compare
            FUN 0 1 >
            VAR 0
            VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 2 2 @>
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 2 @>
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 3 12 term:compare
          FUN 0 1 >
          VAR 0
          VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 2 3 @>=
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 3 @>=
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 13 execution:not
          FUN 3 12 term:compare
            FUN 0 1 <
            VAR 0
            VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 3 16 term:get_functor
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 16 term:get_functor
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 8 compound
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 12 $get_functor
              VAR 0
              VAR 1
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 3 16 term:get_functor
          VAR 0
          VAR 0
          INT 0
        FUN 0 2 []
      FUN 0 2 []
CLS 3 16 term:set_functor
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 16 term:set_functor
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 12 $set_functor
          VAR 1
          VAR 2
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 16 term:set_functor
          VAR 0
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 6 atomic
            VAR 0
          FUN 2 1 .
            FUN 1 7 integer
              VAR 1
            FUN 2 1 .
              FUN 2 1 =
                VAR 1
                INT 0
              FUN 2 1 .
                FUN 0 1 !
                FUN 0 2 []
      FUN 0 2 []
CLS 3 7 functor
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 7 functor
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 16 term:get_functor
              VAR 0
              VAR 1
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 7 functor
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 16 term:set_functor
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 3 7 functor
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 2 13 err:err_check
              FUN 3 7 functor
                VAR 0
                VAR 1
                VAR 2
              FUN 2 1 .
                FUN 1 4 inst
                  VAR 1
                FUN 2 1 .
                  FUN 1 4 inst
                    VAR 2
                  FUN 2 1 .
                    FUN 1 6 atomic
                      VAR 1
                    FUN 2 1 .
                      FUN 1 7 integer
                        VAR 2
                      FUN 2 1 .
                        FUN 1 6 nonneg
                          VAR 2
                        FUN 2 1 .
                          FUN 1 9 max_arity
                            VAR 2
                          FUN 2 1 .
                            FUN 2 2 if
                              FUN 2 1 >
                                VAR 2
                                INT 0
                              FUN 1 4 atom
                                VAR 1
                            FUN 0 2 []
            FUN 0 2 []
        FUN 0 2 []
CLS 3 3 arg
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 3 arg
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 4 $arg
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 3 arg
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 7 $get_bb
            FUN 0 6 $error
            FUN 0 4 true
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 13 err:err_check
                FUN 3 3 arg
                  VAR 0
                  VAR 1
                  VAR 2
                FUN 2 1 .
                  FUN 1 4 inst
                    VAR 0
                  FUN 2 1 .
                    FUN 1 4 inst
                      VAR 1
                    FUN 2 1 .
                      FUN 1 7 integer
                        VAR 0
                      FUN 2 1 .
                        FUN 1 6 nonneg
                          VAR 0
                        FUN 2 1 .
                          FUN 1 8 compound
                            VAR 1
                          FUN 0 2 []
              FUN 0 2 []
      FUN 0 2 []
CLS 2 15 term:length_det
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 15 term:length_det
        FUN 0 2 []
        INT 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 2 15 term:length_det
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 1
          FUN 2 1 .
            FUN 2 15 term:length_det
              VAR 1
              VAR 3
            FUN 2 1 .
              FUN 2 2 is
                VAR 2
                FUN 2 1 +
                  VAR 3
                  INT 1
              FUN 0 2 []
      FUN 0 2 []
CLS 4 13 term:get_args
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 13 term:get_args
        VAR 0
        VAR 1
        INT 0
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 2
            FUN 0 2 []
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 7
        FUN 4 13 term:get_args
          VAR 0
          VAR 1
          VAR 2
          FUN 2 1 .
            VAR 3
            VAR 4
        FUN 2 1 .
          FUN 3 4 $arg
            VAR 1
            VAR 0
            VAR 3
          FUN 2 1 .
            FUN 2 2 is
              VAR 5
              FUN 2 1 +
                VAR 1
                INT 1
            FUN 2 1 .
              FUN 2 2 is
                VAR 6
                FUN 2 1 -
                  VAR 2
                  INT 1
              FUN 2 1 .
                FUN 4 13 term:get_args
                  VAR 0
                  VAR 5
                  VAR 6
                  VAR 4
                FUN 0 2 []
      FUN 0 2 []
CLS 2 3 =..
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 3 =..
        VAR 0
        FUN 2 1 .
          VAR 1
          VAR 2
      FUN 2 1 .
        FUN 1 6 nonvar
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 16 term:get_functor
              VAR 0
              VAR 1
              VAR 3
            FUN 2 1 .
              FUN 4 13 term:get_args
                VAR 0
                INT 1
                VAR 3
                VAR 2
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 2 3 =..
          VAR 0
          FUN 2 1 .
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 2
          FUN 2 1 .
            FUN 2 15 term:length_det
              VAR 2
              VAR 3
            FUN 2 1 .
              FUN 3 16 term:set_functor
                VAR 0
                VAR 1
                VAR 3
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 4 13 term:get_args
                    VAR 0
                    INT 1
                    VAR 3
                    VAR 2
                  FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 5
          FUN 2 3 =..
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 13 err:err_check
              FUN 2 3 =..
                VAR 0
                VAR 1
              FUN 2 1 .
                FUN 1 4 inst
                  VAR 1
                FUN 2 1 .
                  FUN 1 4 list
                    VAR 1
                  FUN 2 1 .
                    FUN 1 8 instlist
                      VAR 1
                    FUN 2 1 .
                      FUN 1 12 nonemptylist
                        VAR 1
                      FUN 2 1 .
                        FUN 1 2 do
                          FUN 2 1 =
                            VAR 1
                            FUN 2 1 .
                              VAR 2
                              VAR 3
                        FUN 2 1 .
                          FUN 1 4 inst
                            VAR 2
                          FUN 2 1 .
                            FUN 1 2 do
                              FUN 2 12 lists:length
                                VAR 3
                                VAR 4
                            FUN 2 1 .
                              FUN 2 2 if
                                FUN 2 1 =
                                  VAR 4
                                  INT 0
                                FUN 1 6 atomic
                                  VAR 2
                              FUN 2 1 .
                                FUN 2 2 if
                                  FUN 2 1 >
                                    VAR 4
                                    INT 0
                                  FUN 1 4 atom
                                    VAR 2
                                FUN 2 1 .
                                  FUN 1 9 max_arity
                                    VAR 4
                                  FUN 0 2 []
            FUN 0 2 []
        FUN 0 2 []
CLS 2 9 char_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 9 char_code
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 1
        FUN 2 1 .
          FUN 2 11 $atom_codes
            VAR 0
            FUN 2 1 .
              VAR 1
              FUN 0 2 []
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 9 char_code
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 3 var
            VAR 0
          FUN 2 1 .
            FUN 2 11 $atom_codes
              VAR 0
              FUN 2 1 .
                VAR 1
                FUN 0 2 []
            FUN 2 1 .
              FUN 0 1 !
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 2 9 char_code
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 11 $atom_codes
              VAR 0
              FUN 2 1 .
                VAR 2
                FUN 0 2 []
            FUN 2 1 .
              FUN 2 11 $atom_codes
                VAR 3
                FUN 2 1 .
                  VAR 1
                  FUN 0 2 []
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 1 =
                    VAR 0
                    VAR 3
                  FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 2 9 char_code
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 2 13 err:err_check
                FUN 2 9 char_code
                  VAR 0
                  VAR 1
                FUN 2 1 .
                  FUN 1 9 character
                    VAR 0
                  FUN 2 1 .
                    FUN 1 4 inst
                      VAR 1
                    FUN 2 1 .
                      FUN 1 7 integer
                        VAR 1
                      FUN 2 1 .
                        FUN 1 9 char_code
                          VAR 1
                        FUN 0 2 []
              FUN 0 2 []
          FUN 0 2 []
CLS 2 16 term:chars_codes
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 16 term:chars_codes
        FUN 0 2 []
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 2 16 term:chars_codes
          FUN 2 1 .
            VAR 0
            VAR 1
          FUN 2 1 .
            VAR 2
            VAR 3
        FUN 2 1 .
          FUN 2 11 $atom_codes
            VAR 0
            FUN 2 1 .
              VAR 2
              FUN 0 2 []
          FUN 2 1 .
            FUN 2 16 term:chars_codes
              VAR 1
              VAR 3
            FUN 0 2 []
      FUN 0 2 []
CLS 2 19 term:chars_to_codes
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 19 term:chars_to_codes
        FUN 0 2 []
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 2 19 term:chars_to_codes
          FUN 2 1 .
            VAR 0
            VAR 1
          FUN 2 1 .
            VAR 2
            VAR 3
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 1
          FUN 2 1 .
            FUN 2 11 $atom_codes
              VAR 0
              FUN 2 1 .
                VAR 2
                FUN 0 2 []
            FUN 2 1 .
              FUN 2 19 term:chars_to_codes
                VAR 1
                VAR 3
              FUN 0 2 []
      FUN 0 2 []
CLS 2 10 atom_codes
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 10 atom_codes
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 2 11 $atom_codes
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 10 atom_codes
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 4 atom
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 11 $atom_codes
                VAR 0
                VAR 1
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 10 atom_codes
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 13 err:err_check
              FUN 2 10 atom_codes
                VAR 0
                VAR 1
              FUN 2 1 .
                FUN 1 4 atom
                  VAR 0
                FUN 2 1 .
                  FUN 1 4 inst
                    VAR 1
                  FUN 2 1 .
                    FUN 1 4 list
                      VAR 1
                    FUN 2 1 .
                      FUN 1 8 instlist
                        VAR 1
                      FUN 2 1 .
                        FUN 1 16 instlistelements
                          VAR 1
                        FUN 2 1 .
                          FUN 1 14 char_code_list
                            VAR 1
                          FUN 0 2 []
            FUN 0 2 []
        FUN 0 2 []
CLS 2 10 atom_chars
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 10 atom_chars
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 1 6 nonvar
            VAR 1
          FUN 2 1 .
            FUN 2 19 term:chars_to_codes
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 11 $atom_codes
                  VAR 0
                  VAR 2
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 10 atom_chars
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 4 atom
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 11 $atom_codes
                VAR 0
                VAR 2
              FUN 2 1 .
                FUN 2 16 term:chars_codes
                  VAR 1
                  VAR 2
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 10 atom_chars
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 13 err:err_check
              FUN 2 10 atom_chars
                VAR 0
                VAR 1
              FUN 2 1 .
                FUN 1 4 atom
                  VAR 0
                FUN 2 1 .
                  FUN 1 4 inst
                    VAR 1
                  FUN 2 1 .
                    FUN 1 4 list
                      VAR 1
                    FUN 2 1 .
                      FUN 1 8 instlist
                        VAR 1
                      FUN 2 1 .
                        FUN 1 16 instlistelements
                          VAR 1
                        FUN 2 1 .
                          FUN 1 9 char_list
                            VAR 1
                          FUN 0 2 []
            FUN 0 2 []
        FUN 0 2 []
CLS 3 18 number_codes$2_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 18 number_codes$2_0_0
        VAR 0
        VAR 1
        FUN 0 3 yes
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 17 term:parse_number
            VAR 1
            VAR 0
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 18 number_codes$2_0_0
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 13 $number_codes
            VAR 0
            VAR 1
          FUN 0 2 []
      FUN 0 2 []
CLS 2 18 number_codes$2_0_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 18 number_codes$2_0_2
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 6 number
          VAR 1
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 18 number_codes$2_0_2
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 3 var
            VAR 1
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              FUN 0 3 yes
            FUN 0 2 []
      FUN 0 2 []
CLS 2 12 number_codes
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 12 number_codes
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 16 term:check_codes
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 18 number_codes$2_0_2
            VAR 2
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 18 number_codes$2_0_0
                VAR 0
                VAR 1
                VAR 2
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 12 number_codes
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 12 number_codes
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 6 number
                VAR 0
              FUN 2 1 .
                FUN 1 4 inst
                  VAR 1
                FUN 2 1 .
                  FUN 1 4 list
                    VAR 1
                  FUN 2 1 .
                    FUN 1 8 instlist
                      VAR 1
                    FUN 2 1 .
                      FUN 1 16 instlistelements
                        VAR 1
                      FUN 2 1 .
                        FUN 1 14 char_code_list
                          VAR 1
                        FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 4 18 number_chars$2_0_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 4 18 number_chars$2_0_0
        VAR 0
        VAR 1
        VAR 2
        FUN 0 3 yes
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 17 term:parse_number
            VAR 2
            VAR 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 18 number_chars$2_0_0
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 13 $number_codes
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 2 16 term:chars_codes
              VAR 0
              VAR 2
            FUN 0 2 []
      FUN 0 2 []
CLS 2 18 number_chars$2_0_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 18 number_chars$2_0_2
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 6 number
          VAR 1
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 18 number_chars$2_0_2
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 3 var
            VAR 1
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              FUN 0 3 yes
            FUN 0 2 []
      FUN 0 2 []
CLS 2 12 number_chars
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 12 number_chars
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 3 16 term:check_chars
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 2 18 number_chars$2_0_2
            VAR 3
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 4 18 number_chars$2_0_0
                VAR 1
                VAR 0
                VAR 2
                VAR 3
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 12 number_chars
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 12 number_chars
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 6 number
                VAR 0
              FUN 2 1 .
                FUN 1 4 inst
                  VAR 1
                FUN 2 1 .
                  FUN 1 4 list
                    VAR 1
                  FUN 2 1 .
                    FUN 1 8 instlist
                      VAR 1
                    FUN 2 1 .
                      FUN 1 16 instlistelements
                        VAR 1
                      FUN 2 1 .
                        FUN 1 9 char_list
                          VAR 1
                        FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 2 16 term:check_codes
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 16 term:check_codes
        VAR 0
        FUN 0 2 no
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 2 16 term:check_codes
          FUN 0 2 []
          FUN 0 3 yes
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 2 16 term:check_codes
            FUN 2 1 .
              VAR 0
              VAR 1
            FUN 0 2 no
          FUN 2 1 .
            FUN 1 3 var
              VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 16 term:check_codes
                  VAR 1
                  VAR 2
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 3
            FUN 2 16 term:check_codes
              FUN 2 1 .
                VAR 0
                VAR 1
              VAR 2
            FUN 2 1 .
              FUN 1 7 integer
                VAR 0
              FUN 2 1 .
                FUN 2 1 >
                  VAR 0
                  INT 0
                FUN 2 1 .
                  FUN 2 1 <
                    VAR 0
                    INT 256
                  FUN 2 1 .
                    FUN 2 16 term:check_codes
                      VAR 1
                      VAR 2
                    FUN 0 2 []
          FUN 0 2 []
CLS 3 16 term:check_chars
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 16 term:check_chars
        VAR 0
        VAR 1
        FUN 0 2 no
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 3 16 term:check_chars
          FUN 0 2 []
          FUN 0 2 []
          FUN 0 3 yes
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 5
          FUN 3 16 term:check_chars
            FUN 2 1 .
              VAR 0
              VAR 1
            VAR 2
            FUN 0 2 no
          FUN 2 1 .
            FUN 1 3 var
              VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 3 16 term:check_chars
                  VAR 1
                  VAR 3
                  VAR 4
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 5
            FUN 3 16 term:check_chars
              FUN 2 1 .
                VAR 0
                VAR 1
              FUN 2 1 .
                VAR 2
                VAR 3
              VAR 4
            FUN 2 1 .
              FUN 2 11 $atom_codes
                VAR 0
                FUN 2 1 .
                  VAR 2
                  FUN 0 2 []
              FUN 2 1 .
                FUN 3 16 term:check_chars
                  VAR 1
                  VAR 3
                  VAR 4
                FUN 0 2 []
          FUN 0 2 []
CLS 2 17 term:parse_number
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 17 term:parse_number
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 21 term:try_parse_number
          VAR 0
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 1
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 2 17 term:parse_number
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 10 atom_codes
            VAR 2
            VAR 0
          FUN 2 1 .
            FUN 3 11 atom_concat
              FUN 0 12 Bad number: 
              VAR 2
              VAR 3
            FUN 2 1 .
              FUN 1 5 throw
                FUN 2 5 error
                  FUN 1 12 syntax_error
                    VAR 3
                  FUN 2 12 number_codes
                    VAR 1
                    VAR 0
              FUN 0 2 []
      FUN 0 2 []
CLS 2 27 term:try_parse_number$2_1_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 27 term:try_parse_number$2_1_0
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 2 >=
          VAR 1
          INT 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 2 is
              VAR 0
              FUN 1 1 -
                VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 27 term:try_parse_number$2_1_0
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 2 is
            VAR 1
            FUN 2 2 <<
              INT 1
              INT 30
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 1 =
                VAR 0
                VAR 1
              FUN 0 2 []
      FUN 0 2 []
CLS 2 21 term:try_parse_number
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 21 term:try_parse_number
        FUN 2 1 .
          VAR 0
          VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 14 term:is_layout
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 21 term:try_parse_number
              VAR 1
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 21 term:try_parse_number
          FUN 2 1 .
            INT 45
            VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 21 term:parse_poz_number
              VAR 0
              VAR 2
            FUN 2 1 .
              FUN 2 27 term:try_parse_number$2_1_0
                VAR 1
                VAR 2
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 21 term:try_parse_number
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 21 term:parse_poz_number
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 2 2 >=
                VAR 1
                INT 0
              FUN 0 2 []
        FUN 0 2 []
CLS 2 21 term:parse_poz_number
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 21 term:parse_poz_number
        FUN 2 1 .
          VAR 0
          VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 14 term:is_layout
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 21 term:parse_poz_number
              VAR 1
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 21 term:parse_poz_number
          FUN 2 1 .
            INT 48
            FUN 2 1 .
              VAR 0
              VAR 1
          VAR 2
        FUN 2 1 .
          FUN 2 12 lists:member
            VAR 0
            FUN 2 1 .
              INT 39
              FUN 2 1 .
                INT 98
                FUN 2 1 .
                  INT 111
                  FUN 2 1 .
                    INT 120
                    FUN 0 2 []
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 3 25 term:parse_special_number
                VAR 0
                VAR 1
                VAR 2
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 2 21 term:parse_poz_number
            FUN 2 1 .
              VAR 0
              VAR 1
            VAR 2
          FUN 2 1 .
            FUN 3 16 term:digit_based
              INT 10
              VAR 0
              VAR 3
            FUN 2 1 .
              FUN 2 13 $number_codes
                VAR 2
                FUN 2 1 .
                  VAR 0
                  VAR 1
              FUN 0 2 []
        FUN 0 2 []
CLS 3 25 term:parse_special_number
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 25 term:parse_special_number
        INT 39
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 22 term:parse_quoted_char
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 2 >=
            VAR 1
            INT 0
          FUN 2 1 .
            FUN 2 1 <
              VAR 1
              INT 256
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 25 term:parse_special_number
          INT 98
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 3 23 term:parse_based_number
            INT 2
            VAR 0
            VAR 1
          FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 25 term:parse_special_number
            INT 111
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 3 23 term:parse_based_number
              INT 8
              VAR 0
              VAR 1
            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 3 25 term:parse_special_number
              INT 120
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 3 23 term:parse_based_number
                INT 16
                VAR 0
                VAR 1
              FUN 0 2 []
          FUN 0 2 []
CLS 2 22 term:parse_quoted_char
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 22 term:parse_quoted_char
        FUN 2 1 .
          INT 39
          VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 2 1 =
            VAR 0
            FUN 2 1 .
              INT 39
              FUN 0 2 []
          FUN 2 1 .
            FUN 2 1 =
              VAR 1
              INT 39
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 2 22 term:parse_quoted_char
          FUN 2 1 .
            INT 92
            VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              FUN 2 1 .
                VAR 2
                VAR 3
            FUN 2 1 .
              FUN 3 17 term:parse_escape
                VAR 2
                VAR 3
                VAR 1
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 2 22 term:parse_quoted_char
            FUN 2 1 .
              VAR 0
              FUN 0 2 []
            VAR 0
          FUN 0 2 []
        FUN 0 2 []
CLS 3 17 term:parse_escape
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 3 17 term:parse_escape
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 16 term:digit_based
          INT 8
          VAR 0
          VAR 3
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 5 16 term:parse_based
              VAR 1
              INT 8
              VAR 3
              VAR 2
              FUN 2 1 .
                INT 92
                FUN 0 2 []
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 3 17 term:parse_escape
          INT 120
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 3 16 term:digit_based
              INT 16
              VAR 0
              VAR 3
            FUN 2 1 .
              FUN 5 16 term:parse_based
                VAR 1
                INT 16
                VAR 3
                VAR 2
                FUN 2 1 .
                  INT 92
                  FUN 0 2 []
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 17 term:parse_escape
            VAR 0
            FUN 0 2 []
            VAR 1
          FUN 2 1 .
            FUN 2 17 term:escape_equiv
              VAR 0
              VAR 1
            FUN 0 2 []
        FUN 0 2 []
CLS 2 17 term:escape_equiv
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 2 17 term:escape_equiv
        INT 97
        INT 7
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 0
        FUN 2 17 term:escape_equiv
          INT 98
          INT 8
        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 0
          FUN 2 17 term:escape_equiv
            INT 114
            INT 13
          FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 0
            FUN 2 17 term:escape_equiv
              INT 102
              INT 12
            FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 0
              FUN 2 17 term:escape_equiv
                INT 116
                INT 9
              FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 0
                FUN 2 17 term:escape_equiv
                  INT 110
                  INT 10
                FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 0
                  FUN 2 17 term:escape_equiv
                    INT 118
                    INT 11
                  FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 0
                    FUN 2 17 term:escape_equiv
                      INT 92
                      INT 92
                    FUN 0 2 []
                  FUN 2 1 .
                    FUN 3 6 clause
                      INT 0
                      FUN 2 17 term:escape_equiv
                        INT 39
                        INT 39
                      FUN 0 2 []
                    FUN 2 1 .
                      FUN 3 6 clause
                        INT 0
                        FUN 2 17 term:escape_equiv
                          INT 34
                          INT 34
                        FUN 0 2 []
                      FUN 2 1 .
                        FUN 3 6 clause
                          INT 0
                          FUN 2 17 term:escape_equiv
                            INT 96
                            INT 96
                          FUN 0 2 []
                        FUN 0 2 []
CLS 3 23 term:parse_based_number
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 3 23 term:parse_based_number
        VAR 0
        FUN 2 1 .
          VAR 1
          VAR 2
        VAR 3
      FUN 2 1 .
        FUN 3 16 term:digit_based
          VAR 0
          VAR 1
          VAR 4
        FUN 2 1 .
          FUN 5 16 term:parse_based
            VAR 2
            VAR 0
            VAR 4
            VAR 3
            FUN 0 2 []
          FUN 0 2 []
    FUN 0 2 []
CLS 5 16 term:parse_based
  FUN 2 1 .
    FUN 3 6 clause
      INT 8
      FUN 5 16 term:parse_based
        FUN 2 1 .
          VAR 0
          VAR 1
        VAR 2
        VAR 3
        VAR 4
        VAR 5
      FUN 2 1 .
        FUN 3 16 term:digit_based
          VAR 2
          VAR 0
          VAR 6
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 20 term:add_digit_based
              VAR 2
              VAR 3
              VAR 6
              VAR 7
            FUN 2 1 .
              FUN 5 16 term:parse_based
                VAR 1
                VAR 2
                VAR 7
                VAR 4
                VAR 5
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 5 16 term:parse_based
          VAR 0
          VAR 1
          VAR 2
          VAR 2
          VAR 0
        FUN 0 2 []
      FUN 0 2 []
CLS 4 20 term:add_digit_based
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 4 20 term:add_digit_based
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 3 5 catch
          FUN 2 2 is
            VAR 3
            FUN 2 1 +
              FUN 2 1 *
                VAR 1
                VAR 0
              VAR 2
          FUN 2 5 error
            FUN 1 16 evaluation_error
              FUN 0 12 int_overflow
            VAR 4
          FUN 0 4 fail
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 4 20 term:add_digit_based
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 3 5 catch
            FUN 2 2 is
              VAR 3
              FUN 2 1 -
                FUN 2 1 *
                  FUN 1 1 -
                    VAR 1
                  VAR 0
                VAR 2
            FUN 2 5 error
              FUN 1 16 evaluation_error
                FUN 0 12 int_overflow
              VAR 4
            FUN 0 4 fail
          FUN 0 2 []
      FUN 0 2 []
CLS 3 16 term:digit_based
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 16 term:digit_based
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 2 2 >=
          VAR 1
          INT 48
        FUN 2 1 .
          FUN 2 1 <
            VAR 1
            FUN 2 1 +
              INT 48
              VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 2 is
                VAR 2
                FUN 2 1 -
                  VAR 1
                  INT 48
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 3 16 term:digit_based
          INT 16
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 2 >=
            VAR 0
            INT 97
          FUN 2 1 .
            FUN 2 2 =<
              VAR 0
              INT 102
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 2 is
                  VAR 1
                  FUN 2 1 +
                    FUN 2 1 -
                      VAR 0
                      INT 97
                    INT 10
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 16 term:digit_based
            INT 16
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 2 2 >=
              VAR 0
              INT 65
            FUN 2 1 .
              FUN 2 2 =<
                VAR 0
                INT 70
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 2 is
                    VAR 1
                    FUN 2 1 +
                      FUN 2 1 -
                        VAR 0
                        INT 65
                      INT 10
                  FUN 0 2 []
        FUN 0 2 []
CLS 1 14 term:is_layout
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 14 term:is_layout
        VAR 0
      FUN 2 1 .
        FUN 2 2 =<
          VAR 0
          INT 32
        FUN 0 2 []
    FUN 0 2 []
CLS 1 17 atom_length$2_0_3
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 17 atom_length$2_0_3
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 17 atom_length$2_0_3
          VAR 0
        FUN 2 1 .
          FUN 1 7 integer
            VAR 0
          FUN 2 1 .
            FUN 2 2 >=
              VAR 0
              INT 0
            FUN 0 2 []
      FUN 0 2 []
CLS 2 11 atom_length
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 11 atom_length
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 4 atom
          VAR 0
        FUN 2 1 .
          FUN 1 17 atom_length$2_0_3
            VAR 1
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 11 $atom_codes
                VAR 0
                VAR 2
              FUN 2 1 .
                FUN 2 12 lists:length
                  VAR 2
                  VAR 1
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 11 atom_length
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 2 11 atom_length
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 4 atom
                  VAR 0
                FUN 2 1 .
                  FUN 1 7 integer
                    VAR 1
                  FUN 2 1 .
                    FUN 1 6 nonneg
                      VAR 1
                    FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 17 atom_concat$3_0_5
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 17 atom_concat$3_0_5
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 17 atom_concat$3_0_5
          VAR 0
        FUN 2 1 .
          FUN 1 4 atom
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 1 17 atom_concat$3_0_6
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 17 atom_concat$3_0_6
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 17 atom_concat$3_0_6
          VAR 0
        FUN 2 1 .
          FUN 1 4 atom
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 3 11 atom_concat
  FUN 2 1 .
    FUN 3 6 clause
      INT 6
      FUN 3 11 atom_concat
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 4 atom
          VAR 2
        FUN 2 1 .
          FUN 1 17 atom_concat$3_0_6
            VAR 0
          FUN 2 1 .
            FUN 1 17 atom_concat$3_0_5
              VAR 1
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 10 atom_codes
                  VAR 2
                  VAR 3
                FUN 2 1 .
                  FUN 3 12 lists:append
                    VAR 4
                    VAR 5
                    VAR 3
                  FUN 2 1 .
                    FUN 2 10 atom_codes
                      VAR 0
                      VAR 4
                    FUN 2 1 .
                      FUN 2 10 atom_codes
                        VAR 1
                        VAR 5
                      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 3 11 atom_concat
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 1 3 var
            VAR 2
          FUN 2 1 .
            FUN 1 4 atom
              VAR 0
            FUN 2 1 .
              FUN 1 4 atom
                VAR 1
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 10 atom_codes
                    VAR 0
                    VAR 3
                  FUN 2 1 .
                    FUN 2 10 atom_codes
                      VAR 1
                      VAR 4
                    FUN 2 1 .
                      FUN 3 12 lists:append
                        VAR 3
                        VAR 4
                        VAR 5
                      FUN 2 1 .
                        FUN 2 10 atom_codes
                          VAR 2
                          VAR 5
                        FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 3 11 atom_concat
            VAR 0
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 2 13 err:err_check
              FUN 3 11 atom_concat
                VAR 0
                VAR 1
                VAR 2
              FUN 2 1 .
                FUN 1 4 atom
                  VAR 2
                FUN 2 1 .
                  FUN 1 4 atom
                    VAR 0
                  FUN 2 1 .
                    FUN 1 4 atom
                      VAR 1
                    FUN 2 1 .
                      FUN 2 2 if
                        FUN 1 3 var
                          VAR 2
                        FUN 1 4 inst
                          VAR 0
                      FUN 2 1 .
                        FUN 2 2 if
                          FUN 1 3 var
                            VAR 2
                          FUN 1 4 inst
                            VAR 1
                        FUN 0 2 []
            FUN 0 2 []
        FUN 0 2 []
CLS 1 15 sub_atom$5_0_11
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 15 sub_atom$5_0_11
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 15 sub_atom$5_0_11
          VAR 0
        FUN 2 1 .
          FUN 1 4 atom
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 5 8 sub_atom
  FUN 2 1 .
    FUN 3 6 clause
      INT 10
      FUN 5 8 sub_atom
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 1 4 atom
          VAR 0
        FUN 2 1 .
          FUN 1 15 sub_atom$5_0_11
            VAR 4
          FUN 2 1 .
            FUN 1 14 term:is_length
              VAR 1
            FUN 2 1 .
              FUN 1 14 term:is_length
                VAR 2
              FUN 2 1 .
                FUN 1 14 term:is_length
                  VAR 3
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 2 10 atom_codes
                      VAR 0
                      VAR 5
                    FUN 2 1 .
                      FUN 3 12 lists:append
                        VAR 6
                        VAR 7
                        VAR 5
                      FUN 2 1 .
                        FUN 3 12 lists:append
                          VAR 8
                          VAR 9
                          VAR 7
                        FUN 2 1 .
                          FUN 2 12 lists:length
                            VAR 6
                            VAR 1
                          FUN 2 1 .
                            FUN 2 12 lists:length
                              VAR 8
                              VAR 2
                            FUN 2 1 .
                              FUN 2 12 lists:length
                                VAR 9
                                VAR 3
                              FUN 2 1 .
                                FUN 2 10 atom_codes
                                  VAR 4
                                  VAR 8
                                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 5 8 sub_atom
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 2 13 err:err_check
            FUN 5 8 sub_atom
              VAR 0
              VAR 1
              VAR 2
              VAR 3
              VAR 4
            FUN 2 1 .
              FUN 1 4 inst
                VAR 0
              FUN 2 1 .
                FUN 1 4 atom
                  VAR 0
                FUN 2 1 .
                  FUN 1 7 integer
                    VAR 1
                  FUN 2 1 .
                    FUN 1 6 nonneg
                      VAR 1
                    FUN 2 1 .
                      FUN 1 7 integer
                        VAR 2
                      FUN 2 1 .
                        FUN 1 6 nonneg
                          VAR 2
                        FUN 2 1 .
                          FUN 1 7 integer
                            VAR 3
                          FUN 2 1 .
                            FUN 1 6 nonneg
                              VAR 3
                            FUN 2 1 .
                              FUN 1 4 atom
                                VAR 4
                              FUN 0 2 []
          FUN 0 2 []
      FUN 0 2 []
CLS 1 14 term:is_length
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 14 term:is_length
        VAR 0
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 14 term:is_length
          VAR 0
        FUN 2 1 .
          FUN 1 7 integer
            VAR 0
          FUN 2 1 .
            FUN 2 2 >=
              VAR 0
              INT 0
            FUN 0 2 []
      FUN 0 2 []
CLS 2 17 term:occurs_check
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 17 term:occurs_check
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 3 \==
              VAR 0
              VAR 1
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 2 17 term:occurs_check
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 3 16 term:get_functor
            VAR 0
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 3 17 term:occurs_check
              VAR 3
              VAR 0
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 3 17 term:occurs_check
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 17 term:occurs_check
        INT 0
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 17 term:occurs_check
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 4 $arg
            VAR 0
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 2 17 term:occurs_check
              VAR 3
              VAR 2
            FUN 2 1 .
              FUN 2 2 is
                VAR 4
                FUN 2 1 -
                  VAR 0
                  INT 1
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 3 17 term:occurs_check
                    VAR 4
                    VAR 1
                    VAR 2
                  FUN 0 2 []
      FUN 0 2 []
CLS 2 23 unify_with_occurs_check
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 23 unify_with_occurs_check
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 1 3 var
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
        INT 2
        FUN 2 23 unify_with_occurs_check
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 1 3 var
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 2 17 term:occurs_check
                VAR 1
                VAR 0
              FUN 2 1 .
                FUN 2 1 =
                  VAR 0
                  VAR 1
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 2 23 unify_with_occurs_check
            VAR 0
            VAR 1
          FUN 2 1 .
            FUN 1 3 var
              VAR 1
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 17 term:occurs_check
                  VAR 0
                  VAR 1
                FUN 2 1 .
                  FUN 2 1 =
                    VAR 0
                    VAR 1
                  FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 2 23 unify_with_occurs_check
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 6 atomic
                VAR 0
              FUN 2 1 .
                FUN 0 1 !
                FUN 2 1 .
                  FUN 2 1 =
                    VAR 0
                    VAR 1
                  FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 4
              FUN 2 23 unify_with_occurs_check
                VAR 0
                VAR 1
              FUN 2 1 .
                FUN 3 16 term:get_functor
                  VAR 0
                  VAR 2
                  VAR 3
                FUN 2 1 .
                  FUN 3 16 term:get_functor
                    VAR 1
                    VAR 2
                    VAR 3
                  FUN 2 1 .
                    FUN 3 28 term:unify_with_occurs_check
                      VAR 3
                      VAR 0
                      VAR 1
                    FUN 0 2 []
            FUN 0 2 []
CLS 3 28 term:unify_with_occurs_check
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 3 28 term:unify_with_occurs_check
        INT 0
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 6
        FUN 3 28 term:unify_with_occurs_check
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 4 $arg
            VAR 0
            VAR 1
            VAR 3
          FUN 2 1 .
            FUN 3 4 $arg
              VAR 0
              VAR 2
              VAR 4
            FUN 2 1 .
              FUN 2 23 unify_with_occurs_check
                VAR 3
                VAR 4
              FUN 2 1 .
                FUN 2 2 is
                  VAR 5
                  FUN 2 1 -
                    VAR 0
                    INT 1
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 3 28 term:unify_with_occurs_check
                      VAR 5
                      VAR 1
                      VAR 2
                    FUN 0 2 []
      FUN 0 2 []
CLS 5 19 term:numbervars_arg
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 5 19 term:numbervars_arg
        INT 0
        VAR 0
        VAR 1
        VAR 2
        VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 9
        FUN 5 19 term:numbervars_arg
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 3 4 $arg
            VAR 1
            VAR 2
            VAR 5
          FUN 2 1 .
            FUN 3 15 term:numbervars
              VAR 5
              VAR 3
              VAR 6
            FUN 2 1 .
              FUN 2 2 is
                VAR 7
                FUN 2 1 +
                  VAR 1
                  INT 1
              FUN 2 1 .
                FUN 2 2 is
                  VAR 8
                  FUN 2 1 -
                    VAR 0
                    INT 1
                FUN 2 1 .
                  FUN 5 19 term:numbervars_arg
                    VAR 8
                    VAR 7
                    VAR 2
                    VAR 6
                    VAR 4
                  FUN 0 2 []
      FUN 0 2 []
CLS 3 15 term:numbervars
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 15 term:numbervars
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
              VAR 0
              FUN 1 4 $VAR
                VAR 1
            FUN 2 1 .
              FUN 2 2 is
                VAR 2
                FUN 2 1 +
                  VAR 1
                  INT 1
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 15 term:numbervars
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 16 term:get_functor
            VAR 0
            VAR 3
            VAR 4
          FUN 2 1 .
            FUN 5 19 term:numbervars_arg
              VAR 4
              INT 1
              VAR 0
              VAR 1
              VAR 2
            FUN 0 2 []
      FUN 0 2 []
CLS 2 16 term:expand_term
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 16 term:expand_term
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
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
        FUN 2 16 term:expand_term
          FUN 2 3 -->
            VAR 0
            VAR 1
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 12 dcg:dcg rule
              FUN 2 3 -->
                VAR 0
                VAR 1
              VAR 2
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 2 16 term:expand_term
            VAR 0
            VAR 0
          FUN 0 2 []
        FUN 0 2 []
CLS 2 19 term:term_variables
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 19 term:term_variables
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 3 17 term:collect_vars
          VAR 0
          VAR 1
          FUN 0 2 []
        FUN 0 2 []
    FUN 0 2 []
CLS 3 17 term:collect_vars
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 17 term:collect_vars
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 1 13 execution:not
            FUN 2 18 term:var_memberchk
              VAR 0
              VAR 2
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
        FUN 3 17 term:collect_vars
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
                FUN 3 18 term:collect_vars0
                  VAR 4
                  VAR 1
                  VAR 2
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 3 17 term:collect_vars
            VAR 0
            VAR 1
            VAR 1
          FUN 0 2 []
        FUN 0 2 []
CLS 3 18 term:collect_vars0
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 3 18 term:collect_vars0
        FUN 0 2 []
        VAR 0
        VAR 0
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 3 18 term:collect_vars0
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 3 17 term:collect_vars
            VAR 0
            VAR 4
            VAR 3
          FUN 2 1 .
            FUN 3 18 term:collect_vars0
              VAR 1
              VAR 2
              VAR 4
            FUN 0 2 []
      FUN 0 2 []
CLS 2 18 term:var_memberchk
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 18 term:var_memberchk
        VAR 0
        FUN 2 1 .
          VAR 1
          VAR 2
      FUN 2 1 .
        FUN 2 2 ==
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 18 term:var_memberchk
          VAR 0
          FUN 2 1 .
            VAR 1
            VAR 2
        FUN 2 1 .
          FUN 2 18 term:var_memberchk
            VAR 0
            VAR 2
          FUN 0 2 []
      FUN 0 2 []
CLS 2 17 term:subsumes_chk
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 17 term:subsumes_chk
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 2 \+
          FUN 2 1 ,
            FUN 3 15 term:numbervars
              VAR 1
              INT 0
              VAR 2
            FUN 1 2 \+
              FUN 2 1 =
                VAR 0
                VAR 1
        FUN 0 2 []
    FUN 0 2 []
CLS 2 12 term:variant
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 12 term:variant
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 17 term:subsumes_chk
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 17 term:subsumes_chk
            VAR 1
            VAR 0
          FUN 0 2 []
    FUN 0 2 []
CLS 1 14 write:put_code
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 14 write:put_code
        VAR 0
      FUN 2 1 .
        FUN 1 9 $put_code
          VAR 0
        FUN 0 2 []
    FUN 0 2 []
CLS 2 17 write:write_style
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 2 17 write:write_style
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 5 15 write:write_out
          VAR 0
          VAR 1
          INT 1200
          FUN 0 5 punct
          VAR 2
        FUN 0 2 []
    FUN 0 2 []
CLS 5 17 write:maybe_paren
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 5 17 write:maybe_paren
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        FUN 0 5 punct
      FUN 2 1 .
        FUN 2 1 >
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 14 write:put_code
              VAR 2
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 5 17 write:maybe_paren
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 3
        FUN 0 2 []
      FUN 0 2 []
CLS 2 17 write:maybe_space
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 17 write:maybe_space
        FUN 0 5 punct
        VAR 0
      FUN 2 1 .
        FUN 0 1 !
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 17 write:maybe_space
          VAR 0
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 14 write:put_code
              INT 32
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 0
          FUN 2 17 write:maybe_space
            FUN 0 5 quote
            FUN 0 5 alpha
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 14 write:put_code
                INT 32
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 2 17 write:maybe_space
              VAR 0
              VAR 1
            FUN 0 2 []
          FUN 0 2 []
CLS 1 16 write:put_string
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 16 write:put_string
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 16 write:put_string
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 14 write:put_code
            VAR 0
          FUN 2 1 .
            FUN 1 16 write:put_string
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 2 16 write:put_string
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 2 16 write:put_string
        FUN 0 2 []
        VAR 0
      FUN 2 1 .
        FUN 1 14 write:put_code
          VAR 0
        FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 16 write:put_string
          FUN 2 1 .
            VAR 0
            VAR 1
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 14 write:put_code
              VAR 0
            FUN 2 1 .
              FUN 1 14 write:put_code
                VAR 0
              FUN 2 1 .
                FUN 2 16 write:put_string
                  VAR 1
                  VAR 0
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 2 16 write:put_string
            FUN 2 1 .
              VAR 0
              VAR 1
            VAR 2
          FUN 2 1 .
            FUN 1 14 write:put_code
              VAR 0
            FUN 2 1 .
              FUN 2 16 write:put_string
                VAR 1
                VAR 2
              FUN 0 2 []
        FUN 0 2 []
CLS 1 20 write:write_variable
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 1 20 write:write_variable
        VAR 0
      FUN 2 1 .
        FUN 2 10 $var_index
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 12 number_codes
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 1 14 write:put_code
              INT 95
            FUN 2 1 .
              FUN 1 16 write:put_string
                VAR 2
              FUN 0 2 []
    FUN 0 2 []
CLS 3 21 write:write_out$5_1_0
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 21 write:write_out$5_1_0
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 1 4 atom
          VAR 2
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 16 write:write_atom
              VAR 2
              FUN 3 5 style
                FUN 0 5 false
                FUN 0 5 false
                FUN 0 5 false
              VAR 1
              VAR 0
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 21 write:write_out$5_1_0
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 15 write:write_VAR
            VAR 2
            VAR 1
            VAR 0
          FUN 0 2 []
      FUN 0 2 []
CLS 2 21 write:write_out$5_1_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 21 write:write_out$5_1_2
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 7 integer
          VAR 1
        FUN 2 1 .
          FUN 2 2 >=
            VAR 1
            INT 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 0 4 true
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 21 write:write_out$5_1_2
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 2 ==
            VAR 0
            FUN 0 4 atom
          FUN 2 1 .
            FUN 1 4 atom
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 2 21 write:write_out$5_2_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 21 write:write_out$5_2_2
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 2 1 <
          VAR 1
          INT 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 17 write:maybe_space
              VAR 0
              FUN 0 5 other
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 21 write:write_out$5_2_2
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 17 write:maybe_space
            VAR 0
            FUN 0 5 alpha
          FUN 0 2 []
      FUN 0 2 []
CLS 2 21 write:write_out$5_3_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 2 21 write:write_out$5_3_1
        VAR 0
        FUN 2 1 ,
          FUN 0 4 true
          FUN 2 1 ,
            VAR 1
            VAR 2
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 4 16 write:write_atom
            VAR 0
            FUN 2 1 ,
              FUN 0 4 true
              FUN 2 1 ,
                VAR 1
                VAR 2
            FUN 0 5 punct
            VAR 3
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 2 21 write:write_out$5_3_1
          VAR 0
          VAR 1
        FUN 2 1 .
          FUN 2 10 atom_codes
            VAR 0
            VAR 2
          FUN 2 1 .
            FUN 1 16 write:put_string
              VAR 2
            FUN 0 2 []
      FUN 0 2 []
CLS 5 15 write:write_out
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 5 15 write:write_out
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        FUN 0 5 alpha
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 2 17 write:maybe_space
              VAR 3
              FUN 0 5 alpha
            FUN 2 1 .
              FUN 1 20 write:write_variable
                VAR 0
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 7
        FUN 5 15 write:write_out
          FUN 1 4 $VAR
            VAR 0
          FUN 3 5 style
            VAR 1
            VAR 2
            VAR 3
          VAR 4
          VAR 5
          VAR 6
        FUN 2 1 .
          FUN 2 3 \==
            VAR 3
            FUN 0 5 false
          FUN 2 1 .
            FUN 2 21 write:write_out$5_1_2
              VAR 3
              VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 3 21 write:write_out$5_1_0
                  VAR 6
                  VAR 5
                  VAR 0
                FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 5
          FUN 5 15 write:write_out
            VAR 0
            VAR 1
            VAR 2
            VAR 3
            FUN 0 5 alpha
          FUN 2 1 .
            FUN 1 6 number
              VAR 0
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 2 21 write:write_out$5_2_2
                  VAR 3
                  VAR 0
                FUN 2 1 .
                  FUN 2 12 number_codes
                    VAR 0
                    VAR 4
                  FUN 2 1 .
                    FUN 1 16 write:put_string
                      VAR 4
                    FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 6
            FUN 5 15 write:write_out
              VAR 0
              VAR 1
              VAR 2
              VAR 3
              FUN 0 5 punct
            FUN 2 1 .
              FUN 1 4 atom
                VAR 0
              FUN 2 1 .
                FUN 2 2 =<
                  VAR 2
                  INT 1200
                FUN 2 1 .
                  FUN 3 10 current_op
                    VAR 4
                    VAR 5
                    VAR 0
                  FUN 2 1 .
                    FUN 2 1 >
                      VAR 4
                      VAR 2
                    FUN 2 1 .
                      FUN 0 1 !
                      FUN 2 1 .
                        FUN 1 14 write:put_code
                          INT 40
                        FUN 2 1 .
                          FUN 2 21 write:write_out$5_3_1
                            VAR 0
                            VAR 1
                          FUN 2 1 .
                            FUN 1 14 write:put_code
                              INT 41
                            FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 5
              FUN 5 15 write:write_out
                VAR 0
                VAR 1
                VAR 2
                VAR 3
                VAR 4
              FUN 2 1 .
                FUN 1 4 atom
                  VAR 0
                FUN 2 1 .
                  FUN 0 1 !
                  FUN 2 1 .
                    FUN 4 16 write:write_atom
                      VAR 0
                      VAR 1
                      VAR 3
                      VAR 4
                    FUN 0 2 []
            FUN 2 1 .
              FUN 3 6 clause
                INT 9
                FUN 5 15 write:write_out
                  VAR 0
                  VAR 1
                  VAR 2
                  VAR 3
                  FUN 0 5 punct
                FUN 2 1 .
                  FUN 2 1 =
                    VAR 1
                    FUN 3 5 style
                      VAR 4
                      FUN 0 4 true
                      VAR 5
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 3 7 functor
                        VAR 0
                        VAR 6
                        VAR 7
                      FUN 2 1 .
                        FUN 4 16 write:write_atom
                          VAR 6
                          VAR 1
                          VAR 3
                          VAR 8
                        FUN 2 1 .
                          FUN 5 16 write:write_args
                            INT 0
                            VAR 7
                            VAR 0
                            INT 40
                            VAR 1
                          FUN 0 2 []
              FUN 2 1 .
                FUN 3 6 clause
                  INT 5
                  FUN 5 15 write:write_out
                    FUN 1 2 {}
                      VAR 0
                    VAR 1
                    VAR 2
                    VAR 3
                    FUN 0 5 punct
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 2 1 .
                      FUN 1 14 write:put_code
                        INT 123
                      FUN 2 1 .
                        FUN 5 15 write:write_out
                          VAR 0
                          VAR 1
                          INT 1200
                          FUN 0 5 punct
                          VAR 4
                        FUN 2 1 .
                          FUN 1 14 write:put_code
                            INT 125
                          FUN 0 2 []
                FUN 2 1 .
                  FUN 3 6 clause
                    INT 6
                    FUN 5 15 write:write_out
                      FUN 2 1 .
                        VAR 0
                        VAR 1
                      VAR 2
                      VAR 3
                      VAR 4
                      FUN 0 5 punct
                    FUN 2 1 .
                      FUN 0 1 !
                      FUN 2 1 .
                        FUN 1 14 write:put_code
                          INT 91
                        FUN 2 1 .
                          FUN 5 15 write:write_out
                            VAR 0
                            VAR 2
                            INT 999
                            FUN 0 5 punct
                            VAR 5
                          FUN 2 1 .
                            FUN 2 16 write:write_tail
                              VAR 1
                              VAR 2
                            FUN 0 2 []
                  FUN 2 1 .
                    FUN 3 6 clause
                      INT 9
                      FUN 5 15 write:write_out
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
                          FUN 5 17 write:maybe_paren
                            INT 1000
                            VAR 3
                            INT 40
                            VAR 4
                            VAR 6
                          FUN 2 1 .
                            FUN 5 15 write:write_out
                              VAR 0
                              VAR 2
                              INT 999
                              VAR 6
                              VAR 7
                            FUN 2 1 .
                              FUN 1 14 write:put_code
                                INT 44
                              FUN 2 1 .
                                FUN 5 15 write:write_out
                                  VAR 1
                                  VAR 2
                                  INT 1000
                                  FUN 0 5 punct
                                  VAR 8
                                FUN 2 1 .
                                  FUN 5 17 write:maybe_paren
                                    INT 1000
                                    VAR 3
                                    INT 41
                                    VAR 8
                                    VAR 5
                                  FUN 0 2 []
                    FUN 2 1 .
                      FUN 3 6 clause
                        INT 7
                        FUN 5 15 write:write_out
                          VAR 0
                          VAR 1
                          VAR 2
                          VAR 3
                          VAR 4
                        FUN 2 1 .
                          FUN 3 7 functor
                            VAR 0
                            VAR 5
                            VAR 6
                          FUN 2 1 .
                            FUN 7 15 write:write_out
                              VAR 6
                              VAR 5
                              VAR 0
                              VAR 1
                              VAR 2
                              VAR 3
                              VAR 4
                            FUN 0 2 []
                      FUN 0 2 []
CLS 3 21 write:write_out$7_0_6
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 21 write:write_out$7_0_6
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 10 current_op
          VAR 2
          FUN 0 2 fx
          VAR 1
        FUN 2 1 .
          FUN 2 2 is
            VAR 0
            FUN 2 1 -
              VAR 2
              INT 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 21 write:write_out$7_0_6
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 10 current_op
            VAR 2
            FUN 0 2 fy
            VAR 1
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              VAR 2
            FUN 0 2 []
      FUN 0 2 []
CLS 3 21 write:write_out$7_1_6
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 21 write:write_out$7_1_6
        VAR 0
        VAR 1
        VAR 2
      FUN 2 1 .
        FUN 3 10 current_op
          VAR 2
          FUN 0 2 xf
          VAR 1
        FUN 2 1 .
          FUN 2 2 is
            VAR 0
            FUN 2 1 -
              VAR 2
              INT 1
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 3 21 write:write_out$7_1_6
          VAR 0
          VAR 1
          VAR 2
        FUN 2 1 .
          FUN 3 10 current_op
            VAR 2
            FUN 0 2 yf
            VAR 1
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              VAR 2
            FUN 0 2 []
      FUN 0 2 []
CLS 4 21 write:write_out$7_2_8
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 21 write:write_out$7_2_8
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 3 10 current_op
          VAR 3
          FUN 0 3 xfy
          VAR 2
        FUN 2 1 .
          FUN 2 2 is
            VAR 1
            FUN 2 1 -
              VAR 3
              INT 1
          FUN 2 1 .
            FUN 2 1 =
              VAR 0
              VAR 3
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 21 write:write_out$7_2_8
          VAR 0
          VAR 1
          VAR 2
          VAR 3
        FUN 2 1 .
          FUN 3 10 current_op
            VAR 3
            FUN 0 3 xfx
            VAR 2
          FUN 2 1 .
            FUN 2 2 is
              VAR 1
              FUN 2 1 -
                VAR 3
                INT 1
            FUN 2 1 .
              FUN 2 1 =
                VAR 0
                VAR 1
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 4 21 write:write_out$7_2_8
            VAR 0
            VAR 1
            VAR 2
            VAR 3
          FUN 2 1 .
            FUN 3 10 current_op
              VAR 3
              FUN 0 3 yfx
              VAR 2
            FUN 2 1 .
              FUN 2 2 is
                VAR 0
                FUN 2 1 -
                  VAR 3
                  INT 1
              FUN 2 1 .
                FUN 2 1 =
                  VAR 1
                  VAR 3
                FUN 0 2 []
        FUN 0 2 []
CLS 7 15 write:write_out
  FUN 2 1 .
    FUN 3 6 clause
      INT 12
      FUN 7 15 write:write_out
        INT 1
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
        VAR 5
      FUN 2 1 .
        FUN 3 21 write:write_out$7_0_6
          VAR 6
          VAR 0
          VAR 7
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 5 17 write:maybe_paren
              VAR 7
              VAR 3
              INT 40
              VAR 4
              VAR 8
            FUN 2 1 .
              FUN 4 16 write:write_atom
                VAR 0
                VAR 2
                VAR 8
                VAR 9
              FUN 2 1 .
                FUN 3 3 arg
                  INT 1
                  VAR 1
                  VAR 10
                FUN 2 1 .
                  FUN 5 15 write:write_out
                    VAR 10
                    VAR 2
                    VAR 6
                    VAR 9
                    VAR 11
                  FUN 2 1 .
                    FUN 5 17 write:maybe_paren
                      VAR 7
                      VAR 3
                      INT 41
                      VAR 11
                      VAR 5
                    FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 12
        FUN 7 15 write:write_out
          INT 1
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
          VAR 5
        FUN 2 1 .
          FUN 3 21 write:write_out$7_1_6
            VAR 6
            VAR 0
            VAR 7
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 5 17 write:maybe_paren
                VAR 7
                VAR 3
                INT 40
                VAR 4
                VAR 8
              FUN 2 1 .
                FUN 3 3 arg
                  INT 1
                  VAR 1
                  VAR 9
                FUN 2 1 .
                  FUN 5 15 write:write_out
                    VAR 9
                    VAR 2
                    VAR 6
                    VAR 8
                    VAR 10
                  FUN 2 1 .
                    FUN 4 16 write:write_atom
                      VAR 0
                      VAR 2
                      VAR 10
                      VAR 11
                    FUN 2 1 .
                      FUN 5 17 write:maybe_paren
                        VAR 7
                        VAR 3
                        INT 41
                        VAR 11
                        VAR 5
                      FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 15
          FUN 7 15 write:write_out
            INT 2
            VAR 0
            VAR 1
            VAR 2
            VAR 3
            VAR 4
            VAR 5
          FUN 2 1 .
            FUN 4 21 write:write_out$7_2_8
              VAR 6
              VAR 7
              VAR 0
              VAR 8
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 5 17 write:maybe_paren
                  VAR 8
                  VAR 3
                  INT 40
                  VAR 4
                  VAR 9
                FUN 2 1 .
                  FUN 3 3 arg
                    INT 1
                    VAR 1
                    VAR 10
                  FUN 2 1 .
                    FUN 5 15 write:write_out
                      VAR 10
                      VAR 2
                      VAR 7
                      VAR 9
                      VAR 11
                    FUN 2 1 .
                      FUN 5 16 write:write_oper
                        VAR 0
                        VAR 8
                        VAR 2
                        VAR 11
                        VAR 12
                      FUN 2 1 .
                        FUN 3 3 arg
                          INT 2
                          VAR 1
                          VAR 13
                        FUN 2 1 .
                          FUN 5 15 write:write_out
                            VAR 13
                            VAR 2
                            VAR 6
                            VAR 12
                            VAR 14
                          FUN 2 1 .
                            FUN 5 17 write:maybe_paren
                              VAR 8
                              VAR 3
                              INT 41
                              VAR 14
                              VAR 5
                            FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 7
            FUN 7 15 write:write_out
              VAR 0
              VAR 1
              VAR 2
              VAR 3
              VAR 4
              VAR 5
              FUN 0 5 punct
            FUN 2 1 .
              FUN 4 16 write:write_atom
                VAR 1
                VAR 3
                VAR 5
                VAR 6
              FUN 2 1 .
                FUN 5 16 write:write_args
                  INT 0
                  VAR 0
                  VAR 2
                  INT 40
                  VAR 3
                FUN 0 2 []
          FUN 0 2 []
CLS 5 16 write:write_oper
  FUN 2 1 .
    FUN 3 6 clause
      INT 5
      FUN 5 16 write:write_oper
        VAR 0
        VAR 1
        VAR 2
        VAR 3
        VAR 4
      FUN 2 1 .
        FUN 2 1 <
          VAR 1
          INT 700
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 4 16 write:write_atom
              VAR 0
              VAR 2
              VAR 3
              VAR 4
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 5
        FUN 5 16 write:write_oper
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          FUN 0 5 punct
        FUN 2 1 .
          FUN 1 14 write:put_code
            INT 32
          FUN 2 1 .
            FUN 4 16 write:write_atom
              VAR 0
              VAR 2
              FUN 0 5 punct
              VAR 4
            FUN 2 1 .
              FUN 1 14 write:put_code
                INT 32
              FUN 0 2 []
      FUN 0 2 []
CLS 1 21 write:write_VAR$3_0_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 21 write:write_VAR$3_0_1
        VAR 0
      FUN 2 1 .
        FUN 2 1 <
          VAR 0
          INT 26
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 0 4 true
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 3
        FUN 1 21 write:write_VAR$3_0_1
          VAR 0
        FUN 2 1 .
          FUN 2 2 is
            VAR 1
            FUN 2 2 //
              VAR 0
              INT 26
          FUN 2 1 .
            FUN 2 12 number_codes
              VAR 1
              VAR 2
            FUN 2 1 .
              FUN 1 16 write:put_string
                VAR 2
              FUN 0 2 []
      FUN 0 2 []
CLS 3 15 write:write_VAR
  FUN 2 1 .
    FUN 3 6 clause
      INT 3
      FUN 3 15 write:write_VAR
        VAR 0
        VAR 1
        FUN 0 5 alpha
      FUN 2 1 .
        FUN 2 17 write:maybe_space
          VAR 1
          FUN 0 5 alpha
        FUN 2 1 .
          FUN 2 2 is
            VAR 2
            FUN 2 1 +
              FUN 2 3 mod
                VAR 0
                INT 26
              INT 65
          FUN 2 1 .
            FUN 1 14 write:put_code
              VAR 2
            FUN 2 1 .
              FUN 1 21 write:write_VAR$3_0_1
                VAR 0
              FUN 2 1 .
                FUN 0 1 !
                FUN 0 2 []
    FUN 0 2 []
CLS 4 22 write:write_atom$4_4_1
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 4 22 write:write_atom$4_4_1
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 2 19 write:classify_name
          VAR 3
          VAR 2
        FUN 2 1 .
          FUN 2 17 write:maybe_space
            VAR 1
            VAR 2
          FUN 2 1 .
            FUN 1 16 write:put_string
              VAR 3
            FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 4
        FUN 4 22 write:write_atom$4_4_1
          FUN 3 5 style
            FUN 0 4 true
            VAR 0
            VAR 1
          VAR 2
          FUN 0 5 quote
          VAR 3
        FUN 2 1 .
          FUN 2 17 write:maybe_space
            VAR 2
            FUN 0 5 quote
          FUN 2 1 .
            FUN 1 14 write:put_code
              INT 39
            FUN 2 1 .
              FUN 2 16 write:put_string
                VAR 3
                INT 39
              FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 3
          FUN 4 22 write:write_atom$4_4_1
            VAR 0
            VAR 1
            FUN 0 5 alpha
            VAR 2
          FUN 2 1 .
            FUN 1 16 write:put_string
              VAR 2
            FUN 0 2 []
        FUN 0 2 []
CLS 4 16 write:write_atom
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 4 16 write:write_atom
        FUN 0 1 !
        VAR 0
        VAR 1
        FUN 0 5 punct
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 14 write:put_code
            INT 33
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 4 16 write:write_atom
          FUN 0 1 ;
          VAR 0
          VAR 1
          FUN 0 5 punct
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 14 write:put_code
              INT 59
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 2
          FUN 4 16 write:write_atom
            FUN 0 2 []
            VAR 0
            VAR 1
            FUN 0 5 punct
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 14 write:put_code
                INT 91
              FUN 2 1 .
                FUN 1 14 write:put_code
                  INT 93
                FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 2
            FUN 4 16 write:write_atom
              FUN 0 2 {}
              VAR 0
              VAR 1
              FUN 0 5 punct
            FUN 2 1 .
              FUN 0 1 !
              FUN 2 1 .
                FUN 1 14 write:put_code
                  INT 123
                FUN 2 1 .
                  FUN 1 14 write:put_code
                    INT 125
                  FUN 0 2 []
          FUN 2 1 .
            FUN 3 6 clause
              INT 5
              FUN 4 16 write:write_atom
                VAR 0
                VAR 1
                VAR 2
                VAR 3
              FUN 2 1 .
                FUN 2 10 atom_codes
                  VAR 0
                  VAR 4
                FUN 2 1 .
                  FUN 4 22 write:write_atom$4_4_1
                    VAR 1
                    VAR 2
                    VAR 3
                    VAR 4
                  FUN 2 1 .
                    FUN 0 1 !
                    FUN 0 2 []
            FUN 0 2 []
CLS 2 19 write:classify_name
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 19 write:classify_name
        FUN 2 1 .
          VAR 0
          VAR 1
        FUN 0 5 alpha
      FUN 2 1 .
        FUN 2 2 >=
          VAR 0
          INT 97
        FUN 2 1 .
          FUN 2 2 =<
            VAR 0
            INT 122
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 25 write:classify_alpha_tail
                VAR 1
              FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 2 19 write:classify_name
          FUN 2 1 .
            VAR 0
            VAR 1
          FUN 0 5 other
        FUN 2 1 .
          FUN 2 15 lists:memberchk
            VAR 0
            FUN 2 1 .
              INT 35
              FUN 2 1 .
                INT 36
                FUN 2 1 .
                  INT 38
                  FUN 2 1 .
                    INT 61
                    FUN 2 1 .
                      INT 45
                      FUN 2 1 .
                        INT 126
                        FUN 2 1 .
                          INT 94
                          FUN 2 1 .
                            INT 96
                            FUN 2 1 .
                              INT 64
                              FUN 2 1 .
                                INT 43
                                FUN 2 1 .
                                  INT 42
                                  FUN 2 1 .
                                    INT 58
                                    FUN 2 1 .
                                      INT 60
                                      FUN 2 1 .
                                        INT 62
                                        FUN 2 1 .
                                          INT 46
                                          FUN 2 1 .
                                            INT 47
                                            FUN 2 1 .
                                              INT 63
                                              FUN 0 2 []
          FUN 2 1 .
            FUN 1 25 write:classify_other_tail
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 1 31 write:classify_alpha_tail$1_1_2
  FUN 2 1 .
    FUN 3 6 clause
      INT 1
      FUN 1 31 write:classify_alpha_tail$1_1_2
        VAR 0
      FUN 2 1 .
        FUN 2 2 >=
          VAR 0
          INT 97
        FUN 2 1 .
          FUN 2 2 =<
            VAR 0
            INT 122
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 1 31 write:classify_alpha_tail$1_1_2
          VAR 0
        FUN 2 1 .
          FUN 2 2 >=
            VAR 0
            INT 65
          FUN 2 1 .
            FUN 2 2 =<
              VAR 0
              INT 90
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 1
          FUN 1 31 write:classify_alpha_tail$1_1_2
            VAR 0
          FUN 2 1 .
            FUN 2 2 >=
              VAR 0
              INT 48
            FUN 2 1 .
              FUN 2 2 =<
                VAR 0
                INT 57
              FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 1
            FUN 1 31 write:classify_alpha_tail$1_1_2
              VAR 0
            FUN 2 1 .
              FUN 2 3 =:=
                VAR 0
                INT 95
              FUN 0 2 []
          FUN 0 2 []
CLS 1 25 write:classify_alpha_tail
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 25 write:classify_alpha_tail
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 25 write:classify_alpha_tail
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 1 31 write:classify_alpha_tail$1_1_2
            VAR 0
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 25 write:classify_alpha_tail
                VAR 1
              FUN 0 2 []
      FUN 0 2 []
CLS 1 25 write:classify_other_tail
  FUN 2 1 .
    FUN 3 6 clause
      INT 0
      FUN 1 25 write:classify_other_tail
        FUN 0 2 []
      FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 2
        FUN 1 25 write:classify_other_tail
          FUN 2 1 .
            VAR 0
            VAR 1
        FUN 2 1 .
          FUN 2 15 lists:memberchk
            VAR 0
            FUN 2 1 .
              INT 35
              FUN 2 1 .
                INT 36
                FUN 2 1 .
                  INT 38
                  FUN 2 1 .
                    INT 61
                    FUN 2 1 .
                      INT 45
                      FUN 2 1 .
                        INT 126
                        FUN 2 1 .
                          INT 94
                          FUN 2 1 .
                            INT 96
                            FUN 2 1 .
                              INT 64
                              FUN 2 1 .
                                INT 43
                                FUN 2 1 .
                                  INT 42
                                  FUN 2 1 .
                                    INT 58
                                    FUN 2 1 .
                                      INT 60
                                      FUN 2 1 .
                                        INT 62
                                        FUN 2 1 .
                                          INT 46
                                          FUN 2 1 .
                                            INT 47
                                            FUN 2 1 .
                                              INT 63
                                              FUN 0 2 []
          FUN 2 1 .
            FUN 1 25 write:classify_other_tail
              VAR 1
            FUN 0 2 []
      FUN 0 2 []
CLS 5 16 write:write_args
  FUN 2 1 .
    FUN 3 6 clause
      INT 4
      FUN 5 16 write:write_args
        VAR 0
        VAR 0
        VAR 1
        VAR 2
        VAR 3
      FUN 2 1 .
        FUN 0 1 !
        FUN 2 1 .
          FUN 1 14 write:put_code
            INT 41
          FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 8
        FUN 5 16 write:write_args
          VAR 0
          VAR 1
          VAR 2
          VAR 3
          VAR 4
        FUN 2 1 .
          FUN 1 14 write:put_code
            VAR 3
          FUN 2 1 .
            FUN 2 2 is
              VAR 5
              FUN 2 1 +
                VAR 0
                INT 1
            FUN 2 1 .
              FUN 3 3 arg
                VAR 5
                VAR 2
                VAR 6
              FUN 2 1 .
                FUN 5 15 write:write_out
                  VAR 6
                  VAR 4
                  INT 999
                  FUN 0 5 punct
                  VAR 7
                FUN 2 1 .
                  FUN 5 16 write:write_args
                    VAR 5
                    VAR 1
                    VAR 2
                    INT 44
                    VAR 4
                  FUN 0 2 []
      FUN 0 2 []
CLS 2 16 write:write_tail
  FUN 2 1 .
    FUN 3 6 clause
      INT 2
      FUN 2 16 write:write_tail
        VAR 0
        VAR 1
      FUN 2 1 .
        FUN 1 3 var
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 14 write:put_code
              INT 124
            FUN 2 1 .
              FUN 1 20 write:write_variable
                VAR 0
              FUN 2 1 .
                FUN 1 14 write:put_code
                  INT 93
                FUN 0 2 []
    FUN 2 1 .
      FUN 3 6 clause
        INT 1
        FUN 2 16 write:write_tail
          FUN 0 2 []
          VAR 0
        FUN 2 1 .
          FUN 0 1 !
          FUN 2 1 .
            FUN 1 14 write:put_code
              INT 93
            FUN 0 2 []
      FUN 2 1 .
        FUN 3 6 clause
          INT 4
          FUN 2 16 write:write_tail
            FUN 2 1 .
              VAR 0
              VAR 1
            VAR 2
          FUN 2 1 .
            FUN 0 1 !
            FUN 2 1 .
              FUN 1 14 write:put_code
                INT 44
              FUN 2 1 .
                FUN 5 15 write:write_out
                  VAR 0
                  VAR 2
                  INT 999
                  FUN 0 5 punct
                  VAR 3
                FUN 2 1 .
                  FUN 2 16 write:write_tail
                    VAR 1
                    VAR 2
                  FUN 0 2 []
        FUN 2 1 .
          FUN 3 6 clause
            INT 3
            FUN 2 16 write:write_tail
              VAR 0
              VAR 1
            FUN 2 1 .
              FUN 1 14 write:put_code
                INT 124
              FUN 2 1 .
                FUN 5 15 write:write_out
                  VAR 0
                  VAR 1
                  INT 999
                  FUN 0 5 punct
                  VAR 2
                FUN 2 1 .
                  FUN 1 14 write:put_code
                    INT 93
                  FUN 0 2 []
          FUN 0 2 []
