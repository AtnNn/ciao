/* Prolog BD_Shape_mpq_class interface code: definitions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2012 BUGSENG srl (http://bugseng.com)

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://bugseng.com/products/ppl/ . */

#include "ppl_prolog_sysdep.hh"
#include "ppl_prolog_common.defs.hh"

extern "C" Prolog_foreign_return_type
  ppl_delete_BD_Shape_mpq_class(Prolog_term_ref t_ph) {
  static const char* where = "ppl_delete_BD_Shape_mpq_class/1";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_UNREGISTER(ph);
    delete ph;
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_space_dimension(Prolog_term_ref t_nd,
                                               Prolog_term_ref t_uoe,
                                               Prolog_term_ref t_ph) {
  static const char* where = "ppl_new_BD_Shape_mpq_class_from_space_dimension/3";
  try {
    BD_Shape<mpq_class>* ph;
    Prolog_atom uoe = term_to_universe_or_empty(t_uoe, where);

    if (uoe == a_empty)
      ph = new BD_Shape<mpq_class>(term_to_unsigned<dimension_type>(t_nd,
                                                                      where),
                                     EMPTY);
    else
      ph = new BD_Shape<mpq_class>(term_to_unsigned<dimension_type>(t_nd,
                                                                      where),
                                     UNIVERSE);

      Prolog_term_ref tmp = Prolog_new_term_ref();
      Prolog_put_address(tmp, ph);
      if (Prolog_unify(t_ph, tmp)) {
        PPL_REGISTER(ph);
        return PROLOG_SUCCESS;
      }
      else
        delete ph;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_C_Polyhedron(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_C_Polyhedron/2";
  try {
    BD_Shape<mpq_class>* ph;
    const C_Polyhedron* ph_source
        = static_cast<const C_Polyhedron*>
        (term_to_handle<C_Polyhedron >(t_ph_source, where));
    PPL_CHECK(ph_source);
        ph = new BD_Shape<mpq_class>(*ph_source);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron/2";
  try {
    BD_Shape<mpq_class>* ph;
    const NNC_Polyhedron* ph_source
        = static_cast<const NNC_Polyhedron*>
        (term_to_handle<NNC_Polyhedron >(t_ph_source, where));
    PPL_CHECK(ph_source);
        ph = new BD_Shape<mpq_class>(*ph_source);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_Grid(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_Grid/2";
  try {
    BD_Shape<mpq_class>* ph;
    const Grid* ph_source
        = static_cast<const Grid*>
        (term_to_handle<Grid >(t_ph_source, where));
    PPL_CHECK(ph_source);
        ph = new BD_Shape<mpq_class>(*ph_source);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_Rational_Box(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_Rational_Box/2";
  try {
    BD_Shape<mpq_class>* ph;
    const Rational_Box* ph_source
        = static_cast<const Rational_Box*>
        (term_to_handle<Rational_Box >(t_ph_source, where));
    PPL_CHECK(ph_source);
        ph = new BD_Shape<mpq_class>(*ph_source);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class/2";
  try {
    BD_Shape<mpq_class>* ph;
    const BD_Shape<mpz_class>* ph_source
        = static_cast<const BD_Shape<mpz_class>*>
        (term_to_handle<BD_Shape<mpz_class> >(t_ph_source, where));
    PPL_CHECK(ph_source);
        ph = new BD_Shape<mpq_class>(*ph_source);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class/2";
  try {
    BD_Shape<mpq_class>* ph;
    const BD_Shape<mpq_class>* ph_source
        = static_cast<const BD_Shape<mpq_class>*>
        (term_to_handle<BD_Shape<mpq_class> >(t_ph_source, where));
    PPL_CHECK(ph_source);
        ph = new BD_Shape<mpq_class>(*ph_source);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class/2";
  try {
    BD_Shape<mpq_class>* ph;
    const Octagonal_Shape<mpz_class>* ph_source
        = static_cast<const Octagonal_Shape<mpz_class>*>
        (term_to_handle<Octagonal_Shape<mpz_class> >(t_ph_source, where));
    PPL_CHECK(ph_source);
        ph = new BD_Shape<mpq_class>(*ph_source);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class/2";
  try {
    BD_Shape<mpq_class>* ph;
    const Octagonal_Shape<mpq_class>* ph_source
        = static_cast<const Octagonal_Shape<mpq_class>*>
        (term_to_handle<Octagonal_Shape<mpq_class> >(t_ph_source, where));
    PPL_CHECK(ph_source);
        ph = new BD_Shape<mpq_class>(*ph_source);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}







extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_C_Polyhedron_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_C_Polyhedron_with_complexity/3";
  try {
    BD_Shape<mpq_class>* ph;
    const C_Polyhedron* ph_source
        = static_cast<const C_Polyhedron*>
        (term_to_handle<C_Polyhedron >(t_ph_source, where));

    Prolog_atom p_cc = term_to_complexity_class(t_cc, where);
    Complexity_Class cc;
    if (p_cc == a_polynomial)
      cc = POLYNOMIAL_COMPLEXITY;
    else if (p_cc == a_simplex)
      cc = SIMPLEX_COMPLEXITY;
    else
      cc = ANY_COMPLEXITY;

    PPL_CHECK(ph_source);
    ph = new BD_Shape<mpq_class>(*ph_source, cc);

    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron_with_complexity/3";
  try {
    BD_Shape<mpq_class>* ph;
    const NNC_Polyhedron* ph_source
        = static_cast<const NNC_Polyhedron*>
        (term_to_handle<NNC_Polyhedron >(t_ph_source, where));

    Prolog_atom p_cc = term_to_complexity_class(t_cc, where);
    Complexity_Class cc;
    if (p_cc == a_polynomial)
      cc = POLYNOMIAL_COMPLEXITY;
    else if (p_cc == a_simplex)
      cc = SIMPLEX_COMPLEXITY;
    else
      cc = ANY_COMPLEXITY;

    PPL_CHECK(ph_source);
    ph = new BD_Shape<mpq_class>(*ph_source, cc);

    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_Grid_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_Grid_with_complexity/3";
  try {
    BD_Shape<mpq_class>* ph;
    const Grid* ph_source
        = static_cast<const Grid*>
        (term_to_handle<Grid >(t_ph_source, where));

    Prolog_atom p_cc = term_to_complexity_class(t_cc, where);
    Complexity_Class cc;
    if (p_cc == a_polynomial)
      cc = POLYNOMIAL_COMPLEXITY;
    else if (p_cc == a_simplex)
      cc = SIMPLEX_COMPLEXITY;
    else
      cc = ANY_COMPLEXITY;

    PPL_CHECK(ph_source);
    ph = new BD_Shape<mpq_class>(*ph_source, cc);

    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_Rational_Box_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_Rational_Box_with_complexity/3";
  try {
    BD_Shape<mpq_class>* ph;
    const Rational_Box* ph_source
        = static_cast<const Rational_Box*>
        (term_to_handle<Rational_Box >(t_ph_source, where));

    Prolog_atom p_cc = term_to_complexity_class(t_cc, where);
    Complexity_Class cc;
    if (p_cc == a_polynomial)
      cc = POLYNOMIAL_COMPLEXITY;
    else if (p_cc == a_simplex)
      cc = SIMPLEX_COMPLEXITY;
    else
      cc = ANY_COMPLEXITY;

    PPL_CHECK(ph_source);
    ph = new BD_Shape<mpq_class>(*ph_source, cc);

    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity/3";
  try {
    BD_Shape<mpq_class>* ph;
    const BD_Shape<mpz_class>* ph_source
        = static_cast<const BD_Shape<mpz_class>*>
        (term_to_handle<BD_Shape<mpz_class> >(t_ph_source, where));

    Prolog_atom p_cc = term_to_complexity_class(t_cc, where);
    Complexity_Class cc;
    if (p_cc == a_polynomial)
      cc = POLYNOMIAL_COMPLEXITY;
    else if (p_cc == a_simplex)
      cc = SIMPLEX_COMPLEXITY;
    else
      cc = ANY_COMPLEXITY;

    PPL_CHECK(ph_source);
    ph = new BD_Shape<mpq_class>(*ph_source, cc);

    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity/3";
  try {
    BD_Shape<mpq_class>* ph;
    const BD_Shape<mpq_class>* ph_source
        = static_cast<const BD_Shape<mpq_class>*>
        (term_to_handle<BD_Shape<mpq_class> >(t_ph_source, where));

    Prolog_atom p_cc = term_to_complexity_class(t_cc, where);
    Complexity_Class cc;
    if (p_cc == a_polynomial)
      cc = POLYNOMIAL_COMPLEXITY;
    else if (p_cc == a_simplex)
      cc = SIMPLEX_COMPLEXITY;
    else
      cc = ANY_COMPLEXITY;

    PPL_CHECK(ph_source);
    ph = new BD_Shape<mpq_class>(*ph_source, cc);

    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity/3";
  try {
    BD_Shape<mpq_class>* ph;
    const Octagonal_Shape<mpz_class>* ph_source
        = static_cast<const Octagonal_Shape<mpz_class>*>
        (term_to_handle<Octagonal_Shape<mpz_class> >(t_ph_source, where));

    Prolog_atom p_cc = term_to_complexity_class(t_cc, where);
    Complexity_Class cc;
    if (p_cc == a_polynomial)
      cc = POLYNOMIAL_COMPLEXITY;
    else if (p_cc == a_simplex)
      cc = SIMPLEX_COMPLEXITY;
    else
      cc = ANY_COMPLEXITY;

    PPL_CHECK(ph_source);
    ph = new BD_Shape<mpq_class>(*ph_source, cc);

    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc)
{
  static const char* where =
                   "ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity/3";
  try {
    BD_Shape<mpq_class>* ph;
    const Octagonal_Shape<mpq_class>* ph_source
        = static_cast<const Octagonal_Shape<mpq_class>*>
        (term_to_handle<Octagonal_Shape<mpq_class> >(t_ph_source, where));

    Prolog_atom p_cc = term_to_complexity_class(t_cc, where);
    Complexity_Class cc;
    if (p_cc == a_polynomial)
      cc = POLYNOMIAL_COMPLEXITY;
    else if (p_cc == a_simplex)
      cc = SIMPLEX_COMPLEXITY;
    else
      cc = ANY_COMPLEXITY;

    PPL_CHECK(ph_source);
    ph = new BD_Shape<mpq_class>(*ph_source, cc);

    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}







extern "C" Prolog_foreign_return_type
  ppl_new_BD_Shape_mpq_class_from_constraints(Prolog_term_ref t_clist,
                                                    Prolog_term_ref t_ph)
{
  static const char* where =
    "ppl_new_BD_Shape_mpq_class_from_constraints/2";
  try {
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    BD_Shape<mpq_class>* ph;
    ph = new BD_Shape<mpq_class>(cs);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_new_BD_Shape_mpq_class_from_congruences(Prolog_term_ref t_clist,
                                                    Prolog_term_ref t_ph)
{
  static const char* where =
    "ppl_new_BD_Shape_mpq_class_from_congruences/2";
  try {
    Congruence_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_congruence(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    BD_Shape<mpq_class>* ph;
    ph = new BD_Shape<mpq_class>(cs);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_new_BD_Shape_mpq_class_from_generators(Prolog_term_ref t_clist,
                                                    Prolog_term_ref t_ph)
{
  static const char* where =
    "ppl_new_BD_Shape_mpq_class_from_generators/2";
  try {
    Generator_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_generator(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    BD_Shape<mpq_class>* ph;
    ph = new BD_Shape<mpq_class>(cs);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}








extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_swap(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_swap/2";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    swap(*lhs, *rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_space_dimension(Prolog_term_ref t_ph, Prolog_term_ref t_sd) {
  static const char* where = "ppl_BD_Shape_mpq_class_space_dimension/2";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    if (unify_ulong(t_sd, ph->space_dimension()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_affine_dimension(Prolog_term_ref t_ph, Prolog_term_ref t_sd) {
  static const char* where = "ppl_BD_Shape_mpq_class_affine_dimension/2";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    if (unify_ulong(t_sd, ph->affine_dimension()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_relation_with_constraint(Prolog_term_ref t_ph,
                                                 Prolog_term_ref t_c,
                                                 Prolog_term_ref t_r) {
  static const char* where =
    "ppl_BD_Shape_mpq_class_relation_with_constraint/3";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);

  Poly_Con_Relation r = ph->relation_with(build_constraint(t_c, where));

  Prolog_term_ref tail = Prolog_new_term_ref();
  Prolog_put_atom(tail, a_nil);
  while (r != Poly_Con_Relation::nothing()) {
    if (r.implies(Poly_Con_Relation::is_disjoint())) {
      Prolog_term_ref t_dis = Prolog_new_term_ref();
      Prolog_put_atom(t_dis, a_is_disjoint);
      Prolog_construct_cons(tail, t_dis, tail);
      r = r - Poly_Con_Relation::is_disjoint();
    }
    else if (r.implies(Poly_Con_Relation::strictly_intersects())) {
      Prolog_term_ref t_sin = Prolog_new_term_ref();
      Prolog_put_atom(t_sin, a_strictly_intersects);
      Prolog_construct_cons(tail, t_sin, tail);
      r = r - Poly_Con_Relation::strictly_intersects();
    }
    else if (r.implies(Poly_Con_Relation::is_included())) {
      Prolog_term_ref t_inc = Prolog_new_term_ref();
      Prolog_put_atom(t_inc, a_is_included);
      Prolog_construct_cons(tail, t_inc, tail);
      r = r - Poly_Con_Relation::is_included();
    }
    else if (r.implies(Poly_Con_Relation::saturates())) {
      Prolog_term_ref t_sat = Prolog_new_term_ref();
      Prolog_put_atom(t_sat, a_saturates);
      Prolog_construct_cons(tail, t_sat, tail);
      r = r - Poly_Con_Relation::saturates();
    }
   }

      if (Prolog_unify(t_r, tail))
        return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_relation_with_generator(Prolog_term_ref t_ph,
                                                 Prolog_term_ref t_c,
                                                 Prolog_term_ref t_r) {
  static const char* where =
    "ppl_BD_Shape_mpq_class_relation_with_generator/3";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);

  Poly_Gen_Relation r = ph->relation_with(build_generator(t_c, where));

  Prolog_term_ref tail = Prolog_new_term_ref();
Prolog_put_atom(tail, a_nil);
while (r != Poly_Gen_Relation::nothing()) {
  if (r.implies(Poly_Gen_Relation::subsumes())) {
    Prolog_term_ref t_sub = Prolog_new_term_ref();
    Prolog_put_atom(t_sub, a_subsumes);
    Prolog_construct_cons(tail, t_sub, tail);
    r = r - Poly_Gen_Relation::subsumes();
  }
 }

      if (Prolog_unify(t_r, tail))
        return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_relation_with_congruence(Prolog_term_ref t_ph,
                                                 Prolog_term_ref t_c,
                                                 Prolog_term_ref t_r) {
  static const char* where =
    "ppl_BD_Shape_mpq_class_relation_with_congruence/3";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);

  Poly_Con_Relation r = ph->relation_with(build_congruence(t_c, where));

  Prolog_term_ref tail = Prolog_new_term_ref();
  Prolog_put_atom(tail, a_nil);
  while (r != Poly_Con_Relation::nothing()) {
    if (r.implies(Poly_Con_Relation::is_disjoint())) {
      Prolog_term_ref t_dis = Prolog_new_term_ref();
      Prolog_put_atom(t_dis, a_is_disjoint);
      Prolog_construct_cons(tail, t_dis, tail);
      r = r - Poly_Con_Relation::is_disjoint();
    }
    else if (r.implies(Poly_Con_Relation::strictly_intersects())) {
      Prolog_term_ref t_sin = Prolog_new_term_ref();
      Prolog_put_atom(t_sin, a_strictly_intersects);
      Prolog_construct_cons(tail, t_sin, tail);
      r = r - Poly_Con_Relation::strictly_intersects();
    }
    else if (r.implies(Poly_Con_Relation::is_included())) {
      Prolog_term_ref t_inc = Prolog_new_term_ref();
      Prolog_put_atom(t_inc, a_is_included);
      Prolog_construct_cons(tail, t_inc, tail);
      r = r - Poly_Con_Relation::is_included();
    }
    else if (r.implies(Poly_Con_Relation::saturates())) {
      Prolog_term_ref t_sat = Prolog_new_term_ref();
      Prolog_put_atom(t_sat, a_saturates);
      Prolog_construct_cons(tail, t_sat, tail);
      r = r - Poly_Con_Relation::saturates();
    }
    else
      break;
   }

      if (Prolog_unify(t_r, tail))
        return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_get_constraints(Prolog_term_ref t_ph,
                                   Prolog_term_ref t_glist) {
  static const char* where = "ppl_BD_Shape_mpq_class_get_constraints/2";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const Constraint_System& gs = ph->constraints();
    for (Constraint_System::const_iterator i = gs.begin(),
           gs_end = gs.end(); i != gs_end; ++i)
      Prolog_construct_cons(tail, constraint_term(*i), tail);

    if (Prolog_unify(t_glist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_get_congruences(Prolog_term_ref t_ph,
                                   Prolog_term_ref t_glist) {
  static const char* where = "ppl_BD_Shape_mpq_class_get_congruences/2";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const Congruence_System& gs = ph->congruences();
    for (Congruence_System::const_iterator i = gs.begin(),
           gs_end = gs.end(); i != gs_end; ++i)
      Prolog_construct_cons(tail, congruence_term(*i), tail);

    if (Prolog_unify(t_glist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_get_minimized_constraints(Prolog_term_ref t_ph,
                                             Prolog_term_ref t_glist) {
  static const char* where = "ppl_BD_Shape_mpq_class_get_minimized_constraints/2";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const Constraint_System& gs = ph->minimized_constraints();
    for (Constraint_System::const_iterator i = gs.begin(),
           gs_end = gs.end(); i != gs_end; ++i)
      Prolog_construct_cons(tail, constraint_term(*i), tail);

    if (Prolog_unify(t_glist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_get_minimized_congruences(Prolog_term_ref t_ph,
                                             Prolog_term_ref t_glist) {
  static const char* where = "ppl_BD_Shape_mpq_class_get_minimized_congruences/2";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const Congruence_System& gs = ph->minimized_congruences();
    for (Congruence_System::const_iterator i = gs.begin(),
           gs_end = gs.end(); i != gs_end; ++i)
      Prolog_construct_cons(tail, congruence_term(*i), tail);

    if (Prolog_unify(t_glist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_is_empty(Prolog_term_ref t_ph) {
  static const char* where = "ppl_BD_Shape_mpq_class_is_empty/1";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    if (ph->is_empty())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_is_universe(Prolog_term_ref t_ph) {
  static const char* where = "ppl_BD_Shape_mpq_class_is_universe/1";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    if (ph->is_universe())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_is_bounded(Prolog_term_ref t_ph) {
  static const char* where = "ppl_BD_Shape_mpq_class_is_bounded/1";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    if (ph->is_bounded())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_contains_integer_point(Prolog_term_ref t_ph) {
  static const char* where = "ppl_BD_Shape_mpq_class_contains_integer_point/1";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    if (ph->contains_integer_point())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_is_topologically_closed(Prolog_term_ref t_ph) {
  static const char* where = "ppl_BD_Shape_mpq_class_is_topologically_closed/1";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    if (ph->is_topologically_closed())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_is_discrete(Prolog_term_ref t_ph) {
  static const char* where = "ppl_BD_Shape_mpq_class_is_discrete/1";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    if (ph->is_discrete())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_topological_closure_assign(Prolog_term_ref t_ph) {
  static const char* where = "ppl_BD_Shape_mpq_class_topological_closure_assign/1";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->topological_closure_assign();
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_bounds_from_above(Prolog_term_ref t_ph,
                                       Prolog_term_ref t_expr) {
  static const char* where = "ppl_BD_Shape_mpq_class_bounds_from_above/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Linear_Expression l = build_linear_expression(t_expr, where);
    if (ph->bounds_from_above(l))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_bounds_from_below(Prolog_term_ref t_ph,
                                       Prolog_term_ref t_expr) {
  static const char* where = "ppl_BD_Shape_mpq_class_bounds_from_below/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Linear_Expression l = build_linear_expression(t_expr, where);
    if (ph->bounds_from_below(l))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_maximize(Prolog_term_ref t_ph, Prolog_term_ref t_le_expr,
                       Prolog_term_ref t_n,  Prolog_term_ref t_d,
                       Prolog_term_ref t_maxmin) {
  static const char* where = "ppl_BD_Shape_mpq_class_maximize/5";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    const Linear_Expression le = build_linear_expression(t_le_expr, where);
    PPL_DIRTY_TEMP_COEFFICIENT(n);
    PPL_DIRTY_TEMP_COEFFICIENT(d);
    bool maxmin;
    if (ph->maximize(le, n, d, maxmin)) {
      Prolog_term_ref t = Prolog_new_term_ref();
      Prolog_atom a = (maxmin ? a_true : a_false);
      Prolog_put_atom(t, a);
      if (Prolog_unify_Coefficient(t_n, n)
          && Prolog_unify_Coefficient(t_d, d)
          && Prolog_unify(t_maxmin, t))
        return PROLOG_SUCCESS;
    }
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_minimize(Prolog_term_ref t_ph, Prolog_term_ref t_le_expr,
                       Prolog_term_ref t_n,  Prolog_term_ref t_d,
                       Prolog_term_ref t_maxmin) {
  static const char* where = "ppl_BD_Shape_mpq_class_minimize/5";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    const Linear_Expression le = build_linear_expression(t_le_expr, where);
    PPL_DIRTY_TEMP_COEFFICIENT(n);
    PPL_DIRTY_TEMP_COEFFICIENT(d);
    bool maxmin;
    if (ph->minimize(le, n, d, maxmin)) {
      Prolog_term_ref t = Prolog_new_term_ref();
      Prolog_atom a = (maxmin ? a_true : a_false);
      Prolog_put_atom(t, a);
      if (Prolog_unify_Coefficient(t_n, n)
          && Prolog_unify_Coefficient(t_d, d)
          && Prolog_unify(t_maxmin, t))
        return PROLOG_SUCCESS;
    }
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_maximize_with_point(Prolog_term_ref t_ph,
                                  Prolog_term_ref t_le_expr,
                                  Prolog_term_ref t_n, Prolog_term_ref t_d,
                                  Prolog_term_ref t_maxmin, Prolog_term_ref t_g) {
  static const char* where = "ppl_BD_Shape_mpq_class_maximize_with_point/6";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    const Linear_Expression le = build_linear_expression(t_le_expr, where);
    PPL_DIRTY_TEMP_COEFFICIENT(n);
    PPL_DIRTY_TEMP_COEFFICIENT(d);
    bool maxmin;
    Generator g(point());
    if (ph->maximize(le, n, d, maxmin, g)) {
      Prolog_term_ref t = Prolog_new_term_ref();
      Prolog_atom a = (maxmin ? a_true : a_false);
      Prolog_put_atom(t, a);
      if (Prolog_unify_Coefficient(t_n, n)
          && Prolog_unify_Coefficient(t_d, d)
          && Prolog_unify(t_maxmin, t)
          && Prolog_unify(t_g, generator_term(g)))
        return PROLOG_SUCCESS;
    }
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_minimize_with_point(Prolog_term_ref t_ph,
                                  Prolog_term_ref t_le_expr,
                                  Prolog_term_ref t_n, Prolog_term_ref t_d,
                                  Prolog_term_ref t_maxmin, Prolog_term_ref t_g) {
  static const char* where = "ppl_BD_Shape_mpq_class_minimize_with_point/6";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    const Linear_Expression le = build_linear_expression(t_le_expr, where);
    PPL_DIRTY_TEMP_COEFFICIENT(n);
    PPL_DIRTY_TEMP_COEFFICIENT(d);
    bool maxmin;
    Generator g(point());
    if (ph->minimize(le, n, d, maxmin, g)) {
      Prolog_term_ref t = Prolog_new_term_ref();
      Prolog_atom a = (maxmin ? a_true : a_false);
      Prolog_put_atom(t, a);
      if (Prolog_unify_Coefficient(t_n, n)
          && Prolog_unify_Coefficient(t_d, d)
          && Prolog_unify(t_maxmin, t)
          && Prolog_unify(t_g, generator_term(g)))
        return PROLOG_SUCCESS;
    }
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_frequency(Prolog_term_ref t_ph,
                        Prolog_term_ref t_le_expr,
                        Prolog_term_ref t_freqn, Prolog_term_ref t_freqd,
                        Prolog_term_ref t_valn, Prolog_term_ref t_vald) {
  static const char* where = "ppl_BD_Shape_mpq_class_frequency/6";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    const Linear_Expression le = build_linear_expression(t_le_expr, where);
    PPL_DIRTY_TEMP_COEFFICIENT(freqn);
    PPL_DIRTY_TEMP_COEFFICIENT(freqd);
    PPL_DIRTY_TEMP_COEFFICIENT(valn);
    PPL_DIRTY_TEMP_COEFFICIENT(vald);
    if (ph->frequency(le, freqn, freqd, valn, vald)) {
      if (Prolog_unify_Coefficient(t_freqn, freqn)
          && Prolog_unify_Coefficient(t_freqd, freqd)
          && Prolog_unify_Coefficient(t_valn, valn)
          && Prolog_unify_Coefficient(t_vald, vald))
        return PROLOG_SUCCESS;
    }
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_contains_BD_Shape_mpq_class(Prolog_term_ref t_lhs,
                                   Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_contains_BD_Shape_mpq_class/2";
  try {
    const BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    if (lhs->contains(*rhs))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_strictly_contains_BD_Shape_mpq_class(Prolog_term_ref t_lhs,
                                   Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_strictly_contains_BD_Shape_mpq_class/2";
  try {
    const BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    if (lhs->strictly_contains(*rhs))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_is_disjoint_from_BD_Shape_mpq_class(Prolog_term_ref t_lhs,
                                   Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_is_disjoint_from_BD_Shape_mpq_class/2";
  try {
    const BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    if (lhs->is_disjoint_from(*rhs))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_equals_BD_Shape_mpq_class(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_equals_BD_Shape_mpq_class/2";
  try {
    const BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    if (*lhs == *rhs)
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_OK(Prolog_term_ref t_ph) {
  static const char* where = "ppl_BD_Shape_mpq_class_OK/1";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    if (ph->OK())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_add_constraint(Prolog_term_ref t_ph, Prolog_term_ref t_c) {
  static const char* where = "ppl_BD_Shape_mpq_class_add_constraint/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->add_constraint(build_constraint(t_c, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_add_congruence(Prolog_term_ref t_ph, Prolog_term_ref t_c) {
  static const char* where = "ppl_BD_Shape_mpq_class_add_congruence/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->add_congruence(build_congruence(t_c, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_add_constraints(Prolog_term_ref t_ph,
                                   Prolog_term_ref t_clist) {
  static const char* where = "ppl_BD_Shape_mpq_class_add_constraints/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    ph->add_constraints(cs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_add_congruences(Prolog_term_ref t_ph,
                                   Prolog_term_ref t_clist) {
  static const char* where = "ppl_BD_Shape_mpq_class_add_congruences/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Congruence_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_congruence(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    ph->add_congruences(cs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_refine_with_constraint(Prolog_term_ref t_ph,
                                             Prolog_term_ref t_c) {
  static const char* where = "ppl_BD_Shape_mpq_class_refine_with_constraint/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->refine_with_constraint(build_constraint(t_c, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_refine_with_congruence(Prolog_term_ref t_ph,
                                             Prolog_term_ref t_c) {
  static const char* where = "ppl_BD_Shape_mpq_class_refine_with_congruence/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->refine_with_congruence(build_congruence(t_c, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_refine_with_constraints(Prolog_term_ref t_ph,
                                   Prolog_term_ref t_clist) {
  static const char* where = "ppl_BD_Shape_mpq_class_refine_with_constraints/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    ph->refine_with_constraints(cs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_refine_with_congruences(Prolog_term_ref t_ph,
                                   Prolog_term_ref t_clist) {
  static const char* where = "ppl_BD_Shape_mpq_class_refine_with_congruences/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Congruence_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_congruence(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    ph->refine_with_congruences(cs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_intersection_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_intersection_assign";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    lhs->intersection_assign(*rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_upper_bound_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_upper_bound_assign";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    lhs->upper_bound_assign(*rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_difference_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_difference_assign";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    lhs->difference_assign(*rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_concatenate_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_concatenate_assign";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    lhs->concatenate_assign(*rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_time_elapse_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_time_elapse_assign";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    lhs->time_elapse_assign(*rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_upper_bound_assign_if_exact
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_upper_bound_assign_if_exact";
  try {
   BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    return lhs->upper_bound_assign_if_exact(*rhs) ? PROLOG_SUCCESS : PROLOG_FAILURE;

  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_simplify_using_context_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_b) {
  static const char* where = "ppl_BD_Shape_mpq_class_simplify_using_context_assign";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    Prolog_term_ref t_is_intersect = Prolog_new_term_ref();
    Prolog_atom is_intersect
      = (lhs->simplify_using_context_assign(*rhs) ? a_true : a_false);
    Prolog_put_atom(t_is_intersect, is_intersect);
    if (Prolog_unify(t_b, t_is_intersect))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_constrains(Prolog_term_ref t_ph,
                          Prolog_term_ref t_v) {
  static const char* where = "ppl_BD_Shape_mpq_class__constrains/1";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    if (ph->constrains(term_to_Variable(t_v, where)))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_unconstrain_space_dimension(Prolog_term_ref t_ph,
                           Prolog_term_ref t_v) {
  static const char* where = "ppl_BD_Shape_mpq_class__unconstrain/1";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->unconstrain(term_to_Variable(t_v, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_unconstrain_space_dimensions(Prolog_term_ref t_ph,
                           Prolog_term_ref t_vlist) {
  static const char* where = "ppl_BD_Shape_mpq_class__unconstrain/1";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Variables_Set unconstrain_variables;
    Prolog_term_ref v = Prolog_new_term_ref();
    while (Prolog_is_cons(t_vlist)) {
      Prolog_get_cons(t_vlist, v, t_vlist);
      unconstrain_variables.insert(term_to_Variable(v, where).id());
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_vlist, where);
    ph->unconstrain(unconstrain_variables);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_affine_image
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_le, Prolog_term_ref t_d) {
  const char* where = "ppl_BD_Shape_mpq_class_affine_image/4";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->affine_image(term_to_Variable(t_v, where),
                   build_linear_expression(t_le, where),
                   term_to_Coefficient(t_d, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_affine_preimage
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_le, Prolog_term_ref t_d) {
  const char* where = "ppl_BD_Shape_mpq_class_affine_preimage/4";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->affine_preimage(term_to_Variable(t_v, where),
                   build_linear_expression(t_le, where),
                   term_to_Coefficient(t_d, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_bounded_affine_image
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_lb_le, Prolog_term_ref t_ub_le,
   Prolog_term_ref t_d) {
  static const char* where = "ppl_BD_Shape_mpq_class_bounded_affine_image/5";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->bounded_affine_image(term_to_Variable(t_v, where),
                           build_linear_expression(t_lb_le, where),
                           build_linear_expression(t_ub_le, where),
                           term_to_Coefficient(t_d, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_bounded_affine_preimage
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_lb_le, Prolog_term_ref t_ub_le,
   Prolog_term_ref t_d) {
  static const char* where = "ppl_BD_Shape_mpq_class_bounded_affine_preimage/5";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->bounded_affine_preimage(term_to_Variable(t_v, where),
                           build_linear_expression(t_lb_le, where),
                           build_linear_expression(t_ub_le, where),
                           term_to_Coefficient(t_d, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_generalized_affine_image
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_r, Prolog_term_ref t_le,
   Prolog_term_ref t_d) {
  static const char* where = "ppl_BD_Shape_mpq_class_generalized_affine_image/5";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->generalized_affine_image(term_to_Variable(t_v, where),
                               term_to_relation_symbol(t_r, where),
                               build_linear_expression(t_le, where),
                               term_to_Coefficient(t_d, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_generalized_affine_preimage
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_r, Prolog_term_ref t_le,
   Prolog_term_ref t_d) {
  static const char* where = "ppl_BD_Shape_mpq_class_generalized_affine_preimage/5";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->generalized_affine_preimage(term_to_Variable(t_v, where),
                               term_to_relation_symbol(t_r, where),
                               build_linear_expression(t_le, where),
                               term_to_Coefficient(t_d, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_generalized_affine_image_lhs_rhs
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_lhs, Prolog_term_ref t_r, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_generalized_affine_image_lhs_rhs/4";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Relation_Symbol r = term_to_relation_symbol(t_r, where);
    ph->generalized_affine_image(build_linear_expression(t_lhs, where),
                               r,
                               build_linear_expression(t_rhs, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_generalized_affine_preimage_lhs_rhs
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_lhs, Prolog_term_ref t_r, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_generalized_affine_preimage_lhs_rhs/4";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Relation_Symbol r = term_to_relation_symbol(t_r, where);
    ph->generalized_affine_preimage(build_linear_expression(t_lhs, where),
                               r,
                               build_linear_expression(t_rhs, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_add_space_dimensions_and_embed
  (Prolog_term_ref t_ph, Prolog_term_ref t_nnd) {
  static const char* where = "ppl_BD_Shape_mpq_class_add_space_dimensions_and_embed/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    dimension_type d = term_to_unsigned<dimension_type>(t_nnd, where);
    ph->add_space_dimensions_and_embed(d);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_add_space_dimensions_and_project
  (Prolog_term_ref t_ph, Prolog_term_ref t_nnd) {
  static const char* where = "ppl_BD_Shape_mpq_class_add_space_dimensions_and_project/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    dimension_type d = term_to_unsigned<dimension_type>(t_nnd, where);
    ph->add_space_dimensions_and_project(d);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_remove_space_dimensions
  (Prolog_term_ref t_ph, Prolog_term_ref t_vlist) {
  static const char* where = "ppl_BD_Shape_mpq_class_remove_space_dimensions/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Variables_Set dead_variables;
    Prolog_term_ref v = Prolog_new_term_ref();
    while (Prolog_is_cons(t_vlist)) {
      Prolog_get_cons(t_vlist, v, t_vlist);
      dead_variables.insert(term_to_Variable(v, where).id());
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_vlist, where);

    ph->remove_space_dimensions(dead_variables);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_remove_higher_space_dimensions
  (Prolog_term_ref t_ph, Prolog_term_ref t_nd) {
  static const char* where = "ppl_BD_Shape_mpq_class_remove_higher_space_dimensions/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->remove_higher_space_dimensions(term_to_unsigned<dimension_type>(t_nd,
                                                                        where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_expand_space_dimension
  (Prolog_term_ref t_ph, Prolog_term_ref t_v, Prolog_term_ref t_nd) {
  static const char* where = "ppl_BD_Shape_mpq_class_expand_space_dimension/3";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->expand_space_dimension(term_to_Variable(t_v, where),
                               term_to_unsigned<dimension_type>(t_nd, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_fold_space_dimensions
  (Prolog_term_ref t_ph, Prolog_term_ref t_vlist, Prolog_term_ref t_v) {
  static const char* where = "ppl_BD_Shape_mpq_class_fold_space_dimensions/3";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Variables_Set fold_variables;
    Prolog_term_ref v = Prolog_new_term_ref();
    while (Prolog_is_cons(t_vlist)) {
      Prolog_get_cons(t_vlist, v, t_vlist);
      fold_variables.insert(term_to_Variable(v, where).id());
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_vlist, where);

    ph->fold_space_dimensions(fold_variables, term_to_Variable(t_v, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_map_space_dimensions
  (Prolog_term_ref t_ph, Prolog_term_ref t_pfunc) {
  static const char* where = "ppl_BD_Shape_mpq_class_map_space_dimensions/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    dimension_type space_dim = ph->space_dimension();
    PPL_CHECK(ph);
    Partial_Function pfunc;
    Prolog_term_ref t_pair = Prolog_new_term_ref();
    while (Prolog_is_cons(t_pfunc)) {
      Prolog_get_cons(t_pfunc, t_pair, t_pfunc);
      Prolog_atom functor;
      int arity;
      Prolog_get_compound_name_arity(t_pair, &functor, &arity);
      if (arity != 2 || functor != a_minus)
        return PROLOG_FAILURE;
      Prolog_term_ref t_i = Prolog_new_term_ref();
      Prolog_term_ref t_j = Prolog_new_term_ref();
      Prolog_get_arg(1, t_pair, t_i);
      Prolog_get_arg(2, t_pair, t_j);
      dimension_type i = term_to_Variable(t_i, where).id();
      dimension_type j = term_to_Variable(t_j, where).id();
      if (i >= space_dim)
        return PROLOG_FAILURE;
      pfunc.insert(i, j);
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_pfunc, where);

    ph->map_space_dimensions(pfunc);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_drop_some_non_integer_points
  (Prolog_term_ref t_ph, Prolog_term_ref t_cc) {
  static const char* where = "ppl_BD_Shape_mpq_class_drop_some_non_integer_points/2";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Prolog_atom p_cc = term_to_complexity_class(t_cc, where);
    Complexity_Class cc;
    if (p_cc == a_polynomial)
      cc = POLYNOMIAL_COMPLEXITY;
    else if (p_cc == a_simplex)
      cc = SIMPLEX_COMPLEXITY;
    else
      cc = ANY_COMPLEXITY;

    ph->drop_some_non_integer_points(cc);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_drop_some_non_integer_points_2
  (Prolog_term_ref t_ph, Prolog_term_ref t_vlist, Prolog_term_ref t_cc) {
  static const char* where = "ppl_BD_Shape_mpq_class_drop_some_non_integer_points_2/3";
  try {
    BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    Variables_Set variables;
    Prolog_term_ref v = Prolog_new_term_ref();
    while (Prolog_is_cons(t_vlist)) {
      Prolog_get_cons(t_vlist, v, t_vlist);
      variables.insert(term_to_Variable(v, where).id());
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_vlist, where);

    Prolog_atom p_cc = term_to_complexity_class(t_cc, where);
    Complexity_Class cc;
    if (p_cc == a_polynomial)
      cc = POLYNOMIAL_COMPLEXITY;
    else if (p_cc == a_simplex)
      cc = SIMPLEX_COMPLEXITY;
    else
      cc = ANY_COMPLEXITY;

    ph->drop_some_non_integer_points(variables, cc);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_ascii_dump
  (Prolog_term_ref t_ph) {
  static const char* where = "ppl_BD_Shape_mpq_class_ascii_dump/1";
  try {
    const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
    PPL_CHECK(ph);
    ph->ascii_dump(std::cout);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_external_memory_in_bytes(Prolog_term_ref t_pps,
                         Prolog_term_ref t_m) {
  static const char* where = "ppl_BD_Shape_mpq_class_external_memory_in_bytes/2";
  try {
    BD_Shape<mpq_class>* pps = term_to_handle<BD_Shape<mpq_class> >(t_pps, where);
    PPL_CHECK(pps);

    if (unify_ulong(t_m, pps->external_memory_in_bytes()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_total_memory_in_bytes(Prolog_term_ref t_pps,
                         Prolog_term_ref t_m) {
  static const char* where = "ppl_BD_Shape_mpq_class_total_memory_in_bytes/2";
  try {
    BD_Shape<mpq_class>* pps = term_to_handle<BD_Shape<mpq_class> >(t_pps, where);
    PPL_CHECK(pps);

    if (unify_ulong(t_m, pps->total_memory_in_bytes()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_BHMZ05_widening_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs,
   Prolog_term_ref t_ti, Prolog_term_ref t_to) {
  static const char* where = "ppl_BD_Shape_mpq_class_BHMZ05_widening_assign_with_tokens/4";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    unsigned t = term_to_unsigned<unsigned>(t_ti, where);
    lhs->BHMZ05_widening_assign(*rhs, &t);
    if (unify_long(t_to, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_H79_widening_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs,
   Prolog_term_ref t_ti, Prolog_term_ref t_to) {
  static const char* where = "ppl_BD_Shape_mpq_class_H79_widening_assign_with_tokens/4";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    unsigned t = term_to_unsigned<unsigned>(t_ti, where);
    lhs->H79_widening_assign(*rhs, &t);
    if (unify_long(t_to, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_BHMZ05_widening_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_BHMZ05_widening_assign/2";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    lhs->BHMZ05_widening_assign(*rhs, 0);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_H79_widening_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_H79_widening_assign/2";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    lhs->H79_widening_assign(*rhs, 0);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_widening_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs,
   Prolog_term_ref t_ti, Prolog_term_ref t_to) {
  static const char* where = "ppl_BD_Shape_mpq_class_widening_assign_with_tokens/4";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    unsigned t = term_to_unsigned<unsigned>(t_ti, where);
    lhs->widening_assign(*rhs, &t);
    if (unify_long(t_to, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_widening_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_widening_assign/2";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    lhs->widening_assign(*rhs, 0);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}





extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist,
   Prolog_term_ref t_ti, Prolog_term_ref t_to) {
  static const char* where = "ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens/5";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    unsigned t = term_to_unsigned<unsigned>(t_ti, where);
    lhs->limited_BHMZ05_extrapolation_assign(*rhs, cs, &t);
    if (unify_long(t_to, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist,
   Prolog_term_ref t_ti, Prolog_term_ref t_to) {
  static const char* where = "ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign_with_tokens/5";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    unsigned t = term_to_unsigned<unsigned>(t_ti, where);
    lhs->limited_H79_extrapolation_assign(*rhs, cs, &t);
    if (unify_long(t_to, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist,
   Prolog_term_ref t_ti, Prolog_term_ref t_to) {
  static const char* where = "ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens/5";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    unsigned t = term_to_unsigned<unsigned>(t_ti, where);
    lhs->limited_CC76_extrapolation_assign(*rhs, cs, &t);
    if (unify_long(t_to, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}








extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist) {
  static const char* where = "ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign/3";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    lhs->limited_BHMZ05_extrapolation_assign(*rhs, cs, 0);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist) {
  static const char* where = "ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign/3";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    lhs->limited_H79_extrapolation_assign(*rhs, cs, 0);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist) {
  static const char* where = "ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign/3";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    lhs->limited_CC76_extrapolation_assign(*rhs, cs, 0);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}








extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_CC76_extrapolation_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs,
   Prolog_term_ref t_ti, Prolog_term_ref t_to) {
  static const char* where = "ppl_BD_Shape_mpq_class_CC76_extrapolation_assign_with_tokens/4";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);

    unsigned t = term_to_unsigned<unsigned>(t_ti, where);
    lhs->CC76_extrapolation_assign(*rhs, &t);
    if (unify_long(t_to, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_CC76_extrapolation_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_CC76_extrapolation_assign/2";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);

    lhs->CC76_extrapolation_assign(*rhs, 0);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_CC76_narrowing_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_BD_Shape_mpq_class_CC76_narrowing_assign/2";
  try {
    BD_Shape<mpq_class>* lhs = term_to_handle<BD_Shape<mpq_class> >(t_lhs, where);
    const BD_Shape<mpq_class>* rhs = term_to_handle<BD_Shape<mpq_class> >(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    lhs->CC76_narrowing_assign(*rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
ppl_BD_Shape_mpq_class_linear_partition(Prolog_term_ref t_ph,
                             Prolog_term_ref t_qh,
                             Prolog_term_ref t_inters,
                             Prolog_term_ref t_pset) {
  try {
    static const char* where = "ppl_BD_Shape_mpq_class_linear_partition/4";
    BD_Shape<mpq_class>* rfh;
    Pointset_Powerset<NNC_Polyhedron>* rsh;
  const BD_Shape<mpq_class>* ph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);
  const BD_Shape<mpq_class>* qh = term_to_handle<BD_Shape<mpq_class> >(t_qh, where);
  PPL_CHECK(ph);
  PPL_CHECK(qh);
  std::pair<BD_Shape<mpq_class>, Pointset_Powerset<NNC_Polyhedron> >
    r = linear_partition(*ph, *qh);
  rfh = new BD_Shape<mpq_class>(0, EMPTY);
  rsh = new Pointset_Powerset<NNC_Polyhedron>(0, EMPTY);
  swap(*rfh, r.first);
  swap(*rsh, r.second);

    Prolog_term_ref t_r_first = Prolog_new_term_ref();
    Prolog_term_ref t_r_second = Prolog_new_term_ref();
    Prolog_put_address(t_r_first, rfh);
    Prolog_put_address(t_r_second, rsh);
    if (Prolog_unify(t_inters, t_r_first)
        && Prolog_unify(t_pset, t_r_second)) {
      PPL_REGISTER(rfh);
      PPL_REGISTER(rsh);
      return PROLOG_SUCCESS;
    }
    else {
      delete rfh;
      delete rsh;
    }
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_BD_Shape_mpq_class_wrap_assign
     (Prolog_term_ref t_ph,
      Prolog_term_ref t_vars,
      Prolog_term_ref t_w,
      Prolog_term_ref t_r,
      Prolog_term_ref t_o,
      Prolog_term_ref t_cs,
      Prolog_term_ref t_complexity,
      Prolog_term_ref t_ind) {
  static const char* where = "ppl_BD_Shape_mpq_class_wrap_assign/8";
  try {
    BD_Shape<mpq_class>* pph = term_to_handle<BD_Shape<mpq_class> >(t_ph, where);

    Variables_Set vars;
    Prolog_term_ref v = Prolog_new_term_ref();
    while (Prolog_is_cons(t_vars)) {
      Prolog_get_cons(t_vars, v, t_vars);
      vars.insert(term_to_Variable(v, where).id());
    }
    // Check the list is properly terminated.
    check_nil_terminating(t_vars, where);

    Prolog_atom p_w = term_to_bounded_integer_type_width(t_w, where);
    Bounded_Integer_Type_Width w;
    if (p_w == a_bits_8)
      w = BITS_8;
    else if (p_w == a_bits_16)
      w = BITS_16;
    else if (p_w == a_bits_32)
      w = BITS_32;
    else if (p_w == a_bits_64)
      w = BITS_64;
    else
      w = BITS_128;

    Prolog_atom p_r = term_to_bounded_integer_type_representation(t_r, where);
    Bounded_Integer_Type_Representation r;
    if (p_r == a_unsigned)
      r = UNSIGNED;
    else
      r = SIGNED_2_COMPLEMENT;
    Prolog_atom p_o = term_to_bounded_integer_type_overflow(t_o, where);
    Bounded_Integer_Type_Overflow o;
    if (p_o == a_overflow_wraps)
      o = OVERFLOW_WRAPS;
    else if (p_o == a_overflow_undefined)
      o = OVERFLOW_UNDEFINED;
    else
      o = OVERFLOW_IMPOSSIBLE;

    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();
    while (Prolog_is_cons(t_cs)) {
      Prolog_get_cons(t_cs, c, t_cs);
      cs.insert(build_constraint(c, where));
    }
    // Check the list is properly terminated.
    check_nil_terminating(t_cs, where);

    unsigned complexity = term_to_unsigned<unsigned>(t_complexity, where);

    Prolog_atom p_ind = term_to_boolean(t_ind, where);
    bool ind = (p_ind == a_true) ? true : false;

    pph->wrap_assign(vars, w, r, o, &cs, complexity, ind);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
 }





extern "C" Prolog_foreign_return_type
  ppl_termination_test_MS_BD_Shape_mpq_class
  (Prolog_term_ref t_pset) {
  static const char* where = "ppl_termination_test_MS_BD_Shape_mpq_class/1";
  try {
    BD_Shape<mpq_class>* pset = term_to_handle<BD_Shape<mpq_class> >(t_pset, where);
    PPL_CHECK(pset);
    if (Parma_Polyhedra_Library::termination_test_MS(*pset))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_termination_test_PR_BD_Shape_mpq_class
  (Prolog_term_ref t_pset) {
  static const char* where = "ppl_termination_test_PR_BD_Shape_mpq_class/1";
  try {
    BD_Shape<mpq_class>* pset = term_to_handle<BD_Shape<mpq_class> >(t_pset, where);
    PPL_CHECK(pset);
    if (Parma_Polyhedra_Library::termination_test_PR(*pset))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}







extern "C" Prolog_foreign_return_type
  ppl_one_affine_ranking_function_MS_BD_Shape_mpq_class
  (Prolog_term_ref t_pset,
   Prolog_term_ref t_g) {
  static const char* where
     = "ppl_one_affine_ranking_function_MS_BD_Shape_mpq_class/2";
  try {
    BD_Shape<mpq_class>* pset = term_to_handle<BD_Shape<mpq_class> >(t_pset, where);
    Generator gg(point());
    PPL_CHECK(pset);
    if (Parma_Polyhedra_Library
        ::one_affine_ranking_function_MS(*pset, gg)
        && Prolog_unify(t_g, generator_term(gg)))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_one_affine_ranking_function_PR_BD_Shape_mpq_class
  (Prolog_term_ref t_pset,
   Prolog_term_ref t_g) {
  static const char* where
     = "ppl_one_affine_ranking_function_PR_BD_Shape_mpq_class/2";
  try {
    BD_Shape<mpq_class>* pset = term_to_handle<BD_Shape<mpq_class> >(t_pset, where);
    Generator gg(point());
    PPL_CHECK(pset);
    if (Parma_Polyhedra_Library
        ::one_affine_ranking_function_PR(*pset, gg)
        && Prolog_unify(t_g, generator_term(gg)))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}







extern "C" Prolog_foreign_return_type
  ppl_all_affine_ranking_functions_MS_BD_Shape_mpq_class
  (Prolog_term_ref t_pset,
   Prolog_term_ref t_ph) {
  static const char* where =
      "ppl_all_affine_ranking_functions_MS_BD_Shape_mpq_class/2";
  try {
    BD_Shape<mpq_class>* pset = term_to_handle<BD_Shape<mpq_class> >(t_pset, where);
    PPL_CHECK(pset);
    C_Polyhedron* ph = new C_Polyhedron();
    Parma_Polyhedra_Library
      ::all_affine_ranking_functions_MS(*pset, *ph);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_all_affine_ranking_functions_PR_BD_Shape_mpq_class
  (Prolog_term_ref t_pset,
   Prolog_term_ref t_ph) {
  static const char* where =
      "ppl_all_affine_ranking_functions_PR_BD_Shape_mpq_class/2";
  try {
    BD_Shape<mpq_class>* pset = term_to_handle<BD_Shape<mpq_class> >(t_pset, where);
    PPL_CHECK(pset);
    NNC_Polyhedron* ph = new NNC_Polyhedron();
    Parma_Polyhedra_Library
      ::all_affine_ranking_functions_PR(*pset, *ph);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}







extern "C" Prolog_foreign_return_type
  ppl_all_affine_quasi_ranking_functions_MS_BD_Shape_mpq_class
  (Prolog_term_ref t_pset,
   Prolog_term_ref t_ph_decreasing,
   Prolog_term_ref t_ph_bounded) {
  static const char* where =
      "ppl_all_affine_quasi_ranking_functions_MS_BD_Shape_mpq_class/3";
  try {
    BD_Shape<mpq_class>* pset = term_to_handle<BD_Shape<mpq_class> >(t_pset, where);
    PPL_CHECK(pset);
    C_Polyhedron* ph_decreasing = new C_Polyhedron();
    C_Polyhedron* ph_bounded = new C_Polyhedron();
    Parma_Polyhedra_Library
      ::all_affine_quasi_ranking_functions_MS(*pset,
                                              *ph_decreasing, *ph_bounded);
    Prolog_term_ref tmp_decreasing = Prolog_new_term_ref();
    Prolog_put_address(tmp_decreasing, ph_decreasing);
    Prolog_term_ref tmp_bounded = Prolog_new_term_ref();
    Prolog_put_address(tmp_bounded, ph_bounded);
    if (Prolog_unify(t_ph_decreasing, tmp_decreasing)
        && Prolog_unify(t_ph_bounded, tmp_bounded)) {
      PPL_REGISTER(ph_decreasing);
      PPL_REGISTER(ph_bounded);
      return PROLOG_SUCCESS;
    }
    else {
      delete ph_decreasing;
      delete ph_bounded;
    }
  }
  CATCH_ALL;
}






extern "C" Prolog_foreign_return_type
  ppl_termination_test_MS_BD_Shape_mpq_class_2
  (Prolog_term_ref t_pset_before, Prolog_term_ref t_pset_after) {
  static const char* where
      = "ppl_termination_test_MS_BD_Shape_mpq_class_2/2";
  try {
    BD_Shape<mpq_class>* pset_before
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_before, where);
    BD_Shape<mpq_class>* pset_after
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_after, where);
    PPL_CHECK(pset_before);
    PPL_CHECK(pset_after);
    if (Parma_Polyhedra_Library
        ::termination_test_MS_2(*pset_before, *pset_after))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_termination_test_PR_BD_Shape_mpq_class_2
  (Prolog_term_ref t_pset_before, Prolog_term_ref t_pset_after) {
  static const char* where
      = "ppl_termination_test_PR_BD_Shape_mpq_class_2/2";
  try {
    BD_Shape<mpq_class>* pset_before
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_before, where);
    BD_Shape<mpq_class>* pset_after
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_after, where);
    PPL_CHECK(pset_before);
    PPL_CHECK(pset_after);
    if (Parma_Polyhedra_Library
        ::termination_test_PR_2(*pset_before, *pset_after))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}







extern "C" Prolog_foreign_return_type
  ppl_one_affine_ranking_function_MS_BD_Shape_mpq_class_2
  (Prolog_term_ref t_pset_before,
   Prolog_term_ref t_pset_after,
   Prolog_term_ref t_g) {
  static const char* where
     = "ppl_one_affine_ranking_function_MS_BD_Shape_mpq_class_2/3";
  try {
    BD_Shape<mpq_class>* pset_before
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_before, where);
    BD_Shape<mpq_class>* pset_after
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_after, where);
    Generator gg(point());
    PPL_CHECK(pset_before);
    PPL_CHECK(pset_after);
    if (Parma_Polyhedra_Library
        ::one_affine_ranking_function_MS_2(*pset_before,
                                                         *pset_after,
                                                         gg)
        && Prolog_unify(t_g, generator_term(gg)))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_one_affine_ranking_function_PR_BD_Shape_mpq_class_2
  (Prolog_term_ref t_pset_before,
   Prolog_term_ref t_pset_after,
   Prolog_term_ref t_g) {
  static const char* where
     = "ppl_one_affine_ranking_function_PR_BD_Shape_mpq_class_2/3";
  try {
    BD_Shape<mpq_class>* pset_before
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_before, where);
    BD_Shape<mpq_class>* pset_after
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_after, where);
    Generator gg(point());
    PPL_CHECK(pset_before);
    PPL_CHECK(pset_after);
    if (Parma_Polyhedra_Library
        ::one_affine_ranking_function_PR_2(*pset_before,
                                                         *pset_after,
                                                         gg)
        && Prolog_unify(t_g, generator_term(gg)))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}







extern "C" Prolog_foreign_return_type
  ppl_all_affine_ranking_functions_MS_BD_Shape_mpq_class_2
  (Prolog_term_ref t_pset_before,
   Prolog_term_ref t_pset_after,
   Prolog_term_ref t_ph) {
  static const char* where =
      "ppl_all_affine_ranking_functions_MS_BD_Shape_mpq_class_2/3";
  try {
    BD_Shape<mpq_class>* pset_before
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_before, where);
    BD_Shape<mpq_class>* pset_after
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_after, where);
    PPL_CHECK(pset_before);
    PPL_CHECK(pset_after);
    C_Polyhedron* ph = new C_Polyhedron();
    Parma_Polyhedra_Library
      ::all_affine_ranking_functions_MS_2(*pset_before,
                                                        *pset_after,
                                                        *ph);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
  ppl_all_affine_ranking_functions_PR_BD_Shape_mpq_class_2
  (Prolog_term_ref t_pset_before,
   Prolog_term_ref t_pset_after,
   Prolog_term_ref t_ph) {
  static const char* where =
      "ppl_all_affine_ranking_functions_PR_BD_Shape_mpq_class_2/3";
  try {
    BD_Shape<mpq_class>* pset_before
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_before, where);
    BD_Shape<mpq_class>* pset_after
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_after, where);
    PPL_CHECK(pset_before);
    PPL_CHECK(pset_after);
    NNC_Polyhedron* ph = new NNC_Polyhedron();
    Parma_Polyhedra_Library
      ::all_affine_ranking_functions_PR_2(*pset_before,
                                                        *pset_after,
                                                        *ph);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      PPL_REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}







extern "C" Prolog_foreign_return_type
  ppl_all_affine_quasi_ranking_functions_MS_BD_Shape_mpq_class_2
  (Prolog_term_ref t_pset_before,
   Prolog_term_ref t_pset_after,
   Prolog_term_ref t_ph_decreasing,
   Prolog_term_ref t_ph_bounded) {
  static const char* where =
      "ppl_all_affine_quasi_ranking_functions_MS_BD_Shape_mpq_class_2/4";
  try {
    BD_Shape<mpq_class>* pset_before
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_before, where);
    BD_Shape<mpq_class>* pset_after
       = term_to_handle<BD_Shape<mpq_class> >(t_pset_after, where);
    PPL_CHECK(pset_before);
    PPL_CHECK(pset_after);
    C_Polyhedron* ph_decreasing = new C_Polyhedron();
    C_Polyhedron* ph_bounded = new C_Polyhedron();
    Parma_Polyhedra_Library
      ::all_affine_quasi_ranking_functions_MS_2(*pset_before, *pset_after,
                                                *ph_decreasing, *ph_bounded);
    Prolog_term_ref tmp_decreasing = Prolog_new_term_ref();
    Prolog_put_address(tmp_decreasing, ph_decreasing);
    Prolog_term_ref tmp_bounded = Prolog_new_term_ref();
    Prolog_put_address(tmp_bounded, ph_bounded);
    if (Prolog_unify(t_ph_decreasing, tmp_decreasing)
        && Prolog_unify(t_ph_bounded, tmp_bounded)) {
      PPL_REGISTER(ph_decreasing);
      PPL_REGISTER(ph_bounded);
      return PROLOG_SUCCESS;
    }
    else {
      delete ph_decreasing;
      delete ph_bounded;
    }
  }
  CATCH_ALL;
}








