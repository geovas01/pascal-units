#include "GLPKFunctions.h"
#include <stdlib.h>

glp_prob *GLPKCreateMaximizationProblem (char *Name)
{
  glp_prob *Handle= glp_create_prob ();
  glp_set_obj_dir (Handle, GLP_MAX);
  glp_set_prob_name (Handle, Name);

  return Handle;

}

glp_prob *GLPKCreateMinimizationProblem (char *Name)
{
  glp_prob *Handle= glp_create_prob ();
  glp_set_obj_dir (Handle, GLP_MIN);
  glp_set_prob_name (Handle, Name);

  return Handle;

}

void GLPKAddNewContinuesVariable (char *VarName, int BoundStatus, double LowerBound, double UpperBound, double CoefInObjFunction, glp_prob *Handle)
{
  int VarIndex= glp_add_cols (Handle, 1);
  glp_set_col_name (Handle, VarIndex, VarName);

  int BoundType= 0;
  switch (BoundStatus)
  {
	case 0:
          BoundType= GLP_FR;
	  break;
	case 1:
          BoundType= GLP_LO;
	  break;
	case 2:
	  BoundType= GLP_UP;
	  break;
	case 3:
	  BoundType= GLP_DB;
	  break;
	case 4:
	  BoundType= GLP_FX;
	  break;
  }

  glp_set_col_bnds (Handle, VarIndex, BoundType, LowerBound, UpperBound);
  glp_set_obj_coef (Handle, VarIndex, CoefInObjFunction);
  glp_set_col_kind (Handle, VarIndex, GLP_CV);

}

void GLPKAddNewIntegerVariable (char *VarName, int BoundStatus, double LowerBound, double UpperBound, double CoefInObjFunction, glp_prob *Handle)
{
  int VarIndex= glp_add_cols (Handle, 1);
  glp_set_col_name (Handle, VarIndex, VarName);

  int BoundType= 0;
  switch (BoundStatus)
  {
	case 0:
          BoundType= GLP_FR;
	  break;
	case 1:
          BoundType= GLP_LO;
	  break;
	case 2:
	  BoundType= GLP_UP;
	  break;
	case 3:
	  BoundType= GLP_DB;
	  break;
	case 4:
	  BoundType= GLP_FX;
	  break;
  }

  glp_set_col_bnds (Handle, VarIndex, BoundType, LowerBound, UpperBound);
  glp_set_obj_coef (Handle, VarIndex, CoefInObjFunction);
  glp_set_col_kind (Handle, VarIndex, GLP_IV);

}


void GLPKAddNewConstraint (char *RowName, int BoundStatus, double LowerBound, double UpperBound, glp_prob *Handle)
{
  int RowIndex= glp_add_rows (Handle, 1);
  glp_set_row_name (Handle, RowIndex, RowName);

  int BoundType= 0;
  switch (BoundStatus)
  {
    case 0:
      BoundType= GLP_FR;
      break;

    case 1:
      BoundType= GLP_LO;
      break;

    case 2:
      BoundType= GLP_UP;
      break;

    case 3:
      BoundType= GLP_DB;
      break;

    case 4:
      BoundType= GLP_FX;
      break;

  }

  glp_set_row_bnds (Handle, RowIndex, BoundType, LowerBound, UpperBound);

}

void GLPKLoadMatrix (int *ia, int *ib, int *ic, int ne, glp_prob *Handle)
{
  double *rc;
  rc= (double *) malloc (sizeof (double)* (ne+ 1));

  int i;
  for (i= 1; i<= ne; ++i)
  {
    rc [i]= ic [i];
  }

  glp_load_matrix (Handle, ne, ia, ib, rc);

}

void GLPKSimplex (glp_prob *Handle)
{
  glp_simplex (Handle, NULL);

}

void GLPKIntOptimization (glp_prob *Handle)
{
  glp_simplex (Handle, NULL);
  glp_intopt (Handle, NULL);

}

double GLPKGetLPVariablePrimeValue (glp_prob *Handle, int VarIndex)
{
  return glp_get_col_prim (Handle, VarIndex);

}

double GLPKGetMIPVariablePrimeValue (glp_prob *Handle, int VarIndex)
{
  return glp_mip_col_val (Handle, VarIndex);

}


double GLPKGetObjectiveFunctionValue (glp_prob *Handle)
{
  return glp_get_obj_val (Handle);

}
