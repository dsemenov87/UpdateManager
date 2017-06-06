module MAptekaGet.Domain

type UpdateName =
  | UpdateName of name:string * compareString:string

type FunctionalName =
  | FunctionalName of name:string * compareString:string

type QualifiedUpdateNameName = 
    | QualifiedPackageName of group:GroupName * package:PackageName