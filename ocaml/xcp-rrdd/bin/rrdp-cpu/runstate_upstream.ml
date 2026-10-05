(*
 * Copyright (C) Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* For a Xen without Xenctrl.Runstateinfo (upstream Xen, which the RISC-V
   port is based on): no runstate data sources, only the domain CPU usage.
   Same signature as runstate_xenserver.ml; dune picks one of the two. *)

open Rrdd_plugin

let dss _xc ~domid:_ ~uuid ~dom_cpu_time dss =
  ( Rrd.VM uuid
  , Ds.ds_make ~name:"cpu_usage" ~units:"(fraction)"
      ~description:"Domain CPU usage" ~value:(Rrd.VT_Float dom_cpu_time)
      ~ty:Rrd.Derive ~default:true ~min:0.0 ~max:1.0 ()
  )
  :: dss
