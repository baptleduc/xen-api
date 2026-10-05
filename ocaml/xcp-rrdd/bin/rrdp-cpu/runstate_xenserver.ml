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

(* Runstate data sources, from Xenctrl.Runstateinfo.V2: a XenServer patch
   to Xen's OCaml bindings and hypervisor. This is the code rrdp_cpu.ml
   carried inline, unchanged. dune picks it unless the build targets a Xen
   without that patch (see runstate_upstream.ml). *)

open Rrdd_plugin

let dss xc ~domid ~uuid ~dom_cpu_time dss =
  let ( ++ ) = Int64.add in
  try
    let ri = Xenctrl.Runstateinfo.V2.domain_get xc domid in
    let runnable_vcpus_ds =
      match ri.Xenctrl.Runstateinfo.V2.runnable with
      | 0L ->
          []
      | _ ->
          [
            ( Rrd.VM uuid
            , Ds.ds_make ~name:"runnable_vcpus" ~units:"(fraction)"
                ~value:
                  (Rrd.VT_Float
                     (Int64.to_float ri.Xenctrl.Runstateinfo.V2.runnable
                     /. 1.0e9
                     )
                  )
                ~description:
                  "Fraction of time that vCPUs of the domain are runnable"
                ~ty:Rrd.Derive ~default:false ~min:0.0 ~max:1.0 ()
            )
          ]
    in
    let nonaffine_vcpus_ds =
      match ri.Xenctrl.Runstateinfo.V2.running with
      | 0L ->
          []
      | _ ->
          [
            ( Rrd.VM uuid
            , Ds.ds_make ~name:"numa_node_nonaffine_vcpus"
                ~units:"(fraction)"
                ~value:
                  (Rrd.VT_Float
                     (Int64.to_float ri.Xenctrl.Runstateinfo.V2.nonaffine
                     /. 1.0e9
                     )
                  )
                ~description:
                  "Fraction of vCPU time running outside of vCPU \
                   soft-affinity"
                ~ty:Rrd.Derive ~default:false ~min:0.0 ~max:1.0 ()
            )
          ]
    in
    ( Rrd.VM uuid
    , Ds.ds_make ~name:"runstate_fullrun" ~units:"(fraction)"
        ~value:
          (Rrd.VT_Float
             (Int64.to_float ri.Xenctrl.Runstateinfo.V2.time0 /. 1.0e9)
          )
        ~description:"Fraction of time that all vCPUs are running"
        ~ty:Rrd.Derive ~default:false ~min:0.0 ~max:1.0 ()
    )
    :: ( Rrd.VM uuid
       , Ds.ds_make ~name:"runstate_full_contention" ~units:"(fraction)"
           ~value:
             (Rrd.VT_Float
                (Int64.to_float ri.Xenctrl.Runstateinfo.V2.time1 /. 1.0e9)
             )
           ~description:
             "Fraction of time that all vCPUs are runnable (i.e., \
              waiting for CPU)"
           ~ty:Rrd.Derive ~default:false ~min:0.0 ~max:1.0 ()
       )
    :: ( Rrd.VM uuid
       , Ds.ds_make ~name:"runstate_concurrency_hazard"
           ~units:"(fraction)"
           ~value:
             (Rrd.VT_Float
                (Int64.to_float ri.Xenctrl.Runstateinfo.V2.time2 /. 1.0e9)
             )
           ~description:
             "Fraction of time that some vCPUs are running and some are \
              runnable"
           ~ty:Rrd.Derive ~default:false ~min:0.0 ~max:1.0 ()
       )
    :: ( Rrd.VM uuid
       , Ds.ds_make ~name:"runstate_blocked" ~units:"(fraction)"
           ~value:
             (Rrd.VT_Float
                (Int64.to_float ri.Xenctrl.Runstateinfo.V2.time3 /. 1.0e9)
             )
           ~description:
             "Fraction of time that all vCPUs are blocked or offline"
           ~ty:Rrd.Derive ~default:false ~min:0.0 ~max:1.0 ()
       )
    :: ( Rrd.VM uuid
       , Ds.ds_make ~name:"runstate_partial_run" ~units:"(fraction)"
           ~value:
             (Rrd.VT_Float
                (Int64.to_float ri.Xenctrl.Runstateinfo.V2.time4 /. 1.0e9)
             )
           ~description:
             "Fraction of time that some vCPUs are running and some are \
              blocked"
           ~ty:Rrd.Derive ~default:false ~min:0.0 ~max:1.0 ()
       )
    :: ( Rrd.VM uuid
       , Ds.ds_make ~name:"runstate_partial_contention"
           ~units:"(fraction)"
           ~value:
             (Rrd.VT_Float
                (Int64.to_float ri.Xenctrl.Runstateinfo.V2.time5 /. 1.0e9)
             )
           ~description:
             "Fraction of time that some vCPUs are runnable and some are \
              blocked"
           ~ty:Rrd.Derive ~default:false ~min:0.0 ~max:1.0 ()
       )
    :: ( Rrd.VM uuid
       , Ds.ds_make ~name:"runnable_any" ~units:"(fraction)"
           ~value:
             (Rrd.VT_Float
                (Int64.to_float
                   (ri.Xenctrl.Runstateinfo.V2.time1
                   ++ ri.Xenctrl.Runstateinfo.V2.time2
                   ++ ri.Xenctrl.Runstateinfo.V2.time5
                   )
                /. 1.0e9
                )
             )
           ~description:
             "Fraction of time that at least one vCPU is runnable in the \
              domain"
           ~ty:Rrd.Derive ~default:false ~min:0.0 ~max:1.0 ()
       )
    :: ( Rrd.VM uuid
       , Ds.ds_make
           ~name:(Printf.sprintf "cpu_usage")
           ~units:"(fraction)"
           ~description:(Printf.sprintf "Domain CPU usage")
           ~value:(Rrd.VT_Float dom_cpu_time) ~ty:Rrd.Derive ~default:true
           ~min:0.0 ~max:1.0 ()
       )
    :: dss
    @ runnable_vcpus_ds
    @ nonaffine_vcpus_ds
  with _ -> dss
