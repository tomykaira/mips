open OUnit
open Definition
open HeapAllocation
open Entity
open Schedule

let test_construct_dependency_graph =
  [
    TestCase (fun ()->
      let inst1 = E (2, Assignment ("c.2", Const (IntVal 2))) in
      let inst2 = E (3, Assignment ("c.3", Add ("c.1", "c.2"))) in
      let insts = [inst1; inst2] in
      let edges = construct_dependency_graph insts in
      assert_equal [{ source = Inst inst2; destination = Dummy; latency = 0; priority = 2; selected = false };
                    { source = Inst inst1; destination = Inst inst2; latency = 0; priority = 1; selected = false }]
        edges);
    TestCase (fun ()->
      let inst1 = E (2, Assignment ("c.2", Const (IntVal 2))) in
      let inst2 = E (3, Assignment ("c.3", Const (IntVal 3))) in
      let inst3 = E (4, Assignment ("c.4", Add("c.2", "c.3"))) in
      let insts = [inst1; inst2; inst3] in
      let edges = construct_dependency_graph insts in
      assert_equal [{ source = Inst inst3; destination = Dummy; latency = 0; priority = 2; selected = false };
                    { source = Inst inst1; destination = Inst inst3; latency = 0; priority = 1; selected = false };
                    { source = Inst inst2; destination = Inst inst3; latency = 0; priority = 1; selected = false }]
        edges);
    TestCase (fun ()->
      let inst1 = E (2, Assignment ("c.2", Const (IntVal 2))) in
      let inst2 = E (3, Assignment ("c.3", Add("c.1", "c.2"))) in
      let inst3 = E (4, Assignment ("c.4", Add("c.2", "c.8"))) in
      let insts = [inst1; inst2; inst3] in
      let edges = construct_dependency_graph insts in
      assert_equal [{ source = Inst inst2; destination = Dummy; latency = 0; priority = 2; selected = false };
                    { source = Inst inst3; destination = Dummy; latency = 0; priority = 2; selected = false };
                    { source = Inst inst1; destination = Inst inst3; latency = 0; priority = 1; selected = false };
                    { source = Inst inst1; destination = Inst inst2; latency = 0; priority = 1; selected = false }]
        edges)
  ]


let test_ready_set =
  [
    TestCase (fun ()->
      let inst1 = E (2, Assignment ("c.2", Const (IntVal 2))) in
      let inst2 = E (3, Assignment ("c.3", Const (IntVal 3))) in
      let inst3 = E (4, Assignment ("c.4", Add("c.2", "c.3"))) in
      let insts = [inst1; inst2; inst3] in
      let graph = construct_dependency_graph insts in
      assert_equal [Inst inst1; Inst inst2] (S.elements (ready_set graph)));
    TestCase (fun ()->
      let inst2 = Inst(E (3, Assignment ("c.3", Const (IntVal 3)))) in
      let inst3 = Inst(E (4, Assignment ("c.4", Add("c.2", "c.3")))) in
      let graph = [{source =inst2;
                    destination =inst3;
                    latency =0; priority =1; selected =false}] in
      assert_equal [inst2] (S.elements (ready_set graph)));
    TestCase (fun ()->
      let inst3 = Inst(E (4, Assignment ("c.4", Add("c.2", "c.3")))) in
      let graph = [{source =Dummy;
                    destination =inst3;
                    latency =0; priority =1; selected =false}] in
      assert_equal [] (S.elements (ready_set graph)));
    TestCase (fun () ->
      let inst1 = Inst (E (4, StoreHeapImm ("store.9", 0))) in
      let inst2 = Inst (E (5, Assignment ("load.10", LoadHeapImm 0))) in
      let graph = [{source =inst1; destination =Dummy; latency =0; priority =3; selected =false};
                   {source =inst2; destination =Dummy; latency =0; priority =1; selected =false};
                   {source =Dummy; destination =inst1; latency =1; priority =2; selected =true}] in
      assert_equal [inst2] (S.elements (ready_set graph)));
    TestCase (fun () ->
      let inst1 = Inst (E (4, StoreHeapImm ("store.9", 0))) in
      let inst2 = Inst (E (5, Assignment ("load.10", LoadHeapImm 0))) in
      let graph = [{source =inst1; destination =Dummy; latency =0; priority =3; selected =false};
                   {source =inst2; destination =Dummy; latency =0; priority =1; selected =false}] in
      assert_equal [inst1; inst2] (S.elements (ready_set graph)))
  ]


let test_select =
  [
    TestCase (fun ()->
      let inst1 = E (2, Assignment ("c.2", Const (IntVal 2))) in
      let inst2 = E (3, Assignment ("c.3", Const (IntVal 3))) in
      let inst3 = E (4, Assignment ("c.4", Add("c.2", "c.3"))) in
      let insts = [inst1; inst2; inst3] in
      let graph = construct_dependency_graph insts in
      let ready_insts = ready_set graph in
      assert_equal inst2 (select graph ready_insts));  (* inst1 is also ok *)
    TestCase (fun () ->
      let inst1 = Inst (E (4, StoreHeapImm ("store.9", 0))) in
      let inst2 = E (5, Assignment ("load.10", LoadHeapImm 0)) in
      let graph = [{source =inst1; destination =Dummy; latency =0; priority =3; selected =false};
                   {source =Inst inst2; destination =Dummy; latency =0; priority =1; selected =false};
                   {source =Dummy; destination =inst1; latency =1; priority =2; selected =true}] in
      assert_equal inst2 (select graph (ready_set graph)))
  ]
let test_tick =
  [
    TestCase (fun () ->
      let inst1 = Inst (E (4, StoreHeapImm ("store.9", 0))) in
      let inst2 = Inst (E (5, Assignment ("load.10", LoadHeapImm 0))) in
      let graph = [{source =inst1; destination =Dummy; latency =0; priority =3; selected =false};
                   {source =inst2; destination =Dummy; latency =0; priority =1; selected =false};
                   {source =Dummy; destination =inst1; latency =1; priority =2; selected =true}] in
      let next_graph = tick_without_selection graph in
      print_endline (Show.show<edge list> next_graph);
      assert_equal [{source =inst1; destination =Dummy; latency =0; priority =3; selected =false};
                    {source =inst2; destination =Dummy; latency =0; priority =1; selected =false};
                    {source =Dummy; destination =inst1; latency =0; priority =2; selected =true}] next_graph
    );
    TestCase (fun () ->
      let inst1 = Inst (E (4, StoreHeapImm ("store.9", 0))) in
      let inst2 = Inst (E (5, Assignment ("load.10", LoadHeapImm 0))) in
      let graph = [{source =inst1; destination =Dummy; latency =0; priority =3; selected =false};
                   {source =inst2; destination =Dummy; latency =0; priority =1; selected =false};
                   {source =Dummy; destination =inst1; latency =0; priority =2; selected =true}] in
      let next_graph = tick_without_selection graph in
      print_endline (Show.show<edge list> next_graph);
      assert_equal [{source =inst1; destination =Dummy; latency =0; priority =3; selected =false};
                    {source =inst2; destination =Dummy; latency =0; priority =1; selected =false}] next_graph
    )
  ]

let test_convert_block =
  [
    TestCase (fun ()->
      let inst1 = E (2, Assignment ("c.2", Const (IntVal 2))) in
      let inst2 = E (3, Assignment ("c.3", Add ("c.1", "c.2"))) in
      let insts = [inst1; inst2] in
      assert_equal [inst1; inst2] (convert_block insts));
    TestCase (fun ()->
      let inst1 = E (2, Assignment ("c.2", Const (IntVal 2))) in
      let inst2 = E (3, Assignment ("c.3", Const (IntVal 3))) in
      let inst3 = E (4, Assignment ("c.4", Add("c.2", "c.3"))) in
      let insts = [inst1; inst2; inst3] in
      let result = (convert_block insts) in
      print_endline (Show.show<instruction Entity.entity list> result);
      assert_equal [inst2; inst1; inst3] (convert_block insts))
  ]

let tests =
  TestList (test_construct_dependency_graph
            @ test_ready_set
            @ test_convert_block
            @ test_select
            @ test_tick)

let _ =
  run_test_tt_main tests
