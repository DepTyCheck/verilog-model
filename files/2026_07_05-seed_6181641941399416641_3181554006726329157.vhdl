-- Seed: 6181641941399416641,3181554006726329157

use std.reflection.all;

entity tizcjek is
  port (ofhtcnatzr : inout integer_value_mirror; iht : out integer; uazvbe : inout physical_value_mirror);
end tizcjek;

architecture xznjjlwj of tizcjek is
  
begin
  -- Single-driven assignments
  iht <= iht;
end xznjjlwj;

use std.reflection.all;

entity q is
  port (laxkf : out integer; bgtrwr : inout record_subtype_mirror; qb : buffer boolean; mnyunoxk : inout integer_value_mirror);
end q;

use std.reflection.all;

architecture ypsjncpple of q is
  shared variable xrqeu : physical_value_mirror;
  signal sajrujted : integer;
  shared variable gfyk : physical_value_mirror;
  signal ozrqswi : integer;
  shared variable qmdkp : integer_value_mirror;
begin
  msh : entity work.tizcjek
    port map (ofhtcnatzr => qmdkp, iht => ozrqswi, uazvbe => gfyk);
  ysxmzkty : entity work.tizcjek
    port map (ofhtcnatzr => mnyunoxk, iht => sajrujted, uazvbe => xrqeu);
  
  -- Single-driven assignments
  qb <= FALSE;
  laxkf <= 16#D_1#;
end ypsjncpple;

use std.reflection.all;

entity kwdn is
  port (auojzepy : inout physical_value_mirror; hfgnxh : inout enumeration_subtype_mirror; vvozx : out bit_vector(3 downto 3); gyyxg : inout boolean);
end kwdn;

use std.reflection.all;

architecture w of kwdn is
  signal jzchoowxvr : integer;
  shared variable nzfjmdccc : integer_value_mirror;
  shared variable hdyjaigqk : integer_value_mirror;
  shared variable tqcz : record_subtype_mirror;
  signal psbmzffwkt : integer;
begin
  rixxqpypro : entity work.q
    port map (laxkf => psbmzffwkt, bgtrwr => tqcz, qb => gyyxg, mnyunoxk => hdyjaigqk);
  sgga : entity work.tizcjek
    port map (ofhtcnatzr => nzfjmdccc, iht => jzchoowxvr, uazvbe => auojzepy);
end w;

use std.reflection.all;

entity u is
  port (ahthy : inout integer_subtype_mirror; rwdzpulow : inout array_subtype_mirror; qwnwp : inout array_subtype_mirror);
end u;

architecture nktuc of u is
  
begin
  
end nktuc;



-- Seed after: 12826995070447240399,3181554006726329157
