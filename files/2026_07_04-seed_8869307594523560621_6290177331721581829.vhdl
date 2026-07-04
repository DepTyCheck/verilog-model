-- Seed: 8869307594523560621,6290177331721581829

entity egtxqhqw is
  port (ebj : in integer; syrcrkhc : in integer);
end egtxqhqw;

architecture rbf of egtxqhqw is
  
begin
  
end rbf;

use std.reflection.all;

entity vmhvjtz is
  port (bzbyhspkop : inout subtype_mirror; b : inout file_value_mirror; bppzntdw : inout record_value_mirror);
end vmhvjtz;

architecture drglovd of vmhvjtz is
  
begin
  
end drglovd;

entity bpqp is
  port (lbp : in boolean_vector(0 to 0); be : in time_vector(4 to 2));
end bpqp;

architecture a of bpqp is
  signal zvjp : integer;
  signal dctyqlgmp : integer;
  signal hevdaj : integer;
  signal gigcmor : integer;
begin
  cfrajutjvl : entity work.egtxqhqw
    port map (ebj => gigcmor, syrcrkhc => hevdaj);
  sp : entity work.egtxqhqw
    port map (ebj => hevdaj, syrcrkhc => dctyqlgmp);
  rqnyzwkm : entity work.egtxqhqw
    port map (ebj => gigcmor, syrcrkhc => hevdaj);
  ynvldldusc : entity work.egtxqhqw
    port map (ebj => dctyqlgmp, syrcrkhc => zvjp);
  
  -- Single-driven assignments
  gigcmor <= gigcmor;
  zvjp <= 8#55464#;
end a;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity bckbdcm is
  port (wlgcnd : inout floating_value_mirror; axf : buffer std_logic_vector(2 to 1));
end bckbdcm;

use std.reflection.all;

architecture lky of bckbdcm is
  shared variable sqzfzgxpp : record_value_mirror;
  shared variable dldkt : file_value_mirror;
  shared variable yluxv : subtype_mirror;
begin
  kuebgraycs : entity work.vmhvjtz
    port map (bzbyhspkop => yluxv, b => dldkt, bppzntdw => sqzfzgxpp);
  
  -- Multi-driven assignments
  axf <= axf;
  axf <= (others => '0');
  axf <= "";
end lky;



-- Seed after: 11218584995807153017,6290177331721581829
