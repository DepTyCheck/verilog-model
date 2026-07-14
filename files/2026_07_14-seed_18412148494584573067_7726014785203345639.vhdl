-- Seed: 18412148494584573067,7726014785203345639

use std.reflection.all;

entity evtkocf is
  port (bfexhyc : inout floating_subtype_mirror; ewoqspqrw : inout integer_subtype_mirror; j : in real_vector(2 downto 3); qa : in time);
end evtkocf;

architecture vi of evtkocf is
  
begin
  
end vi;

use std.reflection.all;

entity ae is
  port (y : inout boolean; sennlndfsx : inout real_vector(0 downto 2); kxm : inout subtype_mirror; k : inout subtype_mirror);
end ae;

use std.reflection.all;

architecture azufeo of ae is
  signal lb : time;
  shared variable wm : integer_subtype_mirror;
  shared variable eaubfvwf : floating_subtype_mirror;
begin
  htjztwviof : entity work.evtkocf
    port map (bfexhyc => eaubfvwf, ewoqspqrw => wm, j => sennlndfsx, qa => lb);
  
  -- Single-driven assignments
  lb <= lb;
  sennlndfsx <= (others => 0.0);
end azufeo;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity kruyy is
  port (kynzvkgj : linkage character; twdmcmdieb : inout access_value_mirror; zvoruo : linkage std_logic);
end kruyy;

use std.reflection.all;

architecture azxoj of kruyy is
  signal gtbrhgmpd : real_vector(2 downto 3);
  shared variable baaq : integer_subtype_mirror;
  shared variable nmt : floating_subtype_mirror;
  signal emzcojixbq : time;
  signal d : real_vector(2 downto 3);
  shared variable hghtohwqq : integer_subtype_mirror;
  shared variable snbtnpoj : floating_subtype_mirror;
begin
  pejgklbtau : entity work.evtkocf
    port map (bfexhyc => snbtnpoj, ewoqspqrw => hghtohwqq, j => d, qa => emzcojixbq);
  dkiwpdlkq : entity work.evtkocf
    port map (bfexhyc => nmt, ewoqspqrw => baaq, j => gtbrhgmpd, qa => emzcojixbq);
  
  -- Single-driven assignments
  emzcojixbq <= emzcojixbq;
  gtbrhgmpd <= (others => 0.0);
  d <= (others => 0.0);
end azxoj;

use std.reflection.all;

entity mspxvglvmg is
  port (ed : inout protected_value_mirror; hv : inout array_value_mirror; mz : in boolean);
end mspxvglvmg;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture zmlcd of mspxvglvmg is
  signal yif : time;
  shared variable nocb : integer_subtype_mirror;
  shared variable yaabyaedh : floating_subtype_mirror;
  signal nckgp : time;
  signal lhfdiep : real_vector(2 downto 3);
  shared variable mbdagty : integer_subtype_mirror;
  shared variable n : floating_subtype_mirror;
  signal wxkbu : std_logic;
  shared variable ptis : access_value_mirror;
  signal azczg : character;
  signal zuh : time;
  signal yncutkmjsg : real_vector(2 downto 3);
  shared variable b : integer_subtype_mirror;
  shared variable gjesgxxvrf : floating_subtype_mirror;
begin
  alvelvthm : entity work.evtkocf
    port map (bfexhyc => gjesgxxvrf, ewoqspqrw => b, j => yncutkmjsg, qa => zuh);
  yu : entity work.kruyy
    port map (kynzvkgj => azczg, twdmcmdieb => ptis, zvoruo => wxkbu);
  eenr : entity work.evtkocf
    port map (bfexhyc => n, ewoqspqrw => mbdagty, j => lhfdiep, qa => nckgp);
  tc : entity work.evtkocf
    port map (bfexhyc => yaabyaedh, ewoqspqrw => nocb, j => yncutkmjsg, qa => yif);
  
  -- Single-driven assignments
  lhfdiep <= yncutkmjsg;
  nckgp <= 4_1_1 fs;
  zuh <= 8#07050# fs;
  yncutkmjsg <= yncutkmjsg;
  
  -- Multi-driven assignments
  wxkbu <= wxkbu;
  wxkbu <= wxkbu;
end zmlcd;



-- Seed after: 5363132046141191582,7726014785203345639
