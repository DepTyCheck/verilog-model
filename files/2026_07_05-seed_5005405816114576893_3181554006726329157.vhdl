-- Seed: 5005405816114576893,3181554006726329157

use std.reflection.all;

entity lfl is
  port (slph : inout time; ypazp : inout physical_value_mirror);
end lfl;

architecture rtdauvpx of lfl is
  
begin
  -- Single-driven assignments
  slph <= 1 hr;
end rtdauvpx;

use std.reflection.all;

entity csfct is
  port (eawslup : inout access_subtype_mirror; jrgij : inout floating_value_mirror);
end csfct;

architecture zagpwvn of csfct is
  
begin
  
end zagpwvn;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity oegrvy is
  port (lo : linkage std_logic; j : linkage integer; bmzkfgi : inout enumeration_subtype_mirror);
end oegrvy;

use std.reflection.all;

architecture lbozm of oegrvy is
  shared variable llfdeksz : physical_value_mirror;
  signal vvk : time;
  shared variable l : physical_value_mirror;
  signal ghbhecq : time;
  shared variable paud : physical_value_mirror;
  signal txnqh : time;
  shared variable om : floating_value_mirror;
  shared variable xpusjth : access_subtype_mirror;
begin
  io : entity work.csfct
    port map (eawslup => xpusjth, jrgij => om);
  m : entity work.lfl
    port map (slph => txnqh, ypazp => paud);
  ho : entity work.lfl
    port map (slph => ghbhecq, ypazp => l);
  a : entity work.lfl
    port map (slph => vvk, ypazp => llfdeksz);
end lbozm;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity pflj is
  port (gxoqju : inout floating_subtype_mirror; yrf : inout floating_subtype_mirror; g : out std_logic);
end pflj;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture f of pflj is
  shared variable pi : floating_value_mirror;
  shared variable iapaei : access_subtype_mirror;
  shared variable czzljrgiyu : floating_value_mirror;
  shared variable vgegw : access_subtype_mirror;
  shared variable dhjj : enumeration_subtype_mirror;
  signal my : integer;
  shared variable bhoxrnbhk : enumeration_subtype_mirror;
  signal szvphjm : integer;
  signal ir : std_logic;
begin
  kzqoidvti : entity work.oegrvy
    port map (lo => ir, j => szvphjm, bmzkfgi => bhoxrnbhk);
  heckic : entity work.oegrvy
    port map (lo => g, j => my, bmzkfgi => dhjj);
  cpewasdr : entity work.csfct
    port map (eawslup => vgegw, jrgij => czzljrgiyu);
  mlchlcv : entity work.csfct
    port map (eawslup => iapaei, jrgij => pi);
end f;



-- Seed after: 14017340416328695251,3181554006726329157
