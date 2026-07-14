-- Seed: 4297199877145418262,7726014785203345639

use std.reflection.all;

entity rcd is
  port (ya : inout access_subtype_mirror; b : inout integer_subtype_mirror);
end rcd;

architecture zdyph of rcd is
  
begin
  
end zdyph;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity dtja is
  port (nhwaxplr : linkage boolean; zhafy : inout time; xdywefgu : buffer std_logic; rqzckfbki : inout protected_value_mirror);
end dtja;

architecture tlxvvj of dtja is
  
begin
  -- Single-driven assignments
  zhafy <= zhafy;
  
  -- Multi-driven assignments
  xdywefgu <= 'W';
end tlxvvj;

use std.reflection.all;

entity qluwiciwf is
  port (hzvhyqs : inout access_value_mirror; onyy : inout access_value_mirror; qryhsxc : linkage boolean);
end qluwiciwf;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture ndcvv of qluwiciwf is
  shared variable w : integer_subtype_mirror;
  shared variable aflv : access_subtype_mirror;
  shared variable rvu : integer_subtype_mirror;
  shared variable mrxcwbz : access_subtype_mirror;
  shared variable friiuxnx : integer_subtype_mirror;
  shared variable afbphrc : access_subtype_mirror;
  shared variable wg : protected_value_mirror;
  signal sgxhaws : std_logic;
  signal car : time;
  signal iul : boolean;
begin
  k : entity work.dtja
    port map (nhwaxplr => iul, zhafy => car, xdywefgu => sgxhaws, rqzckfbki => wg);
  ltmfwbf : entity work.rcd
    port map (ya => afbphrc, b => friiuxnx);
  a : entity work.rcd
    port map (ya => mrxcwbz, b => rvu);
  ldyrupdtkt : entity work.rcd
    port map (ya => aflv, b => w);
  
  -- Multi-driven assignments
  sgxhaws <= sgxhaws;
  sgxhaws <= sgxhaws;
  sgxhaws <= sgxhaws;
end ndcvv;



-- Seed after: 15094482644486570208,7726014785203345639
