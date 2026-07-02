-- Seed: 15501534549917444530,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity vjm is
  port (hqgo : out std_logic; pxxi : inout physical_subtype_mirror; zvdfyuss : inout access_value_mirror);
end vjm;

architecture fna of vjm is
  
begin
  
end fna;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity egpmgbxg is
  port (ujcmrk : inout record_value_mirror; zv : in std_logic_vector(3 to 4); zypwlw : in integer_vector(2 downto 2));
end egpmgbxg;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture qila of egpmgbxg is
  shared variable mvphgdnen : access_value_mirror;
  shared variable ciwjelihh : physical_subtype_mirror;
  shared variable fsv : access_value_mirror;
  shared variable ukpr : physical_subtype_mirror;
  signal neyigz : std_logic;
begin
  naihbxjv : entity work.vjm
    port map (hqgo => neyigz, pxxi => ukpr, zvdfyuss => fsv);
  roxzxgxt : entity work.vjm
    port map (hqgo => neyigz, pxxi => ciwjelihh, zvdfyuss => mvphgdnen);
  
  -- Multi-driven assignments
  neyigz <= '1';
  neyigz <= '-';
end qila;

use std.reflection.all;

entity bevpmqqis is
  port (bwltckqiq : inout access_subtype_mirror; ltx : buffer boolean; cic : out boolean);
end bevpmqqis;

architecture sedfevgzk of bevpmqqis is
  
begin
  -- Single-driven assignments
  cic <= cic;
  ltx <= ltx;
end sedfevgzk;

use std.reflection.all;

entity xgoodu is
  port (adzx : inout record_value_mirror);
end xgoodu;

architecture sljkykibp of xgoodu is
  
begin
  
end sljkykibp;



-- Seed after: 10920108238427988983,14426950258250697445
