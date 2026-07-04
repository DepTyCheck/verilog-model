-- Seed: 3813546031331360593,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;

entity nkd is
  port (tkzwuaxsm : buffer real; nsaklybog : buffer severity_level; th : in std_logic_vector(2 downto 4); wz : out std_logic_vector(4 downto 4));
end nkd;

architecture jocxd of nkd is
  
begin
  -- Single-driven assignments
  nsaklybog <= nsaklybog;
  tkzwuaxsm <= 8#3.751#;
  
  -- Multi-driven assignments
  wz <= "0";
  wz <= "W";
end jocxd;

use std.reflection.all;

entity jcbb is
  port (sbzkedjs : in real; vy : inout bit; izefef : inout access_subtype_mirror; btg : inout array_value_mirror);
end jcbb;

architecture ojauf of jcbb is
  
begin
  -- Single-driven assignments
  vy <= '1';
end ojauf;

use std.reflection.all;

entity sytd is
  port (gvpa : inout protected_value_mirror; eu : inout access_value_mirror);
end sytd;

library ieee;
use ieee.std_logic_1164.all;

architecture tpt of sytd is
  signal lhs : std_logic_vector(4 downto 4);
  signal rxxydzbt : std_logic_vector(2 downto 4);
  signal pac : severity_level;
  signal tnuuafhq : real;
begin
  lzs : entity work.nkd
    port map (tkzwuaxsm => tnuuafhq, nsaklybog => pac, th => rxxydzbt, wz => lhs);
  
  -- Multi-driven assignments
  rxxydzbt <= "";
end tpt;

entity gcfrlz is
  port (rkvvjj : out integer);
end gcfrlz;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture gyqlhrl of gcfrlz is
  signal mnk : std_logic_vector(4 downto 4);
  signal knpl : std_logic_vector(2 downto 4);
  signal ikjt : severity_level;
  shared variable luww : array_value_mirror;
  shared variable qxvk : access_subtype_mirror;
  signal qm : bit;
  signal xszae : real;
  shared variable ttqcobaa : access_value_mirror;
  shared variable iacvm : protected_value_mirror;
begin
  zhcvq : entity work.sytd
    port map (gvpa => iacvm, eu => ttqcobaa);
  w : entity work.jcbb
    port map (sbzkedjs => xszae, vy => qm, izefef => qxvk, btg => luww);
  lmo : entity work.nkd
    port map (tkzwuaxsm => xszae, nsaklybog => ikjt, th => knpl, wz => mnk);
  
  -- Single-driven assignments
  rkvvjj <= 1_1_0;
end gyqlhrl;



-- Seed after: 16747170564304033506,6290177331721581829
