-- Seed: 1969798625647276541,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity tpzdmd is
  port (dehr : buffer std_logic_vector(0 downto 3));
end tpzdmd;

architecture abjjc of tpzdmd is
  
begin
  -- Multi-driven assignments
  dehr <= (others => '0');
end abjjc;

entity afdh is
  port (nxtpyjq : out integer);
end afdh;

library ieee;
use ieee.std_logic_1164.all;

architecture aaxpsvq of afdh is
  signal umfinc : std_logic_vector(0 downto 3);
begin
  zfzvhtqzep : entity work.tpzdmd
    port map (dehr => umfinc);
  gvms : entity work.tpzdmd
    port map (dehr => umfinc);
  dup : entity work.tpzdmd
    port map (dehr => umfinc);
  
  -- Multi-driven assignments
  umfinc <= (others => '0');
  umfinc <= (others => '0');
end aaxpsvq;

entity sdjb is
  port (me : linkage boolean; r : inout severity_level; fyqfw : in string(5 downto 1); wdvo : out real_vector(1 to 4));
end sdjb;

library ieee;
use ieee.std_logic_1164.all;

architecture uctqddvgw of sdjb is
  signal bfaxbpamk : std_logic_vector(0 downto 3);
begin
  cba : entity work.tpzdmd
    port map (dehr => bfaxbpamk);
  raxpx : entity work.tpzdmd
    port map (dehr => bfaxbpamk);
  ugcpkvlbbw : entity work.tpzdmd
    port map (dehr => bfaxbpamk);
  
  -- Single-driven assignments
  wdvo <= (2#01.1#, 16#841.6_B#, 2#1111.1#, 8#3.0#);
  
  -- Multi-driven assignments
  bfaxbpamk <= "";
end uctqddvgw;

entity h is
  port (u : in real; rc : inout severity_level);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture eborz of h is
  signal huiffsnu : real_vector(1 to 4);
  signal o : string(5 downto 1);
  signal vhj : severity_level;
  signal su : boolean;
  signal wuo : std_logic_vector(0 downto 3);
begin
  vu : entity work.tpzdmd
    port map (dehr => wuo);
  oiza : entity work.sdjb
    port map (me => su, r => vhj, fyqfw => o, wdvo => huiffsnu);
  
  -- Multi-driven assignments
  wuo <= "";
  wuo <= (others => '0');
  wuo <= "";
end eborz;



-- Seed after: 4021135352995043595,8118127366649987907
