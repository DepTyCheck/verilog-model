-- Seed: 17509122392498432846,12011142928354116943

entity yjouito is
  port (kdtluexrxw : in time; yaedzlv : out time);
end yjouito;

architecture mxnmtln of yjouito is
  
begin
  -- Single-driven assignments
  yaedzlv <= 2#1_1.0101# fs;
end mxnmtln;

library ieee;
use ieee.std_logic_1164.all;

entity lxdqbxk is
  port (c : inout std_logic);
end lxdqbxk;

architecture yugxeb of lxdqbxk is
  signal kqcqevtibo : time;
  signal r : time;
  signal jizdqbg : time;
begin
  hkj : entity work.yjouito
    port map (kdtluexrxw => jizdqbg, yaedzlv => r);
  ufzi : entity work.yjouito
    port map (kdtluexrxw => jizdqbg, yaedzlv => kqcqevtibo);
  
  -- Single-driven assignments
  jizdqbg <= 2#010# ns;
  
  -- Multi-driven assignments
  c <= '1';
  c <= '1';
  c <= '1';
  c <= '-';
end yugxeb;

library ieee;
use ieee.std_logic_1164.all;

entity imybn is
  port (njq : out boolean; itqe : out std_logic_vector(0 to 3); ovaahxrmmr : out boolean);
end imybn;

architecture kbg of imybn is
  signal vrdqk : time;
  signal jitkvuxo : time;
  signal kyh : time;
  signal jg : time;
  signal ekuyiwtf : time;
begin
  ek : entity work.yjouito
    port map (kdtluexrxw => ekuyiwtf, yaedzlv => ekuyiwtf);
  dhnhzmafr : entity work.yjouito
    port map (kdtluexrxw => jg, yaedzlv => kyh);
  sn : entity work.yjouito
    port map (kdtluexrxw => ekuyiwtf, yaedzlv => jg);
  dviche : entity work.yjouito
    port map (kdtluexrxw => jitkvuxo, yaedzlv => vrdqk);
  
  -- Single-driven assignments
  ovaahxrmmr <= FALSE;
  jitkvuxo <= 120.2_4_0_1 ps;
  njq <= FALSE;
  
  -- Multi-driven assignments
  itqe <= "ZL11";
  itqe <= "0-L0";
  itqe <= "HZHU";
end kbg;



-- Seed after: 11953852015879182661,12011142928354116943
