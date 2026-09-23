-- Seed: 3027793347414516491,8067602802092121131

entity zwaago is
  port (qdbysz : in bit_vector(3 downto 1));
end zwaago;

architecture hpalhjicvo of zwaago is
  
begin
  
end hpalhjicvo;

entity nvbljfve is
  port (eeyon : linkage integer);
end nvbljfve;

architecture jnx of nvbljfve is
  signal ajbqkxqbo : bit_vector(3 downto 1);
  signal bjcvcrus : bit_vector(3 downto 1);
begin
  derbyl : entity work.zwaago
    port map (qdbysz => bjcvcrus);
  pzustqcc : entity work.zwaago
    port map (qdbysz => bjcvcrus);
  dmctgom : entity work.zwaago
    port map (qdbysz => ajbqkxqbo);
  
  -- Single-driven assignments
  bjcvcrus <= ('1', '0', '1');
  ajbqkxqbo <= ('1', '1', '1');
end jnx;

library ieee;
use ieee.std_logic_1164.all;

entity dgp is
  port (lavd : inout string(5 to 2); qc : buffer std_logic; bpjmgtyjk : linkage std_logic_vector(0 downto 1); hwa : linkage std_logic);
end dgp;

architecture bpb of dgp is
  signal dl : integer;
  signal fe : integer;
  signal wu : bit_vector(3 downto 1);
  signal ftatmddc : bit_vector(3 downto 1);
begin
  wkbwb : entity work.zwaago
    port map (qdbysz => ftatmddc);
  svlqydabsj : entity work.zwaago
    port map (qdbysz => wu);
  xjxtxsup : entity work.nvbljfve
    port map (eeyon => fe);
  dcmefzzn : entity work.nvbljfve
    port map (eeyon => dl);
  
  -- Single-driven assignments
  wu <= ('0', '0', '0');
  ftatmddc <= ('1', '1', '1');
  lavd <= "";
  
  -- Multi-driven assignments
  qc <= qc;
  qc <= '0';
  qc <= qc;
end bpb;

entity tqbbg is
  port (d : buffer integer);
end tqbbg;

architecture pe of tqbbg is
  signal vuslyjtwzy : bit_vector(3 downto 1);
  signal zsocpc : bit_vector(3 downto 1);
  signal nkttnznmi : bit_vector(3 downto 1);
begin
  c : entity work.zwaago
    port map (qdbysz => nkttnznmi);
  zohneq : entity work.zwaago
    port map (qdbysz => zsocpc);
  s : entity work.zwaago
    port map (qdbysz => nkttnznmi);
  vgpcdqnn : entity work.zwaago
    port map (qdbysz => vuslyjtwzy);
  
  -- Single-driven assignments
  nkttnznmi <= ('1', '1', '1');
end pe;



-- Seed after: 5320575143401700319,8067602802092121131
