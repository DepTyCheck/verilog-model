-- Seed: 10526543547740718491,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity qpiq is
  port (yjvly : out std_logic_vector(4 downto 0); qkp : buffer real; eos : linkage bit; dndbuay : in time);
end qpiq;

architecture ubkbbrxf of qpiq is
  
begin
  -- Single-driven assignments
  qkp <= 2.2411;
end ubkbbrxf;

entity nvsw is
  port (idvhilq : in time);
end nvsw;

library ieee;
use ieee.std_logic_1164.all;

architecture js of nvsw is
  signal dntusy : bit;
  signal g : real;
  signal jqecrxcr : std_logic_vector(4 downto 0);
  signal upb : time;
  signal fnhka : bit;
  signal zhsiqv : real;
  signal xbonhihi : std_logic_vector(4 downto 0);
begin
  atwu : entity work.qpiq
    port map (yjvly => xbonhihi, qkp => zhsiqv, eos => fnhka, dndbuay => upb);
  xhn : entity work.qpiq
    port map (yjvly => jqecrxcr, qkp => g, eos => dntusy, dndbuay => idvhilq);
  
  -- Multi-driven assignments
  xbonhihi <= xbonhihi;
  jqecrxcr <= xbonhihi;
  xbonhihi <= ('H', '0', 'W', '0', '1');
end js;



-- Seed after: 7452701266904277517,1112937151005418631
