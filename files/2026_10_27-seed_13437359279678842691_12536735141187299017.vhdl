-- Seed: 13437359279678842691,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity dqgo is
  port (y : linkage std_logic_vector(0 downto 4); q : linkage std_logic; zbvxhsb : buffer time; wkotpo : buffer bit);
end dqgo;

architecture uhstu of dqgo is
  
begin
  -- Single-driven assignments
  zbvxhsb <= zbvxhsb;
  wkotpo <= '1';
end uhstu;

library ieee;
use ieee.std_logic_1164.all;

entity zngovhfg is
  port (hgvqey : linkage std_logic_vector(4 downto 4); llmqrp : in bit);
end zngovhfg;

library ieee;
use ieee.std_logic_1164.all;

architecture klahiqhen of zngovhfg is
  signal c : bit;
  signal jcanhwkdg : time;
  signal hpmctbahhw : std_logic;
  signal mwc : bit;
  signal wrzyxf : time;
  signal drfciumbu : std_logic_vector(0 downto 4);
  signal fwolbbxd : bit;
  signal v : time;
  signal dyukhnzy : std_logic;
  signal jgvrwl : std_logic_vector(0 downto 4);
begin
  izmo : entity work.dqgo
    port map (y => jgvrwl, q => dyukhnzy, zbvxhsb => v, wkotpo => fwolbbxd);
  jnapm : entity work.dqgo
    port map (y => drfciumbu, q => dyukhnzy, zbvxhsb => wrzyxf, wkotpo => mwc);
  z : entity work.dqgo
    port map (y => jgvrwl, q => hpmctbahhw, zbvxhsb => jcanhwkdg, wkotpo => c);
end klahiqhen;

library ieee;
use ieee.std_logic_1164.all;

entity liiot is
  port (ix : buffer std_logic_vector(0 to 1); yyhkwwhv : out string(5 downto 5); ui : linkage time; pfquwhw : linkage integer);
end liiot;

library ieee;
use ieee.std_logic_1164.all;

architecture plauqyashi of liiot is
  signal spn : time;
  signal panfuac : std_logic;
  signal eag : std_logic_vector(0 downto 4);
  signal s : bit;
  signal smvu : std_logic_vector(4 downto 4);
begin
  ihnp : entity work.zngovhfg
    port map (hgvqey => smvu, llmqrp => s);
  rfjgbize : entity work.dqgo
    port map (y => eag, q => panfuac, zbvxhsb => spn, wkotpo => s);
  
  -- Single-driven assignments
  yyhkwwhv <= yyhkwwhv;
  
  -- Multi-driven assignments
  eag <= (others => '0');
  smvu <= smvu;
  panfuac <= panfuac;
  ix <= ix;
end plauqyashi;



-- Seed after: 17552617292995115648,12536735141187299017
