-- Seed: 4234100583836163668,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity nhnyam is
  port (gyjgux : inout std_logic_vector(3 downto 4); lzusz : inout std_logic_vector(0 downto 0));
end nhnyam;

architecture xb of nhnyam is
  
begin
  -- Multi-driven assignments
  lzusz <= "W";
  lzusz <= "W";
end xb;

entity a is
  port (ckjavrxzz : linkage boolean; ztiujrjp : out time; ofsnax : out bit);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture ipsowmyucm of a is
  signal gt : std_logic_vector(0 downto 0);
  signal y : std_logic_vector(3 downto 4);
  signal bwmxhhfjlf : std_logic_vector(0 downto 0);
  signal imy : std_logic_vector(3 downto 4);
begin
  xrrouymxil : entity work.nhnyam
    port map (gyjgux => imy, lzusz => bwmxhhfjlf);
  txibgl : entity work.nhnyam
    port map (gyjgux => y, lzusz => gt);
  
  -- Single-driven assignments
  ofsnax <= '1';
  ztiujrjp <= 3_1_2_1 ps;
  
  -- Multi-driven assignments
  imy <= (others => '0');
  y <= "";
  y <= (others => '0');
  imy <= (others => '0');
end ipsowmyucm;

library ieee;
use ieee.std_logic_1164.all;

entity ha is
  port (q : inout character; bxghaogrg : inout std_logic);
end ha;

library ieee;
use ieee.std_logic_1164.all;

architecture rjadlm of ha is
  signal guuieba : std_logic_vector(3 downto 4);
  signal lwk : bit;
  signal bdgqycw : time;
  signal gy : boolean;
  signal u : std_logic_vector(0 downto 0);
  signal js : std_logic_vector(3 downto 4);
begin
  nmwajaj : entity work.nhnyam
    port map (gyjgux => js, lzusz => u);
  pmxvdzrdrm : entity work.a
    port map (ckjavrxzz => gy, ztiujrjp => bdgqycw, ofsnax => lwk);
  hfg : entity work.nhnyam
    port map (gyjgux => js, lzusz => u);
  e : entity work.nhnyam
    port map (gyjgux => guuieba, lzusz => u);
  
  -- Single-driven assignments
  q <= 'm';
end rjadlm;



-- Seed after: 15696020543044652952,6697892553037813751
