-- Seed: 14093812408322705068,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity scloltk is
  port (maipro : buffer bit; eflg : in std_logic_vector(1 downto 2); dt : linkage std_logic);
end scloltk;

architecture hmlsggwu of scloltk is
  
begin
  -- Single-driven assignments
  maipro <= '1';
end hmlsggwu;

entity swyckh is
  port (sazzt : buffer real; yjdewv : out time; ztd : inout bit_vector(4 to 3));
end swyckh;

library ieee;
use ieee.std_logic_1164.all;

architecture g of swyckh is
  signal dfloy : bit;
  signal zwwr : std_logic;
  signal sniyc : std_logic_vector(1 downto 2);
  signal vwwgp : bit;
begin
  phoxf : entity work.scloltk
    port map (maipro => vwwgp, eflg => sniyc, dt => zwwr);
  iamqfxpg : entity work.scloltk
    port map (maipro => dfloy, eflg => sniyc, dt => zwwr);
  
  -- Single-driven assignments
  ztd <= (others => '0');
  yjdewv <= 2#0010.0_0_1_0# fs;
  
  -- Multi-driven assignments
  sniyc <= (others => '0');
  zwwr <= 'W';
end g;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (nr : in std_logic; sbwlocf : inout bit);
end a;

architecture ygcylkslp of a is
  
begin
  -- Single-driven assignments
  sbwlocf <= '0';
end ygcylkslp;

library ieee;
use ieee.std_logic_1164.all;

entity ufxi is
  port (cvkaix : buffer std_logic; hw : buffer integer);
end ufxi;

library ieee;
use ieee.std_logic_1164.all;

architecture qgxrrfsheb of ufxi is
  signal lqr : std_logic;
  signal poncvbgab : std_logic_vector(1 downto 2);
  signal qw : bit;
  signal v : std_logic;
  signal m : std_logic_vector(1 downto 2);
  signal xtepalz : bit;
begin
  znhrnkp : entity work.scloltk
    port map (maipro => xtepalz, eflg => m, dt => v);
  nozxdx : entity work.scloltk
    port map (maipro => qw, eflg => poncvbgab, dt => lqr);
  
  -- Multi-driven assignments
  lqr <= 'W';
  m <= "";
  cvkaix <= '1';
end qgxrrfsheb;



-- Seed after: 41250355440038160,17924494779688682807
