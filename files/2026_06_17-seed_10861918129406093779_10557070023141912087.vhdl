-- Seed: 10861918129406093779,10557070023141912087

entity rsnoe is
  port (lemqlch : buffer time_vector(3 downto 2); hasnxb : in time);
end rsnoe;

architecture nhqmeo of rsnoe is
  
begin
  -- Single-driven assignments
  lemqlch <= (8#70330# us, 2#1# fs);
end nhqmeo;

library ieee;
use ieee.std_logic_1164.all;

entity uelgkij is
  port (ktkcc : out std_logic_vector(2 to 4); uh : in std_logic_vector(2 to 2); ebglwdc : out severity_level; hbhwhyjocm : out integer);
end uelgkij;

architecture xpfvlkht of uelgkij is
  
begin
  -- Single-driven assignments
  hbhwhyjocm <= 431;
  ebglwdc <= FAILURE;
  
  -- Multi-driven assignments
  ktkcc <= ('1', '1', 'W');
  ktkcc <= "WL-";
  ktkcc <= "LX-";
  ktkcc <= "10H";
end xpfvlkht;

library ieee;
use ieee.std_logic_1164.all;

entity rrtcnndula is
  port (deqqaaxh : in std_logic; ddzrgj : buffer std_logic; asgeeboaj : linkage std_logic_vector(3 downto 3));
end rrtcnndula;

library ieee;
use ieee.std_logic_1164.all;

architecture x of rrtcnndula is
  signal uteoa : integer;
  signal zgpvplvbt : severity_level;
  signal ypuvx : std_logic_vector(2 to 2);
  signal fcvmrsgldw : std_logic_vector(2 to 4);
  signal xjb : time;
  signal r : time_vector(3 downto 2);
begin
  ta : entity work.rsnoe
    port map (lemqlch => r, hasnxb => xjb);
  kjsfpl : entity work.uelgkij
    port map (ktkcc => fcvmrsgldw, uh => ypuvx, ebglwdc => zgpvplvbt, hbhwhyjocm => uteoa);
  
  -- Multi-driven assignments
  ddzrgj <= '0';
end x;

library ieee;
use ieee.std_logic_1164.all;

entity ogyzhk is
  port (j : buffer std_logic_vector(4 downto 3); tf : in bit_vector(1 to 0); uyhyyjmz : in std_logic; uarvyad : buffer boolean_vector(3 downto 3));
end ogyzhk;

library ieee;
use ieee.std_logic_1164.all;

architecture oencsqzsqt of ogyzhk is
  signal k : std_logic_vector(3 downto 3);
  signal hvcsjujko : std_logic;
  signal fxvqccdbqm : time_vector(3 downto 2);
  signal ftaxpj : std_logic_vector(3 downto 3);
  signal loww : std_logic;
  signal zczczeylex : time;
  signal graujy : time_vector(3 downto 2);
begin
  n : entity work.rsnoe
    port map (lemqlch => graujy, hasnxb => zczczeylex);
  zekgthq : entity work.rrtcnndula
    port map (deqqaaxh => loww, ddzrgj => loww, asgeeboaj => ftaxpj);
  c : entity work.rsnoe
    port map (lemqlch => fxvqccdbqm, hasnxb => zczczeylex);
  kfitpv : entity work.rrtcnndula
    port map (deqqaaxh => uyhyyjmz, ddzrgj => hvcsjujko, asgeeboaj => k);
  
  -- Single-driven assignments
  uarvyad <= (others => FALSE);
  zczczeylex <= 16#B2C# ns;
end oencsqzsqt;



-- Seed after: 700334175352198818,10557070023141912087
