-- Seed: 5390740872399366196,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity folgap is
  port (m : buffer std_logic; ytaltai : inout time; oqtqmiu : in std_logic_vector(1 to 0));
end folgap;

architecture ob of folgap is
  
begin
  -- Single-driven assignments
  ytaltai <= 1_2_3_3_3.11340 ns;
  
  -- Multi-driven assignments
  m <= 'H';
end ob;

library ieee;
use ieee.std_logic_1164.all;

entity kutn is
  port (pevabto : in time; zmkr : out std_logic_vector(0 downto 2); vpok : buffer time_vector(1 downto 1); ayefcl : linkage time);
end kutn;

library ieee;
use ieee.std_logic_1164.all;

architecture t of kutn is
  signal sfezro : time;
  signal mwmlnm : std_logic;
  signal phtsx : time;
  signal kdb : std_logic_vector(1 to 0);
  signal myunwbleu : time;
  signal ttsonhb : time;
  signal inmkrr : std_logic;
begin
  nvcqctsus : entity work.folgap
    port map (m => inmkrr, ytaltai => ttsonhb, oqtqmiu => zmkr);
  wrvw : entity work.folgap
    port map (m => inmkrr, ytaltai => myunwbleu, oqtqmiu => kdb);
  mnxyutap : entity work.folgap
    port map (m => inmkrr, ytaltai => phtsx, oqtqmiu => zmkr);
  ppgyzlexr : entity work.folgap
    port map (m => mwmlnm, ytaltai => sfezro, oqtqmiu => zmkr);
  
  -- Single-driven assignments
  vpok <= (others => 3_0_3_3_4.41142 ns);
  
  -- Multi-driven assignments
  zmkr <= (others => '0');
  inmkrr <= 'W';
end t;



-- Seed after: 5585901510539166911,17047277710231705797
