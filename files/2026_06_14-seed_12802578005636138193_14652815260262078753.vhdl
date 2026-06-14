-- Seed: 12802578005636138193,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity zkfceczax is
  port (rsmpkabsqm : out std_logic; wec : linkage time; tjsnkjy : buffer std_logic_vector(3 downto 1); vlwbao : out string(3 downto 4));
end zkfceczax;

architecture c of zkfceczax is
  
begin
  -- Single-driven assignments
  vlwbao <= "";
  
  -- Multi-driven assignments
  tjsnkjy <= ('1', '0', 'U');
  rsmpkabsqm <= 'U';
end c;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (qonwfvh : inout std_logic_vector(0 downto 1); mtumrobabh : out real);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture vhwftg of o is
  signal zwta : string(3 downto 4);
  signal ezopn : time;
  signal ecctq : string(3 downto 4);
  signal wjmo : std_logic_vector(3 downto 1);
  signal br : time;
  signal sqz : std_logic;
begin
  qlmzqch : entity work.zkfceczax
    port map (rsmpkabsqm => sqz, wec => br, tjsnkjy => wjmo, vlwbao => ecctq);
  ppgrim : entity work.zkfceczax
    port map (rsmpkabsqm => sqz, wec => ezopn, tjsnkjy => wjmo, vlwbao => zwta);
  
  -- Single-driven assignments
  mtumrobabh <= 16#C_2_D_A.AF9D#;
  
  -- Multi-driven assignments
  qonwfvh <= "";
  qonwfvh <= "";
  sqz <= 'Z';
  qonwfvh <= (others => '0');
end vhwftg;



-- Seed after: 7852348665957767547,14652815260262078753
