-- Seed: 10385604760225167953,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity dcz is
  port (fauxpiu : in std_logic_vector(1 to 1); jeygw : inout std_logic; bmxmkih : inout std_logic);
end dcz;

architecture zvpfqclj of dcz is
  
begin
  
end zvpfqclj;

library ieee;
use ieee.std_logic_1164.all;

entity ggaon is
  port (mtof : out integer; vixcegu : out std_logic_vector(3 downto 4));
end ggaon;

architecture hlzozja of ggaon is
  
begin
  -- Multi-driven assignments
  vixcegu <= "";
  vixcegu <= vixcegu;
end hlzozja;

entity lob is
  port (cmvftbm : buffer time_vector(2 to 1); rawocimdbm : in string(5 downto 2));
end lob;

library ieee;
use ieee.std_logic_1164.all;

architecture ryfx of lob is
  signal sxcgh : std_logic;
  signal q : std_logic_vector(1 to 1);
  signal tcxzcnss : std_logic;
  signal pkjpaorj : std_logic;
  signal fmhpmvyf : std_logic_vector(1 to 1);
  signal wjtzz : std_logic;
  signal xilj : std_logic_vector(1 to 1);
begin
  pkspzyl : entity work.dcz
    port map (fauxpiu => xilj, jeygw => wjtzz, bmxmkih => wjtzz);
  g : entity work.dcz
    port map (fauxpiu => fmhpmvyf, jeygw => pkjpaorj, bmxmkih => tcxzcnss);
  dd : entity work.dcz
    port map (fauxpiu => q, jeygw => sxcgh, bmxmkih => wjtzz);
  
  -- Single-driven assignments
  cmvftbm <= (others => 0 ns);
  
  -- Multi-driven assignments
  fmhpmvyf <= "H";
  xilj <= xilj;
  pkjpaorj <= wjtzz;
end ryfx;



-- Seed after: 4665840482092200143,16159265764638711791
