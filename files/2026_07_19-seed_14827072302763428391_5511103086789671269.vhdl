-- Seed: 14827072302763428391,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity nnlrjml is
  port (rvbsdikfl : buffer bit_vector(1 downto 0); efueadbq : in std_logic);
end nnlrjml;

architecture iqcuqyjcow of nnlrjml is
  
begin
  
end iqcuqyjcow;

entity dvcbj is
  port (nmdovjoq : out character);
end dvcbj;

library ieee;
use ieee.std_logic_1164.all;

architecture rwqlosvm of dvcbj is
  signal jtxjfy : bit_vector(1 downto 0);
  signal hri : std_logic;
  signal vquwubgq : bit_vector(1 downto 0);
  signal aqox : std_logic;
  signal mvddiikdk : bit_vector(1 downto 0);
begin
  r : entity work.nnlrjml
    port map (rvbsdikfl => mvddiikdk, efueadbq => aqox);
  dqv : entity work.nnlrjml
    port map (rvbsdikfl => vquwubgq, efueadbq => hri);
  rnammq : entity work.nnlrjml
    port map (rvbsdikfl => jtxjfy, efueadbq => hri);
  
  -- Single-driven assignments
  nmdovjoq <= 'u';
  
  -- Multi-driven assignments
  aqox <= aqox;
  hri <= aqox;
  aqox <= '0';
  aqox <= aqox;
end rwqlosvm;



-- Seed after: 13489698669939379687,5511103086789671269
