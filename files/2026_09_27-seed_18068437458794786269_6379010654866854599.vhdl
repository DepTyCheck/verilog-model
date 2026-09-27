-- Seed: 18068437458794786269,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity lpvdsjpuha is
  port (zvnbovafh : in std_logic);
end lpvdsjpuha;

architecture qw of lpvdsjpuha is
  
begin
  
end qw;

library ieee;
use ieee.std_logic_1164.all;

entity gebardaw is
  port (fwvb : linkage std_logic_vector(3 downto 2); eqk : inout std_logic);
end gebardaw;

library ieee;
use ieee.std_logic_1164.all;

architecture xlkfsi of gebardaw is
  signal ahxmyqpx : std_logic;
  signal vvqqewrx : std_logic;
begin
  ycbwnba : entity work.lpvdsjpuha
    port map (zvnbovafh => vvqqewrx);
  qxyfl : entity work.lpvdsjpuha
    port map (zvnbovafh => ahxmyqpx);
  nxywubemeg : entity work.lpvdsjpuha
    port map (zvnbovafh => eqk);
  
  -- Multi-driven assignments
  vvqqewrx <= eqk;
  eqk <= eqk;
  eqk <= vvqqewrx;
  eqk <= eqk;
end xlkfsi;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (vdn : inout std_logic; ylyxmp : in real; nez : inout time_vector(3 downto 3));
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture ugvqjr of r is
  signal kjc : std_logic;
begin
  kp : entity work.lpvdsjpuha
    port map (zvnbovafh => kjc);
  oni : entity work.lpvdsjpuha
    port map (zvnbovafh => vdn);
  npaxkuyw : entity work.lpvdsjpuha
    port map (zvnbovafh => vdn);
  
  -- Single-driven assignments
  nez <= nez;
end ugvqjr;



-- Seed after: 12008121996538243830,6379010654866854599
