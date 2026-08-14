-- Seed: 9376366981612317748,8437298063418820479

entity jpbfsaciol is
  port (dsgr : buffer real_vector(4 downto 2); sfikvxtv : out time_vector(1 downto 0); b : inout boolean);
end jpbfsaciol;

architecture df of jpbfsaciol is
  
begin
  -- Single-driven assignments
  sfikvxtv <= (2#01.1_1# ps, 22 ps);
  dsgr <= (01244.2, 2#1.1_0_1_0#, 03244.0_3);
  b <= FALSE;
end df;

library ieee;
use ieee.std_logic_1164.all;

entity rahva is
  port (ydz : out std_logic_vector(4 to 3); fngl : out std_logic; gmio : out std_logic);
end rahva;

architecture jrdl of rahva is
  signal ddmnz : boolean;
  signal j : time_vector(1 downto 0);
  signal xcdfj : real_vector(4 downto 2);
begin
  fal : entity work.jpbfsaciol
    port map (dsgr => xcdfj, sfikvxtv => j, b => ddmnz);
  
  -- Multi-driven assignments
  gmio <= gmio;
  ydz <= "";
  ydz <= (others => '0');
end jrdl;



-- Seed after: 7674794100117502069,8437298063418820479
