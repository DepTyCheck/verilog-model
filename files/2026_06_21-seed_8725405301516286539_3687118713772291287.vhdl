-- Seed: 8725405301516286539,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity vqnt is
  port (ml : inout std_logic_vector(2 to 3); n : in bit_vector(2 downto 2));
end vqnt;

architecture dpkwcdkj of vqnt is
  
begin
  -- Multi-driven assignments
  ml <= ('L', 'Z');
  ml <= "0H";
end dpkwcdkj;

library ieee;
use ieee.std_logic_1164.all;

entity rjzzx is
  port (rowiea : buffer std_logic; qofnlb : inout real; z : linkage time; ddvvhcl : buffer time);
end rjzzx;

library ieee;
use ieee.std_logic_1164.all;

architecture menvvt of rjzzx is
  signal gnmuccitne : bit_vector(2 downto 2);
  signal fhylyd : std_logic_vector(2 to 3);
begin
  ccwlvc : entity work.vqnt
    port map (ml => fhylyd, n => gnmuccitne);
  ljsfbg : entity work.vqnt
    port map (ml => fhylyd, n => gnmuccitne);
  
  -- Single-driven assignments
  ddvvhcl <= 2#00.1_0_0# ps;
  gnmuccitne <= (others => '1');
  qofnlb <= 16#DFBF.10#;
  
  -- Multi-driven assignments
  rowiea <= 'U';
  rowiea <= '1';
  rowiea <= '0';
  rowiea <= 'X';
end menvvt;



-- Seed after: 2741337921042842275,3687118713772291287
