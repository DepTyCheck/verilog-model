-- Seed: 11926755068286686855,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity cev is
  port (vezpsqp : linkage std_logic_vector(4 to 1); hqu : buffer time_vector(3 to 3));
end cev;

architecture avxwvcx of cev is
  
begin
  -- Single-driven assignments
  hqu <= (others => 44 us);
end avxwvcx;

library ieee;
use ieee.std_logic_1164.all;

entity ylpgz is
  port (add : buffer std_logic; os : linkage integer);
end ylpgz;

architecture qcq of ylpgz is
  
begin
  -- Multi-driven assignments
  add <= 'H';
  add <= 'U';
  add <= 'H';
  add <= 'Z';
end qcq;

library ieee;
use ieee.std_logic_1164.all;

entity vehagtvt is
  port (ifgi : out real; rdihmvhral : inout real; qndzalj : inout boolean; tdgq : in std_logic_vector(0 downto 0));
end vehagtvt;

library ieee;
use ieee.std_logic_1164.all;

architecture knhfxj of vehagtvt is
  signal nky : time_vector(3 to 3);
  signal yc : integer;
  signal foh : std_logic;
  signal fq : time_vector(3 to 3);
  signal raabolgdv : std_logic_vector(4 to 1);
begin
  asjkapnts : entity work.cev
    port map (vezpsqp => raabolgdv, hqu => fq);
  wowepbdd : entity work.ylpgz
    port map (add => foh, os => yc);
  lxyfvant : entity work.cev
    port map (vezpsqp => raabolgdv, hqu => nky);
  
  -- Single-driven assignments
  qndzalj <= TRUE;
  rdihmvhral <= 8#1.14#;
  
  -- Multi-driven assignments
  foh <= '-';
  raabolgdv <= "";
  raabolgdv <= "";
end knhfxj;



-- Seed after: 9689962982511288716,13479070923501788437
