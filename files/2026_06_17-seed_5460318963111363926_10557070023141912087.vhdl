-- Seed: 5460318963111363926,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity qtdhfh is
  port (af : inout std_logic; jeoc : in bit_vector(3 to 1); eue : inout boolean; dfawlwiger : linkage std_logic);
end qtdhfh;

architecture zorwfydki of qtdhfh is
  
begin
  -- Multi-driven assignments
  af <= '-';
end zorwfydki;

entity zbbd is
  port (bkyjhsblj : linkage time);
end zbbd;

library ieee;
use ieee.std_logic_1164.all;

architecture ij of zbbd is
  signal kfmo : std_logic;
  signal tzdcqhdomo : boolean;
  signal m : std_logic;
  signal wfq : boolean;
  signal nkcbmp : boolean;
  signal axsrvcmskg : std_logic;
  signal z : boolean;
  signal rvwu : bit_vector(3 to 1);
  signal oxafdpbhpo : std_logic;
begin
  uft : entity work.qtdhfh
    port map (af => oxafdpbhpo, jeoc => rvwu, eue => z, dfawlwiger => oxafdpbhpo);
  dci : entity work.qtdhfh
    port map (af => axsrvcmskg, jeoc => rvwu, eue => nkcbmp, dfawlwiger => axsrvcmskg);
  aimci : entity work.qtdhfh
    port map (af => axsrvcmskg, jeoc => rvwu, eue => wfq, dfawlwiger => m);
  szlh : entity work.qtdhfh
    port map (af => oxafdpbhpo, jeoc => rvwu, eue => tzdcqhdomo, dfawlwiger => kfmo);
  
  -- Single-driven assignments
  rvwu <= (others => '0');
  
  -- Multi-driven assignments
  m <= 'Z';
end ij;



-- Seed after: 2557078976035388812,10557070023141912087
