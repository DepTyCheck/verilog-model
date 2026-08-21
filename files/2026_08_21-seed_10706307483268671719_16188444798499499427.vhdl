-- Seed: 10706307483268671719,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity hdtowgvcm is
  port (km : inout std_logic_vector(0 to 3));
end hdtowgvcm;

architecture acxth of hdtowgvcm is
  
begin
  -- Multi-driven assignments
  km <= ('X', '1', 'X', 'U');
  km <= "LWH-";
  km <= ('L', 'X', 'Z', '-');
  km <= km;
end acxth;

entity rxwkc is
  port (zaohsfw : linkage bit);
end rxwkc;

library ieee;
use ieee.std_logic_1164.all;

architecture qhaud of rxwkc is
  signal ejixtci : std_logic_vector(0 to 3);
  signal g : std_logic_vector(0 to 3);
  signal bmzhdhsnwn : std_logic_vector(0 to 3);
begin
  yc : entity work.hdtowgvcm
    port map (km => bmzhdhsnwn);
  wmdmiczuln : entity work.hdtowgvcm
    port map (km => g);
  imzh : entity work.hdtowgvcm
    port map (km => ejixtci);
  
  -- Multi-driven assignments
  ejixtci <= bmzhdhsnwn;
  ejixtci <= bmzhdhsnwn;
  ejixtci <= "HH00";
end qhaud;



-- Seed after: 6553899317830331562,16188444798499499427
