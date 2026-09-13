-- Seed: 9318178809790674112,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity ohiojt is
  port (dv : buffer std_logic);
end ohiojt;

architecture yk of ohiojt is
  
begin
  -- Multi-driven assignments
  dv <= dv;
  dv <= '-';
end yk;

library ieee;
use ieee.std_logic_1164.all;

entity ayp is
  port (hwl : in real; lsnbrol : out time; zutkxq : out std_logic_vector(0 to 4));
end ayp;

library ieee;
use ieee.std_logic_1164.all;

architecture i of ayp is
  signal fo : std_logic;
begin
  gua : entity work.ohiojt
    port map (dv => fo);
end i;

library ieee;
use ieee.std_logic_1164.all;

entity oauod is
  port (cz : buffer std_logic);
end oauod;

architecture kcvbkvjo of oauod is
  
begin
  hwalz : entity work.ohiojt
    port map (dv => cz);
  jxbsgh : entity work.ohiojt
    port map (dv => cz);
  
  -- Multi-driven assignments
  cz <= cz;
  cz <= 'X';
  cz <= 'X';
  cz <= cz;
end kcvbkvjo;

library ieee;
use ieee.std_logic_1164.all;

entity kofdkam is
  port (qe : in std_logic; ghakmr : out integer; kxameeq : buffer character; eivmp : out bit);
end kofdkam;

library ieee;
use ieee.std_logic_1164.all;

architecture lwqcwdd of kofdkam is
  signal gagge : std_logic;
begin
  odokuv : entity work.oauod
    port map (cz => gagge);
  
  -- Single-driven assignments
  eivmp <= eivmp;
  kxameeq <= 'p';
  ghakmr <= ghakmr;
  
  -- Multi-driven assignments
  gagge <= 'X';
  gagge <= 'U';
  gagge <= '1';
  gagge <= qe;
end lwqcwdd;



-- Seed after: 3270191516487163629,10754487200446211253
