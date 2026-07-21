-- Seed: 14991535220450516446,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity cofb is
  port (ljeh : out std_logic; lwql : out std_logic; vbwumlfbb : in severity_level);
end cofb;

architecture l of cofb is
  
begin
  -- Multi-driven assignments
  lwql <= 'X';
  lwql <= lwql;
  lwql <= lwql;
end l;

entity oamjwxwx is
  port (nypzheot : inout real; lhlqmumn : out real_vector(3 downto 2));
end oamjwxwx;

library ieee;
use ieee.std_logic_1164.all;

architecture rtkqwj of oamjwxwx is
  signal cpahdwrqhu : severity_level;
  signal ylmpdssjdh : std_logic;
  signal aetban : std_logic;
  signal hh : std_logic;
  signal pzgfpghjn : severity_level;
  signal fkuaw : std_logic;
begin
  wwqgwhvl : entity work.cofb
    port map (ljeh => fkuaw, lwql => fkuaw, vbwumlfbb => pzgfpghjn);
  kn : entity work.cofb
    port map (ljeh => hh, lwql => hh, vbwumlfbb => pzgfpghjn);
  m : entity work.cofb
    port map (ljeh => fkuaw, lwql => aetban, vbwumlfbb => pzgfpghjn);
  optxyxir : entity work.cofb
    port map (ljeh => ylmpdssjdh, lwql => fkuaw, vbwumlfbb => cpahdwrqhu);
  
  -- Single-driven assignments
  cpahdwrqhu <= pzgfpghjn;
  
  -- Multi-driven assignments
  aetban <= 'Z';
  fkuaw <= 'W';
  hh <= '-';
  fkuaw <= fkuaw;
end rtkqwj;



-- Seed after: 13177365113616757040,11481034001933599325
