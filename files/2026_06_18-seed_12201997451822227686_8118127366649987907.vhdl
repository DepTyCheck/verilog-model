-- Seed: 12201997451822227686,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity grbnwhg is
  port (ehtoyv : out time; gwrmvn : inout std_logic; nlq : in std_logic_vector(3 downto 0));
end grbnwhg;

architecture ecekova of grbnwhg is
  
begin
  -- Multi-driven assignments
  gwrmvn <= 'L';
  gwrmvn <= 'U';
  gwrmvn <= 'U';
  gwrmvn <= 'W';
end ecekova;

library ieee;
use ieee.std_logic_1164.all;

entity kmk is
  port (vwpaonrleb : out std_logic; ti : out real; o : linkage std_logic_vector(2 to 1); gwucxmiahx : in integer);
end kmk;

library ieee;
use ieee.std_logic_1164.all;

architecture zfpjjado of kmk is
  signal fej : std_logic_vector(3 downto 0);
  signal hejnd : time;
  signal yqofm : std_logic_vector(3 downto 0);
  signal tsnj : time;
  signal umppppp : std_logic_vector(3 downto 0);
  signal rje : time;
begin
  he : entity work.grbnwhg
    port map (ehtoyv => rje, gwrmvn => vwpaonrleb, nlq => umppppp);
  dmb : entity work.grbnwhg
    port map (ehtoyv => tsnj, gwrmvn => vwpaonrleb, nlq => yqofm);
  hatxglfmmg : entity work.grbnwhg
    port map (ehtoyv => hejnd, gwrmvn => vwpaonrleb, nlq => fej);
  
  -- Single-driven assignments
  ti <= 0.120;
  
  -- Multi-driven assignments
  vwpaonrleb <= '-';
end zfpjjado;

entity hi is
  port (vzl : inout time; zwy : out integer; llfdxdddt : in character);
end hi;

library ieee;
use ieee.std_logic_1164.all;

architecture rxtzljqakk of hi is
  signal rpmnrcrzcp : integer;
  signal huwx : std_logic_vector(2 to 1);
  signal wiopfsoxlo : real;
  signal j : std_logic;
begin
  schmiakyi : entity work.kmk
    port map (vwpaonrleb => j, ti => wiopfsoxlo, o => huwx, gwucxmiahx => rpmnrcrzcp);
  
  -- Single-driven assignments
  rpmnrcrzcp <= 3;
  vzl <= 8#4# fs;
  zwy <= 8#114#;
  
  -- Multi-driven assignments
  j <= '1';
end rxtzljqakk;

library ieee;
use ieee.std_logic_1164.all;

entity itkskjlrh is
  port (vxhy : buffer std_logic);
end itkskjlrh;

architecture lpgm of itkskjlrh is
  
begin
  -- Multi-driven assignments
  vxhy <= 'U';
  vxhy <= '1';
  vxhy <= '0';
end lpgm;



-- Seed after: 1314866900203907561,8118127366649987907
