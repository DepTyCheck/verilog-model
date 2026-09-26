-- Seed: 7395163244609944391,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity lnixwet is
  port (iuzooqvrv : in bit_vector(0 to 3); wunorgrd : out std_logic; gpuk : out std_logic; ii : buffer time);
end lnixwet;

architecture hqanm of lnixwet is
  
begin
  -- Multi-driven assignments
  gpuk <= 'Z';
  gpuk <= wunorgrd;
  gpuk <= '1';
  gpuk <= 'H';
end hqanm;

library ieee;
use ieee.std_logic_1164.all;

entity ex is
  port (kolldwhqwo : in severity_level; gjuqe : in std_logic_vector(2 to 4));
end ex;

library ieee;
use ieee.std_logic_1164.all;

architecture tcmaozrzw of ex is
  signal dedyrt : time;
  signal mp : std_logic;
  signal nekma : std_logic;
  signal sm : bit_vector(0 to 3);
begin
  tgfzl : entity work.lnixwet
    port map (iuzooqvrv => sm, wunorgrd => nekma, gpuk => mp, ii => dedyrt);
  
  -- Single-driven assignments
  sm <= ('0', '1', '0', '0');
  
  -- Multi-driven assignments
  nekma <= mp;
  nekma <= 'X';
  nekma <= nekma;
end tcmaozrzw;

entity kvvrqki is
  port (cjmms : linkage time);
end kvvrqki;

library ieee;
use ieee.std_logic_1164.all;

architecture nfinbjxud of kvvrqki is
  signal gbmqkpo : time;
  signal ospzrowe : bit_vector(0 to 3);
  signal swzsfm : std_logic_vector(2 to 4);
  signal au : severity_level;
  signal dgbrcihg : time;
  signal qrvyczc : std_logic;
  signal aw : bit_vector(0 to 3);
begin
  r : entity work.lnixwet
    port map (iuzooqvrv => aw, wunorgrd => qrvyczc, gpuk => qrvyczc, ii => dgbrcihg);
  tzesnmo : entity work.ex
    port map (kolldwhqwo => au, gjuqe => swzsfm);
  qaklmatpxf : entity work.lnixwet
    port map (iuzooqvrv => ospzrowe, wunorgrd => qrvyczc, gpuk => qrvyczc, ii => gbmqkpo);
  
  -- Single-driven assignments
  aw <= ('1', '0', '1', '0');
  ospzrowe <= ('1', '1', '0', '0');
  au <= FAILURE;
  
  -- Multi-driven assignments
  qrvyczc <= '1';
end nfinbjxud;

entity ryuqrhr is
  port (ipeckzta : buffer bit_vector(0 downto 0); otowwlgznn : out boolean_vector(1 downto 1); pw : out integer);
end ryuqrhr;

library ieee;
use ieee.std_logic_1164.all;

architecture accvlbbcoa of ryuqrhr is
  signal vjkfjbpev : time;
  signal pubpg : std_logic;
  signal adroscj : bit_vector(0 to 3);
  signal igwvahgx : std_logic_vector(2 to 4);
  signal v : severity_level;
begin
  milrqpvm : entity work.ex
    port map (kolldwhqwo => v, gjuqe => igwvahgx);
  pwmalrtv : entity work.lnixwet
    port map (iuzooqvrv => adroscj, wunorgrd => pubpg, gpuk => pubpg, ii => vjkfjbpev);
  
  -- Single-driven assignments
  pw <= pw;
  otowwlgznn <= (others => TRUE);
  adroscj <= adroscj;
  
  -- Multi-driven assignments
  igwvahgx <= "HXX";
end accvlbbcoa;



-- Seed after: 4522925107668107137,10875537289884587119
