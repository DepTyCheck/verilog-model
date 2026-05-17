-- Seed: 16988302027404854655,3820899062418988741

library ieee;
use ieee.std_logic_1164.all;

entity mtbbd is
  port (ntwxhr : inout std_logic; hpw : in integer; t : buffer integer);
end mtbbd;



architecture jc of mtbbd is
  
begin
  
end jc;



entity vze is
  port (puuoidyg : out bit; gdoie : buffer real; favnvawjhq : in boolean);
end vze;

library ieee;
use ieee.std_logic_1164.all;

architecture hjowtvmp of vze is
  signal llnrgjavm : std_logic;
  signal ciqr : integer;
  signal mrmvxa : integer;
  signal gzmqov : integer;
  signal qmfmkuwzib : std_logic;
begin
  creagscff : entity work.mtbbd
    port map (ntwxhr => qmfmkuwzib, hpw => gzmqov, t => mrmvxa);
  imibq : entity work.mtbbd
    port map (ntwxhr => qmfmkuwzib, hpw => ciqr, t => gzmqov);
  sj : entity work.mtbbd
    port map (ntwxhr => llnrgjavm, hpw => gzmqov, t => ciqr);
end hjowtvmp;



entity kbf is
  port (acszzfu : out integer; jos : linkage real; vlrjkm : in integer);
end kbf;

library ieee;
use ieee.std_logic_1164.all;

architecture qwyfkc of kbf is
  signal rjnpzrbfl : std_logic;
begin
  dbdv : entity work.mtbbd
    port map (ntwxhr => rjnpzrbfl, hpw => vlrjkm, t => acszzfu);
end qwyfkc;



entity otvr is
  port (zv : out real; lvezbyja : inout integer; quqzpez : inout integer; fclv : out integer);
end otvr;



architecture uyidfa of otvr is
  signal jorlnxrcb : real;
begin
  yzefdeftd : entity work.kbf
    port map (acszzfu => fclv, jos => jorlnxrcb, vlrjkm => fclv);
end uyidfa;



-- Seed after: 12141115547273949168,3820899062418988741
