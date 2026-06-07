-- Seed: 11991979559733876485,13903879141658024201



entity zov is
  port (yap : buffer time_vector(0 downto 2); onpn : out integer; wgk : buffer integer_vector(1 downto 4));
end zov;



architecture kgwrmvdvak of zov is
  
begin
  
end kgwrmvdvak;

library ieee;
use ieee.std_logic_1164.all;

entity ohyi is
  port (vteedsqsym : in std_logic_vector(2 downto 4); hugjrzalv : linkage time_vector(3 downto 4); liiarngs : buffer bit; sapxxhp : linkage std_logic);
end ohyi;



architecture og of ohyi is
  signal xvjbc : integer_vector(1 downto 4);
  signal yheraxg : integer;
  signal uj : time_vector(0 downto 2);
begin
  yxrfpijsj : entity work.zov
    port map (yap => uj, onpn => yheraxg, wgk => xvjbc);
end og;

library ieee;
use ieee.std_logic_1164.all;

entity lelcdeymy is
  port (cwvalcwqba : in std_logic_vector(1 to 4));
end lelcdeymy;

library ieee;
use ieee.std_logic_1164.all;

architecture leihn of lelcdeymy is
  signal hulxzpmj : integer_vector(1 downto 4);
  signal whvpq : integer;
  signal kfvwg : std_logic;
  signal p : bit;
  signal uymphoao : time_vector(0 downto 2);
  signal uunqzewu : std_logic_vector(2 downto 4);
begin
  svfn : entity work.ohyi
    port map (vteedsqsym => uunqzewu, hugjrzalv => uymphoao, liiarngs => p, sapxxhp => kfvwg);
  npkic : entity work.zov
    port map (yap => uymphoao, onpn => whvpq, wgk => hulxzpmj);
end leihn;



-- Seed after: 2445523758999463710,13903879141658024201
