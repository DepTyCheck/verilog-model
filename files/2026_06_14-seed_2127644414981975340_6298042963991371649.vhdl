-- Seed: 2127644414981975340,6298042963991371649

library ieee;
use ieee.std_logic_1164.all;

entity ekcfyjcpii is
  port (gr : linkage bit_vector(4 to 1); io : inout std_logic);
end ekcfyjcpii;



architecture nyh of ekcfyjcpii is
  
begin
  
end nyh;

library ieee;
use ieee.std_logic_1164.all;

entity hribrbjmac is
  port (i : linkage real_vector(2 downto 3); brii : buffer std_logic_vector(1 downto 2); kdyxyukyrg : inout time; mlzwcjrk : linkage std_logic);
end hribrbjmac;



architecture tfnayj of hribrbjmac is
  
begin
  
end tfnayj;

library ieee;
use ieee.std_logic_1164.all;

entity ycubiohqvy is
  port (kro : in real; qtwuzi : in std_logic; urwwaqueou : inout std_logic; czyiopqju : out integer_vector(1 to 4));
end ycubiohqvy;

library ieee;
use ieee.std_logic_1164.all;

architecture uhzm of ycubiohqvy is
  signal rqjxg : std_logic;
  signal f : time;
  signal yomzrw : std_logic_vector(1 downto 2);
  signal zuu : real_vector(2 downto 3);
  signal kgvnvpm : std_logic;
  signal el : bit_vector(4 to 1);
  signal bi : std_logic;
  signal d : bit_vector(4 to 1);
begin
  ajfv : entity work.ekcfyjcpii
    port map (gr => d, io => bi);
  vqb : entity work.ekcfyjcpii
    port map (gr => el, io => kgvnvpm);
  lqfbi : entity work.hribrbjmac
    port map (i => zuu, brii => yomzrw, kdyxyukyrg => f, mlzwcjrk => rqjxg);
  n : entity work.ekcfyjcpii
    port map (gr => d, io => urwwaqueou);
end uhzm;



entity mildiqwhj is
  port (uglsfc : inout time; ejhrlbj : inout time; bnikfe : out integer_vector(0 downto 4); egdewn : buffer integer);
end mildiqwhj;

library ieee;
use ieee.std_logic_1164.all;

architecture xb of mildiqwhj is
  signal xgwrubtv : bit_vector(4 to 1);
  signal kkryi : integer_vector(1 to 4);
  signal maizvfpby : std_logic;
  signal k : std_logic;
  signal p : real;
  signal rfi : std_logic;
  signal cnrn : std_logic;
  signal qjjwqqapho : bit_vector(4 to 1);
begin
  czqxzmul : entity work.ekcfyjcpii
    port map (gr => qjjwqqapho, io => cnrn);
  mezpygujza : entity work.ekcfyjcpii
    port map (gr => qjjwqqapho, io => rfi);
  zakbmxazl : entity work.ycubiohqvy
    port map (kro => p, qtwuzi => k, urwwaqueou => maizvfpby, czyiopqju => kkryi);
  dduuv : entity work.ekcfyjcpii
    port map (gr => xgwrubtv, io => cnrn);
end xb;



-- Seed after: 5656534329534612164,6298042963991371649
