-- Seed: 13390743702039296713,15306146742274979323

entity ncaqomldaq is
  port (ifz : linkage integer_vector(1 to 0));
end ncaqomldaq;

architecture cf of ncaqomldaq is
  
begin
  
end cf;

library ieee;
use ieee.std_logic_1164.all;

entity xms is
  port (ngwrclu : inout std_logic_vector(1 to 2); tt : buffer real_vector(2 downto 0); pruz : buffer integer);
end xms;

architecture keenag of xms is
  signal m : integer_vector(1 to 0);
  signal eopwrpcnzu : integer_vector(1 to 0);
  signal djqfgcgt : integer_vector(1 to 0);
begin
  cmsqgh : entity work.ncaqomldaq
    port map (ifz => djqfgcgt);
  nbvogqow : entity work.ncaqomldaq
    port map (ifz => eopwrpcnzu);
  lm : entity work.ncaqomldaq
    port map (ifz => m);
  
  -- Single-driven assignments
  pruz <= 3_0_2_0;
  tt <= tt;
end keenag;

library ieee;
use ieee.std_logic_1164.all;

entity nrows is
  port (sjdgk : out std_logic_vector(3 to 0); jwuaat : in time);
end nrows;

library ieee;
use ieee.std_logic_1164.all;

architecture qdn of nrows is
  signal azsnkioxpo : integer_vector(1 to 0);
  signal zktiogz : integer;
  signal xbhxt : real_vector(2 downto 0);
  signal cajfjcjka : integer_vector(1 to 0);
  signal iimu : integer;
  signal ewntqn : real_vector(2 downto 0);
  signal nwkdf : std_logic_vector(1 to 2);
begin
  uozibowjee : entity work.xms
    port map (ngwrclu => nwkdf, tt => ewntqn, pruz => iimu);
  ncfid : entity work.ncaqomldaq
    port map (ifz => cajfjcjka);
  uqjaw : entity work.xms
    port map (ngwrclu => nwkdf, tt => xbhxt, pruz => zktiogz);
  lh : entity work.ncaqomldaq
    port map (ifz => azsnkioxpo);
end qdn;



-- Seed after: 9166115964878432734,15306146742274979323
